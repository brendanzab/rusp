#[link(name = "rusp",
       vers = "0.1",
       author = "Brendan Zabarauskas",
       url = "https://github.com/bjz/rusp")];

#[comment = "A minimal Lisp dialect for Rust."];
#[license = "ASL2"];
#[crate_type = "lib"];

// Inspired by Peter Norvig's [Lis.py](http://norvig.com/lispy.html)

use std::hashmap::*;

pub mod builtins;
pub mod parser;
pub mod pprint;

///
/// Performs a recursive decent parse of the source string.
///
pub fn parse(src: &str) -> Result<~[@Value], parser::ParseFailure> {
    let mut parsed = ~[];
    let mut p = parser::Parser::new(src);

    while !p.is_eof() {
        match p.parse() {
            Ok(v) => parsed.push(@v),
            Err(e) => return Err(e)
        }
    }

    Ok(parsed)
}

/// Symbol identifier
pub type Ident = ~str;

/// Holds a typed value
#[deriving(Eq, Clone)]
pub enum Value {
    Bool(bool),
    Int(int),
    Float(float),
    Str(~str),
    List(~[@Value]),
    Symbol(Ident),
    Lambda(~[Ident], @Value),
    Macro(~[Ident], @Value),
    Rust(RustFn),
    RustMacro(RustFn),
}

/// Workaround for `deriving` not working for rust closures
pub struct RustFn(@fn(params: &[@Value], env: @mut Rusp) -> EvalResult);

impl Eq for RustFn {
    fn eq(&self, _: &RustFn) -> bool {
        fail!("Cannot test equality of Rust functions (yet?).");
    }
    fn ne(&self, other: &RustFn) -> bool { !self.eq(other) }
}

impl Clone for RustFn {
    fn clone(&self) -> RustFn {
        // copy the pointer
        *self
    }
}

#[deriving(Eq)]
pub struct Rusp {
    values: HashMap<Ident, @Value>,
    outer: Option<@mut Rusp>,
}

pub type EvalResult = Result<@Value, ~str>;

impl Rusp {
    /// Initializes an empty environment
    pub fn empty() -> @mut Rusp {
        @mut Rusp {
            values: HashMap::new(),
            outer: None,
        }
    }

    /// Initializes an environment containing the default built-ins
    pub fn new() -> @mut Rusp {
        @mut Rusp {
            values: builtins::builtins(),
            outer: None
        }
    }

    /// Initializes a new environment with a parent environment
    pub fn new_with_outer(outer: @mut Rusp) -> @mut Rusp {
        @mut Rusp {
            values: HashMap::new(),
            outer: Some(outer),
        }
    }

    /// Defines a new identifier/value pair in the environment
    pub fn define(&mut self, ident: Ident, val: @Value) -> EvalResult {
        if self.values.insert(ident.clone(), val)  {
            Ok(@List(~[]))
        } else {
            Err(fmt!("`%s` was already defined in this environment.", ident.to_str()))
        }
    }

    /// Attempts to find a value corresponding to the supplied identifier
    pub fn find(&self, ident: &Ident) -> Option<@Value> {
        match self.values.find(ident) {
            Some(val) => Some(*val),
            None => self.outer.chain(|env| env.find(ident)),
        }
    }

    /// Evaluates a Rusp expression in the environment
    pub fn eval(@mut self, value: @Value) -> EvalResult {
        match *value {
            Symbol(ref id) => {
                match self.find(id) {
                    Some(v) => Ok(v),
                    None => Err(fmt!("The value of `%s` was not defined in this environment",
                                     id.to_str())),
                }
            }
            List(ref vals) => {
                match *vals {
                    [ref v, .. args] => {
                        do self.eval(*v).chain |res| {
                            self.eval_call(res, args)
                        }
                    }
                    [] => Ok(@List(~[])),
                }
            }
            // everything else evaluates to itself
            _ => Ok(value)
        }
    }

    /// Eval a call using `func`, which should have been previously
    /// evaluated.
    fn eval_call(@mut self, func: &Value, args: &[@Value]) -> EvalResult {
        match *func {
            Lambda(ref ids, ref body) => {
                if ids.len() != args.len() {
                    return Err(fmt!("lambda expects %u arguments", ids.len()));
                }
                // stores the argument bindings of the function
                let local = Rusp::new_with_outer(self);
                for vec::each2(*ids, args) |id, val| {
                    match self.eval(*val) {
                        Err(e) => return Err(e),
                        Ok(v) => match local.define(id.clone(), v) {
                            Err(e) => return Err(e),
                            _ => {}
                        }
                    }
                }
                local.eval(*body)
            }
            Macro(ref ids, ref body) => {
                if ids.len() != args.len() {
                    return Err(fmt!("macro expects %u arguments", ids.len()));
                }
                // stores the argument bindings of the macro
                let local = Rusp::new_with_outer(self);
                for vec::each2(*ids, args) |id, val| {
                    match local.define(id.clone(), *val) {
                        Err(e) => return Err(e),
                        _ => {}
                    }
                }
                local.eval(*body)
            }
            Rust(ref f) => {
                let mut evaled_args = ~[];
                for args.each |v| {
                    match self.eval(*v) {
                        Err(e) => return Err(e),
                        Ok(evaled) => { evaled_args.push(evaled) }
                    }
                }
                (**f)(evaled_args, self)
            }
            RustMacro(ref f) => {
                (**f)(args, self)
            }
            _ => Err(~"not a function")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_env() {
        let outer = Rusp::new();
        outer.define(~"a", @Int(0));
        outer.define(~"b", @Float(1.0));
        outer.define(~"c", @Str(~"hi"));

        let inner = Rusp::new_with_outer(outer);
        inner.define(~"a", @Int(3));
        inner.define(~"b", @Int(4));

        assert_eq!(outer.find(&~"a"), Some(@Int(0)));
        assert_eq!(outer.find(&~"b"), Some(@Float(1.0)));
        assert_eq!(outer.find(&~"c"), Some(@Str(~"hi")));
        assert_eq!(inner.find(&~"a"), Some(@Int(3)));
        assert_eq!(inner.find(&~"b"), Some(@Int(4)));
        assert_eq!(inner.find(&~"c"), Some(@Str(~"hi")));
    }

    #[test]
    fn test_atoms() {
        assert_eq!(Rusp::new().eval(@Int(1)).get(), @Int(1));
        assert_eq!(Rusp::new().eval(@Float(1.0)).get(), @Float(1.0));
        assert_eq!(Rusp::new().eval(@Str(~"hi")).get(), @Str(~"hi"));
    }

    #[test]
    fn test_list() {
        assert_eq!(Rusp::new().eval(@List(~[])).get(), @List(~[]));
        assert_eq!(
            Rusp::new().eval(@List(~[
                @Symbol(~"list"),
                @Str(~"hi"),
                @List(~[@Symbol(~"quote"), @Symbol(~"x")]),
                @Int(1)
            ])).get(),
            @List(~[
                @Str(~"hi"),
                @Symbol(~"x"),
                @Int(1)
            ])
        );
    }

    #[test]
    fn test_quote() {
        assert_eq!(Rusp::new().eval(
            @List(~[@Symbol(~"quote"), @Symbol(~"x")])).get(),
            @Symbol(~"x")
        );
    }

    #[test]
    fn test_symbol() {
        let env = Rusp::new();
        env.define(~"a", @Int(0));
        env.define(~"b", @Float(1.0));
        env.define(~"c", @Str(~"hi"));

        assert_eq!(env.eval(@Symbol(~"a")).get(), @Int(0));
        assert_eq!(env.eval(@Symbol(~"b")).get(), @Float(1.0));
        assert_eq!(env.eval(@Symbol(~"c")).get(), @Str(~"hi"));
        assert!(env.eval(@Symbol(~"d")).is_err());
    }

    #[test]
    fn test_if() {
        fn not(test: Value) -> EvalResult {
            Rusp::new().eval(@List(~[@Symbol(~"if"), @test, @Bool(false), @Bool(true)]))
        }

        assert_eq!(not(Bool(true)).get(), @Bool(false));
        assert_eq!(not(Bool(false)).get(), @Bool(true));
        assert!(not(Str(~"hi")).is_err());
    }

    #[test]
    fn test_def() {
        let env = Rusp::new();
        assert!(env.eval(@List(~[@Symbol(~"def"), @Symbol(~"a"), @Int(0)])).is_ok());
        assert!(env.eval(@List(~[@Symbol(~"def"), @Symbol(~"b"), @Float(1.0)])).is_ok());
        assert!(env.eval(@List(~[@Symbol(~"def"), @Symbol(~"c"), @Str(~"hi")])).is_ok());

        assert_eq!(env.find(&~"a"), Some(@Int(0)));
        assert_eq!(env.find(&~"b"), Some(@Float(1.0)));
        assert_eq!(env.find(&~"c"), Some(@Str(~"hi")));

        assert!(env.eval(@List(~[@Symbol(~"def"), @Symbol(~"c"), @List(~[])])).is_err());
    }

    #[test]
    fn test_do() {
        assert_eq!(Rusp::new().eval(@List(~[@Symbol(~"do"), @Str(~"hi")])).get(), @Str(~"hi"));
        assert_eq!(
            Rusp::new().eval(@List(~[
                @Symbol(~"do"),
                @List(~[]),
                @List(~[@Symbol(~"do"), @List(~[])]),
                @Str(~"hi")
            ])).get(),
            @Str(~"hi")
        );

        assert!(Rusp::new().eval(@List(~[@Symbol(~"do"), @Str(~"hi"), @List(~[])])).is_err());
    }

    #[test]
    fn test_fn() {
        let env = Rusp::new();
        env.eval(parse("(def f (fn (a b) a))").unwrap()[0]);
        assert_eq!(env.eval(parse("(f 1 2)").unwrap()[0]).unwrap(), @Int(1));

        assert!(env.eval(parse("(f 1)").unwrap()[0]).is_err());
    }

    #[test]
    fn test_macro() {
        let env = Rusp::new();
        env.eval(parse("(def m (macro (a) a))").unwrap()[0]);
        env.eval(parse("(def f (fn (a) a))").unwrap()[0]);
        env.eval(parse("(def x 1)").unwrap()[0]);

        // check the semantics of macros vs fn, x is not evaluated for
        // `m`, but is for `f`.
        assert_eq!(env.eval(parse("(m x)").unwrap()[0]).unwrap(), @Symbol(~"x"));
        assert_eq!(env.eval(parse("(f x)").unwrap()[0]).unwrap(), @Int(1));
    }
}
