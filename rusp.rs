#[link(name = "rusp",
       vers = "0.1",
       author = "Brendan Zabarauskas",
       url = "https://github.com/bjz/rusp")];

#[comment = "A minimal Lisp dialect for Rust."];
#[license = "ASL2"];
#[crate_type = "lib"];

// Inspired by Peter Norvig's [Lis.py](http://norvig.com/lispy.html)

use std::hashmap::*;

pub mod parser;
pub mod pprint;

///
/// Performs a recursive decent parse of the source string.
///
pub fn parse(src: &str) -> Result<~Value, parser::ParseFailure> {
    parser::Parser::new(src).parse()
}

/// Symbol identifier
pub type Ident = ~str;

/// Holds a typed value
#[deriving(Eq, Clone)]
pub enum Value {
    /// A boolean value: either `true` or `false`
    Bool(bool),
    /// An integer value
    Int(int),
    /// A floating point value
    Float(float),
    /// String value: `"<value>"`
    Str(~str),
    /// Hetrogeneous list: `(list <val>*)`
    List(~[~Value]),
    /// Symbol reference: `<ident>`
    Symbol(Ident),
    /// Conditional expression: `(if <test> <conseq> <alt>)`
    If(~Value, ~Value, ~Value),
    /// Symbol definition: `(def <ident> <expr>)`
    Def(Ident, ~Value),
    /// Do expression: `(do <expr>+)`
    Do(~[~Value]),
    /// Quoted expression: `(quote <expr>)`
    Quote(~Value),
    /// A Rust function
    Rust(RustFn),
    /// Anonymous function: `(fn (<ident>*) <expr>)`
    Lambda(~[Ident], ~Value),
    /// Function application: `(<expr> <expr>+)`
    Apply(~Value, ~[~Value]),
}

/// Workaround for `deriving` not working for rust closures
pub struct RustFn(@fn(params: ~[Ident], env: &Rusp) -> Value);

impl Eq for RustFn {
    fn eq(&self, _: &RustFn) -> bool {
        fail!("Cannot test equality of Rust functions (yet?).");
    }
    fn ne(&self, other: &RustFn) -> bool { !self.eq(other) }
}

impl Clone for RustFn {
    fn clone(&self) -> RustFn {
        fail!("Cannot clone Rust functions (yet?)");
    }
}

#[deriving(Eq)]
pub struct Rusp {
    values: HashMap<Ident, Value>,
    outer: Option<@mut Rusp>,
}

pub type EvalResult = Result<Value, ~str>;

impl Rusp {
    /// Initializes an empty environment
    pub fn empty() -> @mut Rusp {
        @mut Rusp {
            values: HashMap::new(),
            outer: None,
        }
    }

    /// Initializes a new environment with the specified values
    pub fn new(outer: @mut Rusp) -> @mut Rusp {
        @mut Rusp {
            values: HashMap::new(),
            outer: Some(outer),
        }
    }

    /// Defines a new identifier/value pair
    pub fn define(&mut self, ident: Ident, val: Value) -> bool {
        self.values.insert(ident, val)
    }

    /// Attempts to find a value corresponding to the supplied identifier
    pub fn find(&self, ident: &Ident) -> Option<Value> {
        match self.values.find(ident) {
            Some(val) => Some(val.clone()),
            None => self.outer.chain(|env| env.find(ident)),
        }
    }

    /// Evaluates a Rusp expression in the environment
    pub fn eval(&mut self, value: &Value) -> EvalResult {
        match *value {
            Bool(b) => Ok(Bool(b)),
            Int(i) => Ok(Int(i)),
            Float(f) => Ok(Float(f)),
            List(ref vals) => {
                let mut evaled = ~[];
                for vals.each |&val| {
                    match self.eval(val) {
                        Ok(v) => evaled.push(~v),
                        Err(e) => return Err(e),
                    }
                }
                Ok(List(evaled))
            }
            Str(ref s) => Ok(Str(s.to_owned())),
            Symbol(ref id) => {
                match self.find(id) {
                    Some(v) => Ok(v),
                    None => Err(fmt!("The value of `%s` was not defined in this environment", id.to_str())),
                }
            }
            If(ref test, ref conseq, ref alt) => {
                do self.eval(*test).chain |val| {
                    match val {
                        Bool(true) => self.eval(*conseq),
                        Bool(false) => self.eval(*alt),
                        _ => Err(fmt!("expected boolean expression, found: %s", val.to_str())),
                    }
                }
            }
            Def(ref id, ref value) => {
                do self.eval(*value).chain |val| {
                    if self.define(id.clone(), val) {
                        Ok(List(~[]))
                    } else {
                        Err(fmt!("`%s` was already defined in this environment.", id.to_str()))
                    }
                }
            }
            Do(ref exprs) => {
                for uint::range(0, exprs.len() - 1) |i| {
                    match self.eval(exprs[i]) {
                        Ok(List(ref l)) if l.is_empty() => (),
                        Ok(v) => return Err(fmt!("expected unit expression, found: %s", v.to_str())),
                        Err(e) => return Err(e),
                    }
                }
                self.eval(exprs[exprs.len() - 1])
            }
            Quote(ref val) => Ok((**val).clone()),
            Rust(_) => fail!("Not yet implemented"),
            Lambda(_,_) => fail!("Not yet implemented"),
            Apply(_,_) => fail!("Not yet implemented"),
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_env() {
        let outer = Rusp::empty();
        outer.define(~"a", Int(0));
        outer.define(~"b", Float(1.0));
        outer.define(~"c", Str(~"hi"));

        let inner = Rusp::new(outer);
        inner.define(~"a", Int(3));
        inner.define(~"b", Int(4));

        assert_eq!(outer.find(&~"a"), Some(Int(0)));
        assert_eq!(outer.find(&~"b"), Some(Float(1.0)));
        assert_eq!(outer.find(&~"c"), Some(Str(~"hi")));
        assert_eq!(inner.find(&~"a"), Some(Int(3)));
        assert_eq!(inner.find(&~"b"), Some(Int(4)));
        assert_eq!(inner.find(&~"c"), Some(Str(~"hi")));
    }

    #[test]
    fn test_atoms() {
        assert_eq!(Rusp::empty().eval(&Int(1)).get(), Int(1));
        assert_eq!(Rusp::empty().eval(&Float(1.0)).get(), Float(1.0));
        assert_eq!(Rusp::empty().eval(&Str(~"hi")).get(), Str(~"hi"));
    }

    #[test]
    fn test_list() {
        assert_eq!(Rusp::empty().eval(&List(~[])).get(), List(~[]));
        assert_eq!(
            Rusp::empty().eval(&List(~[~Str(~"hi"), ~Quote(~Symbol(~"x")), ~Int(1)])).get(),
            List(~[~Str(~"hi"), ~Symbol(~"x"), ~Int(1)])
        );
    }

    #[test]
    fn test_quote() {
        assert_eq!(Rusp::empty().eval(&Quote(~Symbol(~"x"))).get(), Symbol(~"x"));
    }

    #[test]
    fn test_symbol() {
        let env = Rusp::empty();
        env.define(~"a", Int(0));
        env.define(~"b", Float(1.0));
        env.define(~"c", Str(~"hi"));

        assert_eq!(env.eval(&Symbol(~"a")).get(), Int(0));
        assert_eq!(env.eval(&Symbol(~"b")).get(), Float(1.0));
        assert_eq!(env.eval(&Symbol(~"c")).get(), Str(~"hi"));
        assert!(env.eval(&Symbol(~"d")).is_err());
    }

    #[test]
    fn test_if() {
        fn not(test: Value) -> EvalResult {
            Rusp::empty().eval(&If(~test, ~Bool(false), ~Bool(true)))
        }

        assert_eq!(not(Bool(true)).get(), Bool(false));
        assert_eq!(not(Bool(false)).get(), Bool(true));
        assert!(not(Str(~"hi")).is_err());
    }

    #[test]
    fn test_let() {
        let env = Rusp::empty();
        assert!(env.eval(&Def(~"a", ~Int(0))).is_ok());
        assert!(env.eval(&Def(~"b", ~Float(1.0))).is_ok());
        assert!(env.eval(&Def(~"c", ~Str(~"hi"))).is_ok());

        assert_eq!(env.find(&~"a"), Some(Int(0)));
        assert_eq!(env.find(&~"b"), Some(Float(1.0)));
        assert_eq!(env.find(&~"c"), Some(Str(~"hi")));

        assert!(env.eval(&Def(~"c", ~List(~[]))).is_err());
    }

    #[test]
    fn test_do() {
        assert_eq!(Rusp::empty().eval(&Do(~[~Str(~"hi")])).get(), Str(~"hi"));
        assert_eq!(
            Rusp::empty().eval(&Do(~[
                ~List(~[]),
                ~Do(~[~List(~[])]),
                ~Str(~"hi")
            ])).get(),
            Str(~"hi")
        );

        assert!(Rusp::empty().eval(&Do(~[~Str(~"hi"), ~List(~[])])).is_err());
    }

    #[test]
    fn test_apply() {

    }
}