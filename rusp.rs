#[link(name = "rusp",
       vers = "0.1",
       author = "Brendan Zabarauskas",
       url = "https://github.com/bjz/rusp")];

#[comment = "A minimal Lisp dialect for Rust."];
#[license = "ASL2"];
#[crate_type = "lib"];

// Inspired by Peter Norvig's [Lis.py](http://norvig.com/lispy.html)

use std::hashmap::*;

pub mod parse;
pub mod pprint;

///
/// Symbol identifier
///
#[deriving(Eq, Clone, IterBytes)]
pub struct Ident(~str);

///
/// Holds a typed value from the environment
///
#[deriving(Eq, Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Int(int),
    Float(float),
    Str(~str),
    ///
    /// Quoted expression
    ///
    /// ~~~
    /// (quote <expr>)
    /// ~~~
    ///
    Quote(~Expr),
    ///
    /// Anonymous function
    ///
    /// ~~~
    /// (fn (<ident>*) <expr>)
    /// ~~~
    ///
    Fn(~[Ident], ~Expr),
}

///
/// Primitive language expressions
///
#[deriving(Eq, Clone)]
pub enum Expr {
    ///
    /// Symbol reference
    ///
    /// ~~~
    /// <ident>
    /// ~~~
    ///
    Symbol(Ident),
    ///
    /// Constant literal
    ///
    /// ~~~
    /// <val>
    /// ~~~
    ///
    Literal(Value),
    ///
    /// Conditional expression
    ///
    /// ~~~
    /// (if <test> <conseq> <alt>)
    /// ~~~
    ///
    If(~Expr, ~Expr, ~Expr),
    ///
    /// Symbol definition
    ///
    /// ~~~
    /// (let <ident> <expr>)
    /// ~~~
    ///
    Let(Ident, ~Expr),
    /// Do expression
    ///
    /// Evaluates each expression in turn, returning the value of the last expression
    ///
    /// ~~~
    /// (do <expr>+)
    /// ~~~
    ///
    Do(~[~Expr]),
    ///
    /// Procedure call
    ///
    /// ~~~
    /// (<expr> <expr>+)
    /// ~~~
    ///
    Call(~Expr, ~[~Expr]),
}

#[deriving(Eq)]
pub struct Env {
    values: HashMap<Ident, Value>,
    outer: Option<@mut Env>,
}

impl Env {
    /// Initializes an empty environment
    pub fn empty() -> @mut Env {
        @mut Env { values: HashMap::new(), outer: None }
    }

    /// Initializes a new environment with the specified values
    pub fn new(values: &[(Ident, Value)], outer: Option<@mut Env>) -> @mut Env {
        let env = @mut Env {
            values: HashMap::new(),
            outer: outer,
        };
        for values.each |&(id, val)| {
            env.define(id, val);
        }
        env
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
}

pub type EvalResult = Result<Value, ~str>;

impl Expr {
    ///
    /// Evaluates an expression tree in the supplied environment
    ///
    pub fn eval(&self, env: @mut Env) -> EvalResult {
        match *self {
            Symbol(ref id) =>{
                match env.find(id) {
                    Some(val) => Ok(val),
                    None => Err(fmt!("The value of `%s` was not defined in this environment", id.to_str())),
                }
            }
            Literal(ref val) => {
                Ok(val.clone())
            }
            If(ref test, ref conseq, ref alt) => {
                do test.eval(env).chain |val| {
                    match val {
                        Bool(true) => conseq.eval(env),
                        Bool(false) => alt.eval(env),
                        _ => Err(fmt!("expected boolean expression, found: %s", val.to_str())),
                    }
                }
            }
            Let(ref id, ref expr) => {
                do expr.eval(env).chain |val| {
                    if env.define(id.clone(), val) {
                        Ok(Nil)
                    } else {
                        Err(fmt!("`%s` was already defined in this environment.", id.to_str()))
                    }
                }
            }
            Do(ref exprs) => {
                for uint::range(0, exprs.len() - 1) |i| {
                    match exprs[i].eval(env) {
                        Ok(Nil) => (),
                        Ok(val) => return Err(fmt!("expected nil expression, found: %s", val.to_str())),
                        Err(err) => return Err(err),
                    }
                }
                exprs[exprs.len() - 1].eval(env)
            }
            Call(_,_) => fail!(),
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_env() {
        let outer = Env::new([
            (Ident(~"a"), Int(0)),
            (Ident(~"b"), Float(1.0)),
            (Ident(~"c"), Str(~"hi")),
        ], None);

        let inner = Env::new([
            (Ident(~"a"), Int(3)),
            (Ident(~"b"), Int(4)),
        ], Some(outer));

        assert_eq!(outer.find(&Ident(~"a")), Some(Int(0)));
        assert_eq!(outer.find(&Ident(~"b")), Some(Float(1.0)));
        assert_eq!(outer.find(&Ident(~"c")), Some(Str(~"hi")));
        assert_eq!(inner.find(&Ident(~"a")), Some(Int(3)));
        assert_eq!(inner.find(&Ident(~"b")), Some(Int(4)));
        assert_eq!(inner.find(&Ident(~"c")), Some(Str(~"hi")));
    }

    #[test]
    fn test_eval_symbol() {
        let env = Env::new([
            (Ident(~"a"), Int(0)),
            (Ident(~"b"), Float(1.0)),
            (Ident(~"c"), Str(~"hi")),
        ], None);

        assert_eq!(Symbol(Ident(~"a")).eval(env).get(), Int(0));
        assert_eq!(Symbol(Ident(~"b")).eval(env).get(), Float(1.0));
        assert_eq!(Symbol(Ident(~"c")).eval(env).get(), Str(~"hi"));
        assert!(Symbol(Ident(~"d")).eval(env).is_err());
    }

    #[test]
    fn test_eval_literal() {
        assert_eq!(Literal(Nil).eval(Env::empty()).get(), Nil);
        assert_eq!(Literal(Int(1)).eval(Env::empty()).get(), Int(1));
        assert_eq!(Literal(Float(1.0)).eval(Env::empty()).get(), Float(1.0));
        assert_eq!(Literal(Str(~"hi")).eval(Env::empty()).get(), Str(~"hi"));
        assert_eq!(Literal(Quote(~Literal(Nil))).eval(Env::empty()).get(),
                   Quote(~Literal(Nil)));
    }

    #[test]
    fn test_eval_if() {
        fn not(test: Value) -> EvalResult {
            If(
                ~Literal(test),
                ~Literal(Bool(false)),
                ~Literal(Bool(true))
            ).eval(Env::empty())
        }

        assert_eq!(not(Bool(true)).get(), Bool(false));
        assert_eq!(not(Bool(false)).get(), Bool(true));
        assert!(not(Str(~"hi")).is_err());
    }

    #[test]
    fn test_eval_let() {
        let env = Env::empty();
        assert!(Let(Ident(~"a"), ~Literal(Int(0))).eval(env).is_ok());
        assert!(Let(Ident(~"b"), ~Literal(Float(1.0))).eval(env).is_ok());
        assert!(Let(Ident(~"c"), ~Literal(Str(~"hi"))).eval(env).is_ok());

        assert_eq!(env.find(&Ident(~"a")), Some(Int(0)));
        assert_eq!(env.find(&Ident(~"b")), Some(Float(1.0)));
        assert_eq!(env.find(&Ident(~"c")), Some(Str(~"hi")));

        assert!(Let(Ident(~"c"), ~Literal(Nil)).eval(env).is_err());
    }

    #[test]
    fn test_eval_do() {
        assert_eq!(Do(~[~Literal(Str(~"hi"))]).eval(Env::empty()).get(), Str(~"hi"));
        assert_eq!(
            Do(~[
                ~Literal(Nil),
                ~Do(~[~Literal(Nil)]),
                ~Literal(Str(~"hi"))
            ]).eval(Env::empty()).get(),
            Str(~"hi")
        );

        assert!(Do(~[~Literal(Str(~"hi")),~Literal(Nil)]).eval(Env::empty()).is_err());
    }

    #[test]
    fn test_eval_call() {

    }
}