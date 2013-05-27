#[link(name = "rusp",
       vers = "0.1",
       author = "Brendan Zabarauskas",
       url = "https://github.com/bjz/rusp")];

#[comment = "A minimal Lisp dialect for Rust."];
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
    Empty,
    Boolean(bool),
    Integer(int),
    Float(float),
    String(~str),
    ///
    /// Quoted expression
    ///
    /// ~~~
    /// (quote <expr>)
    /// ~~~
    ///
    Quote(@Expr),
    ///
    /// Anonymous function
    ///
    /// ~~~
    /// (fn (<ident>*) <expr>)
    /// ~~~
    ///
    Lambda(~[Ident], @Expr, @mut Env),
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
    If(@Expr, @Expr, @Expr),
    ///
    /// Symbol definition
    ///
    /// ~~~
    /// (let <ident> <expr>)
    /// ~~~
    ///
    Let(Ident, @Expr),
    ///
    /// Procedure call
    ///
    /// ~~~
    /// (<expr> <expr>+)
    /// ~~~
    ///
    Call(@Expr, ~[@Expr]),
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
            If(test, conseq, alt) => {
                do test.eval(env).chain |val| {
                    match val {
                        Boolean(true) => conseq.eval(env),
                        Boolean(false) => alt.eval(env),
                        _ => Err(fmt!("expected Boolean value, found: %?", val)),
                    }
                }
            }
            Let(ref id, expr) => {
                do expr.eval(env).chain |val| {
                    if env.define(id.clone(), val) {
                        Ok(Empty)
                    } else {
                        Err(fmt!("`%s` was already defined in this environment.", id.to_str()))
                    }
                }
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
            (Ident(~"a"), Integer(0)),
            (Ident(~"b"), Float(1.0)),
            (Ident(~"c"), String(~"hi")),
        ], None);

        let inner = Env::new([
            (Ident(~"a"), Integer(3)),
            (Ident(~"b"), Integer(4)),
        ], Some(outer));

        assert_eq!(outer.find(&Ident(~"a")), Some(Integer(0)));
        assert_eq!(outer.find(&Ident(~"b")), Some(Float(1.0)));
        assert_eq!(outer.find(&Ident(~"c")), Some(String(~"hi")));
        assert_eq!(inner.find(&Ident(~"a")), Some(Integer(3)));
        assert_eq!(inner.find(&Ident(~"b")), Some(Integer(4)));
        assert_eq!(inner.find(&Ident(~"c")), Some(String(~"hi")));
    }

    #[test]
    fn test_eval_symbol() {
        let env = Env::new([
            (Ident(~"a"), Integer(0)),
            (Ident(~"b"), Float(1.0)),
            (Ident(~"c"), String(~"hi")),
        ], None);

        assert_eq!(Symbol(Ident(~"a")).eval(env).get(), Integer(0));
        assert_eq!(Symbol(Ident(~"b")).eval(env).get(), Float(1.0));
        assert_eq!(Symbol(Ident(~"c")).eval(env).get(), String(~"hi"));
        assert!(Symbol(Ident(~"d")).eval(env).is_err());
    }

    #[test]
    fn test_eval_literal() {
        assert_eq!(Literal(Empty).eval(Env::empty()).get(), Empty);
        assert_eq!(Literal(Integer(1)).eval(Env::empty()).get(), Integer(1));
        assert_eq!(Literal(Float(1.0)).eval(Env::empty()).get(), Float(1.0));
        assert_eq!(Literal(String(~"hi")).eval(Env::empty()).get(), String(~"hi"));
        assert_eq!(Literal(Quote(@Literal(Empty))).eval(Env::empty()).get(),
                   Quote(@Literal(Empty)));
    }

    #[test]
    fn test_eval_if() {
        fn not(test: Value) -> EvalResult {
            If(
                @Literal(test),
                @Literal(Boolean(false)),
                @Literal(Boolean(true))
            ).eval(Env::empty())
        }

        assert_eq!(not(Boolean(true)).get(), Boolean(false));
        assert_eq!(not(Boolean(false)).get(), Boolean(true));
        assert!(not(String(~"hi")).is_err());
    }

    #[test]
    fn test_eval_lambda() {

    }

    #[test]
    fn test_eval_let() {
        let env = Env::empty();
        assert!(Let(Ident(~"a"), @Literal(Integer(0))).eval(env).is_ok());
        assert!(Let(Ident(~"b"), @Literal(Float(1.0))).eval(env).is_ok());
        assert!(Let(Ident(~"c"), @Literal(String(~"hi"))).eval(env).is_ok());

        assert_eq!(env.find(&Ident(~"a")), Some(Integer(0)));
        assert_eq!(env.find(&Ident(~"b")), Some(Float(1.0)));
        assert_eq!(env.find(&Ident(~"c")), Some(String(~"hi")));

        assert!(Let(Ident(~"c"), @Literal(Empty)).eval(env).is_err());
    }

    #[test]
    fn test_eval_call() {

    }
}