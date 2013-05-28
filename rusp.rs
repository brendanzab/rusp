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

/// Symbol identifier
pub type Ident = ~str;

/// Holds a typed value
pub enum Value {
    /// The unit type: `()`
    Unit,
    /// A boolean value: either `true` or `false`
    Bool(bool),
    /// An integer value
    Int(int),
    /// A floating point value
    Float(float),
    /// String value: `"<value>"`
    Str(~str),
    /// Quoted expression: `(quote <expr>)`
    Quote(~Expr),
    /// A Rust function
    Rust(~fn(params: ~[Ident], env: &Env) -> Value),
    /// Anonymous function: `(fn (<ident>*) <expr>)`
    Fn(~[Ident], ~Expr),
}

impl Eq for Value {
    fn eq(&self, other: &Value) -> bool {
        macro_rules! cmp_other(
            ($cmp_pat:pat => $cmp_expr:expr) => (
                match *other { $cmp_pat => $cmp_expr, _ => false, }
            )
        );
        match *self {
            Unit => cmp_other!(Unit => true),
            Bool(b0) => cmp_other!(Bool(b1) => b0 == b1),
            Int(i0) => cmp_other!(Int(i1) => i0 == i1),
            Float(f0) => cmp_other!(Float(f1) => f0 == f1),
            Str(ref s0) => cmp_other!(Str(ref s1) => *s0 == *s1),
            Quote(ref expr0) => cmp_other!(Quote(ref expr1) => *expr0 == *expr1),
            Rust(_) => cmp_other!(Rust(_) => fail!("Cannot test equality of Rust functions (yet?).")),
            Fn(ref params0, ref expr0) => cmp_other!(Fn(ref params1, ref expr1) => {
                (*params0 == *params1) && (*expr0 == *expr1)
            }),
        }
    }
    fn ne(&self, other: &Value) -> bool { !self.eq(other) }
}

impl Clone for Value {
    fn clone(&self) -> Value {
        match *self {
            Unit => Unit,
            Bool(b) => Bool(b),
            Int(i) => Int(i),
            Float(f) => Float(f),
            Str(ref s) => Str(s.clone()),
            Quote(ref expr) => Quote(expr.clone()),
            Rust(_) => fail!("Cannot test the equality of Rust functions (yet?)"),
            Fn(ref params, ref expr) => Fn(params.clone(), expr.clone()),
        }
    }
}

///
/// Primitive language expressions
///
#[deriving(Eq, Clone)]
pub enum Expr {
    /// Symbol reference: `<ident>`
    Symbol(Ident),
    /// Constant literal: `<val>`
    Literal(Value),
    /// Conditional expression: `(if <test> <conseq> <alt>)`
    If(~Expr, ~Expr, ~Expr),
    /// Symbol definition: `(let <ident> <expr>)`
    Let(Ident, ~Expr),
    /// Do expression: `(do <expr>+)`
    Do(~[~Expr]),
    /// Procedure call: `(<expr> <expr>+)`
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
        @mut Env {
            values: HashMap::new(),
            outer: None,
        }
    }

    /// Initializes a new environment with the specified values
    pub fn new(outer: @mut Env) -> @mut Env {
        @mut Env {
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
}

pub type EvalResult = Result<Value, ~str>;

impl Expr {
    /// Evaluates a Rusp expression
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
                        Ok(Unit)
                    } else {
                        Err(fmt!("`%s` was already defined in this environment.", id.to_str()))
                    }
                }
            }
            Do(ref exprs) => {
                for uint::range(0, exprs.len() - 1) |i| {
                    match exprs[i].eval(env) {
                        Ok(Unit) => (),
                        Ok(val) => return Err(fmt!("expected unit expression, found: %s", val.to_str())),
                        Err(err) => return Err(err),
                    }
                }
                exprs[exprs.len() - 1].eval(env)
            }
            Call(_,_) => fail!("Not yet implemented"),
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_env() {
        let outer = Env::empty();
        outer.define(~"a", Int(0));
        outer.define(~"b", Float(1.0));
        outer.define(~"c", Str(~"hi"));

        let inner = Env::new(outer);
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
    fn test_eval_symbol() {
        let env = Env::empty();
        env.define(~"a", Int(0));
        env.define(~"b", Float(1.0));
        env.define(~"c", Str(~"hi"));

        assert_eq!(Symbol(~"a").eval(env).get(), Int(0));
        assert_eq!(Symbol(~"b").eval(env).get(), Float(1.0));
        assert_eq!(Symbol(~"c").eval(env).get(), Str(~"hi"));
        assert!(Symbol(~"d").eval(env).is_err());
    }

    #[test]
    fn test_eval_literal() {
        assert_eq!(Literal(Unit).eval(Env::empty()).get(), Unit);
        assert_eq!(Literal(Int(1)).eval(Env::empty()).get(), Int(1));
        assert_eq!(Literal(Float(1.0)).eval(Env::empty()).get(), Float(1.0));
        assert_eq!(Literal(Str(~"hi")).eval(Env::empty()).get(), Str(~"hi"));
        assert_eq!(Literal(Quote(~Literal(Unit))).eval(Env::empty()).get(),
                   Quote(~Literal(Unit)));
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
        assert!(Let(~"a", ~Literal(Int(0))).eval(env).is_ok());
        assert!(Let(~"b", ~Literal(Float(1.0))).eval(env).is_ok());
        assert!(Let(~"c", ~Literal(Str(~"hi"))).eval(env).is_ok());

        assert_eq!(env.find(&~"a"), Some(Int(0)));
        assert_eq!(env.find(&~"b"), Some(Float(1.0)));
        assert_eq!(env.find(&~"c"), Some(Str(~"hi")));

        assert!(Let(~"c", ~Literal(Unit)).eval(env).is_err());
    }

    #[test]
    fn test_eval_do() {
        assert_eq!(Do(~[~Literal(Str(~"hi"))]).eval(Env::empty()).get(), Str(~"hi"));
        assert_eq!(
            Do(~[
                ~Literal(Unit),
                ~Do(~[~Literal(Unit)]),
                ~Literal(Str(~"hi"))
            ]).eval(Env::empty()).get(),
            Str(~"hi")
        );

        assert!(Do(~[~Literal(Str(~"hi")),~Literal(Unit)]).eval(Env::empty()).is_err());
    }

    #[test]
    fn test_eval_call() {

    }
}