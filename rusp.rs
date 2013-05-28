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
    Rust(~fn(params: ~[Ident], env: &Env) -> Value),
    /// Anonymous function: `(fn (<ident>*) <expr>)`
    Lambda(~[Ident], ~Value),
    /// Function application: `(<expr> <expr>+)`
    Apply(~Value, ~[~Value]),
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
            If(ref t0, ref c0, ref a0) => cmp_other!(If(ref t1, ref c1, ref a1) =>
                (*t0 == *t1) && (*c0 == *c1) && (*a0 == *a1)),
            Def(ref id0, ref exp0) => cmp_other!(Def(ref id1, ref exp1) =>
                (*id0 == *id1) && (*exp0 == *exp1)),
            Do(ref exprs0) => cmp_other!(Do(ref exprs1) => (*exprs0 == *exprs1)),
            Symbol(ref ident0) => cmp_other!(Symbol(ref ident1) => *ident0 == *ident1),
            Str(ref s0) => cmp_other!(Str(ref s1) => *s0 == *s1),
            Quote(ref expr0) => cmp_other!(Quote(ref expr1) => *expr0 == *expr1),
            Rust(_) => cmp_other!(Rust(_) => fail!("Cannot test equality of Rust functions (yet?).")),
            Lambda(ref params0, ref expr0) => cmp_other!(Lambda(ref params1, ref expr1) =>
                (*params0 == *params1) && (*expr0 == *expr1)),
            Apply(ref expr0, ref params0) => cmp_other!(Apply(ref expr1, ref params1) =>
                (*expr0 == *expr1) && (*params0 == *params1)),
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
            If(ref t, ref c, ref a) => If(t.clone(), c.clone(), a.clone()),
            Def(ref ident, ref expr) => Def(ident.clone(), expr.clone()),
            Do(ref exprs) => Do(exprs.clone()),
            Symbol(ref ident) => Symbol(ident.clone()),
            Quote(ref expr) => Quote(expr.clone()),
            Rust(_) => fail!("Cannot test the equality of Rust functions (yet?)"),
            Lambda(ref params, ref expr) => Lambda(params.clone(), expr.clone()),
            Apply(ref expr, ref params) => Apply(expr.clone(), params.clone()),
        }
    }
}

#[deriving(Eq)]
pub struct Env {
    values: HashMap<Ident, Value>,
    outer: Option<@mut Env>,
}

pub type EvalResult = Result<Value, ~str>;

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

    /// Evaluates a Rusp expression in the environment
    pub fn eval(&mut self, value: &Value) -> EvalResult {
        match *value {
            Unit => Ok(Unit),
            Bool(b) => Ok(Bool(b)),
            Int(i) => Ok(Int(i)),
            Float(f) => Ok(Float(f)),
            Str(ref s) => Ok(Str(s.to_owned())),
            Symbol(ref id) =>{
                match self.find(id) {
                    Some(val) => Ok(val),
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
                        Ok(Unit)
                    } else {
                        Err(fmt!("`%s` was already defined in this environment.", id.to_str()))
                    }
                }
            }
            Do(ref exprs) => {
                for uint::range(0, exprs.len() - 1) |i| {
                    match self.eval(exprs[i]) {
                        Ok(Unit) => (),
                        Ok(val) => return Err(fmt!("expected unit expression, found: %s", val.to_str())),
                        Err(err) => return Err(err),
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
    fn test_eval_literal() {
        assert_eq!(Env::empty().eval(&Unit).get(), Unit);
        assert_eq!(Env::empty().eval(&Int(1)).get(), Int(1));
        assert_eq!(Env::empty().eval(&Float(1.0)).get(), Float(1.0));
        assert_eq!(Env::empty().eval(&Str(~"hi")).get(), Str(~"hi"));
        assert_eq!(Env::empty().eval(&Quote(~Unit)).get(), Quote(~Unit));

        let env = Env::empty();
        env.define(~"a", Int(0));
        env.define(~"b", Float(1.0));
        env.define(~"c", Str(~"hi"));

        assert_eq!(env.eval(&Symbol(~"a")).get(), Int(0));
        assert_eq!(env.eval(&Symbol(~"b")).get(), Float(1.0));
        assert_eq!(env.eval(&Symbol(~"c")).get(), Str(~"hi"));
        assert!(env.eval(&Symbol(~"d")).is_err());
    }

    #[test]
    fn test_eval_if() {
        fn not(test: Value) -> EvalResult {
            Env::empty().eval(&If(~test, ~Bool(false), ~Bool(true)))
        }

        assert_eq!(not(Bool(true)).get(), Bool(false));
        assert_eq!(not(Bool(false)).get(), Bool(true));
        assert!(not(Str(~"hi")).is_err());
    }

    #[test]
    fn test_eval_let() {
        let env = Env::empty();
        assert!(env.eval(&Def(~"a", ~Int(0))).is_ok());
        assert!(env.eval(&Def(~"b", ~Float(1.0))).is_ok());
        assert!(env.eval(&Def(~"c", ~Str(~"hi"))).is_ok());

        assert_eq!(env.find(&~"a"), Some(Int(0)));
        assert_eq!(env.find(&~"b"), Some(Float(1.0)));
        assert_eq!(env.find(&~"c"), Some(Str(~"hi")));

        assert!(env.eval(&Def(~"c", ~Unit)).is_err());
    }

    #[test]
    fn test_eval_do() {
        assert_eq!(Env::empty().eval(&Do(~[~Str(~"hi")])).get(), Str(~"hi"));
        assert_eq!(
            Env::empty().eval(&Do(~[
                ~Unit,
                ~Do(~[~Unit]),
                ~Str(~"hi")
            ])).get(),
            Str(~"hi")
        );

        assert!(Env::empty().eval(&Do(~[~Str(~"hi"), ~Unit])).is_err());
    }

    #[test]
    fn test_eval_call() {

    }
}