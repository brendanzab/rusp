#[link(name = "rusp",
       vers = "0.1",
       author = "Brendan Zabarauskas",
       url = "https://github.com/bjz/rusp")];

#[comment = "A minimal Lisp dialect for Rust."];
#[crate_type = "lib"];

use std::hashmap::*;

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
    String(~str),
    Bool(bool),
    Int(int),
    Float(float),
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
    /// Quotation
    ///
    /// ~~~
    /// (quote <expr>)
    /// ~~~
    ///
    Quote(@Expr),
    ///
    /// Conditional expression
    ///
    /// ~~~
    /// (if <test> <conseq> <alt>)
    /// ~~~
    ///
    If(@Expr, @Expr, @Expr),
    ///
    /// Anonymous function
    ///
    /// ~~~
    /// (|<ident>*| <expr>)
    /// ~~~
    ///
    Lambda(~[Ident], @Expr),
    ///
    /// Symbol definition
    ///
    /// ~~~
    /// (def <ident> <expr>)
    /// ~~~
    ///
    Define(Ident, @Expr),
    ///
    /// Procedure call
    ///
    /// ~~~
    /// (<ident> <expr>+)
    /// ~~~
    ///
    Call(Ident, ~[Expr]),

#[deriving(Eq)]
pub struct Env {
    values: HashMap<Ident, Value>,
    outer: Option<@mut Env>,
}

impl Env {
    /// Initializes an empty environment
    pub fn empty() -> Env {
        Env { values: HashMap::new(), outer: None }
    }

    /// Initializes a new environment
    pub fn new(values: &[(Ident, Value)], outer: Option<@mut Env>) -> @mut Env {
        let env = @mut Env {
            values: HashMap::new(),
            outer: outer,
        };
        for values.each |&(id, val)| {
            env.insert(id, val);
        }
        env
    }

    /// Inserts a new identifier/value pair into the environment
    pub fn insert(&mut self, ident: Ident, val: Value) -> bool {
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

///
/// A position in a source string
///
pub struct Position {
    line: uint,
    col: uint,
}

pub enum ParseResult {
    Success(Expr),
    Failure {
        description: ~str,
        position: Position,
    },
}

///
/// Performs a recursive decent parse of the source string.
///
pub fn parse(src: &str) -> ParseResult {
    for str::each_char(src) |_| {}
    fail!()
}

impl Expr {
    ///
    /// Evaluates an expression tree
    ///
    pub fn eval(&self, env: &Env) -> Value {
        match *self {
            Symbol(ref id) => env.find(id).expect(
                fmt!("The value of %? was not defined in this environment", id)
            ),
            Literal(ref val) => val.clone(),
            Quote(_) => fail!(),
            If(test, conseq, alt) => {
                match test.eval(env) {
                    Bool(val) => {
                        if val { conseq.eval(env) }
                        else   { alt.eval(env)    }
                    }
                    expr => {
                        fail!("expected Boolean expression, found: %?", expr);
                    }
                }
            }
            Lambda(_,_) => fail!(),
            Define(_,_) => fail!(),
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
            (Ident(~"c"), String(~"hi")),
        ], None);

        let inner = Env::new([
            (Ident(~"a"), Int(3)),
            (Ident(~"b"), Int(4)),
        ], Some(outer));

        assert_eq!(outer.find(&Ident(~"a")), Some(Int(0)));
        assert_eq!(outer.find(&Ident(~"b")), Some(Float(1.0)));
        assert_eq!(outer.find(&Ident(~"c")), Some(String(~"hi")));
        assert_eq!(inner.find(&Ident(~"a")), Some(Int(3)));
        assert_eq!(inner.find(&Ident(~"b")), Some(Int(4)));
        assert_eq!(inner.find(&Ident(~"c")), Some(String(~"hi")));
    }

    #[test]
    fn test_eval_if() {
        fn not(test: bool) -> bool {
            match If(
                @Literal(Bool(test)),
                @Literal(Bool(false)),
                @Literal(Bool(true))
            ).eval(&Env::empty()) {
                Bool(b) => b,
                _ => fail!("Should not fail."),
            }
        }

        assert_eq!(not(true), false);
        assert_eq!(not(false), true);
    }
}