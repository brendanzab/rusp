#[link(name = "rusp",
       vers = "0.1",
       author = "Brendan Zabarauskas",
       url = "https://github.com/bjz/rusp")];

#[comment = "A minimal Lisp dialect for Rust."];
#[crate_type = "lib"];

///
/// Symbol identifier
///
#[deriving(Eq, Clone)]
pub struct Ident(~str);

///
/// Holds a typed value from the environment
///
#[deriving(Eq, Clone)]
pub enum Value {
    Str(~str),
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
    pub fn eval(&self) -> Expr {
        match *self {
            Symbol(_) => fail!(),
            Literal(ref val) => Literal(val.clone()),
            Quote(_) => fail!(),
            If(test, conseq, alt) => {
                match test.eval() {
                    Literal(Bool(val)) => {
                        if val { conseq.eval() }
                        else { alt.eval() }
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
    fn test_eval_if() {
        fn not(test: bool) -> bool {
            match If(
                @Literal(Bool(test)),
                @Literal(Bool(false)),
                @Literal(Bool(true))
            ).eval() {
                Literal(Bool(b)) => b,
                _ => fail!("Should not fail."),
            }
        }

        assert_eq!(not(true), false);
        assert_eq!(not(false), true);
    }
}