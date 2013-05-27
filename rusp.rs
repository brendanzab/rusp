#[link(name = "rusp",
       vers = "0.1",
       author = "Brendan Zabarauskas",
       url = "https://github.com/bjz/rusp")];

#[comment = "A minimal Lisp dialect for Rust."];
#[crate_type = "lib"];

///
/// Symbol identifier
///
#[deriving(Eq)]
struct Ident<'self>(&'self str);

///
/// Holds a typed value from the environment
///
#[deriving(Eq)]
enum Value<'self> {
    Str(&'self str),
    Bool(bool),
    Int(int),
    Float(float),
}

///
/// Primitive language expressions
///
/// The bottom turtles.
///
#[deriving(Eq)]
pub enum Expr<'self> {
    ///
    /// Symbol reference
    ///
    /// ~~~
    /// <ident>
    /// ~~~
    ///
    Symbol(
        Ident<'self>
    ),
    ///
    /// Constant literal
    ///
    /// ~~~
    /// <val>
    /// ~~~
    ///
    Literal(
        Value<'self>
    ),
    ///
    /// Quotation
    ///
    /// ~~~
    /// (quote <expr>)
    /// ~~~
    ///
    Quote(
        &'self Expr<'self>
    ),
    ///
    /// Conditional expression
    ///
    /// ~~~
    /// (if <test> <conseq> <alt>)
    /// ~~~
    ///
    If(
        &'self Expr<'self>,
        &'self Expr<'self>,
        &'self Expr<'self>
    ),
    ///
    /// Anonymous function
    ///
    /// ~~~
    /// (|<ident>*| <expr>)
    /// ~~~
    ///
    Lambda(
        &'self [Ident<'self>],
        &'self Expr<'self>
    ),
    ///
    /// Symbol definition
    ///
    /// ~~~
    /// (def <ident> <expr>)
    /// ~~~
    ///
    Define(
        Ident<'self>,
        &'self Expr<'self>
    ),
    ///
    /// Procedure call
    ///
    /// ~~~
    /// (<ident> <expr>+)
    /// ~~~
    ///
    Call(
        Ident<'self>,
        &'self [&'self Expr<'self>]
    ),
}