use std::hashmap::HashMap;
use super::*;


pub fn builtins() -> HashMap<Ident, @Value> {
    let mut h = HashMap::new();

    macro_rules! rust(
        (macro, $name:ident) => (
            @ExternMacro(RustFn(|params, env| $name(params, env)))
        );
        (fn, $name:ident) => (
            @ExternFn(RustFn(|params, env| $name(params, env)))
        );
    );

    // special forms
    h.insert(~"def",                    rust!(macro, builtin_def));
    h.insert(~"if",                     rust!(macro, builtin_if));
    h.insert(~"do",                     rust!(macro, builtin_do));
    h.insert(~"fn",                     rust!(macro, builtin_fn));
    h.insert(~"macro",                  rust!(macro, builtin_macro));
    h.insert(~"quote",                  @Macro(~[~"a"], @Symbol(~"a")));
    h.insert(~"quasiquote",             rust!(macro, builtin_quasiquote));

    // builtin functions

    h.insert(~"eval",                   rust!(fn, builtin_eval));
    h.insert(~"parse",                  rust!(fn, builtin_parse));
    h.insert(~"list",                   rust!(fn, builtin_list));
    h.insert(~"print",                  rust!(fn, builtin_print));

    h.insert(~"bool?",                  rust!(fn, builtin_is_bool));
    h.insert(~"int?",                   rust!(fn, builtin_is_int));
    h.insert(~"float?",                 rust!(fn, builtin_is_float));
    h.insert(~"string?",                rust!(fn, builtin_is_string));
    h.insert(~"list?",                  rust!(fn, builtin_is_list));
    h.insert(~"symbol?",                rust!(fn, builtin_is_symbol));
    h.insert(~"fn?",                    rust!(fn, builtin_is_fn));
    h.insert(~"macro?",                 rust!(fn, builtin_is_macro));
    h.insert(~"extern-fn?",             rust!(fn, builtin_is_extern_fn));
    h.insert(~"extern-macro?",          rust!(fn, builtin_is_extern_macro));

    h.insert(~"+",                      rust!(fn, builtin_add));
    h.insert(~"-",                      rust!(fn, builtin_sub));
    h.insert(~"*",                      rust!(fn, builtin_mul));
    h.insert(~"/",                      rust!(fn, builtin_div));
    h.insert(~"%",                      rust!(fn, builtin_rem));

    h
}

/// Define a new symbol in the environment
fn builtin_def(params: &[@Value], env: @mut Rusp) -> EvalResult {
    match params {
        [@Symbol(ref id), .. rest] => {
            match rest {
                [] => {
                    env.define(id.clone(), @List(~[]));
                    Ok(@List(~[]))
                }
                [ref v] => {
                    env.eval(*v).chain(|res| env.define(id.clone(), res))
                }
                _ => Err(~"`def` expects 1 or 2 arguments")
            }
        }
        _ => Err(~"first argument to `def` should be an identifier")
    }
}

/// Evaluate an S-expression in the current environment
fn builtin_eval(params: &[@Value], env: @mut Rusp) -> EvalResult {
    match params {
        [ref p] => env.eval(*p),
        _ => Err(~"`eval` expects 1 argument")
    }
}

/// Parse S-expressions from a string
fn builtin_parse(params: &[@Value], _: @mut Rusp) -> EvalResult {
    match params {
        [@Str(ref s)] => match parse(*s) {
            Ok(vals) => Ok(@List(vals)),
            Err(parser::ParseFailure { description, _  }) => Err(description)
        },
        _ => Err(~"`parse` expects 1 argument")
    }
}

/// Conditional expression
fn builtin_if(params: &[@Value], env: @mut Rusp) -> EvalResult {
    match params {
        [ref pred, ref conseq, ref alt] => {
            do env.eval(*pred).chain |val| {
                match *val {
                    Bool(true) => env.eval(*conseq),
                    Bool(false) => env.eval(*alt),
                    _ => Err(fmt!("Expected boolean expression, found: %s", val.to_str())),
                }
            }
        }
        _ => Err(~"`if` expects 3 arguments"), // TODO
    }
}

/// Evaluate each expression in turn, returning the value of the last expression
fn builtin_do(params: &[@Value], env: @mut Rusp) -> EvalResult {
    match params {
        [..fst, ref lst] => {
            for fst.each |val| {
                match env.eval(*val) {
                    Ok(@List(ref l)) if l.is_empty() => (),
                    Ok(v) => return Err(fmt!("Expected unit expression, found: %s", v.to_str())),
                    Err(e) => return Err(e),
                }
            }
            env.eval(*lst)
        }
        _ => Err(~""), // TODO
    }
}

/// Return the arguments as a list
fn builtin_list(params: &[@Value], _: @mut Rusp) -> EvalResult {
    Ok(@List(params.to_owned()))
}

/// Print to stdout
fn builtin_print(params: &[@Value], _: @mut Rusp) -> EvalResult {
    let elements = params.map(|v| v.to_str());
    println(str::connect(elements, " "));
    Ok(@List(~[]))
}

/// Evaluate to an anonymous function
fn builtin_fn(params: &[@Value], _: @mut Rusp) -> EvalResult {
    match params {
        [@List(ref symbols), ref val] => {
            let mut idents = ~[];
            for symbols.each |symbol| {
                match **symbol {
                    Symbol(ref ident) => idents.push(ident.clone()),
                    _ => return Err(fmt!("Expected symbol identifier, found: \
                                          %s", symbol.to_str())),
                }
            };
            Ok(@Fn(idents, *val))
        }
        _ => Err(~"`fn` expects 2 arguments"), // TODO
    }
}

/// Evaluate to an anonymous macro
fn builtin_macro(params: &[@Value], _: @mut Rusp) -> EvalResult {
    match params {
        [@List(ref symbols), ref val] => {
            let mut idents = ~[];
            for symbols.each |symbol| {
                match **symbol {
                    Symbol(ref ident) => idents.push(ident.clone()),
                    _ => return Err(fmt!("Expected symbol identifier, found: \
                                          %s", symbol.to_str())),
                }
            };
            Ok(@Macro(idents, *val))
        }
        _ => Err(~"`fn` expects 2 arguments"), // TODO
    }
}

/// Quasiquotation, to allow substitutions inside a quoted structure, e.g.
/// `(def a 1) (quasiquote (a (unquote (list a a a))))`
/// expands to (a (1 1 1))
fn builtin_quasiquote(params: &[@Value], env: @mut Rusp) -> EvalResult {
    return match params {
        [ref p] => unquote(*p, env),
        _ => Err(~"`quasiquote` expects 1 argument")
    };


    // traverse the tree, checking for `(unquote ..)`
    fn unquote(v: @Value, env: @mut Rusp) -> EvalResult {
        match *v {
            List([@Symbol(~"unquote"), .. rest]) => {
                match rest {
                    [ref p] => env.eval(*p),
                    _ => Err(~"`unquote` expects 1 argument")
                }
            }
            List(ref vals) => {
                let mut new = ~[];

                for vals.each |v| {
                    match unquote(*v, env) {
                        Ok(v) => new.push(v),
                        Err(e) => return Err(e)
                    }
                }
                Ok(@List(new))
            }
            Fn(ref args, ref body) => {
                match unquote(*body, env) {
                    Ok(b) => Ok(@Fn(args.clone(), b)),
                    Err(e) => Err(e)
                }
            }
            Macro(ref args, ref body) => {
                match unquote(*body, env) {
                    Ok(b) => Ok(@Macro(args.clone(), b)),
                    Err(e) => Err(e)
                }
            }
            _ => Ok(v)
        }
    }
}

macro_rules! type_pred(
    ($name:ident, $pattern:pat) => (
        fn $name(params: &[@Value], _: @mut Rusp) -> EvalResult {
            match do params.map |&val| {
                match *val {
                    $pattern => @Bool(true),
                    _ => @Bool(false),
                }
            } {
                [ref result] => Ok(*result),
                results => Ok(@List(results)),
            }
        }
    )
)

type_pred!(builtin_is_bool, Bool(*))
type_pred!(builtin_is_int, Int(*))
type_pred!(builtin_is_float, Float(*))
type_pred!(builtin_is_string, Str(*))
type_pred!(builtin_is_list, List(*))
type_pred!(builtin_is_symbol, Symbol(*))
type_pred!(builtin_is_fn, Fn(*))
type_pred!(builtin_is_macro, Macro(*))
type_pred!(builtin_is_extern_fn, ExternFn(*))
type_pred!(builtin_is_extern_macro, ExternMacro(*))

macro_rules! arithmetic_op(
    ($name:ident, $symbol:expr, $method:ident) => (
        fn $name(params: &[@Value], env: @mut Rusp) -> EvalResult {
            if params.len() < 2 {
                return Err(fmt!("`%s` expects at least 2 arguments", $symbol));
            }
            let mut result = *params.head();
            for params.tail().each |&val| {
                match (result, env.eval(val)) {
                    // Basic operations between values of the same type
                    (@Int(prev), Ok(@Int(i))) => result = @Int(prev.$method(&i)),
                    (@Float(prev), Ok(@Float(f))) => result = @Float(prev.$method(&f)),
                    // Handle numeric promotions
                    (@Int(prev), Ok(@Float(f))) => result = @Float((prev as float).$method(&f)),
                    (@Float(prev), Ok(@Int(i))) => result = @Float(prev.$method(&(i as float))),
                    // Type errors
                    (@Float(_), Ok(ref v)) => return Err(fmt!("Expected float but found: %s", v.to_str())),
                    (@Int(_), Ok(ref v)) => return Err(fmt!("Expected integer but found: %s", v.to_str())),
                    (ref prev, Ok(_)) => return Err(fmt!("Expected number but found: %s", prev.to_str())),
                    (_, Err(err)) => return Err(err),
                }
            }
            Ok(result)
        }
    )
)

arithmetic_op!(builtin_add, "+", add)
arithmetic_op!(builtin_sub, "-", sub)
arithmetic_op!(builtin_mul, "*", mul)
arithmetic_op!(builtin_div, "/", div)
arithmetic_op!(builtin_rem, "%", rem)

#[cfg(test)]
mod tests {
    use super::super::*;
    use super::super::parser::Parser;

    macro_rules! test_ok(
        ($src:expr, $expected:expr) => (
            assert_eq!(Rusp::new().eval(
                @Parser::new($src).parse().get()
            ).get().to_str(), $expected);
        )
    )

    macro_rules! test_err(
        ($src:expr) => (
            assert!(Rusp::new().eval(
                @Parser::new($src).parse().get()
            ).is_err());
        )
    )

    #[test]
    fn test_type_preds() {
        test_ok!("(bool? true)", ~"true");
        test_ok!("(bool? 1)", ~"false");
        test_ok!("(int? 1)", ~"true");
        test_ok!("(int? 1.0)", ~"false");
        test_ok!("(float? 1.0)", ~"true");
        test_ok!("(float? \"hi\")", ~"false");
        test_ok!("(string? \"hi\")", ~"true");
        test_ok!("(string? (list 1 2))", ~"false");
        test_ok!("(list? (list 1 2))", ~"true");
        test_ok!("(list? (list))", ~"true");
        test_ok!("(list? 1.0)", ~"false");
        test_ok!("(symbol? (quote x))", ~"true");
        test_ok!("(symbol? (fn () 1))", ~"false");
        test_ok!("(fn? (fn () 1))", ~"true");
        test_ok!("(fn? (macro () 1))", ~"false");
        test_ok!("(macro? (macro () 1))", ~"true");
        test_ok!("(macro? false)", ~"false");
    }

    #[test]
    fn test_arithmetic() {
        test_ok!("(+ 1 2)", ~"3");
        test_ok!("(+ 1 2 4)", ~"7");
        test_ok!("(+ 1.5 3 4)", ~"8.5");
        test_ok!("(+ 3 1.67)", ~"4.67");
        test_ok!("(- 3 0.5)", ~"2.5");
        test_ok!("(- 3 1.5 (+ 2 4))", ~"-4.5");

        test_err!("(+)");
        test_err!("(-)");
        test_err!("(*)");
        test_err!("(/)");
        test_err!("(%)");

        test_err!("(+ 1)");
        test_err!("(- 1)");
        test_err!("(* 1)");
        test_err!("(/ 1)");
        test_err!("(% 1)");

        test_err!("(+ 1 \"hi\")");
        test_err!("(- 1 \"hi\")");
        test_err!("(* 1 \"hi\")");
        test_err!("(/ 1 \"hi\")");
        test_err!("(% 1 \"hi\")");

        test_err!("(+ \"hi\" 1)");
        test_err!("(- \"hi\" 1)");
        test_err!("(* \"hi\" 1)");
        test_err!("(/ \"hi\" 1)");
        test_err!("(% \"hi\" 1)");
    }
}