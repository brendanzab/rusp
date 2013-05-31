//! Built-in functions and macros.

use super::*;


pub fn add_builtins(env: @mut Rusp) {
    macro_rules! rust(
        (NoEnv: $ty:ident, $name:ident) => (
            @FnEnv(ExternFn(|params, env| $name(params, env), $ty), None)
        );
        ($ty:ident, $name:ident) => (
            @FnEnv(ExternFn(|params, env| $name(params, env), $ty), Some(env))
        );
    );

    for [
        // special forms

        // most of these need to capture the environment where they
        // are called (e.g. to create a lexically-scoped closure, or
        // to unquote local variables), *not* the global environment,
        // hence NoEnv.
        (~"fn",                     rust!(NoEnv: Macro, builtin_fn)),
        (~"macro",                  rust!(NoEnv: Macro, builtin_macro)),

        (~"if",                     rust!(NoEnv: Macro, builtin_if)),
        (~"do",                     rust!(NoEnv: Macro, builtin_do)),
        (~"quote",                  @FnEnv(RuspFn(~[~"a"], @Symbol(~"a"), Macro), None)),
        (~"quasiquote",             rust!(NoEnv: Macro, builtin_quasiquote)),

        // this changes the global scope always
        (~"def",                    rust!(Macro, builtin_def)),


        // builtin functions
        (~"eval",                   rust!(Fn, builtin_eval)),
        (~"parse",                  rust!(Fn, builtin_parse)),
        (~"list",                   rust!(Fn, builtin_list)),
        (~"print",                  rust!(Fn, builtin_print)),

        (~"bool?",                  rust!(Fn, builtin_is_bool)),
        (~"int?",                   rust!(Fn, builtin_is_int)),
        (~"float?",                 rust!(Fn, builtin_is_float)),
        (~"string?",                rust!(Fn, builtin_is_string)),
        (~"list?",                  rust!(Fn, builtin_is_list)),
        (~"symbol?",                rust!(Fn, builtin_is_symbol)),
        (~"fn?",                    rust!(Fn, builtin_is_fn)),
        (~"macro?",                 rust!(Fn, builtin_is_macro)),
        (~"extern-fn?",             rust!(Fn, builtin_is_extern_fn)),
        (~"extern-macro?",          rust!(Fn, builtin_is_extern_macro)),

        (~"+",                      rust!(Fn, builtin_add)),
        (~"-",                      rust!(Fn, builtin_sub)),
        (~"*",                      rust!(Fn, builtin_mul)),
        (~"/",                      rust!(Fn, builtin_div)),
        (~"%",                      rust!(Fn, builtin_rem)),
        (~"=",                      rust!(Fn, builtin_eq)),
    ].each |&(name, func)| {
        env.define(name, func);
    }
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

/// Shared functionality between creating functions and macros
fn fn_macro(params: &[@Value], callee_env: @mut Rusp, ty: FnMode) -> EvalResult {
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
            Ok(@FnEnv(RuspFn(idents, *val, ty), Some(callee_env)))
        }
        _ => Err(fmt!("`%s` expects 2 arguments", ty.to_str())), // TODO
    }
}

/// Evaluate to an anonymous function
fn builtin_fn(params: &[@Value], callee_env: @mut Rusp) -> EvalResult {
    fn_macro(params, callee_env, Fn)
}

/// Evaluate to an anonymous macro
fn builtin_macro(params: &[@Value], callee_env: @mut Rusp) -> EvalResult {
    fn_macro(params, callee_env, Macro)
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
            FnEnv(RuspFn(ref args, ref body, ref fn_type), ref subenv) => {
                match unquote(*body, subenv.get_or_default(env)) {
                    Ok(b) => Ok(@FnEnv(RuspFn(args.clone(), b, fn_type.clone()),
                                       subenv.clone())),
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
type_pred!(builtin_is_fn, FnEnv(RuspFn(_, _, Fn), _))
type_pred!(builtin_is_macro, FnEnv(RuspFn(_, _, Macro), _))
type_pred!(builtin_is_extern_fn, FnEnv(ExternFn(_, Fn), _))
type_pred!(builtin_is_extern_macro, FnEnv(ExternFn(_, Macro), _))

macro_rules! arithmetic_op(
    ($name:ident, $symbol:expr, $method:ident) => (
        fn $name(params: &[@Value], _: @mut Rusp) -> EvalResult {
            if params.len() < 2 {
                return Err(fmt!("`%s` expects at least 2 arguments", $symbol));
            }
            let mut result = *params.head();
            for params.tail().each |&val| {
                match (result, val) {
                    // Basic operations between values of the same type
                    (@Int(prev), @Int(i)) => result = @Int(prev.$method(&i)),
                    (@Float(prev), @Float(f)) => result = @Float(prev.$method(&f)),
                    // Handle numeric promotions
                    (@Int(prev), @Float(f)) => result = @Float((prev as float).$method(&f)),
                    (@Float(prev), @Int(i)) => result = @Float(prev.$method(&(i as float))),
                    // Type errors
                    (@Float(_), ref v) => return Err(fmt!("Expected float but found: %s", v.to_str())),
                    (@Int(_), ref v) => return Err(fmt!("Expected integer but found: %s", v.to_str())),
                    (ref prev, _) => return Err(fmt!("Expected number but found: %s", prev.to_str())),
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

fn builtin_eq(params: &[@Value], _: @mut Rusp) -> EvalResult {
    match params {
        [head, .. rest] => {
            for rest.each |&val| {
                // TODO handle numeric promotions? i.e. 1.0 == 1?
                if !(val == head) { return Ok(@Bool(false)); }
            }
            Ok(@Bool(true))
        }
        [] => Err(fmt!("`=` expects at least 1 argument"))
    }
}

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