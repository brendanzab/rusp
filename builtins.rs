use std::hashmap::HashMap;
use super::*;


pub fn builtins() -> HashMap<Ident, Value> {
    let mut h = HashMap::new();

    macro_rules! rust_fn ( ($name:ident, $special:ident) => (
        Rust(RustFn(|params, env| $name(params, env)), $special)
    ));

    // special forms
    h.insert(~"def", rust_fn!(builtin_def, true));
    h.insert(~"if",  rust_fn!(builtin_if, true));
    h.insert(~"do", rust_fn!(builtin_do, true));
    h.insert(~"fn", rust_fn!(builtin_fn, true));
    h.insert(~"macro", rust_fn!(builtin_macro, true));
    // Return an expression without evaluating it
    h.insert(~"quote", Lambda(~[~"a"], ~Symbol(~"a"), true));
    h.insert(~"quasiquote", rust_fn!(builtin_quasiquote, true));

    // builtin functions
    h.insert(~"eval", rust_fn!(builtin_eval, false));
    h.insert(~"parse", rust_fn!(builtin_parse, false));
    h.insert(~"list", rust_fn!(builtin_list, false));
    h.insert(~"print", rust_fn!(builtin_print, false));

    h
}

/// Define a new symbol in the environment
fn builtin_def(params: &[~Value], env: @mut Rusp) -> EvalResult {
    match params {
        [~Symbol(ref id), .. rest] => {
            match rest {
                [] => {
                    env.define(id.clone(), List(~[]));
                    Ok(List(~[]))
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
fn builtin_eval(params: &[~Value], env: @mut Rusp) -> EvalResult {
    match params {
        [ref p] => env.eval(*p),
        _ => Err(~"`eval` expects 1 argument")
    }
}

/// Parse S-expressions from a string
fn builtin_parse(params: &[~Value], _: @mut Rusp) -> EvalResult {
    match params {
        [~Str(ref s)] => match parse(*s) {
            Ok(v) => Ok(v),
            Err(parser::ParseFailure { description, _  }) => Err(description)
        },
        _ => Err(~"`parse` expects 1 argument")
    }
}

/// Conditional expression
fn builtin_if(params: &[~Value], env: @mut Rusp) -> EvalResult {
    match params {
        [ref pred, ref conseq, ref alt] => {
            do env.eval(*pred).chain |val| {
                match val {
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
fn builtin_do(params: &[~Value], env: @mut Rusp) -> EvalResult {
    match params {
        [..fst, ref lst] => {
            for fst.each |val| {
                match env.eval(*val) {
                    Ok(List(ref l)) if l.is_empty() => (),
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
fn builtin_list(params: &[~Value], _env: @mut Rusp) -> EvalResult {
    Ok(List(params.to_owned()))
}

/// Print to stdout
fn builtin_print(params: &[~Value], _env: @mut Rusp) -> EvalResult {
    let elements = params.map(|v| v.to_str());
    println(str::connect(elements, " "));
    Ok(List(~[]))
}

/// Common code between functions and macros
fn builtin_fnmacro(params: &[~Value], no_eval: bool) -> EvalResult {
    match params {
        [~List(ref symbols), ref val] => {
            let mut idents = ~[];
            for symbols.each |symbol| {
                match **symbol {
                    Symbol(ref ident) => idents.push(ident.clone()),
                    _ => return Err(fmt!("Expected symbol identifier, found: \
                                          %s", symbol.to_str())),
                }
            };
            Ok(Lambda(idents, val.clone(), no_eval))
        }
        _ => Err(~"`fn` expects 2 arguments"), // TODO
    }
}

/// Evaluate to an anonymous function
fn builtin_fn(params: &[~Value], _: @mut Rusp) -> EvalResult {
    builtin_fnmacro(params, false)
}

/// Evaluate to an anonymous macro
fn builtin_macro(params: &[~Value], _: @mut Rusp) -> EvalResult {
    builtin_fnmacro(params, true)
}

/// Quasiquotation, to allow substitutions inside a quoted structure, e.g.
/// `(def a 1) (quasiquote (a (unquote (list a a a))))`
/// expands to (a (1 1 1))
fn builtin_quasiquote(params: &[~Value], env: @mut Rusp) -> EvalResult {
    return match params {
        [ref p] => unquote(*p, env),
        _ => Err(~"`quasiquote` expects 1 argument")
    };


    // traverse the tree, checking for `(unquote ..)`
    fn unquote(v: &Value, env: @mut Rusp) -> EvalResult {
        match *v {
            List([~Symbol(~"unquote"), .. rest]) => {
                match rest {
                    [ref p] => env.eval(*p),
                    _ => Err(~"`unquote` expects 1 argument")
                }
            }
            List(ref vals) => {
                let mut new = ~[];

                for vals.each |v| {
                    match unquote(*v, env) {
                        Ok(v) => new.push(~v),
                        Err(e) => return Err(e)
                    }
                }
                Ok(List(new))
            }
            Lambda(ref args, ref body, special) => {
                match unquote(*body, env) {
                    Ok(b) => Ok(Lambda(args.clone(), ~b, special)),
                    Err(e) => Err(e)
                }
            }
            _ => Ok(v.clone())
        }
    }
}