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
    h.insert(~"def", rust!(macro, builtin_def));
    h.insert(~"if",  rust!(macro, builtin_if));
    h.insert(~"do", rust!(macro, builtin_do));
    h.insert(~"fn", rust!(macro, builtin_fn));
    h.insert(~"macro", rust!(macro, builtin_macro));
    // Return an expression without evaluating it
    h.insert(~"quote", @Macro(~[~"a"], @Symbol(~"a")));
    h.insert(~"quasiquote", rust!(macro, builtin_quasiquote));

    // builtin functions
    h.insert(~"eval", rust!(fn, builtin_eval));
    h.insert(~"parse", rust!(fn, builtin_parse));
    h.insert(~"list", rust!(fn, builtin_list));
    h.insert(~"print", rust!(fn, builtin_print));

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
fn builtin_list(params: &[@Value], _env: @mut Rusp) -> EvalResult {
    Ok(@List(params.to_owned()))
}

/// Print to stdout
fn builtin_print(params: &[@Value], _env: @mut Rusp) -> EvalResult {
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