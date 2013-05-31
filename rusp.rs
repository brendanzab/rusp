// Copyright 2013 The Rusp Developers. For a full listing of the authors,
// refer to the AUTHORS file at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#[link(name = "rusp",
       vers = "0.1",
       url = "https://github.com/bjz/rusp")];

#[comment = "A minimal Lisp dialect for Rust."];
#[license = "ASL2"];
#[crate_type = "lib"];

// Inspired by Peter Norvig's [Lis.py](http://norvig.com/lispy.html)

use std::hashmap::*;

pub mod builtins;
pub mod parser;
pub mod pprint;

///
/// Performs a recursive decent parse of the source string.
///
pub fn parse(src: &str) -> Result<~[@Value], parser::ParseFailure> {
    let mut parsed = ~[];
    let mut p = parser::Parser::new(src);

    while !p.is_eof() {
        match p.parse() {
            Ok(v) => parsed.push(@v),
            Err(e) => return Err(e)
        }
    }

    Ok(parsed)
}

/// Symbol identifier
pub type Ident = ~str;

/// Holds a typed value
#[deriving(Eq, Clone)]
pub enum Value {
    Bool(bool),
    Int(int),
    Float(float),
    Str(~str),
    List(~[@Value]),
    Symbol(Ident),
    /// A function and the environment it should be called in; The
    /// environment is optional, since Some(env) gives lexical
    /// scoping, None gives dynamic scoping since callee substitute
    /// its own environment (e.g. to create a closure).
    FnEnv(Fn, Option<@mut Rusp>)
}


/// Flags the mode with which functions are called, `Fn` if arguments
/// are evaluated, `Macro` if not
#[deriving(Eq, Clone)]
pub enum FnMode { Fn, Macro }

impl ToStr for FnMode {
    fn to_str(&self) -> ~str {
        match *self {
            Fn => ~"fn",
            Macro => ~"macro"
        }
    }
}


/// Callables
pub enum Fn {
    /// A Rusp function
    RuspFn(~[Ident], @Value, FnMode),
    /// A FFI function that calls into Rust
    ExternFn(@fn(params: &[@Value], env: @mut Rusp) -> EvalResult, FnMode),
}

impl Eq for Fn {
    fn eq(&self, other: &Fn) -> bool {
        match (self, other) {
            (&RuspFn(ref s_ids, ref s_body, ref s_mode),
             &RuspFn(ref o_ids, ref o_body, ref o_mode)) => {
                *s_mode == *o_mode && *s_ids == *o_ids && s_body == o_body
            }
            (&ExternFn(_, ref s_mode),
             &ExternFn(_, ref o_mode)) => {
                if *s_mode == *o_mode {
                    // TODO: pointer equality to compare extern fns
                    fail!("can't compare extern fns")
                } else {
                    false
                }
            }
            _ => false
        }
    }
    fn ne(&self, other: &Fn) -> bool { !self.eq(other) }
}
impl Clone for Fn {
    fn clone(&self) -> Fn {
        match *self {
            RuspFn(ref ids, ref body, ref mode) => RuspFn(ids.clone(), body.clone(), mode.clone()),
            ExternFn(f, mode) => ExternFn(f, mode.clone())
        }
    }
}

impl Fn {
    /// Eval a call of self, eval-ing the arguments in `arg_env`
    /// (unless it is a macro), and using `paent_env` as the parent
    /// environment of the function call.
    fn call(&self,
            arg_env: @mut Rusp, parent_env: @mut Rusp,
            args: &[@Value]) -> EvalResult {
        match *self {
            RuspFn(ref ids, ref body, fn_mode) => {
                if ids.len() != args.len() {
                    return Err(fmt!("%s expects %u arguments", fn_mode.to_str(), ids.len()));
                }

                // stores the argument bindings of the function/macro
                let local = Rusp::new_with_outer(parent_env);
                match fn_mode {
                    Fn => {
                        // evaluate each argument
                        for vec::each2(*ids, args) |id, val| {
                            match arg_env.eval(*val) {
                                Err(e) => return Err(e),
                                Ok(v) => match local.define(id.clone(), v) {
                                    Err(e) => return Err(e),
                                    _ => {}
                                }
                            }
                        }
                    }
                    Macro => {
                        // no evaluation
                        for vec::each2(*ids, args) |id, val| {
                            match local.define(id.clone(), *val) {
                                Err(e) => return Err(e),
                                _ => {}
                            }
                        }
                    }
                }
                local.eval(*body)
            }
            ExternFn(ref f, Fn) => {
                let mut evaled_args = ~[];
                for args.each |v| {
                    match arg_env.eval(*v) {
                        Err(e) => return Err(e),
                        Ok(evaled) => { evaled_args.push(evaled) }
                    }
                }
                (*f)(evaled_args, parent_env)
            }
            ExternFn(ref f, Macro) => {
                (*f)(args, parent_env)
            }
        }
    }
}

#[deriving(Eq)]
pub struct Rusp {
    values: HashMap<Ident, @Value>,
    outer: Option<@mut Rusp>,
}

pub type EvalResult = Result<@Value, ~str>;

impl Rusp {
    /// Initializes an empty environment
    pub fn empty() -> @mut Rusp {
        @mut Rusp {
            values: HashMap::new(),
            outer: None,
        }
    }

    /// Initializes an environment containing the default built-ins
    pub fn new() -> @mut Rusp {
        let env = Rusp::empty();
        builtins::add_builtins(env);
        env
    }

    /// Initializes a new environment with a parent environment
    pub fn new_with_outer(outer: @mut Rusp) -> @mut Rusp {
        @mut Rusp {
            values: HashMap::new(),
            outer: Some(outer),
        }
    }

    /// Defines a new identifier/value pair in the environment
    pub fn define(&mut self, ident: Ident, val: @Value) -> EvalResult {
        if self.values.insert(ident.clone(), val)  {
            Ok(@List(~[]))
        } else {
            Err(fmt!("`%s` was already defined in this environment.", ident.to_str()))
        }
    }

    /// Attempts to find a value corresponding to the supplied identifier
    pub fn find(&self, ident: &Ident) -> Option<@Value> {
        match self.values.find(ident) {
            Some(val) => Some(*val),
            None => self.outer.chain(|env| env.find(ident)),
        }
    }

    /// Evaluates a Rusp expression in the environment
    pub fn eval(@mut self, value: @Value) -> EvalResult {
        match *value {
            Symbol(ref id) => {
                match self.find(id) {
                    Some(val) => Ok(val),
                    None => Err(fmt!("`%s` was not defined in this environment",
                                     id.to_str())),
                }
            }
            List([ref vals, .. args]) => {
                do self.eval(*vals).chain |res| {
                    match *res {
                        FnEnv(ref func, opt_env) => {
                            func.call(self, // environment in which to eval arguments
                                      opt_env.get_or_default(self), // parent environment of function
                                      args)
                        }
                        _ => Err(~"not a function")
                    }
                }
            }
            // everything else evaluates to itself
            _ => Ok(value)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn eval_string(env: @mut Rusp, s: &str) -> EvalResult {
        env.eval(parse(s).unwrap()[0])
    }

    #[test]
    fn test_env() {
        let outer = Rusp::new();
        outer.define(~"a", @Int(0));
        outer.define(~"b", @Float(1.0));
        outer.define(~"c", @Str(~"hi"));

        let inner = Rusp::new_with_outer(outer);
        inner.define(~"a", @Int(3));
        inner.define(~"b", @Int(4));

        assert_eq!(outer.find(&~"a"), Some(@Int(0)));
        assert_eq!(outer.find(&~"b"), Some(@Float(1.0)));
        assert_eq!(outer.find(&~"c"), Some(@Str(~"hi")));
        assert_eq!(inner.find(&~"a"), Some(@Int(3)));
        assert_eq!(inner.find(&~"b"), Some(@Int(4)));
        assert_eq!(inner.find(&~"c"), Some(@Str(~"hi")));
    }

    #[test]
    fn test_atoms() {
        assert_eq!(Rusp::new().eval(@Int(1)).get(), @Int(1));
        assert_eq!(Rusp::new().eval(@Float(1.0)).get(), @Float(1.0));
        assert_eq!(Rusp::new().eval(@Str(~"hi")).get(), @Str(~"hi"));
    }

    #[test]
    fn test_list() {
        assert_eq!(Rusp::new().eval(@List(~[])).get(), @List(~[]));
        assert_eq!(
            Rusp::new().eval(@List(~[
                @Symbol(~"list"),
                @Str(~"hi"),
                @List(~[@Symbol(~"quote"), @Symbol(~"x")]),
                @Int(1)
            ])).get(),
            @List(~[
                @Str(~"hi"),
                @Symbol(~"x"),
                @Int(1)
            ])
        );
    }

    #[test]
    fn test_quote() {
        assert_eq!(Rusp::new().eval(
            @List(~[@Symbol(~"quote"), @Symbol(~"x")])).get(),
            @Symbol(~"x")
        );
    }

    #[test]
    fn test_symbol() {
        let env = Rusp::new();
        env.define(~"a", @Int(0));
        env.define(~"b", @Float(1.0));
        env.define(~"c", @Str(~"hi"));

        assert_eq!(env.eval(@Symbol(~"a")).get(), @Int(0));
        assert_eq!(env.eval(@Symbol(~"b")).get(), @Float(1.0));
        assert_eq!(env.eval(@Symbol(~"c")).get(), @Str(~"hi"));
        assert!(env.eval(@Symbol(~"d")).is_err());
    }

    #[test]
    fn test_if() {
        fn not(test: Value) -> EvalResult {
            Rusp::new().eval(@List(~[@Symbol(~"if"), @test, @Bool(false), @Bool(true)]))
        }

        assert_eq!(not(Bool(true)).get(), @Bool(false));
        assert_eq!(not(Bool(false)).get(), @Bool(true));
        assert!(not(Str(~"hi")).is_err());
    }

    #[test]
    fn test_def() {
        let env = Rusp::new();
        assert!(env.eval(@List(~[@Symbol(~"def"), @Symbol(~"a"), @Int(0)])).is_ok());
        assert!(env.eval(@List(~[@Symbol(~"def"), @Symbol(~"b"), @Float(1.0)])).is_ok());
        assert!(env.eval(@List(~[@Symbol(~"def"), @Symbol(~"c"), @Str(~"hi")])).is_ok());

        assert_eq!(env.find(&~"a"), Some(@Int(0)));
        assert_eq!(env.find(&~"b"), Some(@Float(1.0)));
        assert_eq!(env.find(&~"c"), Some(@Str(~"hi")));

        assert!(env.eval(@List(~[@Symbol(~"def"), @Symbol(~"c"), @List(~[])])).is_err());
    }

    #[test]
    fn test_do() {
        assert_eq!(Rusp::new().eval(@List(~[@Symbol(~"do"), @Str(~"hi")])).get(), @Str(~"hi"));
        assert_eq!(
            Rusp::new().eval(@List(~[
                @Symbol(~"do"),
                @List(~[]),
                @List(~[@Symbol(~"do"), @List(~[])]),
                @Str(~"hi")
            ])).get(),
            @Str(~"hi")
        );

        assert!(Rusp::new().eval(@List(~[@Symbol(~"do"), @Str(~"hi"), @List(~[])])).is_err());
    }

    #[test]
    fn test_fn() {
        let env = Rusp::new();
        eval_string(env, "(def f (fn (a b) a))");
        assert_eq!(eval_string(env, "(f 1 2)").unwrap(), @Int(1));

        assert!(eval_string(env, "(f 1)").is_err());
    }

    #[test]
    fn test_macro() {
        let env = Rusp::new();
        eval_string(env, "(def m (macro (a) a))");
        eval_string(env, "(def f (fn (a) a))");
        eval_string(env, "(def x 1)");

        // check the semantics of macros vs fn, x is not evaluated for
        // `m`, but is for `f`.
        assert_eq!(eval_string(env, "(m x)").unwrap(), @Symbol(~"x"));
        assert_eq!(eval_string(env, "(f x)").unwrap(), @Int(1));
    }

    #[test]
    fn test_lexical_scoping() {
        let env = Rusp::new();
        eval_string(env, "(def a 10)");
        eval_string(env, "(def curried-add (fn (a) (fn (b) (+ a b))))");

        assert_eq!(eval_string(env, "((curried-add 1) 2)").unwrap(), @Int(3));

        eval_string(env, "(def b 10)");
        assert_eq!(eval_string(env, "((curried-add 1) 2)").unwrap(), @Int(3));
    }
}
