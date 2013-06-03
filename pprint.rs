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

use super::*;

use std::str;

impl ToStr for Value {
    fn to_str(&self) -> ~str {
        match *self {
            Bool(b) => b.to_str(),
            Int(i) => i.to_str(),
            Float(f) => f.to_str(),
            List(ref vals) => fmt!("(%s)", str::connect(vals.map(|val| val.to_str()), " ")),
            Str(ref s) => fmt!("\"%s\"", str::escape_default(*s)),
            Symbol(ref id) => id.to_str(),
            FnEnv(ref func, _) => {
                match *func {
                    RuspFn(ref ids, ref val, ref fn_type) => {
                        fmt!("(%s (%s) %s)",
                             fn_type.to_str(),
                             str::connect(ids.map(|id| id.to_str()), " "),
                             val.to_str())
                    }
                    ExternFn(_, ref fn_type) => {
                        fmt!("(extern %s)", fn_type.to_str())
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::*;
    fn test_pprint() {
        assert_eq!(List(~[
            @Symbol(~"if"),
            @Bool(true),
            @List(~[@FnEnv(RuspFn(~[], @List(~[@Symbol(~"quote"), @Symbol(~"a")]), Fn), None)]),
            @FnEnv(RuspFn(~[], @List(~[@Symbol(~"quote"), @Symbol(~"a")]), Macro), None),
            @Bool(true)
        ]).to_str(), ~"(if true ((fn () (quote a))) (macro () (quote a) true)")
    }
}
