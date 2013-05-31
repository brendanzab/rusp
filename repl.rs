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

extern mod rusp;
extern mod extra;

// for a nicer prompt, with history etc.
use extra::rl;

fn main() {
    unsafe { do rl::complete |_line, _suggest| {} }

    let mut prompt = "> ";
    let mut stored = ~"";
    println("Rusp Repl");
    let env = rusp::Rusp::new();

    loop {
        let line = match unsafe { rl::read(prompt) } {
            Some(line) => line,
            None => break
        };

        if !stored.is_empty() { stored.push_char('\n'); }
        else if line.is_empty() {
            // no input and no history
            loop;
        }
        stored.push_str(line);

        if !line.is_empty() {
            unsafe { rl::add_history(line); }
        }

        let continue_line = match rusp::parse(stored) {
            Ok(exprs) => {
                for exprs.each |ex| {
                    println(match env.eval(*ex) {
                        Ok(evaled) => evaled.to_str(),
                        Err(e) => fmt!("Error: %s", e)
                    })
                }

                false
            }
            Err(ref er) if er.description == ~"Unexpected EOF" => {
                // it's not invalid yet, just haven't got enough
                // input, so wait for more
                true
            }
            Err(ref er) => {
                // give a little caret pointing at the serror
                println(fmt!("%s^\nParse Error: %s",
                             str::repeat(" ", er.position.col - 1 + prompt.len()),
                             er.description));
                false
            }
        };

        prompt = if continue_line {
            "+ "
        } else {
            stored = ~"";
            "> "
        }
    }
}
