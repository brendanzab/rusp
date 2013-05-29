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
            Ok(ex) => {
                let to_print = match env.eval(@ex) {
                    Ok(evaled) => evaled.to_str(),
                    Err(e) => {
                        fmt!("Error: %s", e)
                    }
                };

                if to_print != ~"()" {
                    println(to_print);
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
