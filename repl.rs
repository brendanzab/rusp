extern mod rusp;

fn main() {
    let mut stored = ~"";
    print("Rusp Repl\n> ");
    let env = rusp::Rusp::empty();

    for io::stdin().each_line |line| {
        stored.push_str(line);
        let continue_line = match rusp::parse(stored) {
            Ok(ex) => {
                // print the AST, since that's useful for debugging
                println(fmt!("AST: %?" ex));
                // separate, since this can make the REPL crash
                println(fmt!("%?", env.eval(ex)));
                false
            }
            Err(ref er) if er.description == ~"Expecting ')'" => {
                // unclosed (, so wait for more input
                true
            }
            Err(ref er) => {
                println(fmt!("Error: %?", er.description));
                false
            }
        };

        if continue_line {
            print("+ ");
        } else {
            stored = ~"";
            print("> ");
        }
    }
}
