
.PHONY: all lib check
all: lib
clean:
	rm repl
	rm librusp*

check: check-rusp check-repl

check-rusp: pprint.rs rusp.rs parser.rs
	rustc --test rusp.rs
	./rusp
check-repl: repl.rs lib
	rustc --test -L . repl.rs
	./repl

repl: repl.rs lib
	rustc -L . repl.rs

lib: librusp*

librusp*: pprint.rs rusp.rs parser.rs
	rustc rusp.rs
