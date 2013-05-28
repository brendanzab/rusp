
.PHONY: all lib check
all: lib
clean:
	rm repl
	rm librusp*

check: *.rs
	rustc --test rusp.rs
	./rusp
	rustc --test -L . repl.rs
	./repl

repl: repl.rs lib
	rustc -L . repl.rs

lib: librusp*

librusp*: pprint.rs rusp.rs parse.rs
	rustc rusp.rs
