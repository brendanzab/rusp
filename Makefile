
.PHONY: all lib
all: lib
clean:
	rm repl
	rm librusp*

repl: repl.rs lib
	rustc -L . repl.rs

lib: librusp*

librusp*: pprint.rs rusp.rs parse.rs
	rustc rusp.rs
