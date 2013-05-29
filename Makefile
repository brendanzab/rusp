RUSP_FILES=rusp.rs pprint.rs parser.rs builtins.rs

.PHONY: all lib check
all: repl
clean:
	rm repl
	rm librusp*

check: check-rusp check-repl

check-rusp: $(RUSP_FILES)
	rustc --test rusp.rs -o test-rusp
	./test-rusp
check-repl: repl.rs lib
	rustc --test -L . repl.rs -o test-repl
	./test-repl

repl: repl.rs lib
	rustc -L . repl.rs

lib: librusp*

librusp*: $(RUSP_FILES)
	rustc rusp.rs
