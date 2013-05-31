# Copyright 2013 The Rusp Developers. For a full listing of the authors,
# refer to the AUTHORS file at the top-level directory of this distribution.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

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
