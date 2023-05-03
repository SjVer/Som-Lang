SHELL := /bin/bash
MAKEFLAGS += --no-print-directory
EXE = somc/_build/default/bin/main.exe

SOM_INCL_DIR = /usr/share/som/include

.DEFAULT_GOAL := build

gen_codes_ml:
	@python3 gen_codes_ml.py

build: gen_codes_ml
	@clear -x
	@cd somc && dune build

exec: build
	@$(EXE) $(args)

test: build
	@$(EXE) -i test $(args) --no-prelude test/test.som -o test/test -v

.PHONY: runtime
runtime:
	@$(MAKE) -C stdlib/runtime runtime

install:
	$(MAKE) -C stdlib/runtime install

	sudo cp $(EXE) /usr/bin/somc

	rm -r $(SOM_INCL_DIR)/std || true
	cp -r stdlib/modules $(SOM_INCL_DIR)/std

	cp tools/bash-completion.sh /usr/share/bash-completion/completions/somc
	source ~/.bashrc

clean:
	@cd somc && dune clean
	@cd stdlib/runtime && make clean
