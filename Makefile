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
	@$(EXE) $(args) test/test.som

.PHONY: stdlib
stdlib:
	@cd stdlib && make stdlib

install: build stdlib
	cp $(EXE) /usr/bin/somc

	cp stdlib/bin/libsom.so /usr/lib/
	cp -r stdlib/include $(SOM_INCL_DIR)/std

	cp tools/bash-completion.sh /usr/share/bash-completion/completions/somc
	source ~/.bashrc

clean:
	@cd somc && dune clean
	@cd stdlib && make clean