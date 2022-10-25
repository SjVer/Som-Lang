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
	@$(EXE) -i stdlib/include -i test $(args) test/test.som -v

.PHONY: stdlib
stdlib:
	@cd stdlib && make stdlib

install: build stdlib
	sudo cp $(EXE) /usr/bin/somc

	sudo cp stdlib/bin/libsom.so /usr/lib/
	sudo rm -r $(SOM_INCL_DIR)/std
	sudo cp -r stdlib/include $(SOM_INCL_DIR)/std

	sudo cp tools/bash-completion.sh /usr/share/bash-completion/completions/somc
	source ~/.bashrc

clean:
	@cd somc && dune clean
	@cd stdlib && make clean