SHELL := /bin/bash
MAKEFLAGS += --no-print-directory

build:
	clear
	cd somc && dune build

exec: build
	cd somc && dune exec somc -- $(args)

.PHONY: stdlib
stdlib:
	@cd stdlib && make stdlib

install: build stdlib
	sudo cp somc/_build/default/bin/main.exe /usr/bin/somc

	sudo cp stdlib/bin/libsom.so /usr/lib/

	sudo cp tools/bash-completion.sh /usr/share/bash-completion/completions/somc
	source ~/.bashrc