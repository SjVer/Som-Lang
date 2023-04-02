CC = /usr/bin/clang

DEFS = STDIN_PATH='"/dev/stdin"' STDOUT_PATH='"/dev/stdout"' STDERR_PATH='"/dev/stderr"'
LDFLAGS = -shared
CXXFLAGS = -fPIC $(addprefix -D,$(DEFS))
EXT = .c

SRCDIR = src
INCDIR = include
BINDIR = bin

OBJDIR = $(BINDIR)/obj
LIB = $(BINDIR)/libsom.so

SRC = $(wildcard $(SRCDIR)/*$(EXT))
OBJ = $(SRC:$(SRCDIR)/%$(EXT)=$(OBJDIR)/%.o)

OBJCOUNT_NOPAD = $(shell v=`echo $(OBJ) | wc -w`; echo `seq 1 $$(expr $$v)`)
OBJCOUNT = $(foreach v,$(OBJCOUNT_NOPAD),$(shell printf '%02d' $(v)))

.PRECIOUS: $(OBJ)
SHELL := /bin/bash

stdlib: $(OBJ) | makedirs
	@$(CC) $(LDFLAGS) -o $(LIB) $^

$(OBJDIR)/%.o: $(SRCDIR)/%$(EXT) | makedirs
	@mkdir -p $(dir $@)
	@printf "[$(word 1,$(OBJCOUNT))/$(words $(OBJ))] compiling $(notdir $<) into $(notdir $@)..."
	@$(CC) $(CXXFLAGS) -I$(INCDIR) -o $@ -c $< -fmodule-name=std/$(notdir $(<:$(EXT)=.som))
	@printf "\b\b done!\n"

	@for s in $$(nm $@ | grep -oP 'som_\w+'); \
	do \
		dest=$${s/som_/_som_std_$(notdir $(basename $<))_}; \
		# printf "       - renaming $$s to $$dest\n"; \
		objcopy --redefine-sym $$s="$$dest" $@; \
	done \

	$(eval OBJCOUNT = $(filter-out $(word 1,$(OBJCOUNT)),$(OBJCOUNT)))

.PHONY: clean
clean:
	@rm -rf $(BINDIR)

.PHONY: makedirs
makedirs:
	@mkdir -p $(BINDIR)
	@mkdir -p $(OBJDIR)