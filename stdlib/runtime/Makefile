CC = /usr/bin/clang
LD = /usr/bin/ld

DEFS = STDIN_PATH='"/dev/stdin"' STDOUT_PATH='"/dev/stdout"' STDERR_PATH='"/dev/stderr"' DEBUG_HEAP
LDFLAGS = -shared
CXXFLAGS = -fPIC $(addprefix -D,$(DEFS))
EXT = .c

SRCDIR = src
INCDIR = include
BINDIR = bin

OBJDIR = $(BINDIR)/obj
RUNTIME_LIB = $(BINDIR)/libsom.so

SRC = $(wildcard $(SRCDIR)/*$(EXT))
OBJ = $(SRC:$(SRCDIR)/%$(EXT)=$(OBJDIR)/%.o)

OBJCOUNT_NOPAD = $(shell v=`echo $(OBJ) | wc -w`; echo `seq 1 $$(expr $$v)`)
OBJCOUNT = $(foreach v,$(OBJCOUNT_NOPAD),$(shell printf '%02d' $(v)))

.PRECIOUS: $(OBJ)
SHELL := /bin/bash

all: runtime

.PHONY: makedirs
makedirs:
	@mkdir -p $(BINDIR)
	@mkdir -p $(OBJDIR)

.PHONY: clean
clean:
	@rm -rf $(BINDIR)

.PHONY: install
install:
	cp $(RUNTIME_LIB) /usr/lib/$(notdir $(RUNTIME_LIB))

# =============================

runtime: $(OBJ)
	@$(LD) -o $(RUNTIME_LIB) $(LDFLAGS) $(OBJ)

$(OBJDIR)/%.o: $(SRCDIR)/%$(EXT) | makedirs
	@mkdir -p $(dir $@)
	@printf "[$(word 1,$(OBJCOUNT))/$(words $(OBJ))] compiling $(notdir $<) into $(notdir $@)..."
	@$(CC) $(CXXFLAGS) -I$(INCDIR) -o $@ -c $< -fmodule-name=std/$(notdir $(<:$(EXT)=.som))
	@printf "\b\b done!\n"

	$(eval OBJCOUNT = $(filter-out $(word 1,$(OBJCOUNT)),$(OBJCOUNT)))
