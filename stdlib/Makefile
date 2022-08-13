CC = clang

LDFLAGS = -shared
CXXFLAGS = -D COMPILER=\"$(CC)\"
EXT = .c

SRCDIR = src
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
	@$(CC) $(CXXFLAGS) -I$(SRCDIR) -o $@ -c $< -fmodule-name=$(<:$(EXT)=.som)
	@printf "\b\b done!\n"
	$(eval OBJCOUNT = $(filter-out $(word 1,$(OBJCOUNT)),$(OBJCOUNT)))
# @([ "$(word $(words $(OBJ)), $(OBJ))" == "$@" ] && echo "[stdlib] standard library compiled!") || true

.PHONY: clean
clean:
	@rm -rf $(BINDIR)

.PHONY: makedirs
makedirs:
	@mkdir -p $(BINDIR)
	@mkdir -p $(OBJDIR)