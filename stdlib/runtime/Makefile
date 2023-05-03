CC = /usr/bin/clang
AR = /usr/bin/ar
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
JEMALLOC_LIB = $(BINDIR)/libsom-jemalloc.so

SRC = $(wildcard $(SRCDIR)/*$(EXT))
OBJ = $(SRC:$(SRCDIR)/%$(EXT)=$(OBJDIR)/%.o)
JEMALLOC_SRC_LIB = jemalloc/lib/libjemalloc-som.so.2

OBJCOUNT_NOPAD = $(shell v=`echo $(OBJ) | wc -w`; echo `seq 1 $$(expr $$v)`)
OBJCOUNT = $(foreach v,$(OBJCOUNT_NOPAD),$(shell printf '%02d' $(v)))

.PRECIOUS: $(OBJ)
SHELL := /bin/bash

all: runtime jemalloc

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
	cp $(JEMALLOC_LIB) /usr/lib/$(notdir $(JEMALLOC_LIB))

# =============================

runtime: $(OBJ)
	@$(LD) -o $(RUNTIME_LIB) $(LDFLAGS) $(OBJ)

$(OBJDIR)/%.o: $(SRCDIR)/%$(EXT) | makedirs
	@mkdir -p $(dir $@)
	@printf "[$(word 1,$(OBJCOUNT))/$(words $(OBJ))] compiling $(notdir $<) into $(notdir $@)..."
	@$(CC) $(CXXFLAGS) -I$(INCDIR) -o $@ -c $< -fmodule-name=std/$(notdir $(<:$(EXT)=.som))
	@printf "\b\b done!\n"

	$(eval OBJCOUNT = $(filter-out $(word 1,$(OBJCOUNT)),$(OBJCOUNT)))

# =============================

$(JEMALLOC_SRC_LIB):
	@cd jemalloc && ./autogen.sh \
		--with-jemalloc-prefix=som_je_ \
		--with-install-suffix="-som"
	@$(MAKE) -C jemalloc

.PHONY: jemalloc
jemalloc: $(JEMALLOC_SRC_LIB) | makedirs
	@cp $(JEMALLOC_SRC_LIB) $(JEMALLOC_LIB)

.PHONY: clean-jemalloc
clean-jemalloc:
	@$(MAKE) -C jemalloc clean
