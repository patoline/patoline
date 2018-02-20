# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

UTIL_INCLUDES := -I $(d) $(PACK_UTIL)

$(d)/%.depends: INCLUDES:=$(DEPS_DIR)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx $(d)/%.cma $(d)/%.cmxa : INCLUDES:=$(UTIL_INCLUDES)

# Compute ML files dependencies
SRC_$(d) := $(wildcard $(d)/*.ml) $(wildcard $(d)/*.mli)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(addsuffix .depends,$(SRC_$(d)))
endif
endif

# Building
UTIL_MODS:= Extra Base64 Zipper Util Graph DynArray

UTIL_ML:=$(addsuffix .ml,$(addprefix $(d)/,$(UTIL_MODS)))
UTIL_MLI:=$(wildcard $(d)/*.mli)

UTIL_CMO:=$(UTIL_ML:.ml=.cmo)
UTIL_CMX:=$(UTIL_ML:.ml=.cmx)
UTIL_CMI:=$(UTIL_ML:.ml=.cmi)

$(d)/patutil.cma: $(UTIL_CMO)
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLC) $(OFLAGS) $(INCLUDES) -a -o $@ $^

$(d)/patutil.cmxa: $(UTIL_CMX)
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(INCLUDES) -a -o $@ $^

$(d)/patutil.cmxs: $(UTIL_CMX)
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(INCLUDES) -shared -o $@ $^

# Building everything
all: $(d)/patutil.cmxa $(d)/patutil.cma $(d)/patutil.cmxs

# Cleaning
CLEAN += $(d)/*.cma $(d)/*.cmxa $(d)/*.cmo $(d)/*.cmx $(d)/*.cmi $(d)/*.o $(d)/*.a $(d)/*.cmxs

DISTCLEAN += $(wildcard $(d)/*.depends)

# Installing
install: install-util
.PHONY: install-util
install-util: $(d)/patutil.cma $(d)/patutil.cmxa $(d)/patutil.cmxs \
	  $(d)/patutil.a $(UTIL_CMI) $(UTIL_CMX) $(UTIL_CMO) $(UTIL_MLI) $(d)/META
	$(ECHO) "[INS] patutil"
	- $(OCAMLFIND) remove patutil
	$(Q)$(OCAMLFIND) install patutil $^

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
