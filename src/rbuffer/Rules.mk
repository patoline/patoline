# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

# Compute ML files dependencies
SRC_$(d) := $(wildcard $(d)/*.ml) $(wildcard $(d)/*.mli)

RBUFFER_INCLUDES := -I $(d) $(PACK_RBUFFER)

$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx : INCLUDES:=$(RBUFFER_INCLUDES)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(addsuffix .depends,$(SRC_$(d)))
endif
endif

# Building
RBUFFER_MODS:= rbuffer

RBUFFER_ML:=$(addsuffix .ml,$(addprefix $(d)/,$(RBUFFER_MODS)))
RBUFFER_CMO:=$(RBUFFER_ML:.ml=.cmo)
RBUFFER_CMX:=$(RBUFFER_ML:.ml=.cmx)

RBUFFER_MLI:=$(wildcard $(d)/*.mli)
RBUFFER_CMI:=$(RBUFFER_MLI:.mli=.cmi)

$(d)/rbuffer.cma: $(RBUFFER_CMO)
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLC) -a -o $@ $^

$(d)/rbuffer.cmxa: $(RBUFFER_CMX)
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) -a -o $@ $^

$(d)/rbuffer.cmxs: $(RBUFFER_CMX)
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) -shared -o $@ $^


# Building everything
all: $(d)/rbuffer.cmxa $(d)/rbuffer.cma $(d)/rbuffer.cmxs

# Cleaning
CLEAN += $(d)/*.cma $(d)/*.cmxa $(d)/*.cmo $(d)/*.cmx $(d)/*.cmi $(d)/*.o $(d)/*.a $(d)/*.cmxs

DISTCLEAN += $(wildcard $(d)/*.depends)

# Installing
install: install-rbuffer
.PHONY: install-rbuffer
install-rbuffer: $(d)/rbuffer.cma $(d)/rbuffer.cmxa $(d)/rbuffer.cmxs \
	  $(d)/rbuffer.a $(RBUFFER_CMI) $(RBUFFER_MLI) $(RBUFFER_CMX) \
	  $(RBUFFER_CMO) $(d)/META
	$(ECHO) "[INS] rbuffer"
	- $(OCAMLFIND) remove rbuffer
	$(Q)$(OCAMLFIND) install rbuffer $^

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
