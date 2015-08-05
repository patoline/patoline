# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

RAWLIB_INCLUDES := -I $(d) $(PACK_RAWLIB)
RAWLIB_DEPS_INCLUDES := -I $(d) $(DEPS_PACK_RAWLIB)

$(d)/%.depends: INCLUDES:=$(RAWLIB_DEPS_INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx : INCLUDES:=$(RAWLIB_INCLUDES)

# Compute ML files dependencies
SRC_$(d) := $(wildcard $(d)/*.ml) $(wildcard $(d)/*.mli)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(addsuffix .depends,$(SRC_$(d)))
endif
endif

# Building
RAWLIB_MODS:= Config0 Color Bezier Raw Driver DynDriver Rawlib

RAWLIB_ML:=$(addsuffix .ml,$(addprefix $(d)/,$(RAWLIB_MODS)))

RAWLIB_CMO:=$(RAWLIB_ML:.ml=.cmo)
RAWLIB_CMX:=$(RAWLIB_ML:.ml=.cmx)
RAWLIB_CMI:=$(RAWLIB_ML:.ml=.cmi)

# We cannot run ocamlc and ocamlopt simultaneously on the same input,
# since they both overwrite the .cmi file, which can get corrupted.
# That's why we arbitrarily force the following dependency.
$(RAWLIB_CMX): %.cmx: %.cmo

$(d)/rawlib.cma: $(RAWLIB_CMO)
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLC) -a -o $@ $^

$(d)/rawlib.cmxa: $(RAWLIB_CMX)
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLOPT) -a -o $@ $^

$(d)/rawlib.cmxs: $(RAWLIB_CMX)
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLOPT) -shared -o $@ $^

# Building everything
all: $(d)/rawlib.cmxa $(d)/rawlib.cma $(d)/rawlib.cmxs

# Cleaning
CLEAN += $(d)/*.cma $(d)/*.cmxa $(d)/*.cmo $(d)/*.cmx $(d)/*.cmi $(d)/*.o $(d)/*.a $(d)/*.cmxs

DISTCLEAN += $(wildcard $(d)/*.depends) $(d)/Config0.ml

# Installing
install: install-rawlib
.PHONY: install-rawlib
install-rawlib: $(d)/rawlib.cma $(d)/rawlib.cmxa $(d)/rawlib.cmxs $(d)/rawlib.a $(RAWLIB_CMI) $(RAWLIB_CMX) $(RAWLIB_CMO) $(d)/META
	install -m 755 -d $(DESTDIR)/$(INSTALL_RAWLIB_DIR)
	install -m 644 -p $^ $(DESTDIR)/$(INSTALL_RAWLIB_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
