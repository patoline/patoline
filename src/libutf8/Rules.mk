# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

LIBUTF8_INCLUDES := -I $(d)
LIBUTF8_DEPS_INCLUDES := -I $(d)

$(d)/%.depends: INCLUDES:=$(LIBUTF8_DEPS_INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx : INCLUDES:=$(LIBUTF8_INCLUDES)

# Compute ML files dependencies
SRC_$(d) := $(wildcard $(d)/*.ml) $(wildcard $(d)/*.mli)

-include $(addsuffix .depends,$(SRC_$(d)))

# Building
LIBUTF8_MODS:= UTF8

LIBUTF8_ML:=$(addsuffix .ml,$(addprefix $(d)/,$(LIBUTF8_MODS)))

LIBUTF8_CMO:=$(LIBUTF8_ML:.ml=.cmo)
LIBUTF8_CMX:=$(LIBUTF8_ML:.ml=.cmx)
LIBUTF8_CMI:=$(LIBUTF8_ML:.ml=.cmi)

# We cannot run ocamlc and ocamlopt simultaneously on the same input,
# since they both overwrite the .cmi file, which can get corrupted.
# That's why we arbitrarily force the following dependency.
$(LIBUTF8_CMX): %.cmx: %.cmo

$(d)/libutf8.cma: $(LIBUTF8_CMO)
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLC) -a -o $@ $^

$(d)/libutf8.cmxa: $(LIBUTF8_CMX)
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLOPT) -a -o $@ $^

$(d)/libutf8.cmxs: $(LIBUTF8_CMX)
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLOPT) -shared -o $@ $^

# Building everything
all: $(d)/libutf8.cmxa $(d)/libutf8.cma $(d)/libutf8.cmxs

# Cleaning
CLEAN += $(d)/*.cma $(d)/*.cmxa $(d)/*.cmo $(d)/*.cmx $(d)/*.cmi $(d)/*.o $(d)/*.a $(d)/*.cmxs

DISTCLEAN += $(wildcard $(d)/*.depends)

# Installing
install: install-libutf8
.PHONY: install-libutf8
install-libutf8: $(d)/libutf8.cma $(d)/libutf8.cmxa $(d)/libutf8.cmxs $(d)/libutf8.a $(LIBUTF8_CMI) $(LIBUTF8_CMX) $(LIBUTF8_CMO) $(d)/META
	install -m 755 -d $(DESTDIR)/$(INSTALL_LIBUTF8_DIR)
	install -m 644 -p $^ $(DESTDIR)/$(INSTALL_LIBUTF8_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
