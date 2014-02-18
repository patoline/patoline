# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

UTIL_INCLUDES := -I $(d) -I $(RBUFFER_DIR)

# Compute ML files dependencies
SRC_$(d) := $(wildcard $(d)/*.ml) $(wildcard $(d)/*.mli)

$(d)/%.depends: INCLUDES:=$(UTIL_INCLUDES)

-include $(addsuffix .depends,$(SRC_$(d)))

# Building
UTIL_MODS:= FilenameExtra UsualMake Util

UTIL_ML:=$(addsuffix .ml,$(addprefix $(d)/,$(UTIL_MODS)))
UTIL_CMO:=$(UTIL_ML:.ml=.cmo)
UTIL_CMX:=$(UTIL_ML:.ml=.cmx)
UTIL_CMI:=$(UTIL_ML:.ml=.cmi)

# We cannot run ocamlc and ocamlopt simultaneously on the same input,
# since they both overwrite the .cmi file, which can get corrupted.
# That's why we arbitrarily force the following dependency.
$(UTIL_CMX): %.cmx: %.cmo

$(UTIL_CMI): %.cmi: %.mli
	$(ECHO) "[OCAMLC] $< -> $@"
	$(Q)$(OCAMLC) $(OFLAGS) $(PACK) $(UTIL_INCLUDES) -o $@ -c $<
$(UTIL_CMO): %.cmo: %.ml
	$(ECHO) "[OCAMLC] $< -> $@"
	$(Q)$(OCAMLC) $(OFLAGS) $(PACK) $(UTIL_INCLUDES) -o $@ -c $<
$(UTIL_CMX): %.cmx: %.ml
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(PACK) $(UTIL_INCLUDES) -o $@ -c $<

$(d)/util.cma: $(UTIL_CMO)
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLC) -a -o $@ $^

$(d)/util.cmxa: $(UTIL_CMX)
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLOPT) -a -o $@ $^


# Building everything
all: $(d)/util.cmxa $(d)/util.cma

# Cleaning
CLEAN += $(d)/*.cma $(d)/*.cmxa $(d)/*.cmo $(d)/*.cmx $(d)/*.cmi $(d)/*.o $(d)/*.a

DISTCLEAN += $(wildcard $(d)/*.depends)

# Installing
install: install-util
.PHONY: install-util
install-util: $(d)/util.cma $(d)/util.cmxa $(d)/util.a $(UTIL_CMI) $(UTIL_MLI) $(UTIL_CMX) $(UTIL_CMO) $(d)/META
	install -m 755 -d $(DESTDIR)/$(INSTALL_UTIL_DIR)
	install -m 644 -p $^ $(DESTDIR)/$(INSTALL_UTIL_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
