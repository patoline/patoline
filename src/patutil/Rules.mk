# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

UTIL_INCLUDES := -I $(d) $(PACK_UTIL)
UTIL_DEPS_INCLUDES := -I $(d) $(DEPS_PACK_UTIL)

$(d)/%.depends: INCLUDES:=$(UTIL_DEPS_INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx $(d)/%.cma $(d)/%.cmxa : INCLUDES:=$(UTIL_INCLUDES)

# Compute ML files dependencies
SRC_$(d) := $(wildcard $(d)/*.ml) $(wildcard $(d)/*.mli)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(addsuffix .depends,$(SRC_$(d)))
endif
endif

# Building
UTIL_MODS:= FilenameExtra UsualMake Zipper Util Graph

UTIL_ML:=$(addsuffix .ml,$(addprefix $(d)/,$(UTIL_MODS)))
UTIL_MLI:=$(wildcard $(d)/*.mli)

UTIL_CMO:=$(UTIL_ML:.ml=.cmo)
UTIL_CMX:=$(UTIL_ML:.ml=.cmx)
UTIL_CMI:=$(UTIL_ML:.ml=.cmi)

# We cannot run ocamlc and ocamlopt simultaneously on the same input,
# since they both overwrite the .cmi file, which can get corrupted.
# That's why we arbitrarily force the following dependency.
$(UTIL_CMX): %.cmx: %.cmo

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
	install -m 755 -d $(DESTDIR)/$(INSTALL_UTIL_DIR)
	install -m 644 -p $^ $(DESTDIR)/$(INSTALL_UTIL_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
