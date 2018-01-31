# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

PATOBUILD_INCLUDES := -I $(d) -I $(CONFIG_DIR) $(PACK_PATOBUILD) -package earley,threads -thread

$(d)/%.cmi $(d)/%.cmx $(d)/%.cmo: INCLUDES := $(PATOBUILD_INCLUDES)
$(d)/%.depends $(d)/%.cmi $(d)/%.cmx: OCPP=pa_ocaml

#no cmo here
$(d)/%.cmo: ; touch $@
$(d)/%.cmx: OCAMLOPT=$(OCAMLOPT_NOINTF)

# Compute ML dependencies
SRC_$(d):=$(wildcard $(d)/*.ml)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(addsuffix .depends,$(SRC_$(d))) $(d)/patoline.ml.depends
endif
endif

PATOBUILD_MODS := pragma parallel build patoline patoline

PATOBUILD_CMX  := $(addsuffix .cmx,$(addprefix $(d)/,$(PATOBUILD_MODS)))

all: $(d)/patoline

$(d)/patoline: $(PATOBUILD_CMX) $(UTIL_DIR)/patutil.cmxa
	$(ECHO) "[OPT] $@"
	$(Q)$(OCAMLOPT) $(PATOBUILD_INCLUDES) -package earley,earley.str,threads \
		unix.cmxa patoconfig.cmxa str.cmxa earley.cmxa earleyStr.cmxa \
		threads.cmxa patutil.cmxa -o $@ $^

CLEAN     += $(d)/*.o $(d)/*.cm[iox]
DISTCLEAN += $(d)/*.depends $(d)/patoline

# Installing
install: install-patobuild

.PHONY: install-patobuild
install-patobuild: $(d)/patoline
	install -m 755 -d $(DESTDIR)/$(INSTALL_BIN_DIR)
	install -m 755 -p $^ $(DESTDIR)/$(INSTALL_BIN_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
