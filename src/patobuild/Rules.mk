# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

PATOBUILD_INCLUDES      := -I $(d) -I $(CONFIG_DIR) -package decap,threads -pp pa_ocaml -thread
PATOBUILD_DEPS_INCLUDES := -I $(d) -I $(CONFIG_DIR) -package decap,threads -pp pa_ocaml

$(d)/%.depends: INCLUDES := $(PATOBUILD_DEPS_INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx: INCLUDES := $(PATOBUILD_INCLUDES)

# FIXME twice patoline otherwise patoline.ml.depends is not build ...
# silly error ?
PATOBUILD_MODS := pragma parallel build patoline patoline

PATOBUILD_ML  := $(addsuffix .ml,$(addprefix $(d)/,$(PATOBUILD_MODS)))

# Compute ML dependencies
SRC_$(d) := $(addsuffix .depends,$(PATOBUILD_ML)))

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(SRC_$(d))
endif
endif

PATOBUILD_CMX := $(PATOBUILD_ML:.ml=.cmx)
PATOBUILD_CMO := $(PATOBUILD_ML:.ml=.cmo)

$(PATOBUILD_CMX): %.cmx: %.cmo

all: $(d)/patoline

$(d)/patoline: $(PATOBUILD_CMX)
	$(ECHO) "[OPT] $@"
	$(Q)$(OCAMLOPT) -o $@ $(PATOBUILD_INCLUDES) -package decap,threads \
		unix.cmxa patoconfig.cmxa str.cmxa decap.cmxa threads.cmxa $^

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
