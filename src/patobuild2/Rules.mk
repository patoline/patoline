# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

PATOBUILD2_INCLUDES      := -I $(d) -I $(CONFIG_DIR) -package decap,threads -pp pa_ocaml -thread
PATOBUILD2_DEPS_INCLUDES := -I $(d) -I $(CONFIG_DIR) -package decap,threads -pp pa_ocaml

$(d)/%.depends: INCLUDES := $(PATOBUILD2_DEPS_INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx: INCLUDES := $(PATOBUILD2_INCLUDES)

# FIXME twice patoline otherwise patoline.ml.depends is not build ...
# silly error ?
PATOBUILD2_MODS := pragma parallel build patoline patoline

PATOBUILD2_ML  := $(addsuffix .ml,$(addprefix $(d)/,$(PATOBUILD2_MODS)))

# Compute ML dependencies
SRC_$(d) := $(addsuffix .depends,$(PATOBUILD2_ML)))

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(SRC_$(d))
endif
endif

PATOBUILD2_CMX := $(PATOBUILD2_ML:.ml=.cmx)
PATOBUILD2_CMO := $(PATOBUILD2_ML:.ml=.cmo)

$(PATOBUILD2_CMX): %.cmx: %.cmo

all: $(d)/patoline2

$(d)/patoline2: $(PATOBUILD2_CMX)
	$(ECHO) "[OPT] $@"
	$(Q)$(OCAMLOPT) -o $@ $(PATOBUILD2_INCLUDES) -package decap,threads \
		unix.cmxa patoconfig.cmxa str.cmxa decap.cmxa threads.cmxa $^

CLEAN     += $(d)/*.o $(d)/*.cm[iox]
DISTCLEAN += $(d)/*.depends $(d)/patoline2

# Installing
install: install-patobuild

.PHONY: install-patobuild
install-patobuild: $(d)/patoline2
	install -m 755 -d $(DESTDIR)/$(INSTALL_BIN_DIR)
	install -m 755 -p $^ $(DESTDIR)/$(INSTALL_BIN_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
