# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

# Compute ML files dependencies
SRC_$(d):=$(wildcard $(d)/*.ml)
-include $(addsuffix .depends,$(SRC_$(d)))

PLUGINS_INCLUDES := -I $(d) $(PACK_PLUGINS)
PLUGINS_DEPS_INCLUDES := -I $(d) $(DEPS_PACK_PLUGINS)
$(d)/%.depends: INCLUDES+=$(PLUGINS_DEPS_INCLUDES)
$(d)/%.cmxa $(d)/%.cmo $(d)/%.cmi $(d)/%.cmx: INCLUDES += $(PLUGINS_INCLUDES)

# Building stuff
all: $(d)/caml.cmxs

$(d)/plugins.cmx: $(d)/plugins.cmo

$(d)/caml.cmxs: %.cmxs: %.cmx
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(INCLUDES) -shared -o $@ -linkpkg $<

# Installing
install: install-plugins
.PHONY: install-plugins
install-plugins: $(d)/caml.cmxs
	install -m 755 -d $(DESTDIR)/$(INSTALL_PLUGINS_DIR)
	install -m 644 $^ $(DESTDIR)/$(INSTALL_PLUGINS_DIR)

# Cleaning
CLEAN += $(d)/*.cmxs $(d)/*.cmxa $(d)/*.o $(d)/*.cm[ioxa]
DISTCLEAN += $(d)/*.depends

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
