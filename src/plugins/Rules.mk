# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

# Compute ML files dependencies
SRC_$(d):=$(wildcard $(d)/*.ml)
-include $(addsuffix .depends,$(SRC_$(d)))

# Building stuff
all: $(d)/caml.cmxs

PLUGINS_INCLUDES := -I $(PATOLINE_DIR) -I $(d)/../Util

$(d)/%.depends: INCLUDES:=-I $(d) $(PLUGINS_INCLUDES)

$(d)/caml.cmx: %.cmx: %.ml
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) -c $(PLUGINS_INCLUDES) -o $@ unix.cmxa $<

$(d)/caml.cmxs: %.cmxs: %.cmx
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(PLUGINS_INCLUDES) -shared -o $@ unix.cmxa -linkpkg $<

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
