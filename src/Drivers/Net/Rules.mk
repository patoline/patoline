# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

NET_DRIVER_INCLUDES:=-I $(d) $(PACK_DRIVER_Net) -I $(DRIVERS_DIR)/SVG
NET_DRIVER_DEPS_INCLUDES:=-I $(d) $(DEPS_PACK_DRIVER_Net) -I $(DRIVERS_DIR)/SVG

$(d)/%.ml.depends: INCLUDES += $(NET_DRIVER_DEPS_INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx: INCLUDES += $(NET_DRIVER_INCLUDES)

SRC_$(d):=$(wildcard $(d)/*.ml)
DEPENDS_$(d) := $(addsuffix .depends,$(SRC_$(d)))
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(DEPENDS_$(d))
endif
endif

# cmi race condition
$(d)/Net.cmx: %.cmx: %.cmo

$(d)/Net.cma: $(d)/Net.cmo
	$(ECHO) "[MKLIB]   $<"
	$(Q)$(OCAMLC) -a -o $@ $<

$(d)/Net.cmxa: $(d)/Net.cmx
	$(ECHO) "[OMKLIB]   $<"
	$(Q)$(OCAMLOPT) -a -o $@ $<

$(d)/Net.cmxs: $(d)/Net.cmx
	$(ECHO) "[SHARE]   $<"
	$(Q)$(OCAMLOPT) -shared -o $@ $<

DISTCLEAN += $(DEPENDS_$(d))

# Installing patonet.ml as a plugin
install: install-patonetml
.PHONY: install-patonetml
install-patonetml: install-plugins
	install -m 644 $(DRIVERS_DIR)/Net/patonet.c $(DESTDIR)/$(INSTALL_PLUGINS_DIR)


# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
