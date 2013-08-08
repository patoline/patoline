# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

NET_DRIVER_INCLUDES:=-I $(DRIVERS_DIR)/SVG -I $(TYPOGRAPHY_DIR)

$(d)/%.ml.depends: INCLUDES += $(NET_DRIVER_INCLUDES)

SRC_$(d):=$(wildcard $(d)/*.ml)
DEPENDS_$(d) := $(addsuffix .depends,$(SRC_$(d)))
-include $(DEPENDS_$(d))

$(d)/Net.cmx: %.cmx: %.ml $(TYPOGRAPHY_DIR)/Typography.cmxa
	$(ECHO) "[OPT]    $<"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(PACK) -package $(PACK_DRIVER_Net) $(INCLUDES) $(DRIVERS_INCLUDES) $(NET_DRIVER_INCLUDES) -o $@ -c $<

$(d)/Net.cmxa: $(d)/Net.cmx
	$(ECHO) "[LINK]   $<"
	$(Q)$(OCAMLOPT) -a -o $@ $<

DISTCLEAN += $(DEPENDS_$(d))

# Installing patonet.ml as a plugin
install: install-patonetml
.PHONY: install-patonetml
install-patonetml: install-plugins
	install -m 644 $(DRIVERS_DIR)/Net/patonet.ml $(DESTDIR)/$(INSTALL_PLUGINS_DIR)
	install -m 644 $(DRIVERS_DIR)/Net/patonet.c $(DESTDIR)/$(INSTALL_PLUGINS_DIR)


# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
