# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

NET_DRIVER_INCLUDES:=-I $(DRIVERS_DIR)/SVG -I $(TYPOGRAPHY_DIR)

$(d)/%.ml.depends: INCLUDES += $(NET_DRIVER_INCLUDES)

SRC_$(d):=$(wildcard $(d)/*.ml)
DEPENDS_$(d) := $(addsuffix .depends,$(SRC_$(d)))
-include $(DEPENDS_$(d))

$(d)/Net.cmo: %.cmo: %.ml $(TYPOGRAPHY_DIR)/Typography.cma
	$(ECHO) "[OCAMLC]    $<"
	$(Q)$(OCAMLC) $(OFLAGS) $(PACK) $(INCLUDES) $(DRIVERS_INCLUDES) $(NET_DRIVER_INCLUDES) -o $@ -c $<

$(d)/Net.cmx: %.cmx: %.ml $(TYPOGRAPHY_DIR)/Typography.cmxa
	$(ECHO) "[OPT]    $<"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(PACK) $(INCLUDES) $(DRIVERS_INCLUDES) $(NET_DRIVER_INCLUDES) -o $@ -c $<

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

