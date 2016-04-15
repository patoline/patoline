# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

HTML_DRIVER_INCLUDES:=-I $(d) -I $(DRIVERS_DIR)/SVG $(PACK_DRIVER_Html)
HTML_DRIVER_DEPS_INCLUDES:=-I $(d) -I $(DRIVERS_DIR)/SVG $(DEPS_PACK_DRIVER_Html)
$(d)/%.depends : INCLUDES += $(HTML_DRIVER_DEPS_INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx: INCLUDES += $(HTML_DRIVER_INCLUDES)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(d)/Html.ml.depends
endif
endif

# cmi race condition
$(d)/Html.cmx: %.cmx: %.cmo

$(d)/Html.cma: %.cma: %.cmo $(DRIVERS_DIR)/SVG/HtmlFonts.cmo
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLC) $(INCLUDES) -a -o $@ $(DRIVERS_DIR)/SVG/HtmlFonts.cmo $<

$(d)/Html.cmxa: %.cmxa: %.cmx $(DRIVERS_DIR)/SVG/HtmlFonts.cmx
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) $(INCLUDES) -a -o $@ $(DRIVERS_DIR)/SVG/HtmlFonts.cmx $<

$(d)/Html.cmxs: %.cmxs: %.cmx $(DRIVERS_DIR)/SVG/HtmlFonts.cmx
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) $(INCLUDES) -shared -o $@ $(DRIVERS_DIR)/SVG/HtmlFonts.cmx $<

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
