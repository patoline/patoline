# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

HTML_DRIVER_INCLUDES:=-I $(DRIVERS_DIR)/SVG

$(d)/Html.ml.depends: INCLUDES += $(HTML_DRIVER_INCLUDES)
-include $(d)/Html.ml.depends

$(d)/Html.cmx: %.cmx: %.ml $(TYPOGRAPHY_DIR)/Typography.cmxa
	$(ECHO) "[OPT]    $<"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(PACK) $(INCLUDES) $(DRIVERS_INCLUDES) $(HTML_DRIVER_INCLUDES) -o $@ -c $<

$(d)/Html.cmxa: %.cmxa: %.cmx $(DRIVERS_DIR)/SVG/HtmlFonts.cmx
	$(OCAMLOPT) $(DRIVERS_INCLUDES) $(HTML_DRIVER_INCLUDES) -a -o $@ $(DRIVERS_DIR)/SVG/HtmlFonts.cmx $<

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
