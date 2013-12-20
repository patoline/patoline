# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

SVG_DRIVER_ML:=$(d)/HtmlFonts.ml $(d)/SVG.ml
SVG_DRIVER_CMO:=$(SVG_DRIVER_ML:.ml=.cmo)
SVG_DRIVER_CMX:=$(SVG_DRIVER_ML:.ml=.cmx)

-include $(SVG_DRIVER_ML:.ml=.ml.depends)

$(SVG_DRIVER_CMX): %.cmx: %.ml $(TYPOGRAPHY_DIR)/Typography.cmxa
	$(ECHO) "[OPT]    $<"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(PACK) $(INCLUDES) -I $(<D) $(DRIVERS_INCLUDES) -o $@ -c $<

$(d)/SVG.cmxa: $(SVG_DRIVER_CMX)
	$(ECHO) "[LINK]   $<"
	$(Q)$(OCAMLOPT) -a $(PACK) $(INCLUDES) -I $(<D) $(DRIVERS_INCLUDES) -o $@ $^

$(d)/SVG.cmxs: $(SVG_DRIVER_CMX)
	$(ECHO) "[LINK]   $<"
	$(Q)$(OCAMLOPT) -shared $(PACK) $(INCLUDES) -I $(<D) $(DRIVERS_INCLUDES) -o $@ $^

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
