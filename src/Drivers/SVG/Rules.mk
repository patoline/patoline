# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

SVG_DRIVER_INCLUDES := -I $(d) $(PACK_DRIVER_SVG)
$(d)/%.depends : INCLUDES += $(SVG_DRIVER_INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx: INCLUDES += $(SVG_DRIVER_INCLUDES)

SVG_DRIVER_ML:=$(d)/HtmlFonts.ml $(d)/SVG.ml
SVG_DRIVER_CMO:=$(SVG_DRIVER_ML:.ml=.cmo)
SVG_DRIVER_CMX:=$(SVG_DRIVER_ML:.ml=.cmx)

-include $(SVG_DRIVER_ML:.ml=.ml.depends)
$(SVG_DRIVER_CMX): %.cmx: %.cmo

$(d)/SVG.cma: $(SVG_DRIVER_CMO)
	$(ECHO) "[MKLIB]   $<"
	$(Q)$(OCAMLC) -a $(INCLUDES) -o $@ $^

$(d)/SVG.cmxa: $(SVG_DRIVER_CMX)
	$(ECHO) "[OMKLIB]   $<"
	$(Q)$(OCAMLOPT) -a $(INCLUDES) -o $@ $^

$(d)/SVG.cmxs: $(SVG_DRIVER_CMX)
	$(ECHO) "[SHARE]   $<"
	$(Q)$(OCAMLOPT) -shared $(INCLUDES) -o $@ $^

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
