# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

SVG_INCLUDES := -I $(d) $(PACK_DRIVER_SVG)
SVG_DRIVER_ML:=$(d)/HtmlFonts.ml $(d)/SVG.ml
SVG_DRIVER_CMO:=$(SVG_DRIVER_ML:.ml=.cmo)
SVG_DRIVER_CMX:=$(SVG_DRIVER_ML:.ml=.cmx)

$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx $(d)/%.p.cmx: INCLUDES += $(SVG_INCLUDES)

-include $(SVG_DRIVER_ML:.ml=.ml.depends)
$(SVG_DRIVER_CMX): %.cmx: %.cmo

$(d)/SVG.cma: $(SVG_DRIVER_CMO)
	$(ECHO) "[MKLIB]   $<"
	$(Q)$(OCAMLC) -a $(SVG_INCLUDES) -o $@ $^

$(d)/SVG.cmxa: $(SVG_DRIVER_CMX)
	$(ECHO) "[OMKLIB]   $<"
	$(Q)$(OCAMLOPT) -a $(SVG_INCLUDES) -o $@ $^

$(d)/SVG.cmxs: $(SVG_DRIVER_CMX)
	$(ECHO) "[SHARE]   $<"
	$(Q)$(OCAMLOPT) -shared $(SVG_INCLUDES) -o $@ $^

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
