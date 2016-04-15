# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

SVG_DRIVER_INCLUDES := -I $(d) $(PACK_DRIVER_SVG)
SVG_DRIVER_DEPS_INCLUDES:=-I $(d) $(DEPS_PACK_DRIVER_SVG)
$(d)/%.depends : INCLUDES:=$(SVG_DRIVER_DEPS_INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx $(d)/%.cma $(d)/%.cmxa $(d)/%.cmxs: INCLUDES += $(SVG_DRIVER_INCLUDES)

SVG_DRIVER_ML:=$(wildcard $(d)/*.ml)
SVG_DRIVER_CMO:=$(SVG_DRIVER_ML:.ml=.cmo)
SVG_DRIVER_CMX:=$(SVG_DRIVER_ML:.ml=.cmx)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(SVG_DRIVER_ML:.ml=.ml.depends)
endif
endif

$(SVG_DRIVER_CMX): %.cmx: %.cmo

$(d)/SVG.cma: $(SVG_DRIVER_CMO)
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLC) -a $(INCLUDES) -o $@ $^

$(d)/SVG.cmxa: $(SVG_DRIVER_CMX)
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) -a $(INCLUDES) -o $@ $^

$(d)/SVG.cmxs: $(SVG_DRIVER_CMX)
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) -shared $(INCLUDES) -o $@ $^

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
