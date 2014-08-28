# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

PATONET_DRIVER_INCLUDES:= -I $(d) -I $(DRIVERS_DIR)/SVG $(PACK_DRIVER_Patonet)

$(d)/%.ml.depends: INCLUDES += $(PATONET_DRIVER_INCLUDES) 
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx: INCLUDES += $(PATONET_DRIVER_INCLUDES)

SRC_$(d):=$(wildcard $(d)/*.ml)
DEPENDS_$(d) := $(addsuffix .depends,$(SRC_$(d)))
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(DEPENDS_$(d))
endif
endif

# cmi race condition
$(d)/Patonet.cmx: %.cmx: %.cmo
$(d)/Hammer.cmx: %.cmx: %.cmo

$(d)/Patonet.cma: $(d)/Hammer.cmo $(d)/Patonet.cmo
	$(ECHO) "[MKLIB]    $< -> $@"
	$(Q)$(OCAMLC) $(INCLUDES) $(OFLAGS) -a -o $@ $^

$(d)/Patonet.cmxa: $(d)/Hammer.cmx $(d)/Patonet.cmx
	$(ECHO) "[OMKLIB]    $< -> $@"
	$(Q)$(OCAMLOPT) $(INCLUDES) $(OFLAGS) -a -o $@ $^

$(d)/Patonet.cmxs: $(d)/Hammer.cmx $(d)/Patonet.cmx
	$(ECHO) "[SHARE]    $< -> $@"
	$(Q)$(OCAMLOPT) $(INCLUDES) $(OFLAGS) -linkpkg -shared -o $@ $^

$(d)/Hammer.ml: $(d)/Hammer.js
	$(ECHO) "[JSTOML] $< -> $@"
	$(Q)$(OCAML) ./Tools/file_to_string.ml $< > $@

CLEAN += $(d)/Hammer.ml

DISTCLEAN += $(DEPENDS_$(d))

Hammer.ml.depends: Hammer.ml

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))

