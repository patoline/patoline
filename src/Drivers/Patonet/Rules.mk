# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

PATONET_DRIVER_INCLUDES:= -I $(DRIVERS_DIR)/SVG -I $(d)

$(d)/%.ml.depends: INCLUDES += $(PATONET_DRIVER_INCLUDES) 

SRC_$(d):=$(wildcard $(d)/*.ml)
DEPENDS_$(d) := $(addsuffix .depends,$(SRC_$(d)))
-include $(DEPENDS_$(d))
# cmi race condition
$(d)/Patonet.cmx: %.cmx: %.cmo
$(d)/Hammer.cmx: %.cmx: %.cmo

$(d)/Patonet.cmo: %.cmo: %.ml
	$(ECHO) "[OCAMLC]    $< -> $@"
	$(Q)$(OCAMLC) $(OFLAGS) -package $(PACK_DRIVER_Patonet) $(INCLUDES) -I $(<D) $(DRIVERS_INCLUDES) $(PATONET_DRIVER_INCLUDES) -o $@ -c $<

$(d)/Patonet.cmx: %.cmx: %.ml
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(OFLAGS) -package $(PACK_DRIVER_Patonet) $(INCLUDES) -I $(<D) $(DRIVERS_INCLUDES) $(PATONET_DRIVER_INCLUDES) -o $@ -c $<

$(d)/Patonet.cma: $(d)/Hammer.cmo $(d)/Patonet.cmo
	$(ECHO) "[MKLIB]    $< -> $@"
	$(Q)$(OCAMLC) -package $(PACK_DRIVER_Patonet) $(INCLUDES) $(DRIVERS_INCLUDES) $(PATONET_DRIVER_INCLUDES) $(OFLAGS) -a -o $@ $^

$(d)/Patonet.cmxa: $(d)/Hammer.cmx $(d)/Patonet.cmx
	$(ECHO) "[OMKLIB]    $< -> $@"
	$(Q)$(OCAMLOPT) -package $(PACK_DRIVER_Patonet) $(INCLUDES) $(DRIVERS_INCLUDES) $(PATONET_DRIVER_INCLUDES) $(OFLAGS) -a -o $@ $^

$(d)/Patonet.cmxs: $(d)/Hammer.cmx $(d)/Patonet.cmx
	$(ECHO) "[SHARE]    $< -> $@"
	$(Q)$(OCAMLOPT) $(INCLUDES) $(DRIVERS_INCLUDES) $(PATONET_DRIVER_INCLUDES) $(OFLAGS) -linkpkg -shared -o $@ $^

$(d)/Hammer.ml: $(d)/Hammer.js
	$(ECHO) "[JSTOML] $< -> $@"
	$(Q)$(OCAML) ./Tools/file_to_string.ml $< > $@

CLEAN += $(d)/Hammer.ml

DISTCLEAN += $(DEPENDS_$(d))

Hammer.ml.depends: Hammer.ml

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))

