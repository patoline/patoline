# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

NET_DRIVER_INCLUDES:= -I $(DRIVERS_DIR)/SVG -I $(d)
COUCOU:=-I $(d)

$(d)/%.ml.depends: INCLUDES += $(NET_DRIVER_INCLUDES) 

SRC_$(d):=$(wildcard $(d)/*.ml)
DEPENDS_$(d) := $(addsuffix .depends,$(SRC_$(d)))
-include $(DEPENDS_$(d))

$(d)/Patonet.cmx: $(d)/Patonet.ml
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(OFLAGS) -package $(PACK_DRIVER_Patonet) $(INCLUDES) $(DRIVERS_INCLUDES) $(NET_DRIVER_INCLUDES) $(COUCOU) -o $@ -c $<

$(d)/Patonet.cmxa: $(d)/Hammer.cmx $(d)/Patonet.cmx
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) -package $(PACK_DRIVER_Patonet) $(INCLUDES) $(DRIVERS_INCLUDES) $(NET_DRIVER_INCLUDES) $(OFLAGS) -a -o $@ $^

$(d)/Patonet.cmxs: $(d)/Hammer.cmx $(d)/Patonet.cmx
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(INCLUDES) $(DRIVERS_INCLUDES) $(NET_DRIVER_INCLUDES) $(OFLAGS) -linkpkg -shared -o $@ $^

$(d)/Hammer.ml: $(d)/Hammer.js
	$(ECHO) "[JSTOML] $< -> $@"
	$(Q)$(OCAML) ./Tools/file_to_string.ml $< > $@

DISTCLEAN += $(DEPENDS_$(d))

Hammer.ml.depends: Hammer.ml

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))

