# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

NET_DRIVER_INCLUDES:=-I $(DRIVERS_DIR)/SVG -I $(TYPOGRAPHY_DIR)

$(d)/%.ml.depends: INCLUDES += $(NET_DRIVER_INCLUDES)

SRC_$(d):=$(wildcard $(d)/*.ml)
DEPENDS_$(d) := $(addsuffix .depends,$(SRC_$(d)))
-include $(DEPENDS_$(d))

$(d)/Patonet.cmx: $(d)/Patonet.ml
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) -thread $(OFLAGS) -package $(PACK_DRIVER_Patonet) $(INCLUDES) $(DRIVERS_INCLUDES) $(NET_DRIVER_INCLUDES) -o $@ -c $<

$(d)/Patonet.cmxa: $(d)/Patonet.cmx
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) -thread -package $(PACK_DRIVER_Patonet) $(INCLUDES) $(DRIVERS_INCLUDES) $(NET_DRIVER_INCLUDES) $(OFLAGS) -a -o $@ $<

$(d)/Patonet.cmxs: $(d)/Patonet.cmx
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) -thread $(INCLUDES) $(DRIVERS_INCLUDES) $(NET_DRIVER_INCLUDES) $(OFLAGS) -linkpkg -shared -o $@ $<

DISTCLEAN += $(DEPENDS_$(d))

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))

