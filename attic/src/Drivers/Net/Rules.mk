# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

NET_DRIVER_INCLUDES := \
	-I $(d) $(PACK_DRIVER_Net) -I $(DRIVERS_DIR)/SVG -I $(CONFIG_DIR)

$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx: INCLUDES += $(NET_DRIVER_INCLUDES)

SRC_$(d):=$(wildcard $(d)/*.ml)
DEPENDS_$(d) := $(addsuffix .depends,$(SRC_$(d)))
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(DEPENDS_$(d))
endif
endif

$(d)/Net.cma: $(d)/Net.cmo
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLC) -a -o $@ $<

$(d)/Net.cmxa: $(d)/Net.cmx
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) -a -o $@ $<

$(d)/Net.cmxs: $(d)/Net.cmx
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) -shared -o $@ $<

DISTCLEAN += $(DEPENDS_$(d))


# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
