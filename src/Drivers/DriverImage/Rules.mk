# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

IMAGE_DRIVER_INCLUDES:=-I $(d) $(PACK_DRIVER_DriverImage)
IMAGE_DRIVER_DEPS_INCLUDES:=-I $(d) $(DEPS_PACK_DRIVER_DriverImage)

$(d)/%.ml.depends: INCLUDES += $(IMAGE_DRIVER_DEPS_INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx: INCLUDES += $(IMAGE_DRIVER_INCLUDES)

SRC_$(d):=$(wildcard $(d)/*.ml)
DEPENDS_$(d) := $(addsuffix .depends,$(SRC_$(d)))
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(DEPENDS_$(d))
endif
endif

# cmi race condition
$(d)/DriverImage.cmx: %.cmx: %.cmo

$(d)/DriverImage.cma: $(d)/DriverImage.cmo
	$(ECHO) "[LNk] $@"
	$(Q)$(OCAMLC) -a $(INCLUDES) -o $@ $^

$(d)/DriverImage.cmxa: $(d)/DriverImage.cmx
	$(ECHO) "[LNk] $@"
	$(Q)$(OCAMLOPT) -a $(INCLUDES) -o $@ $^

$(d)/DriverImage.cmxs: $(d)/DriverImage.cmx
	$(ECHO) "[LNk] $@"
	$(Q)$(OCAMLOPT) -shared -o $@ $^

DISTCLEAN += $(DEPENDS_$(d))

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
