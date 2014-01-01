# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

IMAGE_DRIVER_INCLUDES:=-I $(DRIVERS_DIR)/DriverGL -I $(TYPOGRAPHY_DIR) -I $(PATOLINE_DIR)

$(d)/%.ml.depends: INCLUDES += $(IMAGE_DRIVER_INCLUDES)

SRC_$(d):=$(wildcard $(d)/*.ml)
DEPENDS_$(d) := $(addsuffix .depends,$(SRC_$(d)))
-include $(DEPENDS_$(d))

$(d)/Image.cmx: $(PATOLINE_DIR)/Language.cmx
$(d)/Image.cmx: %.cmx: %.ml $(TYPOGRAPHY_DIR)/Typography.cmxa
	$(ECHO) "[OPT]    $<"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(PACK) -package lablgl $(INCLUDES) $(DRIVERS_INCLUDES) $(IMAGE_DRIVER_INCLUDES) -o $@ -c $<

$(d)/Image.cmxa: $(PATOLINE_DIR)/Language.cmx $(d)/Image.cmx
	$(ECHO) "[OMKLIB] ... -> $@"
	$(Q)$(OCAMLMKLIB) $(PACK) -package lablgl $(INCLUDES) $(DRIVERS_INCLUDES) $(IMAGE_DRIVER_INCLUDES) -o $(basename $@) $^

$(d)/Image.cmxs: $(PATOLINE_DIR)/Language.cmx $(d)/Image.cmx
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) -shared -o $@ $^

DISTCLEAN += $(DEPENDS_$(d))

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
