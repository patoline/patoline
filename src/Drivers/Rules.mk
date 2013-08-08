# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

DRIVERS_INCLUDES:=-I $(TYPOGRAPHY_DIR) -I $(RBUFFER_DIR)

DRIVERS_CMXA:=$(foreach drv,$(DRIVERS),src/Drivers/$(drv)/$(drv).cmxa)
DRIVERS_CMX:=$(DRIVERS_CMXA:.cmxa=.cmx)

# Building stuff
all: $(DRIVERS_CMXA)

# Find who has a Rules.mk, in which case we won't apply the generic
# %.cmxa: %.cmx rule from this file.
DRIVERS_WITH_RULES_MK := $(foreach drv,$(patsubst %/Rules.mk,%,$(wildcard $(d)/*/Rules.mk)),$(drv)/$(notdir $(drv)).cmxa)
DRIVERS_WITHOUT_RULES_MK := $(filter-out $(DRIVERS_WITH_RULES_MK),$(DRIVERS_CMXA))

$(DRIVERS_WITHOUT_RULES_MK:.cmxa=.cmx): %.cmx: %.ml $(TYPOGRAPHY_DIR)/Typography.cmxa
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(PACK) $(INCLUDES) -I $(<D) $(DRIVERS_INCLUDES) -o $@ -c $<

$(DRIVERS_WITHOUT_RULES_MK): %.cmxa: %.cmx
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(DRIVERS_INCLUDES) -a -o $@ $<

# Find dependencies
-include $(DRIVERS_CMXA:.cmxa=.ml.depends)

CLEAN+=$(DRIVERS_CMXA) $(DRIVERS_CMX) $(d)/*/*.cmo $(d)/*/*.cmi $(d)/*/*.o $(d)/*/*.cmx $(d)/*/*.a $(d)/*/*.so
DISTCLEAN+=$(d)/*/*.depends

# Installing drivers
install: install-drivers
.PHONY: install-drivers

# install-drivers depends on install-typography, since we first must wait
# for $(DESTDIR)/$(INSTALL_TYPOGRAPHY_DIR) directory to be created
# before putting drivers in it.
install-drivers: install-typography \
  $(DRIVERS_CMXA) $(DRIVERS_CMXA:.cmxa=.a) $(DRIVERS_CMXA:.cmxa=.cmi)
	install -p -m 644 $(DRIVERS_CMXA) $(DRIVERS_CMXA:.cmxa=.a) $(DRIVERS_CMXA:.cmxa=.cmi) \
	  $(DESTDIR)/$(INSTALL_TYPOGRAPHY_DIR)

# Visit subdirectories
$(foreach mod,$(DRIVERS),$(eval -include $(d)/$$(mod)/Rules.mk))

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
