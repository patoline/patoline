# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

DRIVERS_INCLUDES:=-I $(TYPOGRAPHY_DIR) -I $(UTIL_DIR) -I $(UTIL_DIR)/Rbuffer -I $(LIBFONTS_DIR) -I $(LIBFONTS_DIR)/CFF -I $(LIBFONTS_DIR)/Opentype

DRIVERS_CMXA:=$(foreach drv,$(DRIVERS),src/Drivers/$(drv)/$(drv).cmxa)
LIB_DRIVERS_A:=$(wildcard $(d)/*/lib*.a)
DRIVERS_CMX:=$(DRIVERS_CMXA:.cmxa=.cmx)
DRIVERS_CMXS:=$(DRIVERS_CMXA:.cmxa=.cmxs)

# Building stuff
all: $(DRIVERS_CMXA) $(DRIVERS_CMXS)

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

$(DRIVERS_WITHOUT_RULES_MK:.cmxa=.cmxs): %.cmxs: %.cmx
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(DRIVERS_INCLUDES) $(PACK) $(INCLUDES) -linkpkg -shared -o $@ $<

# Find dependencies
-include $(DRIVERS_CMXA:.cmxa=.ml.depends)

CLEAN+=$(DRIVERS_CMXA) $(DRIVERS_CMX) \
       $(d)/*.cmo $(d)/*.cmi $(d)/*.o $(d)/*.cmx $(d)/*.a $(d)/*.so $(d)/*.cmxa \
       $(d)/*/*.cmo $(d)/*/*.cmi $(d)/*/*.o $(d)/*/*.cmx $(d)/*/*.a $(d)/*/*.so \
			 $(d)/*/*.cmxs
DISTCLEAN+=$(d)/*/*.depends

# Installing drivers
install: install-drivers
.PHONY: install-drivers

# install-drivers depends on install-typography, since we first must wait
# for $(DESTDIR)/$(INSTALL_TYPOGRAPHY_DIR) directory to be created
# before putting drivers in it.
install-drivers: install-typography \
  $(DRIVERS_CMXA) $(DRIVERS_CMXA:.cmxa=.a) $(DRIVERS_CMXA:.cmxa=.cmi) $(DRIVERS_CMXA:.cmxa=.cmxs)
	install -d -m 755 $(DESTDIR)/$(INSTALL_DRIVERS_DIR)
	install -p -m 644 $(DRIVERS_CMXA)  $(DRIVERS_CMXA:.cmxa=.a) $(LIB_DRIVERS_A)  $(DRIVERS_CMXA:.cmxa=.cmi) \
	  $(DRIVERS_CMXA:.cmxa=.cmxs) $(DESTDIR)/$(INSTALL_DRIVERS_DIR)

# Visit subdirectories
$(foreach mod,$(DRIVERS),$(eval -include $(d)/$$(mod)/Rules.mk))

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
