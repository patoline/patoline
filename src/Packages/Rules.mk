# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

PACKAGES_INCLUDES := -I $(d) $(PACK_FORMAT)
PACKAGES_DEPS_INCLUDES := -I $(d) $(DEPS_PACK_FORMAT)

# Find dependencies
$(d)/%.depends: INCLUDES += $(PACKAGES_DEPS_INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx: INCLUDES += $(PACKAGES_INCLUDES)

SRC_$(d) := $(wildcard $(d)/*.ml)
CMI_$(d) := $(SRC_$(d):.ml=.cmi)
CMO_$(d) := $(SRC_$(d):.ml=.cmo)
CMX_$(d) := $(SRC_$(d):.ml=.cmx)
PCMX_$(d) := $(SRC_$(d):.ml=.p.cmx)
DEPS_$(d) := $(SRC_$(d):.ml=.ml.depends)
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(addsuffix .depends,$(SRC_$(d)))
endif
endif

# Everything here depends on Typography and pa_patoline
$(CMO_$(d)): $(TYPOGRAPHY_DIR)/Typography.cma $(PA_PATOLINE)
$(CMX_$(d)): $(TYPOGRAPHY_DIR)/Typography.cmxa $(PA_PATOLINE)
$(PCMX_$(d)): $(TYPOGRAPHY_DIR)/Typography.cmxa $(PA_PATOLINE)
$(DEPS_$(d)): $(PA_PATOLINE)

# And compiles with it
$(CMO_$(d)): private OCPP=$(PA_PATOLINE)
$(CMX_$(d)): private OCPP=$(PA_PATOLINE)
$(PCMX_$(d)): private OCPP=$(PA_PATOLINE)
$(DEPS_$(d)): private OCPP=$(PA_PATOLINE)

# FIXME: currently: one package = one file

ALL_PACKAGES_CMXA := $(SRC_$(d):.ml=.cmxa)
ALL_PACKAGES_CMA  := $(SRC_$(d):.ml=.cma)

# Tell make that we want to build all the packages
packages: $(ALL_PACKAGES_CMXA) $(ALL_PACKAGES_CMA)

$(ALL_PACKAGES_CMXA): %.cmxa: %.cmx
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) $(OFLAGS) -o $@ -a $^

$(ALL_PACKAGES_CMA): %.cma: %.cmo
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLC) $(OFLAGS) -o $@ -a $^

# Cleaning
CLEAN += $(d)/*.cmi $(d)/*.cmo $(d)/*.cma $(d)/*.cmxa $(d)/*.a $(d)/*.o $(d)/*.cmx
DISTCLEAN += $(d)/*.depends


# Installing
install: install-packages
.PHONY: install-packages

# install-format depends on install-typography, since we first must wait
# for $(DESTDIR)/$(INSTALL_TYPOGRAPHY_DIR) directory to be created
# before putting formats in it.
PACKAGES_INSTALL :=  $(ALL_PACKAGES_CMXA) $(ALL_PACKAGES_CMA) $(CMI_$(d)) $(CMO_$(d))\
  $(CMX_$(d)) $(SRC_$(d):.ml=.a) $(d)/META
install-packages: install-typography $(ALL_PACKAGES_CMXA) $(ALL_PACKAGES_CMA)
	install -m 755 -d $(DESTDIR)/$(INSTALL_PACKAGES_DIR)
	install -p -m 644  $(PACKAGES_INSTALL) $(DESTDIR)/$(INSTALL_PACKAGES_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
