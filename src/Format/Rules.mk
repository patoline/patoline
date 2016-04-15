# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

FORMAT_INCLUDES := -I $(d) -I $(DRIVERS_DIR)/SVG $(PACK_FORMAT)
FORMAT_DEPS_INCLUDES := -I $(d) -I $(DRIVERS_DIR)/SVG $(DEPS_PACK_FORMAT)

# Find dependencies
$(d)/%.depends: INCLUDES += $(FORMAT_DEPS_INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx: INCLUDES += $(FORMAT_INCLUDES)

SRC_$(d) := $(wildcard $(d)/*.ml)
CMO_$(d) := $(SRC_$(d):.ml=.cmo)
CMX_$(d) := $(SRC_$(d):.ml=.cmx)
PCMX_$(d) := $(SRC_$(d):.ml=.p.cmx)
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(addsuffix .depends,$(SRC_$(d)))
endif
endif

# Everything here depends on Typography
$(CMO_$(d)): $(TYPOGRAPHY_DIR)/Typography.cma
$(CMX_$(d)): $(TYPOGRAPHY_DIR)/Typography.cmxa
$(PCMX_$(d)): $(TYPOGRAPHY_DIR)/Typography.cmxa

# Other formats
OTHER_FORMATS := $(d)/FormatArticle.ml $(d)/FormatLivre.ml $(d)/FormatThese.ml \
  $(d)/FormatLetter.ml $(d)/FormatSlides.ml $(d)/LMFormat.ml $(d)/FormatWeb.ml \
  $(d)/FormatMemoire.ml $(d)/Interactive.ml 

$(OTHER_FORMATS:.ml=.cmxa): %.cmxa: %.cmx
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(FORMAT_INCLUDES) -o $@ -a $^

$(OTHER_FORMATS:.ml=.cma): %.cma: %.cmo
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLC) $(OFLAGS) $(FORMAT_INCLUDES) -o $@ -a $^

# Common ML rules with proper includes
$(CMO_$(d)): %.cmo: %.ml
	$(ECHO) "[BYT] $@"
	$(Q)$(OCAMLC) $(OFLAGS) $(FORMAT_INCLUDES) -o $@ -c $<

$(CMX_$(d)): %.cmx: %.ml
	$(ECHO) "[OPT] $@"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(FORMAT_INCLUDES) -o $@ -c $<

# Tell make that we want to build all formats
all: $(OTHER_FORMATS:.ml=.cmxa) $(OTHER_FORMATS:.ml=.cma)

# Cleaning
CLEAN += $(d)/*.cmi $(d)/*.cmo $(d)/*.cma $(d)/*.cmxa $(d)/*.a $(d)/*.o $(d)/*.cmx
DISTCLEAN += $(d)/*.depends

# Installing
ALL_FORMATS_CMXA := $(OTHER_FORMATS:.ml=.cmxa)

install: install-format
.PHONY: install-format

# install-format depends on install-typography, since we first must wait
# for $(DESTDIR)/$(INSTALL_TYPOGRAPHY_DIR) directory to be created
# before putting formats in it.
install-format: install-typography \
  $(ALL_FORMATS_CMXA) $(ALL_FORMATS_CMXA:.cmxa=.a) $(ALL_FORMATS_CMXA:.cmxa=.cmi)
	install -p -m 644 $(ALL_FORMATS_CMXA) $(ALL_FORMATS_CMXA:.cmxa=.a) $(ALL_FORMATS_CMXA:.cmxa=.cmi) \
	  $(DESTDIR)/$(INSTALL_TYPOGRAPHY_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
