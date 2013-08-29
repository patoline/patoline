# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

FORMAT_INCLUDES := -I $(TYPOGRAPHY_DIR) -I $(RBUFFER_DIR) -I $(DRIVERS_DIR)/SVG

# Find dependencies
$(d)/%.depends: INCLUDES += $(FORMAT_INCLUDES)
SRC_$(d) := $(wildcard $(d)/*.ml)
CMO_$(d) := $(SRC_$(d):.ml=.cmo)
CMX_$(d) := $(SRC_$(d):.ml=.cmx)
PCMX_$(d) := $(SRC_$(d):.ml=.p.cmx)
-include $(addsuffix .depends,$(SRC_$(d)))

# Everything here depends on Typography
$(CMO_$(d)): $(TYPOGRAPHY_DIR)/Typography.cma
$(CMX_$(d)): $(TYPOGRAPHY_DIR)/Typography.cmxa
$(PCMX_$(d)): $(TYPOGRAPHY_DIR)/Typography.cmxa

# Build DefaultFormat
DEFAULT_FORMAT_ML := $(d)/Euler.ml $(d)/Numerals.ml $(d)/TableOfContents.ml \
  $(d)/DefaultFormat.ml

$(d)/DefaultFormat.cmxa: $(DEFAULT_FORMAT_ML:.ml=.cmx)
	$(ECHO) "[OPT]    $<"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(PACK) $(INCLUDES) -I $(<D) $(FORMAT_INCLUDES) -o $@ -a $^

$(d)/DefaultFormat.cma: $(DEFAULT_FORMAT_ML:.ml=.cmo)
	$(ECHO) "[OCAMLC] $<"
	$(Q)$(OCAMLC) $(OFLAGS) $(PACK) $(INCLUDES) -I $(<D) $(FORMAT_INCLUDES) -o $@ -a $^

$(d)/DefaultFormat.p.cma: $(DEFAULT_FORMAT_ML:.ml=.p.cmo)
	$(ECHO) "[OPT -p] $<"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(PACK) $(INCLUDES) -I $(<D) $(FORMAT_INCLUDES) -o $@ -p -a $^

# Other formats
OTHER_FORMATS := $(d)/FormatArticle.ml $(d)/FormatLivre.ml $(d)/FormatThese.ml \
  $(d)/FormatLetter.ml $(d)/FormatSlides.ml $(d)/LMArticle.ml $(d)/LMFormat.ml $(d)/FormatWeb.ml

$(OTHER_FORMATS:.ml=.cmxa): %.cmxa: %.cmx
	$(ECHO) "[OPT]    $<"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(PACK) $(INCLUDES) -I $(<D) $(FORMAT_INCLUDES) -o $@ -a $^

$(OTHER_FORMATS:.ml=.cma): %.cma: %.cmo
	$(ECHO) "[OCAMLC] $<"
	$(Q)$(OCAMLC) $(OFLAGS) $(PACK) $(INCLUDES) -I $(<D) $(FORMAT_INCLUDES) -o $@ -a $^

$(OTHER_FORMATS:.ml=.p.cmxa): %.p.cmxa: %.p.cmx
	$(ECHO) "[OPT -p] $<"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(PACK) $(INCLUDES) -I $(<D) $(FORMAT_INCLUDES) -o $@ -p -a $^

# Common ML rules with proper includes
$(CMO_$(d)): %.cmo: %.ml
	$(ECHO) "[OCAMLC] $<"
	$(Q)$(OCAMLC) $(OFLAGS) $(PACK) $(INCLUDES) -I $(<D) $(FORMAT_INCLUDES) -o $@ -c $<

$(CMX_$(d)): %.cmx: %.ml
	$(ECHO) "[OPT]    $<"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(PACK) $(INCLUDES) -I $(<D) $(FORMAT_INCLUDES) -o $@ -c $<

$(PCMX_$(d)): %.p.cmx: %.ml
	$(ECHO) "[OPT -p] $<"
	$(Q)$(OCAMLOPT) -p $(OFLAGS) $(PACK) $(INCLUDES) -I $(<D) $(FORMAT_INCLUDES) -o $@ -c $<

# Tell make that we want to build all formats
all: $(d)/DefaultFormat.cmxa $(OTHER_FORMATS:.ml=.cmxa)

# Cleaning
CLEAN += $(d)/*.cmi $(d)/*.cmo $(d)/*.cma $(d)/*.cmxa $(d)/*.a $(d)/*.o $(d)/*.cmx
DISTCLEAN += $(d)/*.depends

# Installing
ALL_FORMATS_CMXA := $(d)/DefaultFormat.cmxa $(OTHER_FORMATS:.ml=.cmxa)

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
