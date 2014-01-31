# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

# Useful directories, to be referenced from other Rules.ml
SRC_DIR := $(d)
PATOLINE_IN_SRC := $(d)/Patoline/patoline
PATOLINE_DIR := $(d)/Patoline
TYPOGRAPHY_DIR := $(d)/Typography
DRIVERS_DIR := $(d)/Drivers
FORMAT_DIR := $(d)/Format
UTIL_DIR := $(d)/Util
LIBFONTS_DIR := $(d)/Fonts
CESURE_DIR := $(d)/cesure

# Visit subdirectories
MODULES := Util Fonts Typography Drivers Patoline Pdf cesure Format \
  $(OCAML_BIBI) plot proof plugins
$(foreach mod,$(MODULES),$(eval include $(d)/$$(mod)/Rules.mk))

# Building Patoline's grammar
all: $(d)/DefaultGrammar.txp $(d)/DefaultGrammar.tgx
$(d)/DefaultGrammar.tgx: $(d)/DefaultGrammar.pdf

$(d)/quail.el: $(d)/DefaultGrammar.ttml ;
$(d)/DefaultGrammar_.tml: $(d)/DefaultGrammar.txp $(PATOLINE_IN_SRC)
	$(ECHO) "[PAT]    $< -> $@"
	$(Q)$(PATOLINE_IN_SRC) --main-ml --driver Pdf -o $@ $<

$(d)/DefaultGrammar.ttml: $(d)/DefaultGrammar.txp $(PATOLINE_IN_SRC)
	$(ECHO) "[PAT]    $< -> $@"
	$(Q)$(PATOLINE_IN_SRC) --ml --driver Pdf -o $@ $<

$(d)/DefaultGrammar.cmx: $(d)/DefaultGrammar.ttml $(TYPOGRAPHY_DIR)/Typography.cmxa $(FORMAT_DIR)/DefaultFormat.cmxa
	$(ECHO) "[OPT]    ... -> $@"
	$(Q)$(OCAMLOPT) $(PACK) -I $(UTIL_DIR) -I $(FORMAT_DIR) -I $(TYPOGRAPHY_DIR) Typography.cmxa -c -o $@ -impl $<

$(d)/DefaultGrammar.tmx: $(d)/DefaultGrammar_.tml $(d)/DefaultGrammar.cmx \
  $(UTIL_DIR)/util.cmxa $(LIBFONTS_DIR)/fonts.cmxa $(TYPOGRAPHY_DIR)/Typography.cmxa \
  $(FORMAT_DIR)/DefaultFormat.cmxa $(DRIVERS_DIR)/Pdf/Pdf.cmxa \
  $(TYPOGRAPHY_DIR)/ParseMainArgs.cmx 
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(PACK) -I $(<D) -I $(UTIL_DIR) -I $(FORMAT_DIR) -I $(DRIVERS_DIR) -I $(TYPOGRAPHY_DIR) $(UTIL_DIR)/util.cmxa dynlink.cmxa $(LIBFONTS_DIR)/fonts.cmxa Typography.cmxa -I $(DRIVERS_DIR)/Pdf Pdf.cmxa ParseMainArgs.cmx DefaultFormat.cmxa -linkpkg -o $@ $(@:.tmx=.cmx) -impl $<

$(d)/DefaultGrammar.pdf: $(d)/DefaultGrammar.tmx $(PATOLINE_IN_SRC) $(HYPHENATION_DIR)/hyph-en-us.hdict
	$(ECHO) "[TMX]    $< -> $@"      
	$(Q)$< --extra-fonts-dir $(FONTS_DIR) --extra-hyph-dir $(HYPHENATION_DIR) --extra-driver-dir $(DRIVERS_DIR)/Pdf --driver Pdf

CLEAN += $(d)/DefaultGrammar.tgx $(d)/DefaultGrammar_.tml $(d)/DefaultGrammar.ttml \
	 $(d)/DefaultGrammar.pdf $(d)/DefaultGrammar.tdx  $(d)/DefaultGrammar.tmx \
	 $(d)/DefaultGrammar.cmi $(d)/DefaultGrammar.cmx $(d)/DefaultGrammar.o \
	 $(d)/DefaultGrammar_.cmi $(d)/DefaultGrammar_.cmx $(d)/DefaultGrammar_.o \
	 $(d)/quail.el

# Installing
install: install-grammars
.PHONY: install-grammars

install-grammars: $(d)/DefaultGrammar.txp $(d)/DefaultGrammar.tgx
	install -p -m 755 -d $(DESTDIR)/$(INSTALL_GRAMMARS_DIR)
	install -p -m 644 $(SRC_DIR)/DefaultGrammar.txp $(DESTDIR)/$(INSTALL_GRAMMARS_DIR)/
	install -p -m 644 $(SRC_DIR)/DefaultGrammar.tgx $(DESTDIR)/$(INSTALL_GRAMMARS_DIR)/

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
