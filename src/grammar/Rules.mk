# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

# Building the default grammar
all: $(d)/DefaultGrammar.pdf $(d)/DefaultGrammar.tgy

$(d)/DefaultGrammar.tgy: $(d)/DefaultGrammar.ml

$(EDITORS_DIR)/emacs/quail.el: $(d)/DefaultGrammar.ml

$(d)/DefaultGrammar.ml: $(d)/DefaultGrammar.txp $(PA_PATOLINE_IN_SRC)
	$(ECHO) "[PPP] $@"
	$(Q)$(PA_PATOLINE_IN_SRC) --build-dir . --main --no-default-grammar --ascii \
		--quail-out $(EDITORS_DIR)/emacs/quail.el $< > $@

$(d)/DefaultGrammar_.ml: $(d)/DefaultGrammar.ml

TYPOGRAPHY_CMXA := \
	$(TYPOGRAPHY_DIR)/Typography.cmxa \
	$(TYPOGRAPHY_DIR)/DefaultFormat.cmxa

$(d)/DefaultGrammar.cmx: $(d)/DefaultGrammar.ml $(TYPOGRAPHY_CMXA)
	$(ECHO) "[OPT] $@"
	$(Q)$(OCAMLOPT_SIMPLE) $(PACK_FORMAT) -c -o $@ $<

$(d)/DefaultGrammar.tmx: $(d)/DefaultGrammar_.ml $(d)/DefaultGrammar.cmx \
                         $(RBUFFER_DIR)/rbuffer.cmxa $(UTIL_DIR)/patutil.cmxa \
                         $(LIBFONTS_DIR)/fonts.cmxa $(TYPOGRAPHY_DIR)/Typography.cmxa \
                         $(TYPOGRAPHY_DIR)/DefaultFormat.cmxa $(DRIVERS_DIR)/Pdf/Pdf.cmxa
	$(ECHO) "[OPT] $@"
	$(Q)rm $(GRAMMAR_DIR)/DefaultGrammar_.cmi 2>/dev/null || true #fix for deps
	$(Q)$(OCAMLOPT_NOPP) $(PACK_TYPOGRAPHY) $(PACK_FORMAT) $(PACK_DRIVER_Pdf) \
		-I $(<D) -I $(CONFIG_DIR) -linkpkg $(FONTCONFIG_CMXA) Pdf.cmxa \
		-o $@ $(@:.tmx=.cmx) $<

$(d)/DefaultGrammar.pdf: $(d)/DefaultGrammar.tmx $(PATOLINE_IN_SRC) \
	$(HYPHENATION_DIR)/hyph-en-us.hdict
	$(ECHO) "[TMX] $@"
	$(Q)$< --quiet --extra-fonts-dir $(FONTS_DIR) \
		--extra-hyph-dir $(HYPHENATION_DIR) --driver Pdf

# Cleaning
CLEAN += $(d)/DefaultGrammar.tgx  $(d)/DefaultGrammar_.ml  $(d)/DefaultGrammar.ml \
	 $(d)/DefaultGrammar.pdf  $(d)/DefaultGrammar.tdx  $(d)/DefaultGrammar.tmx \
	 $(d)/DefaultGrammar.cmi  $(d)/DefaultGrammar.cmx  $(d)/DefaultGrammar.o \
	 $(d)/DefaultGrammar_.cmi $(d)/DefaultGrammar_.cmx $(d)/DefaultGrammar_.o \
	 $(d)/DefaultGrammar_.dep $(d)/DefaultGrammar.tdep $(d)/DefaultGrammar.tgy \
	 $(EDITORS_DIR)/emacs/quail.el

# Installing
install: install-grammars
.PHONY: install-grammars

install-grammars: $(d)/DefaultGrammar.txp $(d)/DefaultGrammar.pdf $(d)/DefaultGrammar.tgy
	install -p -m 755 -d $(DESTDIR)/$(INSTALL_GRAMMARS_DIR)
	install -m 644 -p $^ $(DESTDIR)/$(INSTALL_GRAMMARS_DIR)/

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
