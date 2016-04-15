# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

# Building the default grammar
all: $(d)/DefaultGrammar.pdf $(d)/_patobuild/DefaultGrammar.tgy

$(d)/_patobuild/DefaultGrammar.tgy: $(d)/DefaultGrammar.ttml

$(EDITORS_DIR)/emacs/quail.el: $(d)/DefaultGrammar.ttml

$(d)/DefaultGrammar.ttml: $(d)/DefaultGrammar.txp $(PA_PATOLINE_IN_SRC)
	$(ECHO) "[PAPAT]  $< -> $@"
	$(Q)$(PA_PATOLINE_IN_SRC) --ascii --quail-out $(EDITORS_DIR)/emacs/quail.el --driver Pdf --no-default-grammar $< > $@

$(d)/DefaultGrammar_.tml: $(d)/DefaultGrammar.txp $(PATOLINE_IN_SRC)
	$(ECHO) "[PAT]    $< -> $@"
	$(Q)$(PATOLINE_IN_SRC) --no-build-dir --main-ml --driver Pdf -o $@ $<

$(d)/DefaultGrammar.cmx: $(d)/DefaultGrammar.ttml $(TYPOGRAPHY_DIR)/Typography.cmxa $(TYPOGRAPHY_DIR)/DefaultFormat.cmxa
	$(ECHO) "[OPT]    ... -> $@"
	$(Q)$(OCAMLOPT_NOINTF) -intf-suffix .mli $(PACK_FORMAT) -c -o $@ -impl $<

$(d)/DefaultGrammar.tmx: $(d)/DefaultGrammar_.tml $(d)/DefaultGrammar.cmx \
  $(RBUFFER_DIR)/rbuffer.cmxa $(UTIL_DIR)/patutil.cmxa \
	$(LIBFONTS_DIR)/fonts.cmxa $(TYPOGRAPHY_DIR)/Typography.cmxa \
  $(TYPOGRAPHY_DIR)/DefaultFormat.cmxa $(DRIVERS_DIR)/Pdf/Pdf.cmxa
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT_NOPP) $(PACK_TYPOGRAPHY) $(PACK_FORMAT) $(PACK_DRIVER_Pdf) \
		-I $(<D) -I $(CONFIG_DIR) \
		rbuffer.cmxa sqlite3.cmxa unicodelib.cmxa patutil.cmxa fonts.cmxa \
		zip.cmxa bigarray.cmxa imagelib.cmxa dynlink.cmxa rawlib.cmxa \
		patoconfig.cmxa unix.cmxa $(FONTCONFIG_CMXA) Typography.cmxa Pdf.cmxa \
		cesure.cmxa DefaultFormat.cmxa -o $@ $(@:.tmx=.cmx) -impl $<

$(d)/DefaultGrammar.pdf: $(d)/DefaultGrammar.tmx $(PATOLINE_IN_SRC) $(HYPHENATION_DIR)/hyph-en-us.hdict
	$(ECHO) "[TMX]    $< -> $@"
	$(Q)$< --extra-fonts-dir $(FONTS_DIR) --extra-hyph-dir $(HYPHENATION_DIR) --driver Pdf

# Cleaning
CLEAN += $(d)/DefaultGrammar.tgx $(d)/DefaultGrammar_.tml $(d)/DefaultGrammar.ttml \
	 $(d)/DefaultGrammar.pdf $(d)/DefaultGrammar.tdx  $(d)/DefaultGrammar.tmx \
	 $(d)/DefaultGrammar.cmi $(d)/DefaultGrammar.cmx $(d)/DefaultGrammar.o \
	 $(d)/DefaultGrammar_.cmi $(d)/DefaultGrammar_.cmx $(d)/DefaultGrammar_.o \
	 $(d)/DefaultGrammar_.dep $(d)/DefaultGrammar.tdep $(EDITORS_DIR)/emacs/quail.el

CLEANR += $(d)/_patobuild

# Installing
install: install-grammars
.PHONY: install-grammars

install-grammars: $(d)/DefaultGrammar.txp $(d)/DefaultGrammar.pdf $(d)/_patobuild/DefaultGrammar.tgy
	install -p -m 755 -d $(DESTDIR)/$(INSTALL_GRAMMARS_DIR)
	install -m 644 -p $^ $(DESTDIR)/$(INSTALL_GRAMMARS_DIR)/

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
