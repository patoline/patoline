# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

LIBFONTS_INCLUDES := -I $(d) -I $(d)/CFF -I $(d)/Opentype -I $(UTIL_DIR) -I $(UTIL_DIR)/Rbuffer

# Compute ML files dependencies
$(d)/%.depends $(d)/CFF/%.depends $(d)/Opentype/%.depends: INCLUDES:=$(LIBFONTS_INCLUDES)
SRC_$(d):=$(wildcard $(d)/*.ml) $(wildcard $(d)/*.mli) \
          $(wildcard $(d)/CFF/*.ml) $(wildcard $(d)/CFF/*.mli) \
          $(wildcard $(d)/Opentype/*.ml) $(wildcard $(d)/Opentype/*.mli)
-include $(addsuffix .depends,$(SRC_$(d)))

LIBFONTS_MODS:= UnicodeRanges IsoAdobe \
                FBezier FTypes CFF/CFFStd CFF/Type2 CFF/CFF \
								Opentype/Subst Opentype/Cmap Opentype/Opentype_layout \
                Opentype/Opentype Fonts
 
LIBFONTS_ML:=$(addsuffix .ml,$(addprefix $(d)/,$(LIBFONTS_MODS)))
LIBFONTS_CMO:=$(LIBFONTS_ML:.ml=.cmo)
LIBFONTS_CMX:=$(LIBFONTS_ML:.ml=.cmx)

LIBFONTS_MLI:=$(wildcard $(d)/*.mli) $(wildcard $(d)/CFF/*.mli) $(wildcard $(d)/Opentype/*.mli)
LIBFONTS_CMI:=$(LIBFONTS_MLI:.mli=.cmi)

# We cannot run ocamlc and ocamlopt simultaneously on the same input,
# since they both overwrite the .cmi file, which can get corrupted.
# That's why we arbitrarily force the following dependency.
$(LIBFONTS_CMX): %.cmx: %.cmo

$(LIBFONTS_CMI): %.cmi: %.mli
	$(ECHO) "[OCAMLC] $< -> $@"
	$(Q)$(OCAMLC) $(OFLAGS) $(PACK) $(LIBFONTS_INCLUDES) -o $@ -c $<
$(LIBFONTS_CMO): %.cmo: %.ml
	$(ECHO) "[OCAMLC] $< -> $@"
	$(Q)$(OCAMLC) $(OFLAGS) $(PACK) $(LIBFONTS_INCLUDES) -o $@ -c $<
$(LIBFONTS_CMX): %.cmx: %.ml
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(PACK) $(LIBFONTS_INCLUDES) -o $@ -c $<

$(d)/fonts.cma: $(LIBFONTS_CMO)
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLC) -a -o $@ $^

$(d)/fonts.cmxa: $(LIBFONTS_CMX)
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLOPT) -a -o $@ $^

$(d)/unicodeRanges/unicode_ranges_cpp.ml: $(d)/unicodeRanges/unicode_ranges.ml
	$(ECHO) "[CPP]    $< -> $@"
	$(Q)$(OCPP) $< $@

$(d)/UnicodeRanges.ml: $(d)/unicodeRanges/unicode_ranges_cpp.ml $(d)/unicodeRanges/unicode
	$(ECHO) "[UNIC]   ... $@"
	$(Q)$(OCAML) str.cma $^ $@

$(d)/IsoAdobe.ml: $(d)/isoAdobe/ps_standards.ml $(d)/isoAdobe/isoadobe.source
	$(ECHO) "[ISO]    ... $@"
	$(Q)$(OCAML) str.cma $^ $@

# Building everything
all: $(d)/fonts.cmxa $(d)/fonts.cma

# Cleaning
CLEAN += $(d)/fonts.cma $(d)/fonts.cmxa $(d)/fonts.a \
  $(d)/*.cmo $(d)/*.cmx $(d)/*.cmi $(d)/*.o \
  $(d)/CFF/*.cmo $(d)/CFF/*.cmx $(d)/CFF/*.cmi \
	$(d)/Opentype/*.cmo $(d)/Opentype/*.cmx $(d)/Opentype/*.cmi

DISTCLEAN += $(wildcard $(d)/*.depends) \
	     $(wildcard $(d)/CFF/*.depends) \
	     $(wildcard $(d)/Opentype/*.depends) \
			 $(d)/IsoAdobe.ml $(d)/UnicodeRanges.ml $(d)/unicodeRanges/unicode_ranges_cpp.ml

# Installing
install: install-libfonts
.PHONY: install-libfonts

install-libfonts: $(d)/fonts.cmxa $(d)/fonts.cma $(d)/fonts.a $(LIBFONTS_CMI) $(LIBFONTS_CMO) $(LIBFONTS_CMX) $(d)/META
	install -m 755 -d $(DESTDIR)/$(INSTALL_LIBFONTS_DIR)
	install -p -m 644 $^ $(DESTDIR)/$(INSTALL_LIBFONTS_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
