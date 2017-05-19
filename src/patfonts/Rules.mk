# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

LIBFONTS_INCLUDES := -I $(d) $(PACK_FONTS) -I $(UNICODE_DIR) -I $(UNICODE_DIR)/encoding_data
LIBFONTS_DEPS_INCLUDES := -I $(d) $(DEPS_PACK_FONTS) -I $(UNICODE_DIR) -I $(UTIL_DIR) -I $(UNICODE_DIR)/encoding_data

# Compute ML files dependencies
$(d)/%.depends $(d)/CFF/%.depends $(d)/Opentype/%.depends $(d)/unicodeRanges/%.depends: INCLUDES:=$(LIBFONTS_DEPS_INCLUDES)
$(d)/%.cmx $(d)/%.cmo $(d)/%.cmi: INCLUDES:=$(LIBFONTS_INCLUDES)
$(d)/CFF/%.cmx $(d)/CFF/%.cmo $(d)/CFF/%.cmi: INCLUDES:=$(LIBFONTS_INCLUDES)
$(d)/Opentype/%.cmx $(d)/Opentype/%.cmo $(d)/Opentype/%.cmi: INCLUDES:=$(LIBFONTS_INCLUDES)
$(d)/unicodeRanges/%.cmx $(d)/unicodeRanges/%.cmo $(d)/unicodeRanges/%.cmi: INCLUDES:=$(LIBFONTS_INCLUDES)

$(d)/Opentype/Opentype.ml.depends: $(UNICODE_DIR)/ROMAN.ml $(UNICODE_DIR)/LATIN1.ml $(UNICODE_DIR)/UTF8.ml

SRC_$(d):=$(wildcard $(d)/*.ml) $(wildcard $(d)/*.mli) \
          $(wildcard $(d)/CFF/*.ml) $(wildcard $(d)/CFF/*.mli) \
          $(wildcard $(d)/Opentype/*.ml) $(wildcard $(d)/Opentype/*.mli) \
          $(d)/UnicodeRanges.ml $(d)/IsoAdobe.ml
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(addsuffix .depends,$(SRC_$(d)))
endif
endif

LINKS:=$(wildcard $(d)/CFF/*.ml) $(wildcard $(d)/CFF/*.mli) \
       $(wildcard $(d)/Opentype/*.ml) $(wildcard $(d)/Opentype/*.mli)

LIBFONTS_MODS:= UnicodeRanges IsoAdobe \
                FBezier FTypes CFF/CFFStd CFF/Type2 CFF/CFF \
		Opentype/Subst Opentype/Cmap Opentype/Opentype_layout \
                Opentype/Opentype Fonts

LIBFONTS_ML:=$(addsuffix .ml,$(addprefix $(d)/,$(LIBFONTS_MODS)))
LIBFONTS_CMO:=$(LIBFONTS_ML:.ml=.cmo)
LIBFONTS_CMX:=$(LIBFONTS_ML:.ml=.cmx)

LIBFONTS_MLI:=$(wildcard $(d)/*.mli) $(wildcard $(d)/CFF/*.mli) $(wildcard $(d)/Opentype/*.mli)
LIBFONTS_CMI:=$(LIBFONTS_MLI:.mli=.cmi) $(d)/FBezier.cmi $(d)/FTypes.cmi $(d)/IsoAdobe.cmi $(d)/UnicodeRanges.cmi $(d)/Opentype/Opentype.cmi $(d)/CFF/CFF.cmi

# We cannot run ocamlc and ocamlopt simultaneously on the same input,
# since they both overwrite the .cmi file, which can get corrupted.
# That's why we arbitrarily force the following dependency.
$(LIBFONTS_CMX): %.cmx: %.cmo

$(LIBFONTS_MLI:.mli=.cmo): %.cmo: %.cmi
$(LIBFONTS_MLI:.mli=.cmx): %.cmx: %.cmo

$(filter-out $(LIBFONTS_MLI:.mli=.cmi), $(LIBFONTS_CMI)): %.cmi: %.cmo;

$(d)/fonts.cma: $(LIBFONTS_CMO)
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLC) -a -o $@ $^

$(d)/fonts.cmxa: $(LIBFONTS_CMX)
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) -a -o $@ $^

$(d)/fonts.cmxs: $(LIBFONTS_CMX)
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) -shared -o $@ $^

$(d)/unicodeRanges/unicode_ranges_cpp.ml: $(d)/unicodeRanges/unicode_ranges.ml
	$(ECHO) "[CPP] $@"
	$(Q)$(OCPP) $< $@

$(d)/UnicodeRanges.ml: $(d)/unicodeRanges/unicode_ranges_cpp.ml $(d)/unicodeRanges/unicode
	$(ECHO) "[GEN] $@"
	$(Q)$(OCAML) str.cma $^ $@

$(d)/IsoAdobe.ml: $(d)/isoAdobe/ps_standards.ml $(d)/isoAdobe/isoadobe.source
	$(ECHO) "[GEN] $@"
	$(Q)$(OCAML) str.cma $^ $@

# Building everything
all: $(d)/fonts.cmxa $(d)/fonts.cma $(LIBFONTS_CMI)

# Cleaning
CLEAN += $(d)/fonts.cma $(d)/fonts.cmxa $(d)/fonts.a $(d)/fonts.cmxs \
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

install-libfonts: $(d)/fonts.cmxa $(d)/fonts.cmxs $(d)/fonts.cma \
	  $(d)/fonts.a $(LIBFONTS_CMI) $(LIBFONTS_CMO) $(LIBFONTS_CMX) $(d)/META
	$(ECHO) "[INS] patfonts"
	- $(OCAMLFIND) remove patfonts
	$(Q)$(OCAMLFIND) install patfonts $^

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
