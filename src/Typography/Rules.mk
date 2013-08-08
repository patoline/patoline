# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

TYPOGRAPHY_INCLUDES := -I $(d) -I $(RBUFFER_DIR) -I $(d)/Fonts -I $(d)/Fonts/Sfnt -I $(d)/Output

# Compute ML files dependencies
$(d)/%.depends: INCLUDES:=$(TYPOGRAPHY_INCLUDES)
SRC_$(d):=$(wildcard $(d)/*.ml) \
  $(wildcard $(d)/*.mli) \
  $(wildcard $(d)/*/*.ml) \
  $(wildcard $(d)/*/*.mli)
-include $(addsuffix .depends,$(SRC_$(d)))

TYPOGRAPHY_MODS:= TypoLanguage FindPath $(FINDFONT) Config ConfigUtil Util Bezier Distance \
  Offset Fonts/Sfnt/Unicode_ranges Fonts/FTypes Fonts/Type2 Fonts/CFFStd Fonts/CFF \
  Fonts/Opentype_layout Fonts/Cmap Fonts/Opentype Fonts Output/OutputCommon \
  Box Badness Break Document Complete Hyphenate Maths Output/OutputPaper \
  Output/OutputDrawing Geometry Proj3d Diagrams ProofTree

TYPOGRAPHY_ML:=$(addsuffix .ml,$(addprefix $(d)/,$(TYPOGRAPHY_MODS)))
TYPOGRAPHY_CMO:=$(TYPOGRAPHY_ML:.ml=.cmo)
TYPOGRAPHY_CMX:=$(TYPOGRAPHY_ML:.ml=.cmx)

TYPOGRAPHY_MLI:=$(wildcard $(d)/*.mli) $(wildcard $(d)/*/*.mli)
TYPOGRAPHY_CMI:=$(TYPOGRAPHY_MLI:.mli=.cmi)

# We cannot run ocamlc and ocamlopt simultaneously on the same input,
# since they both overwrite the .cmi file, which can get corrupted.
# That's why we arbitrarily force the following dependency.
$(TYPOGRAPHY_CMX): %.cmx: %.cmo

$(d)/Typography.cmo: $(TYPOGRAPHY_CMO)
	$(ECHO) "[PACK]   -> $@"
	$(Q)$(OCAMLC) -pack -o $@ $^
$(d)/Typography.cmx: $(TYPOGRAPHY_CMX)
	$(ECHO) "[PACK]   -> $@"
	$(Q)$(OCAMLOPT) -pack -o $@ $^
$(d)/Typography.cma: $(d)/Typography.cmo
	$(ECHO) "[OCAMLC] $< -> $@"
	$(Q)$(OCAMLC) -a -o $@ $<
$(d)/Typography.cmxa: $(d)/Typography.cmx
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) -a -o $@ $<

$(TYPOGRAPHY_CMI): %.cmi: %.mli
	$(ECHO) "[OCAMLC] $< -> $@"
	$(Q)$(OCAMLC) $(OFLAGS) -for-pack Typography $(PACK) $(INCLUDES) -I $(<D) $(TYPOGRAPHY_INCLUDES) -o $@ -c $<
$(filter-out $(d)/Break.cmo,$(TYPOGRAPHY_CMO)): %.cmo: %.ml
	$(ECHO) "[OCAMLC] $< -> $@"
	$(Q)$(OCAMLC) $(OFLAGS) -for-pack Typography $(PACK) $(INCLUDES) -I $(<D) $(TYPOGRAPHY_INCLUDES) -o $@ -c $<
$(filter-out $(d)/Break.cmx,$(TYPOGRAPHY_CMX)): %.cmx: %.ml
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(OFLAGS) -for-pack Typography $(PACK) $(INCLUDES) -I $(<D) $(TYPOGRAPHY_INCLUDES) -o $@ -c $<

$(d)/Break.cmo: $(d)/Break.ml
	$(ECHO) "[OCAMLC] $< -> $@"
	$(Q)$(OCAMLC) $(OFLAGS) -for-pack Typography -rectypes $(PACK) $(TYPOGRAPHY_INCLUDES) -o $@ -c $<
$(d)/Break.cmx: $(d)/Break.ml
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(OFLAGS) -for-pack Typography -rectypes $(PACK) $(TYPOGRAPHY_INCLUDES) -o $@ -c $<

$(d)/Fonts/Sfnt/Unicode_ranges.ml: PACK:=-package str
$(d)/Fonts/Sfnt/Unicode_ranges.ml: $(d)/Fonts/Sfnt/make_unicode_ranges
	$< $(TYPOGRAPHY_DIR)/Fonts/Sfnt/unicode
$(d)/Fonts/Opentype.cmo: $(d)/Fonts/Sfnt/Unicode_ranges.cmo
$(d)/Fonts/Opentype.cmx: $(d)/Fonts/Sfnt/Unicode_ranges.cmx

# ocamldep cannot find dependencies on Typography
$(d)/ParseMainArgs.cmo: $(d)/Typography.cma
$(d)/ParseMainArgs.cmx: $(d)/Typography.cmxa

# Building everything
all: $(d)/Typography.cmxa $(d)/Typography.cma

# Cleaning
CLEAN += $(d)/Typography.cma $(d)/Typography.cmxa $(d)/Typography.a \
  $(d)/*.cmo $(d)/*.cmx $(d)/*.cmi $(d)/*.o \
  $(d)/Fonts/*.cmo $(d)/Fonts/*.cmx $(d)/Fonts/*.cmi $(d)/Fonts/*.o \
  $(d)/Output/*.cmo $(d)/Output/*.cmx $(d)/Output/*.cmi $(d)/Output/*.o \
  $(d)/Fonts/Sfnt/make_unicode_ranges.cmo $(d)/Fonts/Sfnt/make_unicode_ranges.cmi \
  $(d)/Fonts/Sfnt/make_unicode_ranges $(d)/Fonts/Sfnt/Unicode_ranges* \
  $(d)/Fonts/Sfnt/unicode_ranges.ml

DISTCLEAN += $(wildcard $(d)/*.depends) \
	     $(wildcard $(d)/Output/*.depends) \
	     $(wildcard $(d)/Fonts/*.depends)

# Installing
install: install-typography
.PHONY: install-typography

install-typography: $(d)/Typography.cmxa $(d)/Typography.a \
  $(d)/Typography.cmi $(d)/ParseMainArgs.cmx $(d)/ParseMainArgs.o $(d)/ParseMainArgs.cmi
	install -m 755 -d $(DESTDIR)/$(INSTALL_TYPOGRAPHY_DIR)
	install -p -m 644 $^ $(DESTDIR)/$(INSTALL_TYPOGRAPHY_DIR)
	install -p -m 644 $(TYPOGRAPHY_DIR)/META $(DESTDIR)/$(INSTALL_TYPOGRAPHY_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
