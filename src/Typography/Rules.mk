# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

TYPOGRAPHY_INCLUDES := -I $(d) -I $(d)/Output $(PACK_TYPOGRAPHY)
TYPOGRAPHY_DEPS_INCLUDES := -I $(d) -I $(d)/Output $(DEPS_PACK_TYPOGRAPHY)

# Compute ML files dependencies
$(d)/%.depends $(d)/Output/%.depends: INCLUDES:=$(TYPOGRAPHY_DEPS_INCLUDES)
$(d)/%.cmx $(d)/%.cmo $(d)/%.cmi: INCLUDES:=$(TYPOGRAPHY_INCLUDES)
$(d)/Output/%.cmx $(d)/Output/%.cmo $(d)/Output/%.cmi: INCLUDES:=$(TYPOGRAPHY_INCLUDES)

SRC_$(d):=$(wildcard $(d)/*.ml) \
  $(wildcard $(d)/*.mli) \
  $(wildcard $(d)/*/*.ml) \
  $(wildcard $(d)/*/*.mli)
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(addsuffix .depends,$(SRC_$(d)))
endif
endif

TYPOGRAPHY_MODS:= TypoLanguage FindPath FontPattern $(FINDFONT) Config ConfigUtil Bezier Distance \
  Offset Output/OutputCommon Box Badness Break Document Complete Hyphenate Maths \
	Output/OutputPaper Output/OutputDrawing Geometry Proj3d Diagrams ProofTree Db

TYPOGRAPHY_ML:=$(addsuffix .ml,$(addprefix $(d)/,$(TYPOGRAPHY_MODS)))
TYPOGRAPHY_CMO:=$(TYPOGRAPHY_ML:.ml=.cmo)
TYPOGRAPHY_CMX:=$(TYPOGRAPHY_ML:.ml=.cmx)

TYPOGRAPHY_MLI:=$(wildcard $(d)/*.mli) $(wildcard $(d)/*/*.mli)
TYPOGRAPHY_CMI:=$(TYPOGRAPHY_MLI:.mli=.cmi)

# We cannot run ocamlc and ocamlopt simultaneously on the same input,
# since they both overwrite the .cmi file, which can get corrupted.
# That's why we arbitrarily force the following dependency.
$(TYPOGRAPHY_CMX): %.cmx: %.cmo

$(TYPOGRAPHY_CMI:.cmi=.cmo): %.cmo: %.cmi
$(TYPOGRAPHY_CMI:.cmi=.cmx): %.cmx: %.cmi

$(d)/Typography.cmo: $(TYPOGRAPHY_CMO)
	$(ECHO) "[PACK]   ... -> $@"
	$(Q)$(OCAMLC) -pack -o $@ $^
$(d)/Typography.cmx: $(TYPOGRAPHY_CMX)
	$(ECHO) "[PACK]   ... -> $@"
	$(Q)$(OCAMLOPT) -pack -o $@ $^
$(d)/Typography.cma: $(d)/Typography.cmo
	$(ECHO) "[OCAMLC] $< -> $@"
	$(Q)$(OCAMLC) -a -o $@ $<
$(d)/Typography.cmxa: $(d)/Typography.cmx
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) -a -o $@ $<
$(d)/Typography.cmxs: $(d)/Typography.cmx
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) -shared -o $@ $<

$(TYPOGRAPHY_CMI): %.cmi: %.mli
	$(ECHO) "[OCAMLC] $< -> $@"
	$(Q)$(OCAMLC) $(OFLAGS) -for-pack Typography $(TYPOGRAPHY_INCLUDES) -o $@ -c $<
$(filter-out $(d)/Break.cmo,$(TYPOGRAPHY_CMO)): %.cmo: %.ml
	$(ECHO) "[OCAMLC] $< -> $@"
	$(Q)$(OCAMLC) $(OFLAGS) -for-pack Typography $(TYPOGRAPHY_INCLUDES) -o $@ -c $<
$(filter-out $(d)/Break.cmx,$(TYPOGRAPHY_CMX)): %.cmx: %.ml
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(OFLAGS) -for-pack Typography $(TYPOGRAPHY_INCLUDES) -o $@ -c $<

$(d)/Break.cmo: $(d)/Break.ml
	$(ECHO) "[OCAMLC] $< -> $@"
	$(Q)$(OCAMLC) $(OFLAGS) -for-pack Typography -rectypes $(TYPOGRAPHY_INCLUDES) -o $@ -c $<
$(d)/Break.cmx: $(d)/Break.ml
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(OFLAGS) -for-pack Typography -rectypes $(TYPOGRAPHY_INCLUDES) -o $@ -c $<

# Build DefaultFormat
DEFAULT_FORMAT_ML := $(d)/DefaultFormat/Euler.ml $(d)/DefaultFormat/Numerals.ml \
  $(d)/DefaultFormat/TableOfContents.ml $(d)/DefaultFormat/DefaultFormat.ml
DEFAULTFORMAT_ML:=$(wildcard $(d)/DefaultFormat/*.ml) 
DEFAULTFORMAT_CMI:= $(DEFAULTFORMAT_ML:.ml=.cmi)

$(d)/DefaultFormat.cmxa: $(DEFAULT_FORMAT_ML:.ml=.cmx)
	$(ECHO) "[OPT]    $<"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(TYPOGRAPHY_INCLUDES) -o $@ -a $^

$(d)/DefaultFormat.cmxs: $(DEFAULT_FORMAT_ML:.ml=.cmx)
	$(ECHO) "[OPT]    $<"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(TYPOGRAPHY_INCLUDES) -o $@ -shared $^

$(d)/DefaultFormat.cma: $(DEFAULT_FORMAT_ML:.ml=.cmo)
	$(ECHO) "[OCAMLC] $<"
	$(Q)$(OCAMLC) $(OFLAGS) $(TYPOGRAPHY_INCLUDES) -o $@ -a $^

$(d)/DefaultFormat.p.cma: $(DEFAULT_FORMAT_ML:.ml=.p.cmo)
	$(ECHO) "[OPT -p] $<"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(TYPOGRAPHY_INCLUDES) -o $@ -p -a $^

# ocamldep cannot find dependencies on Typography
$(d)/ParseMainArgs.cmo: $(d)/Typography.cma
$(d)/ParseMainArgs.cmx: $(d)/Typography.cmxa

# Building everything
all: $(d)/Typography.cmxa $(d)/Typography.cma $(d)/DefaultFormat.cma $(d)/DefaultFormat.cmxa $(d)/DefaultFormat.cmxs

# Cleaning
CLEAN += $(d)/*.cma $(d)/*.cmxa $(d)/*.a $(d)/*.cmxs \
  $(d)/*.cmo $(d)/*.cmx $(d)/*.cmi $(d)/*.o \
  $(d)/Output/*.cmo $(d)/Output/*.cmx $(d)/Output/*.cmi $(d)/Output/*.o \
  $(d)/DefaultFormat/*.cmo $(d)/DefaultFormat/*.cmx $(d)/DefaultFormat/*.cmi $(d)/DefaultFormat/*.o \
  $(d)/_tags

DISTCLEAN += $(wildcard $(d)/*.depends) \
	     $(wildcard $(d)/Output/*.depends) \
	     $(wildcard $(d)/DefaultFormat/*.depends) \

# Installing
install: install-typography
.PHONY: install-typography

install-typography: $(d)/Typography.cmxa $(d)/Typography.cma $(d)/Typography.cmxs $(d)/Typography.a \
  $(d)/Typography.cmi $(d)/ParseMainArgs.cmx $(d)/ParseMainArgs.o $(d)/ParseMainArgs.cmi \
  $(d)/DefaultFormat.cma $(d)/DefaultFormat.cmxa $(d)/DefaultFormat.cmxs $(d)/DefaultFormat.a $(DEFAULTFORMAT_CMI)
	install -m 755 -d $(DESTDIR)/$(INSTALL_TYPOGRAPHY_DIR)
	install -p -m 644 $^ $(DESTDIR)/$(INSTALL_TYPOGRAPHY_DIR)
	install -p -m 644 $(TYPOGRAPHY_DIR)/META $(DESTDIR)/$(INSTALL_TYPOGRAPHY_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
