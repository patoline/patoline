# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

TYPOGRAPHY_INCLUDES := -I $(d) $(PACK_TYPOGRAPHY) -I $(CONFIG_DIR)
TYPOGRAPHY_DEPS_INCLUDES := -I $(d) -I $(d)/DefaultFormat $(DEPS_PACK_TYPOGRAPHY) -I $(CONFIG_DIR)

# Compute ML files dependencies
$(d)/%.depends $(wildcard $(d)/*/%.depends): INCLUDES:=$(TYPOGRAPHY_DEPS_INCLUDES)
$(d)/%.cmx $(d)/%.cmo $(d)/%.cmi $(wildcard $(d)/*/%.cmx) $(wildcard $(d)/*/%.cmo) $(wildcard $(d)/*/%.cmi): INCLUDES:=$(TYPOGRAPHY_INCLUDES)

SRC_$(d):=$(wildcard $(d)/*.ml) $(wildcard $(d)/*.mli) $(wildcard $(d)/*/*.ml) $(wildcard $(d)/*/*.mli) $(d)/ConfigFindFont.ml
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(addsuffix .depends,$(SRC_$(d)))
endif
endif

TYPOGRAPHY_MODS:= TypoLanguage FontPattern ConfigFindFont Distance Offset \
	Box Badness Break Document Complete Maths OutputDrawing Geometry Proj3d \
	Diagrams ProofTree Verbatim Typography

TYPOGRAPHY_ML:=$(addsuffix .ml,$(addprefix $(d)/,$(TYPOGRAPHY_MODS)))
TYPOGRAPHY_CMO:=$(TYPOGRAPHY_ML:.ml=.cmo)
TYPOGRAPHY_CMX:=$(TYPOGRAPHY_ML:.ml=.cmx)

TYPOGRAPHY_MLI:=$(wildcard $(d)/*.mli) $(wildcard $(d)/*/*.mli)
TYPOGRAPHY_CMI:=$(TYPOGRAPHY_ML:.ml=.cmi)

# We cannot run ocamlc and ocamlopt simultaneously on the same input,
# since they both overwrite the .cmi file, which can get corrupted.
# That's why we arbitrarily force the following dependency.
$(TYPOGRAPHY_CMX): %.cmx: %.cmo

$(TYPOGRAPHY_CMI:.cmi=.cmo): %.cmo: %.cmi
$(TYPOGRAPHY_CMI:.cmi=.cmx): %.cmx: %.cmi

$(d)/Typography.cmxa: $(d)/patoconfig.cmxa $(d)/patoconfig.a
$(d)/DefaultFormat.cmxa: $(d)/patoconfig.cmxa $(d)/patoconfig.a
$(d)/Typography.cma: $(d)/patoconfig.cma $(d)/patoconfig.a
$(d)/DefaultFormat.cma: $(d)/patoconfig.cma $(d)/patoconfig.a

$(d)/patoconfig.cmxa: $(CONFIG_DIR)/patoconfig.cmxa
	$(ECHO) "[CPY] $@"
	$(Q)cp $< $@

$(d)/patoconfig.cma: $(CONFIG_DIR)/patoconfig.cma
	$(ECHO) "[CPY] $@"
	$(Q)cp $< $@

$(d)/patoconfig.a: $(CONFIG_DIR)/patoconfig.cmxa
	$(ECHO) "[CPY] $@"
	$(Q)cp $(CONFIG_DIR)/patoconfig.a $@

$(d)/ConfigFindFont.ml: $(d)/ConfigFindFont/$(FINDFONT).ml
	$(ECHO) "[CPY] $@"
	$(Q)cp $< $@

$(d)/Typography.cma: $(TYPOGRAPHY_CMO)
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLC) -a -o $@ $(TYPOGRAPHY_CMO)

$(d)/Typography.cmxa: $(TYPOGRAPHY_CMX)
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) -a -o $@ $(TYPOGRAPHY_CMX)

$(d)/Typography.cmxs: $(TYPOGRAPHY_CMX)
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) -shared -o $@ $(TYPOGRAPHY_CMX)

$(d)/Break.cmo: $(d)/Break.ml
	$(ECHO) "[BYT] $@"
	$(Q)$(OCAMLC) $(OFLAGS) -rectypes $(TYPOGRAPHY_INCLUDES) -o $@ -c $<

$(d)/Break.cmx: $(d)/Break.ml
	$(ECHO) "[OPT] $@"
	$(Q)$(OCAMLOPT) $(OFLAGS) -rectypes $(TYPOGRAPHY_INCLUDES) -o $@ -c $<

# Build DefaultFormat; The variable must be ordered
DEFAULTFORMAT_ML := $(d)/DefaultFormat/Euler.ml $(d)/DefaultFormat/Numerals.ml \
  $(d)/DefaultFormat/TableOfContents.ml $(d)/DefaultFormat/PageLayout.ml \
  $(d)/DefaultFormat/DefaultFormat.ml
DEFAULTFORMAT_CMI:= $(DEFAULTFORMAT_ML:.ml=.cmi)
DEFAULTFORMAT_CMX:= $(DEFAULTFORMAT_ML:.ml=.cmx)
DEFAULTFORMAT_CMO:= $(DEFAULTFORMAT_ML:.ml=.cmo)

$(d)/DefaultFormat.cmxa: $(DEFAULTFORMAT_CMX)
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(TYPOGRAPHY_INCLUDES) -o $@ -a $(DEFAULTFORMAT_CMX)

$(d)/DefaultFormat.cmxs: $(DEFAULTFORMAT_CMX)
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(TYPOGRAPHY_INCLUDES) -o $@ -shared $(DEFAULTFORMAT_CMX)

$(d)/DefaultFormat.cma: $(DEFAULTFORMAT_CMO)
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLC) $(OFLAGS) $(TYPOGRAPHY_INCLUDES) -o $@ -a $(DEFAULTFORMAT_CMO)

# Building everything
all: $(d)/Typography.cmxa $(d)/Typography.cma $(d)/DefaultFormat.cma $(d)/DefaultFormat.cmxa $(d)/DefaultFormat.cmxs

# Cleaning
CLEAN += $(d)/*.cma $(d)/*.cmxa $(d)/*.a $(d)/*.cmxs $(d)/*.cmo $(d)/*.cmx \
	$(d)/*.cmi $(d)/*.o $(d)/DefaultFormat/*.cmo $(d)/DefaultFormat/*.cmx \
 	$(d)/DefaultFormat/*.cmi $(d)/DefaultFormat/*.o $(d)/_tags

DISTCLEAN += $(d)/*.depends $(d)/DefaultFormat/*.depends \
	$(d)/ConfigFindFont.ml $(d)/ConfigFindFont/*.depends

# Installing
install: install-typography
.PHONY: install-typography

install-typography: $(TYPOGRAPHY_CMI) $(DEFAULTFORMAT_CMI) \
	$(d)/Typography.cmxa $(d)/Typography.cma $(d)/Typography.cmxs \
	$(d)/Typography.a $(d)/DefaultFormat.cma $(d)/DefaultFormat.cmxa \
	$(d)/DefaultFormat.cmxs $(d)/DefaultFormat.a
	install -m 755 -d $(DESTDIR)/$(INSTALL_TYPOGRAPHY_DIR)
	install -p -m 644 $^ $(DESTDIR)/$(INSTALL_TYPOGRAPHY_DIR)
	install -p -m 644 $(TYPOGRAPHY_DIR)/META $(DESTDIR)/$(INSTALL_TYPOGRAPHY_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
