# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

GLR_INCLUDES := -pp "camlp4o pa_macro.cmo" -I $(d)
PA_GLR_INCLUDES := -pp "camlp4orf" -I $(d) -I +camlp4
$(d)/pa_glr.ml.depends: INCLUDES:=$(PA_GLR_INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx : INCLUDES:=$(GLR_INCLUDES)
$(d)/pa_glr.cmo $(d)/pa_glr.cmi $(d)/pa_glr.cmx : INCLUDES:=$(GLR_INCLUDES)

GLR_ML:=$(d)/charset.ml $(d)/umap.ml $(d)/glr.ml
GLR_MLI:=$(d)/charset.mli $(d)/umap.mli $(d)/glr.mli
GLR_CMO:=$(GLR_ML:.ml=.cmo)
GLR_CMX:=$(GLR_ML:.ml=.cmx)
GLR_CMI:=$(GLR_ML:.ml=.cmi)

# manual deps because pa_patoline.ml.depends needs pa_glr
# and the .ml.depends are all done first
$(d)/glr.cmi: $(d)/charset.cmi

$(d)/umap.cmo: $(d)/umap.cmi
$(d)/umap.cmx: $(d)/umap.cmi
$(d)/charset.cmo: $(d)/charset.cmi
$(d)/charset.cmx: $(d)/charset.cmi

$(d)/glr.cmo: $(d)/glr.cmi $(d)/umap.cmi $(d)/charset.cmi

$(d)/glr.cmx: $(d)/glr.cmi $(d)/umap.cmx $(d)/umap.cmi $(d)/charset.cmx $(d)/charset.cmi

$(d)/glr.cmxa: $(GLR_CMX)
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLOPT) -a -o $@ $^

$(d)/glr.cma: $(GLR_CMO)
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLC) -a -o $@ $^

$(d)/pa_glr.cmx: $(d)/pa_glr.ml $(d)/glr.cmx 
	$(ECHO) "[OPT] $< -> $@"
	$(Q)$(OCAMLOPT) $(PA_GLR_INCLUDES) -c $<

$(d)/pa_glr: $(d)/pa_glr.cmx
	$(ECHO) "[LINK] ... -> $@"
	$(Q)$(OCAMLOPT) -linkall $(GLR_INCLUDES) -I +camlp4 dynlink.cmxa camlp4lib.cmxa \
        unix.cmxa                                               \
        Camlp4Parsers/Camlp4OCamlRevisedParser.cmx             \
        camlp4/Camlp4Parsers/Camlp4QuotationCommon.cmx         \
        camlp4/Camlp4Parsers/Camlp4QuotationExpander.cmx       \
        camlp4/Camlp4Parsers/Camlp4OCamlParser.cmx             \
        camlp4/Camlp4Parsers/Camlp4OCamlRevisedParserParser.cmx \
        camlp4/Camlp4Parsers/Camlp4OCamlParserParser.cmx       \
        camlp4/Camlp4Parsers/Camlp4GrammarParser.cmx           \
        $<                                         \
        Camlp4Printers/Camlp4AutoPrinter.cmx                   \
        Camlp4Bin.cmx -o $@

PA_GLR = $(GLR_DIR)/pa_glr

$(d)/ocaml-parser/pa_ocaml: $(d)/ocaml-parser/pa_ocaml.ml $(PA_GLR)
	$(ECHO) "[LINK] $< -> $@"
	$(Q)$(OCAMLOPT_NOINTF) -pp $(PA_GLR) -I +compiler-libs -package glr -o $@ str.cmxa $(GLR_DIR)/glr.cmxa ocamlcommon.cmxa $<

all: $(d)/pa_glr $(d)/ocaml-parser/pa_ocaml

CLEAN += $(d)/pa_glr $(d)/*.cmx $(d)/*.o $(d)/*.cmi $(d)/*.cmo $(d)/*.cma $(d)/*.cmxa $(d)/*.a
CLEAN += $(d)/ocaml-parser/pa_ocaml $(d)/ocaml-parser/*.cmx $(d)/ocaml-parser/*.o $(d)/ocaml-parser/*.cmi $(d)/ocaml-parser/*.cmo $(d)/ocaml-parser/*.cma $(d)/ocaml-parser/*.cmxa $(d)/ocaml-parser/*.a

DISTCLEAN += $(wildcard $(d)/*.depends)

$(d)/glr.a: $(d)/glr.cmxa;

install: install-glr install-pa-glr
install-glr: $(d)/glr.cmxa $(d)/glr.cma $(d)/glr.a $(GLR_CMI) $(d)/META
	install -m 755 -d $(DESTDIR)/$(INSTALL_GLR_DIR)
	install -m 644 -p $^ $(DESTDIR)/$(INSTALL_GLR_DIR)

install-pa-glr: install-bindir $(PA_GLR)
	install -m 755 $(PA_GLR) $(DESTDIR)/$(INSTALL_BIN_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
