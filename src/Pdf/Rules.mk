# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

# Compute ML files dependencies
SRC_$(d):=$(wildcard $(d)/*.ml) $(d)/obj_parser.ml $(d)/obj_lexer.ml $(d)/obj_parser.mli

$(d)/obj_lexer.ml.depends: $(d)/obj_parser.ml
# Without the following empty rule, make does not know that ocamlyacc
# writes both .ml and .mli files.
$(d)/obj_parser.mli: $(d)/obj_parser.ml ;

-include $(addsuffix .depends,$(SRC_$(d)))

# Building stuff
all: $(d)/pdf_parser.cmxa $(d)/pdf_parser.a $(d)/pdf_parser.cmi
$(d)/%.depends $(d)/%.cmx $(d)/%.cmo: INCLUDES:=-I $(d) -package $(CAMLZIP),Typography

$(d)/pdf_parser.cmi: $(d)/pdf_parser.cmx ;

$(d)/obj_parser.cmx $(d)/pdf_parser.cmx $(d)/pdfutil.cmx: $(TYPOGRAPHY_DIR)/Typography.cmxa
$(d)/obj_parser.cmo $(d)/pdf_parser.cmo $(d)/pdfutil.cmo: $(TYPOGRAPHY_DIR)/Typography.cma

PDF_PARSER_SOURCES := $(d)/pdfutil.ml $(d)/obj_lexer.ml $(d)/obj_parser.ml $(d)/pdf_parser.ml

$(d)/pdf_parser.a: $(d)/pdf_parser.cmxa ;
$(d)/pdf_parser.cmxa: $(PDF_PARSER_SOURCES:.ml=.cmx) $(TYPOGRAPHY_DIR)/Typography.cmxa
	$(ECHO) "[BYT]    ... -> $@"
	$(Q)$(OCAMLOPT) -a -o $@ $(filter-out $(TYPOGRAPHY_DIR)/Typography.cmxa,$^)

$(d)/pdf_parser.cma: $(PDF_PARSER_SOURCES:.ml=.cmo) $(TYPOGRAPHY_DIR)/Typography.cma
	$(ECHO) "[OPT]    ... -> $@"
	$(Q)$(OCAMLC) -a -o $@ $(filter-out $(TYPOGRAPHY_DIR)/Typography.cma,$^)

$(d)/pdf_parser.p.cmxa: $(PDF_PARSER_SOURCES:.ml=.p.cmx) $(TYPOGRAPHY_DIR)/Typography.p.cmxa
	$(ECHO) "[OPT.p]  ... -> $@"
	$(Q)$(OCAMLOPT) -p -a -o $@ $(filter-out $(TYPOGRAPHY_DIR)/Typography.p.cmxa,$^)

# Installing
install: install-pdf
.PHONY: install-pdf
install-pdf: install-typography $(d)/pdf_parser.cmxa $(d)/pdf_parser.a $(d)/pdf_parser.cmi
	install -m 644 $(wordlist 2,$(words $^),$^) $(DESTDIR)/$(INSTALL_TYPOGRAPHY_DIR)

# Cleaning
CLEAN += $(d)/*.cmo $(d)/*.cmx $(d)/*.cmi \
	 $(d)/*.cmxa $(d)/*.cma \
	 $(d)/*.p.cmx $(d)/*.p.cmxa \
	 $(d)/*.o $(d)/*.a \
	 $(d)/obj_parser.ml $(d)/obj_parser.mli $(d)/obj_lexer.ml
DISTCLEAN += $(d)/*.depends

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
