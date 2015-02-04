OCAMLVERSION=$(shell ocamlc -version)
DECAP=..
PA_OCAML=$(DECAP)/pa_ocaml

ifeq ($(OCAMLVERSION),3.12.1)
COMPILER_INC = -I +compiler-libs/parsing -I +compiler-libs/typing -I +compiler-libs/utils
COMPILER_LIBS = misc.cmo config.cmo longident.cmo printast.cmo linenum.cmo warnings.cmo location.cmo
COMPILER_PARSERS = syntaxerr.cmo lexer.cmo clflags.cmo parser.cmo parse.cmo
COMPILER_TOP = toplevellib.cma
else
COMPILER_INC = -I +compiler-libs
COMPILER_LIBS = ocamlcommon.cma
COMPILER_PARSERS =
COMPILER_TOP = ocamlbytecomp.cma ocamltoplevel.cma
endif

COMPILER_LIBO := $(COMPILER_LIBS:.cma=.cmxa)
COMPILER_LIBO := $(COMPILER_LIBO:.cmo=.cmx)
COMPILER_PARSERO := $(COMPILER_PARSERS:.cma=.cmxa)
COMPILER_PARSERO := $(COMPILER_PARSERO:.cmo=.cmx)

all: pa_ast compare.cmx

pa_ast: pa_ast.ml $(DECAP)/decap.cmxa $(DECAP)/decap_ocaml.cmxa
	ocamlopt -pp $(PA_OCAML) -I $(DECAP) $(COMPILER_INC) $(COMPILER_LIBO) -o $@ unix.cmxa str.cmxa decap.cmxa decap_ocaml.cmxa $<

asttypes.mli:
	cp ast-$(OCAMLVERSION)/asttypes.mli .

parsetree.mli:
	cp ast-$(OCAMLVERSION)/parsetree.mli .

# FIXME target construction does not fail if pa_ast fails...
compare.ml: pa_ast generic.ml parsetree.mli asttypes.mli
	cat generic.ml >> $@
	@echo "(* asttypes.mli *)" >> $@
	./pa_ast asttypes.mli >> $@
	@echo "" >> $@
	@echo "(* parsetree.mli *)" >> $@
	./pa_ast parsetree.mli >> $@

compare.cmx: compare.ml
	ocamlopt -c $(COMPILER_INC) $(COMPILER_LIBO) $<

compare.cmo: compare.ml
	ocamlopt -c $(COMPILER_INC) $(COMPILER_LIBO) $<

clean:
	rm -f *.cmi *.cmx *.cmo *.cma *.cmxa *.o

distclean: clean
	rm -f pa_ast compare.ml asttypes.mli parsetree.mli
