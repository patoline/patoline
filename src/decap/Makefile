OCAMLFIND = ocamlfind
OCAMLC = $(OCAMLFIND) ocamlc
OCAMLOPT = $(OCAMLFIND) ocamlopt
BINDIR = /usr/local/bin
LIBDIR = $(shell ocamlc -where)/decap

# do not add decap.cm(x)a because decap in bootstrap is
# does not contain pa_ocaml_prelude and adding decap.cm(x)a
# here will make fail after make distclean
# a more complete all is given below when pa_ocaml binary
# is present

INSTALLED = ahash.cmi ahash.cmo ahash.mli ahash.cmx decap.cmi decap.cmo decap.mli decap.cmx charset.cmi charset.cmo charset.mli charset.cmx input.cmi input.cmo input.mli input.cmx decap.cma decap.cmxa decap.a pa_ocaml_prelude.cmi pa_ocaml_prelude.cmo pa_ocaml_prelude.cmx pa_ocaml.cmi pa_ocaml.cmo pa_ocaml.cmx pa_parser.cmi pa_parser.cmx pa_parser.cmo pa_main.cmi pa_main.cmx pa_main.cmo decap_ocaml.cmxa decap_ocaml.cma decap.a decap_ocaml.a

all: pa_ocaml
HAS_PA_OCAML=$(shell if [ -x pa_ocaml ]; then echo 1; else echo 0; fi)

OCAMLVERSION=$(shell ocamlc -version)

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

%.cmi: %.mli
	$(OCAMLC) -c $<

%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

decap.cmi: charset.cmi input.cmi ahash.cmi

decap.cmo: charset.cmi input.cmi ahash.cmi
decap.cmo: OCAMLFLAGS=-noassert

decap.cmx: charset.cmx charset.cmi input.cmx input.cmi ahash.cmi ahash.cmx
decap.cmx: OCAMLFLAGS=-noassert

decap.cmxa: charset.cmx input.cmx ahash.cmx decap.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) -a -o $@ $^

decap.cma: charset.cmo input.cmo ahash.cmo decap.cmo
	$(OCAMLC) $(OCAMLFLAGS) -a -o $@ $^

decap_ocaml.cmxa: bootstrap/$(OCAMLVERSION)/compare.cmx pa_ocaml_prelude.cmx pa_ast.cmx pa_parser.cmx pa_ocaml.cmx pa_main.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) -a -o $@ $^

decap_ocaml.cma: bootstrap/$(OCAMLVERSION)/compare.cmo pa_ocaml_prelude.cmo pa_ast.cmo pa_parser.cmo pa_ocaml.cmo pa_main.cmo
	$(OCAMLC) $(OCAMLFLAGS) -a -o $@ $^

decap.a: decap.cmxa;
decap_ocaml.a: decap_ocaml.cmxa;

ifeq ($(HAS_PA_OCAML),1)

all: decap.cmxa decap.cma decap_ocaml.cmxa decap_ocaml.cma

bootstrap/$(OCAMLVERSION)/compare.cmo: bootstrap/$(OCAMLVERSION)/compare.ml
	$(OCAMLC) $(OCAMLFLAGS) $(COMPILER_INC) -c $<

bootstrap/$(OCAMLVERSION)/compare.cmx: bootstrap/$(OCAMLVERSION)/compare.ml
	$(OCAMLOPT) $(OCAMLFLAGS) $(COMPILER_INC) -c $<

pa_ocaml_prelude.cmo: pa_ocaml_prelude.ml charset.cmi input.cmi decap.cmi
	$(OCAMLC) $(OCAMLFLAGS) -pp ./pa_ocaml $(COMPILER_INC) -c $<

pa_ast.cmo: pa_ast.ml pa_ocaml_prelude.cmo
	$(OCAMLC) $(OCAMLFLAGS) -pp ./pa_ocaml $(COMPILER_INC) -c $<

pa_ast.cmx: pa_ast.ml pa_ocaml_prelude.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) -pp ./pa_ocaml $(COMPILER_INC) -c $<

pa_ocaml.cmo: pa_ocaml.ml pa_ocaml_prelude.cmo decap.cma
	$(OCAMLC) $(OCAMLFLAGS) -pp ./pa_ocaml $(COMPILER_INC) -c $<

pa_parser.cmo: pa_parser.ml pa_ast.cmo pa_ocaml_prelude.cmo  bootstrap/$(OCAMLVERSION)/compare.cmo decap.cma
	$(OCAMLC) $(OCAMLFLAGS) -pp ./pa_ocaml -I  bootstrap/$(OCAMLVERSION) $(COMPILER_INC) -c $<

pa_main.cmo: pa_main.ml input.cmi pa_ocaml.cmo
	$(OCAMLC) $(OCAMLFLAGS) -pp ./pa_ocaml $(COMPILER_INC) -c $<

pa_ocaml_prelude.cmx: pa_ocaml_prelude.ml charset.cmx input.cmx decap.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) -pp ./pa_ocaml $(COMPILER_INC) -c $<

pa_ocaml.cmx: pa_ocaml.ml pa_ocaml_prelude.cmx decap.cmxa
	$(OCAMLOPT) $(OCAMLFLAGS) -pp ./pa_ocaml $(COMPILER_INC) -c $<

pa_parser.cmx: pa_parser.ml pa_ast.cmx pa_ocaml_prelude.cmx decap.cmxa bootstrap/$(OCAMLVERSION)/compare.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) -pp ./pa_ocaml -I bootstrap/$(OCAMLVERSION) $(COMPILER_INC) -c $<

pa_opt_main.ml: pa_main.ml
	cp pa_main.ml pa_opt_main.ml

pa_main.cmx: pa_main.ml input.cmi input.cmx pa_ocaml.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) -pp ./pa_ocaml $(COMPILER_INC) -c $<

pa_default.cmo: pa_default.ml pa_ocaml_prelude.cmo pa_parser.cmo pa_ocaml.cmo pa_main.cmo
	$(OCAMLC) $(OCAMLFLAGS) -pp ./pa_ocaml $(COMPILER_INC) -c $<

pa_default.cmx: pa_default.ml pa_ocaml_prelude.cmx pa_parser.cmx pa_ocaml.cmx pa_main.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) -pp ./pa_ocaml $(COMPILER_INC) -c $<

pa_ocaml: decap.cmxa decap_ocaml.cmxa pa_default.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) $(COMPILER_INC) -linkall -o $@ unix.cmxa str.cmxa $(COMPILER_LIBO) $^

pa_ocaml.byt: decap.cma decap_ocaml.cma pa_default.cmo
	$(OCAMLC) $(OCAMLFLAGS) $(COMPILER_INC) -linkall -o $@ unix.cma str.cma $(COMPILER_LIBS) $^

test_parsers: decap.cmxa decap_ocaml.cmxa test_parsers.ml
	$(OCAMLOPT) $(OCAMLFLAGS) $(COMPILER_INC) -I +camlp4 -I +camlp4/Camlp4Parsers -o $@ dynlink.cmxa unix.cmxa str.cmxa camlp4lib.cmxa Camlp4OCamlRevisedParser.cmx Camlp4OCamlParser.cmx	$(COMPILER_INC) $(COMPILER_LIBO) $(COMPILER_PARSERO) $^

else

bootstrap/$(OCAMLVERSION)/compare.cmo: bootstrap/$(OCAMLVERSION)/compare.ml
	$(OCAMLC) $(OCAMLFLAGS) $(COMPILER_INC) -c -I bootstrap/$(OCAMLVERSION) $<

bootstrap/$(OCAMLVERSION)/compare.cmx: bootstrap/$(OCAMLVERSION)/compare.ml
	$(OCAMLOPT) $(OCAMLFLAGS) $(COMPILER_INC) -I bootstrap/$(OCAMLVERSION) -c $<

bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmo: bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.ml charset.cmi input.cmi decap.cmi
	$(OCAMLC) $(OCAMLFLAGS) $(COMPILER_INC) -c -I bootstrap/$(OCAMLVERSION) $<

bootstrap/$(OCAMLVERSION)/pa_ocaml.cmo: bootstrap/$(OCAMLVERSION)/pa_ocaml.ml bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmo decap.cma
	$(OCAMLC) $(OCAMLFLAGS) $(COMPILER_INC) -c -I bootstrap/$(OCAMLVERSION) $<

bootstrap/$(OCAMLVERSION)/pa_parser.cmo: bootstrap/$(OCAMLVERSION)/pa_parser.ml bootstrap/$(OCAMLVERSION)/pa_ast.cmo bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmo  bootstrap/$(OCAMLVERSION)/compare.cmo decap.cma
	$(OCAMLC) $(OCAMLFLAGS) $(COMPILER_INC) -c -I bootstrap/$(OCAMLVERSION) $<

bootstrap/$(OCAMLVERSION)/pa_main.cmo: bootstrap/$(OCAMLVERSION)/pa_main.ml input.cmi bootstrap/$(OCAMLVERSION)/pa_ocaml.cmo
	$(OCAMLC) $(OCAMLFLAGS) $(COMPILER_INC) -c -I bootstrap/$(OCAMLVERSION) $<

pa_ocaml.byt: decap.cma bootstrap/$(OCAMLVERSION)/compare.cmo bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmo bootstrap/$(OCAMLVERSION)/pa_ast.cmo bootstrap/$(OCAMLVERSION)/pa_parser.cmo bootstrap/$(OCAMLVERSION)/pa_ocaml.cmo bootstrap/$(OCAMLVERSION)/pa_main.cmo
	$(OCAMLC) $(OCAMLFLAGS) $(COMPILER_INC) -o $@ unix.cma str.cma  $(COMPILER_LIBS) $(COMPILER_TOP) $^

bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmx: bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.ml charset.cmx input.cmx decap.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) $(COMPILER_INC) -I bootstrap/$(OCAMLVERSION) -c $<

bootstrap/$(OCAMLVERSION)/pa_ocaml.cmx: bootstrap/$(OCAMLVERSION)/pa_ocaml.ml bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmx decap.cmxa
	$(OCAMLOPT) $(OCAMLFLAGS) $(COMPILER_INC) -I bootstrap/$(OCAMLVERSION) -c $<

bootstrap/$(OCAMLVERSION)/pa_parser.cmx: bootstrap/$(OCAMLVERSION)/pa_parser.ml bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmx bootstrap/$(OCAMLVERSION)/pa_ast.cmx  bootstrap/$(OCAMLVERSION)/compare.cmx decap.cmxa
	$(OCAMLOPT) $(OCAMLFLAGS) $(COMPILER_INC) -I bootstrap/$(OCAMLVERSION) -c $<

bootstrap/$(OCAMLVERSION)/pa_main.cmx: bootstrap/$(OCAMLVERSION)/pa_main.ml input.cmi input.cmx bootstrap/$(OCAMLVERSION)/pa_ocaml.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) $(COMPILER_INC) -I bootstrap/$(OCAMLVERSION) -c $<

bootstrap/$(OCAMLVERSION)/decap_ocaml.cmxa: bootstrap/$(OCAMLVERSION)/compare.cmx bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmx bootstrap/$(OCAMLVERSION)/pa_ast.cmx bootstrap/$(OCAMLVERSION)/pa_parser.cmx bootstrap/$(OCAMLVERSION)/pa_ocaml.cmx bootstrap/$(OCAMLVERSION)/pa_main.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) -a -o $@ $^

bootstrap/$(OCAMLVERSION)/decap_ocaml.cma: bootstrap/$(OCAMLVERSION)/compare.cmo bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmo bootstrap/$(OCAMLVERSION)/pa_ast.cmo bootstrap/$(OCAMLVERSION)/pa_parser.cmo bootstrap/$(OCAMLVERSION)/pa_ocaml.cmo bootstrap/$(OCAMLVERSION)/pa_main.cmo
	$(OCAMLC) $(OCAMLFLAGS) -a -o $@ $^

bootstrap/$(OCAMLVERSION)/pa_default.cmo: bootstrap/$(OCAMLVERSION)/pa_default.ml bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmo bootstrap/$(OCAMLVERSION)/pa_parser.cmo bootstrap/$(OCAMLVERSION)/pa_ocaml.cmo bootstrap/$(OCAMLVERSION)/pa_main.cmo
	$(OCAMLC) $(OCAMLFLAGS) $(COMPILER_INC) -I bootstrap/$(OCAMLVERSION) -c $<

bootstrap/$(OCAMLVERSION)/pa_default.cmx: bootstrap/$(OCAMLVERSION)/pa_default.ml bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmx bootstrap/$(OCAMLVERSION)/pa_parser.cmx bootstrap/$(OCAMLVERSION)/pa_ocaml.cmx bootstrap/$(OCAMLVERSION)/pa_main.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) $(COMPILER_INC) -I bootstrap/$(OCAMLVERSION) -c $<

bootstrap/$(OCAMLVERSION)/pa_ast.cmo: bootstrap/$(OCAMLVERSION)/pa_ast.ml bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmo
	$(OCAMLC) $(OCAMLFLAGS) $(COMPILER_INC) -I bootstrap/$(OCAMLVERSION) -c $<

bootstrap/$(OCAMLVERSION)/pa_ast.cmx: bootstrap/$(OCAMLVERSION)/pa_ast.ml bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) $(COMPILER_INC) -I bootstrap/$(OCAMLVERSION) -c $<

pa_ocaml: decap.cmxa bootstrap/$(OCAMLVERSION)/decap_ocaml.cmxa bootstrap/$(OCAMLVERSION)/pa_default.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) $(COMPILER_INC)  -I bootstrap/$(OCAMLVERSION) -o $@ unix.cmxa str.cmxa $(COMPILER_LIBO) $^

endif

boot: BACKUP:=bootstrap/$(OCAMLVERSION)/$(shell date +%Y-%m-%d-%H-%M-%S)
boot:
	- if [ ! -d bootstrap/$(OCAMLVERSION) ] ; then mkdir bootstrap/$(OCAMLVERSION); fi
	- if [ ! -d $(BACKUP) ] ; then \
	     mkdir $(BACKUP) ; \
	     cp bootstrap/$(OCAMLVERSION)/*.ml $(BACKUP) ; \
	fi
	export OCAMLVERSION=$(OCAMLVERSION); \
	./pa_ocaml --ascii pa_ocaml_prelude.ml > bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.ml ;\
	./pa_ocaml --ascii pa_parser.ml > bootstrap/$(OCAMLVERSION)/pa_parser.ml ;\
	if [ $(OCAMLVERSION)=3.12.1 ] ; then \
		perl -i.original -pe 's/\(module Ext\) : \(module FExt\)/module Ext : FExt/' bootstrap/$(OCAMLVERSION)/pa_parser.ml; \
	fi; \
	./pa_ocaml --ascii pa_ocaml.ml > bootstrap/$(OCAMLVERSION)/pa_ocaml.ml ;\
	./pa_ocaml --ascii pa_ast.ml > bootstrap/$(OCAMLVERSION)/pa_ast.ml ;\
	./pa_ocaml --ascii pa_main.ml > bootstrap/$(OCAMLVERSION)/pa_main.ml ;\
	./pa_ocaml --ascii pa_default.ml > bootstrap/$(OCAMLVERSION)/pa_default.ml


install: uninstall $(INSTALLED)
	install -m 755 -d $(DESTDIR)/$(LIBDIR)
	install -m 755 -d $(DESTDIR)/$(BINDIR)
	ocamlfind install -destdir $(DESTDIR)/$(dir $(LIBDIR)) decap META $(INSTALLED)
	install -m 755 pa_ocaml $(DESTDIR)/$(BINDIR)

uninstall:
	ocamlfind remove -destdir $(DESTDIR)/$(dir $(LIBDIR)) decap
	rm -rf $(DESTDIR)/$(LIBDIR)
	rm -f $(DESTDIR)/$(BINDIR)/pa_ocaml

clean:
	- rm -f *.cm* *.o *.a
	- rm -f bootstrap/*/*.cm* bootstrap/*/*.o bootstrap/*/*.a
	$(MAKE) -e -j 1 -C ast_tools clean

distclean: clean
	- rm -f pa_ocaml pa_ocaml.byt
	$(MAKE) -e -j 1 -C ast_tools distclean

doc: decap.mli charset.mli input.mli
	mkdir -p html
	ocamldoc -d html -html decap.mli charset.mli input.mli
