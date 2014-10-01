OCAMLC = ocamlc.opt
OCAMLOPT = ocamlopt.opt
BINDIR = $(DESTDIR)/usr/local/bin
LIBDIR = $(DESTDIR)$(shell ocamlc -where)/decap

# do not add decap.cm(x)a because decap in bootstrap is
# does not contain pa_ocaml_prelude and adding decap.cm(x)a
# here will make fail after make distclean
# a more complete all is given below when pa_ocaml binary
# is present

INSTALLED = decap.cmi decap.cmo decap.mli decap.cmx charset.cmi charset.cmo charset.mli charset.cmx input.cmi input.cmo input.mli input.cmx decap.cma decap.cmxa decap.a pa_ocaml_prelude.cmi pa_ocaml_prelude.cmo pa_ocaml_prelude.cmx pa_ocaml.cmi pa_ocaml.cmo pa_ocaml.cmx pa_parser.cmi pa_parser.cmx pa_parser.cmo pa_compose.cmi pa_compose.cmo pa_compose.cmx pa_opt_main.cmi pa_opt_main.cmx pa_byt_main.cmi pa_byt_main.cmo decap_ocaml.cmxa decap_ocaml.cma decap.a decap_ocaml.a

all: pa_ocaml

%.cmi: %.mli
	$(OCAMLC) -c $< 

%.cmo: %.ml %.cmi
	$(OCAMLC) -c $< 

%.cmx: %.ml %.cmi
	$(OCAMLOPT) -c $< 

decap.cmi: charset.cmi input.cmi

decap.cmo: charset.cmi input.cmi

decap.cmx: charset.cmx charset.cmi input.cmx input.cmi

decap.cmxa: charset.cmx input.cmx decap.cmx pa_ocaml_prelude.cmx
	$(OCAMLOPT) -a -o $@ $^

decap.cma: charset.cmo input.cmo decap.cmo pa_ocaml_prelude.cmo
	$(OCAMLC) -a -o $@ $^

decap_ocaml.cmxa: pa_ocaml.cmx pa_compose.cmx pa_opt_main.cmx
	$(OCAMLOPT) -a -o $@ $^

decap_ocaml.cma: pa_ocaml.cmo pa_compose.cmo pa_byt_main.cmo
	$(OCAMLC) -a -o $@ $^

decap.a: decap.cmxa;
decap_ocaml.a: decap_ocaml.cmxa;

HAS_PA_OCAML=$(shell if [ -x pa_ocaml ]; then echo 1; else echo 0; fi)

OCAMLVERSION=$(shell ocamlc -version)

ifeq ($(OCAMLVERSION),3.12.1)
COMPILER_INC = -I +compiler-libs/parsing -I +compiler-libs/typing -I +compiler-libs/utils
COMPILER_LIBS = misc.cmo config.cmo longident.cmo printast.cmo linenum.cmo warnings.cmo location.cmo 
COMPILER_TOP = toplevellib.cma
else
COMPILER_INC = -I +compiler-libs
COMPILER_LIBS = ocamlcommon.cma
COMPILER_TOP = ocamlbytecomp.cma ocamltoplevel.cma
endif

COMPILER_LIBO := $(COMPILER_LIBS:.cma=.cmxa)
COMPILER_LIBO := $(COMPILER_LIBO:.cmo=.cmx)

ifeq ($(HAS_PA_OCAML),1)

all: decap.cmxa decap.cma decap_ocaml.cmxa decap_ocaml.cma pa_parser.cmx pa_parser.cmo

pa_compose.cmo: pa_compose.ml decap.cmo pa_ocaml_prelude.cmo pa_ocaml.cmo decap.cma
	$(OCAMLC) $(COMPILER_INC) -c $< 

pa_compose.cmx: pa_compose.ml pa_ocaml_prelude.cmx pa_ocaml.cmx decap.cmxa
	$(OCAMLOPT) $(COMPILER_INC) -c $< 

pa_ocaml_prelude.cmo: pa_ocaml_prelude.ml charset.cmi input.cmi decap.cmi 
	$(OCAMLC) -pp ./pa_ocaml $(COMPILER_INC) -c $<

pa_ast.cmo: pa_ast.ml pa_ocaml_prelude.cmo
	$(OCAMLC) $(COMPILER_INC) -c $< 

pa_ast.cmx: pa_ast.ml pa_ocaml_prelude.cmx
	$(OCAMLOPT) $(COMPILER_INC) -c $< 

pa_ocaml.cmo: pa_ocaml.ml pa_ocaml_prelude.cmo decap.cma
	$(OCAMLC) -pp ./pa_ocaml $(COMPILER_INC) -c $< 

pa_parser.cmo: pa_parser.ml pa_ast.cmo pa_ocaml_prelude.cmo decap.cma
	$(OCAMLC) -pp ./pa_ocaml $(COMPILER_INC) -c $<

pa_byt_main.ml: pa_main.ml
	cp pa_main.ml pa_byt_main.ml

pa_byt_main.cmo: pa_byt_main.ml pa_compose.cmo input.cmi pa_ocaml.cmo
	export BYTE=1; $(OCAMLC) -pp ./pa_ocaml $(COMPILER_INC) -c $<

pa_ocaml.byt: decap.cma pa_parser.cmo decap_ocaml.cma
	$(OCAMLC) $(COMPILER_INC) -linkall -o $@ unix.cma str.cma $(COMPILER_LIBS) $(COMPILER_TOP) $^

pa_ocaml_prelude.cmx: pa_ocaml_prelude.ml charset.cmx input.cmx decap.cmx 
	$(OCAMLOPT) -pp ./pa_ocaml $(COMPILER_INC) -c $< 

pa_ocaml.cmx: pa_ocaml.ml pa_ocaml_prelude.cmx decap.cmxa
	$(OCAMLOPT) -pp ./pa_ocaml $(COMPILER_INC) -c $< 

pa_parser.cmx: pa_parser.ml pa_ast.cmx pa_ocaml_prelude.cmx decap.cmxa
	$(OCAMLOPT) -pp ./pa_ocaml $(COMPILER_INC) -c $< 

pa_opt_main.ml: pa_main.ml
	cp pa_main.ml pa_opt_main.ml

pa_opt_main.cmx: pa_opt_main.ml pa_compose.cmx input.cmi input.cmx pa_ocaml.cmx 
	$(OCAMLOPT) -pp ./pa_ocaml $(COMPILER_INC) -c $<

pa_ocaml: decap.cmxa pa_ast.cmx pa_parser.cmx decap_ocaml.cmxa 
	$(OCAMLOPT) $(COMPILER_INC) -linkall -o $@ unix.cmxa str.cmxa $(COMPILER_LIBO) $^

test_parsers: decap.cmxa pa_ast.cmx pa_parser.cmx pa_ocaml.cmx pa_compose.cmx test_parsers.ml
	$(OCAMLOPT) $(COMPILER_INC) -I +camlp4 -I +camlp4/Camlp4Parsers -o $@ dynlink.cmxa unix.cmxa str.cmxa camlp4lib.cmxa Camlp4OCamlRevisedParser.cmx Camlp4OCamlParser.cmx	$(COMPILER_INC) $(COMPILER_LIBO) $^

else

#decap in bootstrap does not contain pa_ocaml_prelude

bootstrap/$(OCAMLVERSION)/decap.cmxa: charset.cmx input.cmx decap.cmx
	$(OCAMLOPT) -a -o $@ $^

bootstrap/$(OCAMLVERSION)/decap.cma: charset.cmo input.cmo decap.cmo
	$(OCAMLC) -a -o $@ $^

pa_compose.cmo: pa_compose.ml bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmo bootstrap/$(OCAMLVERSION)/pa_ocaml.cmo bootstrap/$(OCAMLVERSION)/decap.cma
	$(OCAMLC) $(COMPILER_INC) -c -I bootstrap/$(OCAMLVERSION) $< 

pa_compose.cmx: pa_compose.ml bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmx bootstrap/$(OCAMLVERSION)/pa_ocaml.cmx bootstrap/$(OCAMLVERSION)/decap.cmxa
	$(OCAMLOPT) $(COMPILER_INC) -c -I bootstrap/$(OCAMLVERSION) $< 

pa_ast.cmo: pa_ast.ml bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmo
	$(OCAMLC) $(COMPILER_INC) -I bootstrap/$(OCAMLVERSION) -c $< 

pa_ast.cmx: pa_ast.ml bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmx
	$(OCAMLOPT) $(COMPILER_INC) -I bootstrap/$(OCAMLVERSION) -c $< 

bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmo: bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.ml charset.cmi input.cmi decap.cmi
	$(OCAMLC) $(COMPILER_INC) -c -I bootstrap/$(OCAMLVERSION) $<

bootstrap/$(OCAMLVERSION)/pa_ocaml.cmo: bootstrap/$(OCAMLVERSION)/pa_ocaml.ml bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmo bootstrap/$(OCAMLVERSION)/decap.cma
	$(OCAMLC) $(COMPILER_INC) -c -I bootstrap/$(OCAMLVERSION) $< 

bootstrap/$(OCAMLVERSION)/pa_parser.cmo: bootstrap/$(OCAMLVERSION)/pa_parser.ml pa_ast.cmo bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmo bootstrap/$(OCAMLVERSION)/decap.cma
	$(OCAMLC) $(COMPILER_INC) -c -I bootstrap/$(OCAMLVERSION) $<

bootstrap/$(OCAMLVERSION)/pa_byt_main.cmo: bootstrap/$(OCAMLVERSION)/pa_byt_main.ml pa_compose.cmo input.cmi bootstrap/$(OCAMLVERSION)/pa_ocaml.cmo 
	$(OCAMLC) $(COMPILER_INC) -c -I bootstrap/$(OCAMLVERSION) $<

pa_ocaml.byt: bootstrap/$(OCAMLVERSION)/decap.cma bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmo pa_ast.cmo bootstrap/$(OCAMLVERSION)/pa_parser.cmo bootstrap/$(OCAMLVERSION)/pa_ocaml.cmo pa_compose.cmo bootstrap/$(OCAMLVERSION)/pa_byt_main.cmo
	$(OCAMLC) $(COMPILER_INC) -o $@ unix.cma str.cma  $(COMPILER_LIBS) $(COMPILER_TOP) $^

bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmx: bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.ml charset.cmx input.cmx decap.cmx
	$(OCAMLOPT) $(COMPILER_INC) -I bootstrap/$(OCAMLVERSION) -c $< 

bootstrap/$(OCAMLVERSION)/pa_ocaml.cmx: bootstrap/$(OCAMLVERSION)/pa_ocaml.ml bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmx bootstrap/$(OCAMLVERSION)/decap.cmxa
	$(OCAMLOPT) $(COMPILER_INC) -I bootstrap/$(OCAMLVERSION) -c $< 

bootstrap/$(OCAMLVERSION)/pa_parser.cmx: bootstrap/$(OCAMLVERSION)/pa_parser.ml bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmx pa_ast.cmx bootstrap/$(OCAMLVERSION)/decap.cmxa
	$(OCAMLOPT) $(COMPILER_INC) -I bootstrap/$(OCAMLVERSION) -c $< 

bootstrap/$(OCAMLVERSION)/pa_opt_main.cmx: bootstrap/$(OCAMLVERSION)/pa_opt_main.ml pa_compose.cmx input.cmi input.cmx bootstrap/$(OCAMLVERSION)/pa_ocaml.cmx 
	$(OCAMLOPT) $(COMPILER_INC) -I bootstrap/$(OCAMLVERSION) -c $<

pa_ocaml: bootstrap/$(OCAMLVERSION)/decap.cmxa bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmx pa_ast.cmx bootstrap/$(OCAMLVERSION)/pa_parser.cmx bootstrap/$(OCAMLVERSION)/pa_ocaml.cmx pa_compose.cmx bootstrap/$(OCAMLVERSION)/pa_opt_main.cmx 
	$(OCAMLOPT) $(COMPILER_INC)  -I bootstrap/$(OCAMLVERSION) -o $@ unix.cmxa str.cmxa $(COMPILER_LIBO) $^

endif

boot: BACKUP:=bootstrap/$(OCAMLVERSION)/$(shell date +%Y-%m-%d-%H-%M-%S)
boot:
	- if [ ! -d bootstrap/$(OCAMLVERSION) ] ; then mkdir bootstrap/$(OCAMLVERSION); fi
	- if [ ! -d $(BACKUP) ] ; then \
	     mkdir $(BACKUP) ; \
	     mv bootstrap/$(OCAMLVERSION)/*.ml $(BACKUP) ; \
	fi
	export OCAMLVERSION=$(OCAMLVERSION); \
	./pa_ocaml --ascii pa_ocaml_prelude.ml > bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.ml ;\
	./pa_ocaml --ascii pa_parser.ml > bootstrap/$(OCAMLVERSION)/pa_parser.ml ;\
	if [ $(OCAMLVERSION)=3.12.1 ] ; then \
		perl -i.original -pe 's/\(module Ext\) : \(module FExt\)/module Ext : FExt/' bootstrap/$(OCAMLVERSION)/pa_parser.ml; \
	fi; \
	./pa_ocaml --ascii pa_ocaml.ml > bootstrap/$(OCAMLVERSION)/pa_ocaml.ml ;\
	./pa_ocaml --ascii pa_main.ml > bootstrap/$(OCAMLVERSION)/pa_opt_main.ml ;\
	export BYTE=1 ;\
	./pa_ocaml --ascii pa_main.ml > bootstrap/$(OCAMLVERSION)/pa_byt_main.ml

install: uninstall $(INSTALLED)
	install -m 755 -d $(LIBDIR)
	install -m 755 -d $(BINDIR)
	ocamlfind install -destdir $(dir $(LIBDIR)) decap META $(INSTALLED)
	install -m 755 pa_ocaml $(BINDIR)

uninstall:
	ocamlfind remove -destdir $(dir $(LIBDIR)) decap
	rm -rf $(LIBDIR)
	rm -f $(BINDIR)/pa_ocaml

clean:
	- rm -f *.cmi *.cmo *.cmx *.o *.a *.cma *.cmxa
	- rm -f bootstrap/*/*.cm* bootstrap/*/*.o bootstrap/*/*.a

distclean: clean
	- rm -f pa_ocaml pa_ocaml.byt

doc: decap.mli charset.mli input.mli
	mkdir -p html
	ocamldoc -d html -html decap.mli charset.mli input.mli
