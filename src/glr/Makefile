FINDLIB =
CAMLFLAGS =
OCAMLC = $(FINDLIB) ocamlc.opt $(CAMLFLAGS)
OCAMLOPT = $(FINDLIB) ocamlopt.opt $(CAMLFLAGS)
BINDIR = /usr/local/bin
LIBDIR = /$(shell ocamlc -where)/glr

all: decap.cmxa decap.cma decap.cmi doc pa_ocaml

%.cmi: %.mli
	$(OCAMLC) -c $< 

%.cmo: %.ml %.cmi
	$(OCAMLC) -c $< 

%.cmx: %.ml %.cmi
	$(OCAMLOPT) -c $< 

decap.cmi: charset.cmi input.cmi

decap.cmo: charset.cmi input.cmi

decap.cmx: charset.cmx charset.cmi input.cmx input.cmi

decap.cmxa: charset.cmx input.cmx decap.cmx
	$(OCAMLOPT) -a -o decap.cmxa $^

decap.cma: charset.cmo input.cmo decap.cmo
	$(OCAMLC) -a -o decap.cma $^

HAS_PA_OCAML=$(shell if [ -x pa_ocaml ]; then echo 1; else echo 0; fi)

OCAMLVERSION=$(shell ocamlc -version)

ifeq ($(OCAMLVERSION),3.12.1)
COMPILER_INC = -I +compiler-libs/parsing -I +compiler-libs/typing -I +compiler-libs/utils
COMPILER_LIBS = misc.cmo config.cmo longident.cmo printast.cmo
COMPILER_TOP = toplevellib.cma
else
COMPILER_INC = -I +compiler-libs
COMPILER_LIBS = ocamlcommon.cma
COMPILER_TOP = ocamlbytecomp.cma ocamltoplevel.cma
endif

COMPILER_LIBO := $(COMPILER_LIBS:.cma=.cmxa)
COMPILER_LIBO := $(COMPILER_LIBO:.cmo=.cmx)

ifeq ($(HAS_PA_OCAML),1)

pa_compose.cmo: pa_compose.ml pa_ocaml_prelude.cmo pa_ocaml.cmo decap.cma
	$(OCAMLC) $(COMPILER_INC) -c $< 

pa_compose.cmx: pa_compose.ml pa_ocaml_prelude.cmx pa_ocaml.cmx decap.cmxa
	$(OCAMLOPT) $(COMPILER_INC) -c $< 

pa_ocaml_prelude.cmo: pa_ocaml_prelude.ml decap.cma
	$(OCAMLC) -pp ./pa_ocaml $(COMPILER_INC) -c $<

pa_ocaml.cmo: pa_ocaml.ml pa_ocaml_prelude.cmo decap.cma
	$(OCAMLC) -pp ./pa_ocaml $(COMPILER_INC) -c $< 

pa_parser.cmo: pa_parser.ml pa_ocaml_prelude.cmo decap.cma
	$(OCAMLC) -pp ./pa_ocaml $(COMPILER_INC) -c $<

pa_byt_main.ml: pa_main.ml
	cp pa_main.ml pa_byt_main.ml

pa_byt_main.cmo: pa_byt_main.ml pa_compose.cmo input.cmi pa_ocaml.cmo
	export BYTE=1; $(OCAMLC) -pp ./pa_ocaml $(COMPILER_INC) -c $<

pa_ocaml.byt: decap.cma pa_ocaml_prelude.cmo pa_parser.cmo pa_ocaml.cmo pa_compose.cmo pa_byt_main.cmo
	$(OCAMLC) $(COMPILER_INC) -o $@ unix.cma str.cma $(COMPILER_LIBS) $(COMPILER_TOP) $^

pa_ocaml_prelude.cmx: pa_ocaml_prelude.ml decap.cmxa
	$(OCAMLOPT) -pp ./pa_ocaml $(COMPILER_INC) -c $< 

pa_ocaml.cmx: pa_ocaml.ml pa_ocaml_prelude.cmx decap.cmxa
	$(OCAMLOPT) -pp ./pa_ocaml $(COMPILER_INC) -c $< 

pa_parser.cmx: pa_parser.ml pa_ocaml_prelude.cmx decap.cmxa
	$(OCAMLOPT) -pp ./pa_ocaml $(COMPILER_INC) -c $< 

pa_opt_main.ml: pa_main.ml
	cp pa_main.ml pa_opt_main.ml

pa_opt_main.cmx: pa_opt_main.ml pa_compose.cmx input.cmi input.cmx pa_ocaml.cmx 
	$(OCAMLOPT) -pp ./pa_ocaml $(COMPILER_INC) -c $<

pa_ocaml: decap.cmxa pa_ocaml_prelude.cmx pa_parser.cmx pa_ocaml.cmx pa_compose.cmx pa_opt_main.cmx 
	$(OCAMLOPT) $(COMPILER_INC)  -o $@ unix.cmxa str.cmxa $(COMPILER_LIBO) $^

test_parsers: decap.cmxa pa_ocaml_prelude.cmx pa_parser.cmx pa_ocaml.cmx pa_compose.cmx test_parsers.ml
	$(OCAMLOPT) $(COMPILER_INC) -I +camlp4 -I +camlp4/Camlp4Parsers -o $@ dynlink.cmxa unix.cmxa str.cmxa camlp4lib.cmxa Camlp4OCamlRevisedParser.cmx Camlp4OCamlParser.cmx	$(COMPILER_INC) $(COMPILER_LIBO) $^

else

pa_compose.cmo: pa_compose.ml bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmo bootstrap/$(OCAMLVERSION)/pa_ocaml.cmo decap.cma
	$(OCAMLC) $(COMPILER_INC) -c -I bootstrap/$(OCAMLVERSION) $< 

pa_compose.cmx: pa_compose.ml bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmx bootstrap/$(OCAMLVERSION)/pa_ocaml.cmx decap.cmxa
	$(OCAMLOPT) $(COMPILER_INC) -c -I bootstrap/$(OCAMLVERSION) $< 

bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmo: bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.ml decap.cma
	$(OCAMLC) $(COMPILER_INC) -c -I bootstrap/$(OCAMLVERSION) $<

bootstrap/$(OCAMLVERSION)/pa_ocaml.cmo: bootstrap/$(OCAMLVERSION)/pa_ocaml.ml bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmo decap.cma
	$(OCAMLC) $(COMPILER_INC) -c -I bootstrap/$(OCAMLVERSION) $< 

bootstrap/$(OCAMLVERSION)/pa_parser.cmo: bootstrap/$(OCAMLVERSION)/pa_parser.ml bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmo decap.cma
	$(OCAMLC) $(COMPILER_INC) -c -I bootstrap/$(OCAMLVERSION) $<

bootstrap/$(OCAMLVERSION)/pa_byt_main.cmo: bootstrap/$(OCAMLVERSION)/pa_byt_main.ml pa_compose.cmo input.cmi bootstrap/$(OCAMLVERSION)/pa_ocaml.cmo 
	$(OCAMLC) $(COMPILER_INC) -c -I bootstrap/$(OCAMLVERSION) $<

pa_ocaml.byt: decap.cma bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmo bootstrap/$(OCAMLVERSION)/pa_parser.cmo bootstrap/$(OCAMLVERSION)/pa_ocaml.cmo pa_compose.cmo bootstrap/$(OCAMLVERSION)/pa_byt_main.cmo
	$(OCAMLC) $(COMPILER_INC) -o $@ unix.cma str.cma  $(COMPILER_LIBS) $(COMPILER_TOP) $^

bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmx: bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.ml decap.cmxa
	$(OCAMLOPT) $(COMPILER_INC) -I bootstrap/$(OCAMLVERSION) -c $< 

bootstrap/$(OCAMLVERSION)/pa_ocaml.cmx: bootstrap/$(OCAMLVERSION)/pa_ocaml.ml bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmx decap.cmxa
	$(OCAMLOPT) $(COMPILER_INC) -I bootstrap/$(OCAMLVERSION) -c $< 

bootstrap/$(OCAMLVERSION)/pa_parser.cmx: bootstrap/$(OCAMLVERSION)/pa_parser.ml bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmx decap.cmxa
	$(OCAMLOPT) $(COMPILER_INC) -I bootstrap/$(OCAMLVERSION) -c $< 

bootstrap/$(OCAMLVERSION)/pa_opt_main.cmx: bootstrap/$(OCAMLVERSION)/pa_opt_main.ml pa_compose.cmx input.cmi input.cmx bootstrap/$(OCAMLVERSION)/pa_ocaml.cmx 
	$(OCAMLOPT) $(COMPILER_INC) -I bootstrap/$(OCAMLVERSION) -c $<

pa_ocaml: decap.cmxa bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmx bootstrap/$(OCAMLVERSION)/pa_parser.cmx bootstrap/$(OCAMLVERSION)/pa_ocaml.cmx pa_compose.cmx bootstrap/$(OCAMLVERSION)/pa_opt_main.cmx 
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

examples/pa_test_ext.cmx: examples/pa_test_ext.ml pa_ocaml
	$(OCAMLOPT) -pp ./pa_ocaml -c $<

examples/pa_test_ext: decap.cmxa pa_ocaml_prelude.cmx examples/pa_test_ext.cmx pa_ocaml.cmx pa_compose.cmx pa_opt_main.cmx 
	$(OCAMLOPT) $(COMPILER_INC)  -o $@ unix.cmxa str.cmxa $(COMPILER_LIBO) $^

examples/bug: examples/bug.ml decap.cmxa
	$(OCAMLOPT) -pp ./pa_ocaml str.cmxa unix.cmxa bigarray.cmxa decap.cmxa $< -o $@

examples/calc: examples/calc.cmx decap.cmxa
	$(OCAMLOPT) str.cmxa unix.cmxa bigarray.cmxa decap.cmxa $< -o $@

examples/calc.cmx: examples/calc.ml decap.cmxa pa_ocaml
	$(OCAMLOPT) -pp ./pa_ocaml -c $<

examples/calc_all: examples/calc_all.cmx decap.cmxa
	$(OCAMLOPT) str.cmxa unix.cmxa bigarray.cmxa decap.cmxa $< -o $@

examples/calc_all.cmx: examples/calc_all.ml decap.cmxa pa_ocaml
	$(OCAMLOPT) -pp ./pa_ocaml -c $<

examples/calc_prio: examples/calc_prio.cmx decap.cmxa
	$(OCAMLOPT) str.cmxa unix.cmxa bigarray.cmxa decap.cmxa $< -o $@

examples/calc_prio.cmx: examples/calc_prio.ml decap.cmxa pa_ocaml
	$(OCAMLOPT) -pp ./pa_ocaml -c $<

examples/text: examples/text.cmx decap.cmxa
	$(OCAMLOPT) str.cmxa unix.cmxa bigarray.cmxa decap.cmxa $< -o $@

examples/text.cmx: examples/text.ml decap.cmxa pa_ocaml
	$(OCAMLOPT) -pp ./pa_ocaml -c $<

examples/text_opt: examples/text_opt.cmx decap.cmxa
	$(OCAMLOPT) str.cmxa unix.cmxa bigarray.cmxa decap.cmxa $< -o $@

examples/text_opt.cmx: examples/text_opt.ml decap.cmxa pa_ocaml
	$(OCAMLOPT) -pp ./pa_ocaml -c $<

examples/test_merge: examples/test_merge.cmx decap.cmxa
	$(OCAMLOPT) str.cmxa unix.cmxa bigarray.cmxa decap.cmxa $< -o $@

examples/test_merge.cmx: examples/test_merge.ml decap.cmxa
	$(OCAMLOPT) -c $<

examples/test_list: examples/test_list.cmx decap.cmxa
	$(OCAMLOPT) str.cmxa unix.cmxa bigarray.cmxa decap.cmxa $< -o $@

examples/test_list.cmx: examples/test_list.ml decap.cmxa
	$(OCAMLOPT) -pp ./pa_ocaml -c $<

#check: examples/calc examples/calc_strict examples/calc_simple examples/text examples/text_opt examples/test_merge examples/test_list
#	time examples/calc < benchmark/test3.txt
#	time examples/calc < benchmark/test2.txt
#	time examples/calc < benchmark/test1.txt
##	time examples/calc < benchmark/test5.txt
##	time examples/calc < benchmark/test4.txt
#	time examples/calc_simple < benchmark/test3.txt
#	time examples/calc_simple < benchmark/test2.txt
#	time examples/calc_simple < benchmark/test1.txt
#	time examples/calc_simple < benchmark/test5.txt
#	time examples/calc_simple < benchmark/test4.txt
#	time examples/calc_strict < benchmark/test3.txt
#	time examples/calc_strict < benchmark/test2.txt
#	time examples/calc_strict < benchmark/test1.txt
#	time examples/calc_strict < benchmark/test5.txt
#	time examples/calc_strict < benchmark/test4.txt
#	time examples/text < benchmark/text0.txt
#	time examples/text < benchmark/text1.txt
#	time examples/text < benchmark/text2.txt
#	time examples/text < benchmark/text3.txt
#	time examples/text_opt < benchmark/text0.txt
#	time examples/text_opt < benchmark/text1.txt
#	time examples/text_opt < benchmark/text2.txt
#	time examples/text_opt < benchmark/text3.txt
#	time examples/test_merge 15
#	time examples/test_list 12

install: uninstall decap.mli decap.cmxa decap.cma
	- mkdir $(DESTDIR)/$(LIBDIR)
	- ocamlfind remove -destdir $(DESTDIR)/$(dir $(LIBDIR)) glr
	ocamlfind install -destdir $(DESTDIR)/$(dir $(LIBDIR)) glr META decap.cmi decap.cmo decap.mli decap.cmx charset.cmi charset.cmo charset.mli charset.cmx decap.cma decap.cmxa decap.a
	install pa_ocaml $(DESTDIR)/$(BINDIR)

uninstall:
	- ocamlfind remove -destdir $(DESTDIR)/$(dir $(LIBDIR)) glr
	- rm -f $(DESTDIR)/$(BINDIR)/pa_ocaml
clean:
	- rm -f *.cmi *.cmo *.cmx *.o *.a *.cma *.cmxa 
	- rm -f bootstrap/*/*.cm* bootstrap/*/*.o bootstrap/*/*.a
	- cd examples; rm -f *.cmi *.cmo *.cmx *.o *.a *.cma *.cmxa calc calc_strict text text_opt test_merge
	- cd html; rm -f *

distclean: clean
	- rm -f pa_ocaml pa_ocaml.byt

doc: decap.mli charset.mli input.mli
	ocamldoc -d html -html decap.mli
	ocamldoc -d html -html charset.mli
	ocamldoc -d html -html input.mli
