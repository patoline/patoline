FINDLIB = #ocamlfind
CAMLFLAGS =
CAMLC = $(FINDLIB) ocamlc.opt $(CAMLFLAGS)
CAMLOPT = $(FINDLIB) ocamlopt.opt $(CAMLFLAGS)
BINDIR = /usr/local/bin
LIBDIR = #-destdir /usr/local/lib/ocaml
METADIR = #-metadir ...

ifeq ("$(METADIR)","")
	METADIR=$(LIBDIR)
endif

all: pa_glr glr.cmxa glr.cma glr.cmi doc pa_ocaml

%.cmi: %.mli
	$(CAMLC) -c $< 

%.cmo: %.ml %.cmi
	$(CAMLC) -c $< 

%.cmx: %.ml %.cmi
	$(CAMLOPT) -c $< 

glr.cmi: charset.cmi

glr.cmo: umap.cmi charset.cmi input.cmi

glr.cmx: umap.cmx umap.cmi charset.cmx charset.cmi input.cmx input.cmi

glr.cmxa: umap.cmx charset.cmx input.cmx glr.cmx
	$(CAMLOPT) -a -o glr.cmxa $^

glr.cma: umap.cmo charset.cmo input.cmo glr.cmo
	$(CAMLC) -a -o glr.cma $^

OCAMLVERSION=$(shell ocamlc -version)

bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmo: bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.ml glr.cma
	$(CAMLC) -I +compiler-libs -c -I bootstrap/$(OCAMLVERSION) $<

bootstrap/$(OCAMLVERSION)/pa_ocaml.cmo: bootstrap/$(OCAMLVERSION)/pa_ocaml.ml bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmo glr.cma
	$(CAMLC) -I +compiler-libs -c -I bootstrap/$(OCAMLVERSION) $< 

bootstrap/$(OCAMLVERSION)/pa_parser.cmo: bootstrap/$(OCAMLVERSION)/pa_parser.ml bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmo glr.cma
	$(CAMLC) -I +compiler-libs -c -I bootstrap/$(OCAMLVERSION) $<

pa_ocaml.byt: glr.cma bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmo bootstrap/$(OCAMLVERSION)/pa_parser.cmo bootstrap/$(OCAMLVERSION)/pa_ocaml.cmo 
	$(CAMLC) -I +compiler-libs -o $@ unix.cma str.cma ocamlcommon.cma $^

bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmx: bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.ml glr.cmxa
	$(CAMLOPT) -I +compiler-libs -I bootstrap/$(OCAMLVERSION) -c $< 

bootstrap/$(OCAMLVERSION)/pa_ocaml.cmx: bootstrap/$(OCAMLVERSION)/pa_ocaml.ml bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmx glr.cmxa
	$(CAMLOPT) -I +compiler-libs -I bootstrap/$(OCAMLVERSION) -c $< 

bootstrap/$(OCAMLVERSION)/pa_parser.cmx: bootstrap/$(OCAMLVERSION)/pa_parser.ml bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmx glr.cmxa
	$(CAMLOPT) -I +compiler-libs -I bootstrap/$(OCAMLVERSION) -c $< 

pa_ocaml: glr.cmxa bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.cmx bootstrap/$(OCAMLVERSION)/pa_parser.cmx bootstrap/$(OCAMLVERSION)/pa_ocaml.cmx 
	$(CAMLOPT) -I +compiler-libs  -I bootstrap/$(OCAMLVERSION) -o $@ unix.cmxa str.cmxa ocamlcommon.cmxa $^

boot: BACKUP:=bootstrap/$(OCAMLVERSION)/$(shell date +%Y-%m-%d-%H-%M-%S)
boot:
	if [ ! -d bootstrap/$(OCAMLVERSION) ] ; then mkdir bootstrap/$(OCAMLVERSION) ; \
	else if [ ! -d $(BACKUP) ] ; then mkdir $(BACKUP) ; fi ; \
	     mv bootstrap/$(OCAMLVERSION)/*.ml $(BACKUP) ; \
	fi
	export OCAMLVERSION=$(OCAMLVERSION); \
	./pa_ocaml --ascii pa_ocaml_prelude.ml > bootstrap/$(OCAMLVERSION)/pa_ocaml_prelude.ml ;\
	./pa_ocaml --ascii pa_parser.ml > bootstrap/$(OCAMLVERSION)/pa_parser.ml ;\
	./pa_ocaml --ascii pa_ocaml.ml > bootstrap/$(OCAMLVERSION)/pa_ocaml.ml ;\

examples/calc: examples/calc.cmx glr.cmxa
	$(CAMLOPT) str.cmxa unix.cmxa bigarray.cmxa glr.cmxa $< -o $@

examples/calc.cmx: examples/calc.ml glr.cmxa pa_glr
	$(CAMLOPT) -pp ./pa_glr -c $<

examples/calc_simple: examples/calc_simple.cmx glr.cmxa
	$(CAMLOPT) str.cmxa unix.cmxa bigarray.cmxa glr.cmxa $< -o $@

examples/calc_simple.cmx: examples/calc_simple.ml glr.cmxa pa_glr
	$(CAMLOPT) -pp ./pa_glr -c $<

examples/calc_strict: examples/calc_strict.cmx glr.cmxa
	$(CAMLOPT) str.cmxa unix.cmxa bigarray.cmxa glr.cmxa $< -o $@

examples/calc_strict.cmx: examples/calc_strict.ml glr.cmxa pa_glr
	$(CAMLOPT) -pp ./pa_glr -c $<

examples/text: examples/text.cmx glr.cmxa
	$(CAMLOPT) str.cmxa unix.cmxa bigarray.cmxa glr.cmxa $< -o $@

examples/text.cmx: examples/text.ml glr.cmxa pa_glr
	$(CAMLOPT) -pp ./pa_glr -c $<

examples/text_opt: examples/text_opt.cmx glr.cmxa
	$(CAMLOPT) str.cmxa unix.cmxa bigarray.cmxa glr.cmxa $< -o $@

examples/text_opt.cmx: examples/text_opt.ml glr.cmxa pa_glr
	$(CAMLOPT) -pp ./pa_glr -c $<

examples/test_merge: examples/test_merge.cmx glr.cmxa
	$(CAMLOPT) str.cmxa unix.cmxa bigarray.cmxa glr.cmxa $< -o $@

examples/test_merge.cmx: examples/test_merge.ml glr.cmxa
	$(CAMLOPT) -c $<

examples/test_list: examples/test_list.cmx glr.cmxa
	$(CAMLOPT) str.cmxa unix.cmxa bigarray.cmxa glr.cmxa $< -o $@

examples/test_list.cmx: examples/test_list.ml glr.cmxa
	$(CAMLOPT) -pp ./pa_glr -c $<

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

install: uninstall glr.mli glr.cmxa glr.cma
	- mkdir $(LIBDIR)
	- ocamlfind remove glr $(LIBDIR)
	ocamlfind install glr META glr.cmi glr.cmo glr.mli glr.cmx umap.cmi umap.cmo umap.mli umap.cmx charset.cmi charset.cmo charset.mli charset.cmx glr.cma glr.cmxa glr.a $(LIBDIR) $(METADIR)
	install pa_ocaml $(BINDIR)

uninstall:
	- ocamlfind remove glr $(LIBDIR) $(METADIR)
	- rm $(BINDIR)/pa_ocaml
clean:
	- rm -f *.cmi *.cmo *.cmx *.o *.a *.cma *.cmxa 
	- cd examples; rm -f *.cmi *.cmo *.cmx *.o *.a *.cma *.cmxa calc calc_strict text text_opt test_merge
	- cd html; rm -f *

doc: glr.mli
	ocamldoc -d html -html glr.mli
