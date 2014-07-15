FINDLIB = ocamlfind
CAMLFLAGS = -pp "camlp4o pa_macro.cmo"
CAMLC = $(FINDLIB) ocamlc $(CAMLFLAGS)
CAMLOPT = $(FINDLIB) ocamlopt $(CAMLFLAGS)
CAMLMKLIB = $(FIDLIB) ocamlmklib
BINDIR = /usr/local/bin
LIBDIR = #-destdir /usr/local/lib/ocaml
METADIR = #-metadir ...

ifeq ("$(METADIR)","")
	METADIR=$(LIBDIR)
endif

all: pa_glr glr.cmxa glr.cma glr.cmi doc

%.cmi: %.mli
	$(CAMLC) -c $< 

%.cmo: %.ml %.cmi
	$(CAMLC) -c $< 

%.cmx: %.ml %.cmi
	$(CAMLOPT) -c $< 

glr.cmi: charset.cmi

glr.cmo: umap.cmi charset.cmi

glr.cmx: umap.cmx umap.cmi charset.cmx charset.cmi

glr.cmxa: umap.cmx charset.cmx glr.cmx
	$(CAMLOPT) -a -o glr.cmxa $^

glr.cma: umap.cmo charset.cmo glr.cmo
	$(CAMLC) -a -o glr.cma $^

pa_glr.cmo: glr.cmo pa_glr.ml
	$(CAMLC) -pp "camlp4orf" -I +camlp4 -c pa_glr.ml


pa_glr: pa_glr.cmo
	$(CAMLC) -linkall -I +camlp4 dynlink.cma camlp4lib.cma                 \
        unix.cma                                               \
        Camlp4Parsers/Camlp4OCamlRevisedParser.cmo             \
        camlp4/Camlp4Parsers/Camlp4QuotationCommon.cmo         \
        camlp4/Camlp4Parsers/Camlp4QuotationExpander.cmo       \
        camlp4/Camlp4Parsers/Camlp4OCamlParser.cmo             \
        camlp4/Camlp4Parsers/Camlp4OCamlRevisedParserParser.cmo \
        camlp4/Camlp4Parsers/Camlp4OCamlParserParser.cmo       \
        camlp4/Camlp4Parsers/Camlp4GrammarParser.cmo           \
        $<                                         \
        Camlp4Printers/Camlp4AutoPrinter.cmo                   \
        Camlp4Bin.cmo -o $@

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
	install pa_glr $(BINDIR)

uninstall:
	- ocamlfind remove glr $(LIBDIR) $(METADIR)
	- rm $(BINDIR)/pa_glr
clean:
	- rm -f *.cmi *.cmo *.cmx *.o *.a *.cma *.cmxa pa_glr 
	- cd examples; rm -f *.cmi *.cmo *.cmx *.o *.a *.cma *.cmxa calc calc_strict text text_opt test_merge
	- cd html; rm -f *

doc: glr.mli
	ocamldoc -d html -html glr.mli
