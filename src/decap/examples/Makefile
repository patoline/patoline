FINDLIB = ocamlfind
CAMLFLAGS =
CAMLC = $(FINDLIB) ocamlc $(CAMLFLAGS)
CAMLOPT = $(FINDLIB) ocamlopt $(CAMLFLAGS)
CAMLMKLIB = $(FIDLIB) ocamlmklib
BINDIR = /usr/local/bin
LIBDIR = #-destdir /usr/local/lib/ocaml
METADIR = #-metadir ...

ifeq ("$(METADIR)","")
	METADIR=$(LIBDIR)
endif

all: calc text text_opt

calc: calc.cmx ../decap.cmxa
	$(CAMLOPT) str.cmxa unix.cmxa bigarray.cmxa ../decap.cmxa $< -o $@

calc.cmx: calc.ml ../decap.cmx ../pa_glr
	$(CAMLOPT) -I .. -pp ../pa_ocaml -c $<

text: text.cmx ../decap.cmxa
	$(CAMLOPT) str.cmxa unix.cmxa bigarray.cmxa ../decap.cmxa $< -o $@

text.cmx: text.ml ../decap.cmxa ../pa_ocaml
	$(CAMLOPT) -I .. -pp ../pa_ocaml -c $<

text_opt: text_opt.cmx ../decap.cmxa
	$(CAMLOPT) str.cmxa unix.cmxa bigarray.cmxa ../decap.cmxa $< -o $@

text_opt.cmx: text_opt.ml ../decap.cmxa ../pa_ocaml
	$(CAMLOPT) -I .. -pp ../pa_ocaml -c $<


test_merge: test_merge.cmx ../decap.cmxa
	$(CAMLOPT) str.cmxa unix.cmxa bigarray.cmxa ../decap.cmxa $< -o $@

test_merge.cmx: test_merge.ml ../decap.cmxa
	$(CAMLOPT) -c $<

test_list: test_list.cmx ../decap.cmxa
	$(CAMLOPT) str.cmxa unix.cmxa bigarray.cmxa ../decap.cmxa $< -o $@

test_list.cmx: test_list.ml ../decap.cmxa
	$(CAMLOPT) -pp ../pa_ocaml -c $<

check: all
	cd benchmark; tar xvf test.tar.gz; cd ..
	time ./calc < benchmark/test/test3.txt
	time ./calc < benchmark/test/test2.txt
#	time ./calc < benchmark/test/test1.txt
#	time calc < benchmark/test/test5.txt
#	time calc < benchmark/test/test4.txt
	time ./calc_simple < benchmark/test/test3.txt
	time ./calc_simple < benchmark/test/test2.txt
	time ./calc_simple < benchmark/test/test1.txt
	time ./calc_simple < benchmark/test/test5.txt
	time ./calc_simple < benchmark/test/test4.txt
#	time calc_strict < benchmark/test/test3.txt
#	time calc_strict < benchmark/test/test2.txt
#	time calc_strict < benchmark/test/test1.txt
#	time calc_strict < benchmark/test/test5.txt
#	time calc_strict < benchmark/test/test4.txt
	time text < benchmark/test/text0.txt
	time text < benchmark/test/text1.txt
	time text < benchmark/test/text2.txt
	time text < benchmark/test/text3.txt
	time text_opt < benchmark/test/text0.txt
	time text_opt < benchmark/test/text1.txt
	time text_opt < benchmark/test/text2.txt
	time text_opt < benchmark/test/text3.txt
	time test_merge 15
	time test_list 12

clean:
	rm -f *.cmi *.cmo *.cmx *.o *.a *.cma *.cmxa calc calc_strict text text_opt test_merge
