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

all: gcalc gcalc_strict gcalc_simple gtext gtext_opt gtest_merge gtest_list

calc: calc.cmx ../glr.cmxa
	$(CAMLOPT) str.cmxa unix.cmxa bigarray.cmxa ../glr.cmxa $< -o $@

calc.cmx: calc.ml ../glr.cmxa ../pa_glr
	$(CAMLOPT) -pp ../pa_glr -c $<

calc_simple: gcalc_simple.cmx ../glr.cmxa
	$(CAMLOPT) str.cmxa unix.cmxa bigarray.cmxa ../glr.cmxa $< -o $@

calc_simple.cmx: gcalc_simple.ml ../glr.cmxa ../pa_glr
	$(CAMLOPT) -pp ../pa_glr -c $<

gcalc_strict: calc_strict.cmx ../glr.cmxa
	$(CAMLOPT) str.cmxa unix.cmxa bigarray.cmxa ../glr.cmxa $< -o $@

gcalc_strict.cmx: calc_strict.ml ../glr.cmxa ../pa_glr
	$(CAMLOPT) -pp ../pa_glr -c $<

gtext: gtext.cmx ../glr.cmxa
	$(CAMLOPT) str.cmxa unix.cmxa bigarray.cmxa ../glr.cmxa $< -o $@

gtext.cmx: gtext.ml ../glr.cmxa ../pa_glr
	$(CAMLOPT) -pp ../pa_glr -c $<

gtext_opt: gtext_opt.cmx ../glr.cmxa
	$(CAMLOPT) str.cmxa unix.cmxa bigarray.cmxa ../glr.cmxa $< -o $@

gtext_opt.cmx: gtext_opt.ml ../glr.cmxa ../pa_glr
	$(CAMLOPT) -pp ../pa_glr -c $<

gtest_merge: gtest_merge.cmx ../glr.cmxa
	$(CAMLOPT) str.cmxa unix.cmxa bigarray.cmxa ../glr.cmxa $< -o $@

gtest_merge.cmx: gtest_merge.ml ../glr.cmxa
	$(CAMLOPT) -c $<

gtest_list: gtest_list.cmx ../glr.cmxa
	$(CAMLOPT) str.cmxa unix.cmxa bigarray.cmxa ../glr.cmxa $< -o $@

gtest_list.cmx: gtest_list.ml ../glr.cmxa
	$(CAMLOPT) -pp ../pa_glr -c $<

check: all
	cd benchmark; tar xvf test.tar.ge; cd ..
	time gcalc < benchmark/test/test3.txt
	time gcalc < benchmark/test/test2.txt
	time gcalc < benchmark/test/test1.txt
#	time gcalc < benchmark/test/test5.txt
#	time gcalc < benchmark/test/test4.txt
	time gcalc_simple < benchmark/test/test3.txt
	time gcalc_simple < benchmark/test/test2.txt
	time gcalc_simple < benchmark/test/test1.txt
	time gcalc_simple < benchmark/test/test5.txt
	time gcalc_simple < benchmark/test/test4.txt
	time gcalc_strict < benchmark/test/test3.txt
	time gcalc_strict < benchmark/test/test2.txt
	time gcalc_strict < benchmark/test/test1.txt
	time gcalc_strict < benchmark/test/test5.txt
	time gcalc_strict < benchmark/test/test4.txt
	time gtext < benchmark/test/text0.txt
	time gtext < benchmark/test/text1.txt
	time gtext < benchmark/test/text2.txt
	time gtext < benchmark/test/text3.txt
	time gtext_opt < benchmark/test/text0.txt
	time gtext_opt < benchmark/test/text1.txt
	time gtext_opt < benchmark/test/text2.txt
	time gtext_opt < benchmark/test/text3.txt
	time gtest_merge 15
	time gtest_list 12

clean:
	rm -f *.cmi *.cmo *.cmx *.o *.a *.cma *.cmxa calc calc_strict text text_opt test_merge
