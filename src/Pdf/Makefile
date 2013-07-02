include ../Makefile.config

OCAMLOPT=ocamlfind ocamlopt $(PACKAGE_ZIP) -I ../Typography/_build
OCAMLC=ocamlfind ocamlc $(PACKAGE_zip) -I ../Typography/_build

pdf_parser.cmxa:pdfutil.cmx obj_lexer.cmx obj_parser.cmx pdf_parser.cmx
	$(OCAMLOPT) -a -o pdf_parser.cmxa pdfutil.cmx obj_lexer.cmx obj_parser.cmx pdf_parser.cmx

pdf_parser.p.cmxa:pdfutil.p.cmx obj_lexer.p.cmx obj_parser.p.cmx pdf_parser.p.cmx
	$(OCAMLOPT) -p -a -o pdf_parser.p.cmxa pdfutil.p.cmx obj_lexer.p.cmx obj_parser.p.cmx pdf_parser.p.cmx

pdf_parser.cma:pdfutil.cmo obj_lexer.cmo obj_parser.cmo pdf_parser.cmo
	$(OCAMLC) -a -o pdf_parser.cma pdfutil.cmo obj_lexer.cmo obj_parser.cmo pdf_parser.cmo

pdf_parser.cmx:obj_parser.cmx obj_lexer.cmx
pdf_parser.p.cmx:obj_parser.p.cmx obj_lexer.p.cmx
pdf_parser.cmo:obj_parser.cmo obj_lexer.cmo
obj_lexer.ml:obj_parser.cmo
obj_parser.mly:pdfutil.cmo
obj_parser.cmo:obj_parser.cmi
obj_parser.cmi:obj_parser.mli
obj_parser.mli:obj_parser.ml
obj_parser.cmx:obj_parser.cmi
obj_parser.p.cmx:obj_parser.cmi

%.ml:%.mly
	ocamlyacc $<

%.ml:%.mll
	ocamllex -o $@ $<

%.cmx:%.ml
	$(OCAMLOPT) -c -o $@ $<
%.p.cmx:%.ml
	$(OCAMLOPT) -p -c -o $@ $<
%.cmo:%.ml
	$(OCAMLC) -c -o $@ $<
%.cmi:%.mli
	$(OCAMLC) -c -o $@ $<

clean:
	rm -Rf *~ *.o *.cm[xaoi] *.cmxa *.a
