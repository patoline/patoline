include ../Makefile.config
CAMLOPT=ocamlfind ocamlopt $(PACK) -I ../Typography
CAMLC=ocamlfind ocamlc $(PACK) -I ../Typography

pdf_parser.cmxa:obj_parser.cmx obj_lexer.cmx pdf_parser.cmx pdfutil.cmx ../Typography/Typography.cmxa  ../Typography/Typography.cmi
	$(CAMLOPT) -a -o $@ -I ../Typography  pdfutil.cmx obj_parser.cmx obj_lexer.cmx pdf_parser.cmx

pdf_parser.p.cmxa:obj_parser.p.cmx obj_lexer.p.cmx pdf_parser.p.cmx pdfutil.p.cmx ../Typography/Typography.p.cmxa  ../Typography/Typography.cmi
	$(CAMLOPT) -p -a -o $@ -I ../Typography  pdfutil.p.cmx obj_parser.p.cmx obj_lexer.p.cmx pdf_parser.p.cmx

pdf_parser.cma:obj_parser.cmo obj_lexer.cmo pdf_parser.cmo pdfutil.cmo ../Typography/Typography.cma  ../Typography/Typography.cmi
	$(CAMLC) -a -o $@ -I ../Typography pdfutil.cmo obj_parser.cmo obj_lexer.cmo pdf_parser.cmo

%.cmx:%.ml
	$(CAMLOPT) -c -o $@ $<
%.p.cmx:%.ml
	$(CAMLOPT) -c -p -o $@ $<
%.cmo:%.ml
	$(CAMLC) -c -o $@ $<
%.cmi:%.mli
	ocamlc -c -o $@ $<

pdf_parser.cmx:obj_parser.cmi
obj_lexer.cmx:obj_parser.cmx pdfutil.cmx
obj_parser.cmx:obj_parser.cmi pdfutil.cmx
obj_parser.cmi:pdfutil.cmx

pdf_parser.p.cmx:obj_parser.cmi
obj_lexer.p.cmx:obj_parser.p.cmx pdfutil.p.cmx
obj_parser.p.cmx:obj_parser.cmi pdfutil.p.cmx
obj_parser.p.cmi:pdfutil.p.cmx


%.mli:%.mly
	ocamlyacc $<
%.ml:%.mll
	ocamllex $<
%.ml:%.mly
	ocamlyacc $<

clean:
	rm -f *.cm[ixoa] *.cmxa obj_parser.ml obj_lexer.ml obj_parser.mli obj_lexer.mli
