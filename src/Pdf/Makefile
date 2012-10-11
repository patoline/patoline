OCAMLFIND=ocamlfind ocamlopt -package Typography,Typography.Pdf
OCAMLC=ocamlfind ocamlc -package Typography,Typography.Pdf

pdf_parser.cmxa:obj_parser.cmx obj_lexer.cmx pdf_parser.cmx pdfutil.cmx
	$(OCAMLFIND) -a -o $@ pdfutil.cmx obj_parser.cmx obj_lexer.cmx pdf_parser.cmx

pdf_parser.p.cmxa:obj_parser.p.cmx obj_lexer.p.cmx pdf_parser.p.cmx pdfutil.p.cmx
	$(OCAMLFIND) -p -a -o $@ pdfutil.p.cmx obj_parser.p.cmx obj_lexer.p.cmx pdf_parser.p.cmx

pdf_parser.cma:obj_parser.cmo obj_lexer.cmo pdf_parser.cmo pdfutil.cmo
	$(OCAMLC) -a -o $@ pdfutil.cmo obj_parser.cmo obj_lexer.cmo pdf_parser.cmo

%.cmx:%.ml
	$(OCAMLFIND) -c -o $@ $<
%.cmo:%.ml
	$(OCAMLC) -c -o $@ $<
%.cmi:%.mli
	ocamlc -c -o $@ $<

pdf_parser.cmx:obj_parser.cmi
obj_lexer.cmx:obj_parser.cmx pdfutil.cmx
obj_parser.cmx:obj_parser.cmi pdfutil.cmx
obj_parser.cmi:pdfutil.cmx

%.mli:%.mly
	ocamlyacc $<
%.ml:%.mll
	ocamllex $<

%.ml:%.mly
	ocamlyacc $<

clean:
	rm -f *.cm[ixoa] *.cmxa obj_parser.ml obj_lexer.ml obj_parser.mli obj_lexer.mli
