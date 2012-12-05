OCAMLOPT=ocamlopt -I ../Typography/_build
OCAMLC=ocamlc -I ../Typography/_build

pdf_parser.cmxa:obj_lexer.cmx obj_parser.cmx pdf_parser.cmx
	$(OCAMLOPT) -a -o pdf_parser.cmxa obj_lexer.cmx obj_parser.cmx pdf_parser.cmx

pdf_parser.p.cmxa:obj_lexer.p.cmx obj_parser.p.cmx pdf_parser.p.cmx
	$(OCAMLOPT) -p -a -o pdf_parser.p.cmxa obj_lexer.p.cmx obj_parser.p.cmx pdf_parser.p.cmx

pdf_parser.cma:obj_lexer.cmo obj_parser.cmo pdf_parser.cmo
	$(OCAMLC) -a -o pdf_parser.cma obj_lexer.cmo obj_parser.cmo pdf_parser.cmo

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
	rm -Rf *~ *.o *.cmx[aoi] *.cmxa *.a
