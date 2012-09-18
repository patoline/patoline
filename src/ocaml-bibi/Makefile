all:bibi.cmxa bibi.p.cmxa bibi.cma

bibi.cmxa:bibi.mli bibi.ml ../Typography/Typography.cmxa ../Typography/Typography.cmi
	ocamlfind ocamlopt -package camomile,sqlite3 -a -o bibi.cmxa -I ../patoline/src/Typography bibi.ml
bibi.p.cmxa:bibi.mli bibi.ml ../Typography/Typography.p.cmxa ../Typography/Typography.cmi
	ocamlfind ocamlopt -package camomile,sqlite3 -p -a -o bibi.cmxa -I ../patoline/src/Typography bibi.ml
bibi.cma:bibi.mli bibi.ml ../Typography/Typography.cma ../Typography/Typography.cmi
	ocamlfind ocamlc -package camomile,sqlite3 -a -o bibi.cma -I ../patoline/src/Typography bibi.ml

install:bibi.cmxa bibi.p.cmxa bibi.cma
	install -m 755 -d $(DESTDIR)$(shell ocamlfind printconf destdir)/bibi
	install -m 644 bibi.cma bibi.cmxa bibi.p.cmxa bibi.a bibi.cmi META $(DESTDIR)$(shell ocamlfind printconf destdir)/bibi

clean:
	rm -f *.cm[aoix] *.cmxa *.a *.o *~
