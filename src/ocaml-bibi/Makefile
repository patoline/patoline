all:bibi.cmxa bibi.p.cmxa bibi.cma

bibi.cmxa:bibi.cmi bibi.ml ../Typography/Typography.cmxa ../Typography/Typography.cmi
	ocamlfind ocamlopt -package camomile,sqlite3 -a -o bibi.cmxa -I ../Typography bibi.ml
bibi.p.cmxa:bibi.cmi bibi.ml ../Typography/Typography.p.cmxa ../Typography/Typography.cmi
	ocamlfind ocamlopt -package camomile,sqlite3 -p -a -o bibi.p.cmxa -I ../Typography bibi.ml
bibi.cma:bibi.cmi bibi.ml ../Typography/Typography.cma ../Typography/Typography.cmi
	ocamlfind ocamlc -package camomile,sqlite3 -a -o bibi.cma -I ../Typography bibi.ml

bibi.cmi:bibi.mli ../Typography/Typography.cmi
	ocamlfind ocamlc -package camomile,sqlite3 -c -o bibi.cmi -I ../Typography bibi.mli

install:bibi.cmxa bibi.p.cmxa bibi.cma
	install -m 755 -d $(DESTDIR)$(shell ocamlfind printconf destdir)/bibi
	install -m 644 bibi.cma bibi.cmxa bibi.p.cmxa bibi.a bibi.cmi META $(DESTDIR)$(shell ocamlfind printconf destdir)/bibi

clean:
	rm -f *.cm[aoix] *.cmxa *.a *.o *~
