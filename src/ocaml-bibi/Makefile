all:bibi.cmxa bibi.p.cmxa bibi.cma

bibi.cmxa:bibi.cmi bibi.ml ../Typography/_build/Typography.cmi ../Format/DefaultFormat.cmi
	ocamlfind ocamlopt -package camomile,sqlite3 -a -o bibi.cmxa -I ../Typography/_build bibi.ml -I ../Format
bibi.p.cmxa:bibi.cmi bibi.ml ../Typography/_build/Typography.cmi ../Format/DefaultFormat.cmi
	ocamlfind ocamlopt -package camomile,sqlite3 -p -a -o bibi.p.cmxa -I ../Typography/_build bibi.ml -I ../Format
bibi.cma:bibi.cmi bibi.ml ../Typography/_build/Typography.cmi ../Format/DefaultFormat.cmi
	ocamlfind ocamlc -package camomile,sqlite3 -a -o bibi.cma -I ../Typography/_build bibi.ml -I ../Format

bibi.cmi:bibi.mli ../Typography/_build/Typography.cmi ../Format/DefaultFormat.cmi
	ocamlfind ocamlc -package camomile,sqlite3 -c -o bibi.cmi -I ../Typography/_build bibi.mli -I ../Format

install:bibi.cmxa bibi.p.cmxa bibi.cma
	install -m 755 -d $(DESTDIR)$(shell ocamlfind printconf destdir)/bibi
	install -m 644 bibi.cma bibi.cmxa bibi.p.cmxa bibi.a bibi.cmi META $(DESTDIR)$(shell ocamlfind printconf destdir)/bibi

clean:
	rm -f *.cm[aoix] *.cmxa *.a *.o *~
