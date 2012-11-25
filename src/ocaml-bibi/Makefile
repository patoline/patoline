all:bibi.cmxa bibi.p.cmxa bibi.cma

bibi.cmxa:bibi.cmi bibi.ml
	ocamlfind ocamlopt -package camomile,sqlite3 -a -o bibi.cmxa -I ../Typography/_build bibi.ml -I ../Format/_build
bibi.p.cmxa:bibi.cmi bibi.ml
	ocamlfind ocamlopt -package camomile,sqlite3 -p -a -o bibi.p.cmxa -I ../Typography/_build bibi.ml -I ../Format/_build
bibi.cma:bibi.cmi bibi.ml
	ocamlfind ocamlc -package camomile,sqlite3 -a -o bibi.cma -I ../Typography/_build bibi.ml -I ../Format/_build

bibi.cmi:bibi.mli
	ocamlfind ocamlc -package camomile,sqlite3 -c -o bibi.cmi -I ../Typography/_build bibi.mli -I ../Format/_build

install:bibi.cmxa bibi.p.cmxa bibi.cma
	install -m 755 -d $(DESTDIR)$(shell ocamlfind printconf destdir)/bibi
	install -m 644 bibi.cma bibi.cmxa bibi.p.cmxa bibi.a bibi.cmi META $(DESTDIR)$(shell ocamlfind printconf destdir)/bibi

clean:
	rm -f *.cm[aoix] *.cmxa *.a *.o *~
