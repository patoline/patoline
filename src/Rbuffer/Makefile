INST=$(DESTDIR)$(shell ocamlfind printconf destdir)/rbuffer


all:rbuffer.cmxa rbuffer.p.cmxa rbuffer.cma rope.cmxa rope.p.cmxa rope.cma

rbuffer.cmxa:rbuffer.ml rbuffer.cmi rope.cmi
	ocamlopt -a -o $@ $<
rbuffer.p.cmxa:rbuffer.ml rbuffer.cmi rope.cmi
	ocamlopt -p -a -o $@ $<

rbuffer.cma:rbuffer.ml rbuffer.cmi rope.cmi
	ocamlc -a -o $@ $<

rope.cmxa:rope.ml rope.cmi
	ocamlopt -a -o $@ $<
rope.p.cmxa:rope.ml rope.cmi
	ocamlopt -p -a -o $@ $<

rope.cma:rope.ml rope.cmi
	ocamlc -a -o $@ $<

%.cmi:%.mli
	ocamlc -o $@ $<

install:rbuffer.cmxa rbuffer.p.cmxa rbuffer.cma rope.cmxa rope.p.cmxa rope.cma
	install -d $(INST)
	install rbuffer.cmxa rbuffer.p.cmxa rbuffer.a rbuffer.cmi rbuffer.cma $(INST)
	install rope.cmxa rope.p.cmxa rope.a rope.cmi rope.cma $(INST)
	install META $(INST)

clean:
	rm -f *~ *.cm[oixa] *.a *.cmxa *.o
