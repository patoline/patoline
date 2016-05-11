all: patoline

patoline: pragma.cmx parallel.cmx build.cmx patoline.ml
	ocamlfind ocamlopt -package decap,threads -thread -I ../config \
		-pp pa_ocaml -o patoline unix.cmxa configRC.cmx patDefault.cmx \
		patConfig.cmx str.cmxa decap.cmxa threads.cmxa $^

parallel.cmx: parallel.ml
	ocamlfind ocamlopt -package threads -thread -c $<

pragma.cmx: pragma.ml
	ocamlfind ocamlopt -c $<

build.cmx: build.ml parallel.cmx pragma.cmx
	ocamlfind ocamlopt -package decap,threads -I ../config -pp pa_ocaml \
		-thread -c $<

clean:
	rm -rf *.cm[ixo] *.o *~ patoline

install: patoline
	cp patoline /usr/local/bin/patoline2
