
all: patoline-input.el

SubstKey:
	ocamlfind ocamlopt -package str,camomile SubstKey.ml -linkpkg -o SubstKey

SubSuper2.el: SubSuper.el ../src/quail.el SubstKey
	./SubstKey ../src/quail.el SubSuper.el > SubSuper2.el

patoline-input.el:  patoline-input.pre ../src/quail.el SubSuper2.el patoline-input.post
	cat patoline-input.pre ../src/quail.el SubSuper2.el patoline-input.post > patoline-input.el

clean:
	rm -f SubSuper2.el SubstKey patoline-input.el
