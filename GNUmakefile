.PHONY: all
all:
	@dune build

.PHONY: doc
doc:
	@dune build @doc

.PHONY: clean
clean:
	@dune clean

.PHONY: distclean
distclean: clean
	@find . -name "*~" -exec rm {} \;

.PHONY: install
install: all
	@dune install

.PHONY: uninstall
uninstall: all
	@dune uninstall
