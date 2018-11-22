GENERATED_FILES = unicodelib/config.ml patconfig/patDefault.ml

.PHONY: all
all: $(GENERATED_FILES)
	@dune build

.PHONY: doc
doc: $(GENERATED_FILES)
	@dune build @doc

.PHONY: clean
clean:
	@dune clean
	@rm -f $(GENERATED_FILES)

.PHONY: distclean
distclean: clean
	@find . -name "*~" -exec rm {} \;

.PHONY: install
install: all
	@dune install

.PHONY: uninstall
uninstall: all
	@dune uninstall

$(GENERATED_FILES): configure
	@echo 'You should run "ocaml unix.cma configure.ml".'
	@exit 1
