OCAMLC = ocamlc -pp ../../pa_ocaml

SOURCEDIR = $(PWD)
SOURCES = $(wildcard $(SOURCEDIR)/*.ml)
BASE_NAMES = $(SOURCES:.ml=)

all: $(SOURCES)
	@for NAME in $(BASE_NAMES) ; do \
		$(OCAMLC) -o $$NAME $$NAME.ml ; \
		$$NAME > $$NAME.out ; \
		diff $$NAME.reference $$NAME.out > $$NAME.diff ; \
		echo `wc -l $$NAME.diff` ; \
	done

clean:
	rm -rf *.cmo *.cmi *.out $(BASE_NAMES) *.diff
