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

OPAM_SHARE    = $(shell opam var share)
UNICODE_DATA  = $(OPAM_SHARE)/patoline/unicode/unicode.data
FONTS_DIR     = $(OPAM_SHARE)/patoline/fonts
GRAMMARS_DIR  = $(OPAM_SHARE)/patoline/grammars
HYPHEN_DIR    = $(OPAM_SHARE)/patoline/hyphen
FORMAT_FILES  = $(wildcard formats/*.ml)
DRIVER_DIRS   = $(wildcard drivers/*)
AVAIL_DRIVERS = $(foreach d, $(DRIVER_DIRS),"$(basename $(notdir $(d)))";)
AVAIL_FORMATS = $(foreach f, $(FORMAT_FILES),"$(basename $(notdir $(f)))";)
DRIVER_DEPS   = $(shell grep optional drivers/*/dune | \
	sed 's/drivers\/\(\w*\)\/dune: *(optional) *; *requires* \(\w*\)/\1:\2/g')

unicodelib/config.ml: GNUmakefile
	@echo 'let unicode_data_file = "$(UNICODE_DATA)"'    > $@

patconfig/patDefault.ml: GNUmakefile
	@echo 'let fonts_dir          = "$(FONTS_DIR)"'      > $@
	@echo 'let grammars_dir       = "$(GRAMMARS_DIR)"'  >> $@
	@echo 'let hyphen_dir         = "$(HYPHEN_DIR)"'    >> $@
	@echo 'let extra_fonts_dir    = []'                 >> $@
	@echo 'let extra_grammars_dir = []'                 >> $@
	@echo 'let extra_hyphen_dir   = []'                 >> $@
	@echo 'let drivers            = [$(AVAIL_DRIVERS)]' >> $@
	@echo 'let formats            = [$(AVAIL_FORMATS)]' >> $@
	@echo '(* Dependencies: $(DRIVER_DEPS) *)'          >> $@
