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

# Check that Opam is available.
OPAM := $(shell which opam 2> /dev/null)
ifndef OPAM

# FIXME
$(error "The opam package manager is required...")

else

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

DRIVERS_WITH_DEP = $(shell grep optional drivers/*/dune | \
	sed 's/drivers\/\(\w*\)\/dune: *(optional) *; *requires* \(\w*\)/"\1";/g')
DRIVERS_DEP      = $(shell grep optional drivers/*/dune | \
	sed 's/drivers\/\(\w*\)\/dune: *(optional) *; *requires* \(\w*\)/\2/g')
DRIVERS_DEP_OK   = $(foreach d, $(DRIVERS_DEP),\
	$(shell (ocamlfind query -qo -qe $(d) && echo true) || echo false);)

unicodelib/config.ml: GNUmakefile
	@echo 'let unicode_data_file = "$(UNICODE_DATA)"' > $@

patconfig/patDefault.ml: GNUmakefile
	@echo 'let fonts_dir          = "$(FONTS_DIR)"'                     > $@
	@echo 'let grammars_dir       = "$(GRAMMARS_DIR)"'                 >> $@
	@echo 'let hyphen_dir         = "$(HYPHEN_DIR)"'                   >> $@
	@echo 'let extra_fonts_dir    = []'                                >> $@
	@echo 'let extra_grammars_dir = []'                                >> $@
	@echo 'let extra_hyphen_dir   = []'                                >> $@
	@echo 'let formats            = [$(AVAIL_FORMATS)]'                >> $@
	@echo 'let drivers            ='                                   >> $@
	@echo '  let all_drivers = [$(AVAIL_DRIVERS)] in'                  >> $@
	@echo '  (* Dependencies: $(DRIVER_DEPS) *)'                       >> $@
	@echo '  let with_dep = [$(DRIVERS_WITH_DEP)] in'                  >> $@
	@echo '  let dep_ok   = [$(DRIVERS_DEP_OK)] in'                    >> $@
	@echo '  let fn acc d ok = if ok then acc else d::acc in'          >> $@
	@echo '  let blacklist = List.fold_left2 fn [] with_dep dep_ok in' >> $@
	@echo '  let driver_ok d = not (List.mem d blacklist) in'          >> $@
	@echo '  List.filter driver_ok all_drivers'                        >> $@

endif
