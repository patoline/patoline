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

$(GENERATED_FILES): configure.ml
	@printf "\e[33mYou should run \"ocaml unix.cma configure.ml\".\e[39m\n"
	@exit 1

VIMDIR   = $(HOME)/.vim

# Install for the vim mode (in the user's directory).
.PHONY: install_vim
install_vim: $(wildcard editors/vim/*/*.vim)
ifeq ($(wildcard $(VIMDIR)/.),)
	@printf "\e[36mWill not install vim mode.\e[39m\n"
else
	install -d $(VIMDIR)/syntax
	install -d $(VIMDIR)/ftdetect
	install -m 644 editors/vim/syntax/patoline.vim   $(VIMDIR)/syntax
	install -m 644 editors/vim/ftdetect/patoline.vim $(VIMDIR)/ftdetect
	@printf "\e[36mVim mode installed.\e[39m\n"
endif

