# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

all: $(d)/pa_ocaml $(d)/glr.cmxa $(d)/glr.cma

$(d)/pa_ocaml $(d)/glr.cmxa $(d)/glr.cma: export OCAMLOPT := $(OCAMLOPT_NOINTF)

GLR_SRC=$(d)/umap.ml $(d)/charset.ml $(d)/input.ml $(d)/glr.ml
PA_OCAML_SRC=$(d)/pa_ocaml_prelude.ml $(d)/pa_parser.ml $(d)/pa_ocaml.ml $(d)/pa_compose.ml $(d)/pa_opt_main.ml
$(GLR_SRC:.ml=.cmo): $(d)/glr.cma;
$(GLR_SRC:.ml=.cmi): $(d)/glr.cmxa;
$(GLR_SRC:.ml=.cmx): $(d)/glr.cmxa;

$(d)/pa_ocaml: $(d)/glr.cmxa
$(PA_OCAML_SRC:.ml=.cmi): $(d)/pa_ocaml;
$(PA_OCAML_SRC:.ml=.cmx): $(d)/pa_ocaml;
$(PA_OCAML_SRC:.ml=.cmo): $(d)/pa_ocaml.byt;

# possibly make pa_ocaml twice to make sure we have .cmx in src/glr.
$(d)/pa_ocaml: $(d)/pa_*.ml $(d)/glr.cmxa
	if [ ! -x $(GLR_DIR)/pa_ocaml ]; then \
	  $(MAKE) -e -C $(GLR_DIR) pa_ocaml; \
	  $(MAKE) -e -C $(GLR_DIR) pa_ocaml; \
	else \
	  $(MAKE) -e -C $(GLR_DIR) pa_ocaml; \
	fi

$(d)/glr.cmxa: $(d)/glr.ml $(d)/input.ml $(d)/input.mli $(d)/charset.ml $(d)/charset.mli $(d)/umap.ml $(d)/umap.mli
	$(MAKE) -e -C $(GLR_DIR) glr.cmxa
$(d)/glr.cma: $(d)/glr.ml $(d)/input.ml $(d)/input.mli $(d)/charset.ml $(d)/charset.mli $(d)/umap.ml $(d)/umap.mli
	$(MAKE) -e -C $(GLR_DIR) glr.cma

clean: clean-glr

clean-glr:
	$(MAKE) -e -C $(GLR_DIR) clean

distclean: distclean-glr

distclean-glr:
	$(MAKE) -e -C $(GLR_DIR) distclean

$(d)/glr.a: $(d)/glr.cmxa;

install: install-glr

install-glr: export LIBDIR := $(INSTALL_GLR_DIR)
install-glr: export BINDIR := $(INSTALL_BIN_DIR)

install-glr: install-bindir
	$(MAKE) -e -C $(GLR_DIR) install

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
