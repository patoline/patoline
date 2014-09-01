# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

all: $(d)/pa_ocaml $(d)/glr.cmxa $(d)/glr.cma

$(d)/pa_ocaml $(d)/glr.cmxa $(d)/glr.cma: export OCAMLOPT := $(OCAMLOPT_NOINTF)

# make pa_ocaml twice to make sure we have .cmo in src/glr.
$(d)/pa_ocaml: $(d)/pa_*.ml $(d)/glr.cmxa
	$(MAKE) -e -C $(GLR_DIR) pa_ocaml
	$(MAKE) -e -C $(GLR_DIR) pa_ocaml

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

install-glr:
	$(MAKE) -C $(GLR_DIR) install

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
