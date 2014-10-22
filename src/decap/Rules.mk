# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

all: $(d)/pa_ocaml $(d)/decap.cmxa $(d)/decap.cma

$(d)/pa_ocaml $(d)/decap.cmxa $(d)/decap.cma: export OCAMLOPT := $(OCAMLOPT_NOINTF)

PA_OCAML_SRC=$(d)/charset.ml $(d)/input.ml $(d)/decap.ml
PA_OCAML_SRC=$(d)/pa_ocaml_prelude.ml $(d)/pa_parser.ml $(d)/pa_ocaml.ml $(d)/pa_compose.ml $(d)/pa_opt_main.ml
$(PA_OCAML_SRC:.ml=.cmo): $(d)/decap.cma $(d)/pa_ocaml.byt;
$(PA_OCAML_SRC:.ml=.cmi): $(d)/decap.cmxa $(d)/pa_ocaml;
$(PA_OCAML_SRC:.ml=.cmx): $(d)/decap.cmxa $(d)/pa_ocaml;
$(d)/pa_ocaml: $(d)/decap.cmxa

# possibly make pa_ocaml twice to make sure we have .cmx in src/decap.
$(d)/pa_ocaml: $(d)/pa_*.ml $(d)/decap.cmxa
	if [ ! -x $(PA_OCAML_DIR)/pa_ocaml ]; then \
	  $(MAKE) -e -C $(PA_OCAML_DIR) pa_ocaml; \
	  $(MAKE) -e -C $(PA_OCAML_DIR) pa_ocaml; \
	else \
	  $(MAKE) -e -C $(PA_OCAML_DIR) pa_ocaml; \
	fi

$(d)/decap.cmxa: $(d)/decap.ml $(d)/input.ml $(d)/input.mli $(d)/charset.ml $(d)/charset.mli $(d)/pa_ocaml_prelude.ml
	if [ ! -x $(PA_OCAML_DIR)/pa_ocaml ]; then \
	  $(MAKE) -e -C $(PA_OCAML_DIR) pa_ocaml; \
	  $(MAKE) -e -C $(PA_OCAML_DIR) decap.cmxa decap_ocaml.cmxa; \
	else \
	  $(MAKE) -e -C $(PA_OCAML_DIR) decap.cmxa decap_ocaml.cmxa; \
	fi

$(d)/decap.cma: $(d)/decap.ml $(d)/input.ml $(d)/input.mli $(d)/charset.ml $(d)/charset.mli $(d)/pa_ocaml_prelude.ml
	if [ ! -x $(PA_OCAML_DIR)/pa_ocaml ]; then \
	  $(MAKE) -e -C $(PA_OCAML_DIR) pa_ocaml; \
	  $(MAKE) -e -C $(PA_OCAML_DIR) decap.cma decap_ocaml.cma; \
	else \
	  $(MAKE) -e -C $(PA_OCAML_DIR) decap.cma decap_ocaml.cma; \
	fi

clean: clean-decap

clean-decap:
	$(MAKE) -e -C $(PA_OCAML_DIR) clean

distclean: distclean-decap

distclean-decap:
	$(MAKE) -e -C $(PA_OCAML_DIR) distclean

$(d)/decap.a: $(d)/decap.cmxa;

install: install-decap

install-decap: export LIBDIR := $(INSTALL_PA_OCAML_DIR)
install-decap: export BINDIR := $(INSTALL_BIN_DIR)
install-decap: export DESTDIR := $(DESTDIR)

install-decap: install-bindir
	$(MAKE) -e -C $(PA_OCAML_DIR) install

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
