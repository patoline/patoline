# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

all: $(d)/pa_ocaml $(d)/glr.cmxa $(d)/glr.cma

PA_OCAML = $(GLR_DIR)/pa_ocaml

$(d)/pa_ocaml:
	cd $(GLR_DIR); make pa_ocaml
$(d)/glr.cmxa:
	cd $(GLR_DIR); make glr.cmxa
$(d)/glr.cma:
	cd $(GLR_DIR); make glr.cma

clean: clean-glr

clean-glr:
	cd $(GLR_DIR); make clean

$(d)/glr.a: $(d)/glr.cmxa;

install: install-glr

install-glr:
	cd $(GLR_DIR); make install

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
