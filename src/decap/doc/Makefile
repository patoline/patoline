DECAP_DIR = ..
PA_OCAML = $(DECAP_DIR)/pa_ocaml

all: calc_base calc_prio aabb aabbcc text pa_do_try

calc_base: calc_base.ml $(PA_OCAML) $(DECAP_DIR)/decap.cma
	ocamlc -pp $(PA_OCAML) -o $@ -I $(DECAP_DIR) unix.cma str.cma decap.cma $<

calc_prio: calc_prio.ml $(PA_OCAML) $(DECAP_DIR)/decap.cma
	ocamlc -pp $(PA_OCAML) -o $@ -I $(DECAP_DIR) unix.cma str.cma decap.cma $<

calc_prio_left: calc_prio_left.ml $(PA_OCAML) $(DECAP_DIR)/decap.cmxa
	ocamlopt -pp $(PA_OCAML) -o $@ -I $(DECAP_DIR) unix.cmxa str.cmxa decap.cmxa $<

calc_prio_left2: calc_prio_left2.ml $(PA_OCAML) $(DECAP_DIR)/decap.cma
	ocamlc -pp $(PA_OCAML) -o $@ -I $(DECAP_DIR) unix.cma str.cma decap.cma $<

calc_prio2: calc_prio2.ml $(PA_OCAML) $(DECAP_DIR)/decap.cma
	ocamlc -pp $(PA_OCAML) -o $@ -I $(DECAP_DIR) unix.cma str.cma decap.cma $<

calc_prio_left3: calc_prio_left3.ml $(PA_OCAML) $(DECAP_DIR)/decap.cmxa
	ocamlopt -pp $(PA_OCAML) -o $@ -I $(DECAP_DIR) unix.cmxa str.cmxa decap.cmxa $<

test_left: test_left.ml $(PA_OCAML) $(DECAP_DIR)/decap.cma
	ocamlc -pp $(PA_OCAML) -o $@ -I $(DECAP_DIR) unix.cma str.cma decap.cma $<

test_left0: test_left0.ml $(PA_OCAML) $(DECAP_DIR)/decap.cma
	ocamlc -pp $(PA_OCAML) -o $@ -I $(DECAP_DIR) unix.cma str.cma decap.cma $<

test_left2: test_left2.ml $(PA_OCAML) $(DECAP_DIR)/decap.cmxa
	ocamlopt -pp $(PA_OCAML) -g -o $@ -I $(DECAP_DIR) unix.cmxa str.cmxa decap.cmxa $<

test_left3: test_left3.ml $(PA_OCAML) $(DECAP_DIR)/decap.cmxa
	ocamlopt -pp $(PA_OCAML) -g -o $@ -I $(DECAP_DIR) unix.cmxa str.cmxa decap.cmxa $<

aabb: aabb.ml $(PA_OCAML) $(DECAP_DIR)/decap.cma
	ocamlc -pp $(PA_OCAML) -o $@ -I $(DECAP_DIR) unix.cma str.cma decap.cma $<

aabbcc: aabbcc.ml $(PA_OCAML) $(DECAP_DIR)/decap.cma
	ocamlc -pp $(PA_OCAML) -o $@ -I $(DECAP_DIR) unix.cma str.cma decap.cma $<

text: text.ml $(PA_OCAML) $(DECAP_DIR)/decap.cma
	ocamlc -pp $(PA_OCAML) -o $@ -I $(DECAP_DIR) unix.cma str.cma decap.cma $<

pa_do_try: pa_do_try.ml $(PA_OCAML) $(DECAP_DIR)/decap.cmxa $(DECAP_DIR)/decap_ocaml.cmxa
	ocamlopt -pp $(PA_OCAML) -o $@ -I $(DECAP_DIR) -I +compiler-libs \
	unix.cmxa str.cmxa ocamlcommon.cmxa decap.cmxa ../pa_ocaml_prelude.cmx \
  ../pa_ocaml.cmx ../pa_main.cmx pa_do_try.ml

clean:
	rm -f *.cmi *.cmo

distclean: clean
	rm -f calc_base calc_prio aabb aabbcc text pa_do_try
