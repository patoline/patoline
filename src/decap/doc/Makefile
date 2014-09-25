DECAP_DIR = ..
PA_OCAML = $(DECAP_DIR)/pa_ocaml

all: calc_base calc_prio aabb aabbcc text

calc_base: calc_base.ml $(PA_OCAML) $(DECAP_DIR)/decap.cma
	ocamlc -pp $(PA_OCAML) -o $@ -I $(DECAP_DIR) unix.cma str.cma decap.cma $<

calc_prio: calc_prio.ml $(PA_OCAML) $(DECAP_DIR)/decap.cma
	ocamlc -pp $(PA_OCAML) -o $@ -I $(DECAP_DIR) unix.cma str.cma decap.cma $<

aabb: aabb.ml $(PA_OCAML) $(DECAP_DIR)/decap.cma
	ocamlc -pp $(PA_OCAML) -o $@ -I $(DECAP_DIR) unix.cma str.cma decap.cma $<

aabbcc: aabbcc.ml $(PA_OCAML) $(DECAP_DIR)/decap.cma
	ocamlc -pp $(PA_OCAML) -o $@ -I $(DECAP_DIR) unix.cma str.cma decap.cma $<

text: text.ml $(PA_OCAML) $(DECAP_DIR)/decap.cma
	ocamlc -pp $(PA_OCAML) -o $@ -I $(DECAP_DIR) unix.cma str.cma decap.cma $<

clean:
	rm -f *.cmi *.cmo

distclean: clean
	rm -f calc_base calc_prio aabb aabbcc text
