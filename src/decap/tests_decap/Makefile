DECAP_DIR = ..
PA_OCAML = $(DECAP_DIR)/pa_ocaml
TESTS = test_combinators calc_prio_left calc_prio_left2 calc_prio_left3 calc_prio_left4

all: $(TESTS)

clean:
	rm $(TESTS) *.cm* *.o *.a

test_kind.cmx: test_kind.ml $(DECAP_DIR)/decap.cmxa $(DECAP_DIR)/decap.cmi
	ocamlopt -c $<

generate_calc.cmx: generate_calc.ml test_kind.cmx $(DECAP_DIR)/decap.cmxa $(DECAP_DIR)/decap.cmi
	ocamlopt -pp $(PA_OCAML) -I .. -c $<

test_combinators: test_combinators.ml test_kind.cmx $(PA_OCAML) $(DECAP_DIR)/decap.cmxa $(DECAP_DIR)/decap.cmi
	ocamlopt -I $(DECAP_DIR) unix.cmxa str.cmxa decap.cmxa test_kind.cmx $< -o $@

calc_prio_left: calc_prio_left.ml test_kind.cmx generate_calc.cmx $(PA_OCAML) $(DECAP_DIR)/decap.cmxa $(DECAP_DIR)/decap.cmi
	ocamlopt -pp $(PA_OCAML) -o $@ -I $(DECAP_DIR) unix.cmxa str.cmxa decap.cmxa test_kind.cmx generate_calc.cmx $<

calc_prio_left2: calc_prio_left2.ml test_kind.cmx generate_calc.cmx $(PA_OCAML) $(DECAP_DIR)/decap.cmxa $(DECAP_DIR)/decap.cmi
	ocamlopt -pp $(PA_OCAML) -o $@ -I $(DECAP_DIR) unix.cmxa str.cmxa decap.cmxa test_kind.cmx generate_calc.cmx $<

calc_prio_left3: calc_prio_left3.ml test_kind.cmx generate_calc.cmx $(PA_OCAML) $(DECAP_DIR)/decap.cmxa $(DECAP_DIR)/decap.cmi
	ocamlopt -pp $(PA_OCAML) -o $@ -I $(DECAP_DIR) unix.cmxa str.cmxa decap.cmxa test_kind.cmx generate_calc.cmx $<

calc_prio_left4: calc_prio_left4.ml test_kind.cmx generate_calc.cmx $(PA_OCAML) $(DECAP_DIR)/decap.cmxa $(DECAP_DIR)/decap.cmi
	ocamlopt -pp $(PA_OCAML) -o $@ -I $(DECAP_DIR) unix.cmxa str.cmxa decap.cmxa test_kind.cmx generate_calc.cmx $<

quick_tests: $(TESTS)
	for f in $^; do echo $$f; time ./$$f --quick > /dev/null; done

tests: $(TESTS)
	for f in $^; do echo $$f; time ./$$f --normal > /dev/null; done

full_tests: $(TESTS)
	for f in $^; do echo $$f; time ./$$f --full > /dev/null; done
