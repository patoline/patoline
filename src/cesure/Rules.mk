# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

CESURE_OCAMLDEP := ocamlfind ocamldep -I $(d)
CESURE_OCAMLC   := ocamlfind ocamlc -I $(d)
CESURE_OCAMLOPT := ocamlfind ocamlopt -I $(d)

all: $(d)/cesure $(d)/cesure.cmxa $(d)/cesure.cma

$(d)/hyphen.ml.depend: $(d)/hyphen.ml
	$(ECHO) "[DEP] $< -> $@"
	$(Q)$(CESURE_OCAMLDEP) -package unicodelib $<

$(d)/cesure.ml.depend: $(d)/cesure.ml
	$(ECHO) "[DEP] $< -> $@"
	$(Q)$(CESURE_OCAMLDEP) -package unicodelib $<

$(d)/hyphen.cmx: $(d)/hyphen.cmo

$(d)/hyphen.cmo: $(d)/hyphen.ml
	$(ECHO) "[OCAMLC] $< -> $@"
	$(Q)$(CESURE_OCAMLC) -package unicodelib -c $<

$(d)/hyphen.cmx: $(d)/hyphen.ml
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(CESURE_OCAMLOPT) -package unicodelib -c $<

$(d)/cesure.cma: $(d)/hyphen.cmo $(UNICODE_DIR)/unicodelib.cma $(PA_OCAML_DIR)/decap.cmxa
	$(ECHO) "[LINK]   $< -> $@"
	$(Q)$(CESURE_OCAMLC) -package unicodelib -a -o $@ $<

$(d)/cesure.cmxa: $(d)/hyphen.cmx $(UNICODE_DIR)/unicodelib.cmxa
	$(ECHO) "[LINK]   $< -> $@"
	$(Q)$(CESURE_OCAMLOPT) -package unicodelib -a -o $@ $<

$(d)/cesure.cmx: $(d)/cesure.ml $(PA_OCAML_DIR)/decap.cmxa
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(CESURE_OCAMLOPT) -pp $(PA_OCAML) -package decap -c $<

$(d)/cesure: $(PA_OCAML_DIR)/decap.cmxa $(UNICODE_DIR)/unicodelib.cmxa $(d)/hyphen.cmx $(d)/cesure.cmx
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(CESURE_OCAMLOPT) $(INCLUDES) -o $@ -package unicodelib,decap unix.cmxa str.cmxa sqlite3.cmxa $^

CLEAN += $(d)/*.cmx $(d)/*.o $(d)/*.cmi $(d)/*.cmo $(d)/*.a
DISTCLEAN += $(d)/cesure.cma $(d)/cesure.cmxa $(d)/cesure

# Installing
install: install-cesure-bin install-cesure-lib
.PHONY: install-cesure-bin
install-cesure-bin: install-bindir $(d)/cesure
	install -m 755 $(wordlist 2,$(words $^),$^) $(DESTDIR)/$(INSTALL_BIN_DIR)

.PHONY: install-cesure-lib
install-cesure-lib: $(d)/cesure.cma $(d)/cesure.cmxa $(d)/META $(d)/cesure.o $(d)/cesure.a $(d)/hyphen.cmi $(d)/hyphen.cmx $(d)/hyphen.cmo
	install -m 755 -d $(DESTDIR)/$(INSTALL_CESURE_DIR)
	install -m 644 -p $^ $(DESTDIR)/$(INSTALL_CESURE_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
