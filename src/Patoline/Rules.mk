# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

PATOLINE_INCLUDES := -I $(d) $(PACK_PATOLINE)
PATOLINE_DEPS_INCLUDES := -I $(d) $(DEPS_PACK_PATOLINE)
$(d)/%.depends: INCLUDES += $(PATOLINE_DEPS_INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx: INCLUDES += $(PATOLINE_INCLUDES)

PAT_CMX := $(d)/Language.cmx $(d)/BuildDir.cmx $(d)/Build.cmx $(d)/Config2.cmx \
  $(d)/Parser.cmx $(d)/Generateur.cmx $(d)/SimpleGenerateur.cmx $(d)/Main.cmx

# $(PAT_CMX): OCAMLOPT := $(OCAMLOPT_NOINTF)
$(PAT_CMX): %.cmx: %.cmo

# Compute ML dependencies
SRC_$(d) := $(filter-out UnicodeScripts.ml.depends, $(addsuffix .depends,$(wildcard $(d)/*.ml)))
$(d)/Parser.ml.depends: $(d)/Parser.ml
$(d)/Generateur.ml.depends: $(d)/Parser.ml.depends
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(SRC_$(d))
endif
endif

$(d)/patoline: $(TYPOGRAPHY_DIR)/Typography.cmxa $(PAT_CMX)
	$(ECHO) "[OPT]    ... -> $@"
	$(Q)$(OCAMLOPT) -o $@ $(PATOLINE_INCLUDES),threads -thread dyp.cmxa \
		dynlink.cmxa patutil.cmxa str.cmxa unix.cmxa rbuffer.cmxa \
		unicodelib.cmxa threads.cmxa $(PAT_CMX)

all: $(PA_PATOLINE)

PATOLINE_DIR := $(d)

SUBSUP_ML := $(d)/Subsup.ml

$(d)/pa_patoline: $(d)/pa_patoline.cmx $(d)/Subsup.cmx $(UTIL_DIR)/patutil.cmxa $(IMAGELIB_DIR)/imagelib.cmxa
	$(ECHO) "[OPT]    ... -> $@"
	$(Q)$(OCAMLOPT) \
		-package patutil,imagelib,dynlink,str,decap \
		-I $(PA_OCAML_DIR) -I $(PATOLINE_DIR) $(COMPILER_INC) -o $@ \
		bigarray.cmxa unicodelib.cmxa rbuffer.cmxa patutil.cmxa unix.cmxa str.cmxa \
		$(COMPILER_LIBO) decap.cmxa decap_ocaml.cmxa Config2.cmx Subsup.cmx $<

$(d)/pa_patoline.ml.depends: $(d)/pa_patoline.ml
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLDEP) -pp $(PA_OCAML) -I $(<D) -I $(PA_OCAML_DIR) -I $(UTIL_DIR) -package patutil,decap $< > $@

$(d)/pa_patoline.cmx: $(d)/pa_patoline.ml $(d)/Subsup.cmx
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT_NOINTF) -pp $(PA_OCAML) -I $(PATOLINE_DIR) -c -package patutil,decap $(COMPILER_INC) -o $@ $<

$(d)/Subsup.ml.depends: $(d)/Subsup.ml
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLDEP) -pp $(PA_OCAML) -I $(<D) -I $(PA_OCAML_DIR) -I $(UTIL_DIR) -package decap $< > $@

$(d)/Subsup.cmx: $(d)/Subsup.ml
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT_NOINTF) -pp $(PA_OCAML) -I $(PATOLINE_DIR) -c -package decap $(COMPILER_INC) -o $@ $<

#$(d)/patolineGL: $(UTIL_DIR)/util.cmxa $(TYPOGRAPHY_DIR)/Typography.cmxa $(DRIVERS_DIR)/DriverGL/DriverGL.cmxa $(d)/PatolineGL.ml
#	$(ECHO) "[OPT]    $(lastword $^) -> $@"
#	$(Q)$(OCAMLOPT) -o $@ $(PACK) -package $(PACK_DRIVER_DriverGL) -I $(DRIVERS_DIR)/DriverGL -I $(DRIVERS_DIR) -I $(UTIL_DIR) $^

$(d)/Main.cmx: $(d)/Main.ml
	$(ECHO) "[OPT]    $<"
	$(Q)$(OCAMLOPT) -thread -rectypes -I +threads $(OFLAGS) $(PATOLINE_INCLUDES) -o $@ -c $<

$(d)/Main.cmo: $(d)/Main.ml
	$(ECHO) "[OPT]    $<"
	$(Q)$(OCAMLC) -thread -rectypes -I +threads $(OFLAGS) $(PATOLINE_INCLUDES) -o $@ -c $<

PATOLINE_UNICODE_SCRIPTS := $(d)/UnicodeScripts

$(EDITORS_DIR)/emacs/SubSuper.el: $(d)/SubSuper.dyp ;
$(d)/SubSuper.dyp: $(d)/UnicodeData.txt $(PATOLINE_UNICODE_SCRIPTS)
	$(ECHO) "[UNIC]   $< -> $@"
	$(Q)$(PATOLINE_UNICODE_SCRIPTS) $< $@ $(EDITORS_DIR)/emacs/SubSuper.el $(SUBSUP_ML)
$(SUBSUP_ML):$(d)/SubSuper.dyp

$(d)/UnicodeScripts.cmx: $(UNICODELIB_CMX) $(UNICODELIB_DEPS) $(UNICODELIB_ML)

$(d)/UnicodeScripts: $(d)/UnicodeScripts.cmx $(UNICODE_DIR)/unicodelib.cmxa
	$(ECHO) "[OCAMLOPT] $< -> $@"
	$(Q)$(OCAMLOPT) -o $@ -package bigarray,unicodelib -linkpkg $<

$(d)/tmp.dyp: $(d)/Parser.dyp $(d)/SubSuper.dyp
	$(ECHO) "[CAT]    $^ -> $@"
	$(Q)cat $^ > $@

$(d)/Parser.ml: $(d)/tmp.dyp
	$(ECHO) "[DYP]    $< -> $@"
	$(Q)$(DYPGEN) --no-mli --merge-warning $< > /dev/null
	$(Q)mv $(basename $<).ml $@

$(d)/Parser.cmx $(d)/Generateur.cmx: OFLAGS += -rectypes
$(d)/Parser.cmo $(d)/Generateur.cmo: OFLAGS += -rectypes

$(d)/Build.cmo $(d)/Build.cmx: OFLAGS += -thread

CLEAN += $(d)/*.o $(d)/*.cm[iox] $(d)/Parser.ml $(d)/SubSuper.dyp $(d)/patoline $(d)/tmp.dyp $(EDITORS_DIR)/emacs/SubSuper.el $(d)/UnicodeScripts $(d)/pa_patoline
DISTCLEAN += $(d)/*.depends

# Installing
install: install-patoline-bin install-patoline-lib install-pa_patoline

.PHONY: install-patoline-bin install-patoline-lib
install-patoline-bin: install-bindir $(d)/patoline
	install -m 755 $(wordlist 2,$(words $^),$^) $(DESTDIR)/$(INSTALL_BIN_DIR)
install-patoline-lib: install-typography $(d)/Build.cmi
	install -m 644 $(wordlist 2,$(words $^),$^) $(DESTDIR)/$(INSTALL_TYPOGRAPHY_DIR)

.PHONY: install-pa_patoline
install-pa_patoline: install-patoline-bin $(d)/pa_patoline
	install -m 755 $(wordlist 2,$(words $^),$^) $(DESTDIR)/$(INSTALL_BIN_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
