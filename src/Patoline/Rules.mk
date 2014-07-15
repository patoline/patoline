# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

PATOLINE_INCLUDES := -I $(d) $(PACK_PATOLINE)
PATOLINE_DEPS_INCLUDES := -I $(d) $(DEPS_PACK_PATOLINE)
$(d)/%.depends: INCLUDES += $(PATOLINE_DEPS_INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx: INCLUDES += $(PATOLINE_INCLUDES)

PAT_CMX := $(d)/Language.cmx $(d)/Build.cmx $(d)/Config2.cmx \
  $(d)/Parser.cmx $(d)/Generateur.cmx $(d)/SimpleGenerateur.cmx $(d)/Main.cmx

# Compute ML dependencies
DEPENDS_$(d) := $(addsuffix .depends,$(wildcard $(d)/*.ml))
$(filter-out $(d)/Parser.ml.depends,$(filter-out $(d)/pa_patoline.ml.depends,$(DEPENDS_$(d)))): $(d)/Parser.ml.depends
-include $(DEPENDS_$(d))

$(d)/patoline: $(RBUFFER_DIR)/rbuffer.cmxa $(UTIL_DIR)/patutil.cmxa $(IMAGELIB_DIR)/imagelib.cmxa $(PAT_CMX)
	$(ECHO) "[OPT]    ... -> $@"
	$(Q)$(OCAMLOPT) -o $@ -linkpkg $(PATOLINE_INCLUDES),threads -thread $(PAT_CMX)

all: $(PA_PATOLINE)

$(d)/pa_patoline: $(d)/pa_patoline.cmx $(UTIL_DIR)/patutil.cmxa $(IMAGELIB_DIR)/imagelib.cmxa
	$(ECHO) "[OPT]    ... -> $@"
	$(Q)$(OCAMLOPT) -linkpkg -package camlp4,patutil,imagelib,dynlink,glr str.cmxa glr.cmxa camlp4lib.cmxa Camlp4Parsers/Camlp4OCamlRevisedParser.cmx Camlp4Parsers/Camlp4OCamlParser.cmx -o $@ $<

$(d)/pa_patoline.cmx: $(d)/pa_patoline.ml
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT_NOINTF) -pp pa_glr -c -package camlp4,patutil,glr -I +camlp4/Camlp4Parsers -o $@ $< 

$(d)/pa_patoline.ml.depends: $(d)/pa_patoline.ml
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLDEP) -pp pa_glr -package glr,camlp4,patutil $< > $@

#$(d)/patolineGL: $(UTIL_DIR)/util.cmxa $(TYPOGRAPHY_DIR)/Typography.cmxa $(DRIVERS_DIR)/DriverGL/DriverGL.cmxa $(d)/PatolineGL.ml
#	$(ECHO) "[OPT]    $(lastword $^) -> $@"
#	$(Q)$(OCAMLOPT) -o $@ $(PACK) -package $(PACK_DRIVER_DriverGL) -I $(DRIVERS_DIR)/DriverGL -I $(DRIVERS_DIR) -I $(UTIL_DIR) $^

PATOLINE_DIR := $(d)

$(d)/Main.cmx: $(d)/Main.ml
	$(ECHO) "[OPT]    $<"
	$(Q)$(OCAMLOPT_NOINTF) -thread -rectypes -I +threads $(OFLAGS) $(PATOLINE_INCLUDES) -o $@ -c $<

PATOLINE_UNICODE_SCRIPTS := $(d)/UnicodeScripts

$(EDITORS_DIR)/emacs/SubSuper.el: $(d)/SubSuper.dyp ;
$(d)/SubSuper.dyp: $(d)/UnicodeData.txt $(PATOLINE_UNICODE_SCRIPTS)
	$(ECHO) "[UNIC]   $< -> $@"
	$(Q)$(PATOLINE_UNICODE_SCRIPTS) $< $@ $(EDITORS_DIR)/emacs/SubSuper.el

$(d)/UnicodeScripts: $(d)/UnicodeScripts.ml
	$(ECHO) "[OCAMLC] $< -> $@"
	$(Q)$(OCAMLC) -o $@ -package bigarray -package camomile -linkpkg $<

$(d)/tmp.dyp: $(d)/Parser.dyp $(d)/SubSuper.dyp
	$(ECHO) "[CAT]    $^ -> $@"
	$(Q)cat $^ > $@

$(d)/Parser.ml: $(d)/tmp.dyp
	$(ECHO) "[DYP]    $< -> $@"
	$(Q)$(DYPGEN) --no-mli --merge-warning $< > /dev/null
	$(Q)mv $(basename $<).ml $@

$(d)/Parser.cmx $(d)/Generateur.cmx: %.cmx: %.ml
	$(ECHO) "[OPT]    $<"
	$(Q)$(OCAMLOPT_NOINTF) -rectypes $(OFLAGS) $(PATOLINE_INCLUDES) -o $@ -c $<

$(d)/Build.cmo: %.cmo: %.ml
	$(ECHO) "[OPT]    $<"
	$(Q)$(OCAMLC) -thread $(OFLAGS) $(PATOLINE_INCLUDES) -o $@ -c $<

$(d)/Build.cmx: %.cmx: %.ml
	$(ECHO) "[OPT]    $<"
	$(Q)$(OCAMLOPT_NOINTF) -thread $(OFLAGS) $(PATOLINE_INCLUDES) -o $@ -c $<

CLEAN += $(d)/*.o $(d)/*.cm[iox] $(d)/Parser.ml $(d)/SubSuper.dyp $(d)/patoline $(d)/tmp.dyp $(EDITORS_DIR)/emacs/SubSuper.el $(d)/UnicodeScripts $(d)/pa_patoline
DISTCLEAN += $(d)/*.depends

# Installing
install: install-patoline-bin install-patoline-lib
.PHONY: install-patoline-bin install-patoline-lib
install-patoline-bin: install-bindir $(d)/patoline $(PA_PATOLINE) 
	install -m 755 $(wordlist 2,$(words $^),$^) $(DESTDIR)/$(INSTALL_BIN_DIR)
install-patoline-lib: install-typography $(d)/Build.cmi
	install -m 644 $(wordlist 2,$(words $^),$^) $(DESTDIR)/$(INSTALL_TYPOGRAPHY_DIR)

.PHONY: install-pa_patoline
install-pa_patoline: install-patoline-bin $(d)/pa_patoline
	install -m 755 $(wordlist 2,$(words $^),$^) $(DESTDIR)/$(INSTALL_BIN_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
