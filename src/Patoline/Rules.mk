# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

# Compute ML dependencies
DEPENDS_$(d) := $(addsuffix .depends,$(wildcard $(d)/*.ml))
$(filter-out $(d)/Parser.ml.depends,$(DEPENDS_$(d))): $(d)/Parser.ml.depends
-include $(DEPENDS_$(d))

$(d)/patoline: $(d)/Util.cmx $(d)/Language.cmx $(d)/Build.cmx $(d)/Config.cmx \
  $(d)/Parser.cmx $(d)/Generateur.cmx $(d)/SimpleGenerateur.cmx $(d)/Main.cmx \
  $(RBUFFER_DIR)/rbuffer.cmxa
	$(ECHO) "[OPT]    -> $@"
	$(Q)$(OCAMLOPT) -linkpkg -o $@ $(PACK) $(PACKAGE_DYP) -I $(RBUFFER_DIR) -I +threads str.cmxa threads.cmxa rbuffer.cmxa dynlink.cmxa $^

$(d)/patolineGL: $(RBUFFER_DIR)/rbuffer.cmxa $(TYPOGRAPHY_DIR)/Typography.cmxa $(DRIVERS_DIR)/DriverGL/DriverGL.cmxa $(d)/PatolineGL.ml
	$(ECHO) "[OPT]    $(lastword $^) -> $@"
	$(Q)$(OCAMLOPT) -linkpkg -o $@ $(PACK) -package $(PACK_DRIVER_DriverGL) -I $(DRIVERS_DIR)/DriverGL -I $(DRIVERS_DIR) -I $(RBUFFER_DIR) $^

PATOLINE_DIR := $(d)

$(d)/Main.cmx: $(d)/Main.ml
	$(ECHO) "[OPT]    $<"
	$(Q)$(OCAMLOPT) -thread -rectypes -I +threads $(OFLAGS) $(PACK) -I $(PATOLINE_DIR) $(INCLUDES) -o $@ -c $<

PATOLINE_UNICODE_SCRIPTS := $(d)/UnicodeScripts

$(EDITORS_DIR)/emacs/SubSuper.el: $(d)/SubSuper.dyp ;
$(d)/SubSuper.dyp: $(d)/UnicodeData.txt $(PATOLINE_UNICODE_SCRIPTS)
	$(PATOLINE_UNICODE_SCRIPTS) $< $@ $(EDITORS_DIR)/emacs/SubSuper.el

$(d)/UnicodeScripts: $(d)/UnicodeScripts.ml
	$(ECHO) "[OCAMLC] $< -> $@"
	$(Q)$(OCAMLC) -o $@ -package bigarray -package camomile -linkpkg $<

$(d)/tmp.dyp: $(d)/Parser.dyp $(d)/SubSuper.dyp
	cat $^ > $@
$(d)/Parser.ml: $(d)/tmp.dyp
	$(ECHO) "[DYP]    -> $@"
	$(Q)$(DYPGEN) --no-mli --merge-warning $<
	$(Q)mv $(basename $<).ml $@

$(d)/Parser.cmx $(d)/Generateur.cmx: %.cmx: %.ml
	$(ECHO) "[OPT]    $<"
	$(Q)$(OCAMLOPT) -rectypes $(OFLAGS) $(PACK) -package dyp $(INCLUDES) -I $(PATOLINE_DIR) -o $@ -c $<

$(d)/Build.cmx: %.cmx: %.ml
	$(ECHO) "[OPT]    $<"
	$(Q)$(OCAMLOPT) -thread $(OFLAGS) $(PACK) $(INCLUDES) -I $(PATOLINE_DIR) -I $(RBUFFER_DIR) -o $@ -c $<

CLEAN += $(d)/*.o $(d)/*.cm[iox] $(d)/Parser.ml $(d)/SubSuper.dyp $(d)/patoline $(d)/patolineGL $(d)/patolineGL2 $(d)/tmp.dyp $(EDITORS_DIR)/emacs/SubSuper.el $(d)/UnicodeScripts
DISTCLEAN += $(d)/*.depends

# Installing
install: install-patoline-bin install-patoline-lib
.PHONY: install-patoline-bin install-patoline-lib
install-patoline-bin: install-bindir $(d)/patoline
	install -m 755 $(wordlist 2,$(words $^),$^) $(DESTDIR)/$(INSTALL_BIN_DIR)
install-patoline-lib: install-typography $(d)/Build.cmi $(d)/Util.cmi
	install -m 644 $(wordlist 2,$(words $^),$^) $(DESTDIR)/$(INSTALL_TYPOGRAPHY_DIR)

# We do not yet add this target to the global install, since patolineGL
# can only be installed when DriverGL can be built. The file
# <src/Drivers/DriverGL/Rules.mk> takes care of building and installing
# patolineGL.
.PHONY: install-patoline-gl
install-patoline-gl: install-patoline-bin $(d)/patolineGL
	install -m 755 $(wordlist 2,$(words $^),$^) $(DESTDIR)/$(INSTALL_BIN_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
