# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

all: $(d)/emacs/patoline-input.el

# Vim part

$(d)/emacs/%.depends: INCLUDES += -I $(UNICODE_DIR)
# Emacs part
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(d)/emacs/SubstKey.ml.depends
-include $(d)/emacs/UnicodeScripts.ml.depends
endif
endif

$(d)/emacs/SubstKey.cmx: INCLUDES += -I $(UNICODE_DIR) -I $(EDITORS_DIR)/emacs
$(d)/emacs/SubstKey.cmo: INCLUDES += -I $(UNICODE_DIR) -I $(EDITORS_DIR)/emacs
$(d)/emacs/SubstKey.cmx: $(d)/emacs/SubstKey.cmo
$(d)/emacs/SubstKey: $(d)/emacs/SubstKey.cmx $(UNICODE_DIR)/unicodelib.cmxa
	$(ECHO) "[NAT] $@"
	$(Q)$(OCAMLOPT) -package str,unicodelib $< -linkpkg -o $@

$(d)/emacs/UnicodeScripts.cmx: INCLUDES += -I $(UNICODE_DIR) -I $(EDITORS_DIR)/emacs
$(d)/emacs/UnicodeScripts.cmo: INCLUDES += -I $(UNICODE_DIR) -I $(EDITORS_DIR)/emacs
$(d)/emacs/UnicodeScripts.cmx: $(d)/emacs/UnicodeScripts.cmo
$(d)/emacs/UnicodeScripts: $(d)/emacs/UnicodeScripts.cmx $(UNICODE_DIR)/unicodelib.cmxa
	$(ECHO) "[NAT] $@"
	$(Q)$(OCAMLOPT) -package bigarray,unicodelib $< -linkpkg -o $@


$(d)/emacs/Subsup.el $(d)/emacs/Subsup.ml: $(UNICODE_DATA_TXT) $(d)/emacs/UnicodeScripts
	$(ECHO) "[GEN] $@"
	$(Q)$(lastword $^) $< $@ $(EDITORS_DIR)/emacs/Subsup.el

$(d)/emacs/Subsup2.el: $(d)/emacs/Subsup.el $(d)/emacs/quail.el $(d)/emacs/SubstKey
	$(ECHO) "[GEN] $@"
	$(Q)$(lastword $^) $(EDITORS_DIR)/emacs/quail.el $< > $@

$(d)/emacs/patoline-input.el: $(d)/emacs/patoline-input.pre $(d)/emacs/quail.el \
  $(d)/emacs/Subsup2.el $(d)/emacs/patoline-input.post
	$(ECHO) "[GEN] $@"
	$(Q)cat $^ > $@

CLEAN += $(d)/emacs/*.cm[iox] $(d)/emacs/*.o $(d)/emacs/UnicodeScripts \
				 $(d)/emacs/SubstKey $(d)/emacs/Subsup2.el \
				 $(d)/emacs/patoline-input.el $(d)/emacs/Subsup.el
DISTCLEAN += $(d)/emacs/*.depends

# Installing
install: install-editors
.PHONY: install-editors

install-editors: $(d)/emacs/patoline-input.el $(d)/emacs/patoline-mode.el
	install -m 755 -d $(DESTDIR)/$(INSTALL_EMACS_DIR)
	install -p -m 644 $^ $(DESTDIR)/$(INSTALL_EMACS_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
