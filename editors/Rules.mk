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
endif
endif

$(d)/emacs/SubstKey.cmx: INCLUDES += -I $(UNICODE_DIR)

$(d)/emacs/SubstKey: $(d)/emacs/SubstKey.cmx $(UNICODE_DIR)/unicodelib.cmxa
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) -package str,unicodelib $< -linkpkg -o $@

# $(d)/emacs/SubSuper.el is built by src/Patoline/Rules.mk
$(d)/emacs/Subsup2.el: $(d)/emacs/Subsup.el $(d)/emacs/quail.el $(d)/emacs/SubstKey
	$(ECHO) "[SUBST]  ... -> $@"
	$(Q)$(lastword $^) $(EDITORS_DIR)/emacs/quail.el $< > $@

$(d)/emacs/patoline-input.el: $(d)/emacs/patoline-input.pre $(d)/emacs/quail.el \
  $(d)/emacs/Subsup2.el $(d)/emacs/patoline-input.post
	$(ECHO) "[CAT]    ... -> $@"
	$(Q)cat $^ > $@

CLEAN += $(d)/emacs/Subsup2.el $(d)/emacs/SubstKey $(d)/emacs/patoline-input.el $(d)/emacs/*.cmo $(d)/emacs/*.cmi $(d)/emacs/*.o $(d)/emacs/*.cmx
DISTCLEAN += $(d)/emacs/*.depends

# Installing
install: install-editors
.PHONY: install-editors

install-editors: $(d)/emacs/patoline-input.el $(d)/emacs/patoline-mode.el
	install -m 755 -d $(DESTDIR)/$(INSTALL_EMACS_DIR)
	install -p -m 644 $^ $(DESTDIR)/$(INSTALL_EMACS_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
