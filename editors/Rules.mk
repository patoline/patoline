# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

all: $(d)/emacs/patoline-input.el

# Vim part


# Emacs part
-include $(d)/emacs/SubstKey.ml.depends

$(d)/emacs/SubstKey: $(d)/emacs/SubstKey.ml
	$(OCAMLOPT) -package str,camomile $< -linkpkg -o $@

# $(d)/emacs/SubSuper.el is built by src/Patoline/Rules.mk
$(d)/emacs/SubSuper2.el: $(d)/emacs/SubSuper.el $(SRC_DIR)/quail.el $(d)/emacs/SubstKey
	$(lastword $^) $(SRC_DIR)/quail.el $< > $@

$(d)/emacs/patoline-input.el: $(d)/emacs/patoline-input.pre $(SRC_DIR)/quail.el \
  $(d)/emacs/SubSuper2.el $(d)/emacs/patoline-input.post
	cat $^ > $@

CLEAN += $(d)/emacs/SubSuper2.el $(d)/emacs/SubstKey $(d)/emacs/patoline-input.el $(d)/emacs/*.cmo $(d)/emacs/*.cmi $(d)/emacs/*.o $(d)/emacs/*.cmx
DISTCLEAN += $(d)/emacs/*.depends

# Installing
install: install-editors
.PHONY: install-editors

install-editors: $(d)/emacs/patoline-input.el $(d)/emacs/patoline-mode.el \
  $(d)/emacs/SubSuper2.el $(d)/emacs/SubSuper.el
	install -m 755 -d $(DESTDIR)/$(INSTALL_EMACS_DIR)
	install -p -m 644 $^ $(DESTDIR)/$(INSTALL_EMACS_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
