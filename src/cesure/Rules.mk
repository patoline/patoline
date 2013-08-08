# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

-include $(d)/cesure.ml.depends

all: $(d)/cesure

$(d)/cesure: $(RBUFFER_DIR)/rbuffer.cmxa $(TYPOGRAPHY_DIR)/Typography.cmxa $(d)/cesure.ml
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(PACK) -I $(TYPOGRAPHY_DIR) -I $(RBUFFER_DIR) -o $@ -package str -linkpkg $^

CLEAN += $(d)/cesure $(d)/*.cmx $(d)/*.o $(d)/*.cmi $(d)/*.cmo
DISTCLEAN += $(d)/cesure.ml.depends

# Installing
install: install-cesure
.PHONY: install-cesure
install-cesure: install-bindir $(d)/cesure
	install -m 755 $(wordlist 2,$(words $^),$^) $(DESTDIR)/$(INSTALL_BIN_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
