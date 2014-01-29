# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

-include $(d)/cesure.ml.depends

all: $(d)/cesure

CESURE_LINK := -linkpkg $(RBUFFER_DIR)/rbuffer.cmxa -linkpkg $(d)/../Util/UsualMake.cmxa -linkpkg $(d)/../Util/Util.cmxa

$(d)/cesure: $(TYPOGRAPHY_DIR)/Typography.cmxa $(d)/cesure.ml
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(PACK) -I $(TYPOGRAPHY_DIR) -I $(RBUFFER_DIR) dynlink.cmxa -o $@ -package str $(CESURE_LINK) $^

CLEAN += $(d)/cesure $(d)/*.cmx $(d)/*.o $(d)/*.cmi $(d)/*.cmo
DISTCLEAN += $(d)/cesure.ml.depends

# Installing
install: install-cesure
.PHONY: install-cesure
install-cesure: install-bindir $(d)/cesure
	install -m 755 $(wordlist 2,$(words $^),$^) $(DESTDIR)/$(INSTALL_BIN_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
