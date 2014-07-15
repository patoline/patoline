# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

-include $(d)/cesure.ml.depends

CESURE_INCLUDES := -I $(d) -package str,imagelib,patutil,Typography
all: $(d)/cesure

$(d)/cesure: $(d)/cesure.ml $(TYPOGRAPHY_DIR)/Typography.cmxa $(LIBFONTS_DIR)/fonts.cmxa $(UTIL_DIR)/patutil.cmxa
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(PACK) $(CESURE_INCLUDES) -o $@ -linkpkg $<

CLEAN += $(d)/cesure $(d)/*.cmx $(d)/*.o $(d)/*.cmi $(d)/*.cmo
DISTCLEAN += $(d)/cesure.ml.depends

# Installing
install: install-cesure
.PHONY: install-cesure
install-cesure: install-bindir $(d)/cesure
	install -m 755 $(wordlist 2,$(words $^),$^) $(DESTDIR)/$(INSTALL_BIN_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
