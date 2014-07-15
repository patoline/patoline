# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

-include $(d)/cesure.ml.depends

CESURE_INCLUDES := -I $(d) $(PACK_CESURE)
CESURE_DEPS_INCLUDES := -I $(d) $(DEPS_PACK_CESURE)
$(d)/%.depends: INCLUDES+=$(CESURE_DEPS_INCLUDES)
$(d)/cesure $(d)/%.cmo $(d)/%.cmi $(d)/%.cmx: INCLUDES += $(CESURE_INCLUDES)

all: $(d)/cesure

$(d)/cesure: $(d)/cesure.cmx $(TYPOGRAPHY_DIR)/Typography.cmxa $(TYPOGRAPHY_DIR)/DefaultFormat.cmxa $(LIBFONTS_DIR)/fonts.cmxa $(UTIL_DIR)/patutil.cmxa
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(INCLUDES) -linkpkg -o $@ $<

CLEAN += $(d)/cesure $(d)/*.cmx $(d)/*.o $(d)/*.cmi $(d)/*.cmo
DISTCLEAN += $(d)/cesure.ml.depends

# Installing
install: install-cesure
.PHONY: install-cesure
install-cesure: install-bindir $(d)/cesure
	install -m 755 $(wordlist 2,$(words $^),$^) $(DESTDIR)/$(INSTALL_BIN_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
