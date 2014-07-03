# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

-include $(d)/cesure.ml.depends

CESURE_INCLUDES := -I $(TYPOGRAPHY_DIR) -I $(d)
all: $(d)/cesure

CESURE_LINK :=  $(RBUFFER_DIR)/rbuffer.cmxa $(UTIL_DIR)/util.cmxa $(LIBFONTS_DIR)/fonts.cmxa -linkpkg

$(d)/cesure: $(TYPOGRAPHY_DIR)/Typography.cmxa $(LIBFONTS_DIR)/fonts.cmxa $(IMAGELIB_DIR)/imagelib.cmxa $(UTIL_DIR)/util.cmxa $(d)/cesure.ml 
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(PACK) $(CESURE_INCLUDES) dynlink.cmxa -o $@ -package str -I $(IMAGELIB_DIR) -I $(UTIL_DIR) util.cmxa imagelib.cmxa $(CESURE_LINK) $^

CLEAN += $(d)/cesure $(d)/*.cmx $(d)/*.o $(d)/*.cmi $(d)/*.cmo
DISTCLEAN += $(d)/cesure.ml.depends

# Installing
install: install-cesure
.PHONY: install-cesure
install-cesure: install-bindir $(d)/cesure
	install -m 755 $(wordlist 2,$(words $^),$^) $(DESTDIR)/$(INSTALL_BIN_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
