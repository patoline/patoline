# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(d)/cesure.ml.depends
endif
endif

CESURE_INCLUDES := -I $(d) $(PACK_CESURE)
CESURE_DEPS_INCLUDES := -I $(d) $(DEPS_PACK_CESURE)
$(d)/%.depends: INCLUDES+=$(CESURE_DEPS_INCLUDES)
$(d)/cesure $(d)/%.cmo $(d)/%.cmi $(d)/%.cmx: INCLUDES += $(CESURE_INCLUDES)

$(d)/cesure.cmx: $(d)/cesure.cmo

all: $(d)/cesure

$(d)/cesure: $(IMAGELIB_DIR)/imagelib.cmxa $(UTIL_DIR)/patutil.cmxa $(LIBFONTS_DIR)/fonts.cmxa $(TYPOGRAPHY_DIR)/Typography.cmxa $(TYPOGRAPHY_DIR)/DefaultFormat.cmxa $(d)/cesure.cmx 
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(INCLUDES) -o $@ zip.cmxa dynlink.cmxa str.cmxa unix.cmxa \
		rbuffer.cmxa sqlite3.cmxa unicodelib.cmxa bigarray.cmxa $^

CLEAN += $(d)/cesure $(d)/*.cmx $(d)/*.o $(d)/*.cmi $(d)/*.cmo
DISTCLEAN += $(d)/cesure.ml.depends

# Installing
install: install-cesure
.PHONY: install-cesure
install-cesure: install-bindir $(d)/cesure
	install -m 755 $(wordlist 2,$(words $^),$^) $(DESTDIR)/$(INSTALL_BIN_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
