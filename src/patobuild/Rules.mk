# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

PATOBUILD_INCLUDES := -I $(d) $(PACK_PATOBUILD) -I $(CONFIG_DIR)
PATOBUILD_DEPS_INCLUDES := -I $(d) $(DEPS_PACK_PATOBUILD) -I $(CONFIG_DIR)
$(d)/%.depends: INCLUDES += $(PATOBUILD_DEPS_INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx: INCLUDES += $(PATOBUILD_INCLUDES)

PAT_CMX := $(d)/Language.cmx $(d)/BuildDir.cmx $(d)/Build.cmx $(d)/Main.cmx

# $(PAT_CMX): OCAMLOPT := $(OCAMLOPT_NOINTF)
$(PAT_CMX): %.cmx: %.cmo

# Compute ML dependencies
SRC_$(d) := $(addsuffix .depends,$(wildcard $(d)/*.ml))

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(SRC_$(d))
endif
endif

all: $(d)/patoline

$(d)/patoline: $(CONFIG_DIR)/patoconfig.cmxa $(TYPOGRAPHY_DIR)/Typography.cmxa $(PAT_CMX)
	$(ECHO) "[NAT] $@"
	$(Q)$(OCAMLOPT) -o $@ $(PATOBUILD_INCLUDES) -I +threads -thread \
		dynlink.cmxa patutil.cmxa str.cmxa unix.cmxa rbuffer.cmxa \
		unicodelib.cmxa threads.cmxa patoconfig.cmxa $(PAT_CMX)

PATOBUILD_UNICODE_SCRIPTS := $(d)/UnicodeScripts

$(EDITORS_DIR)/emacs/Subsup.el $(d)/Subsup.ml: $(d)/UnicodeData.txt $(d)/UnicodeScripts
	$(ECHO) "[GEN] $@"
	$(Q)$(PATOBUILD_UNICODE_SCRIPTS) $< $@ $(EDITORS_DIR)/emacs/Subsup.el

$(d)/Main.cmx: $(d)/Main.ml $(CONFIG_DIR)/patoconfig.cmxa
	$(ECHO) "[OPT] $@"
	$(Q)$(OCAMLOPT) -thread -rectypes -I +threads $(OFLAGS) $(PATOBUILD_INCLUDES) -o $@ -c $<

$(d)/Main.cmo: $(d)/Main.ml
	$(ECHO) "[BYT] $@"
	$(Q)$(OCAMLC) -thread -rectypes -I +threads $(OFLAGS) $(PATOBUILD_INCLUDES) -o $@ -c $<

$(d)/UnicodeScripts.cmx: $(UNICODELIB_CMX) $(UNICODELIB_DEPS) $(UNICODELIB_ML)

$(d)/UnicodeScripts: $(d)/UnicodeScripts.cmx $(UNICODE_DIR)/unicodelib.cmxa
	$(ECHO) "[NAT] $@"
	$(Q)$(OCAMLOPT) -o $@ -package bigarray,unicodelib -linkpkg $<

$(d)/Build.cmo $(d)/Build.cmx: OFLAGS += -thread

CLEAN += $(d)/*.o $(d)/*.cm[iox] $(EDITORS_DIR)/emacs/Subsup.el $(d)/UnicodeScripts

DISTCLEAN += $(d)/*.depends $(d)/patoline

# Installing
install: install-patoline-bin install-patoline-lib

.PHONY: install-patoline-bin install-patoline-lib
install-patoline-bin: install-bindir $(d)/patoline
	install -m 755 $(wordlist 2,$(words $^),$^) $(DESTDIR)/$(INSTALL_BIN_DIR)
install-patoline-lib: install-typography $(d)/Build.cmi
	install -m 644 $(wordlist 2,$(words $^),$^) $(DESTDIR)/$(INSTALL_TYPOGRAPHY_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
