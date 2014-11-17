# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

# Compute ML files dependencies
SRC_$(d):=$(wildcard $(d)/*.ml)
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(addsuffix .depends,$(SRC_$(d)))
endif
endif

PROOF_INCLUDES := -I $(d) -I $(DRIVERS_DIR)/Pdf $(PACK_PROOF)
PROOF_DEPS_INCLUDES := -I $(d) -I $(DRIVERS_DIR)/Pdf $(DEPS_PACK_PROOF)
$(d)/%.depends: INCLUDES+=$(PROOF_DEPS_INCLUDES)
$(d)/proof $(d)/%.cmo $(d)/%.cmi $(d)/%.cmx: INCLUDES += $(PROOF_INCLUDES)

$(d)/proof.cmx: $(d)/proof.cmo

# Building stuff
all: $(d)/proof

$(d)/%.depends: INCLUDES:=$(PROOF_INCLUDES)

$(d)/proof: $(d)/proof.cmx $(UNICODE_DIR)/unicodelib.cmxa $(RBUFFER_DIR)/rbuffer.cmxa $(UTIL_DIR)/patutil.cmxa $(IMAGELIB_DIR)/imagelib.cmxa $(LIBFONTS_DIR)/fonts.cmxa $(TYPOGRAPHY_DIR)/Typography.cmxa 
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLOPT) $(INCLUDES) -o $@ -I $(DRIVERS_DIR)/Pdf -package Typography $(DRIVERS_DIR)/Pdf/Pdf.cmxa  -linkpkg $<

# Installing
install: install-proof
.PHONY: install-proof
install-proof: install-bindir $(d)/proof
	install -m 755 $(wordlist 2,$(words $^),$^) $(DESTDIR)/$(INSTALL_BIN_DIR)

# Cleaning
CLEAN += $(d)/*.cmx $(d)/proof $(d)/*.cmi $(d)/*.cmo
DISTCLEAN += $(d)/*.depends

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
