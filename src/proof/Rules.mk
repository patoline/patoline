# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

# Compute ML files dependencies
SRC_$(d):=$(wildcard $(d)/*.ml)
-include $(addsuffix .depends,$(SRC_$(d)))

# Building stuff
all: $(d)/proof

PROOF_INCLUDES := -I $(d) -package Typography,patutil,rbuffer,imagelib,patfonts  -I $(DRIVERS_DIR)/Pdf

$(d)/%.depends: INCLUDES:=$(PROOF_INCLUDES)

$(d)/proof: $(d)/proof.cmx $(RBUFFER_DIR)/rbuffer.cmxa $(UTIL_DIR)/patutil.cmxa $(IMAGELIB_DIR)/imagelib.cmxa $(LIBFONTS_DIR)/fonts.cmxa $(TYPOGRAPHY_DIR)/Typography.cmxa $(TYPOGRAPHY_DIR)/DefaultFormat.cmxa $(DRIVERS_DIR)/Pdf/Pdf.cmx 
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLOPT) dynlink.cmxa -linkpkg $(PACK) $(PACK_DRIVER_Pdf) $(PROOF_INCLUDES) $(INCLUDES) -o $@ $(DRIVERS_DIR)/Pdf/Pdf.cmx $<

$(d)/proof.cmx: %.cmx: %.ml
	$(ECHO) "[OCAMLC] $< -> $@"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(PROOF_INCLUDES) $(PACK) $(INCLUDES) -I $(<D) -o $@ -c $<

# Installing
install: install-proof
.PHONY: install-proof
install-proof: install-bindir $(d)/proof
	install -m 755 $(wordlist 2,$(words $^),$^) $(DESTDIR)/$(INSTALL_BIN_DIR)

# Cleaning
CLEAN += $(d)/*.cmx $(d)/proof $(d)/*.cmi
DISTCLEAN += $(d)/*.depends

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
