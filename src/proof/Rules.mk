# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

# Compute ML files dependencies
SRC_$(d):=$(wildcard $(d)/*.ml)
-include $(addsuffix .depends,$(SRC_$(d)))

# Building stuff
all: $(d)/proof

PROOF_INCLUDES := -I $(TYPOGRAPHY_DIR) -I $(DRIVERS_DIR)/Pdf -I $(RBUFFER_DIR)

$(d)/%.depends: INCLUDES:=-I $(d) $(PROOF_INCLUDES)

$(d)/proof: $(RBUFFER_DIR)/rbuffer.cmxa $(TYPOGRAPHY_DIR)/Typography.cmxa $(DRIVERS_DIR)/Pdf/Pdf.cmx $(d)/proof.cmx
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLOPT) dynlink.cmxa -linkpkg $(PACK) $(PROOF_INCLUDES) $(INCLUDES) -o $@ $^

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
