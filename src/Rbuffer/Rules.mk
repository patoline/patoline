# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

# Compute ML files dependencies
SRC_$(d):=$(wildcard $(d)/*.ml) $(wildcard $(d)/*.mli)

$(d)/%.depends $(d)/%.cmx $(d)/%.cmo: INCLUDES:=-I $(d)

-include $(addsuffix .depends,$(SRC_$(d)))

# Building stuff
RBUFFER_CMXA := $(d)/rbuffer.cmxa $(d)/rope.cmxa
RBUFFER_CMA  := $(RBUFFER_CMXA:.cmxa=.cma)
RBUFFER_PCMXA:= $(RBUFFER_CMXA:.cmxa=.p.cmxa)

RBUFFER_LIBS := $(RBUFFER_CMXA) $(RBUFFER_CMA) $(RBUFFER_PCMXA)

all: $(RBUFFER_LIBS)

$(RBUFFER_CMA): %.cma: %.cmo
	$(ECHO) "[LINK]   $<"
	$(Q)$(OCAMLC) -a -o $@ $<

$(RBUFFER_CMXA): %.cmxa: %.cmx
	$(ECHO) "[LINK]   $<"
	$(Q)$(OCAMLOPT) -a -o $@ $<

$(RBUFFER_PCMXA): %.p.cmxa: %.p.cmx
	$(ECHO) "[LINK]   $<"
	$(Q)$(OCAMLOPT) -a -p -o $@ $<


# Cleaning
CLEAN += $(d)/*.cmo $(d)/*.cmi $(d)/*.cma $(d)/*.cmx $(d)/*.a $(d)/*.cmxa $(d)/*.o

DISTCLEAN := $(DISTCLEAN) \
  $(wildcard $(d)/*.depends)

# Installing
install: install-rbuffer
.PHONY: install-rbuffer
install-rbuffer: $(RBUFFER_LIBS) $(d)/rope.p.a $(d)/rope.a $(d)/rbuffer.p.a $(d)/rbuffer.a $(d)/META
	install -m 755 -d $(DESTDIR)/$(INSTALL_RBUFFER_DIR)
	install -m 644 -p $^ $(DESTDIR)/$(INSTALL_RBUFFER_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
