# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

# Building everything
all: $(d)/patoconfig.cma $(d)/patoconfig.cmxa

$(d)/configRC.cmi: $(d)/configRC.mli
	$(ECHO) "[BYT] $@"
	$(Q)$(OCAMLC) -I $(CONFIG_DIR) -c -o $@ $<

$(d)/patConfig.cmi: $(d)/patConfig.mli
	$(ECHO) "[BYT] $@"
	$(Q)$(OCAMLC) -I $(CONFIG_DIR) -c -o $@ $<

$(d)/configRC.cmo: $(d)/configRC.ml $(d)/configRC.cmi
	$(ECHO) "[BYT] $@"
	$(Q)$(OCAMLC) -I $(CONFIG_DIR) -c -o $@ $<

$(d)/configRC.cmx: $(d)/configRC.ml $(d)/configRC.cmi
	$(ECHO) "[OPT] $@"
	$(Q)$(OCAMLOPT) -I $(CONFIG_DIR) -c -o $@ $<

$(d)/patDefault.cmo: $(d)/patDefault.ml
	$(ECHO) "[BYT] $@"
	$(Q)$(OCAMLC) -I $(CONFIG_DIR) -c -o $@ $<

$(d)/patDefault.cmx: $(d)/patDefault.ml
	$(ECHO) "[OPT] $@"
	$(Q)$(OCAMLOPT) -I $(CONFIG_DIR) -c -o $@ $<

$(d)/patConfig.cmo: $(d)/patConfig.ml $(d)/patDefault.cmo $(d)/configRC.cmi $(d)/patConfig.cmi
	$(ECHO) "[BYT] $@"
	$(Q)$(OCAMLC) -I $(CONFIG_DIR) -c -o $@ $<

$(d)/patConfig.cmx: $(d)/patConfig.ml $(d)/patDefault.cmx $(d)/configRC.cmx $(d)/patConfig.cmi
	$(ECHO) "[OPT] $@"
	$(Q)$(OCAMLOPT) -I $(CONFIG_DIR) -c -o $@ $<

$(d)/patoconfig.cma: $(d)/configRC.cmo $(d)/patDefault.cmo $(d)/patConfig.cmo
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLC) -I $(CONFIG_DIR) -a -o $@ $^

$(d)/patoconfig.cmxa: $(d)/configRC.cmx $(d)/patDefault.cmx $(d)/patConfig.cmx
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) -I $(CONFIG_DIR) -a -o $@ $^

# Cleaning
CLEAN += $(d)/*.cma $(d)/*.cmxa $(d)/*.cmo $(d)/*.cmx $(d)/*.cmi $(d)/*.o $(d)/*.a $(d)/*.cmxs
DISTCLEAN += $(d)/patDefault.ml

# Installing
install: install-config
.PHONY: install-unicodelib

CMFILES := $(wildcard $(d)/*.cm[iox]) $(wildcard $(d)/*.mli)

install-config: $(d)/patoconfig.cma $(d)/patoconfig.cmxa $(d)/patoconfig.a $(CMFILES)
	install -m 755 -d $(DESTDIR)/$(INSTALL_TYPOGRAPHY_DIR)
	install -m 644 -p $^ $(DESTDIR)/$(INSTALL_TYPOGRAPHY_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
