# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

all: $(d)/pa_patoline

$(d)/Subsup.cmx: $(d)/Subsup.ml
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT_NOINTF) -pp pa_ocaml -package decap -I $(PA_PATOLINE_DIR) -o $@ -c $<

$(d)/prefixTree.cmx: $(d)/prefixTree.ml
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT_NOINTF) -I $(PA_PATOLINE_DIR) -o $@ -c $<

$(d)/pa_patoline.cmx: $(d)/pa_patoline.ml $(d)/Subsup.cmx $(d)/prefixTree.cmx $(CONFIG_DIR)/patoconfig.cmxa
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT_NOINTF) -pp pa_ocaml -package patutil,decap,compiler-libs \
		-I $(CONFIG_DIR) -I $(PATOBUILD_DIR) -I $(PA_PATOLINE_DIR) -rectypes \
		-o $@ -c $<

$(d)/pa_patoline: $(d)/pa_patoline.cmx $(d)/Subsup.cmx $(d)/prefixTree.cmx $(UTIL_DIR)/patutil.cmxa $(CONFIG_DIR)/patoconfig.cmxa
	$(ECHO) "[OPT]    ... -> $@"
	$(Q)$(OCAMLOPT) -package patutil,imagelib,dynlink,str,decap,compiler-libs \
		-I $(PATOBUILD_DIR) -I $(PA_PATOLINE_DIR) -o $@ \
		bigarray.cmxa unicodelib.cmxa rbuffer.cmxa patutil.cmxa unix.cmxa \
		str.cmxa ocamlcommon.cmxa decap.cmxa decap_ocaml.cmxa \
		Subsup.cmx prefixTree.cmx $(CONFIG_DIR)/patoconfig.cmxa $<

CLEAN += $(d)/*.o $(d)/*.cm[iox]
DISTCLEAN += $(d)/pa_patoline

# Installing
install: install-pa_patoline

.PHONY: install-pa_patoline
install-pa_patoline: $(d)/pa_patoline
	install -m 755 -d $(DESTDIR)/$(INSTALL_BIN_DIR)
	install -m 755 -p $^ $(DESTDIR)/$(INSTALL_BIN_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
