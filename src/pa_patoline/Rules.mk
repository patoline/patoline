# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

all: $(d)/pa_patoline

PA_PATOLINE_INCLUDES := -I $(d) -I $(PATOBUILD_DIR) -I $(CONFIG_DIR) -I $(CONFIG_DIR) -I $(UTIL_DIR) -I $(UNICODE_DIR)

$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx: INCLUDES += $(PA_PATOLINE_INCLUDES)

# no .cmo here
$(d)/%.cmo: ; touch $@
$(d)/%.cmx: private OCAMLOPT=$(OCAMLOPT_NOINTF)

SRC_$(d) := $(wildcard $(d)/*.ml)
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(addsuffix .depends,$(SRC_$(d)))
endif
endif

$(d)/%.cmx $(d)/%.ml.depends: private OCPP=pa_ocaml
$(d)/%.cmx: private OFLAGS+=-package earley,earley_ocaml

$(d)/pa_patoline: $(d)/pa_patoline.cmx $(d)/Subsup.cmx $(d)/prefixTree.cmx $(UTIL_DIR)/patutil.cmxa $(CONFIG_DIR)/patoconfig.cmxa $(UNICODE_DIR)/unicodelib.cmxa
	$(ECHO) "[NAT] $@"
	$(Q)$(OCAMLOPT) -package patutil,imagelib,dynlink,str,earley,earley.str,earley_ocaml,compiler-libs \
		-I $(PATOBUILD_DIR) -I $(PA_PATOLINE_DIR) -o $@ \
		bigarray.cmxa unicodelib.cmxa patutil.cmxa unix.cmxa \
		str.cmxa ocamlcommon.cmxa earley.cmxa earleyStr.cmxa earley_ocaml.cmxa \
		Subsup.cmx prefixTree.cmx $(CONFIG_DIR)/patoconfig.cmxa $<

CLEAN += $(d)/*.o $(d)/*.cm[iox]
DISTCLEAN += $(d)/pa_patoline $(d)/*.ml.depends

# Installing
install: install-pa_patoline

.PHONY: install-pa_patoline
install-pa_patoline: $(d)/pa_patoline
	install -m 755 -d $(DESTDIR)/$(INSTALL_BIN_DIR)
	install -m 755 -p $^ $(DESTDIR)/$(INSTALL_BIN_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
