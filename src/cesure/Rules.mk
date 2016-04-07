# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

CESURE_INCLUDES := -I $(d) $(PACK_CESURE) -pp pa_ocaml
CESURE_DEPS_INCLUDES := -I $(d) $(DEPS_PACK_CESURE) -pp pa_ocaml

$(d)/%.ml.depends: INCLUDES += $(CESURE_DEPS_INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx $(d)/%.cma $(d)/%.cmxa $(d)/cesure: INCLUDES:=$(CESURE_INCLUDES)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(d)/cesure.ml.depends $(d)/hyphen.ml.depends
endif
endif

all: $(d)/cesure $(d)/cesure.cmxa $(d)/cesure.cma

$(d)/cesure.cma: $(d)/hyphen.cmo $(UNICODE_DIR)/unicodelib.cma
	$(ECHO) "[LINK]   $< -> $@"
	$(Q)$(OCAMLC) $(OFLAGS) $(INCLUDES) -a -o $@ decap.cma $<

$(d)/cesure.cmxa: $(d)/hyphen.cmx $(UNICODE_DIR)/unicodelib.cmxa
	$(ECHO) "[LINK]   $< -> $@"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(INCLUDES) -a -o $@ $<

$(d)/cesure: $(UNICODE_DIR)/unicodelib.cmxa $(d)/hyphen.cmx $(d)/cesure.cmx
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(INCLUDES) -o $@ unix.cmxa str.cmxa sqlite3.cmxa decap.cmxa $^

CLEAN += $(d)/*.cmx $(d)/*.o $(d)/*.cmi $(d)/*.cmo $(d)/*.a
DISTCLEAN += $(d)/cesure.cma $(d)/cesure.cmxa $(d)/cesure

# Installing
install: install-cesure-bin install-cesure-lib
.PHONY: install-cesure-bin
install-cesure-bin: install-bindir $(d)/cesure
	install -m 755 $(wordlist 2,$(words $^),$^) $(DESTDIR)/$(INSTALL_BIN_DIR)

.PHONY: install-cesure-lib
install-cesure-lib: $(d)/cesure.cma $(d)/cesure.cmxa $(d)/META $(d)/cesure.o $(d)/cesure.a $(d)/hyphen.cmi $(d)/hyphen.cmx $(d)/hyphen.cmo
	install -m 755 -d $(DESTDIR)/$(INSTALL_CESURE_DIR)
	install -m 644 -p $^ $(DESTDIR)/$(INSTALL_CESURE_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
