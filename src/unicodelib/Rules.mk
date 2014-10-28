# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

UNICODELIB_INCLUDES := -I $(d)
UNICODELIB_DEPS_INCLUDES := -I $(d)

$(d)/%.depends: INCLUDES:=$(UNICODELIB_DEPS_INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx : INCLUDES:=$(UNICODELIB_INCLUDES)

# Compute ML files dependencies
SRC_$(d) := $(filter-out $(d)/pa_convert.ml, $(wildcard $(d)/*.ml)) $(wildcard $(d)/*.mli)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(addsuffix .depends,$(SRC_$(d)))
endif
endif

# Building
UNICODELIB_MODS:= UChar UTF UTF8 UTF16 UTF32 UTFConvert unicode_type

UNICODELIB_ML:=$(addsuffix .ml,$(addprefix $(d)/,$(UNICODELIB_MODS)))

UNICODELIB_CMO:=$(UNICODELIB_ML:.ml=.cmo)
UNICODELIB_CMX:=$(UNICODELIB_ML:.ml=.cmx)
UNICODELIB_CMI:=$(UNICODELIB_ML:.ml=.cmi)

# We cannot run ocamlc and ocamlopt simultaneously on the same input,
# since they both overwrite the .cmi file, which can get corrupted.
# That's why we arbitrarily force the following dependency.
$(UNICODELIB_CMX): %.cmx: %.cmo

# To be used at build time to generate 8bit-enconding to UTF-X converters
$(d)/pa_convert: $(d)/pa_convert.ml $(PA_OCAML_DIR)/decap.cmxa $(PA_OCAML_DIR)/decap_ocaml.cmxa $(PA_OCAML)
	$(ECHO) "[OPT]    ... -> $@"
	$(Q) ocamlopt -pp $(PA_OCAML) -o $@ \
		-I +decap -I +compiler-libs unix.cmxa str.cmxa ocamlcommon.cmxa \
		$(PA_OCAML_DIR)/decap.cmxa $(PA_OCAML_DIR)/decap_ocaml.cmxa $<

$(d)/unicode_parse.ml.depends: $(d)/unicode_parse.ml $(PA_OCAML)
	$(ECHO) "[DEPS]   ... -> $@"
	$(Q)$(OCAMLDEP) -pp $(PA_OCAML) $(UNICODELIB_INCLUDES) $< > $@

$(d)/unicode_parse.cmo: $(d)/unicode_parse.ml $(PA_OCAML)
	$(ECHO) "[OCAMLC]   ... -> $@"
	$(Q)$(OCAMLC) -package decap -pp $(PA_OCAML) $(COMPILER_INC) $(UNICODELIB_INCLUDES) -c $<

$(d)/unicode_parse.cmx: $(d)/unicode_parse.ml $(PA_OCAML)
	$(ECHO) "[OPT]   ... -> $@"
	$(Q)$(OCAMLOPT_NOINTF) -package decap -pp $(PA_OCAML) $(COMPILER_INC) $(UNICODELIB_INCLUDES) -c $<

$(d)/unicode_parse: $(d)/UChar.cmx $(PA_OCAML_DIR)/decap.cmxa $(PA_OCAML_DIR)/decap_ocaml.cmxa $(d)/unicode_type.cmx $(d)/unicode_parse.cmx 
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLOPT) -linkpkg -package decap $(COMPILER_INC) $(COMPILER_LIBO) $(PA_OCAML_DIR)/decap.cmxa -o $@ $^

src/Patoline/UnicodeData.cmx: src/Patoline/UnicodeData.txt $(d)/unicode_parse $(d)/UChar.cmx $(d)/unicode_type.cmx
	$(ECHO) "[OPT] ... -> ^@"
	$(Q)$(OCAMLOPT_NOINTF) -package decap,str -pp $(UNICODE_DIR)/unicode_parse -impl $< $(UNICODELIB_INCLUDES) -c

$(d)/unicodelib.cma: $(UNICODELIB_CMO)
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLC) -a -o $@ $^

$(d)/unicodelib.cmxa: $(UNICODELIB_CMX)
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLOPT) -a -o $@ $^

$(d)/unicodelib.cmxs: $(UNICODELIB_CMX)
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLOPT) -shared -o $@ $^

# Building everything
all: $(d)/unicodelib.cmxa $(d)/unicodelib.cma $(d)/unicodelib.cmxs

# Cleaning
CLEAN += $(d)/*.cma $(d)/*.cmxa $(d)/*.cmo $(d)/*.cmx $(d)/*.cmi $(d)/*.o $(d)/*.a $(d)/*.cmxs

DISTCLEAN += $(wildcard $(d)/*.depends) $(d)/pa_convert

# Installing
install: install-unicodelib
.PHONY: install-unicodelib
install-unicodelib: $(d)/unicodelib.cma $(d)/unicodelib.cmxa $(d)/unicodelib.cmxs $(d)/unicodelib.a $(UNICODELIB_CMI) $(UNICODELIB_CMX) $(UNICODELIB_CMO) $(d)/META
	install -m 755 -d $(DESTDIR)/$(INSTALL_UNICODELIB_DIR)
	install -m 644 -p $^ $(DESTDIR)/$(INSTALL_UNICODELIB_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
