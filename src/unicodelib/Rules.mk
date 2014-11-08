# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

UNICODELIB_INCLUDES := -I $(d)
UNICODELIB_DEPS_INCLUDES := -I $(d)

$(d)/%.depends: INCLUDES:=$(UNICODELIB_DEPS_INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx : INCLUDES:=$(UNICODELIB_INCLUDES)

# Compute ML files dependencies
SRC_$(d) := $(filter-out $(d)/pa_convert.ml, $(filter-out $(d)/pa_UnicodeData.ml, $(wildcard $(d)/*.ml))) $(wildcard $(d)/*.mli)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(addsuffix .depends,$(SRC_$(d)))
endif
endif

# Building
UNICODELIB_MODS:= UChar UTF UTF8 UTF16 UTF32 UTFConvert PermanentMap UnicodeLibConfig UCharInfo

UNICODELIB_ML:=$(addsuffix .ml,$(addprefix $(d)/,$(UNICODELIB_MODS)))

UNICODELIB_CMO:=$(UNICODELIB_ML:.ml=.cmo)
UNICODELIB_CMX:=$(UNICODELIB_ML:.ml=.cmx)
UNICODELIB_CMI:=$(UNICODELIB_ML:.ml=.cmi)

# We cannot run ocamlc and ocamlopt simultaneously on the same input,
# since they both overwrite the .cmi file, which can get corrupted.
# That's why we arbitrarily force the following dependency.
$(UNICODELIB_CMX): %.cmx: %.cmo

### Generation of the configuration file
$(d)/UnicodeLibConfig.ml:
	$(ECHO) "[CONF]   ... -> $@"
	$(Q) echo 'let datafile = "$(INSTALL_UNICODELIB_DIR)/UnicodeData.data"' > $@
###

### To be used at build time to generate 8bit-enconding to UTF-X converters
ENCODING_DATA := $(wildcard $(d)/encoding_data/*.TXT)
ENCODING_ML   := $(addprefix $(d)/, $(notdir $(ENCODING_DATA:.TXT=.ml)))
ENCODING_CMO  := $(ENCODING_ML:.ml=.cmo)
ENCODING_CMX  := $(ENCODING_ML:.ml=.cmx)
ENCODING_CMI  := $(ENCODING_ML:.ml=.cmi)
ENCODING_O    := $(ENCODING_ML:.ml=.o)

$(ENCODING_CMX): %.cmx: %.cmo

PA_CONV:=$(d)/pa_convert

$(PA_CONV): $(d)/pa_convert.ml $(PA_OCAML_DIR)/decap.cmxa $(PA_OCAML_DIR)/decap_ocaml.cmxa $(PA_OCAML)
	$(ECHO) "[OPT]    ... -> $@"
	$(Q) ocamlopt -pp $(PA_OCAML) -o $@ \
		-I $(PA_OCAML_DIR) -I +compiler-libs unix.cmxa str.cmxa ocamlcommon.cmxa \
		$(PA_OCAML_DIR)/decap.cmxa $(PA_OCAML_DIR)/decap_ocaml.cmxa $<

$(ENCODING_ML): %.ml: $(PA_CONV)
	$(ECHO) "[PA_CNV] ... -> $@"
	$(Q) $(PA_CONV) --ascii $(@D)/encoding_data/$(basename $(@F)).TXT > $@

$(ENCODING_CMO): %.cmo: %.ml
	$(ECHO) "[OCAMLC] ... -> $@"
	$(Q) ocamlc $(UNICODELIB_INCLUDES) -c $<

$(ENCODING_CMX): %.cmx: %.ml
	$(ECHO) "[OPT]    ... -> $@"
	$(Q) ocamlopt $(UNICODELIB_INCLUDES) -c $<
###

### Parsing and data generation for UnicodeData.txt
$(d)/PermanentMap.cmo: $(d)/PermanentMap.ml
	$(ECHO) "[OCAMLC] ... -> $@"
	$(Q) ocamlfind ocamlc -package sqlite3 $(UNICODELIB_INCLUDES) -c $<

$(d)/PermanentMap.cmx: $(d)/PermanentMap.ml
	$(ECHO) "[OPT]    ... -> $@"
	$(Q) ocamlfind ocamlopt -package sqlite3 $(UNICODELIB_INCLUDES) -c $<

$(d)/pa_UnicodeData.cmx: $(d)/pa_UnicodeData.ml $(PA_OCAML)
	$(ECHO) "[OCAMLC] ... -> $@"
	$(Q) ocamlfind ocamlopt -package decap -pp $(PA_OCAML) -I +compiler-libs \
		$(UNICODELIB_INCLUDES) -c $<

$(d)/pa_UnicodeData: $(PA_OCAML_DIR)/decap.cmxa $(d)/UChar.cmx $(d)/PermanentMap.cmx $(d)/UnicodeLibConfig.cmx $(d)/UCharInfo.cmx $(d)/pa_UnicodeData.cmx
	$(ECHO) "[OPT]    ... -> $@"
	$(Q) ocamlfind ocamlopt -linkpkg -package sqlite3,decap -I +compiler-libs \
		$(UNICODELIB_INCLUDES) -o $@ ocamlcommon.cmxa $^

UNICODE_DATA_TXT := $(d)/data/UnicodeData.txt
UNICODE_DATABASE := $(d)/UnicodeData.data

$(d)/UnicodeData.data: $(d)/pa_UnicodeData $(UNICODE_DATA_TXT)
	$(ECHO) "[PA_Uni] ... -> $@"
	$(Q) rm -f $(UNICODE_DATABASE)
	$(Q) $< $(UNICODE_DATA_TXT) $(UNICODE_DATABASE)
###

$(d)/unicodelib.cma: $(UNICODELIB_CMO) $(ENCODING_CMO)
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLC) -a -o $@ $^

$(d)/unicodelib.cmxa: $(UNICODELIB_CMX) $(ENCODING_CMX)
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLOPT) -a -o $@ $^

$(d)/unicodelib.cmxs: $(UNICODELIB_CMX) $(ENCODING_CMX)
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLOPT) -shared -o $@ $^

# Building everything
all: $(d)/unicodelib.cmxa $(d)/unicodelib.cma $(d)/unicodelib.cmxs $(UNICODE_DATABASE)

# Cleaning
CLEAN += $(d)/*.cma $(d)/*.cmxa $(d)/*.cmo $(d)/*.cmx $(d)/*.cmi $(d)/*.o $(d)/*.a $(d)/*.cmxs $(ENCODING_CMO) $(ENCODING_CMX) $(ENCODING_CMI) $(ENCODING_O)

DISTCLEAN += $(wildcard $(d)/*.depends) $(d)/pa_convert $(ENCODING_ML) $(d)/UnicodeData.data $(d)/pa_UnicodeData

# Installing
install: install-unicodelib
.PHONY: install-unicodelib
install-unicodelib: $(d)/unicodelib.cma $(d)/unicodelib.cmxa $(d)/unicodelib.cmxs $(d)/unicodelib.a $(UNICODELIB_CMI) $(UNICODELIB_CMX) $(UNICODELIB_CMO) $(d)/META $(ENCODING_CMO) $(ENCODING_CMX) $(ENCODING_CMI) $(UNICODE_DATABASE)
	install -m 755 -d $(DESTDIR)/$(INSTALL_UNICODELIB_DIR)
	install -m 644 -p $^ $(DESTDIR)/$(INSTALL_UNICODELIB_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
