# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

UNICODELIB_INCLUDES := -I $(d)
UNICODELIB_DEPS_INCLUDES := -I $(d)

$(d)/%.depends: OCAMLDEP:=ocamlfind ocamldep -pp pa_ocaml
$(d)/%.depends: INCLUDES:=$(UNICODELIB_DEPS_INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx: INCLUDES:=$(UNICODELIB_INCLUDES)
$(d)/%.cmo $(d)/%.cmi: OCAMLC:=ocamlfind ocamlc -pp pa_ocaml $(INCLUDES)
$(d)/%.cmx: OCAMLOPT:=ocamlfind ocamlopt -pp pa_ocaml $(INCLUDES)

# Compute ML files dependencies
# Building
UNICODELIB_MODS:= UChar UTF UTF8 UTF16 UTF32 UTFConvert PermanentMap UnicodeLibConfig UCharInfo

UNICODELIB_ML:=$(addsuffix .ml,$(addprefix $(d)/,$(UNICODELIB_MODS)))
UNICODELIB_DEPS:=$(addsuffix .depends,$(UNICODELIB_ML)) $(d)/pa_convert.ml.depends

$(UNICODELIB_DEPS): $(d)/UnicodeLibConfig.ml

UNICODELIB_CMO:=$(UNICODELIB_ML:.ml=.cmo)
UNICODELIB_CMX:=$(UNICODELIB_ML:.ml=.cmx)
UNICODELIB_CMI:=$(UNICODELIB_ML:.ml=.cmi)

#pb with ocamldep: undetected deps inside quotation
pa_convert.cmo: $(UNICODELIB_CMO) $(UNICODELIB_CMI)
pa_convert.cmx: $(UNICODELIB_CMX) $(UNICODELIB_CMI)


ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(UNICODELIB_DEPS)
endif
endif

### Generation of the configuration file
$(d)/UnicodeLibConfig.ml:
	$(ECHO) "[CONF]   ... -> $@"
	$(Q) echo 'let datafile = ref "$(INSTALL_UNICODELIB_DIR)/UnicodeData.data"' > $@
###

### To be used at build time to generate 8bit-enconding to UTF-X converters
ENCODING_DATA := $(wildcard $(d)/encoding_data/*.TXT)
ENCODING_ML   := $(addprefix $(d)/, $(notdir $(ENCODING_DATA:.TXT=.ml)))
ENCODING_CMO  := $(ENCODING_ML:.ml=.cmo)
ENCODING_CMX  := $(ENCODING_ML:.ml=.cmx)
ENCODING_CMI  := $(ENCODING_ML:.ml=.cmi)
ENCODING_O    := $(ENCODING_ML:.ml=.o)

$(ENCODING_CMX): %.cmx: %.cmo

PA_CONV:=$(d)/pa_convert --ascii

$(PA_CONV): $(d)/pa_convert.ml
	$(ECHO) "[OPT]    ... -> $@"
	$(Q) $(OCAMLOPT) -pp pa_ocaml -o $@ -I $(UNICODE_DIR) \
		-package decap $(COMPILER_INC) $(COMPILER_LIBO) unix.cmxa str.cmxa \
		decap.cmxa decap_ocaml.cmxa $<

$(ENCODING_ML): %.ml: $(PA_CONV)
	$(ECHO) "[PA_CNV] ... -> $@"
	$(Q) $(PA_CONV) $(@D)/encoding_data/$(basename $(@F)).TXT > $@

$(ENCODING_CMO): %.cmo: %.ml
	$(ECHO) "[OCAMLC] ... -> $@"
	$(Q) $(OCAMLC) $(UNICODELIB_INCLUDES) -c $<

$(ENCODING_CMX): %.cmx: %.ml
	$(ECHO) "[OPT]    ... -> $@"
	$(Q) $(OCAMLOPT) $(UNICODELIB_INCLUDES) -c $<
###

### Parsing and data generation for UnicodeData.txt
$(d)/PermanentMap.cmo: $(d)/PermanentMap.ml
	$(ECHO) "[OCAMLC] ... -> $@"
	$(Q) $(OCAMLC) -package sqlite3 $(UNICODELIB_INCLUDES) -c $<

$(d)/PermanentMap.cmx: $(d)/PermanentMap.ml
	$(ECHO) "[OPT]    ... -> $@"
	$(Q) $(OCAMLOPT) -package sqlite3 $(UNICODELIB_INCLUDES) -c $<

$(d)/pa_UnicodeData.cmx: $(d)/pa_UnicodeData.ml
	$(ECHO) "[OCAMLC] ... -> $@"
	$(Q) $(OCAMLOPT) -package decap -pp pa_ocaml $(COMPILER_INC) \
		$(UNICODELIB_INCLUDES) -c $<

$(d)/pa_UnicodeData: $(d)/UChar.cmx $(d)/PermanentMap.cmx $(d)/UnicodeLibConfig.cmx $(d)/UCharInfo.cmx $(d)/pa_UnicodeData.cmx
	$(ECHO) "[OPT]    ... -> $@"
	$(Q) $(OCAMLOPT) -linkpkg -package sqlite3,decap $(COMPILER_INC)\
		$(UNICODELIB_INCLUDES) -o $@ $(COMPILER_LIBO) $^

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

DISTCLEAN += $(wildcard $(d)/*.depends) $(d)/pa_convert $(ENCODING_ML) $(d)/UnicodeData.data $(d)/pa_UnicodeData $(d)/UnicodeLibConfig.ml

# Installing
install: install-unicodelib
.PHONY: install-unicodelib
install-unicodelib: $(d)/unicodelib.cma $(d)/unicodelib.cmxa $(d)/unicodelib.cmxs $(d)/unicodelib.a $(UNICODELIB_CMI) $(UNICODELIB_CMX) $(UNICODELIB_CMO) $(d)/META $(ENCODING_CMO) $(ENCODING_CMX) $(ENCODING_CMI) $(UNICODE_DATABASE)
	install -m 755 -d $(DESTDIR)/$(INSTALL_UNICODELIB_DIR)
	install -m 644 -p $^ $(DESTDIR)/$(INSTALL_UNICODELIB_DIR)

$(d)/UTF16.cmo : $(d)/UTF.cmo $(d)/UChar.cmo
$(d)/UTF16.cmx : $(d)/UTF.cmx $(d)/UChar.cmx
$(d)/UTF8.cmo : $(d)/UTF.cmo $(d)/UChar.cmo
$(d)/UTF8.cmx : $(d)/UTF.cmx $(d)/UChar.cmx
$(d)/UTF32.cmo : $(d)/UTF.cmo $(d)/UChar.cmo
$(d)/UTF32.cmx : $(d)/UTF.cmx $(d)/UChar.cmx
$(d)/UTFConvert.cmo : $(d)/UTF8.cmo \
    $(d)/UTF32.cmo $(d)/UTF16.cmo
$(d)/UTFConvert.cmx : $(d)/UTF8.cmx \
    $(d)/UTF32.cmx $(d)/UTF16.cmx
$(d)/UTF.cmo : $(d)/UChar.cmo
$(d)/UTF.cmx : $(d)/UChar.cmx
$(d)/LATIN1.cmo: $(d)/UTF8.cmo
$(d)/LATIN1.cmx: $(d)/UTF8.cmx
src/unicodelib/UCharInfo.cmo : src/unicodelib/UnicodeLibConfig.cmo \
    src/unicodelib/UChar.cmo src/unicodelib/PermanentMap.cmo
src/unicodelib/UCharInfo.cmx : src/unicodelib/UnicodeLibConfig.cmx \
    src/unicodelib/UChar.cmx src/unicodelib/PermanentMap.cmx

$(ENCODING_CMO): %.cmo: $(UNICODE_CMO)
$(ENCODING_CMX): %.cmx: $(UNICODE_CMX)



# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
