# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

UNICODELIB_INCLUDES := -I $(d) $(PACK_UNICODELIB)
UNICODELIB_DEPS_INCLUDES := -I $(d) $(DEPS_PACK_UNICODELIB)

PA_CONV:=$(d)/pa_convert

$(d)/%.depends: OCAMLDEP:=ocamlfind ocamldep -pp pa_ocaml
$(d)/%.depends: INCLUDES:=$(UNICODELIB_DEPS_INCLUDES)
$(d)/pa_%.depends: OCAMLDEP:=ocamlfind ocamldep -pp pa_ocaml $(OFLAGS) $(INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx $(d)/%.cma $(d)/%.cmxa: INCLUDES:=$(UNICODELIB_INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cma: OCAMLC:=ocamlfind ocamlc -pp pa_ocaml $(OFLAGS) $(INCLUDES)
$(d)/%.cmx $(d)/%.cmxa: OCAMLOPT:=ocamlfind ocamlopt -pp pa_ocaml $(OFLAGS) $(INCLUDES)
$(PA_CONV): INCLUDES:=$(UNICODELIB_INCLUDES)

# Compute ML files dependencies
# Building
UNICODELIB_MODS:= UChar UTF UTF8 UTF16 UTF32 UTFConvert PermanentMap UnicodeLibConfig UCharInfo

UNICODELIB_ML:=$(addsuffix .ml,$(addprefix $(d)/,$(UNICODELIB_MODS)))
UNICODELIB_DEPS:=$(addsuffix .depends,$(wildcard $(d)/*.ml))

$(UNICODELIB_DEPS): $(d)/UnicodeLibConfig.ml

UNICODELIB_CMO:=$(UNICODELIB_ML:.ml=.cmo)
UNICODELIB_CMX:=$(UNICODELIB_ML:.ml=.cmx)
UNICODELIB_CMI:=$(UNICODELIB_ML:.ml=.cmi)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(UNICODELIB_DEPS)
endif
endif

### Generation of the configuration file
$(d)/UnicodeLibConfig.ml:
	$(ECHO) "[GEN] $@"
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

$(PA_CONV): $(d)/pa_convert.ml
	$(ECHO) "[OPT] $@"
	$(Q)$(OCAMLOPT_NOPP) $(OFLAGS) $(INCLUDES) -pp pa_ocaml \
		-package compiler-libs -o $@ ocamlcommon.cmxa unix.cmxa str.cmxa \
		decap.cmxa decap_ocaml.cmxa $<

$(ENCODING_ML): %.ml: $(PA_CONV)
	$(ECHO) "[GEN] $@"
	$(Q)$(PA_CONV) --ascii $(@D)/encoding_data/$(basename $(@F)).TXT > $@

###

### Parsing and data generation for UnicodeData.txt
$(d)/pa_UnicodeData.cmx: $(d)/pa_UnicodeData.ml
	$(ECHO) "[BYT] $@"
	$(Q)$(OCAMLOPT_NOPP) $(OFLAGS) $(INCLUDES) -pp pa_ocaml -package compiler-libs -c $<

$(d)/pa_UnicodeData: $(d)/UChar.cmx $(d)/PermanentMap.cmx $(d)/UnicodeLibConfig.cmx $(d)/UCharInfo.cmx $(d)/pa_UnicodeData.cmx
	$(ECHO) "[OPT] $@"
	$(Q)$(OCAMLOPT_NOPP) $(OFLAGS) $(INCLUDES) -package compiler-libs -linkpkg \
		$(UNICODELIB_INCLUDES) -o $@ ocamlcommon.cmxa $^

UNICODE_DATA_TXT := $(d)/data/UnicodeData.txt
UNICODE_DATABASE := $(d)/UnicodeData.data

$(d)/UnicodeData.data: $(d)/pa_UnicodeData $(UNICODE_DATA_TXT)
	$(ECHO) "[UNI] $@"
	$(Q) rm -f $(UNICODE_DATABASE)
	$(Q) $< $(UNICODE_DATA_TXT) $(UNICODE_DATABASE)
###

$(d)/unicodelib.cma: $(UNICODELIB_CMO) $(ENCODING_CMO)
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLC) -a -o $@ $^

$(d)/unicodelib.cmxa: $(UNICODELIB_CMX) $(ENCODING_CMX)
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) -a -o $@ $^

$(d)/unicodelib.cmxs: $(UNICODELIB_CMX) $(ENCODING_CMX)
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) -shared -o $@ $^

# Building everything
all: $(d)/unicodelib.cmxa $(d)/unicodelib.cma $(d)/unicodelib.cmxs $(UNICODE_DATABASE)

# Cleaning
CLEAN += $(d)/*.cma $(d)/*.cmxa $(d)/*.cm[oxi] $(d)/*.o $(d)/*.a $(d)/*.cmxs \
	$(ENCODING_CMO) $(ENCODING_CMX) $(ENCODING_CMI) $(ENCODING_O)

DISTCLEAN += $(d)/*.depends $(d)/pa_convert $(ENCODING_ML) \
	$(d)/UnicodeData.data $(d)/pa_UnicodeData $(d)/UnicodeLibConfig.ml

# Installing
install: install-unicodelib
.PHONY: install-unicodelib
install-unicodelib: $(d)/unicodelib.cma $(d)/unicodelib.cmxa \
	$(d)/unicodelib.cmxs $(d)/unicodelib.a $(d)/META \
	$(UNICODELIB_CMI) $(UNICODELIB_CMX) $(UNICODELIB_CMO) \
	$(ENCODING_CMO) $(ENCODING_CMX) $(ENCODING_CMI) $(UNICODE_DATABASE)
	install -m 755 -d $(DESTDIR)/$(INSTALL_UNICODELIB_DIR)
	install -m 644 -p $^ $(DESTDIR)/$(INSTALL_UNICODELIB_DIR)

$(ENCODING_CMO): %.cmo: $(UNICODE_CMO)
$(ENCODING_CMX): %.cmx: $(UNICODE_CMX)



# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
