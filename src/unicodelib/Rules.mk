# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

UNICODELIB_INCLUDES := -I $(d) $(PACK_UNICODELIB)
PA_CONV:=$(d)/pa_convert

$(d)/%.depends $(d)/%.cmo $(d)/%.cmi $(d)/%.cmx: OCPP=$(PA_OCAML)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx $(d)/%.cma $(d)/%.cmxa: INCLUDES:=$(UNICODELIB_INCLUDES)
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
	$(Q) echo 'let unicodelib_path = "$(INSTALL_UNICODELIB_DIR)"' > $@
###

### To be used at build time to generate 8bit-enconding to UTF-X converters
ENCODING_DATA := $(wildcard $(d)/encoding_data/*.TXT)
ENCODING_ML   := $(addprefix $(d)/, $(notdir $(ENCODING_DATA:.TXT=.ml)))
ENCODING_CMO  := $(ENCODING_ML:.ml=.cmo)
ENCODING_CMX  := $(ENCODING_ML:.ml=.cmx)
ENCODING_CMI  := $(ENCODING_ML:.ml=.cmi)
ENCODING_O    := $(ENCODING_ML:.ml=.o)

# During compilation, the map file UnicodeData.data won't be available
# at its system-wide path. Ensure with the following variable that we pick
# the version in the current directory.
UNICODELIB_PATH := $(d)
export UNICODELIB_PATH

$(PA_CONV): $(d)/pa_convert.ml
	$(ECHO) "[OPT] $@"
	$(Q)$(OCAMLOPT_NOPP) $(OFLAGS) $(UNICODELIB_INCLUDES) -pp pa_ocaml \
		-package earley.ocaml -o $@ unix.cmxa str.cmxa \
		earley_core.cmxa earley_str.cmxa ocamlcommon.cmxa earley_ocaml.cmxa $<

$(ENCODING_ML): %.ml: $(PA_CONV)
	$(ECHO) "[GEN] $@"
	$(Q)$(PA_CONV) --ascii $(@D)/encoding_data/$(basename $(@F)).TXT > $@

###

$(d)/pa_UnicodeData: $(d)/UChar.cmx $(d)/PermanentMap.cmx $(d)/UnicodeLibConfig.cmx $(d)/UCharInfo.cmx $(d)/pa_UnicodeData.cmx
	$(ECHO) "[OPT] $@"
	$(Q)$(OCAMLOPT_NOPP) $(OFLAGS) $(INCLUDES) -package compiler-libs -linkpkg \
		$(UNICODELIB_INCLUDES) -o $@ $^

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
	  $(d)/unicodelib.cmxs $(d)/unicodelib.a $(UNICODELIB_CMI) \
		$(UNICODELIB_CMX) $(UNICODELIB_CMO) $(ENCODING_CMO) $(ENCODING_CMX) \
		$(ENCODING_CMI) $(UNICODE_DATABASE) $(d)/META
	$(ECHO) "[INS] unicodelib"
	- $(OCAMLFIND) remove unicodelib
	$(Q)$(OCAMLFIND) install unicodelib $^

$(ENCODING_CMO): $(UNICODELIB_CMO)
$(ENCODING_CMX): $(UNICODELIB_CMX) $(ENCODING_CMO)
$(UNICODELIB_CMX): $(UNICODELIB_CMO)



# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
