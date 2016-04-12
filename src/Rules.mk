# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

# Useful directories, to be referenced from other Rules.ml
SRC_DIR := $(d)
PATOLINE_IN_SRC := $(d)/Patoline/patoline
PA_PATOLINE_IN_SRC := $(d)/Patoline/pa_patoline
PATOLINE_DIR := $(d)/Patoline
TYPOGRAPHY_DIR := $(d)/Typography
RAWLIB_DIR := $(d)/rawlib
DB_DIR := $(d)/db
DRIVERS_DIR := $(d)/Drivers
FORMAT_DIR := $(d)/Format
UTIL_DIR := $(d)/patutil
RBUFFER_DIR := $(d)/rbuffer
LIBFONTS_DIR := $(d)/patfonts
CESURE_DIR := $(d)/cesure
UNICODE_DIR := $(d)/unicodelib

$(d)/Patoline/Rules.mk: $(UNICODELIB_CMX)

# Visit subdirectories
MODULES := unicodelib rbuffer patutil patfonts rawlib db Typography Drivers \
	Pdf cesure Format $(OCAML_BIBI) plot proof plugins Patoline grammar

$(foreach mod,$(MODULES),$(eval include $(d)/$$(mod)/Rules.mk))

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
