# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

# Useful directories, to be referenced from other Rules.ml
SRC_DIR := $(d)
PATOLINE_IN_SRC := $(d)/patobuild/patoline
PATOBUILD_DIR := $(d)/patobuild
TYPOGRAPHY_DIR := $(d)/Typography
RAWLIB_DIR := $(d)/rawlib
DB_DIR := $(d)/db
PDF_DIR := $(d)/Pdf
PLOT_DIR := $(d)/plot
DRIVERS_DIR := $(d)/Drivers
FORMAT_DIR := $(d)/Format
UTIL_DIR := $(d)/patutil
RBUFFER_DIR := $(d)/rbuffer
LIBFONTS_DIR := $(d)/patfonts
CESURE_DIR := $(d)/cesure
UNICODE_DIR := $(d)/unicodelib
CONFIG_DIR := $(d)/config
GRAMMAR_DIR := $(d)/grammar
PACKAGES_DIR := $(d)/Packages
DEFAULT_FORMAT_DIR := $(TYPOGRAPHY_DIR)/DefaultFormat
PA_PATOLINE_DIR := $(d)/pa_patoline
PA_PATOLINE_IN_SRC := $(PA_PATOLINE_DIR)/pa_patoline
PA_PATOLINE_OPTS := --no-default-grammar --grammar $(GRAMMAR_DIR)/DefaultGrammar

DEPS_DIR := -I $(PA_PATOLINE_DIR) -I $(PATOBUILD_DIR) -I $(PACKAGES_DIR) \
  -I $(TYPOGRAPHY_DIR) -I $(RAWLIB_DIR) -I $(DB_DIR) -I $(DRIVERS_DIR) \
  -I $(FORMAT_DIR) -I $(UTIL_DIR) -I $(RBUFFER_DIR) -I $(LIBFONTS_DIR) \
  -I $(CESURE_DIR) -I $(UNICODE_DIR) -I $(CONFIG_DIR) -I $(GRAMMAR_DIR) \
  -I $(DEFAULT_FORMAT_DIR) -I $(LIBFONTS_DIR)/CFF -I $(LIBFONTS_DIR)/isoAdobe \
  -I $(LIBFONTS_DIR)/Opentype -I $(LIBFONTS_DIR)/unicodeRanges \
  -I $(DRIVERS_DIR)/Bin -I $(DRIVERS_DIR)/DriverCairo -I $(DRIVERS_DIR)/DriverGL \
  -I $(DRIVERS_DIR)/DriverImage -I $(DRIVERS_DIR)/Html -I $(DRIVERS_DIR)/Net \
  -I $(DRIVERS_DIR)/None -I $(DRIVERS_DIR)/Patonet -I $(DRIVERS_DIR)/Pdf \
  -I $(DRIVERS_DIR)/SVG -I $(DRIVERS_DIR) -I $(PDF_DIR) -I $(PLOT_DIR)

# Visit subdirectories
MODULES := unicodelib rbuffer patutil patfonts rawlib db Typography \
	Drivers Pdf cesure Format bibi plot proof patobuild grammar config \
	pa_patoline

$(foreach mod,$(MODULES),$(eval include $(d)/$$(mod)/Rules.mk))

#foreach to define mod ...bof FIXME
ifeq ($(MAKECMDGOALS),clean)
  $(foreach mod,Packages,$(eval include $(d)/$$(mod)/Rules.mk))
endif
ifeq ($(MAKECMDGOALS),distclean)
  $(foreach mod,Packages,$(eval include $(d)/$$(mod)/Rules.mk))
endif
ifeq ($(filter packages,$(MAKECMDGOALS)),packages)
  $(foreach mod,Packages,$(eval include $(d)/$$(mod)/Rules.mk))
endif
ifeq ($(filter install,$(MAKECMDGOALS)),install)
  $(foreach mod,Packages,$(eval include $(d)/$$(mod)/Rules.mk))
endif

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
