# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

FILES:=$(wildcard $(d)/*.tex)
DICTS:=$(FILES:.tex=.hdict)

all: $(DICTS)

%.hdict: %.tex $(CESURE_DIR)/cesure
	$(ECHO) "[HYP] $@"
	$(Q)$(CESURE_DIR)/cesure $<

CLEAN += $(d)/*.hdict

# Installing
install: install-hyphen
.PHONY: install-hyphen

install-hyphen: $(DICTS)
	install -p -m 755 -d $(DESTDIR)/$(INSTALL_HYPHEN_DIR)
	install -p -m 644 $(DICTS) $(DESTDIR)/$(INSTALL_HYPHEN_DIR)/


# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
