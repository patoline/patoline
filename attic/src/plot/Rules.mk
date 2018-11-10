# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

PLOT_INCLUDES := -I $(d) $(PACK_PLOT)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx $(d)/%.p.cmx: INCLUDES += $(PLOT_INCLUDES)

SRC_$(d):=$(wildcard $(d)/*.ml)
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(addsuffix .depends,$(SRC_$(d)))
endif
endif

# Building stuff
PATOPLOT_LIBS := $(d)/plot.cmxa $(d)/plot.cma
all: $(PATOPLOT_LIBS)

$(d)/plot.cmxa: %.cmxa: %.cmx $(TYPOGRAPHY_DIR)/Typography.cmxa
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) $(PLOT_INCLUDES) -a -o $@ $<

$(d)/plot.cma: %.cma: %.cmo $(TYPOGRAPHY_DIR)/Typography.cma
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLC) $(PLOT_INCLUDES) -a -o $@ $<

$(d)/plot.p.cmxa: %.p.cmxa: %.p.cmx $(TYPOGRAPHY_DIR)/Typography.cmxa
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) $(PLOT_INCLUDES) -a -p -o $@ $<

# Installing
install: install-plot
.PHONY: install-plot
install-plot: $(PATOPLOT_LIBS) $(d)/META
	install -m 755 -d $(DESTDIR)/$(INSTALL_PATOPLOT_DIR)
	install -m 644 $^ $(DESTDIR)/$(INSTALL_PATOPLOT_DIR)

# Cleaning
CLEAN += $(d)/*.cmo $(d)/*.cmx $(d)/*.cmi \
	 $(d)/*.cmxa $(d)/*.cma \
	 $(d)/*.p.cmx $(d)/*.p.cmxa \
	 $(d)/*.o $(d)/*.a
DISTCLEAN += $(d)/*.depends

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
