# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

# Compute ML files dependencies
SRC_$(d):=$(wildcard $(d)/*.ml)
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(addsuffix .depends,$(SRC_$(d)))
endif
endif

# Building stuff
PATOPLOT_LIBS := $(d)/plot.cmxa $(d)/plot.a $(d)/plot.cmi
all: $(PATOPLOT_LIBS)

PLOT_INCLUDES := -I $(d) $(PACK_PLOT)
PLOT_DEPS_INCLUDES := -I $(d) $(DEPS_PACK_PLOT)
$(d)/%.depends: INCLUDES+=$(PLOT_DEPS_INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx: INCLUDES += $(PLOT_INCLUDES)

# We cannot run ocamlc and ocamlopt simultaneously on the same input,
# since they both overwrite the .cmi file, which can get corrupted.
# That's why we arbitrarily force the following dependencies.
$(d)/plot.cmx: $(d)/plot.cmo
$(d)/plot.p.cmx: $(d)/plot.cmo
$(d)/plot.cmi: $(d)/plot.cmo ;
$(d)/plot.a: $(d)/plot.cmxa ;

$(d)/plot.cmxa: %.cmxa: %.cmx $(TYPOGRAPHY_DIR)/Typography.cmxa
	$(ECHO) "[LINK]   $<"
	$(Q)$(OCAMLOPT) -a -o $@ $<

$(d)/plot.cma: %.cma: %.cmo $(TYPOGRAPHY_DIR)/Typography.cma
	$(ECHO) "[LINK]   $<"
	$(Q)$(OCAMLC) -a -o $@ $<

$(d)/plot.p.cmxa: %.p.cmxa: %.p.cmx $(TYPOGRAPHY_DIR)/Typography.cmxa
	$(ECHO) "[LINK]   $<"
	$(Q)$(OCAMLOPT) -a -p -o $@ $<

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
