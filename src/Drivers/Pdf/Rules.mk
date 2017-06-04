# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

PDF_DRIVERS_INCLUDES:= -I $(d) -I $(DRIVERS_DIR)/SVG $(DRIVERS_INCLUDES) $(PACK_DRIVER_Pdf)

$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx: INCLUDES += $(PDF_DRIVERS_INCLUDES)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(d)/Pdf.ml.depends
endif
endif

$(d)/Pdf.cma: %.cma: %.cmo
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLC) $(INCLUDES) -a -o $@ $<

$(d)/Pdf.cmxa: %.cmxa: %.cmx
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) $(INCLUDES) -a -o $@ $<

$(d)/Pdf.cmxs: %.cmxs: %.cmx
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) $(INCLUDES) -linkpkg -shared -o $@ $<

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
