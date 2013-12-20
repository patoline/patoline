# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

PDF_DRIVER_INCLUDES:=-I $(RBUFFER_DIR) -I $(TYPOGRAPHY_DIR)

-include $(d)/Pdf.ml.depends

$(d)/Pdf.cmo: %.cmo: %.ml $(TYPOGRAPHY_DIR)/Typography.cma
	$(ECHO) "[OCAMLC] $<"
	$(Q)$(OCAMLC) $(OFLAGS) $(PACK) $(INCLUDES) $(DRIVERS_INCLUDES) $(PDF_DRIVER_INCLUDES) -o $@ -c $<

$(d)/Pdf.cmx: %.cmx: %.ml $(TYPOGRAPHY_DIR)/Typography.cmxa
	$(ECHO) "[OPT]    $<"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(PACK) $(INCLUDES) $(DRIVERS_INCLUDES) $(PDF_DRIVER_INCLUDES) -o $@ -c $<

$(d)/Pdf.cmxa: %.cmxa: %.cmx
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(PACK) $(DRIVERS_INCLUDES) $(PDF_DRIVER_INCLUDES) -a -o $@ $<

$(d)/Pdf.cmxs: %.cmxs: %.cmx
	$(ECHO) "[OPT]    $< -> $@"
	echo $(Q)$(OCAMLOPT) $(PACK) $(DRIVERS_INCLUDES) $(PDF_DRIVER_INCLUDES) -linkpkg -shared -o $@ $<
	$(Q)$(OCAMLOPT) $(PACK) $(DRIVERS_INCLUDES) $(PDF_DRIVER_INCLUDES)  -linkpkg -shared -o $@ $<

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
