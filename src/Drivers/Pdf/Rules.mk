# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

PDF_DRIVER_INCLUDES:=-I $(UTIL_DIR) -I $(TYPOGRAPHY_DIR)

-include $(d)/Pdf.ml.depends

# cmi race condition
$(d)/Pdf.cmx: %.cmx: %.cmo

$(d)/Pdf.cmo: %.cmo: %.ml $(TYPOGRAPHY_DIR)/Typography.cma
	$(ECHO) "[OCAMLC] $<"
	$(Q)$(OCAMLC) $(OFLAGS) $(PACK) -package $(PACK_DRIVER_Pdf) $(INCLUDES) -I $(<D) $(DRIVERS_INCLUDES) $(PDF_DRIVER_INCLUDES) -o $@ -c $<

$(d)/Pdf.cmx: %.cmx: %.ml $(TYPOGRAPHY_DIR)/Typography.cmxa
	$(ECHO) "[OPT]    $<"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(PACK) -package $(PACK_DRIVER_Pdf) $(INCLUDES) -I $(<D) $(DRIVERS_INCLUDES) $(PDF_DRIVER_INCLUDES) -o $@ -c $<

$(d)/Pdf.cma: %.cma: %.cmo
	$(ECHO) "[MKLIB]    $< -> $@"
	$(Q)$(OCAMLC) $(PACK) -package $(PACK_DRIVER_Pdf) $(DRIVERS_INCLUDES) $(PDF_DRIVER_INCLUDES) -a -o $@ $<

$(d)/Pdf.cmxa: %.cmxa: %.cmx
	$(ECHO) "[OMKLIB]    $< -> $@"
	$(Q)$(OCAMLOPT) $(PACK) -package $(PACK_DRIVER_Pdf) $(DRIVERS_INCLUDES) $(PDF_DRIVER_INCLUDES) -a -o $@ $<

$(d)/Pdf.cmxs: %.cmxs: %.cmx
	$(ECHO) "[SHARE]    $< -> $@"
	$(Q)$(OCAMLOPT) $(PACK) -package $(PACK_DRIVER_Pdf) $(DRIVERS_INCLUDES) $(PDF_DRIVER_INCLUDES)  -linkpkg -shared -o $@ $<

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
