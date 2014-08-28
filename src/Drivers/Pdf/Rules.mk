# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(d)/Pdf.ml.depends
endif
endif

PDF_DRIVERS_INCLUDES:= -I $(d) -I $(DRIVERS_DIR)/SVG $(DRIVERS_INCLUDES) $(PACK_DRIVER_Pdf)
$(d)/%.ml.depends: INCLUDES += $(PDF_DRIVERS_INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx: INCLUDES += $(PDF_DRIVERS_INCLUDES)

# cmi race condition
$(d)/Pdf.cmx: %.cmx: %.cmo

$(d)/Pdf.cmo: %.cmo: %.ml $(TYPOGRAPHY_DIR)/Typography.cma
	$(ECHO) "[OCAMLC] $<"
	$(Q)$(OCAMLC) $(OFLAGS) $(INCLUDES) -o $@ -c $<

$(d)/Pdf.cmx: %.cmx: %.ml $(TYPOGRAPHY_DIR)/Typography.cmxa
	$(ECHO) "[OPT]    $<"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(INCLUDES) -o $@ -c $<

$(d)/Pdf.cma: %.cma: %.cmo
	$(ECHO) "[MKLIB]    $< -> $@"
	$(Q)$(OCAMLC) $(INCLUDES) -a -o $@ $<

$(d)/Pdf.cmxa: %.cmxa: %.cmx
	$(ECHO) "[OMKLIB]    $< -> $@"
	$(Q)$(OCAMLOPT) $(INCLUDES) -a -o $@ $<

$(d)/Pdf.cmxs: %.cmxs: %.cmx
	$(ECHO) "[SHARE]    $< -> $@"
	$(Q)$(OCAMLOPT) $(INCLUDES) -linkpkg -shared -o $@ $<

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
