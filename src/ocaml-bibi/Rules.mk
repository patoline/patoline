# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

BIBI_INCLUDES := -I $(TYPOGRAPHY_DIR) -I $(FORMAT_DIR)
BIBI_PACK := -package camomile,sqlite3

# Finding dependencies
$(d)/bibi.ml.depends: INCLUDES += $(BIBI_INCLUDES)
-include $(d)/bibi.ml.depends

# Building stuff
BIBI_LIBS := $(d)/bibi.cmxa $(d)/bibi.p.cmxa $(d)/bibi.cma
all: $(BIBI_LIBS)

$(d)/bibi.cmxa: %.cmxa: %.cmx
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(BIBI_INCLUDES) $(PACK) $(INCLUDES) -I $(<D) -o $@ -a $<

$(d)/bibi.p.cmxa: %.p.cmxa: %.p.cmx
	$(ECHO) "[OPT -p] $< -> $@"
	$(Q)$(OCAMLOPT) -p $(OFLAGS) $(BIBI_INCLUDES) $(PACK) $(INCLUDES) -I $(<D) -o $@ -a $<

$(d)/bibi.cma: %.cma: %.cmo $(TYPOGRAPHY_DIR)/Typography.cma
	$(ECHO) "[OCAMLC] $< -> $@"
	$(Q)$(OCAMLC) $(OFLAGS) $(BIBI_INCLUDES) $(PACK) $(INCLUDES) -I $(<D) -o $@ -a $<

$(d)/bibi.cmo: %.cmo: %.ml
	$(ECHO) "[OCAMLC] $< -> $@"
	$(Q)$(OCAMLC) $(OFLAGS) $(BIBI_INCLUDES) $(PACK) $(BIBI_PACK) $(INCLUDES) -I $(<D) -o $@ -c $<

$(d)/bibi.cmx: %.cmx: %.ml $(TYPOGRAPHY_DIR)/Typography.cmxa
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(BIBI_INCLUDES) $(PACK) $(BIBI_PACK) $(INCLUDES) -I $(<D) -o $@ -c $<

$(d)/bibi.p.cmx: %.p.cmx: %.ml $(TYPOGRAPHY_DIR)/Typography.cmxa
	$(ECHO) "[OPT -p] $< -> $@"
	$(Q)$(OCAMLOPT) -p $(OFLAGS) $(BIBI_INCLUDES) $(PACK) $(BIBI_PACK) $(INCLUDES) -I $(<D) -o $@ -c $<

$(d)/bibi.cmi: $(d)/bibi.cmo ;

# Installing
install: install-bibi
.PHONY: install-bibi
install-bibi: $(BIBI_LIBS) $(d)/bibi.a $(d)/bibi.cmi $(d)/META
	install -m 755 -d $(DESTDIR)/$(INSTALL_BIBI_DIR)
	install -m 644 $^ $(DESTDIR)/$(INSTALL_BIBI_DIR)

# Cleaning
CLEAN += $(d)/*.cm[aoix] $(d)/*.cmxa $(d)/*.a $(d)/*.o
DISTCLEAN += $(d)/*.depends

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
