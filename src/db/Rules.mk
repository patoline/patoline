# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

DB_INCLUDES := -I $(d) $(PACK_DB)
DB_DEPS_INCLUDES := -I $(d) $(DEPS_PACK_DB)

$(d)/%.depends: INCLUDES:=$(DB_DEPS_INCLUDES)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx : INCLUDES:=$(DB_INCLUDES)

# Compute ML files dependencies
SRC_$(d) := $(wildcard $(d)/*.ml) $(wildcard $(d)/*.mli)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(addsuffix .depends,$(SRC_$(d)))
endif
endif

# Building
DB_MODS:= Db

DB_ML:=$(addsuffix .ml,$(addprefix $(d)/,$(DB_MODS)))

DB_CMO:=$(DB_ML:.ml=.cmo)
DB_CMX:=$(DB_ML:.ml=.cmx)
DB_CMI:=$(DB_ML:.ml=.cmi)

# We cannot run ocamlc and ocamlopt simultaneously on the same input,
# since they both overwrite the .cmi file, which can get corrupted.
# That's why we arbitrarily force the following dependency.
$(DB_CMX): %.cmx: %.cmo

$(d)/db.cma: $(DB_CMO)
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLC) -a -o $@ $^

$(d)/db.cmxa: $(DB_CMX)
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLOPT) -a -o $@ $^

$(d)/db.cmxs: $(DB_CMX)
	$(ECHO) "[LINK]   ... -> $@"
	$(Q)$(OCAMLOPT) -shared -o $@ $^

# Building everything
all: $(d)/db.cmxa $(d)/db.cma $(d)/db.cmxs

# Cleaning
CLEAN += $(d)/*.cma $(d)/*.cmxa $(d)/*.cmo $(d)/*.cmx $(d)/*.cmi $(d)/*.o $(d)/*.a $(d)/*.cmxs

DISTCLEAN += $(wildcard $(d)/*.depends)

# Installing
install: install-db
.PHONY: install-db
install-db: $(d)/db.cma $(d)/db.cmxa $(d)/db.cmxs $(d)/db.a $(DB_CMI) $(DB_CMX) $(DB_CMO) $(d)/META
	install -m 755 -d $(DESTDIR)/$(INSTALL_DB_DIR)
	install -m 644 -p $^ $(DESTDIR)/$(INSTALL_DB_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
