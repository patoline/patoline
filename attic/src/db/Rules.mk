# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

DB_INCLUDES := -I $(d) $(PACK_DB)

$(d)/%.depends: INCLUDES:=$(DEPS_DIR)
$(d)/%.cmo $(d)/%.cmi $(d)/%.cmx : INCLUDES:=$(DB_INCLUDES)

DB_MODS:= Db DbMemory DbSqlite3
ifdef MYSQL_ENABLED
  DB_MODS += DbMysql
endif

DB_ML:=$(addsuffix .ml,$(addprefix $(d)/,$(DB_MODS)))
DB_CMO:=$(DB_ML:.ml=.cmo)
DB_CMX:=$(DB_ML:.ml=.cmx)
DB_CMI:=$(DB_ML:.ml=.cmi)

# Compute ML files dependencies
SRC_$(d) := $(DB_ML)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(addsuffix .depends,$(SRC_$(d)))
endif
endif

# Building

$(d)/db.cma: $(DB_CMO)
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLC) -a -o $@ $^

$(d)/db.cmxa: $(DB_CMX)
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) -a -o $@ $^

$(d)/db.cmxs: $(DB_CMX)
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) -shared -o $@ $^

# Building everything
all: $(d)/db.cmxa $(d)/db.cma $(d)/db.cmxs

# Cleaning
CLEAN += $(d)/*.cma $(d)/*.cmxa $(d)/*.cmo $(d)/*.cmx $(d)/*.cmi $(d)/*.o $(d)/*.a $(d)/*.cmxs

DISTCLEAN += $(wildcard $(d)/*.depends)

# Installing
install: install-db
.PHONY: install-db
install-db: $(d)/db.cma $(d)/db.cmxa $(d)/db.cmxs $(d)/db.a $(DB_CMI) \
	  $(DB_CMX) $(DB_CMO) $(d)/META
	$(ECHO) "[INS] db"
	- $(OCAMLFIND) remove db
	$(Q)$(OCAMLFIND) install db $^

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
