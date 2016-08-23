# This makefile is the main entry point for building Patoline, yet all
# the magic happens in Rules.mk.
#
# The current file does only sanity checks (GNU Make version, pre-make
# configuration) before including the main rules.
#
#
# QUIET OUTPUT
# ============
# We define $(Q) and $(ECHO) variables to handle pretty printing and
# quiet output.
#
# The makefiles of this project define standard pattern rules to call
# various OCaml tools.  They suppress make's usual output, and echo
# instead a short summary of the command being run.
#
# $(Q), standing for "Quiet", is used to control this behavior. Its
# default value is "@" to suppress make default output. If you want to
# get back the full compilation trace, append "Q=" to make invocation,
# as in:
#
#    make Q=
#
# i.e., setting $(Q) to the empty string.
#
# Every command in Patoline makefiles should be prefixed by $(Q).
#
# The $(ECHO) variable is a regular call to "echo" when quiet output is
# activated. When normal output is chosen instead, $(ECHO) does nothing.
Q=@
ifeq "$(strip $(Q))" "@"
  ECHO=@echo
else
  ECHO=@\#
endif



# Check that we have at least GNU Make 3.81. This works as long as
# lexicographic order on strings coincides with the order of gmake
# versions.
need_gmake := 3.81
ifeq "$(strip $(filter $(need_gmake),$(firstword $(sort $(MAKE_VERSION) $(need_gmake)))))" ""
  $(error This Makefile requires at least GNU Make version $(need_gmake))
endif

# We will soon require GNU Make 4.
soon_gmake := 4.0
define soon_gmake_message

****************************************************************************
****************************************************************************
**** This Makefile will soon require GNU Make version $(soon_gmake),
**** but you only have $(MAKE_VERSION).
**** Please upgrade your GNU Make installation.
****************************************************************************
****************************************************************************
endef
ifeq "$(strip $(filter $(soon_gmake),$(firstword $(sort $(MAKE_VERSION) $(soon_gmake)))))" ""
  $(warning $(soon_gmake_message))
endif

# Compilers and various tools
OCAML    := ocaml
OCAMLC   := ocamlfind ocamlc $(if $(OCPP),-pp '$(OCPP)',)
OCAMLOPT_NOPP := ocamlfind ocamlopt -g -intf-suffix .cmi
OCAMLOPT_NOINTF := $(OCAMLOPT_NOPP) $(if $(OCPP),-pp '$(OCPP)',)
OCAMLOPT := $(OCAMLOPT_NOINTF) -intf-suffix .cmi
OCAMLDEP := ocamlfind ocamldep $(if $(OCPP),-pp '$(OCPP)',) $(OCAMLDEPEXTRAS)
OCAMLMKLIB := ocamlfind ocamlmklib
OCAMLDOC := ocamlfind ocamldoc $(if $(OCPP),-pp '$(OCPP)',)
OCAMLYACC := ocamlyacc
OCAMLLEX := ocamllex
DYPGEN := dypgen

export OCAML OCAMLC OCAMLOPT OCAMLDEP OCAMLMKLIB

# Useful directories, to be referenced from other Rules.ml
FONTS_DIR := Fonts
FORMAT_DIR := Format
HYPHENATION_DIR := Hyphenation
EDITORS_DIR := editors

# Environment variable for ocamlfind to search for local package
# and ignore duplicate definition due to previous installation
OCAMLPATH_SAVE := $(OCAMLPATH)
OCAMLPATH = $(SRC_DIR):$(OCAMLPATH_SAVE)
export OCAMLPATH
OCAMLFIND_IGNORE_DUPS_IN=$(shell ocamlfind printconf destdir)
export OCAMLFIND_IGNORE_DUPS_IN

# Main rule prerequisites are expected to be extended by each Rules.mk
# We just declare it here to make it the (phony) default target.
all: configure testconfig

configure: configure.ml
	$(ECHO) "[OPT] -> $@"
	$(Q)rm -f src/Makefile.config
	$(Q)ocamlfind ocamlopt -package bytes,unix,str,findlib unix.cmxa str.cmxa \
		findlib.cmxa configure.ml -o configure

.PHONY: testconfig
testconfig:
	@ if [ ! -f "src/Makefile.config" ]; then echo Run './configure [options]' before make; exit 1; fi

distclean: distclean-configure

.PHONY: distclean-configure
distclean-configure:
	rm -f configure configure.cmi configure.cmx configure.o

# When configure is ok, include the main make rules.
ifneq "$(wildcard ./configure)" ""
ifneq "$(wildcard src/Makefile.config)" ""
  include Rules.mk
endif
endif

# Phony targets
.PHONY: install doc test clean distclean

install: install-bindir
install-bindir:
	install -m 755 -d $(DESTDIR)/$(INSTALL_BIN_DIR)

# Prevent make from removing intermediate build targets
.SECONDARY:	$(CLEAN) $(DISTCLEAN)

# Common rules for OCaml
Q=@
ifeq "$(strip $(Q))" "@"
  ECHO=@echo
else
  ECHO=@\#
endif

# Force INCLUDES to be an immediate variable
INCLUDES:=

%.ml.depends: %.ml
	$(ECHO) "[DEP] $@"
	$(Q)$(OCAMLDEP) $(INCLUDES) -I $(<D) $< > $@

%.mli.depends: %.mli
	$(ECHO) "[DEP] $@"
	$(Q)$(OCAMLDEP) $(INCLUDES) $< > $@

%.cmi: %.mli %.ml.depends
	$(ECHO) "[BYT] $@"
	$(Q)$(OCAMLC) $(OFLAGS) $(INCLUDES) -o $@ -c $<

%.cmo: %.ml %.ml.depends
	$(ECHO) "[BYT] $@"
	$(Q)$(OCAMLC) $(OFLAGS) $(INCLUDES) -o $@ -c $<
%.cmx: %.cmo

%.cmx: %.ml %.ml.depends
	$(ECHO) "[OPT] $@"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(INCLUDES) -o $@ -c $<

%: %.cmo
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLC) -linkpkg $(INCLUDES) -o $@ $<

%: %.cmx
	$(ECHO) "[LNK] $@"
	$(Q)$(OCAMLOPT) -linkpkg $(INCLUDES) -o $@ $<

%.ml: %.mly
	$(ECHO) "[YAC] $@"
	$(Q)$(OCAMLYACC) $<

%.ml: %.mll
	$(ECHO) "[LEX] $@"
	$(Q)$(OCAMLLEX) -q $<
