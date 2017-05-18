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

# Declare common phony targets
.PHONY: all clean distclean install doc test check

# Many targets are created by redirecting stdout. Yet, when the command
# fails, the target is nevertheless created as a zero-length file. On
# subsequent calls to make, the target is not considered anymore. The
# following ensures that targets are always removed when their rules
# fail.
.DELETE_ON_ERROR:

# Main rule prerequisites are expected to be extended by each Rules.mk
# We just declare it here to make it the (phony) default target.
all: configure testconfig

configure: configure.ml
	$(ECHO) "[OPT] -> $@"
	$(Q)rm -f src/Makefile.config
	$(Q)ocamlfind ocamlopt -package bytes,unix,str,findlib unix.cmxa str.cmxa \
		findlib.cmxa configure.ml -o configure

.PHONY: packages

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
