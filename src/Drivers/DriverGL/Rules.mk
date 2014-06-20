# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

GL_DRIVER_INCLUDES:=-I $(UTIL_DIR) -I $(TYPOGRAPHY_DIR) -I $(d)

SRC_$(d):=$(wildcard $(d)/*.ml)
DEPENDS_$(d) := $(addsuffix .depends,$(SRC_$(d)))
-include $(DEPENDS_$(d))

DRIVERGL_MODS:= GlFBO Vec3 DriverGL

DRIVERGL_ML:=$(addsuffix .ml,$(addprefix $(d)/,$(DRIVERGL_MODS)))
DRIVERGL_CMO:=$(DRIVERGL_ML:.ml=.cmo)
DRIVERGL_CMX:=$(DRIVERGL_ML:.ml=.cmx)

UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
FPIC_FLAGS=-fPIC
endif
ifeq ($(UNAME), Darwin)
FPIC_FLAGS=-I$(shell ocamlc -where) -fPIC # -framework GLUT -framework OpenGL
endif

# We cannot run ocamlc and ocamlopt simultaneously on the same input,
# since they both overwrite the .cmi file, which can get corrupted.
# That's why we arbitrarily force the following dependency.
$(DRIVERGL_CMX): %.cmx: %.cmo

$(d)/FrameBuffer.o: $(d)/FrameBuffer.c
	$(ECHO) "[CC]     $<"
	$(Q)$(CC) $(FPIC_FLAGS) $(CFLAGS) -c $< -o $@

#$(d)/GLNet.cmx: %.cmx: %.ml
#	$(ECHO) "[OPT]    $<"
#	$(Q)$(OCAMLOPT) $(OFLAGS) $(PACK) -package $(PACK_DRIVER_DriverGL) $(INCLUDES) $(DRIVERS_INCLUDES) $(GL_DRIVER_INCLUDES) -o $@ -c $<

$(d)/DriverGL.cmo: %.cmo: %.ml $(TYPOGRAPHY_DIR)/Typography.cma
	$(ECHO) "[OCAMLC]    $<"
	$(Q)$(OCAMLC) $(OFLAGS) $(PACK) -thread -package $(PACK_DRIVER_DriverGL) $(INCLUDES) -I $(<D) $(DRIVERS_INCLUDES) $(GL_DRIVER_INCLUDES) -o $@ -c $<

$(d)/DriverGL.cmx: %.cmx: %.ml $(TYPOGRAPHY_DIR)/Typography.cmxa
	$(ECHO) "[OPT]    $<"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(PACK) -thread -package $(PACK_DRIVER_DriverGL) $(INCLUDES) -I $(<D) $(DRIVERS_INCLUDES) $(GL_DRIVER_INCLUDES) -o $@ -c $<

$(d)/DriverGL.cma: $(d)/FrameBuffer.o $(d)/GlFBO.cmo $(d)/Vec3.cmo $(d)/DriverGL.cmo
	$(ECHO) "[MKLIB] ... -> $@"
	$(Q)$(OCAMLMKLIB) -package $(PACK_DRIVER_DriverGL) -linkpkg -dllpath $(INSTALL_DLLS_DIR) -o $(basename $@) $^

$(d)/DriverGL.cmxa: $(d)/FrameBuffer.o $(d)/GlFBO.cmx $(d)/Vec3.cmx $(d)/DriverGL.cmx
	$(ECHO) "[OMKLIB] ... -> $@"
	$(Q)$(OCAMLMKLIB) -package $(PACK_DRIVER_DriverGL) -linkpkg -dllpath $(INSTALL_DLLS_DIR) -o $(basename $@) $^

$(d)/DriverGL.cmxs: $(d)/FrameBuffer.o $(d)/GlFBO.cmx $(d)/Vec3.cmx $(d)/DriverGL.cmx
	$(ECHO) "[SHARE]    $< -> $@"
	$(Q)$(OCAMLOPT) -package $(PACK_DRIVER_DriverGL) -shared -linkpkg -o $@ $^

CLEAN += $(d)/DriverGL.cma

DISTCLEAN += $(DEPENDS_$(d))

install:install-dll-gl
install-dll-gl:
	install -m 755 src/Drivers/DriverGL/dllDriverGL.so $(DESTDIR)/$(INSTALL_DLLS_DIR)

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
