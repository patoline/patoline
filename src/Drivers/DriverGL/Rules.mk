# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

GL_DRIVER_INCLUDES:=-I $(RBUFFER_DIR) -I $(TYPOGRAPHY_DIR) -I $(d)

SRC_$(d):=$(wildcard $(d)/*.ml)
DEPENDS_$(d) := $(addsuffix .depends,$(SRC_$(d)))
-include $(DEPENDS_$(d))

UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
FPIC_FLAGS=-fPIC
endif
ifeq ($(UNAME), Darwin)
FPIC_FLAGS=-I$(shell ocamlc -where) -fPIC # -framework GLUT -framework OpenGL
endif

$(d)/FrameBuffer.o: $(d)/FrameBuffer.c
	$(ECHO) "[CC]     $<"
	$(Q)$(CC) $(FPIC_FLAGS) $(CFLAGS) -c $< -o $@

#$(d)/GLNet.cmx: %.cmx: %.ml
#	$(ECHO) "[OPT]    $<"
#	$(Q)$(OCAMLOPT) $(OFLAGS) $(PACK) -package $(PACK_DRIVER_DriverGL) $(INCLUDES) $(DRIVERS_INCLUDES) $(GL_DRIVER_INCLUDES) -o $@ -c $<

$(d)/DriverGL.cmx: %.cmx: %.ml $(TYPOGRAPHY_DIR)/Typography.cmxa
	$(ECHO) "[OPT]    $<"
	$(Q)$(OCAMLOPT) $(OFLAGS) $(PACK) -package $(PACK_DRIVER_DriverGL) $(INCLUDES) $(DRIVERS_INCLUDES) $(GL_DRIVER_INCLUDES) -o $@ -c $<

$(d)/DriverGL.cmxa: $(d)/FrameBuffer.o $(d)/GlFBO.cmx $(d)/Vec3.cmx $(d)/DriverGL.cmx
	$(ECHO) "[OMKLIB] ... -> $@"
	$(Q)$(OCAMLMKLIB) -package $(PACK_DRIVER_DriverGL) -o $(basename $@) $^

$(d)/DriverGL.cmxs: $(d)/FrameBuffer.o $(d)/GlFBO.cmx $(d)/Vec3.cmx $(d)/DriverGL.cmx
	$(ECHO) "[OPT]    $< -> $@"
	$(Q)$(OCAMLOPT) -package lablgl,lablgl.glut -shared -linkpkg -o $@ $^

DISTCLEAN += $(DEPENDS_$(d))

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))
