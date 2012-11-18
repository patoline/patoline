include ../Makefile.config

ifdef PACK_DRIVER_GL
PACK:=$(PACK) -package $(PACK_DRIVER_GL)
NATIVE_EXTRA+=Patoline/PatolineGL
endif
ifdef PACK_DRIVER_GL2
PACK:=$(PACK) -package $(PACK_DRIVER_GL2)
NATIVE_EXTRA+=Patoline/PatolineGL2
endif

CAMLOPT=ocamlfind ocamlopt $(PACK) -pp $(CPP) -I ../Typography/_build -I ../Rbuffer -I ../Patoline
CAMLMKLIB=ocamlfind ocamlmklib $(PACK) -I ../Typography/_build -I ../Rbuffer -I ../Patoline
CAMLC=ocamlfind ocamlc $(PACK) -pp $(CPP) -I ../Typography/_build -I ../Rbuffer -I ../Patoline


UNAME := $(shell uname)

ifeq ($(UNAME), Linux)
CFLAGS=-fPIC
endif
ifeq ($(UNAME), Darwin)
CFLAGS=-fPIC -framework GLUT -framework OpenGL
endif


Html.cmxa:Html.cmx
	$(CAMLOPT) -a -o $@ HtmlFonts.cmx SVG.cmx Html.cmx
SVG.cmxa:SVG.cmx HtmlFonts.cmx
	$(CAMLOPT) -a -o $@ HtmlFonts.cmx SVG.cmx
GL.cmxa:GL.cmx FrameBuffer.o GlFBO.cmx
	$(CAMLMKLIB) -package str,netcgi2,nethttpd -o GL FrameBuffer.o GlFBO.cmx GLNet.cmx Vec3.cmx GL.cmx 
Image.cmxa:GL.cmx Image.cmx
	$(CAMLMKLIB) -o Image FrameBuffer.o GL.cmx Language.cmx Image.cmx
Pdf.cmxa:Pdf.cmx
	$(CAMLOPT) -a -o $@ Pdf.cmx
Bin.cmxa:Bin.cmx
	$(CAMLOPT) -a -o $@ Bin.cmx
GL2.cmxa:GL2.cmx
	$(CAMLOPT) -a -o $@ GL2.cmx
Net.cmxa:Net.cmx
	$(CAMLOPT) -a -o $@ Net.cmx

DriverCairo.cmxa:DriverCairo.cmx
	$(CAMLOPT) -a -o $@ DriverCairo.cmx

DriverCairo.cmx:DriverCairo.ml
	$(CAMLOPT) -package cairo -c -o $@ DriverCairo.ml

Html.cmx:Html.ml SVG.cmx HtmlFonts.cmx
SVG.cmx:SVG.ml HtmlFonts.cmx
Net.cmx:Net.ml SVG.cmx HtmlFonts.cmx
GL.cmx:GL.ml FrameBuffer.o GLNet.cmx GlFBO.cmx Vec3.cmx
	$(CAMLOPT) -package netcgi2,nethttpd -package $(PACK_DRIVER_GL) -c -o $@ $<
Image.cmx:Image.ml GL.cmx ../Patoline/Language.cmx
GLNet.cmx:GLNet.ml
	$(CAMLOPT) -package netcgi2,nethttpd,$(PACK_DRIVER_GL) -c -o $@ $<

%.cmx:%.ml
	$(CAMLOPT) -c -o $@ $<

clean:
	rm -f *.cm[aoix] *.cmxa *.o *.a *~ \#*\#
