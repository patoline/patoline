include ../Makefile.config
ifdef GL_PACK
PACK+=$(GL_PACK)
NATIVE_EXTRA+=Patoline/PatolineGL
else
  DRIVERS:=$(filter-out Drivers/GL.ml,$(DRIVERS))
endif
ifdef GLGTK_PACK
PACK+=$(GLGTK_PACK)
NATIVE_EXTRA+=Patoline/PatolineGL2
else
  DRIVERS:=$(filter-out Drivers/GL2.ml,$(DRIVERS))
endif

CAMLOPT=ocamlfind ocamlopt $(PACK) -pp $(CPP) -I ../Typography -I ../Rbuffer -I ../Patoline
CAMLMKLIB=ocamlfind ocamlmklib $(PACK) -I ../Typography -I ../Rbuffer -I ../Patoline
CAMLC=ocamlfind ocamlc $(PACK) -pp $(CPP) -I ../Typography -I ../Rbuffer -I ../Patoline


UNAME := $(shell uname)

ifeq ($(UNAME), Linux)
CFLAGS=-fPIC
endif
ifeq ($(UNAME), Darwin)
CFLAGS=-fPIC -framework GLUT -framework OpenGL
endif


Html.cmxa:Html.cmx ../Typography/Typography.cmxa
	$(CAMLOPT) -a -o $@ HtmlFonts.cmx SVG.cmx Html.cmx
SVG.cmxa:SVG.cmx HtmlFonts.cmx ../Typography/Typography.cmxa
	$(CAMLOPT) -a -o $@ HtmlFonts.cmx SVG.cmx
GL.cmxa:GL.cmx FrameBuffer.o GlFBO.cmx ../Typography/Typography.cmxa
	$(CAMLMKLIB) -package netcgi2,nethttpd -o GL FrameBuffer.o GlFBO.cmx GLNet.cmx GL.cmx
Image.cmxa:GL.cmxa Image.cmx ../Typography/Typography.cmxa
	$(CAMLMKLIB) -o Image FrameBuffer.o GL.cmx Language.cmx Image.cmx
Pdf.cmxa:Pdf.cmx
	$(CAMLOPT) -a -o $@ Pdf.cmx
Bin.cmxa:Bin.cmx
	$(CAMLOPT) -a -o $@ Bin.cmx
GL2.cmxa:GL2.cmx
	$(CAMLOPT) -a -o $@ GL2.cmx
Net.cmxa:Net.cmx
	$(CAMLOPT) -a -o $@ Net.cmx

Html.cmx:Html.ml SVG.cmx HtmlFonts.cmx
SVG.cmx:SVG.ml HtmlFonts.cmx
Net.cmx:Net.ml SVG.cmx HtmlFonts.cmx
GL.cmx:GL.ml FrameBuffer.o GLNet.cmx GlFBO.cmx
Image.cmx:Image.ml ../Patoline/Language.cmx
GLNet.cmx:GLNet.ml
	$(CAMLOPT) -package netcgi2,nethttpd -c -o $@ $<

%.cmx:%.ml ../Typography/Typography.cmxa
	$(CAMLOPT) -c -o $@ $<

clean:
	rm -f *.cm[aoix] *.cmxa *.o *.a *~ \#*\#
