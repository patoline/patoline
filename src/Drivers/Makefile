include ../Makefile.config
ifdef GL_PACK
PACK+=$(GL_PACK)
NATIVE_EXTRA+=Patoline/PatolineGL
else
  DRIVERS:=$(filter-out Drivers/GL.ml,$(DRIVERS))
endif

CAMLOPT=ocamlfind ocamlopt $(PACK) -pp "cpp -w" -I ../Typography -I ../Rbuffer
CAMLC=ocamlfind ocamlc $(PACK) -pp "cpp -w" -I ../Typography -I ../Rbuffer


Html.cmxa:Html.cmx
	$(CAMLOPT) -a -o $@ HtmlFonts.cmx SVG.cmx Html.cmx
SVG.cmxa:SVG.cmx HtmlFonts.cmx
	$(CAMLOPT) -a -o $@ HtmlFonts.cmx SVG.cmx

Html.cmx:SVG.cmx HtmlFonts.cmx
SVG.cmx:HtmlFonts.cmx

%.cmxa:%.cmx ../Typography/Typography.cmxa ../Typography/Typography.cmi
	$(CAMLOPT) -a -o $@ $<

%.cmx:%.ml ../Typography/Typography.cmxa ../Typography/Typography.cmi
	$(CAMLOPT) -c -o $@ $<
