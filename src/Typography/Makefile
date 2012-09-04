include ../Makefile.config

CAMLOPT=ocamlfind ocamlopt -pp $(CPP) -I Fonts -I Output -I Fonts/Sfnt $(PACK)
CAMLC=ocamlfind ocamlc -pp $(CPP) -I Fonts -I Output -I Fonts/Sfnt $(PACK)

SOURCES=Config.cmx new_map.cmx Util.cmx Bezier.cmx Offset.cmx Fonts/FTypes.cmx Fonts/Type2.cmx Fonts/CFFStd.cmx Fonts/CFF.cmx Fonts/Sfnt/unicode_ranges.cmx Fonts/Cmap.cmx Fonts/Opentype.cmx Fonts.cmx Output/OutputCommon.cmx Line.cmx Box.cmx Badness.cmx Language.cmx Break.cmx Document.cmx Complete.cmx Hyphenate.cmx Maths.cmx Output/OutputPaper.cmx Output/OutputDrawing.cmx Diagrams.cmx

VPATH=Fonts:Fonts/Sfnt:Output

all:Typography.cmxa #Typography.p.cmxa Typography.cma

Typography.cmxa:Typography.cmx
	$(CAMLOPT) -a -o Typography.cmxa Typography.cmx
Typography.cmx: $(SOURCES)
	$(CAMLOPT) -pack -o Typography.cmx $(SOURCES)

Typography.p.cmxa:Typography.p.cmx
	$(CAMLOPT) -a -p -o Typography.p.cmxa Typography.p.cmx
Typography.p.cmx: $(SOURCES:.cmx=.p.cmx)
	$(CAMLOPT) -p -pack -o Typography.p.cmx $(SOURCES:.cmx=.p.cmx)

Typography.cma: Typography.cmo
	$(CAMLC) -a -o Typography.cma Typography.cmo
Typography.cmo: $(SOURCES:.cmx=.cmo)
	$(CAMLC) -pack -o Typography.cmo $(SOURCES:.cmx=.cmo)

Util.cmo : new_map.cmi Config.cmo Util.cmi
Util.cmx : new_map.cmx Config.cmx Util.cmi
Util.p.cmx : new_map.p.cmx Config.p.cmx Util.cmi

Fonts.cmo : Util.cmi Fonts/Opentype.cmo Fonts/FTypes.cmi Fonts/CFF.cmo Bezier.cmi
Fonts.cmx : Util.cmi Fonts/Opentype.cmx Fonts/FTypes.cmx Fonts/CFF.cmx Bezier.cmx
Fonts.p.cmx : Util.cmi Fonts/Opentype.p.cmx Fonts/FTypes.p.cmx Fonts/CFF.p.cmx Bezier.p.cmx
Fonts.cmi : Util.cmi Fonts.cmo

Fonts/Opentype.cmo:Fonts/FTypes.cmi Fonts/FTypes.cmo Fonts/CFF.cmo Cmap.cmo unicode_ranges.cmo
Fonts/Opentype.cmx:Fonts/FTypes.cmi Fonts/FTypes.cmx Fonts/CFF.cmx  Cmap.cmx unicode_ranges.cmx
Fonts/Opentype.p.cmx:Fonts/FTypes.cmi Fonts/FTypes.p.cmx Fonts/CFF.p.cmx Cmap.p.cmx unicode_ranges.p.cmx
Fonts/Opentype.cmi:Fonts/Opentype.cmo

Fonts/CFF.cmo:FTypes.cmi Util.cmi Bezier.cmi Bezier.cmo Type2.cmo CFFStd.cmo
Fonts/CFF.cmx:FTypes.cmi Util.cmi Bezier.cmi Bezier.cmx Type2.cmx CFFStd.cmx
Fonts/CFF.p.cmx:FTypes.cmi Util.cmi Bezier.cmi Bezier.p.cmx Type2.p.cmx CFFStd.p.cmx
Fonts/CFF.cmi:CFF.cmo

Fonts/FTypes.cmo:FTypes.cmi Bezier.cmi Util.cmi
Fonts/FTypes.cmx:FTypes.cmi Bezier.cmi Util.cmi
Fonts/FTypes.p.cmx:FTypes.cmi Bezier.cmi Util.cmi

Fonts/Sfnt/Sfnt.cmo:Util.cmi Fonts/Opentype.cmi Fonts.cmo Fonts/Sfnt/unicode_ranges.cmo
Fonts/Sfnt/Sfnt.cmx:Util.cmi Fonts/Opentype.cmi Fonts.cmx Fonts/Sfnt/unicode_ranges.cmx
Fonts/Sfnt/Sfnt.p.cmx:Util.cmi Fonts/Opentype.cmi Fonts.p.cmx Fonts/Sfnt/unicode_ranges.p.cmx

Fonts/Cmap.cmo:Util.cmi
Fonts/Cmap.cmx:Util.cmi
Fonts/Cmap.p.cmx:Util.cmi

Fonts/Sfnt/unicode_ranges.ml:Fonts/Sfnt/make_unicode_ranges.ml Fonts/Sfnt/unicode
	ocamlc -o Fonts/Sfnt/make_unicode_ranges str.cma Fonts/Sfnt/make_unicode_ranges.ml
	cd Fonts/Sfnt; ./make_unicode_ranges



Output/OutputCommon.cmo:Output/OutputCommon.cmi Fonts.cmo
Output/OutputCommon.cmx:Output/OutputCommon.cmi Fonts.cmx
Output/OutputCommon.p.cmx:Output/OutputCommon.cmi Fonts.p.cmx
Output/OutputCommon.cmi:Fonts.cmx

Output/OutputPaper.cmo:Output/OutputCommon.cmi Output/OutputPaper.cmi Fonts.cmo Box.cmi Document.cmo
Output/OutputPaper.cmx:Output/OutputCommon.cmi Output/OutputPaper.cmi Fonts.cmx Box.cmi Document.cmx
Output/OutputPaper.p.cmx:Output/OutputCommon.cmi Output/OutputPaper.cmi Fonts.p.cmx Box.cmi Document.p.cmx
Output/OutputPaper.cmi:Output/OutputCommon.cmi

Output/OutputDrawing.cmo:Output/OutputCommon.cmi Fonts.cmo Box.cmi Document.cmo
Output/OutputDrawing.cmx:Output/OutputCommon.cmi Fonts.cmx Box.cmi Document.cmx
Output/OutputDrawing.p.cmx:Output/OutputCommon.cmi Fonts.p.cmx Box.cmi Document.p.cmx

Break.cmo:Output/OutputCommon.cmi Box.cmi Language.cmo Break.cmi
	$(CAMLC) -c -rectypes -o $@ Break.ml
Break.cmx:Output/OutputCommon.cmi Box.cmi Language.cmx Break.cmi Box.cmx
	$(CAMLOPT) -for-pack Typography -c -rectypes -o $@ Break.ml
Break.p.cmx:Output/OutputCommon.cmi Box.cmi Language.p.cmx Break.cmi Box.p.cmx
	$(CAMLOPT) -for-pack Typography -p -c -rectypes -o $@ Break.ml
Break.cmi:Box.cmi Language.p.cmx Break.ml

Hyphenate.cmo:Hyphenate.cmi
Hyphenate.cmx:Hyphenate.cmi new_map.cmx
Hyphenate.p.cmx:Hyphenate.cmi new_map.p.cmx
Maths.cmo:Util.cmi Output/OutputCommon.cmi Box.cmi Document.cmo
Maths.cmx:Util.cmi Output/OutputCommon.cmi Box.cmi Document.cmx
Maths.cmi:Maths.cmx
Maths.p.cmx:Util.cmi Output/OutputCommon.cmi Box.cmi Document.p.cmx
Maths.p.cmi:Maths.cmx

Document.cmo:Config.cmo Util.cmi Fonts.cmo Output/OutputCommon.cmi Box.cmi Break.cmi Badness.cmi Badness.cmo Break.cmo
Document.cmx:Config.cmx Util.cmi Fonts.cmx Output/OutputCommon.cmi Box.cmi Break.cmi Badness.cmi Badness.cmx Break.cmx
Document.p.cmx:Config.p.cmx Util.cmi Fonts.p.cmx Output/OutputCommon.cmi Box.cmi Break.cmi Badness.cmi Badness.p.cmx Break.p.cmx

Badness.cmx:Box.cmx Badness.cmi
Badness.p.cmx:Box.p.cmx Badness.cmi
Badness.cmo:Box.cmi Badness.cmi
Badness.cmi:Box.cmi

Complete.cmx:Box.cmi Break.cmi Document.cmx
Complete.p.cmx:Box.cmi Break.cmi Document.p.cmx
Complete.cmo:Box.cmi Break.cmi Document.cmo

Offset.cmo:Offset.cmi Bezier.cmo
Offset.cmx:Offset.cmi Bezier.cmx
Offset.p.cmx:Offset.cmi Bezier.p.cmx

Diagrams.cmo:Maths.cmo Maths.cmi
Diagrams.cmx:Maths.cmx Maths.cmi
Diagrams.p.cmx:Maths.p.cmx Maths.p.cmi

Box.cmi:Line.cmx Output/OutputCommon.cmi
Box.cmo:Box.cmi
Box.cmx:Box.cmi
Box.p.cmx:Box.cmi

Language.cmo:Line.cmo
Language.cmx:Line.cmx
Language.p.cmx:Line.p.cmx
Util.cmo:Util.cmi new_map.cmi
Util.cmx:Util.cmi new_map.cmi
Util.p.cmx:Util.cmi new_map.cmi
Util.cmi:new_map.cmi
new_map.cmo: new_map.cmi
new_map.cmx: new_map.cmi
new_map.p.cmx: new_map.cmi
Bezier.cmo:Bezier.cmi
Bezier.cmx:Bezier.cmi
Bezier.p.cmx:Bezier.cmi

%.cmx:%.ml
	$(CAMLOPT) -for-pack Typography  -c -o $@ $<
%.p.cmx:%.ml
	$(CAMLOPT) -p -for-pack Typography  -c -o $@ $*.ml
%.cmo:%.ml
	$(CAMLC) -c -o $@ $<
%.cmi:%.mli
	$(CAMLC) -c -o $@ $<

clean:
	rm -f *~ \#*\# *.cm[xoia] *.cmxa *.o *.a
	cd Fonts; rm -f *.o *.cm[iox]
	cd Fonts/Sfnt; rm -f *.o *.cm[iox]
	cd Output; rm -f *.o *.cm[iox]
