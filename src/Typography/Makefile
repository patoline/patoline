include ../Makefile.config

CAMLOPT=ocamlfind ocamlopt -pp $(CPP) -I Fonts -I Output -I Fonts/Sfnt $(PACK)
CAMLC=ocamlfind ocamlc -pp $(CPP) -I Fonts -I Output -I Fonts/Sfnt $(PACK)

SOURCES=Config.cmx new_map.cmx Util.cmx Bezier.cmx Offset.cmx Fonts/FTypes.cmx Fonts/Type2.cmx Fonts/CFFStd.cmx Fonts/CFF.cmx Fonts/Sfnt/unicode_ranges.cmx Fonts/Cmap.cmx Fonts/Layout.cmx Fonts/Opentype.cmx Fonts.cmx Output/OutputCommon.cmx Line.cmx Box.cmx Badness.cmx Language.cmx Break.cmx Document.cmx Complete.cmx Hyphenate.cmx Maths.cmx Output/OutputPaper.cmx Output/OutputDrawing.cmx Diagrams.cmx

VPATH=Fonts:Fonts/Sfnt:Output

all:Typography.cmxa Typography.p.cmxa Typography.cma

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
Fonts.cmi : Util.cmi Util.cmx Fonts.cmx

Fonts/Opentype.cmo:Fonts/FTypes.cmi Fonts/FTypes.cmo Fonts/CFF.cmo Fonts/Cmap.cmo Fonts/Layout.cmo Fonts/Sfnt/unicode_ranges.cmo
Fonts/Opentype.cmx:Fonts/FTypes.cmi Fonts/FTypes.cmx Fonts/CFF.cmx Fonts/Cmap.cmx Fonts/Layout.cmx Fonts/Sfnt/unicode_ranges.cmx
Fonts/Opentype.p.cmx:Fonts/FTypes.cmi Fonts/FTypes.p.cmx Fonts/CFF.p.cmx Fonts/Cmap.p.cmx Fonts/Layout.p.cmx Fonts/Sfnt/unicode_ranges.p.cmx
Fonts/Opentype.cmi:Fonts/Opentype.cmx

Fonts/Layout.cmo : Util.cmi Util.cmo
Fonts/Layout.cmx : Util.cmi Util.cmx
Fonts/Layout.p.cmx : Util.cmi Util.p.cmx
Fonts/Layout.cmi:Fonts/Layout.cmx

Fonts/CFF.cmo:FTypes.cmi Util.cmi Bezier.cmo Type2.cmo CFFStd.cmo
Fonts/CFF.cmx:FTypes.cmx Util.cmx Bezier.cmx Type2.cmx CFFStd.cmx
Fonts/CFF.p.cmx:FTypes.p.cmx Util.p.cmx Bezier.p.cmx Type2.p.cmx CFFStd.p.cmx
Fonts/CFF.cmi:Fonts/CFF.cmx

Fonts/FTypes.cmo:FTypes.cmi Bezier.cmi Util.cmi
Fonts/FTypes.cmx:FTypes.cmi Bezier.cmi Util.cmi
Fonts/FTypes.p.cmx:FTypes.cmi Bezier.cmi Util.cmi
Fonts/FTypes.cmi:Util.cmi Bezier.cmi

Fonts/Sfnt/Sfnt.cmo:Util.cmi Fonts/Opentype.cmi Fonts.cmo Fonts/Sfnt/unicode_ranges.cmo
Fonts/Sfnt/Sfnt.cmx:Util.cmi Fonts/Opentype.cmi Fonts.cmx Fonts/Sfnt/unicode_ranges.cmx
Fonts/Sfnt/Sfnt.p.cmx:Util.cmi Fonts/Opentype.cmi Fonts.p.cmx Fonts/Sfnt/unicode_ranges.p.cmx

Fonts/Cmap.cmo:Util.cmi Util.cmo
Fonts/Cmap.cmx:Util.cmi Util.cmx
Fonts/Cmap.p.cmx:Util.cmi Util.p.cmx
Fonts/Cmap.cmi:Fonts/Cmap.cmx

Fonts/Type2.cmi:Fonts/Type2.cmx
Fonts/CFFStd.cmi:Fonts/CFFStd.cmx

Fonts/Sfnt/unicode_ranges.ml:Fonts/Sfnt/make_unicode_ranges.ml Fonts/Sfnt/unicode
	ocamlc -o Fonts/Sfnt/make_unicode_ranges str.cma Fonts/Sfnt/make_unicode_ranges.ml
	cd Fonts/Sfnt; ./make_unicode_ranges
Fonts/Sfnt/unicode_ranges.cmi:Fonts/Sfnt/unicode_ranges.cmx



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
Output/OutputDrawing.cmi:Output/OutputDrawing.cmx


Break.cmo:Output/OutputCommon.cmi Box.cmi Language.cmo Break.cmi
	$(CAMLC) -c -rectypes -o $@ Break.ml
Break.cmx:Output/OutputCommon.cmi Output/OutputCommon.cmx Box.cmi Language.cmx Break.cmi Box.cmx
	$(CAMLOPT) -for-pack Typography -c -rectypes -o $@ Break.ml
Break.p.cmx:Output/OutputCommon.cmi Output/OutputCommon.cmx Box.cmi Language.p.cmx Break.cmi Box.p.cmx
	$(CAMLOPT) -for-pack Typography -p -c -rectypes -o $@ Break.ml
Break.cmi:Box.cmi Language.p.cmx Break.ml

Hyphenate.cmo:Hyphenate.cmi
Hyphenate.cmx:Hyphenate.cmi new_map.cmx
Hyphenate.p.cmx:Hyphenate.cmi new_map.p.cmx
Hyphenate.cmi:new_map.cmx

Maths.cmo:Util.cmi Output/OutputCommon.cmi Box.cmi Document.cmo
Maths.cmx:Util.cmi Output/OutputCommon.cmi Box.cmi Document.cmx
Maths.cmi:Maths.cmx
Maths.p.cmx:Util.cmi Output/OutputCommon.cmi Box.cmi Document.p.cmx
Maths.p.cmi:Maths.cmx

Document.cmo:Config.cmo Util.cmi Fonts.cmo Output/OutputCommon.cmi Box.cmi Break.cmi Badness.cmi Badness.cmo Break.cmo
Document.cmx:Config.cmx Util.cmi Fonts.cmx Output/OutputCommon.cmi Box.cmi Break.cmi Badness.cmi Badness.cmx Break.cmx
Document.p.cmx:Config.p.cmx Util.cmi Fonts.p.cmx Output/OutputCommon.cmi Box.cmi Break.cmi Badness.cmi Badness.p.cmx Break.p.cmx
Document.cmi:Document.cmx

Badness.cmx:Box.cmx Badness.cmi
Badness.p.cmx:Box.p.cmx Badness.cmi
Badness.cmo:Box.cmi Badness.cmi
Badness.cmi:Box.cmi

Complete.cmx:Box.cmi Break.cmi Document.cmx
Complete.p.cmx:Box.cmi Break.cmi Document.p.cmx
Complete.cmo:Box.cmi Break.cmi Document.cmo
Complete.cmi:Complete.cmx

Offset.cmo:Offset.cmi Bezier.cmo
Offset.cmx:Offset.cmi Bezier.cmx
Offset.p.cmx:Offset.cmi Bezier.cmi Bezier.p.cmx
Offset.cmi:Bezier.cmi Bezier.cmx

Diagrams.cmo:Maths.cmo Maths.cmi
Diagrams.cmx:Maths.cmx Maths.cmi
Diagrams.p.cmx:Maths.p.cmx Maths.p.cmi
Diagrams.cmi:Diagrams.cmx

Box.cmi:Line.cmx Output/OutputCommon.cmi
Box.cmo:Box.cmi
Box.cmx:Box.cmi
Box.p.cmx:Box.cmi

Language.cmo:Line.cmo
Language.cmx:Line.cmx
Language.p.cmx:Line.p.cmx
Language.cmi:Language.cmx

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

NORMALSOURCES=$(filter-out Break%, $(SOURCES))

$(NORMALSOURCES):%.cmx:%.ml
	$(CAMLOPT) -for-pack Typography  -c -o $@ $<

$(NORMALSOURCES:.cmx=.p.cmx):%.p.cmx:%.ml
	$(CAMLOPT) -for-pack Typography -p -c -o $@ $<
	-cp -n $*.p.cmi $*.cmi

%.cmo:%.ml
	$(CAMLC) -c -o $@ $<

%.cmi:%.mli
	$(CAMLC) -c -o $@ $<

clean:
	rm -f *~ \#*\# *.cm[xoia] *.cmxa *.o *.a
	cd Fonts; rm -f *~ \#*\# *.a *.o *.cm[iox]
	cd Fonts/Sfnt; rm -f *~ \#*\# *.a *.o *.cm[iox]
	cd Output; rm -f *~ \#*\# *.a *.o *.cm[iox]
