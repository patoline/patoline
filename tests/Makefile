.PHONY:articles

all:articles

articles:
	../src/Patoline/patoline --recompile -I ../src --extra-hyph-dir ../Hyphenation --extra-fonts-dir ../Fonts --format FormatArticle min.txp
	../src/Patoline/patoline --recompile -I ../src --extra-hyph-dir ../Hyphenation --extra-fonts-dir ../Fonts test_delimiters.txp
	../src/Patoline/patoline --recompile -I ../src --extra-hyph-dir ../Hyphenation --extra-fonts-dir ../Fonts test_small_fraction.txp
	../src/Patoline/patoline --recompile -I ../src --extra-hyph-dir ../Hyphenation --extra-fonts-dir ../Fonts test_virgule.txp
	../src/Patoline/patoline --recompile -I ../src --extra-hyph-dir ../Hyphenation --extra-fonts-dir ../Fonts test_break_badness.txp
	../src/Patoline/patoline --recompile -I ../src --extra-hyph-dir ../Hyphenation --extra-fonts-dir ../Fonts test_distance.txp
