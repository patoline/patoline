all: doc.pdf

doc.pdf: doc.txp skeleton.ml
	patoline doc.txp

clean:
	rm -rf _patobuild

distclean: clean
	rm -f doc.pdf
