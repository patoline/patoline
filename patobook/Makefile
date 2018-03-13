all: patobook.pdf

patobook.pdf: $(wildcard *.txp)
	patoline patobook.txp

clean:
	patoline --clean
	rm -f *~

distclean: clean
	rm -f patobook.pdf
