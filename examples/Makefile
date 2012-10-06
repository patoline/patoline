all:

slides:
	../src/Patoline/patoline --recompile --format FormatSlides --driver Net -I ../src --ml slides.txp
	ocamlfind ocamlopt -package camomile -linkpkg -I ../src/Format -I ../src/Drivers -I ../src/Typography -I ../src/Rbuffer rbuffer.cmxa Typography.cmxa SVG.cmxa Pdf.cmxa Net.cmxa DefaultFormat.cmxa FormatSlides.cmxa -linkpkg  -o 'slides.tmx'  -impl 'slides.tml'
	./slides.tmx --extra-plugins-dir ../src/Drivers

	ocamlfind ocamlopt -o test -package "netstring,netsys,unix,nethttpd,netcgi2,cryptokit" -thread -linkpkg slides.ml


clean:
	rm *.tml *.cm[ioax]
