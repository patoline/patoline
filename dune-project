(lang dune 2.7)
(generate_opam_files true)

(name patoline)
(authors
 "Pierre-Étienne Meunier"
 "Christophe Raffalli <christophe@raffalli.eu>"
 "Rodolphe Lepigre <lepigre@mpi-sws.org>"
 "Tom Hirschowitz"
 "Florian Hatat <patoline@hatat.me>")
(maintainers
 "Rodolphe Lepigre <lepigre@mpi-sws.org>")
(source (github patoline/patoline))
(homepage "https://patoline.github.io")
(license "GPL-2.0")

(package
 (name patoline)
 (synopsis "Patoline typesetting system and libraries")
 (description
  "Patoline is a collection of typesetting libraries designed to be combined
   into a variety of possible applications. Its primary goal is not to offer
   the definitive answer to all typesetting problems, but rather to write the
   painful and boring parts of the job, so that interesting tools could be
   written easily. Obviously, a related (but much smaller) project is the
   Patoline compiler, which compiles a mixed Wiki/LaTeX/Ocaml syntax into a
   variety of output formats, including traditional PDF files, but also web
   servers that deliver dynamic contents.")
 (depends
  (ocaml (>= 4.03.0))
  (dune :build)
  ocamlfind
  (earley (= 2.0.0))
  camlzip
  sqlite3
  (imagelib (= 20180522)))
 (depopts
  lablgl
  cairo2
  cryptokit))
