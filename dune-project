(lang dune 3.0)
(generate_opam_files true)

(name patoline)
(version 853d82f-dirty)
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
 (tags ("latex" "typesetting" "documentation" "typography"))
 (depends
  (ocaml
   (and
    (>= 5.0.0)
    (< 5.5.0)))
  ocamlfind
  (earley (= 3.0.0))
  (stdlib-shims (>= 0.1.0))
  (camlzip (>= 1.0))
  (sqlite3 (>= 5.0))
  (imagelib (>= 20180522))
  uucp
  uunf)
 (depopts
  cairo2
  cryptokit))
