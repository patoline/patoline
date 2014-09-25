(* #FORMAT FormatArticle *)
==============================================================================
  Getting started writing parsers and syntax extensions using ##DeCaP## and
  ##pa_ocaml##
------------------------------------------------------------------------------
  Rodolphe Lepigre & Christophe Raffalli
------------------------------------------------------------------------------
  Lama, UMR 5127 CNRS, Université Savoie Mont-Blanc
==============================================================================

\linesBefore(2) 
\Caml(
let _ = TableOfContents.do_begin_env()
let _ = TableOfContents.do_end_env()
) \linesAfter(2)

//Delimited Continuation Parser// (or ##DeCaP##) is a small parser combinator
library written in ##OCaml##. Unlike most combinator libraries, its
performance is close to that of ##ocamlyacc##, as it relies heavily on
//continuation passing style// (CPS). ##DeCaP## provides a notion of //blank
function// which is used to discard parts of the input that should be ignored
(comments for example). Ambiguous grammars can be handled either by returning
the list of all possible parse trees or by raising an error in case of
ambiguity.

##DeCaP## has been used to write a full-featured ##OCaml## parser called
##pa_ocaml##, which operates at more than twice the speed of ##camlp4##, and
is only five times slower than the original ##OCaml## parser (written with
##ocamlyacc##). When used in conjunction with ##DeCaP##, ##pa_ocaml## offers
a simple and integrated way to write parsers, and even syntax extensions for
the ##OCaml## language. It is intended to be simpler to use than ##camlp4##,
and does not require the use of a lexer, which gives the user more freedom to
write syntax extensions. A quotation and anti-quotation mechanism similar to
that of ##camlp4## is provided, which greatly simplifies the writing of syntax
extensions.

This document is not intended as a full documentation of ##DeCaP## and
##pa_ocaml##, but rather as a quick description of their main features,
illustrated with simple examples. It should contain enough material to get
started writing parsers and syntax extensions, while providing an overall
idea of the working principles of ##DeCaP## and ##pa_ocaml##.

Even though ##DeCaP## can be used directly, a ##pa_ocaml## syntax extension
called ##pa_parser## allows the user to write parsers using a BNF-like
syntax. There is, however, one main restriction: //left recursive// grammars
are forbidden.

\Include{Internals}
\Include{Parsers}
\Include{Extension}
