This is (* this is a comment *)
a paragraph.(* This is
  another
    comment *)

(* This is yet another comment *)This is another(* Again a
comment *) paragraph.

Yet another (* ... (* This is a comment in a comment. *)
... *) paragraph.

And one more paragraph, but nothing more.

(*
Arbitrary string litterals are allowed in comments.
Even those containing the substring "*)" or "*(".
New OCaml 4.02 string literals are also allowed.
Even those containing the substring {foo|*)|foo} or {bar|(*|bar}.
Of course, unclosed strings are also allowed:
"this is an unclosed string...
{|this is also an unclosed string...
*)

(* A lonely comment at the end... *)
