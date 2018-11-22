(** Selecting fonts *)
type slant = Roman | Italic
type weight =
  | Regular
  | Bold
  | Black
type t = { family : string; slant : slant; weight : weight }
