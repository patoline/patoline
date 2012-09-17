exception File_not_found of (string*string list)
let findPath f path=
  let rec findPath f=function
      []->raise (File_not_found (f,path))
    | h::s ->
      if Sys.file_exists (Filename.concat h f) then
	Filename.concat h f
      else
	findPath f s
  in
    findPath f path

module Str_ = struct
  type t = string
  let compare = compare
end

module StrSet = Set.Make(Str_)
module StrMap=Map.Make(Str_)
module IntMap=Map.Make(struct type t=int let compare=compare end)
