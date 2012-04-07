open Config

exception File_not_found of (string*string list)
let rec findPath f path=function
    []->raise (File_not_found (f,path))
  | h::s when Sys.file_exists (Filename.concat h f)->(Filename.concat h f)
  | h::s -> (findPath f path s)

let findFont f=let path= "."::(!fontsdir) in findPath f path path
let findGrammar f=let path= "." :: (!grammarsdir) in findPath f path path
let findHyph f=let path="."::(!hyphendir) in findPath f path path

let pt_of_mm x=(72.*.x)/.25.4
let mm_of_pt x=(25.4*.x)/.72.

let a4=(210.,297.)
let phi=(1.+.(sqrt 5.))/.2.
