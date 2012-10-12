val fields : (string * string) list
val field_num : string -> int
val create : Sqlite3.db -> unit
exception Auteur of string
exception Notfound of string
exception Publisher of string
exception Bib_error of string
val set_auteurs : Sqlite3.db -> int64 -> int64 list -> unit
val insert_author : Sqlite3.db -> string -> int64
val insert_publisher : Sqlite3.db -> string -> int64
val make_name : string -> string * string
val intercalate : 'a -> 'a list -> 'a list
val dbCite :
  Sqlite3.db ->
  bool -> string -> (int*'a Typography.Document.content list) list
val bibitem :
  string -> string
   -> (int*'a Typography.Document.content list) list
val author:string->string->string list
val authorCite : string -> string -> string
val cite : string -> Typography.Document.user Typography.Document.content list
module type Format=sig
  val parameters :
    Typography.Document.user Typography.Document.environment ->
    Typography.Document.user Typography.Box.box array array ->
    Typography.Box.drawingBox array ->
    Typography.Line.parameters ->
    Typography.Break.figurePosition Typography.Util.IntMap.t ->
    Typography.Line.line Typography.Document.TS.UMap.t ->
    Typography.Line.line -> Typography.Line.line -> Typography.Line.parameters
end
module TheBibliography : functor (F:Format)-> functor (D : Typography.Document.DocumentStructure) -> sig end
