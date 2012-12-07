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
val bibfile : string -> unit
val dbCite :
  Sqlite3.db ->
  bool -> string -> (int*Typography.Document.content list) list
val bibitem :
  string -> string
   -> (int*Typography.Document.content list) list
val author:string->Typography.Document.content list
val authorCite : string -> string -> string
val cite : string -> Typography.Document.content list
module TheBibliography : functor (D : Typography.Document.DocumentStructure) -> sig end
