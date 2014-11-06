type t = { filename    : string
         ; db          : Sqlite3.db
         ; insert_stmt : Sqlite3.stmt
         ; delete_stmt : Sqlite3.stmt
         ; select_stmt : Sqlite3.stmt
         }

(**
[new_map file] creates a new map file named [file]. If [file] already exists,
then exception [Invalid_arg] is raised.
*)
let new_map : string -> unit = fun file ->
  if Sys.file_exists file then
    invalid_arg "PermanentMap.open_map: map file already exists...";
  (* Opening database and creating table *)
  let db = Sqlite3.db_open file in
  let sql = "CREATE TABLE map (key INTEGER PRIMARY KEY, val BLOB)" in
  let stmt = Sqlite3.prepare db sql in
  if Sqlite3.step stmt != Sqlite3.Rc.DONE then assert false;
  if Sqlite3.finalize stmt != Sqlite3.Rc.OK then assert false;
  (* Closing database *)
  if not (Sqlite3.db_close db) then assert false else ()

(**
[open_map file] opens a map file previously created using [new_map]. If [file]
does not exist, the exception [Invalid_arg] is raised. The value returned is
a map (i.e. an object of type [t]).
*)
let open_map : string -> t = fun file ->
  if not (Sys.file_exists file) then
    invalid_arg "PermanentMap.open_map: map file does not exist...";
  (* Opening database *)
  let db = Sqlite3.db_open file in
  (* Creating statements *)
  let insert_sql = "INSERT INTO map VALUES (?, ?)" in
  let insert_stmt = Sqlite3.prepare db insert_sql in
  let delete_sql = "DELETE FROM map WHERE key = ?" in
  let delete_stmt = Sqlite3.prepare db delete_sql in
  let select_sql = "SELECT val FROM map WHERE key = ?" in
  let select_stmt = Sqlite3.prepare db select_sql in
  { filename    = file
  ; db          = db
  ; insert_stmt = insert_stmt
  ; delete_stmt = delete_stmt
  ; select_stmt = select_stmt }

(**
[close_map m] closes a map previously opened with [open_map]. This operation
does not seem to be necessary, but it is probably a good idea to use it
anyway.
*)
let close_map : t -> unit = fun m ->
  if Sqlite3.finalize m.insert_stmt != Sqlite3.Rc.OK then assert false;
  if Sqlite3.finalize m.delete_stmt != Sqlite3.Rc.OK then assert false;
  if Sqlite3.finalize m.select_stmt != Sqlite3.Rc.OK then assert false;
  if not (Sqlite3.db_close m.db) then assert false else ()

(**
[add m k v] adds the value [v] with the key [k] to the map [m]. It is the
user's responsibility not to add a value with an already existing key. Doing
so will lead to an unspecified behaviour. If you are not sure whether a key is
mapped or not, use the function [set].
*)
let add : t -> int -> 'a -> unit = fun m k v ->
  let k64 = Int64.of_int k in
  let sv = Marshal.to_string v [] in
  let _ = Sqlite3.bind m.insert_stmt 1 (Sqlite3.Data.INT k64) in
  let _ = Sqlite3.bind m.insert_stmt 2 (Sqlite3.Data.BLOB sv) in
  let res = Sqlite3.step m.insert_stmt in
  let _ = Sqlite3.reset m.insert_stmt in
  if res != Sqlite3.Rc.DONE then assert false

(**
[add_many m l] is equivalent to [List.iter (fun (k, v) -> add m k v) l] but it
is a lot faster. On should prefer this function over add when adding more than
a few entries. One should consider using the [compact] function after adding
many entries as it might significantly reduce the size of the map file.
*)
let add_many : t -> (int * 'a) list -> unit = fun m l ->
  (* begin a transaction *)
  let r = Sqlite3.exec m.db "BEGIN" in
  if r <> Sqlite3.Rc.OK then assert false;
  (* perform all adds *)
  List.iter (fun (k, v) -> add m k v) l;
  (* end the transaction *)
  let r = Sqlite3.exec m.db "COMMIT" in
  if r <> Sqlite3.Rc.OK then assert false

(**
[del m k] deletes the entry with the key [k] from the map [m] if it exists.
*)
let del : t -> int -> unit = fun m k ->
  let k64 = Int64.of_int k in
  let _ = Sqlite3.bind m.delete_stmt 1 (Sqlite3.Data.INT k64) in
  let res = Sqlite3.step m.delete_stmt in
  let _ = Sqlite3.reset m.delete_stmt in
  if res != Sqlite3.Rc.DONE then assert false

(**
[set m k v] replaces the entry with the key [k] from the map [m] with the
value [v]. This function behaves as [add] if no value with the key [k] is in
the map.
*)
let set : t -> int -> 'a -> unit = fun m k v ->
  del m k;
  add m k v

(**
[get m k] retreave the value associated to the key [k] in the map [m]. If no
value is mapped to [k], the exception [Not_found] is raised.
*)
let get : t -> int -> 'a = fun m k ->
  let k64 = Int64.of_int k in
  let _ = Sqlite3.bind m.select_stmt 1 (Sqlite3.Data.INT k64) in
  let res = Sqlite3.step m.select_stmt in
  if res != Sqlite3.Rc.ROW then
    (if res = Sqlite3.Rc.DONE then raise Not_found else assert false);
  let e = Sqlite3.column m.select_stmt 0 in
  let _ = Sqlite3.reset m.select_stmt in
  match e with
  | Sqlite3.Data.BLOB b -> Marshal.from_string b 0
  | _                   -> assert false

(**
[compact m] reduces the size of the given map if possible.
*)
let compact : t -> unit = fun m ->
  let r = Sqlite3.exec m.db "VACUUM" in
  if r <> Sqlite3.Rc.OK then assert false
