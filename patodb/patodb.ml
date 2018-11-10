(*
  Copyright Florian Hatat, Tom Hirschowitz, Pierre Hyvernat,
  Pierre-Etienne Meunier, Christophe Raffalli, Guillaume Theyssier 2012.

  This file is part of Patoline.

  Patoline is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Patoline is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Patoline.  If not, see <http://www.gnu.org/licenses/>.
*)

(**
Managing authentication and associated data for interactive sessions.

Patoline documents can be interactive, allowing for example an audience
to connect remotely to a presentation and interact with it, possibly
changing some parts of it (such as answering a poll, results being
updated on-screen immediately).

This module provides tools for managing user sessions and a storage
space for key/value pairs associated to each session.
*)

open Patutil
open Util

(** {1 Encoding and decoding data in the database} *)

type 'a data = {
  name : string;
  init : 'a;
  read : unit -> 'a;
  write : 'a -> unit;
  reset : unit -> unit;
  distribution : ?group:string -> unit -> int * ('a * int) list;
  (* the first int is a grand total of everybody (in the group, if given), even those having no
     value for this specific data. total is 1 even if it should be 0 !!! *)
}

(** Exception raised when actually trying to use the {!val:dummyData}
below. *)
exception DummyData

(** Dummy inhabitant of type {!type:data}, which can be used to
initialize data structure, but whose content should never be used
(calling any member function raises {!exception:DummyData}). *)
let dummyData = {
  name = "Dummy data ! Do not use";
  init = "Dummy data ! Do not use";
  read = (fun _ -> raise DummyData) ;
  write = (fun _ -> raise DummyData) ;
  reset = (fun _ -> raise DummyData) ;
  distribution = (fun ?group _ -> raise DummyData) ;
}

type 'a coding = {
  encode : 'a -> string;
  (** Serializing some value of type ['a] to a string which can be
  stored in database. *)

  decode : string -> 'a;
  (** Parsing a string from the database and creating a value of type
  ['a]. This should be the inverse of [encode]. *)
}

(** Default polymorphic coding, using OCaml {!module:Marshal} module,
escaped using base64. *)
let default_coding = {
  encode = (fun v -> Base64.encode (Marshal.to_string v []));
  decode = (fun s -> Marshal.from_string (Base64.decode s) 0);
}

(** Strings coding, which ony uses base64 escaping. *)
let string_coding = {
  encode = (fun v -> Base64.encode v);
  decode = (fun s -> Base64.decode s);
}

(** Coding boolean as strings ["true"] and ["false"]. Any value other
than ["true"] in the database is decoded as [false]. *)
let bool_coding = {
  encode = string_of_bool;
  decode = (fun s -> s = "true");
}

(** {1 Connection to databases } *)

(** Interface to be implemented by database managers *)
module type DbInterface = sig
  type dbinfo
  (** Connection information needed to connect to a database *)

  type database
  (** Handler to a database connection *)

  val connect : dbinfo -> database
  (** Connecting to a database with the given database information *)

  val init_db : database -> string -> unit
  (** Create the table structure in the database. Data are stored in a
  table whose name is given by the second argument. *)

  val create_data : database -> string -> ?log:bool -> ?visibility:visibility -> 'a coding -> string -> 'a -> 'a data
  (** Blah *)

  val disconnect : database -> unit
  (** Disconnecting from the database *)
end

(** Generic type for database instances *)
module type DbInstance = sig
  module Interface : DbInterface
  val db : Interface.database
end

type database = (module DbInstance)
type db = {
  db : unit -> database;
  create_data : 'a.( ?log:bool -> ?visibility:visibility -> 'a coding -> string -> 'a -> 'a data);
  disconnect : unit -> unit;
}

let init_db (type a) (module Dbms : DbInterface with type dbinfo = a) table_name db_info =
  let dbd = Dbms.connect db_info in
  Dbms.init_db dbd table_name;
  let db = (module struct
    module Interface = Dbms
    let db = dbd
  end : DbInstance) in
  {
    db = (fun () -> db);
    create_data = (fun ?(log=false) ?(visibility=Private) coding name
    vinit -> Dbms.create_data dbd table_name ~log ~visibility coding name vinit);
    disconnect = fun () -> Dbms.disconnect dbd;
  }

(** {1 Hooks} *)

(** List of functions called when the interactive session starts *)
let interaction_start_hook = ref ([]: (unit -> unit) list)

(** Hooks called when some data is being read from a database *)
let read_hook : (string -> visibility -> unit) list ref = ref []

(** Hooks called when some data is being written to a database *)
let write_hook : (string -> visibility -> unit) list ref = ref []

let record hook f a =
  let l = ref [] in
  let r = fun s v ->
    l := (s, v) :: !l in
  hook := r :: !hook;
  let pop () = match !hook with
    | r' :: l when r == r' -> hook := l
    | _ -> assert false
  in
  try
    let res = f a in
    pop (); (res, !l)
  with e -> pop (); raise e

let stop_record hook f a =
  let save = !hook in
  hook:=[];
  try
    let res = f a in
    hook := save;
    res
  with e ->
    hook := save; raise e

let record_read f a = record read_hook f a
let record_write f a = snd (record write_hook f a)

let stop_record_read f a = stop_record read_hook f a
let stop_record_write f a = stop_record write_hook f a

(** Run all hooks in {!val:interaction_start_hook}. *)
let do_interaction_start_hook () =
  List.iter (fun f -> f ()) (!interaction_start_hook)

(** Run all hooks in {!val:read_hook}. *)
let do_record_read  = fun d v -> List.iter (fun f -> f d.name v) !read_hook

(** Run all hooks in {!val:write_hook}. *)
let do_record_write = fun d v -> List.iter (fun f -> f d.name v) !write_hook

(** {1 Utility functions} *)

let sessid = ref (None: (string * string * (string * string) list) option)
(* the first string is the login *)
(* the second string is the group, "guest" is reserved for guest *)
(* the list of pairs of strings are the "friends login and group" *)

let secret = ref ""

let make_sessid () =
  let size = 32 in
  let str = Bytes.create size in
  for i = 0 to size - 1 do
    let c = Random.int 62 in
    let d =
      if c < 26 then Char.chr (c + Char.code 'a')
      else if c < 52 then Char.chr (c - 26 + Char.code 'A')
      else Char.chr (c - 52 + Char.code '0')
    in
    Bytes.set str i d;
  done;
  let str = Bytes.to_string str in
  sessid := Some (str, "guest", []);
  str

let friends_from_string str =
  try
    List.map (fun s ->
              match
                Str.split (Str.regexp_string ",") s with
                [s;g] -> s,g
              | _ -> raise Exit)
             (Str.split (Str.regexp_string "+") str)
  with Exit -> []

let friends_to_string l =
  let str = String.concat "+" (List.map (fun (s,g) -> s ^ "," ^ g) l) in
  if str <> "" then "+" ^ str else str
