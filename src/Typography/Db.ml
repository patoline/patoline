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

open Util
open Mysql

type dbinfo =
  Mysql of db
| Sqlite of string

type database =
  MysqlDb of dbd
(*| SqliteDb of ...*)

type db = {
  db : unit -> database;
  create_data : 'a.( ?global:bool -> string -> 'a -> (unit -> 'a) * ('a -> unit));
}

let interaction_start_hook = ref ([]: (unit -> unit) list)

let sessid = ref (None: string option)

let secret = ref ""

let init_db table_name db_info = 
  match db_info with
    Sqlite filename -> 
      (* FIXME: implement Sqlite support, with concurrent access, must manage the Busy error *) 
      Printf.eprintf "Sqlite support not yet implemented\n";
      exit 1
  | Mysql db_info ->
    let dbptr = ref None in
    
    let db () = 
      match !dbptr with
	None -> let db = connect db_info in Printf.eprintf "Reconnected to db\n%!"; dbptr := Some db; db
      | Some db -> db
    in

    interaction_start_hook := (fun () ->
      match !dbptr with None -> () | Some db -> disconnect db; dbptr := None; Printf.eprintf "Disconnected from db\n%!")::!interaction_start_hook;

    (let sql = Printf.sprintf "CREATE TABLE IF NOT EXISTS `%s` (
      `sessid` char(33), `key` varchar(32), `value` text);" table_name in
     let _r = exec (db ()) sql in
     match errmsg (db ()) with
     | None -> () 
     | Some err -> Printf.eprintf "DB Error: %s\n%!" err);

    { db = (fun () -> MysqlDb (db ()));
      create_data = fun ?(global=false) name vinit ->
	let v = base64_encode (Marshal.to_string vinit []) in 
	let tbl = Hashtbl.create 7 in
	let sessid () = if global then "shared_variable" else match !sessid with None -> "not ready" | Some s -> s in 
	let init () = 
	  let sessid = sessid () in
	  if not (Hashtbl.mem tbl sessid) then (
            let sql = Printf.sprintf "SELECT count(*) FROM `%s` WHERE `sessid` = '%s' AND `key` = '%s';" table_name sessid name in
            let r = exec (db ()) sql in
            (match errmsg (db ()), fetch r with	
            | None, Some row -> 
	      let count = match row.(0) with None -> 0 | Some n -> int_of_string n in
	      (match count with
		0 -> 
		  let sql = Printf.sprintf "INSERT INTO `%s` (`sessid`, `key`, `value`) VALUES ('%s','%s','%s');" table_name sessid name v in		    
		  let _r = exec (db ()) sql in	      
		  (match errmsg (db ()) with
		  | None -> () 
		  | Some err -> raise (Failure err))
              | 1 -> ()
      	      | _ -> raise (Failure "SQL duplicate data in base"))
            | Some err, _ -> raise (Failure err)
            | _ -> raise (Failure "SQL unexpected problem")));
          sessid
	in
	let read () =
          let sessid = init () in
          let sql = Printf.sprintf "SELECT `value` FROM `%s` WHERE `sessid` = '%s' AND `key` = '%s';" table_name sessid name in
          let r = exec (db ()) sql in
          try match errmsg (db ()), fetch r with
    	  | None, Some row -> (match row.(0) with None -> vinit | Some n -> Marshal.from_string (base64_decode n) 0)
          | Some err, _ -> Printf.eprintf "DB Error: %s\n%!" err; vinit
          | _ -> assert false   
          with Mysql.Error err ->
            Printf.eprintf "Mysql Error: %s\n%!" err; vinit
	in
	let write v =
          let sessid = init () in
          let v = base64_encode (Marshal.to_string v []) in 
          let sql = Printf.sprintf "UPDATE `%s` SET `value`='%s' WHERE `key` = '%s' AND `sessid` = '%s';" table_name v name sessid in
          let _r = exec (db ()) sql in
          match errmsg (db ()) with
          | None -> () 
          | Some err -> Printf.eprintf "DB Error: %s\n%!" err
	in 
	try 
	  ignore (init ()); (read, write)
	with Failure err | Mysql.Error err ->
          Printf.eprintf "Failed to create data %s, SQL error is %s\n%!" name err;
	  let r = ref vinit in
	  (fun () -> !r), (fun v -> r := v)}


let make_sessid () = 
  let size = 32 in
  let str = String.create size in
  for i = 0 to size - 1 do
    let c = Random.int 62 in
    let d = 
      if c < 26 then Char.chr (c + Char.code 'a')
      else if c < 52 then Char.chr (c - 26 + Char.code 'A')
      else Char.chr (c - 52 + Char.code '0')
    in
    str.[i] <- d;
  done;
  sessid:=Some str;
  str
