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
#ifdef MYSQL
open Mysql
#endif

type dbinfo =
| Memory
#ifdef MYSQL
| Mysql of Mysql.db
#endif
#ifdef SQLITE3
| Sqlite of string
#endif

type database =
| MemoryDb
#ifdef MYSQL
| MysqlDb of dbd
#endif
#ifdef SQLITE3
(*| SqliteDb of ...*)
#endif

type 'a data = {
  read : unit -> 'a;
  write : 'a -> unit;
  reset : unit -> unit;
  distribution : ?group:string -> unit -> int * ('a * int) list;
  (* the first int is a grand total of everybody (in the group, if given), even those having no
     value for this specific data. total is 1 even if it should be 0 !!! *)
}

exception DummyData

let dummyData = {
  read = (fun _ -> raise DummyData) ;
  write = (fun _ -> raise DummyData) ;
  reset = (fun _ -> raise DummyData) ;
  distribution = (fun ?group _ -> raise DummyData) ;
}

type db = {
  db : unit -> database;
  create_data : 'a.( ?global:bool -> string -> 'a -> 'a data);
}

let interaction_start_hook = ref ([]: (unit -> unit) list)

let sessid = ref (None: (string * string) option) (* the second string is the group, "guest" is reserved for guest *)

let secret = ref ""

let init_db table_name db_info = 
  match db_info with
  | Memory -> 
    let total_table = Hashtbl.create 1001 in
    { db = (fun () -> MemoryDb);
      create_data = fun ?(global=false) name vinit ->
	let table = Hashtbl.create 1001 in
	let sessid () = match !sessid with None -> raise Exit | Some (s,g) -> if global then "shared_variable", g else s, g in 
	let read = fun () ->
	  try Hashtbl.find table (sessid ()) with Exit | Not_found -> vinit in
	let write = fun v ->
	  try
	    let s, g as sessid = sessid () in
	    let old = try Hashtbl.find total_table g with Not_found -> [] in
	    if not (List.mem s old) then Hashtbl.add total_table g (s::old);
	    Hashtbl.add table sessid v
	  with Exit -> ()
	in
	let reset () =  Hashtbl.remove table (sessid ()) in
	let distribution ?group () =
	  let total = match group with 
	    | None ->
	      Hashtbl.fold (fun k l acc -> acc + List.length l) total_table 0
	    | Some g -> 
	      try List.length (Hashtbl.find total_table g) with Not_found -> 1
	  in
	  let res = Hashtbl.create 101 in
	  Hashtbl.iter (fun k v ->
	    let old = try Hashtbl.find res v with Not_found -> 0 in
	    Hashtbl.add res v (old + 1)) table;
	  total, Hashtbl.fold (fun v n acc -> (v,n)::acc) res [];
	in
	{read; write; reset; distribution};
    }

#ifdef MYSQL
  | Sqlite filename -> 
      (* FIXME: implement Sqlite support, with concurrent access, must manage the Busy error *) 
      Printf.eprintf "Sqlite support not yet implemented\n";
      exit 1
  | Mysql db_info ->
    let dbptr = ref None in
(* FIXME : how to test that data are created only once ?
   at the moment create_data is called more that once in create_data occur under dynamic contents therefore
   the 3 commented lines below report wrongly allready created data.
   But not testing duplicate could lead to segfault, if type differs (due to marshalling) ...
*)
(*    let created = Hashtbl.create 1001 in*)
    
    let db () = 
      match !dbptr with
	None -> let db = connect db_info in Printf.eprintf "Reconnected to db\n%!"; dbptr := Some db; db
      | Some db -> db
    in

    interaction_start_hook := (fun () ->
      match !dbptr with None -> () | Some db -> disconnect db; dbptr := None; Printf.eprintf "Disconnected from db\n%!")::!interaction_start_hook;

    (let sql = Printf.sprintf "CREATE TABLE IF NOT EXISTS `%s` (
      `sessid` CHAR(33), `groupid` CHAR(33), `key` VARCHAR(32), `VALUE` text,
      `createtime` DATETIME NOT NULL,
      `modiftime` TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP);" table_name in
     let _r = exec (db ()) sql in
     match errmsg (db ()) with
     | None -> () 
     | Some err -> Printf.eprintf "DB Error: %s\n%!" err);

    { db = (fun () -> MysqlDb (db ()));
      create_data = fun ?(global=false) name vinit ->
(*	if Hashtbl.mem created name then (Printf.eprintf "Data with name '%s' allready created\n%!" name; exit 1);
	Hashtbl.add created name ();*)
	let v = base64_encode (Marshal.to_string vinit []) in 
	let tbl = Hashtbl.create 7 in
	let sessid () = match !sessid with None -> raise Exit | Some (s,g) -> if global then "shared_variable", g else s, g in 
	let init () = 
	  let sessid, groupid = sessid () in
	  if not (Hashtbl.mem tbl sessid) then (
            let sql = Printf.sprintf "SELECT count(*) FROM `%s` WHERE `sessid` = '%s' AND `groupid` = '%s' AND `key` = '%s';"
	      table_name sessid groupid name in
            let r = exec (db ()) sql in
            (match errmsg (db ()), fetch r with	
            | None, Some row -> 
	      let count = match row.(0) with None -> 0 | Some n -> int_of_string n in
	      (match count with
		0 -> 
		  let sql = Printf.sprintf "INSERT INTO `%s` (`sessid`, `groupid`, `key`, `value`, `createtime`) VALUES ('%s','%s','%s','%s', NOW());"
		    table_name sessid groupid name v in		    
		  let _r = exec (db ()) sql in	      
		  (match errmsg (db ()) with
		  | None -> () 
		  | Some err -> raise (Failure err))
              | 1 -> ()
      	      | _ -> raise (Failure "SQL duplicate data in base"))
            | Some err, _ -> raise (Failure err)
            | _ -> raise (Failure "SQL unexpected problem")));
          sessid, groupid
	in
	let read () =
	  try
            let sessid, groupid = init () in
            let sql = Printf.sprintf "SELECT `value` FROM `%s` WHERE `sessid` = '%s' AND `groupid` = '%s' AND `key` = '%s';"
	      table_name sessid groupid name in
            let r = exec (db ()) sql in
            match errmsg (db ()), fetch r with
    	    | None, Some row -> (match row.(0) with None -> vinit | Some n -> Marshal.from_string (base64_decode n) 0)
            | Some err, _ -> Printf.eprintf "DB Error: %s\n%!" err; vinit
            | _ -> assert false   
            with 
	      Mysql.Error err ->
		Printf.eprintf "Mysql Error: %s\n%!" err; vinit
	    | Exit -> vinit
	in
	let write v =
          try
	    let sessid, groupid = init () in
            let v = base64_encode (Marshal.to_string v []) in 
            let sql = Printf.sprintf "UPDATE `%s` SET `value`='%s' WHERE `key` = '%s' AND `sessid` = '%s' AND `groupid` = '%s';"
	      table_name v name sessid groupid in
            let _r = exec (db ()) sql in
            match errmsg (db ()) with
            | None -> () 
            | Some err -> Printf.eprintf "DB Error: %s\n%!" err
	  with Exit -> ()
	in
	let reset () =
	  try
	    let sessid, groupid = init () in
            let sql = Printf.sprintf "DELETE FROM `%s` WHERE `key` = '%s' AND `sessid` = '%s' AND `groupid` = '%s';"
	      table_name name sessid groupid in
	    let _r = exec (db ()) sql in
	    ()
	  with Exit -> ()
	in
	let distribution ?group () =
	  try
	    let _ = init () in
	    let group, agroup = match group with
		None -> "", ""
	      | Some g -> Printf.sprintf "WHERE `groupid` = '%s' " g, Printf.sprintf "AND `groupid` = '%s' " g
	    in
	    let sql = Printf.sprintf "SELECT `value`,COUNT(DISTINCT `sessid`) FROM `%s` WHERE `key` = '%s' %s GROUP BY `value`"
	      table_name name agroup in
	    let sql' = Printf.sprintf "SELECT COUNT(DISTINCT `sessid`) FROM `%s` %s" table_name group in
	    Printf.eprintf "total: %s\n%!" sql';
	    
	    let f = function None -> "" | Some s -> s in
	    let f' = function None -> 0 | Some s -> int_of_string s in
	    let r = Mysql.exec (db ()) sql' in
	    let total =
	      match Mysql.fetch r with 
		None -> 0
	      | Some row -> f' row.(0)
	    in
	    let r = Mysql.exec (db ()) sql in
	    let scores =
	      let l = ref [] in
	      try while true do
		  match Mysql.fetch r with 
		    None -> raise Exit
		  | Some row -> l := (Marshal.from_string (base64_decode (f row.(0))) 0, f' row.(1))::!l
		done; []
	      with Exit -> !l
	    in 
	    total, scores
	  with Exit -> 0, []
	in
	{read; write; reset; distribution}}
#endif

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
  sessid:=Some (str, "guest");
  str
