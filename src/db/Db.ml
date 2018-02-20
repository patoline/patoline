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

type dbinfo =
| Memory
#ifdef MYSQL
| Mysql of Mysql.db
#endif
#ifdef SQLITE3
| Sqlite3 of string
#endif

type database =
| MemoryDb
#ifdef MYSQL
| MysqlDb of Mysql.dbd
#endif
#ifdef SQLITE3
| Sqlite3Db of Sqlite3.db
#endif

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

exception DummyData

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
  decode : string -> 'a;
}

let default_coding = {
  encode = (fun v -> Base64.encode (Marshal.to_string v []));
  decode = (fun s -> Marshal.from_string (Base64.decode s) 0);
}

let string_coding = {
  encode = (fun v -> Base64.encode v);
  decode = (fun s -> Base64.decode s);
}

let bool_coding = {
  encode = (fun v -> if v then "true" else "false");
  decode = (fun s -> s = "true");
}

type db = {
  db : unit -> database;
  create_data : 'a.( ?log:bool -> ?visibility:visibility -> 'a coding -> string -> 'a -> 'a data);
  disconnect : unit -> unit;
}

let interaction_start_hook = ref ([]: (unit -> unit) list)

let do_interaction_start_hook () =
  List.iter (fun f -> f ()) (!interaction_start_hook)

let sessid = ref (None: (string * string * (string * string) list) option)
(* the first string is the login *)
(* the second string is the group, "guest" is reserved for guest *)
(* the list of pairs of strings are the "friends login and group" *)

let secret = ref ""

let read_hook : (string -> visibility -> unit) list ref = ref []
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

let do_record_read  = fun d v -> List.iter (fun f -> f d.name v) !read_hook
let do_record_write = fun d v -> List.iter (fun f -> f d.name v) !write_hook

let init_db table_name db_info =
  let log_name = table_name ^"_log" in
  match db_info with
  | Memory -> (* The model does not nor use the provided coding, not needed *)
    let total_table = Hashtbl.create 1001 in
    { db = (fun () -> MemoryDb);
      disconnect = (fun () -> ());
      create_data = fun ?(log=false) ?(visibility=Private) coding name init ->
        let rec data =
        let table = Hashtbl.create 1001 in
        let sessid () = match !sessid with
          | None -> ("", "", [])
          | Some (s,g,fs) ->
             match visibility with
             | Private -> (s, g, fs)
             | Group   -> ("ANY", g, [])
             | Public  -> ("ANY", "ANY", [])
        in
        let read = fun () ->
          let s,g,_ = sessid () in
          do_record_read data visibility;
          try Hashtbl.find table (s,g) with Exit | Not_found -> init in
        let add_to_table ((s,g) as key) v =
          let old = try Hashtbl.find total_table g with Not_found -> [] in
          if not (List.mem s old) then Hashtbl.replace total_table g (s::old);
          Hashtbl.replace table key v
        in
        let write = fun v ->
          try
            let s, g, fs = sessid () in
            do_record_write data visibility;
            add_to_table (s,g) v;
            List.iter (fun key -> add_to_table key v) fs;
          with Exit -> ()
        in
        let reset () =
          let s,g,fs = sessid () in
          do_record_write data visibility;
          Hashtbl.remove table (s, g);
        in
        let distribution ?group () =
          do_record_read data (if group = None then Public else max Group visibility);
          let total = match group with
            | None ->
              Hashtbl.fold (fun k l acc -> acc + List.length l) total_table 0
            | Some g ->
              try List.length (Hashtbl.find total_table g) with Not_found -> 1
          in
          let res = Hashtbl.create 101 in
          Hashtbl.iter (fun k v ->
            let old = try Hashtbl.find res v with Not_found -> 0 in
            Hashtbl.replace res v (old + 1)) table;
          total, Hashtbl.fold (fun v n acc -> (v,n)::acc) res [];
        in
        { name; init; read; write; reset; distribution}
        in
        data
    }

#ifdef MYSQL
  | Mysql db_info ->
    let dbptr = ref None in
(* FIXME : how to test that data are created only once ?
   at the moment create_data is called more that once in create_data occur under dynamic contents therefore
   the 3 commented lines below report wrongly allready created data.
   But not testing duplicate could lead to segfault, if type differs (due to marshalling) ...
*)
    let created = Hashtbl.create 1001 in

    let db () =
      match !dbptr with
        None -> let db = Mysql.connect db_info in Printf.eprintf "Reconnected to db\n%!"; dbptr := Some db; db
      | Some db -> db
    in

    interaction_start_hook := (fun () ->
      match !dbptr with None -> () | Some db -> Mysql.disconnect db; dbptr := None; Printf.eprintf "Disconnected from db\n%!")::!interaction_start_hook;
      (
     (* FIXME: new sql ? *)
    let sql = Printf.sprintf "CREATE TABLE IF NOT EXISTS `%s` (
      `sessid` CHAR(33), `groupid` CHAR(33), `key` CHAR(64), `VALUE` text,
      `createtime` DATETIME NOT NULL,
      `modiftime` TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (`sessid`,`groupid`,`key`));" table_name in
     let _r = Mysql.exec (db ()) sql in
     let sql = Printf.sprintf "CREATE TABLE IF NOT EXISTS `%s` (
      `sessid` CHAR(33), `groupid` CHAR(33), `key` text, `time` DATETIME NOT NULL);" log_name in
     let _r = Mysql.exec (db ()) sql in
     match Mysql.errmsg (db ()) with
     | None -> ()
     | Some err -> Printf.eprintf "DB Error: %s\n%!" err);

    { db = (fun () -> MysqlDb (db ()));
      disconnect = (fun () ->
        match !dbptr with None -> ()
        | Some db -> Mysql.disconnect db);
      create_data = fun ?(log=false) ?(visibility=Private) coding name vinit ->
        if Hashtbl.mem created name then (Printf.eprintf "Data with name '%s' allready created\n%!" name; exit 1);
        let rec data =
        Hashtbl.add created name ();
        let v = coding.encode vinit in
        let sessid () = match !sessid with
            None -> "", "guest", []
          | Some (s,g,fs) ->
             match visibility with
             | Private -> (s, g, fs)
             | Group   -> ("ANY", g, [])
             | Public  -> ("ANY", "ANY", [])
        in
        let init () =
          let sessid, groupid, friends = sessid () in
          let fn (sessid, grouid) =
            let sql = Printf.sprintf "SELECT count(*) FROM `%s` WHERE `sessid` = '%s' AND `key` = '%s';"
                                     table_name sessid name in
            let r = Mysql.exec (db ()) sql in
            match Mysql.errmsg (db ()), Mysql.fetch r with
            | None, Some row ->
               let count = match row.(0) with None -> 0 | Some n -> int_of_string n in
               (match count with
                  0 ->
                  let sql = Printf.sprintf "INSERT INTO `%s` (`sessid`, `groupid`, `key`, `value`, `createtime`) VALUES ('%s','%s','%s','%s', NOW());"
                                           table_name sessid groupid name v in
                  (*Printf.eprintf "inserting: %s\n%!" sql;*)
                  let _r = Mysql.exec (db ()) sql in
                  (match Mysql.errmsg (db ()) with
                  | None -> (*Printf.eprintf "inserting OK\n%!";*)()
                   | Some err -> raise (Failure err));
                  if (log) then begin
                    let sql = Printf.sprintf "INSERT INTO `%s` (`sessid`, `groupid`, `key`, `time`) VALUES ('%s','%s','%s', NOW());"
                                           log_name sessid groupid name in
                      (*Printf.eprintf "inserting: %s\n%!" sql;*)
                     let _r = Mysql.exec (db ()) sql in
                    (match Mysql.errmsg (db ()) with
                    | None -> (*Printf.eprintf "inserting OK\n%!";*)()
                    | Some err -> raise (Failure err))
                  end
                | 1 -> ()
                      | _ -> raise (Failure "SQL duplicate data in base"))
            | Some err, _ -> raise (Failure err)
            | _ -> raise (Failure "SQL unexpected problem")
          in
          fn (sessid, groupid);
          List.iter fn friends;
          sessid, groupid, friends
        in
        let read () =
          try
            let sessid, groupid, _  = init () in
            do_record_read data visibility;
            let sql = Printf.sprintf "SELECT `value` FROM `%s` WHERE `sessid` = '%s' AND `key` = '%s';"
              table_name sessid name in
            (*Printf.eprintf "Sending request %s\n%!" sql;*)
            let r = Mysql.exec (db ()) sql in
            (*Printf.eprintf "Sent request\n%!";*)
            match Mysql.errmsg (db ()), Mysql.fetch r with
                | None, Some row ->
               (match row.(0) with
               | None -> vinit
               | Some n -> coding.decode n)
            | Some err, _ -> Printf.eprintf "DB Error: %s\n%!" err; vinit
            | _ -> assert false
            with
              Mysql.Error err ->
                Printf.eprintf "Mysql Error: %s\n%!" err; vinit
            | Exit -> vinit
        in
        let write v =
            let sessid, groupid, friends = init () in
            do_record_write data visibility;
            let fn (sessid, groupid) =
              try
                let v = coding.encode v in
                let sql = Printf.sprintf "UPDATE `%s` SET `value`='%s',`groupid`='%s' WHERE `key` = '%s' AND `sessid` = '%s';"
                                         table_name v groupid name sessid in
                let _r = Mysql.exec (db ()) sql in
                (match Mysql.errmsg (db ()) with
                | None -> ()
                | Some err -> Printf.eprintf "DB Error: %s\n%!" err);
                if log then begin
                  let sql = Printf.sprintf "INSERT INTO `%s` (`sessid`, `groupid`, `key`, `time`) VALUES ('%s','%s','%s', NOW());"
                    log_name sessid groupid name in
                  (*Printf.eprintf "inserting: %s\n%!" sql;*)
                  let _r = Mysql.exec (db ()) sql in
                  (match Mysql.errmsg (db ()) with
                  | None -> (*Printf.eprintf "inserting OK\n%!";*)()
                  | Some err -> Printf.eprintf "DB Error: %s\n%!" err)
                end
              with Exit -> ()
            in
            fn (sessid, groupid);
            List.iter fn friends
        in
        let reset () =
          try
            let sessid, groupid, _ = init () in
            do_record_write data visibility;
            let sql = Printf.sprintf "DELETE FROM `%s` WHERE `key` = '%s' AND `sessid` = '%s';"
              table_name name sessid in
            let _r = Mysql.exec (db ()) sql in
            ()
          with Exit -> ()
        in
        let distribution ?group () =
          do_record_read data (if group = None then Public else max Group visibility);
          try
            let _ = init () in
            let group = match group with
                None -> ""
              | Some g -> Printf.sprintf "AND `groupid` = '%s' " g
            in
            let sql = Printf.sprintf
              "SELECT `value`,COUNT(DISTINCT `sessid`) FROM `%s` WHERE `key` = '%s' %s GROUP BY `value`"
              table_name name group in
            let sql' = Printf.sprintf "SELECT COUNT(DISTINCT `sessid`) FROM `%s` WHERE `key` = '%s' %s"
              table_name name group in
            (*Printf.eprintf "total: %s\n%!" sql';*)

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
                  | Some row -> l := (coding.decode (f row.(0)), f' row.(1))::!l
                done; []
              with Exit -> !l
            in
            total, scores
          with Exit -> 0, []
        in
        {name; init=vinit; read; write; reset; distribution}
        in data
    }
#endif
#ifdef SQLITE3
  | Sqlite3 filename ->
    let dbptr = ref None in
    let created = Hashtbl.create 1001 in
    let db () =
      match !dbptr with
        None -> let db = Sqlite3.db_open ~mutex:`FULL ~cache:`PRIVATE filename in
                Printf.eprintf "Reconnected to db\n%!"; dbptr := Some db; db
      | Some db -> db
    in
    let exec ?(cb=fun _ -> ()) sql =
      try
        let r = Sqlite3.exec_no_headers ~cb (db ()) sql in
        if r <> Sqlite3.Rc.OK then begin
            Printf.eprintf "sql fails: '%s' (%s)\n%!" sql (Sqlite3.Rc.to_string r);
            Printf.eprintf "  %s\n%!" (Sqlite3.errmsg (db ()));
            exit 1
          end
      with
        e ->
        Printf.eprintf "sql fails: '%s'\n%!" sql;
        exit 1
    in

    interaction_start_hook := (fun () ->
      match !dbptr with
      | None -> ()
      | Some db -> ignore (Sqlite3.db_close db); dbptr := None;
                   Printf.eprintf "Disconnected from db\n%!")::!interaction_start_hook;
      (
     (* FIXME: new sql ? *)
    let sql = Printf.sprintf "CREATE TABLE IF NOT EXISTS `%s` (
      `sessid` CHAR(33), `groupid` CHAR(33), `key` text, `VALUE` text,
      `createtime` DATETIME NOT NULL,
                              `modiftime` TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (`sessid`,`groupid`,`key`));" table_name in
    (* TODO TIMESTAMP *)
     exec sql;
     let sql = Printf.sprintf "CREATE TABLE IF NOT EXISTS `%s` (
      `sessid` CHAR(33), `groupid` CHAR(33), `key` text, `time` DATETIME NOT NULL);" log_name in
     exec sql;


     let sql = Printf.sprintf "CREATE TRIGGER IF NOT EXISTS [%s_Update] AFTER UPDATE ON %s
                               FOR EACH ROW
                               WHEN NEW.`modiftime` < OLD.`modiftime`
                               BEGIN
                                 UPDATE %s SET `modiftime`=CURRENT_TIMESTAMP WHERE
                                   `sessid`=OLD.`sessid` AND
                                   `groupid`=OLD.`groupid` AND
                                   `key`=OLD.`key`
                                 ;
                               END" table_name table_name table_name
     in
     exec sql;

    { db = (fun () -> Sqlite3Db (db ()));
      disconnect = (fun () ->
        match !dbptr with None -> ()
        | Some db -> ignore (Sqlite3.db_close db));
      create_data = fun ?(log=false) ?(visibility=Private) coding name vinit ->
        if Hashtbl.mem created name then (Printf.eprintf "Data with name '%s' allready created\n%!" name; exit 1);
        let rec data =
        Hashtbl.add created name ();
        let v = coding.encode vinit in
        let sessid () = match !sessid with
            None -> "", "guest", []
          | Some (s,g,fs) ->
             match visibility with
             | Private -> (s, g, fs)
             | Group   -> ("ANY", g, [])
             | Public  -> ("ANY", "ANY", [])
        in
        let init () =
          let sessid, groupid, friends = sessid () in
          let fn (sessid, grouid) =
            let sql = Printf.sprintf "SELECT count(*) FROM `%s` WHERE `sessid` = '%s' AND `key` = '%s';"
                                     table_name sessid name in
            let cb row =
               let count = match row.(0) with None -> 0 | Some n -> int_of_string n in
               (match count with
                  0 ->
                  let sql = Printf.sprintf "INSERT INTO `%s` (`sessid`, `groupid`, `key`, `value`, `createtime`) VALUES ('%s','%s','%s','%s', datetime('now'));"
                                           table_name sessid groupid name v in
                  (*Printf.eprintf "inserting: %s\n%!" sql;*)
                  exec sql;
                  if (log) then begin
                    let sql = Printf.sprintf "INSERT INTO `%s` (`sessid`, `groupid`, `key`, `time`) VALUES ('%s','%s','%s', datetime('now'));"
                                           log_name sessid groupid name in
                      (*Printf.eprintf "inserting: %s\n%!" sql;*)
                     exec sql;
                  end
                | 1 -> ()
                      | _ -> raise (Failure "SQL duplicate data in base"))
            in
            exec ~cb sql
          in
          fn (sessid, groupid);
          List.iter fn friends;
          sessid, groupid, friends
        in
        let read () =
          try
            let sessid, groupid, _  = init () in
            do_record_read data visibility;
            let sql = Printf.sprintf "SELECT `value` FROM `%s` WHERE `sessid` = '%s' AND `key` = '%s';"
                                     table_name sessid name in
            let res = ref vinit in
            let cb row =
              match row.(0) with
              | None -> ()
              | Some n -> res := coding.decode n
            in
            exec ~cb sql;
            !res
          with
            Exit -> vinit
        in
        let write v =
            let sessid, groupid, friends = init () in
            do_record_write data visibility;
            let fn (sessid, groupid) =
              try
                let v = coding.encode v in
                let sql = Printf.sprintf "UPDATE `%s` SET `value`='%s',`groupid`='%s' WHERE `key` = '%s' AND `sessid` = '%s';"
                                         table_name v groupid name sessid in
                exec sql;
                if log then begin
                    let sql = Printf.sprintf "INSERT INTO `%s` (`sessid`, `groupid`, `key`, `time`) VALUES ('%s','%s','%s', datetime('now'));"
                    log_name sessid groupid name in
                    (*Printf.eprintf "inserting: %s\n%!" sql;*)
                    exec sql
                  end
              with Exit -> ()
            in
            fn (sessid, groupid);
            List.iter fn friends
        in
        let reset () =
          try
            let sessid, groupid, _ = init () in
            do_record_write data visibility;
            let sql = Printf.sprintf "DELETE FROM `%s` WHERE `key` = '%s' AND `sessid` = '%s';"
              table_name name sessid in
            exec sql;
          with Exit -> ()
        in
        let distribution ?group () =
          do_record_read data (if group = None then Public else max Group visibility);
          try
            let _ = init () in
            let group = match group with
                None -> ""
              | Some g -> Printf.sprintf "AND `groupid` = '%s' " g
            in
            let sql = Printf.sprintf
              "SELECT `value`,COUNT(DISTINCT `sessid`) FROM `%s` WHERE `key` = '%s' %s GROUP BY `value`"
              table_name name group in
            let sql' = Printf.sprintf "SELECT COUNT(DISTINCT `sessid`) FROM `%s` WHERE `key` = '%s' %s"
              table_name name group in
            (*Printf.eprintf "total: %s\n%!" sql';*)

            let f = function None -> "" | Some s -> s in
            let f' = function None -> 0 | Some s -> int_of_string s in
            let total =
              let total = ref 0 in
              let cb row =
                match row.(0) with
                | None -> ()
                | Some s -> total := int_of_string s
              in
              exec ~cb sql';
              !total
            in
            let scores =
              let l = ref [] in
              let cb row = l := (coding.decode (f row.(0)), f' row.(1))::!l in
              exec ~cb sql;
              !l
            in
            total, scores
          with Exit -> 0, []
        in
        {name; init=vinit; read; write; reset; distribution}
        in data
    })
#endif

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
