(** Implementation of {!modtype:Patodb.DbInterface} using Sqlite3 *)

open Patutil
open Util

(** Connection information for Sqlite3 only consists of a filename *)
type dbinfo = string

(** Handler to the Sqlite3 database *)
type database =
  { dbd : Sqlite3.db; created : (string, unit) Hashtbl.t }

(** Opening a Sqlite3 database *) 
let connect filename =
  let dbd = Sqlite3.db_open ~mutex:`FULL ~cache:`PRIVATE filename in
  Printf.eprintf "Reconnected to db\n%!";
  { dbd; created = Hashtbl.create 1001 }

(** Closing a Sqlite3 database *)
let disconnect db =
  Sqlite3.db_close db

let exec db ?(cb=fun _ -> ()) sql =
  try
    let r = Sqlite3.exec_no_headers ~cb db sql in
    if r <> Sqlite3.Rc.OK then begin
        Printf.eprintf "sql fails: '%s' (%s)\n%!" sql (Sqlite3.Rc.to_string r);
        Printf.eprintf "  %s\n%!" (Sqlite3.errmsg db);
        exit 1
      end
  with _ ->
    Printf.eprintf "sql fails: '%s'\n%!" sql;
    exit 1

(** Database structure initialization *)
let init_db db table_name =
  let log_name = table_name ^"_log" in

  Patodb.interaction_start_hook := (fun () ->
    ignore (Sqlite3.db_close db.dbd); Printf.eprintf "Disconnected from db\n%!")::!Patodb.interaction_start_hook;

  begin
    (* FIXME: new sql ? *)
    let sql = Printf.sprintf "CREATE TABLE IF NOT EXISTS `%s` (
      `sessid` CHAR(33),
      `groupid` CHAR(33),
      `key` TEXT, `VALUE` TEXT,
      `createtime` DATETIME NOT NULL,
      `modiftime` TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (`sessid`,`groupid`,`key`));" table_name in
    (* TODO TIMESTAMP *)
    exec db.dbd sql;

    let sql = Printf.sprintf "CREATE TABLE IF NOT EXISTS `%s` (
      `sessid` CHAR(33),
      `groupid` CHAR(33),
      `key` TEXT,
      `time` DATETIME NOT NULL);" log_name in
    exec db.dbd sql;


    let sql = Printf.sprintf "CREATE TRIGGER IF NOT EXISTS [%s_Update] AFTER UPDATE ON %s
                               FOR EACH ROW
                               WHEN NEW.`modiftime` < OLD.`modiftime`
                               BEGIN
                                 UPDATE %s SET `modiftime`=CURRENT_TIMESTAMP WHERE
                                   `sessid`=OLD.`sessid` AND
                                   `groupid`=OLD.`groupid` AND
                                   `key`=OLD.`key`
                                 ;
                               END" table_name table_name table_name in
    exec db.dbd sql;
  end

let create_data db table_name ?(log=false) ?(visibility=Private) coding name vinit =
  let log_name = table_name ^"_log" in
  if Hashtbl.mem db.created name then (Printf.eprintf "Data with name '%s' already created\n%!" name; exit 1);
  let rec data =
  Hashtbl.add db.created name ();
  let v = coding.Patodb.encode vinit in
  let sessid () = match !Patodb.sessid with
      None -> "", "guest", []
    | Some (s,g,fs) ->
       match visibility with
       | Private -> (s, g, fs)
       | Group   -> ("ANY", g, [])
       | Public  -> ("ANY", "ANY", [])
  in
  let init () =
    let sessid, groupid, friends = sessid () in
    let fn (sessid, _) =
      let sql = Printf.sprintf "SELECT count(*) FROM `%s` WHERE `sessid` = '%s' AND `key` = '%s';"
                               table_name sessid name in
      let cb row =
         let count = match row.(0) with None -> 0 | Some n -> int_of_string n in
         (match count with
            0 ->
            let sql = Printf.sprintf "INSERT INTO `%s` (`sessid`, `groupid`, `key`, `value`, `createtime`) VALUES ('%s','%s','%s','%s', datetime('now'));"
                                     table_name sessid groupid name v in
            (*Printf.eprintf "inserting: %s\n%!" sql;*)
            exec db.dbd sql;
            if (log) then begin
              let sql = Printf.sprintf "INSERT INTO `%s` (`sessid`, `groupid`, `key`, `time`) VALUES ('%s','%s','%s', datetime('now'));"
                                     log_name sessid groupid name in
                (*Printf.eprintf "inserting: %s\n%!" sql;*)
               exec db.dbd sql;
            end
          | 1 -> ()
                | _ -> raise (Failure "SQL duplicate data in base"))
      in
      exec db.dbd ~cb sql
    in
    fn (sessid, groupid);
    List.iter fn friends;
    sessid, groupid, friends
  in
  let read () =
    try
      let (sessid, _, _)  = init () in
      Patodb.do_record_read data visibility;
      let sql = Printf.sprintf "SELECT `value` FROM `%s` WHERE `sessid` = '%s' AND `key` = '%s';"
                               table_name sessid name in
      let res = ref vinit in
      let cb row =
        match row.(0) with
        | None -> ()
        | Some n -> res := coding.Patodb.decode n
      in
      exec db.dbd ~cb sql;
      !res
    with
      Exit -> vinit
  in
  let write v =
      let sessid, groupid, friends = init () in
      Patodb.do_record_write data visibility;
      let fn (sessid, groupid) =
        try
          let v = coding.Patodb.encode v in
          let sql = Printf.sprintf "UPDATE `%s` SET `value`='%s',`groupid`='%s' WHERE `key` = '%s' AND `sessid` = '%s';"
                                   table_name v groupid name sessid in
          exec db.dbd sql;
          if log then begin
              let sql = Printf.sprintf "INSERT INTO `%s` (`sessid`, `groupid`, `key`, `time`) VALUES ('%s','%s','%s', datetime('now'));"
              log_name sessid groupid name in
              (*Printf.eprintf "inserting: %s\n%!" sql;*)
              exec db.dbd sql
            end
        with Exit -> ()
      in
      fn (sessid, groupid);
      List.iter fn friends
  in
  let reset () =
    try
      let (sessid, _, _) = init () in
      Patodb.do_record_write data visibility;
      let sql = Printf.sprintf "DELETE FROM `%s` WHERE `key` = '%s' AND `sessid` = '%s';"
        table_name name sessid in
      exec db.dbd sql;
    with Exit -> ()
  in
  let distribution ?group () =
    Patodb.do_record_read data (if group = None then Public else max Group visibility);
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
        exec db.dbd ~cb sql';
        !total
      in
      let scores =
        let l = ref [] in
        let cb row = l := (coding.Patodb.decode (f row.(0)), f' row.(1))::!l in
        exec db.dbd ~cb sql;
        !l
      in
      total, scores
    with Exit -> 0, []
  in
  Patodb.({name; init=vinit; read; write; reset; distribution})
  in data




