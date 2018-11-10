open Patutil
open Util

type dbinfo = Mysql.db
type database = 
  { dbd : Mysql.dbd; created : (string, unit) Hashtbl.t }

let connect db_info =
  let dbd = Mysql.connect db_info in
  Printf.eprintf "Reconnected to db\n%!";
  { dbd; created = Hashtbl.create 1001 }

let disconnect db =
  Mysql.disconnect db.dbd

let init_db db table_name =
  let log_name = table_name ^"_log" in
(* FIXME : how to test that data are created only once ?
   at the moment create_data is called more that once in create_data occur under dynamic contents therefore
   the 3 commented lines below report wrongly already created data.
   But not testing duplicate could lead to segfault, if type differs (due to marshalling) ...
*)
  Patodb.interaction_start_hook := (fun () ->
    Mysql.disconnect db.dbd;
    Printf.eprintf "Disconnected from db\n%!")::!Patodb.interaction_start_hook;

  (* Creating the table structure *)
  begin
    (* FIXME: new sql ? *)
    let sql = Printf.sprintf "CREATE TABLE IF NOT EXISTS `%s` (
      `sessid` CHAR(33),
      `groupid` CHAR(33),
      `key` CHAR(64),
      `VALUE` TEXT,
      `createtime` DATETIME NOT NULL,
      `modiftime` TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (`sessid`,`groupid`,`key`));" table_name in
    let _r = Mysql.exec db.dbd sql in

    let sql = Printf.sprintf "CREATE TABLE IF NOT EXISTS `%s` (
      `sessid` CHAR(33),
      `groupid` CHAR(33),
      `key` TEXT,
      `time` DATETIME NOT NULL);" log_name in
     let _r = Mysql.exec db.dbd sql in

     match Mysql.errmsg db.dbd with
     | None -> ()
     | Some err -> Printf.eprintf "DB Error: %s\n%!" err
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
      let fn (sessid, grouid) =
        let sql = Printf.sprintf "SELECT count(*) FROM `%s` WHERE `sessid` = '%s' AND `key` = '%s';"
                                 table_name sessid name in
        let r = Mysql.exec db.dbd sql in
        match Mysql.errmsg db.dbd, Mysql.fetch r with
        | None, Some row ->
           let count = match row.(0) with None -> 0 | Some n -> int_of_string n in
           (match count with
              0 ->
              let sql = Printf.sprintf "INSERT INTO `%s` (`sessid`, `groupid`, `key`, `value`, `createtime`) VALUES ('%s','%s','%s','%s', NOW());"
                                       table_name sessid groupid name v in
              (*Printf.eprintf "inserting: %s\n%!" sql;*)
              let _r = Mysql.exec db.dbd sql in
              (match Mysql.errmsg db.dbd with
              | None -> (*Printf.eprintf "inserting OK\n%!";*)()
               | Some err -> raise (Failure err));
              if (log) then begin
                let sql = Printf.sprintf "INSERT INTO `%s` (`sessid`, `groupid`, `key`, `time`) VALUES ('%s','%s','%s', NOW());"
                                       log_name sessid groupid name in
                  (*Printf.eprintf "inserting: %s\n%!" sql;*)
                 let _r = Mysql.exec db.dbd sql in
                (match Mysql.errmsg db.dbd with
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
        Patodb.do_record_read data visibility;
        let sql = Printf.sprintf "SELECT `value` FROM `%s` WHERE `sessid` = '%s' AND `key` = '%s';"
          table_name sessid name in
        (*Printf.eprintf "Sending request %s\n%!" sql;*)
        let r = Mysql.exec db.dbd sql in
        (*Printf.eprintf "Sent request\n%!";*)
        match Mysql.errmsg db.dbd, Mysql.fetch r with
            | None, Some row ->
           (match row.(0) with
           | None -> vinit
           | Some n -> coding.Patodb.decode n)
        | Some err, _ -> Printf.eprintf "DB Error: %s\n%!" err; vinit
        | _ -> assert false
        with
          Mysql.Error err ->
            Printf.eprintf "Mysql Error: %s\n%!" err; vinit
        | Exit -> vinit
    in
    let write v =
        let sessid, groupid, friends = init () in
        Patodb.do_record_write data visibility;
        let fn (sessid, groupid) =
          try
            let v = coding.Patodb.encode v in
            let sql = Printf.sprintf "UPDATE `%s` SET `value`='%s',`groupid`='%s' WHERE `key` = '%s' AND `sessid` = '%s';"
                                     table_name v groupid name sessid in
            let _r = Mysql.exec db.dbd sql in
            (match Mysql.errmsg db.dbd with
            | None -> ()
            | Some err -> Printf.eprintf "DB Error: %s\n%!" err);
            if log then begin
              let sql = Printf.sprintf "INSERT INTO `%s` (`sessid`, `groupid`, `key`, `time`) VALUES ('%s','%s','%s', NOW());"
                log_name sessid groupid name in
              (*Printf.eprintf "inserting: %s\n%!" sql;*)
              let _r = Mysql.exec db.dbd sql in
              (match Mysql.errmsg db.dbd with
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
        Patodb.do_record_write data visibility;
        let sql = Printf.sprintf "DELETE FROM `%s` WHERE `key` = '%s' AND `sessid` = '%s';"
          table_name name sessid in
        let _r = Mysql.exec db.dbd sql in
        ()
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
        let r = Mysql.exec db.dbd sql' in
        let total =
          match Mysql.fetch r with
            None -> 0
          | Some row -> f' row.(0)
        in
        let r = Mysql.exec db.dbd sql in
        let scores =
          let l = ref [] in
          try while true do
              match Mysql.fetch r with
                None -> raise Exit
              | Some row -> l := (coding.Patodb.decode (f row.(0)), f' row.(1))::!l
            done; []
          with Exit -> !l
        in
        total, scores
      with Exit -> 0, []

    in Patodb.({name; init=vinit; read; write; reset; distribution})

    in data
