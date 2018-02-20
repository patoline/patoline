open Util

type dbinfo = unit
type database = (string, string) Hashtbl.t

let connect () =
  Hashtbl.create 1001

let disconnect _ =
  ()

let init_db _ _ =
  ()

let create_data total_table _ ?(log=false) ?(visibility=Private) coding name init =
  let rec data =
    let table = Hashtbl.create 1001 in
    let sessid () = match !Db.sessid with
      | None -> ("", "", [])
      | Some (s,g,fs) ->
         match visibility with
         | Private -> (s, g, fs)
         | Group   -> ("ANY", g, [])
         | Public  -> ("ANY", "ANY", [])
    in
    let read = fun () ->
      let s,g,_ = sessid () in
      Db.do_record_read data visibility;
      try Hashtbl.find table (s,g) with Exit | Not_found -> init in
    let add_to_table ((s,g) as key) v =
      let old = try Hashtbl.find total_table g with Not_found -> [] in
      if not (List.mem s old) then Hashtbl.replace total_table g (s::old);
      Hashtbl.replace table key v
    in
    let write = fun v ->
      try
        let s, g, fs = sessid () in
        Db.do_record_write data visibility;
        add_to_table (s,g) v;
        List.iter (fun key -> add_to_table key v) fs;
      with Exit -> ()
    in
    let reset () =
      let s,g,fs = sessid () in
      Db.do_record_write data visibility;
      Hashtbl.remove table (s, g);
    in
    let distribution ?group () =
      Db.do_record_read data (if group = None then Public else max Group visibility);
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
    Db.({ name; init; read; write; reset; distribution})
    in
    data
