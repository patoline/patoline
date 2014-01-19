open Typography
open Document
open Db
open OutputCommon
open Box
open Util

module type ModDb = sig
  val db : db
  val base_dir : string
end

let strip s =
  let len = String.length s in
  let i = ref 0 in
  while !i < len && List.mem s.[!i] [' ';'\n';'\t';'\r'] do
    incr i
  done;
  let j = ref (len-1) in
  while !i < !j && List.mem s.[!j] [' ';'\n';'\t';'\r'] do
    decr j
  done;
  String.sub s !i (!j - !i + 1)

let read_file file =
  let ch = open_in file in
  let len = in_channel_length ch in
  let str = String.create len in
  let _ = input ch str 0 len in
  let res = strip str in
  close_in ch;
  res


let arrow = tT ">>" :: hspace(1.0) 

module Make(D:DocumentStructure)
 (Format:module type of DefaultFormat.Format(D) 
   (* a strange way to remove Output from the
      signature to allow Format to change the
      Output functor *)
   with module Output := DefaultFormat.Format(D).Output)
 (MyDb:ModDb) = struct

  open Format
  open MyDb

type result =
| Ok
| FailTest
| DoNotCompile
| NotTried

let scoreBar (module EnvDiagram : Diagrams.EnvDiagram) height width data =
  let open Diagrams in 
  let open EnvDiagram in
  let total = max 1 (List.fold_left (fun acc ((_: color),x) -> acc + x) 0 data) in
  let cur_pos = ref 0.0 in
  let data = List.map (fun (color, x) ->
    let x1 = !cur_pos +. float x *. width /. float total in
    let res = (color, x, !cur_pos, x1) in
    cur_pos := x1;
    res) data
  in

  let h2 = height /. 2. in

  List.iter (fun ((col:color), x, x0, x1) ->
    let _ = path Edge.([draw;fill (col:color);noStroke]) (x0, 0.0)
      [[x1, 0.0] ; [x1, height] ; [x0, height]] in
    let text = [tT (string_of_int x)] in
    let text_box = Document.draw_boxes env (boxify_scoped env text) in
    let (x0',_,x1',_) = bounding_box_full text_box in
    if x1' -. x0' < 1.75 *. (x1 -. x0) then ignore (node Node.([at ((x0 +. x1) /. 2., h2); outerSep 0.0; innerSep 0.0]) text)
  )
    data

let scoreBarProg (module EnvDiagram : Diagrams.EnvDiagram) height width data =
  let data = List.sort (fun (x,_) (x',_) -> compare x x') data in
  let data = List.map (fun
    (x,n) -> 
      let color = match x with
	  Ok -> green
	| FailTest -> yellow
        | DoNotCompile -> red
	| _ -> grey
      in (color, n)) data
  in
  scoreBar (module EnvDiagram : Diagrams.EnvDiagram) height width data

let ascii = 
  let str = String.make (2*(128-32)) ' ' in
  for i = 32 to 127 do
    str.[2*(i-32)] <- Char.chr i
  done;
  [Scoped(verbEnv, [tT str] @ bold [tT str])]

let editableText ?(global=false) ?(empty_case="Type in here")
      ?(min_line=5) ?(init_text="") ?(lang=lang_default)
      ?extra name =
    
    let name' = name^"_target" in
    let name'' = name^"_target2" in

    let readT, writeT = db.create_data ~global name init_text in

    let update () =
      let s = readT() in
      let s' = if s = "" then empty_case else s in
      s, Util.split '\n' s'
    in

    dynamic name'
      (function Edit(n, t) when name = n -> writeT t; Private 
      | _ -> Unchanged)
      ascii 	  
      (fun () ->
        let s, lines = update () in
	let missing = max 0 (min_line - List.length lines) in
	let lines = lines @ Array.to_list (Array.make missing "") in
        (button ~btype:(Editable(s, init_text))
           name
	   [name';name'']
           [bB(fun env -> 
	     let env = { env with 
	       normalLead = DefaultFormat.defaultEnv.normalLead *. 0.8;
	       lead = DefaultFormat.defaultEnv.normalLead *. 0.8;
	     } in
	     List.map (fun x-> Drawing (snd x))
	       (IntMap.bindings 
		  (OutputDrawing.minipage (verbEnv env)
		     (let i = ref 0 in 
		      let next () =
			incr i;
 			let line = string_of_int !i in
			let miss = 3 - String.length line in
			[glue_space miss;tT line;glue_space 1]
		      in
		      let acc = List.fold_left (fun acc line ->
			let para=Paragraph 
			  {par_contents=next () @ (lang line);
			   par_env=(fun e -> e);
			   par_post_env=(fun env1 env2 -> 
			     { env1 with names=names env2; counters=env2.counters; user_positions=user_positions env2 });
			   par_parameters=ragged_left;
			   par_badness=badness;
			   par_completeLine=Complete.normal; par_states=[]; par_paragraph=(-1) }
			in up (newChildAfter acc para)) (Node empty, []) lines
		   in		
		  let lines = match extra with None -> []
		  | Some f ->
  		      let readR, writeR = db.create_data ~global (name^"_results") NotTried in
		      let writeR = if s <> init_text then
                        writeR
                      else
                        fun _ -> writeR NotTried
                      in
		      Util.split '\n' (f writeR (readT()))
		  in
       		  (List.fold_left (fun acc line ->
		    let para=Paragraph
		      {par_contents= arrow @ (lang_default line) ;
		       par_env=(fun env -> {env with size = env.size *. 0.8});
		       par_post_env=(fun env1 env2 -> { env1 with names=names env2; counters=env2.counters; user_positions=user_positions env2 });
		       par_parameters=ragged_left;
		       par_badness=badness;
		       par_completeLine=Complete.normal; par_states=[]; par_paragraph=(-1) }
		    in up (newChildAfter acc para)) acc lines)))))]))

let ocaml_dir () =
  let sessid = match !Db.sessid with 
    None -> "unknown"
  | Some(s,_) -> s
  in
  (* FIXME: use a subfolder per group ? *)
  let name = Filename.concat base_dir sessid in
  if not (Sys.file_exists  name) then
    Unix.mkdir name 0o755;
  name

let test_ocaml ?(run=true) ?filename ?(prefix="") ?(suffix="") writeR prg =
  let dir, filename, target, exec, run, delete_all =
    match filename with
      None -> 
        let prefix = Filename.temp_file "demo" "" in
	Sys.remove prefix;
	Unix.mkdir prefix 0o700;
        prefix, "tmp.ml", "tmp.cmo", "tmp.byte", run, true
    | Some name ->
        let prefix = Filename.chop_extension name in
	if Filename.check_suffix name ".ml" then
	  ocaml_dir (), name,
	  prefix^".cmo",
          prefix ^ ".byte", run, false
        else
          ocaml_dir (), name,
	  prefix^".cmi",
	  "", false, false
  in
  let ch = open_out (Filename.concat dir filename) in
  output_string ch prefix;
  output_string ch (Printf.sprintf "# 1 \"%s\"\n" (if delete_all then "" else filename));
  output_string ch prg;
  output_string ch suffix;
  close_out ch;
  let tmpfile2 = Filename.temp_file "demo" ".txt" in
  let tmpfile3 = Filename.temp_file "demo" ".txt" in
  let cmd = Printf.sprintf "cd %s; ocamlbuild -quiet %s >%s" dir target tmpfile3 in
(*  Printf.eprintf "running: %s\n%!" cmd;*)
  let _ = Sys.command cmd in
  let err = read_file tmpfile3 in
  let err = 
    try 
      let n = 8 + Str.search_forward (Str.regexp_string "File \"\",") err 0 in
      String.sub err n (String.length err - n) 
    with Not_found -> err
  in
  let err, out =
    if err <> "" || not run then (
      if err <> "" then writeR DoNotCompile else if not run then writeR Ok;
      err, "" ) else (
    let cmd = Printf.sprintf "cd %s; ocamlbuild -quiet %s" dir exec in
    let _ = Sys.command cmd in
(*    Printf.eprintf "running: %s\n%!" cmd;*)
    let cmd = Printf.sprintf "cd %s; ulimit -t 5; ./%s 2>%s >%s" dir exec tmpfile3 tmpfile2 in
(*    Printf.eprintf "running: %s\n%!" cmd;*)
    let _ = Sys.command cmd in
    let err = read_file tmpfile3 in
    if err <> "" then writeR FailTest else writeR Ok;
    let out = read_file tmpfile2 in
    err,out)
  in
  (try Sys.remove tmpfile2 with _ -> ());
  (try Sys.remove tmpfile3 with _ -> ());
  if delete_all then (
    let _ = Sys.command (Printf.sprintf "cd %s; ocamlbuild -clean -quiet" dir) in
    if run then (try Sys.remove (Filename.concat dir exec) with _ -> ());
    (try Sys.remove (Filename.concat dir target) with _ -> ());
    (try Sys.remove (Filename.concat dir filename) with _ -> ());
    (try Unix.rmdir dir with _ -> ());
  );
  if err <> "" then err else 
  if out <> "" then out else "No error and no output"

let test_python ?(prefix="") ?(suffix="") writeR prg =
  let tmpfile = Filename.temp_file "demo" ".ml" in
  let ch = open_out tmpfile in
  output_string ch prefix;
  output_string ch "# 1 \"\"\n";
  output_string ch prg;
  output_string ch suffix;
  close_out ch;
  let tmpfile2 = Filename.temp_file "demo" ".txt" in
  let tmpfile3 = Filename.temp_file "demo" ".txt" in
  ignore (Sys.command (Printf.sprintf "python3 %s 2>%s >%s" tmpfile tmpfile3 tmpfile2));
  let err = read_file tmpfile3 in
  let out = read_file tmpfile2 in
  (try Sys.remove tmpfile with _ -> ());
  (try Sys.remove tmpfile2 with _ -> ());
  (try Sys.remove tmpfile3 with _ -> ());
  if err <> "" then (writeR FailTest; err) else 
  (writeR Ok; if out <> "" then out else "No error and no output")

let distribution ?group table key =
  let group, agroup = match group with
      None -> "", ""
    | Some g -> Printf.sprintf "WHERE `groupid` = '%s' " g, Printf.sprintf "AND `groupid` = '%s' " g
  in
  let sql = Printf.sprintf "SELECT `value`,COUNT(DISTINCT `sessid`) FROM `%s` WHERE `key` = '%s' %s GROUP BY `value`" table key agroup in
  let sql' = Printf.sprintf "SELECT COUNT(DISTINCT `sessid`) FROM `%s` %s" table group in

  let mysql_db = match db.db () with
      MysqlDb db -> db(* | _ -> assert false*) in
  let f = function None -> "" | Some s -> s in
  let f' = function None -> 0 | Some s -> int_of_string s in
  let r = Mysql.exec mysql_db sql' in
  let total =
    match Mysql.fetch r with 
      None -> 0
    | Some row -> f' row.(0)
  in
  let r = Mysql.exec mysql_db sql in
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

let score ?group table sample display exo =
  let exo' = exo ^"_results" in

  dynamic (exo^"_target2")  (fun _ -> Public) sample (fun () ->
    let total, scores = distribution ?group:(match group with None -> None | Some g -> Some (g ())) table exo' in
    let total = max 1 total in
    let scores = List.filter (fun (x,_) -> x <> NotTried) scores in
    let total' = List.fold_left (fun acc (_,n) -> acc + n) 0 scores in
    let scores = (NotTried,total - total') :: scores in
 
    display scores)
end

