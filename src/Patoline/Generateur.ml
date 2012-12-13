(*
  Copyright Tom Hirschowitz, Florian Hatat, Pierre-Etienne Meunier,
  Christophe Raffalli and others, 2012.

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
module StrRegexp=Str
open Lexing
open Parser
open Language
open Util
(* let fugue=ref true *)
(* let spec = [("--extra-fonts-dir",Arg.String (fun x->fontsdir:=x::(!fontsdir)), "Adds directories to the font search path"); *)
(*             ("-c",Arg.Unit (fun ()->fugue:=false), "compile separately"); *)
(*            ] *)
(* let filename=ref [] *)
(* let _=Arg.parse spec (fun x->filename:=x::(!filename)) "Usage :" *)


(* let math arg = [Document.bB (fun env0 -> List.map (fun b -> Box.resize env0.Document.size b) (\* math <$lala$> *\) *)
(*   (let style = Mathematical.Text and _env = (Maths.env_style Maths.default Mathematical.Text) in  *)
(*    Maths.draw_maths Maths.default style ((arg ))))] *)

let edit_link = ref false
let line_directive = ref true

let includeList = ref []

type amble=Noamble | Separate | Main

let apply_options n arg opts = 
  let rec fn = function
  [] -> arg
  | `Arg_pat(i,s)::_ when i = n ->
    StrMap.find s !Build.macros arg
  | _::l -> fn l
  in
  fn opts

let _= Build.macros:=
  StrMap.add "diagram" (fun x->
    "[bB (fun env -> \n" ^
      "let module Res = struct\n "^
      "module Lib = Env_Diagram (struct let env = env end) \n open Lib \n"^
      x^
      "\n end \n"^ 
      "in [ Drawing (Res.Lib.make ()) ])]\n")
  (StrMap.add "genumerate" (fun s->
    let pos = StrRegexp.search_forward (StrRegexp.regexp "&\\([1iIaA]\\)") s 0 in
    let c = String.make 1 s.[pos+1] in
    let prefix = String.sub s 0 pos in
    let suffix = String.sub s (pos+2) (String.length s - pos - 2) in
    "('"^c^"',(fun num_sec -> " ^ prefix ^ "\" ^ num_sec ^ \"" ^ suffix ^ "))"
  )
  !Build.macros)


let hashed="(Sys.executable_name^\".aux\")"
let env_stack=ref []
let preambule format driver suppl amble filename=
  match amble with
      Noamble->suppl^"\n"
    | _->(
      Printf.sprintf
        "(* #FORMAT %s *)
(* #DRIVER %s *)
%s
open Typography
open Typography.Util
open Typography.Box
open Typography.Config
open Typography.Document
open Typography.OutputCommon
%s
open DefaultFormat.MathsFormat
"
        format
        driver
        suppl
        (match amble with
            Main->
              Printf.sprintf "module D=(struct let structure=ref (Node { empty with node_tags=[\"InTOC\",\"\"] },[]) let fixable=ref false end:DocumentStructure)
module Patoline_Format=%s.Format(D);;
module Patoline_Output=Patoline_Format.Output(%s)
open %s;;
open Patoline_Format;;
let _driver=ref \"Pdf\"
let _atmost=ref 3
let _spec = [(\"--extra-fonts-dir\",Arg.String (fun x->Config.fontspath:=x::(!Config.fontspath)),\"Adds directories to the font search path\");
(\"--extra-hyph-dir\",Arg.String (fun x->Config.hyphenpath:=x::(!Config.hyphenpath)), \"Adds directories to the search path for hyphenation dictionaries\");
(\"--extra-plugins-dir\",Arg.String (fun x->Config.pluginspath:=x::(!Config.pluginspath)), \"Adds directories to the plugins search path\");
(\"-I\",Arg.String (fun x->Config.local_path:=x::(!Config.local_path)), \"Adds directories to the font search path\");
(\"--at-most\",Arg.Int (fun x->_atmost:=x),\"Compile at most n times\")
]

let _=Arg.parse _spec ignore \"Usage :\";;" format driver format
          | Separate->Printf.sprintf "module Document=functor(D:DocumentStructure)->struct
module Patoline_Format=%s.Format(D);;
open %s;;
open Patoline_Format;;\n" format format
          | _->""
        ))


let postambule driver outfile = Printf.sprintf "
let _ =Patoline_Output.output Patoline_Output.outputParams (fst (top !D.structure)) Patoline_Format.defaultEnv %S
" outfile

module Source = struct
  type t = int -> string -> int -> int -> unit (* ; *)
	   (* pos in the source, dest, pos in dest, size *)

  let of_string = String.blit

  let of_in_channel ch source_pos dest pos size =
	let n = pos_in ch in
	let _ = seek_in ch source_pos in
	let _ = really_input ch dest pos size in
	let _ = seek_in ch n in
	()

  let of_buffer = Buffer.blit

  let of_function f source_pos dest pos size =
    f source_pos dest pos size

end

let verb_files = Hashtbl.create 13

let moduleCounter=ref 0
let argsCounter=ref 0
let no_ind = { up_right = None; up_left = None; down_right = None; down_left = None }

let split_ind indices =
  { no_ind with up_left = indices.up_left; down_left = indices.down_left; },
  { no_ind with up_right = indices.up_right; down_right = indices.down_right; }

let rec print_math_buf parser_pp op buf m =
  (* Printf.fprintf stderr "Entering print_math_buf.\n" ; flush stderr ; *)
  let rec print_math_expr indices buf m =
    match m with
	Var name | Num name ->
	  let elt = "Maths.glyphs \""^name^"\"" in
	  Printf.bprintf buf "[Maths.Ordinary %a ]" (print_math_deco (CamlSym elt)) indices
      | Symbol name ->
	(* let elt = "Maths.symbol \""^name^"\"" in *)
	Printf.bprintf buf "[Maths.Ordinary %a ]" (print_math_deco name) indices

      | Fun name ->
	let elt = "fun env -> Maths.glyphs \""^name^"\" (Maths.change_fonts env env.font)" in
	Printf.bprintf buf "[ Maths.Ordinary %a ]" (print_math_deco (CamlSym elt)) indices
      | Indices(ind', m) ->
	print_math_expr ind' buf m
      | Binary(_, a, _,SimpleSym "over",_,b) ->
	if (indices <> no_ind) then failwith "Indices on fraction.";
	Printf.bprintf buf "[Maths.fraction (%a) (%a)]" (print_math_expr indices) a (print_math_expr indices) b
      | Binary(pr, a, _,SimpleSym "",_,b) ->
	if (indices <> no_ind) then failwith "Indices on empty binary.";
	Printf.bprintf buf "[Maths.bin_invisible %d (%a) (%a)]" pr (print_math_expr indices) a (print_math_expr indices) b
      | Binary(pr,a,nsl,op,nsr,b) ->
	Printf.bprintf buf "[Maths.bin %d (Maths.Normal(%b,%a, %b)) (%a) (%a)]" pr nsl (print_math_deco op) indices nsr
          (print_math_expr no_ind) a (print_math_expr no_ind) b
      | Apply(f,a) ->
	let ind_left, ind_right = split_ind indices in
	Printf.bprintf buf "[Maths.bin_invisible 4 (%a) (%a)]" (print_math_expr ind_left) f (print_math_expr ind_right) a
(*	
	Printf.bprintf buf "(%a)@(%a)" (print_math_expr ind_left) f (print_math_expr ind_right) a 
*)
      | MathMacro (macro, args) ->
	wrap_deco_math_default buf indices
	  (fun buf ->
	  Printf.bprintf buf "(%s " macro ;
	  List.iter
	    (fun arg ->
	      Printf.bprintf buf "(%a) " (print_math_expr no_ind) arg)
	    args ;
	  Printf.bprintf buf ")"
	)
      | MathCaml (ld,gr,s,e,txps,pos) -> begin
	  let buf' = Buffer.create 80 in
            print_caml_buf parser_pp ld gr op buf' s e txps pos;
	    let s = Buffer.contents buf' in
	      Printf.bprintf buf " ( %s ) " s
      end
      | MathString s ->	Printf.bprintf buf "%s" s
      | Delim(op,a,cl) ->
	wrap_deco_math_default buf indices
	  (fun buf ->
	    Printf.bprintf buf "[Maths.Decoration (Maths.open_close (%a) (%a), %a)]"
	      print_math_multi_symbol op print_math_multi_symbol cl (print_math_expr no_ind) a)
      | Prefix(pr, op, nsp, b) ->
	  Printf.bprintf buf "[Maths.bin %d (Maths.Normal(true,%a,%b)) [] (%a)]" pr (print_math_deco op) indices nsp (print_math_expr no_ind) b
      | Postfix(pr, a, nsp, op) ->
	  Printf.bprintf buf "[Maths.bin %d (Maths.Normal(true,%a, %b)) (%a)  []]" pr (print_math_deco op) indices nsp (print_math_expr no_ind) a
      | Limits_operator(op, a) ->
	  Printf.bprintf buf "[Maths.op_limits [] (%a) (%a)]" (print_math_deco (CamlSym op)) indices (print_math_expr no_ind) a
      | Operator(op, a) ->
	  Printf.bprintf buf "[Maths.op_nolimits [] (%a) (%a)]" (print_math_deco (CamlSym op)) indices (print_math_expr no_ind) a
      | MScope a->
	  Printf.bprintf buf "[Maths.Scope (";
          List.iter (print_math_expr indices buf) a;
          Printf.bprintf buf ")]";
  and print_math_deco elt buf ind =
    let gn name ind =
      match ind with 
	  None -> assert false
	| Some m ->
	  Printf.bprintf buf "%s = (%a);" name (print_math_expr no_ind) m
    in
    if ind = no_ind then (
      Printf.bprintf buf "(Maths.noad (%a))" print_math_symbol elt
    ) else begin
      Printf.bprintf buf "{ (Maths.noad (%a)) with " print_math_symbol elt;
      if ind.up_right <> None then gn "Maths.superscript_right" ind.up_right;
      if ind.up_left <> None then gn "Maths.superscript_left" ind.up_left;
      if ind.down_right <> None then gn "Maths.subscript_right" ind.down_right;
      if ind.down_left <> None then gn "Maths.subscript_left" ind.down_left;
      Printf.bprintf buf "}"
    end

  and wrap_deco_math_default buf deco print_my_math =
    if deco = no_ind then
      print_my_math buf
    else (
      Buffer.add_string buf "[Maths.Ordinary ";
      let buf'=Buffer.create 100 in
        Buffer.add_string buf' "fun envs st->Maths.draw [envs] ";
        print_my_math buf';
        print_math_deco (CamlSym (Buffer.contents buf')) buf deco;
        Buffer.add_string buf "]"
    )

  and print_math_symbol buf sym=
    match sym with
        SimpleSym s->Printf.bprintf buf "Maths.glyphs \"%s\"" s
      | CamlSym s->Printf.bprintf buf "(%s)" s

  and print_math_multi_symbol buf  l=
	Printf.bprintf buf "Maths.multi_glyphs (%s)" l;
	
  in print_math_expr no_ind buf m

and print_math parser_pp op ch m = begin
  let buf = Buffer.create 80 in
  print_math_buf parser_pp op buf m ;
  output_string ch (Buffer.contents buf) ;
end

and print_math_par_buf parser_pp op buf display m =
  (* Printf.fprintf stderr "Entering print_math_par_buf.\n" ; flush stderr ; *)
  let style = if display then "Mathematical.Display" else "env0.mathStyle" in
  Printf.bprintf buf
    "[bB (fun env0 -> Maths.draw [ { env0 with mathStyle = %s } ] ("
    style ;
  print_math_buf parser_pp op buf m;
  Printf.bprintf buf "))] "

and print_math_par pos parser_pp op ch display m = begin 
  let buf = Buffer.create 80 in
  print_ext_link pos (fun buf () -> print_math_par_buf parser_pp op buf display m) buf;
  output_string ch (Buffer.contents buf) 
end

and print_macro_buf parser_pp buf op mtype name args opts =
  (* Printf.fprintf stderr "Entering print_macro_buf.\n" ; flush stderr ; *)
  begin
    match mtype with
      | `Single -> 
	begin
	  (if List.mem `Is_idt opts then
	      Printf.bprintf buf " ("
	  else
	    Printf.bprintf buf " (%s " name);
	  let num = ref 1 in
	  List.iter (function x ->
	    let main_buf = buf in
	    let buf = Buffer.create 80 in
	    let use_par = not (List.mem (`Arg_nopar !num) opts) in
	    (match x with
              | Paragraph(_,p) -> Printf.bprintf buf "%a" (print_contents_buf use_par parser_pp op) p
	      | Caml(ld,gr,s,e,txps,pos) ->
		if use_par then Printf.bprintf buf "(";
		print_caml_buf parser_pp ld gr op buf s e txps pos;
		if use_par then Printf.bprintf buf ")";
	      | String s ->
		Printf.bprintf buf "%s" s
	      | _ -> assert false);
	    let arg = apply_options !num (Buffer.contents buf) opts in
	    Printf.bprintf main_buf " %s" arg ;
	    incr num
	  ) args;
	  if args = [] then Printf.bprintf buf " ()";
	end ;
	Printf.bprintf buf ") ";
      | `Preproc -> 
	begin
	  match args with 
	    | Caml (ld,gr,s,e,txps,pos) :: _ -> begin
	      let buf' = Buffer.create 80 in
              print_caml_buf parser_pp ld gr op buf' s e txps pos;
	      let s = Buffer.contents buf' in 
              let f=StrMap.find name !Build.macros in
	      let s' = f s in
	      (* Printf.fprintf stderr "Started from : \n %s \n" s ; *)
	      (* Printf.fprintf stderr "Printed : \n %s \n" s' ; *)
	      Printf.bprintf buf " ( %s ) " s'
	    end
	    | _ -> assert false
	end
      | `Module | `Begin -> 
        incr moduleCounter;
	Printf.bprintf buf "module TEMP%d = struct\n" !moduleCounter;
	let modname =if mtype = `Begin then "Env_"^name else name in
	let end_open =
	  if args = [] then ""
	  else begin
	    let num = ref 1 in
            incr argsCounter;
	    Printf.bprintf buf "module Args%d = struct\n" !argsCounter;
	    List.iter (function x ->
	      let main_buf = buf in
	      let buf = Buffer.create 80 in
	      let use_par = not (List.mem (`Arg_nopar !num) opts) in
	      (match x with
		| Paragraph(_,p) -> Printf.bprintf buf "%a" (print_contents_buf use_par parser_pp op) p
		| Caml(ld,gr,s,e,txps,pos) ->
		  if use_par then Printf.bprintf buf "(";
		  print_caml_buf parser_pp ld gr op buf s e txps pos;
		  if use_par then Printf.bprintf buf ")";
		| _ -> assert false);
	      let arg = apply_options !num (Buffer.contents buf) opts in
	      Printf.bprintf main_buf "let arg%d = begin %s end\n" !num arg;
	      incr num) args;
	    Printf.bprintf buf "end\n";
	    Printf.sprintf "(Args%d)" !argsCounter
	  end
	in
	begin
          incr moduleCounter;
	  let num = !moduleCounter in
          let s=String.make 1 modname.[0] in
          modname.[0]<-(String.uppercase s).[0];
	  Printf.bprintf buf "module TEMP%d = %s%s\nopen TEMP%d\n let _ = TEMP%d.do_begin_env()"
	    num modname end_open num num; (* name *)
          if mtype=`Begin then env_stack:=(num,name)::(!env_stack) else
	    Printf.bprintf buf "let _=TEMP%d.do_end_env()\nend\n" num
	end
      | `End ->(
          let n,name'=List.hd !env_stack in
          if name'<>name then failwith ("Environment not closed: "^name');
	  Printf.bprintf buf "let _ = TEMP%d.do_end_env()\nend" n(* name *);
          env_stack:=List.tl !env_stack
        )
      | `Include ->
	  incr moduleCounter;
	  includeList := name :: !includeList;
	  Printf.bprintf buf
	    "module TEMP%d=%s.Document(D);;\nopen TEMP%d" !moduleCounter name !moduleCounter
  end

and print_macro parser_pp ch op mtype name args opts = begin
  let buf = Buffer.create 80 in
  print_macro_buf parser_pp buf op mtype name args opts;
  output_string ch (Buffer.contents buf) 
end

and print_caml_buf parser_pp ld gr op buf s e txps (file,line,col) = 
  (* Printf.fprintf stderr "Entering print_caml_buf.\n" ; flush stderr ; *)
  if !line_directive then Printf.bprintf buf "\n# %d \"%s\"\n%s" line file (String.make col ' ');
  match txps with
  | [] -> 
    let size = e - s in
    (* let _ = Buffer.add_buffer buf (op.Source.sub_buffer 0 size) in *)
    let buf'=String.make size (char_of_int 0) in
    let _= op s buf' 0 size in
    Printf.bprintf buf "%s" buf'
  | (style, s',e',line,col) :: txps' -> begin
    (* On imprime du caml avant le premier "<<" *)
    let offset = match style with
      | TxpMath -> 2
      | TxpText -> 2
    in
    let size = s' - s - offset in
    (* Printf.bprintf stderr "s = %d, s' = %d, e' = %d, e = %d\n" s s' e' e; *)
    let buf'=String.create size in
    let _= op s buf' 0 size in
    Printf.bprintf buf "%s" buf';
    (* On imprime la premiere section texprime *)
    let size_txp = e' - s' in
    let input =
      let count = ref 0 in
      let s'' = ref s' in
      fun s n ->
    	let n' = min n (size_txp - !count) in
	if n' > 0 then begin
    	  let _ = if n' > 0 then op !s'' s 0 n' else () in
      	  (*Printf.fprintf stderr "Arguments to blit: \"%s\" %d %d.\n" s 0 n' ; *)
    	  (*flush stderr ; *)
  	  let _ = (count := n' + !count) in
    	  let _ = (s'' := n' + !s'') in
    	  n'
	end else begin
    	  (*Printf.fprintf stderr "End of stream\n" ; *)
    	  (*flush stderr ; *)
	  0
	end
    in
    let lexbuf_txp = Dyp.from_function (parser_pp) input in
    (* let buf'=String.create size_txp in *)
    (* let _ = op s' buf' 0 size_txp in *)
    (* let lexbuf_txp = Dyp.from_string (parser_pp) buf' in *)
    (* Printf.fprintf stderr "Texprime parse dans du Caml: %s\n" buf'; (\* Debug *\) *)
    begin match style with
      | TxpMath ->  begin
	(* Printf.fprintf stderr "Calling Dypgen.\n" ; flush stderr ; *)
	let parser_pilot = { (parser_pp) with Dyp.pp_ld = ld ; Dyp.pp_dev = gr;  } in
	let txp = Dyp.lexparse parser_pilot "allmath" lexbuf_txp in
	(* Printf.fprintf stderr "End Dypgen.\n" ; flush stderr ; *)
	match txp with
	  | (Obj_allmath docs, _) :: _ -> 
	    let sub_input source_pos dest pos size =
	      op (s' + source_pos) dest pos size
	    in
	    print_math_buf parser_pp (Source.of_function sub_input) buf docs
	  | _ -> assert false

      end
      | TxpText -> begin
	(* Printf.fprintf stderr "Calling Dypgen.\n" ; flush stderr ; *)
	let parser_pilot = { (parser_pp) with Dyp.pp_ld = ld ; Dyp.pp_dev = gr;  } in
	let txp = Dyp.lexparse parser_pilot "allparagraph" lexbuf_txp in
	(* Printf.fprintf stderr "End Dypgen.\n" ; flush stderr ; *)
	match txp with
	  | (Obj_allparagraph docs, _) :: _ -> 
	    let sub_input source_pos dest pos size =
	      op (s' + source_pos) dest pos size
	    in
	    print_contents_buf true parser_pp (Source.of_function sub_input) buf docs
	  | _ -> assert false
      end
    end ;
    print_caml_buf parser_pp ld gr op buf (e' + offset) e txps' (file,line,col)
  end

and print_caml parser_pp ld gr op (ch : out_channel) s e txps pos = begin
  let buf = Buffer.create 80 in
  Printf.bprintf buf " ";
  print_caml_buf parser_pp ld gr op buf s e txps pos;
  Printf.bprintf buf " ";
  Buffer.output_buffer ch buf
end

and print_ext_link pos f buf=
  if pos = "" or not !edit_link then f buf ()
  else
    Printf.bprintf buf 
      "(let (file, l, c, _) = %s in extLink (\"edit:\"^file^\"@\"^string_of_int l^\"@\"^string_of_int c) %a)"
	        pos f ()

and print_contents_buf use_par parser_pp op buf l = 
  (* Printf.fprintf stderr "Entering print_contents_buf.\n" ; flush stderr ; *)
  if use_par then Printf.bprintf buf "(";
  let rec fn l = 
    begin match l with
      [] ->  Printf.bprintf buf "[]";
    | (TC (pos, s) :: l' ) as l ->
      if !edit_link then
	(print_ext_link pos (fun buf () -> Printf.bprintf buf "[tT(\"%s\")]" (String.escaped s)) buf;
	 Printf.bprintf buf "@"; fn l')
      else 
	(Printf.bprintf buf "(tT(\""; gn l)
    | GC :: (MC(_,_,_,opts)::_ as l) when List.mem `Eat_left opts -> 
      fn l
    | GC :: l -> 
      Printf.bprintf buf "(tT \" \")::";
      fn l
    | MC(mtype, name, args, opts) :: l -> 
      Printf.bprintf buf " (";
      print_macro_buf parser_pp buf op mtype name args opts;
      Printf.bprintf buf ")@ ";
      (match l with
	GC :: l when  List.mem `Eat_right opts ->
	    fn l
      | l -> fn l)
    | FC(pos,b,m) :: l ->
      Printf.bprintf buf "(";
      print_ext_link pos (fun buf () -> print_math_par_buf parser_pp op buf b m) buf;
      Printf.bprintf buf ")@";
      fn l
    end;
  and gn l =
    begin match l with
    | TC(_pos,s) :: l -> 
      Printf.bprintf buf "%s" (String.escaped s);
      gn l
    | GC :: ((TC _ :: _) as l) -> 
      Printf.bprintf buf " ";
      gn l
    | l ->
      Printf.bprintf buf "\"))::" ;
      fn l
    end
  in 
  fn l;
  if use_par then Printf.bprintf buf ")"

and print_contents parser_pp op (ch : out_channel) l = begin
  let buf = Buffer.create 80 in
  print_contents_buf true parser_pp op buf l;
  output_string ch (Buffer.contents buf) 
end

and output_list parser_pp from where no_indent lvl docs = 
  (* Printf.fprintf stderr "Entering output_list.\n" ; flush stderr ; *)
  match docs with
      [] ->()
 	(* for i = 1 to lvl - 1 do *)
	(*   Printf.fprintf where "let _ = go_up D.structure;;(\* 1 *\)\n\n" *)
	(* done *)
    | doc::docs -> 
      let lvl = ref lvl in 
      let next_no_indent = ref false in
      (match doc with
	| Paragraph(options, p) ->
	  let env = if no_indent or not options.indent_paragraph 
	    then "(fun x -> { x with par_indent = [] })" 
	    else "(fun x -> x)"
	  in
	  let param =
              (* FIXME : cette option ne marche pas, il y a un
                 problème dans le parser. Celui-ci teste si le
                 paragraphe commence après au moins quatre caractères
                 quelconques. Du coup, on se retrouve à centrer les \item *)

              (* if options.center_paragraph then  *)
	      (*   "(Typography.Document.do_center Patoline_Format.parameters)" *)
	      (* else *)
	    "Patoline_Format.parameters"
	  in
	  Printf.fprintf where "let _ = newPar D.structure ~environment:%s Complete.normal %s %a;;\n" 
	    env param (print_contents parser_pp from) p
	| Caml(ld,gr,s,e,txps,pos) -> print_caml parser_pp ld gr from where s e txps pos
	| String s -> Printf.fprintf where "%s" s

	| Struct(title, numbered, docs) ->
	  let num = if numbered=Toc_num then "" else " ~numbered:false" in
          let toc = if numbered=Not_in_toc then " ~in_toc:false" else "" in
	  let print_title where title = 
	      print_contents parser_pp from where title
	  in
	  (match docs with
	      Relative docs ->
		Printf.fprintf where "let _ = newStruct%s%s D.structure %a;;\n\n" num toc print_title title;
		output_list parser_pp from where true (!lvl + 1) docs;
		Printf.fprintf where "let _ = go_up D.structure ;;(* 2 *)\n\n"
	     | Absolute l ->
	      if l > !lvl + 1 then failwith "Illegal level skip";
	      for i = 0 to !lvl - l do
		Printf.fprintf where "let _ = go_up D.structure ;;(* 3 *)\n\n"
	      done;
	      Printf.fprintf where "let _ = newStruct%s D.structure %a;;\n\n" num print_title title;
	      lvl := l; 
	      next_no_indent := true
	  );
	| Macro(mtype, name, args,opts) ->
	  print_macro parser_pp where from mtype name args opts;
	  Printf.fprintf where "\n\n" 
	| Preproc t -> begin
	    Printf.fprintf where "%s\n\n" t ;
	    (* Printf.fprintf stderr "Printed : \n %s \n" t ; *)
	  end
	| Math(pos, m) ->
	  Printf.fprintf where "let _ = newPar D.structure ~environment:(fun x->{x with par_indent = []}) Complete.normal displayedFormula %a;;\n"
	    (fun ch -> print_math_par pos parser_pp from ch true) m
        | Ignore -> 
	  next_no_indent := no_indent
	| Verbatim(lang, filename, lines) ->
	  let lang = match lang with
	      None -> "lang_default"
	    | Some s -> "lang_"^s
	  in
	  let linenum = match filename with
	      None -> ""
	    | Some f ->
	      let f = String.sub f 1 (String.length f - 2) in
	      let ch =
		try
		  Hashtbl.find verb_files f
		with Not_found ->
		  let ch = open_out f in
		  Hashtbl.add verb_files f ch;
		  ch
	      in
	      List.iter (fun l ->
		Printf.fprintf ch "%s\n" l) lines;
	      flush ch;
	      Printf.sprintf "verb_counter \"verb_file_%s\" @" f
	  in
	  List.iter (fun l ->
	    Printf.fprintf where
	      "let _ = newPar D.structure ~environment:verbEnv Complete.normal ragged_left (%s %s \"%s\");;\n"
	      linenum lang (String.escaped l))
	    lines;
      );
      output_list parser_pp from where !next_no_indent !lvl docs

let gen_ml format driver suppl amble filename from wherename where pdfname =
  try
    begin
    (* match filename with *)
    (*     []-> Printf.fprintf stderr "no input files\n" *)
    (*   | h::_-> *)
    (* let op=open_in h in *)

      (* On commence par copier le fichier avec un \n au début *)
      let ftmp=Filename.temp_file (Filename.basename filename) "" in
      let _=
        let x=open_out_bin ftmp in
        output_char x '\n';
        let str=String.create 1000 in
        let rec copy_files ()=
          let n=input from str 0 (String.length str) in
          if n>0 then (
            output x str 0 n;
            copy_files ()
          ) else (
            close_out x;
            close_in from
          )
        in
        copy_files ()
      in
      let from'=open_in ftmp in

      Parser.fprint_caml_buf :=
        (fun ld gr buf s e txps opos ->
          let pos = pos_in from' in
          print_caml_buf (Parser.pp ()) ld gr (Source.of_in_channel from') buf s e txps opos;
          seek_in from' pos);


      let parser_pp = Parser.pp () in
      let lexbuf=Dyp.from_channel parser_pp from' in
      let l = Dyp.std_lexbuf lexbuf in
      l.lex_curr_p <- { l.lex_curr_p with pos_fname = filename };
      try
        let docs = Parser.main lexbuf in
        let nbdocs = List.length docs in
        Printf.fprintf stderr "%s\n" 
	  (Language.message (Language.End_of_parsing nbdocs));
        flush stderr;
        let source = Source.of_in_channel from' in
        let tmp_pos=
          incr moduleCounter;
          !moduleCounter
        in
        match docs with
	    [] -> assert false
	  | ((caml_header, pre, docs), _) :: _  ->
	    begin
              Printf.fprintf where "%s" (preambule format driver suppl amble filename);
              (match amble with
                  Main | Noamble -> ()
                | Separate->Printf.fprintf where "\nlet temp%d = List.map fst (snd !D.structure)\n"
                  tmp_pos);
              match pre with
		  None -> ()
	        | Some(title, at) -> 
		  let extra_tags =
		    let buf = Buffer.create 80 in
		    match at with
		        None -> ""
		      | Some(auth,inst) ->
		        Printf.bprintf buf "~extra_tags:((\"Author\", string_of_contents %a)::"
			  (print_contents_buf true parser_pp source) auth;
		        (match inst with
			    None -> ()
			  | Some(inst) ->
			    Printf.bprintf buf "(\"Institute\", string_of_contents %a)::"
			      (print_contents_buf true parser_pp source) inst);
		        Printf.bprintf buf "[])";
		        Buffer.contents buf
		  in
		  (match caml_header with
                      None->()
                    | Some a->output_list parser_pp source where true 0 [a]);
		  Printf.fprintf where "let _ = Patoline_Format.title D.structure %s (%a);;\n\n"
		    extra_tags (print_contents parser_pp source) title;
	    end;
	    output_list parser_pp source where true 0 docs;
	  (* close_in op; *)
            match amble with
                Main->output_string where (postambule driver pdfname)
              | Noamble->()
              | Separate->Printf.fprintf where "\nlet _ = D.structure:=follow (top !D.structure) (List.rev temp%d)\nend\n"
                tmp_pos
      with
        | Dyp.Syntax_error ->
	  raise
	    (Parser.Syntax_Error (Dyp.lexeme_start_p lexbuf,
				  Parse_error))
        | Failure("lexing: empty token") ->
	  raise
	    (Parser.Syntax_Error (Dyp.lexeme_start_p lexbuf,
				  Unexpected_char))
    end
  with
      Parser.Syntax_Error(pos,msg) ->
	Sys.remove wherename;
	Printf.fprintf stderr "%s\n"
	  (Language.message (Language.Syntax_error (filename, pos, msg)));
	exit 1
