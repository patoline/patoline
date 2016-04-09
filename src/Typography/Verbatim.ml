open Document
open FTypes
open Box

(*
let verb_counter filename =
  let get_line env =
    if filename = "" then 1 else
      try
	match StrMap.find filename env.counters
	with a,[line] -> line
	| _ -> raise Not_found
      with Not_found -> 1
  in
  C (fun env ->
    let line = string_of_int (get_line env) in
    let miss = 4 - String.length line in
    glue_space miss ::  color Color.(mix 0.5 white black) [tT line] @ [tT " "])::
    Env (fun env ->
      let line = get_line env in
      {env with counters = StrMap.add filename (-1,[line+1]) env.counters})::
  []*)

let file_cache = Hashtbl.create 31

(* [lines_to_file lines fn] writes the lines [lines] to the optional file
   [fn] if it is provided. Do nothing otherwise. *)
let lines_to_file : string list -> string option -> unit = fun lines fn ->
  match fn with
  | None    -> ()
  | Some fn ->
     let nb_lines = List.length lines in
     let oc, nbl =
       try
	 Hashtbl.find file_cache fn
       with Not_found ->
	 (open_out fn, 1)
     in
     List.iter (Printf.fprintf oc "%s\n") lines;
     Hashtbl.replace file_cache fn (oc, nbl + nb_lines);
     flush oc

(* [glue_space n] corresponds to [n] spaces from the font. *)
let glue_space : int -> content = fun n ->
  let f env =
	  let font,_,_,_ = selectFont env.fontFamily Regular false in
    let glyph_index = Fonts.glyph_of_char font ' ' in
    let sp = Fonts.loadGlyph font {empty_glyph with glyph_index} in
    let spw = Fonts.glyphWidth sp in
	  let w =  float_of_int n *. env.size *. spw /. 1000.0 in
	  [glue w w w]
  in bB f

(* [line_per_line f lines] builds each line of [lines] using [f] and display
   each of them using the verbatim font. *)
let line_per_line : 'a -> (string -> content list) -> string list -> unit =
  fun str f lines ->
    let par = do_ragged_left parameters in
    let len = List.length lines in
    let draw_line i l =
      let cs = f l in
      let cs = if i = 0 then linesBefore 2 @ cs else cs in
      let cs = if i = len - 1 then linesAfter 2 @ cs else cs in
      newPar str ~environment:verbEnv Complete.normal par cs
    in
    List.iteri draw_line lines

(* [handle_spaces w_to_c line] builds a line using the function [w_to_c] to
   build words (i.e. sections without spaces) and takes care of the spaces
   (one text space is as wide as any other character). *)
let handle_spaces : (string -> content list) -> string -> content list =
  fun w_to_c l ->
    let len = String.length l in
    let rec l_to_a pos =
      if pos >= len then []
      else if l.[pos] = ' ' then
        let nbsp = ref 1 in
        while pos + !nbsp < len && l.[pos + !nbsp] = ' ' do incr nbsp done;
        let pos = pos + !nbsp in
        if pos >= len then []
        else glue_space !nbsp :: l_to_a pos
      else
        let nbnsp = ref 1 in
        while pos + !nbnsp < len && l.[pos + !nbnsp] <> ' ' do incr nbnsp done;
        let str = String.sub l pos !nbnsp in
        let pos = pos + !nbnsp in
        w_to_c str @ l_to_a pos
    in l_to_a 0

(* [fit_on_grid c] adds spacing on both sides of the content element [c] so
   that its width becomes a multiple of the width of a single character. *)
let fit_on_grid : content -> content = fun c ->
  let boxwidth env cs =
    let (x0,_,x1,_) = RawContent.bounding_box (draw env cs) in
    (x1 -. x0) /. env.size
  in
  let f env =
    let charw = boxwidth env [tT "a"] in
    let symw  = boxwidth env [c] in
    let extra = (ceil (symw /. charw)) *. charw -. symw in
    let sp = hspace (extra /. 2.0) in
    sp @ (c :: sp)
  in C f


(* [symbol s] build a mathematical symbole from the string [s]. Its width
   on the page will be a multiple of the width of one verbatim character. *)
let symbol : string -> content = fun s ->
  let f env =
    let open Mathematical in
    let menv = Maths.env_style env.mathsEnvironment Display in
    let font = Lazy.force menv.mathsFont in
    let gl =
      { glyph_utf8  = s
      ; glyph_index = Fonts.glyph_of_uchar font (UTF8.look s 0) }
    in
    [bB (fun _ ->
      [glyphCache font gl env.fontColor (menv.mathsSize *. env.size)])]
  in
  fit_on_grid (C f)

(* Parameter type for the word handler bellow. *)
type param =
  { keywords : string list
  ; symbols  : (string * content) list }

exception Found_symbol of int * string * content

(* [handle_word par w] build the contents corresponding to the word [w]. The
   [par] variable provides parameters for keywords to be put in bold and
   special symbols to be displayed using the maths font. *)
let handle_word : param -> string -> content list = fun par w ->
  if List.mem w par.keywords then bold [tT w] else
  (*if List.mem w par.symbols then [symbol w] else*)
  let find_special str start =
    let test_special str i =
      let test_eq (s,cs) =
        let t = try String.sub str i (String.length s) with _ -> "" in
        if t = s then raise (Found_symbol (i, s, cs))
      in
      List.iter test_eq par.symbols
    in
    try
      for i = start to String.length str - 1 do
        test_special str i
      done;
      None
    with Found_symbol (i,s,cs) -> Some (i,s,cs)
  in
  let rec build_word str acc pos =
    match find_special str pos with
    | None      ->
        let last = String.sub str pos (String.length str - pos) in
        acc @ [tT last]
    | Some(i,s,cs) ->
        [tT (String.sub str pos (i - pos)); fit_on_grid cs] @
        let pos = i + String.length s in
        build_word str acc pos
  in build_word w [] 0

(* [verb_text build s] builds verbatim contents from a string [s] and a
   generation function [build]. *)
let verb_text : (string -> content list) -> string -> content list =
  fun build s ->
    let f env =
      let size = env.size *. env.fontMonoRatio in
      {(envFamily env.fontMonoFamily env) with size}
    in
    [Scoped (f, build s)]



let param_SML =
  { keywords = ["fun";"as";"fn";"*";"(";")";",";";";"val";
		"and";"type";"|";"=";"case";"of";
		"datatype";"let";"rec";"end"]
  ; symbols = [("->", symbol "→");  ("=>", symbol "⇒")]
  }

let param_OCaml =
  { keywords = ["fun";"as";"function";"(";")";"*";";";",";"val";
		"and";"type";"|";"=";"match";"with";
		"rec";"let";"begin";"end";"while";"for";"do";"done";
		"struct"; "sig"; "module"; "functor"; "if"; "then";
          "else"; "try"; "parser"; "in" ]
  ; symbols = [("->", symbol "→");  ("=>", symbol "⇒")]
  }

let param_Python =
  { keywords = ["def";"(";")";"*";";";",";"|";"=";":";
		"while";"for";"if";"else";"return";"try";"except";"break"]
  ; symbols  = [] }

let lang_Default lines =
  List.map (handle_spaces (fun s -> [tT s])) lines

let lines_to_contents param lines =
  List.map (handle_spaces (handle_word param)) lines

let lang_OCaml = lines_to_contents param_OCaml
let lang_SML = lines_to_contents param_SML
let lang_Python = lines_to_contents param_Python

  (*
let verb_OCaml = verb_Lang
let verb_SML = verb_Lang param_SML
let verb_Python = verb_Lang param_Python
let verb_default fn lines =
  lines_to_file lines fn;
  let build_line = handle_spaces (fun s -> [tT s]) in
  lines

let verb_Lang param fn lines =
  lines_to_file lines fn;
  let build_line = handle_spaces (handle_word param) in
  lines


  *)
