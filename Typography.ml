(** La "classe" de documents par defaut. *)

open Binary
open Constants
open Fonts
open Fonts.FTypes
open Drivers
open Util

(** Pour choisir la police, et d'autres paramètres, on a un
   environnement. On peut tout modifier de manière uniforme sur tout
   le document à partir de n'importe où (voir le type content, plus
   bas, pour les scopes) *)

type fontAlternative = Regular | Bold | Caps | Demi
 
(* Italic is second *)
type fontFamily = (fontAlternative * (font Lazy.t * font Lazy.t)) list

let lmroman =
  [ Regular, (
       Lazy.lazy_from_fun (fun () -> 
	 loadFont "Otf/LatinModernRoman/lmroman10-regular.otf"),
       Lazy.lazy_from_fun (fun () -> 
	 loadFont "Otf/LatinModernRoman/lmroman10-italic.otf"));
    Bold, (
       Lazy.lazy_from_fun (fun () -> 
	 loadFont "Otf/LatinModernRoman/lmroman10-bold.otf"),
       Lazy.lazy_from_fun (fun () -> 
	 loadFont "Otf/LatinModernRoman/lmroman10-bolditalic.otf"));
    Caps, (
      Lazy.lazy_from_fun (fun () -> 
	loadFont "Otf/LatinModernRoman/lmromancaps10-regular.otf"),
      Lazy.lazy_from_fun (fun () -> 
	loadFont "Otf/LatinModernRoman/lmromancaps10-oblique.otf"));
    Demi, (
      Lazy.lazy_from_fun (fun () -> 
	loadFont "Otf/LatinModernRoman/lmromandemi10-regular.otf"),
      Lazy.lazy_from_fun (fun () -> 
	loadFont "Otf/LatinModernRoman/lmromandemi10-oblique.otf"));
  ]

let lmmono =
  [ Regular, (
       Lazy.lazy_from_fun (fun () -> 
	 loadFont "Otf/LatinModernMono/lmmonolt10-regular.otf"),
       Lazy.lazy_from_fun (fun () -> 
	 loadFont "Otf/LatinModernMono/lmmonolt10-oblique.otf"));
    Bold, (
       Lazy.lazy_from_fun (fun () -> 
	 loadFont "Otf/LatinModernMono/lmmonolt10-bold.otf"),
       Lazy.lazy_from_fun (fun () -> 
	 loadFont "Otf/LatinModernMono/lmmonolt10-boldoblique.otf"));
    Caps, (
      Lazy.lazy_from_fun (fun () -> 
	loadFont "Otf/LatinModernMono/lmmonocaps10-regular.otf"),
      Lazy.lazy_from_fun (fun () -> 
	loadFont "Otf/LatinModernMono/lmmonocaps10-oblique.otf"));
    Demi, (
      Lazy.lazy_from_fun (fun () -> 
	loadFont "Otf/LatinModernMono/lmmonoltcond10-regular.otf"),
      Lazy.lazy_from_fun (fun () -> 
	loadFont "Otf/LatinModernMono/lmmonoltcond10-oblique.otf"));
  ]

(* Alegreya raises : Opentype.Table_not_found *)
let alegreya =
  [ Regular, (
       Lazy.lazy_from_fun (fun () -> 
	 loadFont "Otf/Alegreya/Alegreya-Regular.otf"),
       Lazy.lazy_from_fun (fun () -> 
	 loadFont "Otf/Alegreya/Alegreya-Italic.otf"));
    Bold, (
       Lazy.lazy_from_fun (fun () -> 
	 loadFont "Otf/Alegreya/Alegreya-Bold.otf"),
       Lazy.lazy_from_fun (fun () -> 
	 loadFont "Otf/Alegreya/Alegreya-BoldItalic.otf"));
    Caps, (
      Lazy.lazy_from_fun (fun () -> 
	loadFont "Otf/Alegreya/AlegreyaSC-Regular.otf"),
      Lazy.lazy_from_fun (fun () -> 
	loadFont "Otf/Alegreya/AlegreyaSC-Italic.otf"));
    Demi, (
      Lazy.lazy_from_fun (fun () -> 
	loadFont "Otf/Alegreya/Alegreya-Black.otf"),
      Lazy.lazy_from_fun (fun () -> 
	loadFont "Otf/Alegreya/Alegreya-BlackItalic.otf"));
  ]

let selectFont fam alt it =
  try
    let r, i = List.assoc alt fam in
    Lazy.force (if it then i else r)
  with Not_found ->
    /* FIXME: keep the font name and print a better message */
    Printf.fprintf stderr "Font not found in family.\n"; 
    exit 1


type user=
    NamedCitation of string
  | Citation of int

module TS=Typeset.Make (struct
                          type t=user
                          let compare=compare
                          let citation x=Citation x
                        end)

module C=Parameters.Completion (TS.UMap)

type 'a environment={
  mutable fontFamily:fontFamily;
  mutable fontItalic:bool;
  mutable fontAlternative:fontAlternative;
  mutable fontFeatures:string list;
  mutable font:font;
  mutable size:float;
  mutable par_indent:'a box list;
  mutable stdGlue:'a box;
  mutable hyphenate:string->(string*string) array;
  mutable substitutions:glyph_id list -> glyph_id list;
  mutable positioning:glyph_ids list -> glyph_ids list;
}

let defaultFam= lmroman
let defaultMono= lmmono
let defaultEnv:user environment=
  let f=selectFont lmroman Regular false in
  let fsize=5. in
    {
      fontFamily=lmroman;
      fontItalic=false;
      fontAlternative=Regular;
      fontFeatures= [ Opentype.standardLigatures ];
      font=f;
      substitutions=
        (fun glyphs -> List.fold_left apply glyphs (
           Fonts.select_features f [ Opentype.standardLigatures ]
         ));
      positioning=positioning f;
      size=4.;
      par_indent = [Drawing { drawing_min_width= 4.0 *. phi;
                              drawing_max_width= 4.0 *. phi;
                              drawing_y0=0.;drawing_y1=0.;
                              drawing_nominal_width= 4.0 *. phi;
                              drawing_contents=(fun _->[]);
                              drawing_badness=fun _-> 0. }];
      stdGlue=Glue { drawing_min_width= 2.*. fsize/.9.;
                     drawing_max_width= fsize/.2.;
                     drawing_y0=0.; drawing_y1=0.;
                     drawing_nominal_width= fsize/.3.;
                     drawing_contents=(fun _->[]);
                     drawing_badness=knuth_h_badness (fsize/.3.) };
      hyphenate=fun _->[||](*
        fun str->
          let hyphenation_dict=
            let i=open_in "dict_en" in
            let inp=input_value i in
              close_in i;
              inp
          in
          let hyphenated=Hyphenate.hyphenate hyphenation_dict str in
          let pos=Array.make (List.length hyphenated-1) ("","") in
          let rec hyph l i cur=match l with
              []->()
            | h::s->(
                pos.(i)<-(cur^"-", List.fold_left (^) "" l);
                hyph s (i+1) (cur^h)
              )
          in
            match hyphenated with
                []->[||]
              | h::s->(hyph s 0 h; pos)
                           *);
    }


(* Type du contenu. B est une boîte quelconque. Les espaces dans T
   seront transformés en la boîte stdGlue de l'environnement, qui
   n'est pas nécessairement une GlueBox *)
type 'a content=
    B of ('a environment->'a box list)
  | T of string
  | FileRef of (string*int*int)
  | Scoped of ('a environment->'a environment)*('a content list)



(****************************************************************)



(* Le jeu est de construire la structure de document suivante :
   C'est un arbre, avec du contenu texte à chaque nœud. *)

type 'a node={
  name:string;
  displayname:'a content list;
  children:'a tree IntMap.t;
  mutable tree_paragraph:int;
}
and 'a paragraph={
  par_contents:'a content list;
  par_env:'a environment;
  parameters:'a box array array -> drawingBox array -> parameters -> line IntMap.t -> line TS.UMap.t -> line -> parameters;
  completeLine:'a box array array -> drawingBox array -> line IntMap.t -> line TS.UMap.t -> line -> bool -> line list
}
and 'a tree=
    Node of 'a node
  | Paragraph of 'a paragraph
  | Figure of string*('a environment -> drawingBox)*
      ('a box array array -> drawingBox array -> parameters -> line IntMap.t -> line TS.UMap.t -> line -> parameters)

let empty={ name=""; displayname = []; children=IntMap.empty; tree_paragraph= (-1) }

(* La structure actuelle *)
let str=ref (Node empty)
(* Le chemin vers le noeud courant *)
let cur=ref []

(* Sortie en dot de la structure du document *)
let doc_graph out t0=
  Printf.fprintf out "digraph {\n";
  let rec do_it path t=
    Printf.fprintf out "%s [label=\"%s\"];\n" path t.name;
    List.iter (fun (i,x)->match x with
                   Paragraph _
                 | Figure _-> ()
                 | Node n->(
                     let p=path^"_"^(string_of_int i) in
                       Printf.fprintf out "%s -> %s;\n" path p;
                       do_it p n)) (IntMap.bindings t.children)
  in
    (match t0 with
         Node t->do_it "x0" t
       | _->());
    Printf.fprintf out "}\n"


let next_key t=try fst (IntMap.max_binding t)+1 with Not_found -> 0

(* Exemple de manipulation de la structure : faire un nouveau paragraphe *)
let newPar ?(environment=defaultEnv) complete parameters par=
  let para=Paragraph {par_contents=par; par_env=environment; parameters=parameters; completeLine=complete } in
  let rec newPar tree path=
    match path with
        []->(match tree with
                 Node t->Node { t with children=IntMap.add (next_key t.children) para t.children }
               | _ -> Node { empty with children=IntMap.add 1 tree (IntMap.add 2 para IntMap.empty) })
      | h::s->
          (match tree with
               Node t->(let t'=try IntMap.find h t.children with _->Node empty in
                          Node { t with children=IntMap.add h (newPar t' s) t.children })

             | _ -> Node { empty with children=IntMap.add 1 tree (IntMap.add 2 (newPar (Node empty) s) IntMap.empty) })
  in
    str:=newPar !str !cur




(* Remonter d'un niveau dans l'arbre, par rapport au noeud courant *)
let up ()=
  let rec up=function
      []->[]
    | [h]->[]
    | h::s-> h::up s
  in
    cur:=up !cur

let string_of_contents l =
  let s = ref "" in
  List.iter (function
    T str -> 
      if !s = "" then s:= str else s:= !s ^" " ^str
  | _ -> ()) l;
  !s

let newStruct ?label displayname =
  let name = match label with
      None -> string_of_contents displayname
    | Some s -> s
  in
  let para=Node { empty with name=name; displayname = displayname } in
  let rec newStruct tree path=
    match path with
        []->(match tree with
                 Node t->(
                   let next=next_key t.children in
                     cur:= !cur @ [next];
                     Node { t with children=IntMap.add next para t.children }
                 )
               | _ -> Node { empty with children=IntMap.add 1 tree (IntMap.add 2 para IntMap.empty) })
      | h::s->
          (match tree with
               Node t->(let t'=try IntMap.find h t.children with _->Node empty in
                          Node { t with children=IntMap.add h (newStruct t' s) t.children })

             | _ -> Node { empty with children=IntMap.add 1 tree (IntMap.add 2 (newStruct (Node empty) s) IntMap.empty) })
  in
    str:=newStruct !str !cur

let title ?label displayname =
  let name = match label with
      None -> string_of_contents displayname
    | Some s -> s
  in
  match !str with
      Paragraph _ | Figure _->str:= Node { name=name; displayname = displayname; 
				children=IntMap.singleton 1 !str; tree_paragraph=0 }
    | Node n -> str:=Node { n with name=name; displayname = displayname }


(****************************************************************)


(* Quelques Exemples d'environnement *)

let updateFont env font =
     { env with
       font=font;
       substitutions=
         (fun glyphs -> List.fold_left apply glyphs (
           Fonts.select_features font env.fontFeatures
          ));
       positioning=positioning font }
  
(* Changer de font dans un scope, ignore la famille, attention, à éviter en direct *)
let font f t=
  let font=loadFont f in
    [Scoped ((fun env-> updateFont env font), t)]

let envItalic b env =
  let font = selectFont env.fontFamily env.fontAlternative b in
  let env = { env with fontItalic = b } in
  updateFont env font
      
let italic t =
  [Scoped(envItalic true, t)]

module Italic = struct
  let do_begin_Italic () = ()
  let do_end_Italic () = ()
  let defaultEnv = envItalic true defaultEnv
end

module Env_Italic = Italic

let notItalic t =
  [Scoped (envItalic false, t)]

let toggleItalic t =
  [Scoped ((fun env -> envItalic (not env.fontItalic) env), t)]
 
let envAlternative features alt env =
  let font = selectFont env.fontFamily alt env.fontItalic in
  let env = { env with fontAlternative = alt } in
  updateFont env font
 
let alternative ?features alt t =
  [Scoped ((fun env -> 
    let features = match features with
	None -> env.fontFeatures
      | Some f -> f
    in
    envAlternative features alt env), t)]

let envFamily fam env =
  let font = selectFont fam env.fontAlternative env.fontItalic in
  let env = { env with fontFamily = fam } in
  updateFont env font

let family fam t =
  [Scoped ((fun env -> envFamily fam env), t)]

(* Changer de taille dans un scope *)
let size fsize t=
  Scoped ((fun env ->
             { env with
                 size=fsize;
                 stdGlue=Glue { drawing_min_width= 2.*. fsize/.9.;
                                drawing_max_width= fsize/.2.;
                                drawing_y0=0.;drawing_y1=0.;
                                drawing_nominal_width= fsize/.3.;
                                drawing_contents = (fun _->[]);
                                drawing_badness=knuth_h_badness (fsize/.3.) }}), t)
let glues t=
  Scoped ((fun env ->
             match env.stdGlue with
                 Glue g ->(
                   let rec select_glue=function
                       []->Glue g
                     | Alternative a::_->(
                         let gl=
                           (glyphCache env.font { empty_glyph with glyph_index=a.(Random.int (Array.length a-1)) })
                         in
                           Glue { g with
                                    drawing_contents=(
                                      fun w->[
                                        Drivers.Glyph { gl with
                                                          glyph_y=0.;
                                                          glyph_x=
                                            (w-.env.size*.Fonts.glyphWidth gl.glyph/.1000.)/.2.;
                                                          glyph_size=env.size }
                                      ]);
                                    drawing_min_width=(
                                      g.drawing_min_width+.
                                        env.size*.Fonts.glyphWidth gl.glyph/.1000.);
                                    drawing_nominal_width=(
                                      g.drawing_nominal_width+.
                                        env.size*.Fonts.glyphWidth gl.glyph/.1000.);
                                    drawing_max_width=(
                                      g.drawing_max_width+.
                                        env.size*.Fonts.glyphWidth gl.glyph/.1000.);

                                })
                       | _::s-> select_glue s
                   in
                     { env with stdGlue=select_glue (select_features env.font [Opentype.ornaments]) }
                 )
               |_->env
          ), t)



(* Rajouter une liste de features, voir Fonts.FTypes pour savoir ce
   qui existe *)
let features f t=
  Scoped ((fun env->
             { env with substitutions=
                 (fun glyphs -> List.fold_left apply glyphs
                    (Fonts.select_features env.font f))}), t)

(****************************************************************)

(* Partie compliquée : il faut comprendre ce que fait l'optimiseur
   pour toucher à ça, ou apprendre en touchant ça *)





let parameters paragraphs figures last_parameters last_figures last_users (line:Util.line)=
  let mes0=150. in
  let lead=5. in
  let mes=
    if IntMap.cardinal last_figures > 0 then
      let fig,fig_line=IntMap.max_binding last_figures in
        if line.page=fig_line.page &&
          line.height<=
          fig_line.height +.
            (ceil ((figures.(fig).drawing_y1-.figures.(fig).drawing_y0))) then

              mes0 -. figures.(fig).drawing_nominal_width -. 1.
        else
          mes0
    else
      mes0
  in
    { measure=mes ;
      page_height=if line.page_line <= 0 then 45.*.lead else last_parameters.page_height;
      left_margin=(fst a4-.150.)/.2.;
      local_optimization=0;
      min_page_diff=0;
      next_acceptable_height=(fun _ h->lead*.(1.+.ceil (h/.lead)));
      min_height_before=0.;
    }


let center paragraphs figures last_parameters lastFigures lastUsers l=
  let param=parameters paragraphs figures last_parameters lastFigures lastUsers l in
  let b=l.nom_width in
    if param.measure >= b then
      { param with measure=b; left_margin=param.left_margin +. (param.measure-.b)/.2. }
    else
      param


let figure ?(name="") ?(parameters=center) drawing=
  let rec fig tree path=
    match path with
        []->(match tree with
                 Node t->Node { t with children=IntMap.add (next_key t.children) (Figure (name,drawing,parameters)) t.children }
               | _ -> Node { empty with children=IntMap.add 1 tree (IntMap.add 2 (Figure (name,drawing,parameters)) IntMap.empty) })
      | h::s->
          (match tree with
               Node t->(let t'=try IntMap.find h t.children with _->Node empty in
                          Node { t with children=IntMap.add h (fig t' s) t.children })

             | _ -> Node { empty with children=IntMap.add 1 tree (IntMap.add 2 (fig (Node empty) s) IntMap.empty) })
  in
    str:=fig !str !cur


(****************************************************************)

(* Fonctions auxiliaires qui produisent un document optimisable à
   partir de l'arbre *)


let structNum path name=
  let n=match path with
      h::s->List.fold_left (fun x y -> x^"."^(string_of_int (y+1))) (string_of_int (h+1)) s
    | []->"0"
  in
  if List.length path <= 2 then
    [Scoped ((fun env->{(envAlternative (Opentype.oldStyleFigures::env.fontFeatures) Caps env) with
      size=(if List.length path = 1 then sqrt phi else sqrt (sqrt phi))*.env.size
    }), (T n::B (fun env -> [env.stdGlue])::name))]
  else
    [Scoped ((fun env-> envAlternative (Opentype.oldStyleFigures::env.fontFeatures) Caps env),
	     (T n::B (fun env -> [env.stdGlue])::name))]


let is_space c=c=' ' || c='\n' || c='\t'
let sources=ref StrMap.empty

let rec boxify env =function
    []->[]
  | (B b)::s->(b env)@(boxify env s)
  | (T t)::s->(
    let rec cut_str i0 i result=
      if i>=String.length t then (
        if i0<>i then (
          if result<>[] then
            result @ (env.stdGlue :: (hyphenate env.hyphenate env.substitutions env.positioning env.font env.size
                                        (String.sub t i0 (i-i0))))
          else
            hyphenate env.hyphenate env.substitutions env.positioning env.font env.size (String.sub t i0 (i-i0))
        ) else result
      ) else (
        if is_space t.[i] then
          cut_str (i+1) (i+1) (
            if i0<>i then (
              if result<>[] then
                result @ (env.stdGlue :: (hyphenate env.hyphenate env.substitutions env.positioning env.font env.size
                                            (String.sub t i0 (i-i0))))
              else
                hyphenate env.hyphenate env.substitutions env.positioning env.font env.size (String.sub t i0 (i-i0))
            ) else result
          )
        else (
          cut_str i0 (i+1) result
        )
      )
    in
    let c=cut_str 0 0 [] in
    c @ (boxify env s)
  )
  | FileRef (file,off,size)::s -> (
    let i=try 
	    StrMap.find file !sources 
      with _-> (let i=open_in file in sources:= StrMap.add file i !sources; i) 
    in
    let buf=String.create size in
    let _=seek_in i off; input i buf 0 size in
    boxify env (T buf::s)
  )
  | Scoped (env', p)::s->(
    let c=(boxify (env' env) p) in
    c@(boxify env s)
  )

let flatten env0 str=
  let lead=5. in
  let paragraphs=ref [] in
  let figures=ref IntMap.empty in
  let figure_names=ref StrMap.empty in
  let fig_param=ref IntMap.empty in
  let param=ref [] in
  let compl=ref [] in
  let n=ref 0 in


  let add_paragraph p=
    paragraphs:=(Array.of_list (boxify p.par_env p.par_contents))::(!paragraphs);
    compl:=(p.completeLine)::(!compl);
    param:=(p.parameters)::(!param);
    incr n;
  in

  let rec flatten env path tree=
    match tree with
        Paragraph p -> add_paragraph p
      | Figure (name, f, p) -> (
          let n=IntMap.cardinal !figures in
            figure_names:=StrMap.add name n !figure_names;
            fig_param:=IntMap.add n p !fig_param;
            figures:=IntMap.add n (f env) !figures
        )
      | Node s-> (
          s.tree_paragraph <- !n;
          if path<>[] then (
            add_paragraph ({ par_contents=structNum path s.displayname;
                             par_env=env;
                             parameters=
                               (fun a b c d e f ->
                                  { (parameters a b c d e f) with
                                      next_acceptable_height=(fun _ h->h+.lead*.2.); min_height_before=2.*.lead });
                             completeLine=C.normal (fst a4) });
          ) else if s.name<>"" then (
            add_paragraph {par_contents=[size 10. [T (s.name)] ];
                           par_env=env;
                           parameters=(
                             fun a b c d e f ->
                               let cen=center a b c d e f in
                                 { cen with min_height_before=max (4.*.lead) c.min_height_before });
                           completeLine=C.normal (fst a4) }
          );
          let rec flat_children num indent= function
              []->()
            | (_, (Paragraph p))::s->(
                flatten env path (
                  Paragraph { p with par_contents=
                      (if indent then [B (fun _->p.par_env.par_indent)] else []) @ p.par_contents }
                );
                flat_children num true s
              )
            | (_,(Figure _ as h))::s->(
                flatten env path h;
                flat_children num true s
              )
            | (_, (Node _ as tr))::s->(
                flatten env (path@[num]) tr;
                flat_children (num+1) false s
              )
          in
            flat_children 0 false (IntMap.bindings s.children)
        )
  in

    flatten env0 [] str;
    let figures_resolved=
      List.map (Array.map (function
                               User (NamedCitation s) -> User (Citation (StrMap.find s !figure_names))
                             | x -> x)) !paragraphs
    in
    let params=Array.make (IntMap.cardinal !figures) center in
      IntMap.iter (fun n p->params.(n)<-p) !fig_param;
      (params,
       Array.of_list (List.rev !param),
       Array.of_list (List.rev !compl),
       Array.of_list (List.rev figures_resolved),
       Array.of_list (List.map snd (IntMap.bindings !figures)))


let rec make_struct positions tree=
  match tree with
      Paragraph _ | Figure _->
          { Drivers.name="";
	    Drivers.displayname=[];
            Drivers.page=0;
            Drivers.struct_x=0.;
            Drivers.struct_y=0.;
            Drivers.substructures=[||] }
    | Node s-> (
        let (p,x,y)=positions.(s.tree_paragraph) in
        let rec make=function
            []->[]
          | (_,Paragraph _) :: s | (_,Figure _) :: s->make s
          | (_,Node u)::s -> (make_struct positions (Node u))::(make s)
        in
        let a=Array.of_list (make (IntMap.bindings s.children)) in
          { Drivers.name=s.name;
	    Drivers.displayname=[] (* FIXME boxify ?env [T s.name] *);
            Drivers.page=p;
            Drivers.struct_x=x;
            Drivers.struct_y=y;
            Drivers.substructures=a }
      )
