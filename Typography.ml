(** La "classe" de documents par defaut. *)

open Binary
open Constants
open Fonts
open Fonts.FTypes
open OutputCommon
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
    (* FIXME: keep the font name and print a better message *)
    Printf.fprintf stderr "Font not found in family.\n"; 
    exit 1


type user=
    NamedCitation of string
  | FigureRef of int
  | Pageref of string
  | Structure of int list
  | Figure of int
  | Footnote of int*drawingBox

let names=ref StrMap.empty
let user_count=ref 0

module TS=Typeset.Make (struct
                          type t=user
                          let compare=compare
                          let figureRef x=FigureRef x
                          let figure x=Figure x
                          let isFigure=function Figure _->true | _->false
                          let figureNumber=function Figure x->x | _-> -1
                        end)

module C=Parameters.Completion (TS)

type 'a node={
  name:string;
  displayname:'a content list;
  in_toc : bool;
  children:'a tree IntMap.t;
  node_env:'a environment -> 'a environment;
  node_post_env:'a environment -> 'a environment -> 'a environment;
  mutable tree_paragraph:int;
}
and 'a paragraph={
  par_contents:'a content list;
  par_env:'a environment -> 'a environment;
  par_post_env:'a environment -> 'a environment -> 'a environment;
  parameters:'a environment -> 'a box array array -> drawingBox array -> parameters -> line TS.UMap.t -> line -> parameters;
  completeLine:'a box array array -> drawingBox array -> line TS.UMap.t -> line -> bool -> line list
}
and 'a tree=
    Node of 'a node
  | Paragraph of 'a paragraph
  | FigureDef of string*('a environment -> drawingBox)*
      ('a environment -> 'a box array array -> drawingBox array -> parameters -> line TS.UMap.t -> line -> parameters)

and 'a environment={
 fontFamily:fontFamily;
 fontItalic:bool;
 fontAlternative:fontAlternative;
 fontFeatures:string list;
 fontColor:OutputCommon.color;
 font:font;
 size:float;
 normalMeasure:float;
 normalLead:float;
 normalLeftMargin:float;
 par_indent:'a box list;
 stdGlue:'a box list;
 hyphenate:string->(string*string) array;
 substitutions:glyph_id list -> glyph_id list;
 positioning:glyph_ids list -> glyph_ids list;
 counters:(int list) StrMap.t;
 user_positions:Util.line TS.UMap.t;
 mutable fixable:bool
}

(* Type du contenu. B est une boîte quelconque. Les espaces dans T
   seront transformés en la boîte stdGlue de l'environnement, qui
   n'est pas nécessairement une GlueBox *)
and 'a content=
    B of ('a environment->'a box list)
  | BFix of ('a environment->'a box list)
  | C of ('a environment->'a content list)
  | CFix of ('a environment->'a content list)
  | T of string
  | FileRef of (string*int*int)
  | Env of ('a environment -> 'a environment)
  | Scoped of ('a environment->'a environment)*('a content list)

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
      fontColor=OutputCommon.black;
      font=f;
      substitutions=
        (fun glyphs -> List.fold_left apply glyphs (
           Fonts.select_features f [ Opentype.standardLigatures ]
         ));
      positioning=positioning f;
      size=4.;
      normalMeasure=150.;
      normalLead=5.;
      normalLeftMargin=(fst a4-.150.)/.2.;
      par_indent = [Drawing { drawing_min_width= 4.0 *. phi;
                              drawing_max_width= 4.0 *. phi;
                              drawing_y0=0.;drawing_y1=0.;
                              drawing_nominal_width= 4.0 *. phi;
                              drawing_contents=(fun _->[]);
                              drawing_badness=fun _-> 0. }];
      stdGlue=[Glue { drawing_min_width= 2.*. fsize/.9.;
                      drawing_max_width= fsize/.2.;
                      drawing_y0=0.; drawing_y1=0.;
                      drawing_nominal_width= fsize/.3.;
                      drawing_contents=(fun _->[]);
                      drawing_badness=knuth_h_badness (fsize/.3.) }];
      hyphenate=
        (fun str->
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
               | h::s->(hyph s 0 h; pos));

      counters=StrMap.singleton "structure" [0];
      user_positions=TS.UMap.empty;
      fixable=false
    }



(****************************************************************)



(* Le jeu est de construire la structure de document suivante :
   C'est un arbre, avec du contenu texte à chaque nœud. *)


let empty : user node={ name=""; in_toc = true;
                        displayname = []; children=IntMap.empty;
                        node_env=(fun x->x);
                        node_post_env=(fun x _->x);
                        tree_paragraph= (-1) }

type 'a cxt=(int*'a tree) list
let next_key t=try fst (IntMap.max_binding t)+1 with Not_found -> 0


let child (t,cxt) i=try
  match t with
      Node x->(IntMap.find i x.children, (i,t)::cxt)
    | _->(Node empty, (i,t)::cxt)
with
    Not_found -> (Node empty, (i,t)::cxt)

let newChild (t,cxt) chi=
  match t with
      Node x->(chi, (next_key x.children,t)::cxt)
    | _->(chi, (0,Node empty)::cxt)

let up (t,cxt) = match cxt with
    []->(t,cxt)
  | (a,Node b)::s->(Node { b with children=IntMap.add a t b.children }, s)
  | (a,b)::s->(Node { empty with children=IntMap.singleton a t }, s)

let rec top (a,b)=if b=[] then (a,b) else top (up (a,b))

let rec follow t l=match l with
    []->t
  | (a,_)::s->follow (child t a) s

(* La structure actuelle *)
let initTree ()=ref (Node empty, [])
let str=initTree ()
(* Le chemin vers le noeud courant *)

let fixable=ref false

(* Sortie en dot de la structure du document *)
let doc_graph out t0=
  Printf.fprintf out "digraph {\n";
  let rec do_it path t=
    Printf.fprintf out "%s [label=\"%s\"];\n" path t.name;
    List.iter (fun (i,x)->match x with
                   Paragraph _
                 | FigureDef _-> ()
                 | Node n->(
                     let p=path^"_"^(string_of_int i) in
                       Printf.fprintf out "%s -> %s;\n" path p;
                       do_it p n)) (IntMap.bindings t.children)
  in
    (match t0 with
         Node t->do_it "x0" t
       | _->());
    Printf.fprintf out "}\n"



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
      
let italic t = [ Scoped(envItalic true, t) ]

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
                 stdGlue=List.map (resize (fsize/.env.size)) env.stdGlue }), t)

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




let parameters env paragraphs figures last_parameters last_users (line:Util.line)=
  let lead=5. in
  let mes0=150. in
  let measure=ref mes0 in
  let page_footnotes=ref 0 in
    TS.UMap.iter (fun k a->
                    if a.isFigure then (
                      if line.page=a.page &&
                        line.height<=
                        a.height +.
                          (ceil ((figures.(TS.User.figureNumber k)).drawing_y1-.
                                   figures.(TS.User.figureNumber k).drawing_y0))
                      then
                        measure:=mes0 -. figures.(TS.User.figureNumber k).drawing_nominal_width -. 1.
                    ) else (
                      match k with
                          Footnote _ when a.page=line.page -> incr page_footnotes
                        | _->()
                    )) last_users;
    let footnote_h=fold_left_line paragraphs (fun fn box->match box with
                                                  User (Footnote (_,x))->fn+.x.drawing_y1-.x.drawing_y0
                                                | _->fn) 0. line
    in
      { measure= !measure;
        page_height=(if line.page_line <= 0 then 45.*.lead else last_parameters.page_height)
        -. (if footnote_h>0. && !page_footnotes=0 then (footnote_h+.2.*.lead) else footnote_h);
        left_margin=(fst a4-.150.)/.2.;
        local_optimization=0;
        min_page_diff=0;
        next_acceptable_height=(fun _ h->lead*.(1.+.ceil (h/.lead)));
        min_height_before=0.;
      }


let center env paragraphs figures last_parameters lastUsers l=
  let param=parameters env paragraphs figures last_parameters lastUsers l in
  let b=l.nom_width in
    if param.measure >= b then
      { param with measure=b; left_margin=param.left_margin +. (param.measure-.b)/.2. }
    else
      param


let figure ?(name="") ?(parameters=center) drawing=
  str:=up (newChild !str
             (FigureDef (name,drawing,parameters)))


(****************************************************************)




let newPar ?(structure=str) ?(environment=(fun x->x)) complete parameters par=
  let para=Paragraph {par_contents=par; par_env=environment;
                      par_post_env=(fun env1 env2 -> { env1 with counters=env2.counters; user_positions=env2.user_positions });
                      parameters=parameters; completeLine=complete }
  in
    structure:=up (newChild !structure para)


let string_of_contents l =
  let s = ref "" in
  List.iter (function
    T str ->
      if !s = "" then s:= str else s:= !s ^" " ^str
  | _ -> ()) l;
  !s

let newStruct ?(structure=str)  ?label displayname =
  let name = match label with
      None -> string_of_contents displayname
    | Some s -> s
  in

  let para=Node { 
    empty with
      name=name;
      displayname =displayname;
      node_env=(
        fun env->
          { env with
              counters=StrMap.add "structure" (
                try
                  0::(StrMap.find "structure" env.counters)
                with
                    Not_found -> [0]
              ) env.counters }
      );
      node_post_env=(
        fun env env'->
          { env with
              counters=StrMap.add "structure" (
                try
                  match StrMap.find "structure" env'.counters with
                      _::h::s->(h+1)::s
                    | _ -> [0]
                with
                    Not_found -> [0]
              ) env'.counters }
      );
  }
  in
  let section_name=[
    Scoped ((fun env->
               let path=drop 1 (try StrMap.find "structure" env.counters with Not_found -> []) in
                 { (envAlternative (Opentype.oldStyleFigures::env.fontFeatures) Caps env) with
                     size=(if List.length path = 1 then sqrt phi else sqrt (sqrt phi))*.env.size
                 }),
            B (fun env->
                 let path=drop 1 (try StrMap.find "structure" env.counters with Not_found -> []) in
                 [User (Structure path)])::
              [C (fun env->
                    let path=drop 1 (try StrMap.find "structure" env.counters with Not_found -> []) in
                      T (String.concat "." (List.map (fun x->string_of_int (x+1)) (List.rev path))) ::
                        (B (fun env->env.stdGlue))::
                        displayname
                 )]
           )
  ] in
    structure:=newChild !structure para;
    let lead = 5. in
    let par={
      par_contents=section_name;
      par_env=(fun x->{ x with par_indent=[]});
      par_post_env=(fun env1 env2 -> { env1 with counters=env2.counters; user_positions=env2.user_positions });
      parameters=
        (fun a b c d e f->
           { (parameters a b c d e f) with
               next_acceptable_height=(fun _ h->h+.lead*.2.); min_height_before=2.*.lead });
      completeLine=C.normal (fst a4) }
    in
      structure:=up (newChild !structure (Paragraph par))





let newStruct' ?(structure=str) ?label displayname =
  let name = match label with
      None -> string_of_contents displayname
    | Some s -> s
  in

  let para=Node { 
    empty with
      name=name;
      displayname =displayname;
      in_toc = false;
      node_env=(
        fun env->
          { env with
              counters=StrMap.add "structure" (
                try
                  0::(StrMap.find "structure" env.counters)
                with
                    Not_found -> [0]
              ) env.counters }
      );
      node_post_env=(
        fun env env'->
          { env with
              counters=StrMap.add "structure" (
                try
                  match StrMap.find "structure" env'.counters with
                      _::s->s
                    | [] -> [0]
                with
                    Not_found -> [0]
              ) env'.counters }
      );
  }
  in
  let section_name=[
    Scoped ((fun env->
               let path=drop 1 (try StrMap.find "structure" env.counters with Not_found -> []) in
                 { (envAlternative (Opentype.oldStyleFigures::env.fontFeatures) Caps env) with
                     size=(if List.length path = 1 then sqrt phi else sqrt (sqrt phi))*.env.size
                 }),
              displayname
           )
  ] in
    structure:=newChild !structure para;
    let lead = 5. in
    let par={
      par_contents=section_name;
      par_env=(fun x->{ x with par_indent=[]});
      par_post_env=(fun env1 env2 -> { env1 with counters=env2.counters; user_positions=env2.user_positions });
      parameters=
        (fun a b c d e f->
           { (parameters a b c d e f) with
               next_acceptable_height=(fun _ h->h+.lead*.2.); min_height_before=2.*.lead });
      completeLine=C.normal (fst a4) }
    in
      structure:=up (newChild !structure (Paragraph par))



let title ?label displayname =
  let name = match label with
      None -> string_of_contents displayname
    | Some s -> s
  in
  let (t,path)= !str in
  let (t0,_)=top !str in
  let t0'=
    match t0 with
        Paragraph _ | FigureDef _->Node { name=name; in_toc = false;
                                          displayname = displayname; 
				          children=IntMap.singleton 1 t0;
                                          node_env=(fun x->x);
                                          node_post_env=(fun x _->x);
                                          tree_paragraph=0 }
      | Node n -> Node { n with name=name; displayname = displayname }
  in
    str:=follow (t0',[]) path







(* Fonctions auxiliaires qui produisent un document optimisable à
   partir de l'arbre *)



let is_space c=c=' ' || c='\n' || c='\t'
let sources=ref StrMap.empty

let rec boxify env=function
    []->([], env)
  | (B b)::s->let u,v=boxify env s in (b env@u, v)
  | (C b)::s->(boxify env ((b env)@s))
  | (CFix b)::s->(fixable:=true; boxify env ((b env)@s))
  | (BFix b)::s->(fixable:=true; let u,v=boxify env s in (b env)@u,v)
  | Env f::s->boxify (f env) s
  | (T t)::s->(
    let rec cut_str i0 i result=
      if i>=String.length t then (
        if i0<>i then (
          if result<>[] then
            result @ (env.stdGlue @ (hyphenate env.hyphenate env.substitutions env.positioning env.font env.size
                                        env.fontColor
                                        (String.sub t i0 (i-i0))))
          else
            hyphenate env.hyphenate env.substitutions env.positioning env.font env.size env.fontColor (String.sub t i0 (i-i0))
        ) else result
      ) else (
        if is_space t.[i] then
          cut_str (i+1) (i+1) (
            if i0<>i then (
              if result<>[] then
                result @ (env.stdGlue @ (hyphenate env.hyphenate env.substitutions env.positioning env.font env.size env.fontColor
                                            (String.sub t i0 (i-i0))))
              else
                hyphenate env.hyphenate env.substitutions env.positioning env.font env.size env.fontColor (String.sub t i0 (i-i0))
            ) else result
          )
        else (
          cut_str i0 (i+1) result
        )
      )
    in
    let c=cut_str 0 0 [] in
    let u,v=boxify env s in
      c@u, v
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
  | Scoped (fenv, p)::s->(
      let env'=fenv env in
      let b,_=boxify env' p in
      let u,v=boxify env s in
        b@u, v
  )


let boxify_scoped env x=fst (boxify env x)

let flatten env0 str=
  let paragraphs=ref [] in
  let figures=ref IntMap.empty in
  let figure_names=ref StrMap.empty in
  let fig_param=ref IntMap.empty in
  let param=ref [] in
  let compl=ref [] in
  let n=ref 0 in


  let add_paragraph env p=
    let u,v=boxify (p.par_env env) p.par_contents in
    paragraphs:=(Array.of_list u)::(!paragraphs);
    compl:=(p.completeLine)::(!compl);
    param:=(p.parameters env)::(!param);
    incr n;
    v
  in

  let rec flatten env tree=
    match tree with
        Paragraph p -> add_paragraph env p
      | FigureDef (name, f, p) -> (
          let n=IntMap.cardinal !figures in
          let w=try let (_,_,w)=StrMap.find name !names in w with Not_found -> uselessLine in
            names := StrMap.add name (Figure n,"",w) !names;
            fig_param:=IntMap.add n (p env) !fig_param;
            figures:=IntMap.add n (f env) !figures;
            env
        )
      | Node s-> (
          s.tree_paragraph <- !n;
            let rec flat_children indent env1= function
                []->env1
              | (_, (Paragraph p))::s->(
                  let env2=flatten env1 (
                    Paragraph { p with par_contents=
                        (if indent then [B (fun env->(p.par_env env).par_indent)] else []) @ p.par_contents }
                  ) in
                    flat_children true (p.par_post_env env1 env2) s
                )
              | (_,(FigureDef _ as h))::s->(
                  let env2=flatten env1 h in
                    flat_children indent env2 s
                )
              | (_, (Node h as tr))::s->(
                  let env2=h.node_env env1 in
                  let env3=flatten env2 tr in
                    flat_children false (h.node_post_env env1 env3) s
                )
            in
              flat_children false env (IntMap.bindings s.children)
        )
  in

    flatten env0 str;
    let figures_resolved=
      List.map (Array.map (function
                               User (NamedCitation s) -> User (match StrMap.find s !names with
                                                                   (Figure n,_,_)->FigureRef n
                                                                 | _->raise Not_found)
                             | x -> x)) !paragraphs
    in
    let params=Array.make (IntMap.cardinal !figures) (center env0) in
      IntMap.iter (fun n p->params.(n)<-p) !fig_param;
      (params,
       Array.of_list (List.rev !param),
       Array.of_list (List.rev !compl),
       Array.of_list (List.rev figures_resolved),
       Array.of_list (List.map snd (IntMap.bindings !figures)))


let rec make_struct positions tree=
  match tree with
      Paragraph _ | FigureDef _->
          { OutputCommon.name="";
	    OutputCommon.displayname=[];
            OutputCommon.page=0;
            OutputCommon.struct_x=0.;
            OutputCommon.struct_y=0.;
            OutputCommon.substructures=[||] }
    | Node s-> (
        let (p,x,y)=positions.(s.tree_paragraph) in
        let rec make=function
            []->[]
          | (_,Paragraph _) :: s | (_,FigureDef _) :: s->make s
          | (_,Node u)::s -> (make_struct positions (Node u))::(make s)
        in
        let a=Array.of_list (make (IntMap.bindings s.children)) in
          { OutputCommon.name=s.name;
	    OutputCommon.displayname=[] (* FIXME boxify ?env [T s.name] *);
            OutputCommon.page=p;
            OutputCommon.struct_x=x;
            OutputCommon.struct_y=y;
            OutputCommon.substructures=a }
      )


let table_of_contents tree max_level=
  newPar ~environment:(fun x->{x with par_indent=[]}) (C.normal 150.) parameters [
    BFix (
      fun env->
        let x0=75. in
        let spacing=1. in
        let r=0.3 in
        let x_height=
          let x=Fonts.loadGlyph env.font ({empty_glyph with glyph_index=Fonts.glyph_of_char env.font 'x'}) in
            (Fonts.glyph_y1 x)/.1000.
        in
        let orn=translate x0 (env.size*.x_height/.2.-.r) (Path ({default with fillColor=Some black;strokingColor=None }, [circle r])) in
        let (orn_x0,_,orn_x1,_)=bounding_box [orn] in
        let y=orn_x1-.orn_x0 in
        let rec toc env0 level tree=
          match tree with
              Paragraph p -> []
            | FigureDef (name, f, p) -> []
            | Node s when level <= max_level-> (
                let rec flat_children env1=function
                    []->[]
                  | (_,(FigureDef _))::s
                  | (_,(Paragraph _))::s->flat_children env1 s
                  | (_,(Node h as tr))::s->(
                      let env'=h.node_env env1 in
                        (toc env' (level+1) tr)@
                          flat_children (h.node_post_env env1 env') s
                    )
                in
                let chi=flat_children env0 (IntMap.bindings s.children) in

                let count=drop 1 (try StrMap.find "structure" (env0.counters) with _->[]) in
                  if s.in_toc && count<>[] then (
                    try
                      let page=(1+(TS.UMap.find (Structure count) env0.user_positions).Util.page) in
                      let fenv env={ env with
                                       substitutions=(fun glyphs->
                                                        List.fold_left Fonts.FTypes.apply (env.substitutions glyphs)
                                                          (Fonts.select_features env.font [ Opentype.oldStyleFigures ]))
                                   }
                      in
                      let env'=fenv env0 in
                      let name= boxify_scoped env' s.displayname in
                      let w=List.fold_left (fun w b->let (_,w',_)=box_interval b in w+.w') 0. name in
                      let cont=
                        (List.map (translate (x0-.w-.spacing) 0.) (draw_boxes name))@
                          orn::
                          List.map (translate (x0+.y+.spacing) 0.)
                          (draw_boxes (boxify_scoped (fenv (envItalic true env0)) [T (Printf.sprintf "page %d" page)]))
                      in
                      let (a,b,c,d)=OutputCommon.bounding_box cont in
                        Drawing {
                          drawing_min_width=150.;
                          drawing_nominal_width=150.;
                          drawing_max_width=150.;
                          drawing_y0=b;
                          drawing_y1=d;
                          drawing_badness=(fun _->0.);
                          drawing_contents=(fun _->cont)
                        }::(glue 0. 0. 0.)::chi
                    with
                        Not_found -> chi
                  )
                  else chi
              )
            | Node _->[]
        in
          toc { env with counters=StrMap.add "structure" [0] env.counters }
            0 (fst (top !str))
    )]


let update_names user=
  (* TS.UMap.iter(fun k a->(match k with *)
  (*                            Structure l->(Printf.printf "->Structure ";List.iter (Printf.printf "%d ") l;Printf.printf "\n") *)
  (*                          | _->()); *)
  (*                print_line a *)
  (*              ) user; *)

  let needs_reboot=ref false in
    StrMap.iter (fun k (a,b,c)->try
                   (* Printf.printf "update : %s\n" k; print_line c; *)
                   (* (match a with *)
                   (*      Structure l->(Printf.printf "Structure ";List.iter (Printf.printf "%d ") l;Printf.printf "\n") *)
                   (*    | _->()); *)
                   let pos=TS.UMap.find a user in
                     (* if pos<>c then (print_line pos;print_line c;Printf.printf "\n");flush stdout; *)
                     needs_reboot:= !needs_reboot || (pos<>c);
                     names:=StrMap.add k (a,b,pos) !names;
                 with Not_found -> needs_reboot:=true
                ) !names;
    !needs_reboot

let pageref x=
  [CFix (fun env->try
           let (_,_,node)=StrMap.find x !names in
             [T (string_of_int (1+node.page))]
         with Not_found -> []
        )]

let label name=
  [B (fun env ->
        let w=try let (_,_,w)=StrMap.find name !names in w with Not_found -> uselessLine in
          names:=StrMap.add name (Structure (drop 1 (StrMap.find "structure" env.counters)),
                                  "",w) !names;
          let path=drop 1 (try StrMap.find "structure" env.counters with Not_found -> []) in
            [User (Structure path)])
  ]

let sectref name=
  [ CFix (fun env->try
            match StrMap.find name !names with
                (Structure num,_,_)->
                  [T (String.concat "." (List.map (fun x->string_of_int (x+1))
                                           (List.rev num)))]
              | _->[]
          with
              Not_found -> []
         )]
