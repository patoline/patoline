(** La "classe" de documents par defaut. *)
open Config
(* let spec = [("--extra-fonts-dir",Arg.String (fun x->fontsdir:=x::(!fontsdir)), "Adds directories to the font search path")] *)
(* let _=Arg.parse spec ignore "Usage :" *)

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

(* font, substitutions, positioning *)

let simpleFamilyMember:font->font*(string->string)*(glyph_id list -> glyph_id list)*(glyph_ids list -> glyph_ids list) =
  fun a->(a,(fun x->x),(fun x->x),(fun x->x))

(* Italic is second *)
type fontFamily = (fontAlternative * ((font*(string->string)*(glyph_id list -> glyph_id list)*(glyph_ids list -> glyph_ids list)) Lazy.t * (font*(string->string)*(glyph_id list -> glyph_id list)*(glyph_ids list -> glyph_ids list)) Lazy.t)) list


let selectFont fam alt it =
  try
    let r, i = List.assoc alt fam in
    Lazy.force (if it then i else r)
  with Not_found ->
    (* FIXME: keep the font name and print a better message *)
    Printf.fprintf stderr "Font not found in family.\n";
    exit 1


type user=
    Label of string
  | FigureRef of int
  | Pageref of string
  | Structure of int list
  | Figure of int
  | BeginFigure of int
  | FlushFigure of int
  | Footnote of int*drawingBox
  | AlignmentMark

module TS=Break.Make
  (struct
     type t=line
     let hash=Hashtbl.hash
     let compare=compare
   end)
  (struct
     type t=user
     let compare=compare
     let figureRef x=FigureRef x
     let figure x=Figure x
     let flushedFigure=function FlushFigure x -> x |_-> -1
     let beginFigure=function BeginFigure x -> x |_-> -1
     let isFigure=function Figure _->true | _->false
     let figureNumber=function Figure x->x | _-> -1
   end)

type tag=
    InTOC
  | Author of string
  | Institute of string
  | Numbered
  | Structural

type 'a node={
  name:string;
  displayname:'a content list;
  children:'a tree IntMap.t;
  node_tags:tag list;
  node_env:'a environment -> 'a environment;
  node_post_env:'a environment -> 'a environment -> 'a environment;
  mutable tree_paragraph:int;
}
and 'a paragraph={
  par_contents:'a content list;
  par_env:'a environment -> 'a environment;
  par_post_env:'a environment -> 'a environment -> 'a environment;
  par_parameters:'a environment -> 'a box array array -> drawingBox array -> parameters ->  Break.figurePosition IntMap.t ->line TS.UMap.t -> line -> parameters;
  par_completeLine:float -> 'a box array array -> drawingBox array -> Break.figurePosition IntMap.t ->line TS.UMap.t -> line -> bool -> line list
}
and 'a figuredef={
  fig_contents:'a environment->drawingBox;
  fig_env:'a environment -> 'a environment;
  fig_post_env:'a environment -> 'a environment -> 'a environment;
  fig_parameters:'a environment -> 'a box array array -> drawingBox array -> parameters -> Break.figurePosition IntMap.t -> line TS.UMap.t -> line -> parameters
}
and 'a tree=
    Node of 'a node
  | Paragraph of 'a paragraph
  | FigureDef of 'a figuredef

and 'a environment={
  fontFamily:fontFamily;
  fontItalic:bool;
  fontAlternative:fontAlternative;
  fontFeatures:string list;
  fontColor:OutputCommon.color;
  font:font;
  size:float;
  lead:float;
  footnote_y:float;
  normalMeasure:float;
  normalLead:float;
  normalLeftMargin:float;
  par_indent:'a box list;
  stdGlue:'a box list;
  hyphenate:string->(string*string) array;
  word_substitutions:string->string;
  substitutions:glyph_id list -> glyph_id list;
  positioning:glyph_ids list -> glyph_ids list;
  counters:(int*int list) StrMap.t;
  names:((int*int list) StrMap.t * string * line) StrMap.t;
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

module type DocumentStructure=sig
  val structure:(user tree*(int*user tree) list) ref
  val fixable:bool ref
end
module type Format=sig
  type user
  val defaultEnv:user environment
  val postprocess_tree:user tree->user tree
  val title:
    (user tree * ((user tree) list)) ref ->
    ?label:string ->
    ?displayname:user content list ->
    string -> unit
  val author:string->unit
  val institute:string->unit
end

let incr_counter ?(level= -1) env name=
  { env with counters=
      StrMap.add name (try let a,b=StrMap.find name env.counters in
                         match b with
                             h::s -> (a,(h+1)::s)
                           | _->a,[0]
                       with
                           Not_found -> level, [0]
                      ) env.counters }

let pop_counter env name=
  { env with counters=
      StrMap.add name (let a,b=StrMap.find name env.counters in (a,drop 1 b)) env.counters }

let push_counter env name=
  { env with counters=
      StrMap.add name (let a,b=StrMap.find name env.counters in (a,0::b)) env.counters }

let tags=function
    Node n->n.node_tags
  | _->[]
(****************************************************************)



(* Le jeu est de construire la structure de document suivante :
   C'est un arbre, avec du contenu texte à chaque nœud. *)


let empty : user node={ name="";
                        node_tags=[];
                        displayname = []; children=IntMap.empty;
                        node_env=(fun x->x);
                        node_post_env=(fun x y->{ x with
                                                    counters=y.counters;
                                                    names=y.names;
                                                    user_positions=y.user_positions });
                        tree_paragraph=0 }

type 'a cxt=(int*'a tree) list
let next_key t=try fst (IntMap.max_binding t)+1 with Not_found -> 0
let prev_key t=try fst (IntMap.min_binding t)-1 with Not_found -> 0


let child (t,cxt) i=try
  match t with
      Node x->(IntMap.find i x.children, (i,t)::cxt)
    | _->(Node empty, (i,t)::cxt)
with
    Not_found -> (Node empty, (i,t)::cxt)

let newChildAfter (t,cxt) chi=
  match t with
      Node x->(chi, (next_key x.children,t)::cxt)
    | _->(chi, (0,Node empty)::cxt)

let newChildBefore (t,cxt) chi=
  match t with
      Node x->(chi, (prev_key x.children,t)::cxt)
    | _->(chi, (0,Node empty)::cxt)

let up (t,cxt) = match cxt with
    []->(t,cxt)
  | (a,Node b)::s->(Node { b with children=IntMap.add a t b.children }, s)
  | (a,b)::s->(Node { empty with children=IntMap.singleton a t }, s)
let go_up str=
  (if snd !str=[] then Printf.fprintf stderr "Warning : go_up\n");
  str:=(up !str)

let rec top (a,b)=if b=[] then (a,b) else top (up (a,b))

let rec follow t l=match l with
    []->t
  | (a,_)::s->follow (child t a) s

(* La structure actuelle *)
(* let str=Printf.printf "str : init\n";ref [(Node empty,[])] *)
(* Le chemin vers le noeud courant *)

(* Sortie en dot de la structure du document *)
let doc_graph out t0=
  Printf.fprintf out "digraph {\n";
  let rec do_it path t=
    let col=
      if List.mem Structural t.node_tags then
        if List.mem Numbered t.node_tags then "blue" else "red" else "black" in
    Printf.fprintf out "%s [label=\"%s\", color=\"%s\"];\n" path t.name col;
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

let change_env t fenv=match t with
    (Node n,l)->(Node { n with node_env=fun x->fenv (n.node_env x) }, l)
  | (Paragraph n,l)->(Paragraph { n with par_env=fun x->fenv (n.par_env x) }, l)
  | (FigureDef f, l)->
      FigureDef {f with fig_env=fun x->fenv (f.fig_env x) }, l
(****************************************************************)


(* Quelques Exemples d'environnement *)

let updateFont env font str subst pos=
  let feat=Fonts.select_features font env.fontFeatures in
    { env with
        font=font;
        word_substitutions=str;
        substitutions=(fun glyphs -> List.fold_left apply (subst glyphs) feat);
        positioning=(fun x->pos (positioning font x)) }

(* Changer de font dans un scope, ignore la famille, attention, à éviter en direct *)
let font f t=
  let font=loadFont f in
    [Scoped ((fun env-> updateFont env font (fun x->x) (fun x->x) (fun x->x)), t)]

let envItalic b env =
  let font, str,subst, pos= selectFont env.fontFamily env.fontAlternative b in
  let env = { env with fontItalic = b } in
    updateFont env font str subst pos

let italic t = [ Scoped(envItalic true, t) ]

(* module Italic = struct *)
(*   let do_begin_Italic () = () *)
(*   let do_end_Italic () = () *)
(*   let defaultEnv = envItalic true defaultEnv *)
(* end *)

(* module Env_Italic = Italic *)

let notItalic t =
  [Scoped (envItalic false, t)]

let toggleItalic t =
  [Scoped ((fun env -> envItalic (not env.fontItalic) env), t)]
 
let envAlternative features alt env =
  let font,str,subs,pos = selectFont env.fontFamily alt env.fontItalic in
  let env = { env with fontAlternative = alt } in
  updateFont env font str subs pos
 
let alternative ?features alt t =
  [Scoped ((fun env -> 
    let features = match features with
	None -> env.fontFeatures
      | Some f -> f
    in
    envAlternative features alt env), t)]

let envFamily fam env =
  let font,str,subs,pos = selectFont fam env.fontAlternative env.fontItalic in
  let env = { env with fontFamily = fam } in
  updateFont env font str subs pos

let family fam t =
  [Scoped ((fun env -> envFamily fam env), t)]

let resize_env fsize env=
  { env with
      size=fsize;
      lead=env.lead*.fsize/.env.size;
      stdGlue=List.map (resize (fsize/.env.size)) env.stdGlue }

(* Changer de taille dans un scope *)
let size fsize t=
  Scoped (resize_env fsize, t)

(* Rajouter une liste de features, voir Fonts.FTypes pour savoir ce
   qui existe *)
let add_features features env=
  let feat=Fonts.select_features env.font (features@env.fontFeatures) in
    { env with
        fontFeatures=features@env.fontFeatures;
        substitutions=(fun glyphs -> List.fold_left apply (env.substitutions glyphs) feat);
    }

(****************************************************************)



(* Partie compliquée : il faut comprendre ce que fait l'optimiseur
   pour toucher à ça, ou apprendre en touchant ça *)




let parameters env paragraphs figures last_parameters last_figures last_users (line:Util.line)=
  let measure=ref env.normalMeasure in
  let page_footnotes=ref 0 in
    IntMap.iter (fun i aa->match aa with
                     Break.Placed a->
                       if line.page=a.page &&
                         line.height<=
                         a.height +.
                           (ceil ((figures.(i)).drawing_y1-.
                                    figures.(i).drawing_y0))
                       then
                         measure:=env.normalMeasure -. figures.(i).drawing_nominal_width -. 1.
                   | _->()
                ) last_figures;

    TS.UMap.iter (fun k a->
                    match k with
                        Footnote _ when a.page=line.page -> incr page_footnotes
                      | _->()
                 ) last_users;
    let footnote_h=fold_left_line paragraphs (fun fn box->match box with
                                                  User (Footnote (_,x))->fn+.x.drawing_y1-.x.drawing_y0
                                                | _->fn) 0. line
    in
      { measure= !measure;
        page_height=(if line.page_line <= 0 then 45.*.env.normalLead else last_parameters.page_height)
        -. (if footnote_h>0. && !page_footnotes=0 then (footnote_h+.env.footnote_y) else footnote_h);
        left_margin=env.normalLeftMargin;
        local_optimization=0;
        min_page_before=0;
        min_page_after=0;
        next_acceptable_height=(fun node params nextNode nextParams->
                                  if node==nextNode then
                                    let min_height=node.height+.params.min_height_after in
                                      env.lead*.(ceil (min_height/.env.lead))
                                  else
                                    if node.page=nextNode.page then
                                      let min_height=max (nextNode.height+.env.lead) (node.height +. max params.min_height_after nextParams.min_height_before) in
                                        env.lead*.(ceil (min_height/.env.lead))
                                    else
                                      let min_height=nextNode.height+.max nextParams.min_height_before env.lead in
                                        env.lead*.(ceil (min_height/.env.lead))
                               );
        min_height_before=env.lead;
        min_height_after=env.lead;
      }


let center env paragraphs figures last_parameters lastFigures lastUsers l=
  let param=parameters env paragraphs figures last_parameters lastFigures lastUsers l in
  let b=l.nom_width in
    if param.measure >= b then
      { param with measure=b; left_margin=param.left_margin +. (param.measure-.b)/.2. }
    else
      param
let ragged_left a b c d e f line=
  let par=parameters a b c d e f line in
  { par with measure=line.nom_width }

let ragged_right a b c d e f line=
  let par=parameters a b c d e f line in
  { par with
    measure=line.nom_width;
    left_margin=par.left_margin+.par.measure-.line.nom_width }

let in_text_figure a b c d e f line=
  let par=parameters a b c d e f line in
  { par with
    measure=line.nom_width;
    left_margin=par.left_margin+.par.measure-.line.nom_width }


let figure str ?(parameters=center) ?(name="") drawing=
    str:=up (newChildAfter !str (FigureDef { fig_contents=drawing;
                                        fig_env=(fun x->
                                                   let l,cou=try StrMap.find "figures" x.counters with Not_found -> -1, [-1] in
                                                   let cou'=StrMap.add "figures" (l,match cou with []->[0] | h::_->[h+1]) x.counters in
                                                     { x with
                                                         names=if name="" then x.names else (
                                                           let w=try let (_,_,w)=StrMap.find name x.names in w with Not_found -> uselessLine in
                                                             StrMap.add name (cou', "figure", w) x.names
                                                         );
                                                         counters=cou'
                                                     });
                                        fig_post_env=(fun x y->{ x with names=y.names; counters=y.counters; user_positions=y.user_positions });
                                        fig_parameters=parameters }))

let flush_figure name=
  [BFix (fun env->
        try
          match StrMap.find "figures" env.counters with
              _,h::_->[User (FlushFigure h)]
            | _->[]
        with
            Not_found -> []
     )]
let begin_figure name=
  [BFix (fun env->
        try
          match StrMap.find "figures" env.counters with
              _,h::_->[User (BeginFigure h)]
            | _->[]
        with
            Not_found -> []
     )]

(****************************************************************)




let newPar str ?(environment=(fun x->x)) complete parameters par=
  let para=Paragraph {par_contents=par; par_env=environment;
                      par_post_env=(fun env1 env2 -> { env1 with names=env2.names; counters=env2.counters; user_positions=env2.user_positions });
                      par_parameters=parameters; par_completeLine=complete }
  in
    str:=up (newChildAfter !str para)


let string_of_contents l =
  let s = ref "" in
  List.iter (function
    T str ->
      if !s = "" then s:= str else s:= !s ^" " ^str
  | _ -> ()) l;
  !s

let newStruct str ?(in_toc=true) ?label ?(numbered=true) displayname =
  let name = match label with
      None -> string_of_contents displayname
    | Some s -> s
  in

  let para=Node {
    empty with
      name=name;
      displayname =displayname;
      node_tags= (if in_toc then [InTOC] else []) @ [Structural] @(if numbered then [Numbered] else []);
      node_env=(
        fun env->
          { env with
              counters=StrMap.add "structure" (
                try
                  let (a,b)=StrMap.find "structure" env.counters in
                    a,0::b
                with
                    Not_found -> (-1,[0;0])
              ) env.counters }
      );
      node_post_env=(
        fun env env'->
          { env with
              names=env'.names;
              user_positions=env'.user_positions;
              counters=StrMap.add "structure" (
                try
                  let a,b=StrMap.find "structure" env'.counters in
                  match b with
                      _::h::s when numbered ->a,(h+1)::s
                    | _::h::s ->a,h::s
                    | _ -> a, [0]
                with
                    Not_found -> -1,[0]
              ) env'.counters }
      );
  }
  in
    str:=newChildAfter !str para


(* Fonctions auxiliaires qui produisent un document optimisable à
   partir de l'arbre *)



let is_space c=c=' ' || c='\n' || c='\t'
let sources=ref StrMap.empty

let rec boxify fixable env=function
    []->([], env)
  | (B b)::s->let u,v=boxify fixable env s in (b env@u, v)
  | (C b)::s->(boxify fixable env ((b env)@s))
  | (CFix b)::s->(fixable:=true; boxify fixable env ((b env)@s))
  | (BFix b)::s->(fixable:=true; let u,v=boxify fixable env s in (b env)@u,v)
  | Env f::s->boxify fixable (f env) s
  | (T t)::s->(
    let rec cut_str i0 i result=
      if i>=String.length t then (
        if i0<>i then (
          if result<>[] then
            result @ (env.stdGlue @ (hyphenate env.hyphenate env.substitutions env.positioning env.font env.size
                                        env.fontColor
                                        (env.word_substitutions (String.sub t i0 (i-i0)))))
          else
            hyphenate env.hyphenate env.substitutions env.positioning env.font env.size env.fontColor (env.word_substitutions (String.sub t i0 (i-i0)))
        ) else result
      ) else (
        if is_space t.[i] then
          cut_str (i+1) (i+1) (
            if i0<>i then (
              if result<>[] then
                result @ (env.stdGlue @ (hyphenate env.hyphenate env.substitutions env.positioning env.font env.size env.fontColor
                                           (env.word_substitutions (String.sub t i0 (i-i0)))))
              else
                hyphenate env.hyphenate env.substitutions env.positioning env.font env.size env.fontColor (env.word_substitutions (String.sub t i0 (i-i0)))
            ) else result
          )
        else (
          cut_str i0 (i+1) result
        )
      )
    in
    let c=cut_str 0 0 [] in
    let u,v=boxify fixable env s in
      c@u, v
  )
  | FileRef (file,off,size)::s -> (
    let i=try 
	    StrMap.find file !sources 
      with _-> (let i=open_in file in sources:= StrMap.add file i !sources; i) 
    in
    let buf=String.create size in
    let _=seek_in i off; input i buf 0 size in
      boxify fixable env (T buf::s)
  )
  | Scoped (fenv, p)::s->(
      let env'=fenv env in
      let b,_=boxify fixable env' p in
      let u,v=boxify fixable env s in
        b@u, v
  )


let boxify_scoped env x=fst (boxify (ref false) env x)

let flatten env0 fixable str=
  let paragraphs=ref [] in
  let figures=ref IntMap.empty in
  let fig_param=ref IntMap.empty in
  let param=ref [] in
  let compl=ref [] in
  let n=ref 0 in


  let add_paragraph env p=
    let u,v=boxify fixable env p.par_contents in
      paragraphs:=u::(!paragraphs);
      compl:=(p.par_completeLine env.normalMeasure)::(!compl);
      param:=(p.par_parameters env)::(!param);
      incr n;
      v
  in

  let rec flatten env path tree=
    let level=List.length path in
    match tree with
        Paragraph p -> add_paragraph env p
      | FigureDef f -> (
          let env1=f.fig_env env in
          let n=IntMap.cardinal !figures in
            fig_param:=IntMap.add n (f.fig_parameters env1) !fig_param;
            figures:=IntMap.add n (f.fig_contents env1) !figures;
            paragraphs:=(match !paragraphs with
                             []->[]
                           | h::s->(h@[User (BeginFigure n)])::s);
            f.fig_post_env env env1
        )
      | Node s-> (
          s.tree_paragraph <- List.length !paragraphs;
          let flushes=ref [] in
          let flat_children k a (indent, env1)=match a with
            | (Paragraph p)->(
                let env2=flatten (p.par_env env1) (k::path) (
                  Paragraph { p with par_contents=
                      (if indent then [B (fun env->(p.par_env env).par_indent)] else []) @ p.par_contents }
                ) in
                  true, p.par_post_env env1 env2
              )
            | FigureDef _ as h->(
                let env2=flatten env1 (k::path) h in
                  flushes:=(IntMap.cardinal !figures)::(!flushes);
                  indent,env2
              )
            | Node h as tr->(
                let env2=h.node_env env1 in
                let env2'=
                  let cou=
                    StrMap.add "path" (-1,k::path)
                      (if List.mem Structural h.node_tags then
                         StrMap.filter (fun _ (a,b)->a<=level) env2.counters
                       else
                         env2.counters)
                  in
                    {env2 with counters=cou }
                in
                let env3=flatten env2' (k::path) tr in
                  false, h.node_post_env env1 env3
              )
          in
          let _,env2=IntMap.fold flat_children s.children (false,env) in
            paragraphs:=(match !paragraphs with
                             []->[]
                           | h::s->(h@(List.map (fun x->User (FlushFigure x)) !flushes))::s);
            env2
        )
  in
  let env1=match str with
      Node n->n.node_env env0
    | Paragraph n->n.par_env env0
    | _->env0
  in
  let env2=flatten env1 [] str in
  let params=Array.make (IntMap.cardinal !figures) (center env0) in
    IntMap.iter (fun n p->params.(n)<-p) !fig_param;
    (env2, params,
     Array.of_list (List.rev !param),
     Array.of_list (List.rev !compl),
     Array.of_list (List.rev (List.map Array.of_list !paragraphs)),
     Array.of_list (List.map snd (IntMap.bindings !figures)))

let rec make_struct positions tree=
  match tree with
      Node s -> (
        let (p,x,y)=try positions.(s.tree_paragraph) with _->(0,0.,0.) in
        let rec make=function
            []->[]
          | (_,Node u)::s when List.mem InTOC u.node_tags -> (make_struct positions (Node u))::(make s)
          | _ :: s->make s
        in
        let a=Array.of_list (make (IntMap.bindings s.children)) in
          { OutputCommon.name=s.name;
	    OutputCommon.displayname=[] (* FIXME boxify ?env [T s.name] *);
            OutputCommon.page=p;
            OutputCommon.struct_x=x;
            OutputCommon.struct_y=y;
            OutputCommon.substructures=a }
      )
    | _->
        { OutputCommon.name="";
	  OutputCommon.displayname=[];
          OutputCommon.page=0;
          OutputCommon.struct_x=0.;
          OutputCommon.struct_y=0.;
          OutputCommon.substructures=[||] }




let update_names env figs user=
  let fil=TS.UMap.filter (fun k a->match k with Structure _->true |_->false) in
  let needs_reboot=ref (fil user<>fil env.user_positions) in
  let env'={ env with user_positions=user; counters=StrMap.empty; names=
      StrMap.fold (fun k (a,b,c) m->try
                     Printf.printf "%s %s %d %d\n" k b (IntMap.cardinal figs) ((List.hd (snd (StrMap.find "figures" a))));
                     let pos=
                       if b="figure" then
                         match IntMap.find (List.hd (snd (StrMap.find "figures" a))) figs with
                             Break.Placed l->l
                           | _->raise Not_found
                       else
                         TS.UMap.find (Label k) user
                     in
                       needs_reboot:= !needs_reboot || (pos<>c);
                       StrMap.add k (a,b,pos) m
                   with Not_found -> (Printf.printf "not found\n";needs_reboot:=true; m)
                  ) env.names env.names
           }
  in
    env',!needs_reboot

let pageref x=
  [CFix (fun env->try
           let (_,_,node)=StrMap.find x env.names in
             [T (string_of_int (1+node.page))]
         with Not_found -> []
        )]

let label ?(labelType="structure") name=
  [Env (fun env->
          let w=try let (_,_,w)=StrMap.find name env.names in w with Not_found -> uselessLine in
            { env with names=StrMap.add name (env.counters, labelType, w) env.names });

   B (fun env ->
        [User (Label name)])
  ]

let sectref name=
  [ CFix (fun env->try
            let (nums,_,_)=StrMap.find name env.names in
            let _,num=try StrMap.find "structure" nums with Not_found -> -1, [] in
              [T (String.concat "." (List.map (fun x->string_of_int (x+1))
                                       (List.rev (drop 1 num))))]
          with
              Not_found -> []
         )]

let generalRef ?(refType="structure") name=
  [ CFix (fun env->try
            let counters,_,_=StrMap.find name env.names in
            let lvl,num=(StrMap.find refType counters) in
            let _,str_counter=StrMap.find "structure" counters in
            let sect_num=drop (List.length str_counter - lvl) str_counter in
              [ T (String.concat "." (List.map (fun x->string_of_int (x+1)) (sect_num@num))) ]
          with
              Not_found -> []
         )]
