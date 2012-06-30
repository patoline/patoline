(** Le type du contenu, et comment on le transforme en boîtes. *)
let _=Gc.set {(Gc.get ()) with Gc.minor_heap_size=1 lsl 20 }
open Config
open Util
open Fonts
open Fonts.FTypes
open OutputCommon
open Box
open Line
open CamomileLibrary

type fontAlternative = Regular | Bold | Caps | Demi

(* font, substitutions, positioning *)

let simpleFamilyMember:(unit->font)->(font*(string->string)*(glyph_id list -> glyph_id list)*(glyph_ids list -> glyph_ids list)) Lazy.t =
  fun a->Lazy.lazy_from_fun (fun ()->(a (),(fun x->x),(fun x->x),(fun x->x)))

let make_ligature l gl x=
  let rec match_lig l x=match (l,x) with
      [],[]->Some []
    | _::_,[]->None
    | [],_->Some x
    | h1::s1, h2::s2 when h1=h2.glyph_index-> match_lig s1 s2
    | _::_,_::_->None
  in
  let rec make_ligature x=match x with
      []->[]
    | h::s->(
        match match_lig l x with
            None->h::make_ligature s
          | Some g->gl::make_ligature g
      )
  in
    make_ligature x

(* Italic is second *)
type fontFamily = (fontAlternative * ((font*(string->string)*(glyph_id list -> glyph_id list)*(glyph_ids list -> glyph_ids list)) Lazy.t * (font*(string->string)*(glyph_id list -> glyph_id list)*(glyph_ids list -> glyph_ids list)) Lazy.t)) list


(** C'est là qu'on veut des variants polymorphes, mais caml ne veut pas de mon module TS polymorphe en user *)
type user=
    Label of string
  | FigureRef of int
  | Pageref of string
  | Structure of int list
  | Footnote of int*drawingBox
  | BeginURILink of string
  | BeginLink of string
  | EndLink
  | AlignmentMark

(** Module de typesetting Ce module contient une map des boîtes
    user, mais ne fait aucune hypothèse de plus sur le type
    user.*)
module TS=Break.Make
  (struct
     type t=line
     let compare a b=
       if a.paragraph<b.paragraph then -1 else
         if a.paragraph>b.paragraph then 1 else

           if a.lineStart<b.lineStart then -1 else
             if a.lineStart>b.lineStart then 1 else

           if a.lineEnd<b.lineEnd then -1 else
             if a.lineEnd>b.lineEnd then 1 else

           if a.hyphenStart<b.hyphenStart then -1 else
             if a.hyphenStart>b.hyphenStart then 1 else

           if a.hyphenEnd<b.hyphenEnd then -1 else
             if a.hyphenEnd>b.hyphenEnd then 1 else

           if a.lastFigure<b.lastFigure then -1 else
             if a.lastFigure>b.lastFigure then 1 else

           if a.isFigure<b.isFigure then -1 else
             if a.isFigure>b.isFigure then 1 else

           if a.page<b.page then -1 else
             if a.page>b.page then 1 else

           if a.height<b.height then -1 else
             if a.height>b.height then 1 else
               0
   end)
  (struct
     type t=user
     let compare=compare
   end)


module Mathematical=struct
  type env={
    mathsFont:Fonts.font Lazy.t;
    mathsSize:float;
    mathsSubst:glyph_id list -> glyph_id list;
    numerator_spacing:float;
    denominator_spacing:float;
    sub1:float;
    sub2:float;
    sup1:float;
    sup2:float;
    sup3:float;
    sub_drop:float;
    sup_drop:float;
    default_rule_thickness:float;
    subscript_distance:float;
    superscript_distance:float;
    limit_subscript_distance:float;
    limit_superscript_distance:float;
    invisible_binary_factor:float;
    open_dist:float;
    close_dist:float;
    left_op_dist:float;
    right_op_dist:float;
    kerning:bool;
  (* None means precise, Some x mean unprecise, but subdivise 
     Bezier curve until the thickness of the polygon is less than x *)
    precise_kerning:float option;
    priorities:float array;
    priority_unit:float
  }
  and environment=env array             (* doit etre de taille 8 *)
  and style=
      Display
    | Display'
    | Text
    | Text'
    | Script
    | Script'
    | ScriptScript
    | ScriptScript'
end

(** Structure du document. Un [node] est un nœud interne de l'arbre. *)
type 'a node={
  name:string;
  displayname:'a content list;
  children:'a tree IntMap.t;       (** Les [int] qui sont là n'ont rien à voir avec la numérotation officielle, c'est juste un tableau extensible. *)
  node_tags:(string*string) list;
  node_env:'a environment -> 'a environment; (** Changement d'environnement quand on rentre dans le nœud *)
  node_post_env:'a environment -> 'a environment -> 'a environment;(** Changement d'environnement quand on en sort *)
  mutable tree_paragraph:int;
}
and 'a paragraph={
  par_contents:'a content list;
  par_env:'a environment -> 'a environment;
  par_post_env:'a environment -> 'a environment -> 'a environment;
  par_parameters:'a environment -> 'a box array array -> drawingBox array -> parameters ->  Break.figurePosition IntMap.t ->line TS.UMap.t -> line -> parameters;
  par_completeLine:'a environment -> 'a box array array -> drawingBox array -> Break.figurePosition IntMap.t ->line TS.UMap.t -> line -> bool -> line list
}
and 'a figuredef={
  fig_contents:'a environment->drawingBox;
  fig_env:'a environment -> 'a environment;
  fig_post_env:'a environment -> 'a environment -> 'a environment;
  fig_parameters:'a environment -> 'a box array array -> drawingBox array -> parameters -> Break.figurePosition IntMap.t -> line TS.UMap.t -> line -> parameters
}
and ('a) tree=
    Node of ('a) node
  | Paragraph of 'a paragraph
  | FigureDef of 'a figuredef

and 'a environment={
  fontFamily:fontFamily;
  fontMonoFamily:fontFamily;
  fontItalic:bool;
  fontAlternative:fontAlternative;
  fontFeatures:string list;
  fontColor:OutputCommon.color;
  font:font;
  mathsEnvironment:Mathematical.environment;
  mathStyle:Mathematical.style;
  size:float;
  lead:float;
  footnote_y:float;
  normalMeasure:float;
  normalLead:float;
  normalLeftMargin:float;
  par_indent:'a box list;
  hyphenate:string->(string*string) array;
  word_substitutions:string->string;
  substitutions:glyph_id list -> glyph_id list;
  positioning:glyph_ids list -> glyph_ids list;
  counters:(int*int list) StrMap.t;     (** Niveau du compteur, état.  *)
  names:((int*int list) StrMap.t * string * line) StrMap.t; (** Niveaux de tous les compteurs à cet endroit, type, position  *)
  user_positions:line TS.UMap.t;
  show_boxes:bool;
}

(** {3 Contenu} *)

and 'a content=
    B of ('a environment->'a box list)        (** Une liste de boîtes, dépendante de l'environnement *)
  | BFix of ('a environment->'a box list)     (** Une liste de boîtes dépendante des positions des boîtes [User] *)
  | C of ('a environment->'a content list)    (** Le symmétrique de [C] pour [BFix]. On peut implémenter la paresse avec ça, par exemple. *)
  | CFix of ('a environment->'a content list) (** Pareil que [BFix], mais avec du contenu au lieu des boîtes *)
  | T of string                               (** Un texte simple *)
  | FileRef of (string*int*int)               (** Un texte simple, récupéré d'un fichier à l'exécution et donné par position de départ et taille *)
  | Env of ('a environment -> 'a environment) (** Une modification de l'environnement (par exemple des compteurs *)
  | Scoped of ('a environment->'a environment)*('a content list) (** Comme son nom et son type l'indiquent *)


let incr_counter ?(level= -1) name env=
  { env with counters=
      StrMap.add name (try let a,b=StrMap.find name env.counters in
                         match b with
                             h::s -> (a,(h+1)::s)
                           | _->a,[0]
                       with
                           Not_found -> level, [0]
                      ) env.counters }

let pop_counter name env=
  { env with counters=
      StrMap.add name (let a,b=StrMap.find name env.counters in (a,drop 1 b)) env.counters }

let push_counter name env=
  { env with counters=
      StrMap.add name (let a,b=StrMap.find name env.counters in (a,0::b)) env.counters }

let tags=function
    Node n->n.node_tags
  | _->[]
(****************************************************************)



(* Le jeu est de construire la structure de document suivante :
   C'est un arbre, avec du contenu texte à chaque nœud. *)


let empty={ name="";
            node_tags=[];
            displayname = []; children=IntMap.empty;
            node_env=(fun x->x);
            node_post_env=(fun x y->{ x with
                                        counters=y.counters;
                                        names=y.names;
                                        user_positions=y.user_positions });
            tree_paragraph=0 }

type ('a) cxt=(int*('a) tree) list
let next_key t=try fst (IntMap.max_binding t)+1 with Not_found -> 0
let prev_key t=try fst (IntMap.min_binding t)-1 with Not_found -> 0

let rec map_paragraphs f = function
  | Node n -> Node  { n with children=IntMap.map (map_paragraphs f) n.children }
  | Paragraph p -> Paragraph (f p)
  | x -> x

let up (t,cxt) = match cxt with
    []->(t,cxt)
  | (a,Node b)::s->(Node { b with children=IntMap.add a t b.children }, s)
  | (a,b)::s->(Node { empty with children=IntMap.singleton a t }, s)

let child (t,cxt) i=try
  match t with
      Node x->(IntMap.find i x.children, (i,t)::cxt)
    | _->(Node empty, (i,t)::cxt)
with
    Not_found -> (Node empty, (i,t)::cxt)

let lastChild (t,cxt)=try
  match t with
      Node x->(
        let i,t'=IntMap.max_binding x.children in
          t', (i,t)::cxt
      )
    | _->(t,cxt)
with
    Not_found -> (t,cxt)

let rec newChildAfter (t,cxt) chi=
  match t with
      Node x->(chi, (next_key x.children,t)::cxt)
    | _ when cxt=[]->(chi, (1,Node { empty with children=IntMap.singleton 0 t })::cxt)
    | _->newChildAfter (up (t,cxt)) chi

let rec newChildBefore (t,cxt) chi=
  match t with
      Node x->(chi, (prev_key x.children,t)::cxt)
    | _ when cxt=[]->(chi, (1,Node { empty with children=IntMap.singleton 0 t })::cxt)
    | _->newChildBefore (up (t,cxt)) chi


let rec prev f (t,cxt) =
  if f t then (t,cxt) else (
    match t with
        Node nt->
          let bin=List.rev (IntMap.bindings nt.children) in
          let rec prevs=function
              []->raise Not_found
            | (h,ht)::s->
                try
                  prev f (ht, (h,t)::cxt)
                with
                    Not_found->prevs s
          in
            prevs bin
      | _->raise Not_found
  )


let go_up str=
  (if snd !str=[] then Printf.fprintf stderr "Warning : go_up\n");
  str:=(up !str)

let rec top (a,b)=if b=[] then (a,b) else top (up (a,b))

let rec follow t l=match l with
    []->t
  | a::s->follow (child t a) s

(* La structure actuelle *)
(* let str=Printf.printf "str : init\n";ref [(Node empty,[])] *)
(* Le chemin vers le noeud courant *)

(* Sortie en dot de la structure du document *)
let doc_graph out t0=
  Printf.fprintf out "digraph {\n";
  let rec do_it path t=
    let col=
      if List.mem_assoc "Structural" t.node_tags then
        if List.mem_assoc "Numbered" t.node_tags then "blue" else "red" else "black" in
    Printf.fprintf out "%s [label=\"%s\", color=\"%s\"];\n" path t.name col;
    List.iter (fun (i,x)->match x with
                   Paragraph _->(
                     let p=path^"_"^(string_of_int i) in
                       Printf.fprintf out "%s[color=green];\n" p;
                       Printf.fprintf out "%s -> %s;\n" path p;
                   )
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

exception Not_found_in_family

let selectFont fam alt it =
  try
    let r,i = List.assoc alt fam in
    Lazy.force (if it then i else r)
  with Not_found -> raise Not_found_in_family

let updateFont env font str subst pos=
  let feat=Fonts.select_features font env.fontFeatures in
    { env with
        font=font;
        word_substitutions=str;
        substitutions=(fun glyphs -> List.fold_left (fun a b->apply b a) (subst glyphs) feat);
        positioning=(fun x->pos (positioning font x)) }

let change_font f env = updateFont env f (fun x->x) (fun x->x) (fun x->x)

(* Changer de font dans un scope, ignore la famille, attention, à éviter en direct *)
let font f t=
    [Scoped (change_font f, t)]

(* Rajouter une liste de features, voir Fonts.FTypes pour savoir ce
   qui existe *)
let add_features features env=
  let feat=Fonts.select_features env.font (features@env.fontFeatures) in
    { env with
        fontFeatures=features@env.fontFeatures;
        substitutions=(fun glyphs -> List.fold_left (fun a b->apply b a)
                         (env.substitutions glyphs) feat);
    }


let envItalic b env =
  let font, str, subst, pos= selectFont env.fontFamily env.fontAlternative b in
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
  add_features features (updateFont env font str subs pos)

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
      lead=env.lead*.fsize/.env.size }

(* Changer de taille dans un scope *)
let size fsize t=
  Scoped (resize_env fsize, t)


(****************************************************************)



(* Partie compliquée : il faut comprendre ce que fait l'optimiseur
   pour toucher à ça, ou apprendre en touchant ça *)



let parameters env paragraphs figures last_parameters last_figures last_users (line:line)=
  let page_footnotes=ref 0 in
  let measure=IntMap.fold (fun i aa m->match aa with
                               Break.Placed a->
                                 (if line.page=a.page &&
                                    line.height<=
                                    a.height -.figures.(i).drawing_y0
                                      && line.height>=
                                      a.height-. figures.(i).drawing_y1
                                  then
                                    env.normalMeasure -. figures.(i).drawing_nominal_width
                                  else m)
                             | _->m
                          ) last_figures env.normalMeasure
  in
    TS.UMap.iter (fun k a->
                    match k with
                        Footnote _ when a.page=line.page -> incr page_footnotes
                      | _->()
                 ) last_users;
    let footnote_h=fold_left_line paragraphs (fun fn box->match box with
                                                  User (Footnote (_,x))->fn+.x.drawing_y1-.x.drawing_y0
                                                | _->fn) 0. line
    in
      { measure=measure;
        page_height=min  250. (if line.page_line <= 0 then 45.*.env.normalLead else last_parameters.page_height)
          -. (if footnote_h>0. && !page_footnotes=0 then (footnote_h+.env.footnote_y) else footnote_h);
        left_margin=env.normalLeftMargin;
        local_optimization=0;
        min_page_before=0;
        min_page_after=0;
        next_acceptable_height=(fun node params nextNode nextParams height->
                                  if node==nextNode then
                                    let min_height=node.height+.params.min_height_after in
                                      env.lead*.(ceil (min_height/.env.lead))
                                  else
                                    if node.page=nextNode.page then (
                                      let min_height=max (nextNode.height+.env.lead) (node.height +. max params.min_height_after nextParams.min_height_before) in
                                      let h0=min_height/.env.lead in
                                      let h=if h0-.floor h0 < 1e-10 then env.lead*.floor h0 else
                                        env.lead*.ceil h0
                                      in
                                        h
                                    ) else (
                                      let min_height=nextNode.height+.max nextParams.min_height_before env.lead in
                                        env.lead*.(ceil (min_height/.env.lead))
                                    )
                               );
        min_height_before=0.;
        min_height_after=0.;
        not_last_line=false;
        not_first_line=false;
        really_next_line=true;
        absolute=false
      }

let vspaceBefore x=[B (fun _->[Parameters (fun p->{ p with min_height_before=p.min_height_before+.x })])]
let vspaceAfter x=[B (fun _->[Parameters (fun p->{ p with min_height_after=p.min_height_after+.x })])]
let hspace x =[B (fun env-> let x = x *. env.size in [glue x x x])]

let do_center parameters env paragraphs figures last_parameters lastFigures lastUsers l=
  let param=parameters env paragraphs figures last_parameters lastFigures lastUsers l in
  let b=l.nom_width in
    if param.measure >= b then
      { param with measure=b; left_margin=param.left_margin +. (param.measure-.b)/.2. }
    else
      param
let center = do_center parameters
(* let center env paragraphs figures last_parameters lastFigures lastUsers l= *)
(*   let param=parameters env paragraphs figures last_parameters lastFigures lastUsers l in *)
(*   let b=l.nom_width in *)
(*     if param.measure >= b then *)
(*       { param with measure=b; left_margin=param.left_margin +. (param.measure-.b)/.2. } *)
(*     else *)
(*       param *)

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
  str:=up (newChildAfter !str (
             FigureDef
               { fig_contents=drawing;
                 fig_env=(fun x->
                            let l,cou=try StrMap.find "_figure" x.counters with
                                Not_found -> -1, [0] in
                            let l0,cou0=try StrMap.find "figure" x.counters with
                                Not_found -> -1, [0] in
                            let counters'=StrMap.add "_figure"
                              (l,match cou with []->[0] | h::_->[h+1]) x.counters in
                            let counters''=StrMap.add "figure" (l0,match cou0 with []->[0]
                                                                  | h::_->[h+1]) counters' in
                            { x with
                                names=if name="" then x.names else (
                                  let w=try let (_,_,w)=StrMap.find name x.names in w with Not_found -> uselessLine in
                                  StrMap.add name (x.counters, "_figure", w) x.names
                                );
                                counters=counters''
                            });
                 fig_post_env=(fun x y->{ x with names=y.names; counters=y.counters; user_positions=y.user_positions });
                 fig_parameters=parameters }))

let flushFigure name=
  [CFix (fun env->
        try
          let (counters,_,_)=StrMap.find name env.names in
            match StrMap.find "_figure" counters with
                _,h::_->[B (fun _->[FlushFigure h])]
              | _->[Env (incr_counter "_figure")]
        with
            Not_found ->[]
     )]


let beginFigure name=
  [CFix (fun env->
        try
          let (counters,_,_)=StrMap.find name env.names in
            match StrMap.find "_figure" counters with
                _,h::_->[B (fun _->[BeginFigure h])]
              | _->[Env (incr_counter "_figure")]
        with
            Not_found ->[]
     )]

(****************************************************************)




let newPar str ?(environment=(fun x->x)) complete parameters par=
  match !str with
      Paragraph p,path->
        str:=up (Paragraph {p with par_contents=p.par_contents@par}, path)
    | _->
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
      node_tags= (if in_toc then ["InTOC",""] else []) @ ["Structural",""] @(if numbered then ["Numbered",""] else []);
      node_env=(
        fun env->
          { env with
              counters=StrMap.add "_structure" (
                try
                  let (a,b)=StrMap.find "_structure" env.counters in
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
              counters=StrMap.add "_structure" (
                try
                  let a,b=StrMap.find "_structure" env'.counters in
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

let pageref x=
  [CFix (fun env->try
           let (_,_,node)=StrMap.find x env.names in
             [B (fun _->[User (BeginLink x)]);
              T (string_of_int (1+node.page));
              B (fun _->[User EndLink])]
         with Not_found -> []
        )]

let label ?(labelType="_structure") name=
  [Env (fun env->
          let w=try let (_,_,w)=StrMap.find name env.names in w with Not_found -> uselessLine in
            { env with names=StrMap.add name (env.counters, labelType, w) env.names });

   B (fun env ->
        [User (Label name)])
  ]



let generalRef refType name=
  [ CFix (fun env->try
            let counters=
              if name="_here" then env.counters else
                let a,_,_=StrMap.find name env.names in a
            in
            let lvl,num_=(StrMap.find refType counters) in
            let num=if refType="_structure" then drop 1 num_ else num_ in
            let _,str_counter=StrMap.find "_structure" counters in
            let sect_num=drop (List.length str_counter - max 0 lvl+1) str_counter in
              [B (fun _->[User (BeginLink name)]);
               T (String.concat "." (List.map (fun x->string_of_int (x+1))
                                       (List.rev (num@sect_num))));
               B (fun _->[User EndLink])]
          with
              Not_found -> []
         )]

let sectref x=generalRef "_structure" x

let extLink a b=B (fun _->[User (BeginURILink a)])::b@[B (fun _->[User EndLink])]
let link a b=B (fun _->[User (BeginLink a)])::b@[B (fun _->[User EndLink])]
let notFirstLine _=B (fun _->[Parameters (fun p->{p with not_first_line=true})])
let notLastLine _=B (fun _->[Parameters (fun p->{p with not_last_line=true})])

#ifdef CAMLIMAGES
let includeGraphics ?width:(width=0.) ?height:(height=0.) imageFile=
  [B (fun env->
        let image=(OImages.load imageFile []) in
        let w,h=Images.size image#image in
        let fw,fh=
          if width=0. then
            if height=0. then
              env.normalMeasure, env.normalMeasure*.(float_of_int h)/.(float_of_int w)
            else
              height*.(float_of_int w)/.(float_of_int h), height
          else
            width, width*.(float_of_int h)/.(float_of_int w)
        in
        let i={image_file=imageFile;
               image_width=fw;
               image_height=fh;
               image_x=0.;
               image_y=0.
              }
        in
        let img=Drawing {
          drawing_min_width=fw;
          drawing_max_width=fw;
          drawing_nominal_width=fw;
          drawing_y0=0.;
          drawing_y1=fh;
          drawing_badness=(fun _->0.);
          drawing_contents=(fun _->[OutputCommon.Image i])
        }
        in
          image#destroy;
          [img]
     )]
#else
  let includeGraphics ?scale _=[]

#endif

(** {3 Boitification et "classes" de documents}*)

(** Comment on cache des trucs à ocamldoc mais pas à caml ? Ça pourrait être utilisé ici *)
let sources=ref StrMap.empty
let rStdGlue:(float*user box) ref=ref (0.,glue 0. 0. 0.)

(* let ambientBuf=ref ([||],0) *)
(** Fabrique une glue à partir d'une espace en unicode *)
let makeGlue env x0=
  let stdGlue=
    (if fst !rStdGlue <> env.size then rStdGlue:=(env.size, glue (2.*.env.size/.9.) (env.size/.3.) (env.size/.2.)));
    snd !rStdGlue
  in
    if (x0>=0x0009 && x0<=0x000d) || x0=0x0020 then stdGlue else
      match x0 with
          0x00a0->(match stdGlue with
                       Glue y->Drawing y
                     | y->y)
        | 0x1680->stdGlue
        | 0x180e->(glue 0. 0. 0.)
        | 0x2000->let w=env.size/.2. in (glue w w w)
        | 0x2001->let w=env.size in (glue w w w)
        | 0x2002->let w=env.size/.2. in (glue (w*.2./.3.) w (w*.3./.2.))
        | 0x2003->let w=env.size in (glue (w*.2./.3.) w (w*.3./.2.))
        | 0x2004->let w=env.size/.3. in (glue (w*.2./.3.) w (w*.3./.2.))
        | 0x2005->let w=env.size/.4. in (glue (w*.2./.3.) w (w*.3./.2.))
        | 0x2006->let w=env.size/.6. in (glue (w*.2./.3.) w (w*.3./.2.))
        | 0x2007->(
            let w0=
              glyph_of_string env.substitutions env.positioning env.font env.size
                env.fontColor
                "0"
            in
            let w=env.size*.(List.fold_left (fun w1 b->w1+.box_width 0. b) 0. w0) in (glue (w*.2./.3.) w (w*.3./.2.))
          )
        | 0x2008->(
            let w0=
              glyph_of_string env.substitutions env.positioning env.font env.size
                env.fontColor
                "."
            in
            let w=env.size*.(List.fold_left (fun w1 b->w1+.box_width 0. b) 0. w0) in (glue (w*.2./.3.) w (w*.3./.2.))
          )
        | 0x2009->let w=env.size/.5. in (glue (w*.2./.3.) w (w*.3./.2.))
        | 0x200a->let w=env.size/.8. in (glue (w*.2./.3.) w (w*.3./.2.))
        | 0x202f->
            let w=env.size/.5. in
              (match glue (w*.2./.3.) w (w*.3./.2.) with
                   Glue y->Drawing y
                 | y->y)
        | 0x205f->let w=env.size*.4./.18. in (glue (w*.2./.3.) w (w*.3./.2.))
        | 0xfeff->(glue 0. 0. 0.)
        | _->stdGlue


let rec gl_of_str env string=
  try
    hyphenate env.hyphenate env.substitutions env.positioning env.font env.size
      env.fontColor
      (env.word_substitutions string)
  with
      Glyph_not_found g ->(
        let family'=List.remove_assoc env.fontAlternative env.fontFamily in
          try
            let font, str, subst, pos= selectFont family' env.fontAlternative env.fontItalic in
            let env'=updateFont env font str subst pos in
              gl_of_str env' string
          with
              Not_found_in_family->[](* raise (Glyph_not_found g) *)
      )

let append buf nbuf x=
  let arr=
    if !nbuf>=Array.length !buf then
      Array.init (max 1 (2*Array.length !buf)) (fun j->if j< !nbuf then (!buf).(j) else Empty)
    else !buf
  in
    arr.(!nbuf)<-x;
    buf:=arr;
    incr nbuf


(** La sémantique de cette fonction par rapport aux espaces n'est pas
    évidente. S'il n'y a que des espaces, seul le dernier est pris en
    compte. Sinon, le dernier de la suite d'espaces entre deux mots
    consécutifs est pris en compte *)
let boxify buf nbuf fixable env0 l=
  let rec boxify env=function
      []->env
    | (B b)::s->(List.iter (append buf nbuf) (b env); boxify env s)
    | (C b)::s->(boxify env ((b env)@s))
    | (CFix b)::s->(fixable:=true; boxify env ((b env)@s))
    | (BFix b)::s->(fixable:=true; List.iter (append buf nbuf) (b env); boxify env s)
    | Env f::s->boxify (f env) s
    | (T t)::s->(
        let rec cut_str only_spaces needs_glue gl i0 i=
          if i>=String.length t then (
            if only_spaces then append buf nbuf gl;
            if i0<>i then (
              if needs_glue then append buf nbuf gl;
              List.iter (append buf nbuf)
                (gl_of_str env (String.sub t i0 (i-i0)))
            )
          ) else (
            if is_space (UTF8.look t i) then (
              let sp=makeGlue env (UChar.uint_code (UTF8.look t i)) in
                if i0<>i && needs_glue then append buf nbuf gl;
                List.iter (append buf nbuf)
                  (gl_of_str env (String.sub t i0 (i-i0)));
                cut_str (only_spaces && i=i0) (needs_glue || i<>i0) sp (UTF8.next t i) (UTF8.next t i)
            ) else (
              cut_str false needs_glue gl i0 (UTF8.next t i)
            )
          )
        in
          cut_str true false (snd !rStdGlue) (UTF8.first t) (UTF8.first t);
          boxify env s
      )
    | FileRef (file,off,size)::s -> (
        let i=try 
	  StrMap.find file !sources 
        with _-> (let i=open_in_bin file in sources:= StrMap.add file i !sources; i) 
        in
        let buffer=String.create size in
        let _=seek_in i off; really_input i buffer 0 size in
          boxify env (T buffer::s)
      )
    | Scoped (fenv, p)::s->(
        let env'=fenv env in
        let _=boxify env' p in
          boxify env s
      )
  in
    boxify env0 l

let boxify_scoped env x=
  let buf=ref [||] in
  let nbuf=ref 0 in
  let _=boxify buf nbuf (ref false) env x in
    Array.to_list (Array.sub !buf 0 !nbuf)



module type DocumentStructure=sig
  val structure:((user) tree*(int*(user) tree) list) ref
  val fixable:bool ref
end
module type Format=sig
  type user
  val defaultEnv:user environment
  val postprocess_tree:(user) tree->(user) tree
  val title:
    ((user) tree * (((user) tree) list)) ref ->
    ?label:string ->
    ?displayname:user content list ->
    string -> unit
  val author:string->unit
  val institute:string->unit
end


let flatten env0 fixable str=
  let paragraphs=ref [] in
  let figures=ref IntMap.empty in
  let fig_param=ref IntMap.empty in
  let param=ref [] in
  let compl=ref [] in
  let n=ref 0 in

  let buf=ref [||] in
  let nbuf=ref 0 in
  let frees=ref 0 in
  let add_paragraph env p=
    nbuf:= !frees;
    let v=boxify buf nbuf fixable env p.par_contents in
      paragraphs:=(Array.sub !buf 0 !nbuf)::(!paragraphs);
      compl:=(p.par_completeLine env)::(!compl);
      param:=(p.par_parameters env)::(!param);
      incr n;
      frees:=0;
      v
  in

  let rec flatten flushes env path tree=
    let level=List.length path in
      match tree with
          Paragraph p -> add_paragraph env p
        | FigureDef f -> (
            let env1=f.fig_env env in
            let n=IntMap.cardinal !figures in
              fig_param:=IntMap.add n (f.fig_parameters env1) !fig_param;
              figures:=IntMap.add n (f.fig_contents env1) !figures;
              append buf frees (BeginFigure n);
              f.fig_post_env env env1
          )
        | Node s-> (
            s.tree_paragraph <- List.length !paragraphs;
            let flushes'=ref [] in
            let flat_children k a (is_first,indent, env1)=match a with
                Paragraph p->(
                  let env2=flatten flushes' (p.par_env env1) (k::path) (
                    Paragraph { p with par_contents=
                        (if is_first then (
                           let name=String.concat "_" ("_"::List.map string_of_int path) in
                             [Env (fun env->
                                     let w=try let (_,_,w)=StrMap.find name env.names in w with
                                         Not_found -> uselessLine in
                                       { env with names=StrMap.add name (env.counters, "_", w)
                                           env.names });
                              B (fun _->[User (Label name)])
                             ]
                         ) else [])@
                          (if indent then [B (fun env->(p.par_env env).par_indent)] else []) @ p.par_contents
                              }
                  ) in
                    false,true, p.par_post_env env1 env2
                )
              | FigureDef f as h->(
                  let env2=flatten flushes' env1 (k::path) h in
                  let num=try
                    match StrMap.find "_figure" env1.counters with
                        _,h::_->h
                      | _->0
                  with
                      Not_found ->0
                  in
                    flushes':=FlushFigure num::(!flushes');
                    is_first,indent,env2
                )
              | Node h as tr->(
                  let env2=h.node_env env1 in
                  let env3=flatten flushes' env2 (k::path) tr in
                    is_first,false, h.node_post_env env1 env3
                )
            in
            let _,_,env2=IntMap.fold flat_children s.children (true,false,env) in
              paragraphs:=(match !paragraphs with
                               []->[]
                             | h::s->Array.append h (Array.of_list !flushes')::s);
              env2
          )
  in
  let env1=match str with
      Node n->n.node_env env0
    | Paragraph n->n.par_env env0
    | _->env0
  in
  let env2=flatten (ref []) env1 [] str in
  let params=Array.init
    (IntMap.cardinal !figures)
    (fun i->IntMap.find i !fig_param)
  in
    (env2, params,
     Array.of_list (List.rev !param),
     Array.of_list (List.rev !compl),
     Array.of_list (List.rev !paragraphs),
     Array.of_list (List.map snd (IntMap.bindings !figures)))

let rec make_struct positions tree=
  match tree with
      Node s -> (
        let (p,x,y)=try positions.(s.tree_paragraph) with _->(0,0.,0.) in
        let rec make=function
            []->[]
          | (_,Node u)::s when List.mem_assoc "InTOC" u.node_tags -> (make_struct positions (Node u))::(make s)
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

let tag str tags=
  match str with
      Node n->Node { n with node_tags=tags@n.node_tags }
    | _->Node { empty with node_tags=tags; children=IntMap.singleton 0 str }


let update_names env figs user=
  (* let fil=TS.UMap.filter (fun k a->match k with Structure _->true |_->false) in *)
  let needs_reboot=ref false in (* (fil user<>fil env.user_positions) in; *)
  let env'={ env with user_positions=user; counters=StrMap.map (fun (l,_)->l,[0]) env.counters; names=
      StrMap.fold (fun k (a,b,c) m->try
                     let pos=
                       if b="_figure" then
                         match IntMap.find (List.hd (snd (StrMap.find "_figure" a))) figs with
                             Break.Placed l->l
                           | _->raise Not_found
                       else
                         TS.UMap.find (Label k) user
                     in
                       if pos<>c && b<>"_" then (
                         Printf.fprintf stderr "reboot : position of %S changed\n" k
                       );
                       needs_reboot:= !needs_reboot || (pos<>c);
                       StrMap.add k (a,b,pos) m
                   with Not_found -> (Printf.fprintf stderr "reboot : position of %S not found\n" k;needs_reboot:=true; m)
                  ) env.names env.names
           }
  in
    flush stderr;
    env',!needs_reboot
