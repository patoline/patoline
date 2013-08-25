(*
  Copyright Florian Hatat, Tom Hirschowitz, Pierre Hyvernat,
  Pierre-Etienne Meunier, Christophe Raffalli, Guillaume Theyssier 2012.

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

open Config
open Util
open Fonts
open Fonts.FTypes
open OutputCommon
open Box
open Box
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

           if a.height<b.height then -1 else
             if a.height>b.height then 1 else
               0
     let hash a=Hashtbl.hash a
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
    sqrt_dist:float;
    kerning:bool;
  (* None means precise, Some x mean unprecise, but subdivise 
     Bezier curve until the thickness of the polygon is less than x *)
    priorities:float array;
    priority_unit:float;
    delimiter_up_tolerance:float;
    delimiter_down_tolerance:float;
    op_tolerance:float;
    op_limits_tolerance:float;
    punctuation_factor:float;
    optical_alpha:float;
    optical_beta:float;
    precise_kerning:float;
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

(** Document structure. A [node] is an internal node of the document tree. *)
type node={
  name:string;
  displayname:content list;
  mutable boxified_displayname:raw list;
  children:tree IntMap.t;       (** The [int] there have nothing to do with section numbering. This only implements an extensible array *)
  node_tags:(string*string) list;
  node_env:environment -> environment; (** Environment modification function at node beginning. *)
  node_post_env:environment -> environment -> environment;(** Environment modification when getting out of the node. *)
  node_states:Util.IntSet.t;                              (** The page states in which the contents of this node appears. *)
  mutable node_paragraph:int;
}

(** The first type of leaves in a document: paragraphs. *)
and paragraph={
  par_contents:content list;
  par_env:environment -> environment;
  par_post_env:environment -> environment -> environment;
  par_parameters:environment -> Box.box array array -> drawingBox array -> parameters ->  Break.figurePosition IntMap.t ->line MarkerMap.t -> line -> line -> parameters;
  par_badness: environment -> Box.box array array -> drawingBox array->Break.figurePosition IntMap.t -> Box.line -> Box.box array -> int -> Box.parameters -> float -> Box.line -> Box.box array -> int -> Box.parameters -> float -> float;
  par_completeLine:environment -> Box.box array array -> Box.drawingBox array -> Break.figurePosition IntMap.t ->line MarkerMap.t -> line -> bool -> line list;
  par_states:Util.IntSet.t;
  mutable par_paragraph:int
}

(** Figure definitions. *)
and figuredef={
  fig_contents:environment->drawingBox;
  fig_env:environment -> environment;
  fig_post_env:environment -> environment -> environment;
  fig_parameters:environment -> Box.box array array -> drawingBox array -> parameters -> Break.figurePosition IntMap.t -> line MarkerMap.t -> line -> line -> parameters
}

(** This is the type of document trees. *)
and tree=
    Node of node
  | Paragraph of paragraph
  | FigureDef of figuredef

(** Environments. These are typically folded on document trees, and
    control many different things about the fonts, counters, or
    labels. *)
and environment={
  fontFamily:fontFamily;
  fontMonoFamily:fontFamily;
  fontMonoRatio:float; (* size adjustment of the two previous family *)
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
  normalPageFormat:float*float;
  par_indent:box list;
  hyphenate:string->(string*string) array;
  word_substitutions:string->string;
  substitutions:glyph_id list -> glyph_id list;
  positioning:glyph_ids list -> glyph_ids list;
  counters:(int*int list) StrMap.t;     (** Niveau du compteur, état.  *)
  names:((int*int list) StrMap.t * string * line) StrMap.t; (** Niveaux de tous les compteurs à cet endroit, type, position  *)
  new_page:Box.frame_zipper->Box.frame_zipper;
  new_line:environment->line->parameters->
           line->parameters->Box.frame_zipper->float->float;
  user_positions:line MarkerMap.t;
  show_boxes:bool;
  show_frames:bool;
  adjust_optical_alpha:float;
  adjust_optical_beta:float;
  adjust_epsilon:float;
  adjust_min_space:float;
  math_break_badness:float; (* pas dans l'environement math, car aucun sens en dehors du TextStyle *)
}

(** {3 Contents} *)

and content=
    B of (environment->box list) * box list option ref
  (** A list of boxes, depending on the environment. The second
      parameters is a cache, in the case we should iterate compilation
      to resolve the names. *)
  | C of (environment->content list)
  (** A contents list depending on the environment. This may be used
      for instance to typeset the state of a counter. *)
  | T of string*(box list IntMap.t option) ref
  (** Simple text. *)
  | FileRef of (string*int*int)
  (** Also simple text, converted to [T] by reading the corresponding
      file at given offset, for the given number of bytes. *)
  | Env of (environment -> environment)
  (** An environment modification function, for instance to register a
      name or modify the state of a counter. *)
  | Scoped of (environment->environment)*(content list)
(** A scoped environment transformation, applied on a small list of contents. *)


let bB f = B(f,ref None)
let tT f = T(f,ref None)
let env_accessed=ref false
let uT f = C(fun _->env_accessed:=true;[tT f])
let string_of_contents l =
  let buf=Rbuffer.create 1000 in
  let rec fill_buf t=match t with
      T (str,_)::s->(
        if Rbuffer.length buf>0 then (
          Rbuffer.add_string buf " ";
        );
        Rbuffer.add_string buf str;
        fill_buf s
      )
    (* | C f::s->( *)
    (*   fill_buf (f defaultEnv); *)
    (*   fill_buf s *)
    (* ) *)
    | _::s -> fill_buf s
    | []->()
  in
  fill_buf l;
  Rbuffer.contents buf

let names env=
  env_accessed:=true;
  env.names
let user_positions env=
  env_accessed:=true;
  env.user_positions
let displayname n=
  env_accessed:=true;
  n.displayname

let _names env=
  env.names
let _user_positions env=
  env.user_positions

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

(**/**)
let default_new_page pageFormat t=
  let zip=Box.make_page pageFormat (frame_top t) in
  let w=(fst zip).frame_x1-.(fst zip).frame_x0
  and h=(fst zip).frame_y1-.(fst zip).frame_y0 in
  let x0=((fst zip).frame_x0+.1.*.w/.6.) in
  let y0=((fst zip).frame_y0+.1.*.h/.6.) in
  let x1=((fst zip).frame_x1-.1.*.w/.6.) in
  let y1=((fst zip).frame_y1-.1.*.h/.6.) in
  frame x0 y0 x1 y1 zip
(**/**)


(* Le jeu est de construire la structure de document suivante :
   C'est un arbre, avec du contenu texte à chaque nœud. *)


let empty:node=
  { name="";
    node_tags=[];
    displayname = []; boxified_displayname=[];
    children=IntMap.empty;
    node_env=(fun x->x);
    node_post_env=(fun x y->{ x with
      counters=y.counters;
      names=names y;
      user_positions=user_positions y });
    node_states=IntSet.empty;
    node_paragraph=0 }

type cxt=(int*tree) list
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

exception Found

(** Finds the last node satisfying a given predicate in a document tree. *)
let find_last f tr=
  let result=ref None in
  let rec find_last path tr=match tr with
    | x when f tr->(
      result:=Some (List.rev path);
      raise Found
    )
    | Node n->(
        let k1,_=IntMap.max_binding n.children in
        let k0,_=IntMap.min_binding n.children in
        for i=k1 downto k0 do
          try
            find_last (i::path) (IntMap.find i n.children);
          with
              Not_found -> ()
        done;
      )
    | _->raise Not_found
  in
  try
    find_last [] tr;
    raise Not_found
  with
      Found->(
        match !result with
            None->raise Not_found
          | Some a->a
      )

(** Is the node a paragraph ? *)
let is_paragraph x=match x with
    Paragraph _->true
  | _->false

(** Is the node an internal node ? *)
let is_node x=match x with
    Node _->true
  | _->false


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

let rec up_n n tr=if n<=0 then tr else up_n (n-1) (up tr)

(* La structure actuelle *)
(* let str=Printf.printf "str : init\n";ref [(Node empty,[])] *)
(* Le chemin vers le noeud courant *)

(* Sortie en dot de la structure du document *)
let doc_graph out t0=
  Printf.fprintf out "digraph {\n";
  let rec do_it path t=
    let col=
      if List.mem_assoc "structural" t.node_tags then
        if List.mem_assoc "numbered" t.node_tags then "blue" else "red" else "black"
    in
    Printf.fprintf out "%s [label=\"%s\", color=\"%s\"];\n" path t.name col;
    let mb=try fst (IntMap.min_binding t.children) with Not_found->0 in
    List.iter (fun (i,x)->match x with
        Paragraph par->(
          let p=path^"_"^(string_of_int (i-mb)) in
          Printf.fprintf out "%s[color=green,label=\"%s\"];\n" p (string_of_contents par.par_contents);
          Printf.fprintf out "%s -> %s;\n" path p;
        )
      | FigureDef _-> ()
      | Node n->(
        let p=path^"_"^(string_of_int (i-mb)) in
        Printf.fprintf out "%s -> %s;\n" path p;
        do_it p n)) (IntMap.bindings t.children)
  in
    (match t0 with
         Node t->do_it "x0" t
       | _->());
    Printf.fprintf out "}\n"


(** {3 Environment transformations} *)

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
        substitutions=(fun glyphs -> Fonts.apply_features font feat (subst glyphs));
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
        substitutions=(fun glyphs -> Fonts.apply_features env.font feat
          (env.substitutions glyphs));
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
  [Scoped (resize_env fsize, t)]

let color col t=
  [Scoped ((fun env->{env with fontColor=col}), t)]


let bold a=alternative Bold a

let sc a=alternative Caps a

let verbEnv x =
  { (envFamily x.fontMonoFamily x)
    with size = x.size *. x.fontMonoRatio; normalMeasure=infinity; par_indent = [];
      (*lead = x.lead *. x.fontMonoRatio*)}

let verb p =
  [Scoped ((fun x ->
    { (envFamily x.fontMonoFamily x) with size = x.size *. x.fontMonoRatio}), p)]

let emph=toggleItalic
let id x=x

(****************************************************************)



(* Partie compliquée : il faut comprendre ce que fait l'optimiseur
   pour toucher à ça, ou apprendre en touchant ça *)




let vspaceBefore x=[bB (fun _->[Parameters (fun p->{ p with min_height_before=max p.min_height_before x })])]
let vspaceAfter x=[bB (fun _->[Parameters (fun p->{ p with min_height_after=max p.min_height_after x })])]
let pagesBefore x=[bB (fun _->[Parameters (fun p->{ p with min_page_before=max p.min_page_before x })])]
let pagesAfter x=[bB (fun _->[Parameters (fun p->{ p with min_page_after=max p.min_page_after x })])]
let linesBefore x=[bB (fun _->[Parameters (fun p->{ p with min_lines_before=max p.min_lines_before x })])]
let linesAfter x=[bB (fun _->[Parameters (fun p->{ p with min_lines_after=max p.min_lines_after x })])]
let notFirstLine _=bB (fun _->[Parameters (fun p->{p with not_first_line=true})])
let notLastLine _=bB (fun _->[Parameters (fun p->{p with not_last_line=true})])

let hspace x =[bB (fun env-> let x = x *. env.size in [glue x x x])]
let hfill () = [bB (fun env-> let x = env.normalMeasure in [glue 0. (0.5 *. x) x])]

let do_center parameters env paragraphs figures last_parameters lastFigures lastUsers lastLine l=
  let param=parameters env paragraphs figures last_parameters lastFigures lastUsers lastLine l in
  let a=l.min_width
  and b=l.nom_width in
  if param.measure >= b then
    { param with measure=b; left_margin=param.left_margin +. (param.measure-.b)/.2. }
  else
    if param.measure >= a then
      param
    else
      { param with measure=a; left_margin=param.left_margin +. (param.measure-.a)/.2. }


let do_ragged_left parameters a b c d e f g line=
  let par=parameters a b c d e f g line in
  { par with measure=line.nom_width }

let do_ragged_right parameters a b c d e f g line=
  let par=parameters a b c d e f g line in
  { par with
    measure=line.nom_width;
    left_margin=par.left_margin+.par.measure-.line.nom_width }




let badness
    env
    paragraphs
    figures
    figureStates
    node_i line_i max_i params_i comp_i
    node_j line_j max_j params_j comp_j=

  if node_j.paragraph>=Array.length paragraphs then 0. else (
    let v_bad=
      if page node_i=page node_j then (
        Badness.v_badness
          (node_j.height-.node_i.height)
          line_i max_i params_i comp_i
          line_j max_j params_j comp_j
      ) else (
        if node_i.hyphenEnd>=0 then infinity else 0.
      )
    in
    (Badness.h_badness paragraphs params_j.measure node_j comp_j)
    +. v_bad
    (* Page pas assez remplie *)
    +. (if page node_i<>page node_j &&
        node_i.height>=(fst node_i.layout).frame_y0+.env.lead then 10000. else 0.)
      (* Cesures *)
    +. (if node_j.hyphenEnd >=0 then
        (if node_j.hyphenStart >=0 then
            1e10
         else
            1e8)
      else
        (if node_j.hyphenStart >=0 then
            1e8
         else
            0.)
    )
    (* Badness de couper ici *)
    +. (if node_j.lineEnd<Array.length paragraphs.(node_j.paragraph)
        && not node_j.isFigure then
        match paragraphs.(node_j.paragraph).(node_j.lineEnd) with
            Glue g->g.drawing_break_badness
          | _->0.
      else 0.0
    )
    (* Différence de compression entre deux lignes consécutives *)
    +. (1000.*.(abs_float (comp_i-.comp_j)))
  )

(** {3 Figures} *)

let figure str parameters ?(name="") drawing=
  str:=up (newChildAfter !str (
             FigureDef
               { fig_contents=drawing;
                 fig_env=(fun x->
                            let l,cou=try StrMap.find "_figure" x.counters with
                                Not_found -> -1, [] in
                            let l0,cou0=try StrMap.find "figure" x.counters with
                                Not_found -> -1, [] in
                            let counters'=
                              (StrMap.add "_figure"
                                 (l,match cou with h::s->(h+1)::s | _->[0])
                                 (StrMap.add "figure"
                                    (l0,match cou0 with h::s->(h+1)::s | _->[0]) x.counters)
                              )
                            in
                            { x with
                                names=if name="" then names x else (
                                  let w=
                                    try let (_,_,w)=StrMap.find name (names x) in w
                                    with Not_found -> uselessLine
                                  in
                                  StrMap.add name (counters', "_figure", w) (names x)
                                );
                                counters=counters'
                            });
                 fig_post_env=(fun x y->{ x with names=names y; counters=y.counters; user_positions=user_positions y });
                 fig_parameters=parameters }))

let flushFigure name=
  [C (fun env->
        try
          env_accessed:=true;
          let (counters,_,_)=StrMap.find name (names env) in
            match StrMap.find "_figure" counters with
                _,h::_->[bB (fun _->[FlushFigure h])]
              | _->[]
        with
            Not_found ->[]
     )]


let beginFigure name=
  [C (fun env->
        try
          env_accessed:=true;
          let (counters,_,_)=StrMap.find name (names env) in
            match StrMap.find "_figure" counters with
                _,h::_->[bB (fun _->[BeginFigure h])]
              | _->[]
        with
            Not_found ->[]
     )]

(****************************************************************)

(** {3 Editing document trees} *)




(** Adds a new paragraph with the given parameters, just below the current [node]. *)
let newPar str ?(environment=(fun x->x)) ?(badness=badness) ?(states=IntSet.empty) complete parameters par=
  match !str with
      Paragraph p,path-> 
	(* Tom: j'ai l'impression que ce bout de code n'est jamais utilise. *)
        str:=up (Paragraph {p with par_contents=p.par_contents@par}, path)
    | _->
        let para=Paragraph {par_contents=par; par_env=environment;
                            par_post_env=(fun env1 env2 -> { env1 with names=names env2; counters=env2.counters; user_positions=user_positions env2 });
                            par_parameters=parameters;
                            par_badness=badness;
                            par_completeLine=complete; par_states=states; par_paragraph=(-1) }
        in
          str:=up (newChildAfter !str para)


(** Adds a new node, just below the last one. *)
let newStruct str ?(in_toc=true) ?label ?(numbered=true) displayname =
  let name = match label with
      None -> string_of_contents displayname
    | Some s -> s
  in
  let displayname=match displayname with
      []->(match label with Some s->[tT s] | None->[])
    | _->displayname
  in
  let para=Node {
    empty with
      name=name;
      displayname =[C (fun _->env_accessed:=true;displayname)];
      node_tags= (if in_toc then ["intoc",""] else []) @ ["structural",""] @(if numbered then ["numbered",""] else []);
      node_env=(
        fun env->
          { env with
              counters=StrMap.add "_structure" (
                try
                  let (a,b)=StrMap.find "_structure" env.counters in
                    a,0::(match b with []->[0] | _->b)
                with
                    Not_found -> (-1,[0;0])
              ) env.counters }
      );
      node_post_env=(
        fun env env'->
          { env with
              names=names env';
              user_positions=user_positions env';
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

(** {3 References, labels and links} *)



let pageref x=
  [C (fun env->
    try
      env_accessed:=true;
      let (_,_,node)=StrMap.find x (names env) in
      [bB (fun _->[Marker (BeginLink x)]);
       tT (string_of_int (1+page node));
       bB (fun _->[Marker EndLink])]
    with Not_found -> []
  )]

let make_name name=
  let realName=UTF8.Buf.create (String.length name) in
  let rec fill i sp=
    if UTF8.out_of_range name i then
      UTF8.Buf.contents realName
    else (
      if is_space (UTF8.look name i) then
        if sp then fill (i+1) true
        else (
          UTF8.Buf.add_char realName (UChar.of_char ' ');
          fill (UTF8.next name i) true
        )
      else (
        UTF8.Buf.add_char realName (UTF8.look name i);
        fill (UTF8.next name i) false
      )
    )
  in
  fill 0 true


let label ?(labelType="_structure") name=
  let name=make_name name in
  [Env (fun env->
    let w=try let (_,_,w)=StrMap.find name (names env) in w with Not_found -> uselessLine in
    { env with names=StrMap.add name (env.counters, labelType, w) (names env) });
   bB (fun env ->
     [Marker (Label name)])
  ]



let generalRef refType name=
  let name=make_name name in
  [ C (fun env->
    try
      env_accessed:=true;
      let counters=
        if name="_here" then env.counters else
          let a,_,_=StrMap.find name (names env) in a
      in
      let lvl,num_=StrMap.find refType counters in
      let num=if refType="_structure" then drop 1 num_ else num_ in
      let str_counter=
        try
          let _,str_counter=StrMap.find "_structure" counters in
          str_counter
        with
            Not_found->[]
      in
      let sect_num=drop (List.length str_counter - max 0 lvl+1) str_counter in
      [bB (fun _->[Marker (BeginLink name)]);
       tT (String.concat "." (List.map (fun x->string_of_int (x+1))
                                (List.rev (num@sect_num))));
       bB (fun _->[Marker EndLink])]
    with
        Not_found -> []
  )]

let sectref x=generalRef "_structure" x

let extLink a b=bB (fun _->[Marker (BeginURILink a)])::b@[bB (fun _->[Marker EndLink])]
let link a b=bB (fun _->[Marker (BeginLink a)])::b@[bB (fun _->[Marker EndLink])]

(** {3 Images} *)

#ifdef CAMLIMAGES
let image ?scale:(scale=0.) ?width:(width=0.) ?height:(height=0.) ?offset:(offset=0.) imageFile env=
  let image=(OImages.load imageFile []) in
  let w,h=Images.size image#image in
  let fw,fh=
    if width=0. then
      if height=0. then
        if scale=0. then
          if env.normalMeasure<(float_of_int w)/.7. then
            env.normalMeasure, env.normalMeasure*.(float_of_int h)/.(float_of_int w)
          else
            (float_of_int w)/.7.,(float_of_int h)/.7.
        else
          (float_of_int w)*.scale,(float_of_int h)*.scale
      else
        height*.(float_of_int w)/.(float_of_int h), height
    else
      width, width*.(float_of_int h)/.(float_of_int w)
  in
  let i={image_file=imageFile;
         image_width=fw;
         image_height=fh;
         image_x=0.;
         image_y=offset;
         image_order=0
        }
  in
  let img={
    drawing_min_width=fw;
    drawing_max_width=fw;
    drawing_nominal_width=fw;
    drawing_width_fixed = true;
    drawing_adjust_before = false;
    drawing_y0=offset;
    drawing_y1=fh+.offset;
    drawing_break_badness=0.;
    drawing_states=IntSet.empty;
    drawing_badness=(fun _->0.);
    drawing_contents=(fun _->[OutputCommon.Image i])
  }
  in
  image#destroy;
  img

#else
let image ?scale:(scale=0.) ?width:(width=0.) ?height:(height=0.) ?offset:(offset=0.) (_:string) (_:environment)=
  {
    drawing_min_width=0.;
    drawing_max_width=0.;
    drawing_nominal_width=0.;
    drawing_width_fixed = true;
    drawing_adjust_before = false;
    drawing_y0=0.;
    drawing_y1=0.;
    drawing_states=IntSet.empty;
    drawing_break_badness=0.;
    drawing_badness=(fun _->0.);
    drawing_contents=(fun _->[])
  }
#endif

let includeGraphics ?scale:(scale=0.) ?width:(width=0.) ?height:(height=0.) ?offset:(offset=0.) imageFile=
  [bB (fun env->[Drawing (image ~scale ~width ~height ~offset imageFile env)])]

(** {3 Boxification}*)


(**/**)
let sources=ref StrMap.empty
let rStdGlue:(float*box) ref=ref (0.,glue 0. 0. 0.)
(**/**)

(* let ambientBuf=ref ([||],0) *)
(** Makes a glue from the unicode character code given in the argument. *)
let makeGlue env x0=
  let stdGlue=
    (if fst !rStdGlue <> env.size then rStdGlue:=(env.size, glue (2.*.env.size/.9.) (env.size/.3.) (env.size/.2.)));
    snd !rStdGlue
  in
  if (x0>=0x0009 && x0<=0x000d) || x0=0x0020 then stdGlue else
      match x0 with
          0x00a0->(match stdGlue with
              Glue y->(
                Drawing y
              )
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

(** Converts a [string] to a list of glyphs, according to the environment. *)
let gl_of_str env string=
  try
    hyphenate env.hyphenate env.substitutions env.positioning env.font env.size
      env.fontColor
      (env.word_substitutions string)
  with
      Glyph_not_found g -> []

(**/**)
let append buf nbuf x=
  let arr=
    if !nbuf>=Array.length !buf then
      Array.init (max 1 (2*Array.length !buf)) (fun j->if j< !nbuf then (!buf).(j) else Empty)
    else !buf
  in
    arr.(!nbuf)<-x;
    buf:=arr;
    incr nbuf

let concat buf1 nbuf1 buf2 nbuf2=
  for i=0 to nbuf2-1 do
    append buf1 nbuf1 buf2.(i)
  done

let mappend m x=
  let a=try fst (IntMap.max_binding m) with Not_found -> -1 in
  IntMap.add (a+1) x m


module UNF8=CamomileLibraryDefault.Camomile.UNF.Make(CamomileLibrary.UTF8)
(**/**)


(** Converts a list of contents into a list of boxes, which is the next Patoline layer. *)
let boxify buf nbuf fixable env0 l=
  let rec boxify keep_cache env=function
      []->env
    | B (b, cache)::s->
      let l = match !cache with
	  Some l when keep_cache-> l
        | _ -> (
            let acc= !env_accessed in
            env_accessed:=false;
            let l = b env in
            (if keep_cache && not !env_accessed then cache := Some l else fixable:=true);
            env_accessed:=acc || !env_accessed;
            l
          )
      in
      (List.iter (append buf nbuf) l; boxify keep_cache env s)
    | (C b)::s->(
        let acc= !env_accessed in
        env_accessed:=false;
        let c = b env in
        let env'=if !env_accessed then (
          fixable:=true;
          boxify false env c
        ) else boxify keep_cache env c
        in
        env_accessed:=acc || !env_accessed;
        boxify keep_cache env' s
      )
    | Env f::s->boxify keep_cache (f env) s
    | (T (t,cache))::s->(
      match !cache with
	    Some l when keep_cache ->(
              IntMap.iter (fun _->List.iter (append buf nbuf)) l;
              boxify keep_cache env s
            )
          | _ ->(
            (* let buf=ref [|Empty|] in *)
            (* let nbuf=ref 0 in *)
            let t=try UTF8.validate t;t with _->(
              Printf.fprintf stderr "%s\n" (TypoLanguage.message (TypoLanguage.BadEncoding t));
              let rec count i n=if i>=String.length t then n else
                  count (i+1) (if t.[i]>char_of_int 0x7f then n else (n+1))
              in
              let tt=String.create (count 0 0) in
              let rec filter i n=if i<String.length t then (
                if t.[i]>char_of_int 0x7f then (tt.[n]<-t.[i]; filter (i+1) (n+1))
                else filter (i+1) n
              )
              in
              filter 0 0;tt
            )
            in
            let l=ref IntMap.empty in
            let rec cut_str i0 i=
              if i>=String.length t then (
                let sub=String.sub t i0 (i-i0) in
                l:=mappend !l (gl_of_str env (UNF8.nfkc sub));
              ) else (
                if is_space (UTF8.look t i) then (
                  let sub=String.sub t i0 (i-i0) in
                  l:=mappend !l (gl_of_str env (UNF8.nfkc sub));
                  if i<>i0 || i=0 then l:=mappend !l [makeGlue env (UChar.uint_code (UTF8.look t i))];
                  cut_str (UTF8.next t i) (UTF8.next t i)
                ) else (
                  cut_str i0 (UTF8.next t i)
                )
              )
            in
            cut_str 0 0;
            if keep_cache then cache:=Some !l;
            IntMap.iter (fun _->List.iter (append buf nbuf)) !l;
            boxify keep_cache env s
          )
    )
    | FileRef (file,off,size)::s -> (
        let i=try
	  StrMap.find file !sources
        with _-> (let i=open_in_bin file in sources:= StrMap.add file i !sources; i)
        in
        let buffer=String.create size in
        let _=seek_in i off; really_input i buffer 0 size in
        boxify keep_cache env (tT buffer::s)
      )
    | Scoped (fenv, p)::s->(
        let env'=fenv env in
        let _=boxify keep_cache env' p in
        boxify keep_cache env s
      )
  in
  boxify true env0 l

(** Typesets boxes on a single line, then converts them to a list of basic
    drawing elements: [OutputCommon.raw]. *)
let draw_boxes env l=
  let rec draw_boxes x y dr l=match l with
      []->dr,x
    | Kerning kbox::s ->(
      let dr',x'=draw_boxes (x+.kbox.kern_x0) (y+.kbox.kern_y0) dr [kbox.kern_contents] in
      draw_boxes (x'+.kbox.advance_width) y dr' s
    )
    | Hyphen h::s->(
      let dr1,w1=Array.fold_left (fun (dr',x') box->
        let dr'',x''=draw_boxes x' y dr' [box] in
        dr'',x''
      ) (dr,x) h.hyphen_normal
      in
      draw_boxes w1 y dr1 s
    )
    | GlyphBox a::s->(
      let box=OutputCommon.Glyph { a with glyph_x=a.glyph_x+.x;glyph_y=a.glyph_y+.y } in
      let w=a.glyph_size*.Fonts.glyphWidth a.glyph/.1000. in
      draw_boxes (x+.w) y (box::dr) s
    )
    | Glue g::s
    | Drawing g ::s->(
      let w=g.drawing_nominal_width in
      let box=(List.map (translate (x) (y)) (g.drawing_contents w)) in
      draw_boxes (x+.w) y (box@dr) s
    )
    | Marker (BeginURILink l)::s->(
      let link={ link_x0=x;link_y0=y;link_x1=x;link_y1=y;uri=l;
                 link_order=0;
                 link_closed=false;
                 dest_page=(-1);dest_x=0.;dest_y=0.;is_internal=false;
                 link_contents=[] }
      in
      draw_boxes x y (Link link::dr) s
    )
    | Marker (BeginLink l)::s->(
      let dest_page=
        try
          let line=MarkerMap.find (Label l) env.user_positions in
          page line
        with
            Not_found->(-1)
      in
      let link={ link_x0=x;link_y0=y;link_x1=x;link_y1=y;uri=l;
                 link_order=0;
                 link_closed=false;
                 dest_page=dest_page;
                 dest_x=0.;dest_y=0.;is_internal=true;
                 link_contents=[]
               }
      in
      draw_boxes x y (Link link::dr) s
    )
    | Marker EndLink::s->(
      let rec link_contents u l=match l with
          []->List.rev u
        | (Link h)::s when not h.link_closed->(
          h.link_contents<-u;
          let (_,y0,_,y1)=bounding_box u in
          h.link_y0<-y0;
          h.link_y1<-y1;
          h.link_closed<-true;
          h.link_x1<-x;
          Link h::s
        )
        | h::s->link_contents (h::u) s
      in
      draw_boxes x y (link_contents [] dr) s
    )

    | b::s->
      let _,w,_=box_interval b in
      draw_boxes (x+.w) y dr s
  in
  fst (draw_boxes 0. 0. [] l)


let rec bezier_of_boxes=function
    []->[]
  | Glyph g::s->
      let out=Fonts.outlines g.glyph in
        (List.map (fun (x,y)->Array.map (fun xx->g.glyph_x+.xx *. g.glyph_size/.1000.) x,
                     Array.map (fun xx->g.glyph_y+.xx *. g.glyph_size/.1000.) y)
           (List.concat out)) @ (bezier_of_boxes s)
  | Path (_,p)::s->
      (List.concat (List.map Array.to_list p))@(bezier_of_boxes s)
  | _::s-> bezier_of_boxes s

let adjust_width env buf nbuf =
  (* FIXME : à prendre dans l'env *)
  let alpha = env.adjust_optical_alpha in
  let beta = env.adjust_optical_beta in 
  let char_space = env.normalLead *. env.adjust_min_space in
  let epsilon = env.adjust_epsilon in
  let dsup,dinf as dir = (-.cos(alpha), sin(alpha)), (-.cos(alpha), -.sin(alpha)) in
  let dsup',dinf' as dir' = (cos(alpha), -.sin(alpha)), (cos(alpha), sin(alpha)) in
  let profile_left = ref [] in
  let buf = !buf in
  let i0 = ref 0 in
  while !i0 < !nbuf do
    match buf.(!i0) with
    | Glue x ->
      profile_left := Distance.translate_profile !profile_left (-.x.drawing_nominal_width);
      incr i0;

    | Drawing _ | GlyphBox _ | Hyphen _ as x0-> (
	let adjust = ref (match x0 with
	    Drawing x -> if x.drawing_width_fixed then None else Some(x0,!i0)
	  | _ -> None)
	in
	let min = ref 0.0 in
	let nominal = ref 0.0 in
	let max = ref 0.0 in

	let left = draw_boxes env [x0] in
	let bezier_left = bezier_of_boxes left in
	let profile_left' = Distance.bezier_profile dir epsilon bezier_left in
	let (x0_l,y0_l,x1_l,y1_l)=bounding_box_kerning left in

	if !Distance.debug then
	  Printf.fprintf stderr "Drawing(1): i0 = %d (%d,%d)\n" !i0 (List.length !profile_left) (List.length profile_left');

	profile_left := Distance.translate_profile (Distance.profile_union dir  !profile_left  profile_left') (x0_l -. x1_l);

	incr i0;
	try while !i0 < !nbuf do
	  match buf.(!i0) with
	  | Marker AlignmentMark -> incr i0; raise Exit
	  | Marker _ -> incr i0	  
	  | Drawing x as b when x.drawing_nominal_width = 0.0 ->
	    if !Distance.debug then Printf.fprintf stderr "0 Drawing(2)\n";
	    if !adjust = None && not x.drawing_width_fixed then adjust := Some(b,!i0);
	    incr i0
	  | Glue x as b ->
	    min := !min +.  x.drawing_min_width;
	    max := !max +.  x.drawing_max_width;
	    nominal := !nominal +. x.drawing_nominal_width;
            profile_left := Distance.translate_profile !profile_left (-.x.drawing_nominal_width);
	    if !adjust = None && not x.drawing_width_fixed then adjust := Some(b,!i0);
	    incr i0
	  | Drawing _ | GlyphBox _ | Hyphen _ as y0 -> (
	    let before = 
	      match y0 with 
		Drawing y when !adjust = None && y.drawing_adjust_before ->
		  adjust := Some(y0, !i0);
		  true
	      | _ -> false
	    in
	    match !adjust with
	    | None -> raise Exit
	    | Some (b,i) -> 


	      let right = draw_boxes env [y0] in
	      let profile_left = !profile_left in
	      let bezier_right = bezier_of_boxes right in
	      let profile_right = Distance.bezier_profile dir' epsilon bezier_right in
	      if !Distance.debug then
		Printf.fprintf stderr "Drawing(2): i0 = %d (%d,%d)\n" !i0 (List.length profile_left) (List.length profile_right);
	      if profile_left = [] || profile_right = [] then raise Exit;

	      if !Distance.debug then
		Printf.fprintf stderr "Drawing(2b): i0 = %d\n" !i0;

	      let d space = 
		let pr = List.map (fun (x,y) -> (x+.space,y)) profile_right in
		let r = Distance.distance beta dir profile_left pr in
		r
	      in
	
	      let (x0_r,y0_r,x1_r,y1_r)=bounding_box_kerning right in
	      let (x0_r',y0_r',x1_r',y1_r')=bounding_box_full right in


 	      let nominal' = !nominal +. char_space in
	      let min' = Pervasives.min  (Pervasives.max (x0_r -. x1_r) (x0_l -. x1_l))  (!min -. nominal') in
	      let max' = Pervasives.max (2. *. char_space) (!max -. nominal') in
	      let da = d min' in
	      let db = d max' in
	      let target = nominal' in
	      
	      if !Distance.debug then
		Printf.fprintf stderr "start Adjust: min = %f => %f, max = %f => %f, target = %f\n" min' da max' db nominal';

	      let epsilon = epsilon /. 16. in
	      let r  =
		if da > target then min' else
		  if db < target then max' else (
		    
		    let rec fn sa da sb db  =
		      let sc = (sa +. sb) /. 2.0 in
		      let dc = d sc in
		      if abs_float (dc -. target) < epsilon || (sb -. sa) < epsilon then sc
		      else if dc < target then fn sc dc sb db 
		      else fn sa da sc dc
		    in
		    fn min' da max' db)
												    
	      in
	 
(*	      let r = r -. x0_r' +. x0_r -. x1_l +. x1_l' in*)

	      if !Distance.debug then Printf.fprintf stderr "end Adjust: r = %f nominal = %f" r !nominal;
   
	      buf.(i) <- 
		(match b with
		| Drawing x when before -> Drawing { x with 
		  drawing_contents = 
		      (fun w -> List.map (translate (r +. x0_r' -. x0_r) 0.0) (x.drawing_contents w))
		}
		| Drawing x -> Drawing { x with 
		  drawing_nominal_width = r +. x.drawing_nominal_width;
		  drawing_min_width = r +. x.drawing_min_width;
		  drawing_max_width = r +. x.drawing_max_width;
		}
		| Glue x -> Glue { x with
		  drawing_nominal_width = r +. x.drawing_nominal_width;
		  drawing_min_width = r +. x.drawing_min_width;
		  drawing_max_width = r +. x.drawing_max_width;
		}
		| _ -> assert false);
	      raise Exit)
	    | _ -> 
	      incr i0;
	      raise Exit

	  done with Exit -> ())
    | _ -> incr i0
  done

(** The same as boxify, but discards the final environment. *)
let boxify_scoped env x=
  let buf=ref [||] in
  let nbuf=ref 0 in
  let _=boxify buf nbuf (ref false) env x in
  adjust_width env buf nbuf;
  Array.to_list (Array.sub !buf 0 !nbuf)

(** Composes [boxify] and [draw_boxes] *)
let draw env x=
  let buf=ref [||] in
  let nbuf=ref 0 in
  let env'=boxify buf nbuf (ref false) env x in
  adjust_width env buf nbuf;
  draw_boxes env' (Array.to_list (Array.sub !buf 0 !nbuf))






module type DocumentStructure=sig
  val structure:(tree*(int*tree) list) ref
  val fixable:bool ref
end
module type Format=sig
  val defaultEnv:environment
  val postprocess_tree:tree->tree
  val title :
    (tree *
       (IntMap.key *
          tree)
            list)
           ref ->
    ?label:'a ->
    ?extra_tags:(string * string) list ->
    content list -> bool
  val parameters:environment -> box array array -> drawingBox array -> parameters ->  Break.figurePosition IntMap.t ->line MarkerMap.t -> line -> parameters
end


(** "flattens" a document tree to an array of paragraphs, a paragraph
    being an array of boxes. *)
let flatten env0 fixable str=
  let paragraphs=ref [] in
  let trees=ref [] in
  let figures=ref IntMap.empty in
  let figure_trees=ref IntMap.empty in
  let fig_param=ref IntMap.empty in
  let param=ref [] in
  let new_page_list=ref [] in
  let new_line_list=ref [] in
  let compl=ref [] in
  let bads=ref [] in
  let n=ref 0 in

  let buf=ref [||] in
  let nbuf=ref 0 in
  let frees=ref 0 in
  let add_paragraph env tree path p=
    nbuf:= !frees;
    let v=boxify buf nbuf fixable env p.par_contents in
    adjust_width env buf nbuf;
    paragraphs:=(Array.sub !buf 0 !nbuf)::(!paragraphs);
    trees:=(tree,path)::(!trees);
    compl:=(p.par_completeLine env)::(!compl);
    param:=(p.par_parameters env)::(!param);
    new_page_list:=(env.new_page)::(!new_page_list);
    new_line_list:=(env.new_line env)::(!new_line_list);
    bads:=(p.par_badness env)::(!bads);
    incr n;
    frees:=0;
    v
  in

  let rec flatten flushes env_ path noindent tree=
    match tree with
        Paragraph p -> (
          p.par_paragraph <- List.length !paragraphs;
          add_paragraph env_ tree path p
        )
      | FigureDef f -> (
        let env1=f.fig_env env_ in
        let n=IntMap.cardinal !figures in
        fig_param:=IntMap.add n (f.fig_parameters env1) !fig_param;
        figures:=IntMap.add n (f.fig_contents env1) !figures;
        figure_trees:=IntMap.add n (tree,path) !figure_trees;
        append buf frees (BeginFigure n);
        f.fig_post_env env_ env1
      )
      | Node s-> (
        let env=
          let level=
            try
              List.length (snd (StrMap.find "_structure" env_.counters))
            with Not_found->0
          in
          { env_ with counters=StrMap.map (fun (lvl,l)->if lvl>level then lvl,[] else lvl,l)
              env_.counters }
        in
        s.node_paragraph <- List.length !paragraphs;
        s.boxified_displayname <- draw_boxes env (boxify_scoped env s.displayname);
        let flushes'=ref [] in
        let flat_children k a (is_first,indent, env1)=match a with
            Paragraph p->(
              let env2=flatten flushes' (p.par_env env1) ((k,tree)::path)
                (noindent || List.mem_assoc "noindent" s.node_tags)
                (Paragraph { p with par_contents=
                    (if is_first then (
                      let name=String.concat "_" ("_"::List.map string_of_int (List.map fst path)) in
                      [Env (fun env->
                        let w=try let (_,_,w)=StrMap.find name (names env) in w with
                            Not_found -> uselessLine in
                        { env with names=StrMap.add name (env.counters, "_", w)
                            (names env) });
                       bB (fun _->[Marker (Label name)])
                      ]
                     ) else [])@
                      (if indent
                          && not (List.mem_assoc "noindent" s.node_tags)
                       then
                          [bB (fun env->(p.par_env env).par_indent);
                           Env (fun env->{env with par_indent=[]})]
                       else
                          [])
                              @ p.par_contents
                          }
              ) in
              false,true, p.par_post_env env1 env2
            )
          | FigureDef f as h->(
            let env2=flatten flushes' env1 ((k,tree)::path) noindent h in
            let num=try
                      match StrMap.find "_figure" env2.counters with
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
            let env3=flatten flushes' env2 ((k,tree)::path)
              (noindent || List.mem_assoc "noindent" h.node_tags)
              tr
            in
            (is_first),
            (not (List.mem_assoc "structural" h.node_tags) &&
               not (List.mem_assoc "structure" h.node_tags)
            ),
            h.node_post_env env1 env3
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
  let env2=flatten (ref []) env1 [] false str in
  let params=Array.init
    (IntMap.cardinal !figures)
    (fun i->IntMap.find i !fig_param)
  in
  (env2, params,
   Array.of_list (List.rev !param),
   Array.of_list (List.rev !new_page_list),
   Array.of_list (List.rev !new_line_list),
   Array.of_list (List.rev !compl),
   Array.of_list (List.rev !bads),
   Array.of_list (List.rev !paragraphs),
   Array.of_list (List.rev !trees),
   Array.of_list (List.map snd (IntMap.bindings !figures)),
   Array.of_list (List.map snd (IntMap.bindings !figure_trees)))

let rec make_struct positions tree=
  match tree with
      Node s when s.node_paragraph>=0 && s.node_paragraph<Array.length positions -> (
        let (p,x,y)=positions.(s.node_paragraph) in
        let rec make=function
        []->[]
          | (_,Node u)::s when List.mem_assoc "intoc" u.node_tags -> (make_struct positions (Node u))::(make s)
          | _ :: s->make s
        in
        let a=Array.of_list (make (IntMap.bindings s.children)) in
        { OutputCommon.name=s.name;
          OutputCommon.metadata=[];
	  OutputCommon.displayname=s.boxified_displayname;
          OutputCommon.tags=s.node_tags;
          OutputCommon.page=p;
          OutputCommon.struct_x=x;
          OutputCommon.struct_y=y;
          OutputCommon.substructures=a }
      )
    | Node s -> (
      let rec make=function
      []->[]
        | (_,Node u)::s when List.mem_assoc "intoc" u.node_tags -> (make_struct positions (Node u))::(make s)
        | _ :: s->make s
      in
      let a=Array.of_list (make (IntMap.bindings s.children)) in
      { OutputCommon.name=s.name;
        OutputCommon.metadata=[];
	OutputCommon.displayname=s.boxified_displayname;
        OutputCommon.tags=s.node_tags;
        OutputCommon.page=0;
        OutputCommon.struct_x=0.;
        OutputCommon.struct_y=0.;
        OutputCommon.substructures=a }
    )
    | _->
      { OutputCommon.name="";
        OutputCommon.metadata=[];
	OutputCommon.displayname=[];
        OutputCommon.tags=[];
        OutputCommon.page=0;
        OutputCommon.struct_x=0.;
        OutputCommon.struct_y=0.;
        OutputCommon.substructures=[||] }

(** Adds a tag to the given structure. *)
let tag str tags=
  match str with
      Node n->Node { n with node_tags=tags@n.node_tags }
    | _->Node { empty with node_tags=tags; children=IntMap.singleton 0 str }

(** Label updating after optimization. *)
let update_names env figs user=
  let user=MarkerMap.fold (MarkerMap.add) user env.user_positions in
  let needs_reboot=ref false in (* (fil user<>fil env.user_positions) in; *)
  let env'={ env with user_positions=user;
               names=
      StrMap.fold (fun k (a,b,c) m->try
                     let pos=
                       if b="_figure" then
                         (match StrMap.find "_figure" a with
                              _,[]->(Printf.fprintf stderr "figure not found (1):%S\n" k;
                                   raise Not_found)
                            | _,(h::_)->(
                                match IntMap.find h figs with
                                    Break.Placed l->l
                                  | _->raise Not_found
                              )
                         )
                       else
                         MarkerMap.find (Label k) user
                     in
                       if pos<>c && b<>"_" then (
                         (* Printf.fprintf stderr "reboot : position of %S (%S) changed\n" k b *)
                       );
                       needs_reboot:= !needs_reboot || (pos<>c);
                       StrMap.add k (a,b,pos) m
                   with Not_found -> ((* Printf.fprintf stderr "reboot : position of %S (%S) not found\n" k b; *)needs_reboot:=true; m)
                  ) (names env) (names env)
           }
  in
    flush stderr;
    env',!needs_reboot

(** Resets all the counters, preserving their levels. *)
let reset_counters env=
  { env with
    counters=StrMap.map (fun (l,_)->(l,[])) env.counters }

let animation names ft f0 contents =
  [bB (fun env -> let contents a = draw env (contents a) in
  let contents_0 = contents f0 in
  let r = Animation(contents_0, names, ft, contents) in
  let (x0,y0,x1,y1)=bounding_box contents_0 in
  let w = x1 -. x0 in
  [Drawing {
    drawing_min_width=w;
    drawing_max_width=w;
    drawing_nominal_width=w;
    drawing_width_fixed = true;
    drawing_adjust_before = false;
    drawing_y0=y0;
    drawing_y1=y1;
    drawing_states=IntSet.empty;
    drawing_break_badness=0.;
    drawing_badness=(fun _->0.);
    drawing_contents=(fun _->[r])
  }])]
