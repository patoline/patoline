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

(** High-level representation of documents

The module defines the {!type:tree} type, which describes whole
documents. This tree is typically produced by running the OCaml
executable obtained after parsing a [.txp] file, but can be produced by
anyy other mean. It is the main input to Patoline Typography library in
order to produce the final document.

Values of type {!type:tree} are meant to be transformed by some format's
output routine.

We also provide a tree zipper interface to ease construction of a
{!type:tree} when reading linearly an input file.
*)

open Patoraw
open Unicodelib
open Patutil
open Patfonts
open Extra
open Fonts
open FTypes
open RawContent
open Driver
open Box


(** {2 Font, substitutions, positioning} *)

type fontAlternative = Regular | Bold | Caps | Demi

let simpleFamilyMember:(unit->font)->(font*(glyph_id list -> glyph_id list)*(glyph_ids list -> glyph_ids list)) Lazy.t =
  fun a->Lazy.from_fun (fun ()->(a (),(fun x->x),(fun x->x)))

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
type fontFamily =
  fontAlternative *
    ((font*(glyph_id list -> glyph_id list)*(glyph_ids list -> glyph_ids list)) Lazy.t *
     (font*(glyph_id list -> glyph_id list)*(glyph_ids list -> glyph_ids list)) Lazy.t)


module TS = Break.Make(
  struct
    type t = line

    let compare a b =
      if a.paragraph   < b.paragraph   then -1 else
      if a.paragraph   > b.paragraph   then  1 else
      if a.lineStart   < b.lineStart   then -1 else
      if a.lineStart   > b.lineStart   then  1 else
      if a.lineEnd     < b.lineEnd     then -1 else
      if a.lineEnd     > b.lineEnd     then  1 else
      if a.hyphenStart < b.hyphenStart then -1 else
      if a.hyphenStart > b.hyphenStart then  1 else
      if a.hyphenEnd   < b.hyphenEnd   then -1 else
      if a.hyphenEnd   > b.hyphenEnd   then  1 else
      if a.lastFigure  < b.lastFigure  then -1 else
      if a.lastFigure  > b.lastFigure  then  1 else
      if a.isFigure    < b.isFigure    then -1 else
      if a.isFigure    > b.isFigure    then  1 else
      if a.height      < b.height      then -1 else
      if a.height      > b.height      then  1 else 0

    let hash a=Hashtbl.hash a
  end)


(** {2 Mathematical formulae} *)

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

(** {2 Environments} *)

(** Environments. These are typically folded on document trees, and
    control many different things about the fonts, counters, or
    labels. *)
type environment={
  fontFamily:fontFamily list;
  fontMonoFamily:fontFamily list;
  fontMonoRatio:float; (* size adjustment of the two previous family *)
  fontItalic:bool;
  fontAlternative:fontAlternative;
  fontFeatures:string list;
  fontColor:Color.color;
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
  substitutions:glyph_id list -> glyph_id list;
  positioning:glyph_ids list -> glyph_ids list;
  counters:(int*int list) StrMap.t;     (** Niveau du compteur, état.  *)
  last_changed_counter:string;
  names:((int*int list) StrMap.t * string * line) StrMap.t; (** Niveaux de tous les compteurs à cet endroit, type, position  *)
  fixable:bool ref;
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
  stdGlue:float*float*float;
}

let env_accessed=ref false
let names env=
  env_accessed:=true;
  env.names
let user_positions env=
  env_accessed:=true;
  env.user_positions
let displayname n=
  env_accessed:=true;
  n.raw_name

(** {2 Document content} *)

(** Main type used to hold document contents. *)
type content =
  | B of (environment -> box list) * box list option ref
  (** List of boxes depending on an environment. The second parameters is a
     cache used when compilation is iterated to resolve names. *)

  | C of (environment -> content list)
  (** A contents list depending on the environment. This may be used to
     typeset the state of a counter for example. *)

  | T of string * (box list IntMap.t option) ref
  (** Simple text. *)

  | Env of (environment -> environment)
  (** Environment modification function. It can be used to register a name
     or modify the state of a counter for instance. *)

  | Scoped of (environment -> environment) * (content list)
  (** A scoped environment transformation applied on a (small) list of
     contents. *)

  | N of tree
  (** A document tree. *)

(** First type of leaves in a document: paragraphs. *)
and paragraph =
  { par_contents   : content list
  ; par_env        : environment -> environment
  ; par_post_env   : environment -> environment -> environment
  ; par_parameters : environment -> Box.box array array -> Box.drawingBox array
                       -> parameters ->  Break.figurePosition IntMap.t
                       -> line MarkerMap.t -> line -> line -> parameters
  ; par_badness    : environment -> Box.box array array -> Box.drawingBox array
                       -> Break.figurePosition IntMap.t -> Box.line
                       -> Box.box array -> int -> Box.parameters -> float
                       -> Box.line -> Box.box array -> int
                       -> Box.parameters -> float -> float
  ; par_completeLine : environment -> Box.box array array
                         -> Box.drawingBox array
                         -> Break.figurePosition IntMap.t -> line MarkerMap.t
                         -> line -> bool -> line list
  ; par_states : int list
  ; par_paragraph : int
}

(** Second type of leaves in a document: figures. *)
and figuredef =
  { fig_contents   : environment -> Box.drawingBox
  ; fig_env        : environment -> environment
  ; fig_post_env   : environment -> environment -> environment
  ; fig_parameters : environment -> Box.box array array -> Box.drawingBox array
                       -> parameters -> Break.figurePosition IntMap.t
                       -> line MarkerMap.t -> line -> line -> parameters
}

(** Internal node of the document tree (e.g. section, chapter...). *)
and node =
  { name          : string
  ; displayname   : content list
  ; mutable boxified_displayname : raw list
  (* Extensible array of childrens : *)
  ; children      : tree IntMap.t
  ; node_tags     : (string * string) list
  (* Environment modification function applied when entering the node : *)
  ; node_env      : environment -> environment
  (* Environment modification function applied when leaving the node : *)
  ; node_post_env : environment -> environment -> environment
  (* Page states in which the contents is visible. *)
  ; node_states   : int list
  ; mutable node_paragraph : int }

(** Type of a document tree. *)
and tree =
  | Paragraph of paragraph
  | FigureDef of figuredef
  | Node      of node

(** Empty node (with no child tree). *)
let empty : node =
  { name = ""
  ; node_tags = []
  ; displayname = []
  ; boxified_displayname = []
  ; children = IntMap.empty
  ; node_env = (fun x->x)
  ; node_post_env =
    (fun x y -> { x with counters = y.counters ; names = names y
                ; user_positions = user_positions y })
  ; node_states = []
  ; node_paragraph = 0 }

(** Build a node with a single child tree. *)
let singleton : tree -> node = fun t ->
  { empty with children = IntMap.singleton 0 t }

(** The main datatype is a zipper over a document tree. It consists in a
   couple whose first component is a tree. The second component represents
   the context identifying a position in the tree.

   The tree represented by the zipper [(t, [(p1,t1), ... , (pn,tn)])] is
   built by:
   + appending the tree [t] at position [p1] in [t1],
   + appending the resulting tree at position [p2] in [t2],
   + ...
   + appending the resulting tree at position [pn] in [tn]. *)
module TreeData = struct
  type nonrec node = node
  type nonrec tree = tree

  let tree_of_node node = Node(node)

  let node_of_tree = function
    | Node(node) -> node
    | _ -> invalid_arg "Document.TreeData.node_of_tree"

  let get_child node i = IntMap.find i node.children

  let set_child node i tree =
    {node with children = IntMap.add i tree node.children}

  let remove_child node i =
    {node with children = IntMap.remove i node.children}

  let has_child node i =
    IntMap.mem i node.children

  let min_index node =
    fst (IntMap.min_binding node.children)

  let max_index node =
    fst (IntMap.max_binding node.children)

end

module DocZipper = Zipper.Make(TreeData)

type tree_zipper = DocZipper.zipper

(** Build a zipper from a tree. The resulting zipper points to the root
    of the tree. *)
let zipper_of_tree = DocZipper.zipper_to_tree

(** Build a zipper whose single node is {!val:empty}. *)
let empty_zipper = DocZipper.empty empty

(** Function that takes a tree zipper [(t,cxt)] pointing to some node
   [t] and returns a zipper pointing to the father node of [t]. If this
   function is called on a zipper that points to the root of the tree, a
   new empty node is created to have [t] as its only child. *)
let up = DocZipper.up
  [@@ocaml.deprecated "Use DocZipper.up instead"]

(** Function that applies {!val:up} n times on a zipper, effectively moving the
   zipper to the n-th ancestor of the currently pointed node. *)
let up_n = DocZipper.up_n

(** Move the zipper to the root of the tree *)
let top = DocZipper.top

(** Retrieve the complete tree from a zipper *)
let tree_of_zipper = DocZipper.zipper_to_tree

(** Move the zipper to point to the child of the pointed node with the higher
   index. If the pointed tree is not a node the zipper is left unchanged. *)
let lastChild zip =
  try DocZipper.down_last zip
  with Invalid_argument(_) -> zip

(** Take a zipper [zip] and a tree [c] and adds [c] as the last child of the
   pointed node. If the pointed subtree is not a node, a new node is
   created to hold [t] and [c]. The returned zipper points to [c]. *)
let rec newChildAfter : tree_zipper -> tree -> tree_zipper =
  let next_key t = try fst (IntMap.max_binding t) + 1 with Not_found -> 0 in
  fun (t,cxt) c ->
    match (t, cxt) with
    | (Node x, _ ) -> (c, (next_key x.children,x)::cxt)
    | (_     , []) -> (c, [(1, singleton t)])
    | _            -> newChildAfter (up (t,cxt)) c

(** Same as {!val:newChildAfter} but adds the tree as the first child. *)
let rec newChildBefore : tree_zipper -> tree -> tree_zipper =
  let prev_key t = try fst (IntMap.min_binding t) - 1 with Not_found -> 0 in
  fun (t,cxt) c ->
    match (t, cxt) with
    | (Node x, _)  -> (c, (prev_key x.children,x)::cxt)
    | (_     , []) -> (c, [(1, singleton t)])
    | _            -> newChildBefore (up (t,cxt)) c

(** Take a zipper pointing to a node and move it down its i-th child. If the
   zipper does not point to a node, [Invalid_argument] is raised. If the i-th
   child does not exists, it is created as a new empty node. *)
let child : tree_zipper -> int -> tree_zipper =
  fun (t,cxt) i ->
    match t with
    | Node n -> let t =
                  try IntMap.find i n.children with Not_found -> Node empty
                in (t, (i,n)::cxt)
    | _      -> raise (Invalid_argument "Typography.child")

(** Take a tree zipper and an path represented as a list of integers and move
   the zipper down the path (i.e. calling child on the successive indices. *)
let rec follow : tree_zipper -> int list -> tree_zipper =
  fun z -> function
    | []      -> z
    | n :: ns -> follow (child z n) ns

(** Module type of a document format. *)
module type Format =
  sig
    val defaultEnv : environment
    val postprocess_tree : tree -> tree
    val title : (tree * (IntMap.key * tree) list) ref -> ?label:'a
          -> ?extra_tags:(string * string) list -> content list -> bool
    val parameters : environment -> box array array -> Box.drawingBox array
          -> parameters -> Break.figurePosition IntMap.t -> line MarkerMap.t
          -> line -> parameters
  end

(** Module type to be used as a document wrapper. The document structure is
   stored in its zipper form in a reference. Functions are provided below
   to edit the document tree. *)
module type DocumentStructure =
  sig
    val structure : tree_zipper ref
  end






let doc_tags n=match n with
    Node n->n.node_tags
  | _->[]


let init_env_hook = ref ([] : (environment -> environment) list)
let add_env_hook f = init_env_hook := f::!init_env_hook

let bB f = B(f,ref None)
let uB f = C(fun _->env_accessed:=true;[bB f])
let tT f = T(f,ref None)
let uT f = C(fun _->env_accessed:=true;[tT f])
let string_of_contents l =
  let buf=Buffer.create 1000 in
  let rec fill_buf t=match t with
      T (str,_)::s->(
        if Buffer.length buf>0 then (
          Buffer.add_string buf " ";
        );
        Buffer.add_string buf str;
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
  Buffer.contents buf

let raw : (environment -> RawContent.raw list) -> content = fun f ->
  let contents _ =
    let dr env =
      let raw = f env in
      let (x0,y0,x1,y1) = RawContent.bounding_box raw in
      let w = x1 -. x0 in
      let open Box in
      { drawing_min_width     = w
      ; drawing_nominal_width = w
      ; drawing_max_width     = w
      ; drawing_width_fixed   = true
      ; drawing_adjust_before = false
      ; drawing_y0            = y0
      ; drawing_y1            = y1
      ; drawing_badness       = (fun _ -> 0.0)
      ; drawing_break_badness = infinity
      ; drawing_states        = []
      ; drawing_contents      = (fun _ -> raw) }
    in
    [bB (fun env -> [Drawing (dr env)])]
  in
  C contents

let _names env=
  env.names
let _user_positions env=
  env.user_positions

let incr_counter ?(level= -1) name env=
  { env with
    last_changed_counter=name;
    counters=
      StrMap.add name (try let a,b=StrMap.find name env.counters in
                         match b with
                             h::s -> (a,(h+1)::s)
                           | _->a,[0]
                       with
                           Not_found -> level, [0]
                      ) env.counters }

let pop_counter name env=
  { env with
    last_changed_counter=name;
    counters=
      StrMap.add name (let a,b=StrMap.find name env.counters in (a, List.drop 1 b)) env.counters }

let push_counter name env=
  { env with
    last_changed_counter=name;
    counters=
      StrMap.add name (let a,b=StrMap.find name env.counters in (a,0::b)) env.counters }

let tags=function
    Node n->n.node_tags
  | _->[]

(** Creates a new page, using 1/6th of the given lengths for margins.
 A page is implemented as two nested frames: the outer frame has the
 actual size of the whole page, while the inner frame size is the
 papersize minus margins.

 This function returns the inner frame.
 *)
let default_new_page pageFormat zip =
  let ((page, _) as zip)=Box.make_page pageFormat (frame_top zip) in
  let w = page.frame_x1 -. page.frame_x0
  and h = page.frame_y1 -. page.frame_y0 in
  let x0=(page.frame_x0+.1.*.w/.6.) in
  let y0=(page.frame_y0+.1.*.h/.6.) in
  let x1=(page.frame_x1-.1.*.w/.6.) in
  let y1=(page.frame_y1-.1.*.h/.6.) in
  frame x0 y0 x1 y1 zip

(** Creates a new page without any margin *)
let raw_new_page pageFormat zip =
  let (page, _) as zip = Box.make_page pageFormat (frame_top zip) in
  frame page.frame_x0 page.frame_y0 page.frame_x1 page.frame_y1 zip
(**/**)


let envApp l env =
  List.fold_left (fun env f -> f env) env l

let rec map_paragraphs f = function
  | Node n -> Node  { n with children=IntMap.map (map_paragraphs f) n.children }
  | Paragraph p -> Paragraph (f p)
  | x -> x


exception Found

(** Finds the last node satisfying a given predicate in a document tree. *)
let find_last f tr=
  let result=ref None in
  let rec find_last path tr=match tr with
    | _ when f tr->(
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

let n_go_up n str =
  for _ = 1 to n do go_up str done

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

let updateFont env font subst pos=
  let feat=Fonts.select_features font env.fontFeatures in
    { env with
        font=font;
        substitutions=(fun glyphs -> Fonts.apply_features font feat (subst glyphs));
        positioning=(fun x->pos (positioning font x)) }

let change_font f env = updateFont env f (fun x->x) (fun x->x)

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
  let font, subst, pos= selectFont env.fontFamily env.fontAlternative b in
  let env = { env with fontItalic = b } in
    updateFont env font subst pos

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

let envAlternative ?(features:'a option) alt env =
  let features = match features with
      None -> env.fontFeatures
    | Some f -> f
  in
  let font,subs,pos = selectFont env.fontFamily alt env.fontItalic in
  let env = { env with fontAlternative = alt } in
  add_features features (updateFont env font subs pos)

let alternative ?(features:'a option) alt t =
  [Scoped ((fun env -> envAlternative ?features alt env), t)]

let font_size_ratio font1 font2 =
  let x_h f =
    let f,_,_ = Lazy.force (fst (List.assoc Regular f)) in
    let x=Fonts.loadGlyph f
      ({empty_glyph with glyph_index=Fonts.glyph_of_char f 'o'}) in
    Fonts.glyph_y1 x -.  Fonts.glyph_y0 x
  in
  x_h font1 /. x_h font2

let envFamily fam env =
  let font,subs,pos = selectFont fam env.fontAlternative env.fontItalic in
  let env = { env with fontFamily = fam; size = font_size_ratio env.fontFamily fam *. env.size } in
  updateFont env font subs pos

let family fam t =
  [Scoped ((fun env -> envFamily fam env), t)]

let envMonoFamily fam env =
  { env with
    fontMonoFamily = fam;
    fontMonoRatio=font_size_ratio env.fontFamily fam }

let monoFamily fam t =
  [Scoped ((fun env -> envMonoFamily fam env), t)]

let envSize fsize env=
  { env with
      size=fsize;
      lead=env.lead*.fsize/.env.size }

(* Changer de taille dans un scope *)
let size fsize t=
  [Scoped (envSize fsize, t)]

let envScale alpha env =
  { env with size = env.size *. alpha }

(* Changer de taille dans un scope *)
let scale alpha t=
  [Scoped (envScale alpha, t)]

let envScaleLead alpha env=
  { env with
      lead=env.lead *. alpha }

let scaleLead alpha t=
  [Scoped (envScaleLead alpha, t)]

let envColor color env =
  {env with fontColor=color}

let color color t=
  [Scoped (envColor color, t)]

let envBold = envAlternative Bold
let bold    = alternative Bold

let envSv = envAlternative Caps
let sc = alternative Caps

let verbEnv x =
  { (envFamily x.fontMonoFamily (envScale x.fontMonoRatio x))
  with normalMeasure=infinity; par_indent = [] } (* For full paragraph *)

let verb p =
  [Scoped ((fun x -> envFamily x.fontMonoFamily
                               (envScale x.fontMonoRatio x)),
           p)] (* for inline text *)

let emph=toggleItalic
let id x=x

(****************************************************************)



(* Partie compliquée : il faut comprendre ce que fait l'optimiseur
   pour toucher à ça, ou apprendre en touchant ça *)


let parameters env pars figures _ last_figures _ _ line =
  let fn i figPos m =
    let open Break in
    match figPos with
    | Placed(l) when layout_page line = layout_page l
                  && line.height >= l.height +. figures.(i).drawing_y0
                  && line.height <= l.height +. figures.(i).drawing_y1
        -> env.normalMeasure -. figures.(i).drawing_nominal_width
    | _ -> m
  in
  let params =
    { measure            = IntMap.fold fn last_figures env.normalMeasure
    ; left_margin        = env.normalLeftMargin
    ; local_optimization = 0
    ; min_page_before    = 0
    ; min_page_after     = 0
    ; min_height_before  = 0.0
    ; min_height_after   = 0.0
    ; not_last_line      = false
    ; not_first_line     = false
    ; min_lines_before   = 1
    ; min_lines_after    = 0
    ; absolute           = false }
  in
  let fn params b = match b with Parameters(f) -> f params | _ -> params in
  fold_left_line pars fn params line

let set_parameters : (parameters -> parameters) -> content list =
  fun f -> [bB (fun _ -> [Parameters(f)])]

let vspaceBefore : float -> content list = fun sp ->
  let fn p = {p with min_height_before = max p.min_height_before sp} in
  set_parameters fn

let vspaceAfter : float -> content list = fun sp ->
  let fn p = {p with min_height_after = max p.min_height_after sp} in
  set_parameters fn

let pagesBefore : int -> content list = fun nb ->
  let fn p = {p with min_page_before = max p.min_page_before nb} in
  set_parameters fn

let pagesAfter : int -> content list = fun nb ->
  let fn p = {p with min_page_after = max p.min_page_after nb} in
  set_parameters fn

let linesBefore : int -> content list = fun nb ->
  let fn p = {p with min_lines_before = max p.min_lines_before nb} in
  set_parameters fn

let linesAfter : int -> content list = fun nb ->
  let fn p = {p with min_lines_after = max p.min_lines_after nb} in
  set_parameters fn

let notFirstLine : content list =
  set_parameters (fun p -> {p with not_first_line = true})

let notLastLine : content list =
  set_parameters (fun p -> {p with not_last_line = true})

let hspace : float -> content list = fun sp ->
  [bB (fun env -> let sp = sp *. env.size in [glue sp sp sp])]

let hfill : content list =
  [bB (fun env -> let mes = env.normalMeasure in [glue 0.0 (0.5 *. mes) mes])]

let do_center parameters a b c d e f g line =
  let param = parameters a b c d e f g line in
  let min_w = line.min_width in
  let nom_w = line.nom_width in
  if param.measure >= nom_w then
    let left_margin = param.left_margin +. (param.measure -. nom_w) /. 2.0 in
    {param with measure = nom_w; left_margin}
  else if param.measure < min_w then
    let left_margin = param.left_margin +. (param.measure -. min_w) /. 2.0 in
    {param with measure = min_w; left_margin}
  else param

let do_ragged_left parameters a b c d e f g line =
  let param = parameters a b c d e f g line in
  {param with measure = line.nom_width}

let do_ragged_right parameters a b c d e f g line =
  let param = parameters a b c d e f g line in
  let left_margin = param.left_margin +. param.measure -. line.nom_width in
  {param with measure = line.nom_width; left_margin}



let badness env paragraphs _ _
    node_i line_i max_i params_i comp_i
    node_j line_j max_j params_j comp_j=

  if node_j.paragraph>=Array.length paragraphs then 0. else (
    let v_bad=
      if layout_page node_i=layout_page node_j then (
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
    +. (if layout_page node_i<>layout_page node_j &&
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
                                counters=counters';
                                last_changed_counter="_figure"
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


(* Add a new paragraph (with given parameters) below the current node. *)
let newPar str ?(environment=(fun x->x)) ?(badness=badness) ?(states=[]) complete parameters par=
  let para =
    { par_contents     = par
    ; par_env          = environment
    ; par_post_env     = (fun env1 env2 ->
                            { env1 with names = names env2
                            ; counters        = env2.counters
                            ; user_positions  = user_positions env2 })
    ; par_parameters   = parameters
    ; par_badness      = badness
    ; par_completeLine = complete
    ; par_states       = states
    ; par_paragraph    = (-1) }
  in up (newChildAfter str (Paragraph para))

(** Adds a new node, just below the last one. *)
let newStruct str ?(in_toc=true) ?label ?(numbered=true) ?(extra_tags=[]) displayname =
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
      node_tags= extra_tags @ (if in_toc then ["intoc",""] else []) @ ["structural",""] @ (if numbered then ["numbered",""] else []);
      node_env=(
        fun env->
        { env with
          last_changed_counter="_structure";
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
  in newChildAfter str para


(** {3 References, labels and links} *)

let pageref x=
  [C (fun env->
    try
      env_accessed:=true;
      let (_,_,node)=StrMap.find x (names env) in
      [bB (fun _->[Marker (BeginLink (Intern x))]);
       tT (string_of_int (1+layout_page node));
       bB (fun _->[Marker EndLink])]
    with Not_found -> []
  )]

let make_name name=
  let realName=UTF8.Buf.create (String.length name) in
  let rec fill i sp=
    if UTF8.out_of_range name i then
      UTF8.Buf.contents realName
    else (
      if UChar.is_space (UTF8.look name i) then
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


let label ?labelType name=
  let name=make_name name in
  [Env (fun env->
        let w=try let (_,_,w)=StrMap.find name (names env) in w with Not_found -> uselessLine in
        let labelType=match labelType with None->env.last_changed_counter | Some t->t in
        { env with names=StrMap.add name (env.counters, labelType, w) (names env) });
   bB (fun _ -> [Marker (Label name)])
  ]

let pass_number = ref (-1)

let lref ?refType name=
  let name=make_name name in
  [ C (fun env->
    try
      env_accessed:=true;
      let counters,refType_=
        if name="_here" then env.counters,env.last_changed_counter else
          let a,t,_=StrMap.find name (names env) in a,t
      in
      let refType=match refType with Some x->x | None->refType_ in
      let lvl,num_=StrMap.find refType counters in
      let num=if refType="_structure" then List.drop 1 num_ else num_ in
      let str_counter=
        try
          let _,str_counter=StrMap.find "_structure" counters in
          str_counter
        with
            Not_found->[]
      in
      let sect_num = List.drop (List.length str_counter - max 0 lvl+1) str_counter in
      [bB (fun _->[Marker (BeginLink (Intern name))]);
       tT (String.concat "." (List.map (fun x->string_of_int (x+1))
                                (List.rev (num@sect_num))));
       bB (fun _->[Marker EndLink])]
    with
      Not_found ->
        let refType=match refType with Some x->x | None-> "Default" in
        if !pass_number <> 0 then Printf.eprintf "Unknown label %S of labelType %S (%d)\n%!" name refType !pass_number;
        color Color.red [tT "??"]
  )]

let generalRef t x = lref ~refType:t x
let sectref x=lref ~refType:"_structure" x

let extLink a b=bB (fun _->[Marker (BeginLink (Extern a))])::b@[bB (fun _->[Marker EndLink])]
let link a b=bB (fun _->[Marker (BeginLink (Intern a))])::b@[bB (fun _->[Marker EndLink])]
let button_name =
  let c = ref 0 in
  fun () -> let x = !c in c := x+1; "button_" ^ string_of_int x
let button =
  fun btype b ->
    bB (fun _->[Marker (BeginLink (Button(btype, button_name ())))])::
      b @ bB (fun _->[Marker EndLink]) :: []

(** {3 Images} *)

let image ?scale:(scale=0.) ?width:(width=0.) ?height:(height=0.) ?offset:(offset=0.) imageFile _ =
  let i=RawContent.image imageFile in
  let dr={
    drawing_min_width=i.image_width;
    drawing_max_width=i.image_width;
    drawing_nominal_width=i.image_width;
    drawing_width_fixed = true;
    drawing_adjust_before = false;
    drawing_y0=(-.offset);
    drawing_y1=(-.offset) -. i.image_height;
    drawing_break_badness=0.;
    drawing_states=[];
    drawing_badness=(fun _->0.);
    drawing_contents=(fun _->[RawContent.translate 0. (-.offset) (Image i)])
  }
  in
  let scale =
    if scale >0. then scale
    else if width > 0. then width /. i.image_width
    else if height > 0. then height /. i.image_height
    else 0.
  in
  if scale>0. then resize_drawing scale dr
  else dr

let video ?scale:(scale=0.) ?width:(width=0.) ?height:(height=0.) ?offset:(offset=0.) imageFile env=
  let tmp=(try Filename.chop_extension imageFile with _->imageFile) in
  if not (Sys.file_exists (tmp^"-1.png")) ||
    (Unix.stat (tmp^"-1.png")).Unix.st_mtime
    < (Unix.stat imageFile).Unix.st_mtime then (
      let _=Sys.command (Printf.sprintf "ffmpeg -i %s -t 1 -r 1 %s-%%d.png" imageFile tmp) in
      ()
    );
  let w,h = ImageLib_unix.size (tmp^"-1.png") in
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
  let i={video_file=imageFile;
         video_width=fw;
         video_height=fh;
         video_pixel_width=w;
         video_pixel_height=h;
         video_x=0.;
         video_y=offset;
         video_order=0
        }
  in
  {
    drawing_min_width=fw;
    drawing_max_width=fw;
    drawing_nominal_width=fw;
    drawing_width_fixed = true;
    drawing_adjust_before = false;
    drawing_y0=offset;
    drawing_y1=fh+.offset;
    drawing_break_badness=0.;
    drawing_states=[];
    drawing_badness=(fun _->0.);
    drawing_contents=(fun _->[RawContent.Video i])
  }

let includeGraphics ?scale:(scale=0.) ?width:(width=0.) ?height:(height=0.) ?offset:(offset=0.) imageFile=
  [bB (fun env->[Drawing (image ~scale ~width ~height ~offset imageFile env)])]

let includeVideo ?scale:(scale=0.) ?width:(width=0.) ?height:(height=0.) ?offset:(offset=0.) imageFile=
  [bB (fun env->[Drawing (video ~scale ~width ~height ~offset imageFile env)])]

(** {3 Boxification}*)


(**/**)
let rStdGlue:(float*box) ref=ref (0.,glue 0. 0. 0.)
(**/**)

(* let ambientBuf=ref ([||],0) *)
(** Makes a glue from the unicode character code given in the argument. *)
let makeGlue env x0=
  let stdGlue=

    if fst !rStdGlue <> env.size then
      begin
        let (mi,no,ma) = env.stdGlue in
        rStdGlue:=(env.size,
                   glue (mi*.env.size) (no*.env.size) (ma*.env.size))
      end;
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
let gl_of_str env str =
  try
    let res = hyphenate env.hyphenate env.substitutions env.positioning env.font
                        env.size env.fontColor str
    in
    res
  with Glyph_not_found _ ->
    Printf.eprintf "glyph not found in: %s (%S)\n%!" str str;
    []

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


(* let nfkc = UNF8.nfkc *)
let nfkc x = x
(**/**)


(** Converts a list of contents into a list of boxes, which is the next Patoline layer. *)
let boxify buf nbuf env0 l=
  let rec boxify keep_cache env = function
    | []->env
    | B (b, cache) :: s ->
        let l =
          match !cache with
          | Some l when keep_cache -> l
          | _                      ->
              let acc = !env_accessed in
              env_accessed := false;
              let l = b env in
              if keep_cache then
                (if not !env_accessed then cache := Some l
                 else env0.fixable := true);
              env_accessed := acc || !env_accessed; l
        in
        List.iter (append buf nbuf) l;
        boxify keep_cache env s
    | (C b)::s->(
        let acc= !env_accessed in
        env_accessed:=false;
        let c = b env in
        let env'=if !env_accessed then (
          env0.fixable:=true;
          boxify false env c
        ) else boxify keep_cache env c
        in
        env_accessed:=acc || !env_accessed;
        boxify keep_cache env' s
      )
    | Env f::s->boxify keep_cache (f env) s
    (* The following (commented) case is a mistake and causes non-transparent behavior, for instance when defining "tT"s in a global variable. *) (*
    | T (t,cache)::T (t',_)::s->
       boxify keep_cache env (T (t^t',match !cache with Some _->cache | _->cache)::s)
    *)
    | T (t,cache) :: s -> (
        match !cache with
        | Some l when keep_cache ->
           IntMap.iter (fun _->List.iter (append buf nbuf)) l;
           boxify keep_cache env s
        | _                      ->
           let l = ref IntMap.empty in
           let t = nfkc t in
           let rec cut_str i0 i =
             if i >= String.length t then
               let sub = String.sub t i0 (i-i0) in
               l := mappend !l (gl_of_str env sub)
             else if UChar.is_space (UTF8.look t i) then
               let sub = String.sub t i0 (i-i0) in
               l := mappend !l (gl_of_str env (nfkc sub));
               if i <> i0 || i = 0 then
                 l:=mappend !l [makeGlue env (UChar.code (UTF8.look t i))];
               cut_str (UTF8.next t i) (UTF8.next t i)
             else
               cut_str i0 (UTF8.next t i)
           in cut_str 0 0;
           if keep_cache then cache := Some !l;
           IntMap.iter (fun _->List.iter (append buf nbuf)) !l;
           boxify keep_cache env s)

    | Scoped (fenv, p)::s->
        let env'=fenv env in
        let _=boxify keep_cache env' p in
        boxify keep_cache env s
    | N _ :: _->
      failwith "boxify: wrong argument (N)";
      (*boxify keep_cache env s*)
  in
  boxify true env0 l


(** Typesets boxes on a single line, then converts them to a list of basic
    drawing elements: [RawContent.raw]. *)
let draw_boxes env l=
  let rec draw_boxes x y dr l=match l with
      []->dr,x
    | Kerning kbox::s ->(
      let dr',x'=draw_boxes (x+.kbox.kern_x0) (y+.kbox.kern_y0) dr [kbox.kern_contents] in
      draw_boxes (x'+.kbox.advance_width) y dr' s
    )
    | Hyphen h::s->(
      let dr1,w1=Array.fold_left (fun (dr',x') box->
        draw_boxes x' y dr' [box]
      ) (dr,x) h.hyphen_normal
      in
      draw_boxes w1 y dr1 s
    )
    | GlyphBox a::s->(
      let box=RawContent.Glyph { a with glyph_x=a.glyph_x+.x;glyph_y=a.glyph_y+.y } in
      let w=a.glyph_size*.Fonts.glyphWidth a.glyph/.1000. in
      draw_boxes (x+.w) y (box::dr) s
    )
    | Glue g::s
    | Drawing g ::s->(
      let w=g.drawing_nominal_width in
      let box=(List.map (RawContent.translate (x) (y)) (g.drawing_contents w)) in
      draw_boxes (x+.w) y (box@dr) s
    )
    | Marker (BeginLink l)::s->(
      (*      Printf.fprintf stderr "****BeginURILink %S****\n" l;*)
      let k = match l with
      Box.Extern l -> RawContent.Extern l;
    | Box.Intern l ->
        let dest_page=
            try
              let line=MarkerMap.find (Label l) env.user_positions in
              layout_page line
            with
              Not_found->(-1)
      in
      RawContent.Intern(l,dest_page,0.,0.);
    | Box.Button(t,n) -> RawContent.Button(t,n)
      in
      let link={ link_x0=x;link_y0=y;link_x1=x;link_y1=y;link_kind=k;
                 link_order=0;
                 link_closed=false;
                 link_contents=[] }
      in
      draw_boxes x y (Link link::dr) s
    )
    | Marker EndLink::s->(
      (* Printf.fprintf stderr "****EndLink****\n"; *)
      let rec link_contents u l =
        match l with
        | []                                 -> assert false
        | (Link h)::_ when not h.link_closed ->
            let u = List.rev u in
            h.link_contents<-u;
            let (_,y0,_,y1)=bounding_box u in
            h.link_y0<-y0;
            h.link_y1<-y1;
            h.link_closed<-true;
            h.link_x1<-x;
            l
        | h::s->link_contents (h::u) s
      in
      let dr'=link_contents [] dr in
      (* List.iter (print_raw) dr'; *)
      (* Printf.fprintf stderr "***************\n";flush stderr; *)
      draw_boxes x y dr' s
    )

    | b::s->
      let _,w,_=box_interval b in
      draw_boxes (x+.w) y dr s
  in
  let dr,_ = draw_boxes 0. 0. [] l in dr

let rec bezier_of_boxes=function
    []->[]
  | Glyph g::s->
      let out=Fonts.outlines g.glyph in
        (List.map (fun (x,y)->Array.map (fun xx->g.glyph_x+.xx *. g.glyph_size/.1000.) x,
                     Array.map (fun xx->g.glyph_y+.xx *. g.glyph_size/.1000.) y)
           (List.concat out)) @ (bezier_of_boxes s)
  | Path (param,p)::s->
     let l = List.concat (List.map Array.to_list p) in
     if param.strokingColor <> None then (
       let lw = param.lineWidth /. 2.0 in
       let l1 = List.map (fun (xa, ya) -> Array.map (fun x -> x +. lw) xa, ya) l in
       let l2 = List.map (fun (xa, ya) -> Array.map (fun x -> x -. lw) xa, ya) l in
       let l3 = List.map (fun (xa, ya) -> xa, Array.map (fun x -> x +. lw) ya) l in
       let l4 = List.map (fun (xa, ya) -> xa, Array.map (fun x -> x -. lw) ya) l in
       l1@l2@l3@l4@(bezier_of_boxes s))
     else
       l@(bezier_of_boxes s)
  | Dynamic(d)::s ->
     (bezier_of_boxes (d.dyn_contents ()))@(bezier_of_boxes s)
  | Link(l)::s ->
     (bezier_of_boxes l.link_contents)@(bezier_of_boxes s)
  | _::s->
     bezier_of_boxes s (* TODO more cases ?, Affine and States ? *)

let adjust_width env buf nbuf =
  (* FIXME : à prendre dans l'env *)
  let alpha = env.adjust_optical_alpha in
  let beta = env.adjust_optical_beta in
  let char_space = env.normalLead *. env.adjust_min_space in
  let epsilon = env.adjust_epsilon in
  let dir = (-.cos(alpha), sin(alpha)), (-.cos(alpha), -.sin(alpha)) in
  let dir' = (cos(alpha), -.sin(alpha)), (cos(alpha), sin(alpha)) in
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
        let (x0_l,_,x1_l,_) = bounding_box_kerning left in

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

              let (x0_r,_,x1_r,_) = bounding_box_kerning right in
              let (x0_r',_,_,_) = bounding_box_full right in


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

(*              let r = r -. x0_r' +. x0_r -. x1_l +. x1_l' in*)

              if !Distance.debug then Printf.fprintf stderr "end Adjust: r = %f nominal = %f" r !nominal;

              buf.(i) <-
                (match b with
                | Drawing x when before -> Drawing { x with
                  drawing_contents =
                      (fun w -> List.map (RawContent.translate (r +. x0_r' -. x0_r) 0.0) (x.drawing_contents w))
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
  let _=boxify buf nbuf env x in
  adjust_width env buf nbuf;
  Array.to_list (Array.sub !buf 0 !nbuf)

(** Composes [boxify] and [draw_boxes] *)
let draw env x=
  let buf=ref [||] in
  let nbuf=ref 0 in
  let env'=boxify buf nbuf env x in
  adjust_width env buf nbuf;
  draw_boxes env' (Array.to_list (Array.sub !buf 0 !nbuf))

let states st x=
  [uB (fun env->
    let d=draw env x in
    let (_,off,_,_)=bounding_box_kerning d in
    [Drawing
        (drawing ~offset:off
            [States { states_contents=d;
                      states_states=st;
                      states_order=0 }]
        )]
  )]

let altStates l =
  [uB (fun env->
    let ds = List.map (fun (st,x) -> (st, draw env x)) l in
    (* FIXME : each state should have its own offset !!!*)
    let off = List.fold_left (fun acc (_,d) ->
      let (_,off,_,_) = bounding_box_kerning d in
      min acc off) 0.0 ds
    in
    [Drawing
        (drawing ~offset:off
            (List.map (fun (st, d) ->
              States { states_contents=d;
                      states_states=st;
                      states_order=0 }) ds
            ))]
  )]





(** "flattens" a document tree to an array of paragraphs, a paragraph
    being an array of boxes. *)
let flatten ?(initial_path=[]) env0 str=
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
  let states=ref [] in
  let n=ref 0 in

  let buf=ref [||] in
  let nbuf=ref 0 in
  let frees=ref 0 in
  let add_paragraph env tree path p=
    let cont = bB (fun env->(p.par_env env).par_indent) :: p.par_contents in
    nbuf:= !frees;
    let env=boxify buf nbuf env cont in
    adjust_width env buf nbuf;
    paragraphs:=(Array.sub !buf 0 !nbuf)::(!paragraphs);
    trees:=(tree,path)::(!trees);
    compl:=(p.par_completeLine env)::(!compl);
    param:=(p.par_parameters env)::(!param);
    new_page_list:=(env.new_page)::(!new_page_list);
    new_line_list:=(env.new_line env)::(!new_line_list);
    bads:=(p.par_badness env)::(!bads);
    states:=(p.par_states)::(!states);
    incr n;
    frees:=0;
    env
  in

  let rec flatten flushes env0 path tree=
    match tree with
    | Paragraph p -> (
      let env1 = p.par_env env0 in
      let add_node env cur =
          add_paragraph env tree path
                        { p with par_paragraph = List.length !paragraphs;
                                 par_contents=List.rev cur }
      in
      let rec collect_nodes env1 l cur =
        match l with
        | []-> (env1, cur)
        | C(f)::s-> collect_nodes env1 (f env1@s) cur
        | Scoped(f,s')::s->
           let env2 = f env1 in
           let (_, res) = collect_nodes env2 s' [] in
           collect_nodes env1 s (Scoped((fun _ -> env2),List.rev res)::cur)
        | Env f::s ->
           let env1 = f env1 in
           collect_nodes env1 s (Env (fun _ -> env1)::cur)
        | N n::s->
           let env1 = add_node env1 cur in
           let env1 = flatten flushes env1 path n in
           collect_nodes env1 s []
        | (T _ | B _ as h)::s-> collect_nodes env1 s (h::cur)
      in
      let (env1, cur) = collect_nodes env1 p.par_contents [] in
      let env1 = add_node env1 cur in
      p.par_post_env env0 env1
    )
    | FigureDef f -> (
      let env1=f.fig_env env0 in
      let n=IntMap.cardinal !figures in
      fig_param:=IntMap.add n (f.fig_parameters env1) !fig_param;
      figures:=IntMap.add n (f.fig_contents env1) !figures;
      figure_trees:=IntMap.add n (tree,path) !figure_trees;
      append buf frees (BeginFigure n);
      f.fig_post_env env0 env1
    )
    | Node s-> (
      let env1 = s.node_env env0 in
      let env1=
        let level=
          try
            List.length (snd (StrMap.find "_structure" env1.counters))
          with Not_found->0
        in
        { env1 with counters=StrMap.map (fun (lvl,l)->if lvl>level then lvl,[] else lvl,l)
                                        env1.counters }
      in
      s.node_paragraph <- List.length !paragraphs;
      s.boxified_displayname <- draw_boxes env1 (boxify_scoped env1 s.displayname);
      let flushes'=ref [] in
      let flat_children k a (is_first, env1)=match a with
          Paragraph p->(
          let env2=flatten flushes' env1 ((k,tree)::path)
                (Paragraph { p with par_contents=
                    (if is_first then (
                      (* Set up a marker to be able to obtain section page.
                         It is added to the MarkerMap in Break. *)
                      let name=String.concat "_" ("_"::List.map string_of_int ((List.map fst path)@initial_path)) in
                      [Env (fun env->
                        let w=try let (_,_,w)=StrMap.find name (names env) in w with
                            Not_found -> uselessLine in
                        { env with names=StrMap.add name (env.counters, "_", w)
                            (names env) });
                       bB (fun _->[Marker (Label name)])
                      ]
                     ) else [])@ p.par_contents
                           }
                ) in
          false, env2
        )
        | FigureDef _ as h->(
          let env2=flatten flushes' env1 ((k,tree)::path) h in
          let num=try
              match StrMap.find "_figure" env2.counters with
                _,h::_->h
              | _->0
            with
              Not_found ->0
          in
          flushes':=FlushFigure num::(!flushes');
          is_first,env2
        )
        | Node _ as tr->(
          (is_first, flatten flushes' env1 ((k,tree)::path) tr)
        )
        in
        let _,env2=IntMap.fold flat_children s.children (true,env1) in
        paragraphs:=(match !paragraphs with
            []->[]
          | h::s->Array.append h (Array.of_list !flushes')::s);
        s.node_post_env env0 env2
      )
  in
  let env1=flatten (ref []) env0 [] str in
  let params=Array.init
    (IntMap.cardinal !figures)
    (fun i->IntMap.find i !fig_param)
  in
  (env1, params,
   Array.of_list (match List.rev !param with []->[parameters env1] | l->l),
   Array.of_list (match List.rev !new_page_list with []->[env1.new_page] | l->l),
   Array.of_list (match List.rev !new_line_list with []->[env1.new_line env1] | l->l),
   Array.of_list (List.rev !compl),
   Array.of_list (List.rev !bads),
   Array.of_list (List.rev !paragraphs),
   Array.of_list (List.rev !trees),
   Array.of_list (List.map snd (IntMap.bindings !figures)),
   Array.of_list (List.map snd (IntMap.bindings !figure_trees)),
   Array.of_list (List.rev !states))

let rec make_struct positions = function
  | Node s ->
      let rec make = function
        | [] -> []
        | (_,Node u)::s when List.mem_assoc "intoc" u.node_tags ->
            (make_struct positions (Node u))::(make s)
        | _ :: s->make s
      in
      let a = Array.of_list (make (IntMap.bindings s.children)) in
      let (p,x,y) =
        let lenpos = Array.length positions in
        if s.node_paragraph >= 0 && s.node_paragraph < lenpos then
          positions.(s.node_paragraph)
        else (0,0.,0.)
      in
      { Driver.name     = s.name
      ; Driver.metadata = []
      ; Driver.raw_name = s.boxified_displayname
      ; Driver.tags     = s.node_tags
      ; Driver.page     = p
      ; Driver.struct_x = x
      ; Driver.struct_y = y
      ; Driver.children = a }
  | _ -> Driver.empty_structure

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
      StrMap.fold (fun k (a,b,c) m->
        try
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
          if not (lines_eq pos c) && b<>"_" then (
            (* Printf.fprintf stderr "reboot : position of %S (%S) changed\n" k b; *)
            (* print_line pos; *)
            (* print_line c; *)
          );
          needs_reboot:= !needs_reboot || (not (lines_eq pos c));
          StrMap.add k (a,b,pos) m
        with Not_found -> ((* Printf.fprintf stderr "reboot : position of %S (%S) not found\n" k b; *)
                           needs_reboot:=true; m)
      ) (names env) (names env)
           }
  in
  flush stderr;
  env',!needs_reboot

(** Resets all the counters, preserving their levels. *)
let reset_counters env=
  { env with
    counters=StrMap.map (fun (l,_)->(l,[])) env.counters }
