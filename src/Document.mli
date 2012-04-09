val spec : (string * Arg.spec * string) list
type fontAlternative = Regular | Bold | Caps | Demi
type fontFamily =
    (fontAlternative * (Fonts.font Lazy.t * Fonts.font Lazy.t)) list
val lmroman :
  (fontAlternative * (Fonts.font Lazy.t * Fonts.font Lazy.t)) list
val lmmono : (fontAlternative * (Fonts.font Lazy.t * Fonts.font Lazy.t)) list
val alegreya :
  (fontAlternative * (Fonts.font Lazy.t * Fonts.font Lazy.t)) list
val selectFont : ('a * ('b Lazy.t * 'b Lazy.t)) list -> 'a -> bool -> 'b
type user =
    Label of string
  | FigureRef of int
  | Pageref of string
  | Structure of int list
  | Figure of int
  | BeginFigure of int
  | FlushFigure of int
  | Footnote of int * Util.drawingBox
module TS :
  sig
    module User :
      sig
        type t = user
        val compare : t -> t -> int
        val figureRef : int -> t
        val figure : int -> t
        val beginFigure : t -> int
        val flushedFigure : t -> int
        val isFigure : t -> bool
        val figureNumber : t -> int
      end
    module UMap :
      sig
        type key = User.t
        type 'a t = 'a New_map.Make(User).t
        val empty : 'a t
        val is_empty : 'a t -> bool
        val mem : key -> 'a t -> bool
        val add : key -> 'a -> 'a t -> 'a t
        val singleton : key -> 'a -> 'a t
        val remove : key -> 'a t -> 'a t
        val merge :
          (key -> 'a option -> 'b option -> 'c option) ->
          'a t -> 'b t -> 'c t
        val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
        val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
        val for_all : (key -> 'a -> bool) -> 'a t -> bool
        val exists : (key -> 'a -> bool) -> 'a t -> bool
        val filter : (key -> 'a -> bool) -> 'a t -> 'a t
        val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
        val cardinal : 'a t -> int
        val bindings : 'a t -> (key * 'a) list
        val min_binding : 'a t -> key * 'a
        val max_binding : 'a t -> key * 'a
        val choose : 'a t -> key * 'a
        val split : key -> 'a t -> 'a t * 'a option * 'a t
        val find : key -> 'a t -> 'a
        val map : ('a -> 'b) -> 'a t -> 'b t
        val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
      end
    val typeset :
      completeLine:(UMap.key Util.box array array ->
                    Util.drawingBox array ->
                    Util.line UMap.t -> Util.line -> bool -> Util.line list)
                   array ->
      figures:Util.drawingBox array ->
      figure_parameters:(UMap.key Util.box array array ->
                         Util.drawingBox array ->
                         Util.parameters ->
                         Util.line UMap.t -> Util.line -> Util.parameters)
                        array ->
      parameters:(UMap.key Util.box array array ->
                  Util.drawingBox array ->
                  Util.parameters ->
                  Util.line UMap.t -> Util.line -> Util.parameters)
                 array ->
      badness:(Util.line ->
               UMap.key Util.box array ->
               int ->
               Util.parameters ->
               float ->
               Util.line ->
               UMap.key Util.box array ->
               int -> Util.parameters -> float -> float) ->
      UMap.key Util.box array array ->
      Log.error_log list * (Util.parameters * Util.line) list array *
      Util.line UMap.t
  end
module C :
  sig
    type 'a completion =
        float ->
        'a Util.box array array ->
        Util.drawingBox array ->
        Util.line TS.UMap.t -> Util.line -> bool -> Util.line list
    val normal :
      float ->
      'a Util.box array array ->
      Util.drawingBox array ->
      Util.line TS.UMap.t -> Util.line -> bool -> Util.line list
  end
type 'a node = {
  name : string;
  displayname : 'a content list;
  in_toc : bool;
  children : 'a tree Binary.IntMap.t;
  node_env : 'a environment -> 'a environment;
  node_post_env : 'a environment -> 'a environment -> 'a environment;
  mutable tree_paragraph : int;
}
and 'a paragraph = {
  par_contents : 'a content list;
  par_env : 'a environment -> 'a environment;
  par_post_env : 'a environment -> 'a environment -> 'a environment;
  par_parameters :
    'a environment ->
    'a Util.box array array ->
    Util.drawingBox array ->
    Util.parameters -> Util.line TS.UMap.t -> Util.line -> Util.parameters;
  par_completeLine :
    float ->
    'a Util.box array array ->
    Util.drawingBox array ->
    Util.line TS.UMap.t -> Util.line -> bool -> Util.line list;
}
and 'a figuredef = {
  fig_contents : 'a environment -> Util.drawingBox;
  fig_env : 'a environment -> 'a environment;
  fig_post_env : 'a environment -> 'a environment -> 'a environment;
  fig_parameters :
    'a environment ->
    'a Util.box array array ->
    Util.drawingBox array ->
    Util.parameters -> Util.line TS.UMap.t -> Util.line -> Util.parameters;
}
and 'a tree =
    Node of 'a node
  | Paragraph of 'a paragraph
  | FigureDef of 'a figuredef
and 'a environment = {
  fontFamily : fontFamily;
  fontItalic : bool;
  fontAlternative : fontAlternative;
  fontFeatures : string list;
  fontColor : OutputCommon.color;
  font : Fonts.font;
  size : float;
  footnote_y : float;
  normalMeasure : float;
  normalLead : float;
  normalLeftMargin : float;
  par_indent : 'a Util.box list;
  stdGlue : 'a Util.box list;
  hyphenate : string -> (string * string) array;
  substitutions : Fonts.FTypes.glyph_id list -> Fonts.FTypes.glyph_id list;
  positioning : Fonts.FTypes.glyph_ids list -> Fonts.FTypes.glyph_ids list;
  counters : (int * int list) Binary.StrMap.t;
  names :
    ((int * int list) Binary.StrMap.t * string * Util.line) Binary.StrMap.t;
  user_positions : Util.line TS.UMap.t;
  mutable fixable : bool;
}
and 'a content =
    B of ('a environment -> 'a Util.box list)
  | BFix of ('a environment -> 'a Util.box list)
  | C of ('a environment -> 'a content list)
  | CFix of ('a environment -> 'a content list)
  | T of string
  | FileRef of (string * int * int)
  | Env of ('a environment -> 'a environment)
  | Scoped of ('a environment -> 'a environment) * 'a content list
module type DocumentStructure =
  sig val structure : (user tree * (int * user tree) list) list ref end
val defaultFam :
  (fontAlternative * (Fonts.font Lazy.t * Fonts.font Lazy.t)) list
val defaultMono :
  (fontAlternative * (Fonts.font Lazy.t * Fonts.font Lazy.t)) list
val defaultEnv : user environment
val incr_counter :
  ?level:int -> 'a environment -> string -> 'a environment
val pop_counter : 'a environment -> string -> 'a environment
val push_counter : 'a environment -> string -> 'a environment
val empty : user node
type 'a cxt = (int * 'a tree) list
val next_key : 'a Binary.IntMap.t -> int
val child :
  user tree * (int * user tree) list ->
  int -> user tree * (int * user tree) list
val newChild :
  user tree * (int * user tree) list -> 'a -> 'a * (int * user tree) list
val up :
  user tree * (int * user tree) list ->
  user tree * (int * user tree) list
val go_up :
  (user tree * (int * user tree) list) list ref -> unit
val top :
  user tree * (int * user tree) list ->
  user tree * (int * user tree) list
val follow :
  user tree * (int * user tree) list ->
  (int * 'a) list ->
  user tree * (int * user tree) list
val fixable : bool ref
val doc_graph : out_channel -> 'a tree -> unit
val change_env :
  'a tree * 'b -> ('a environment -> 'a environment) -> 'a tree * 'b
val updateFont : 'a environment -> Fonts.font -> 'a environment
val font : string -> 'a content list -> 'a content list
val envItalic : bool -> 'a environment -> 'a environment
val italic : 'a content list -> 'a content list
module Italic :
  sig
    val do_begin_Italic : unit -> unit
    val do_end_Italic : unit -> unit
    val defaultEnv : user environment
  end
module Env_Italic :
  sig
    val do_begin_Italic : unit -> unit
    val do_end_Italic : unit -> unit
    val defaultEnv : user environment
  end
val notItalic : 'a content list -> 'a content list
val toggleItalic : 'a content list -> 'a content list
val envAlternative :
  'a -> fontAlternative -> 'b environment -> 'b environment
val alternative :
  ?features:string list ->
  fontAlternative -> 'a content list -> 'a content list
val envFamily : fontFamily -> 'a environment -> 'a environment
val family : fontFamily -> 'a content list -> 'a content list
val size : float -> 'a content list -> 'a content
val features : string list -> 'a content list -> 'a content
val parameters :
  'a environment ->
  user Util.box array array ->
  Util.drawingBox array ->
  Util.parameters -> Util.line TS.UMap.t -> Util.line -> Util.parameters
val center :
  'a environment ->
  user Util.box array array ->
  Util.drawingBox array ->
  Util.parameters -> Util.line TS.UMap.t -> Util.line -> Util.parameters
val ragged_left :
  'a environment ->
  user Util.box array array ->
  Util.drawingBox array ->
  Util.parameters -> Util.line TS.UMap.t -> Util.line -> Util.parameters
val ragged_right :
  'a environment ->
  user Util.box array array ->
  Util.drawingBox array ->
  Util.parameters -> Util.line TS.UMap.t -> Util.line -> Util.parameters
val in_text_figure :
  'a environment ->
  user Util.box array array ->
  Util.drawingBox array ->
  Util.parameters -> Util.line TS.UMap.t -> Util.line -> Util.parameters
val figure :
  (user tree * (int * user tree) list) list ref ->
  ?parameters:(user environment ->
               user Util.box array array ->
               Util.drawingBox array ->
               Util.parameters ->
               Util.line TS.UMap.t -> Util.line -> Util.parameters) ->
  ?name:string -> (user environment -> Util.drawingBox) -> unit
val flush_figure : 'a -> user content list
val begin_figure : 'a -> user content list
val newPar :
  (user tree * (int * user tree) list) list ref ->
  ?environment:(user environment -> user environment) ->
  (float ->
   user Util.box array array ->
   Util.drawingBox array ->
   Util.line TS.UMap.t -> Util.line -> bool -> Util.line list) ->
  (user environment ->
   user Util.box array array ->
   Util.drawingBox array ->
   Util.parameters -> Util.line TS.UMap.t -> Util.line -> Util.parameters) ->
  user content list -> unit
val string_of_contents : 'a content list -> string
val newStruct :
  (user tree * (int * user tree) list) list ref ->
  ?label:string -> user content list -> unit
val newStruct' :
  (user tree * (int * user tree) list) list ref ->
  ?label:string -> user content list -> unit
val title :
  (user tree * 'a list) list ref ->
  ?label:string -> 'b -> user content list -> unit
val is_space : char -> bool
val sources : in_channel Binary.StrMap.t ref
val boxify :
  'a environment -> 'a content list -> 'a Util.box list * 'a environment
val boxify_scoped : 'a environment -> 'a content list -> 'a Util.box list
val flatten :
  user environment ->
  user tree ->
  user environment *
  (user Util.box array array ->
   Util.drawingBox array ->
   Util.parameters -> Util.line TS.UMap.t -> Util.line -> Util.parameters)
  array *
  (user Util.box array array ->
   Util.drawingBox array ->
   Util.parameters -> Util.line TS.UMap.t -> Util.line -> Util.parameters)
  array *
  (user Util.box array array ->
   Util.drawingBox array ->
   Util.line TS.UMap.t -> Util.line -> bool -> Util.line list)
  array * user Util.box array array * Util.drawingBox array
val make_struct :
  (int * float * float) array -> 'a tree -> OutputCommon.structure
val table_of_contents :
  (user tree * (int * user tree) list) list ref ->
  'a -> int -> unit
val update_names :
  'a environment -> Util.line TS.UMap.t -> 'a environment * bool
val pageref : string -> 'a content list
val label : ?labelType:string -> string -> user content list
val sectref : string -> 'a content list
val generalRef :
  ?refType:string -> string -> 'a content list
