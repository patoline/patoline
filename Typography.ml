open Util
open Binary
open Constants
open Fonts
open Fonts.FTypes


(* Pour choisir la police, et d'autres paramètres, on a un
   environnement. On peut tout modifier de manière uniforme sur tout
   le document à partir de n'importe où (voir le type content pour les
   scopes) *)
type environment={
  mutable font:font;
  mutable size:float;
  mutable stdGlue:box;
  mutable hyphenate:string->(string*string) array;
  mutable substitutions:glyph_id list -> glyph_id list;
  mutable positioning:glyph_ids list -> glyph_ids list;
}
let defaultEnv=
  let f=loadFont "AGaramondPro-Regular.otf" in
  let fsize=5. in
    {
      font=f;
      size=4.;
      stdGlue=Glue { glue_min_width= 2.*. fsize/.9.;
                     glue_max_width= fsize/.2.;
                     glue_nominal_width= fsize/.3.;
                     glue_badness=knuth_h_badness (fsize/.3.) };
      hyphenate=(
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
      );
      substitutions=
        (fun glyphs -> List.fold_left apply glyphs (
           Fonts.select_features f [ StandardLigatures ]
         ));
      positioning=positioning f
    }


(* Type du contenu. B est une boîte quelconque. Les espaces dans T
   seront transformés en la boîte stdGlue de l'environnement, qui
   n'est pas nécennairement une GlueBox *)
type content=
    B of box
  | T of string
  | Scoped of (environment->environment)*(content list)



(****************************************************************)



(* Le jeu est de construire la structure de document suivante :
   C'est un arbre, avec du contenu texte à chaque nœud. *)

type tree={
  name:string;
  content:content list list;
  children:tree IntMap.t;
  mutable tree_paragraph:int;
}
let empty={ name=""; content=[]; children=IntMap.empty; tree_paragraph= (-1) }

(* La structure actuelle *)
let str=ref empty
(* Le chemin vers le noeud courant *)
let cur=ref []

(* Liste des figures. La définition des drawingBox est dans Util.ml,
   c'est essentiellement le type du driver avec un cadre autour *)
let figures:drawingBox list ref=ref []

(* Sortie en dot de la structure du document *)
let doc_graph out t0=
  Printf.fprintf out "digraph {\n";
  let rec do_it path t=
    Printf.fprintf out "%s [label=\"%s\"];\n" path t.name;
    List.iter (fun (i,x)->
                 let p=path^"_"^(string_of_int i) in
                   Printf.fprintf out "%s -> %s;\n" path p;
                   do_it p x) (IntMap.bindings t.children)
  in
    do_it "x0" t0;
    Printf.fprintf out "}\n"


let next_key t=try fst (IntMap.max_binding t)+1 with Not_found -> 0

(* Exemple de manipulation de la structure : faire un nouveau paragraphe *)
let newPar par=
  let rec newPar tree path=
    match path with
        []->{ tree with content=par::tree.content }
      | h::t->(
          let t'=try IntMap.find h tree.children with _->empty in
            { tree with children=IntMap.add h (newPar t' t) tree.children }
        )
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


let newStruct name=
  let rec newStruct tree path=
    match path with
        []-> (let next=next_key tree.children in
                cur:= !cur @ [next];
                { tree with children=IntMap.add next { empty with name=name } tree.children })
      | h::t-> let t'=try IntMap.find h tree.children with _->empty in
          { tree with children=IntMap.add h (newStruct t' t) tree.children }
  in
    str:=newStruct !str !cur

let title t=str:= { !str with name=t }


(****************************************************************)


(* Quelques Exemples d'environnement *)

(* Changer de font dans un scope *)
let font f t=
  let font=loadFont f in
    Scoped ((fun env->
               { env with
                   font=font;
                   substitutions=
                   (fun glyphs -> List.fold_left apply glyphs (
                      Fonts.select_features font [ StandardLigatures ]
                    ));
                   positioning=positioning font }), t)

(* Changer de taille dans un scope *)
let size fsize t=
  Scoped ((fun env -> { env with
                          size=fsize;
                          stdGlue=Glue { glue_min_width= 2.*. fsize/.9.;
                                         glue_max_width= fsize/.2.;
                                         glue_nominal_width= fsize/.3.;
                                         glue_badness=knuth_h_badness (fsize/.3.) }
                      }), t)


(* Rajouter une liste de features, voir Fonts.FTypes pour savoir ce
   qui existe *)
let add_features f t=
  Scoped ((fun env->
             { env with substitutions=
                 (fun glyphs -> List.fold_left apply glyphs (
                    Fonts.select_features env.font f
                  ))
             }),
          t)

(****************************************************************)

(* Partie compliquée : il faut comprendre ce que fait l'optimiseur
   pour toucher à ça, ou apprendre en le touchant *)

let exceptions=ref IntMap.empty

(* Garantit que la paragraphe p sera séparé du p-1 par au moins n
   lignes. Voir l'usage plus loin dans flatten *)
let lineBreak n p=
  let f=
    try
      let f0=IntMap.find p !exceptions in
        (fun x line a b c -> if line.lineStart=0 then { (f0 x line a b c) with min_height_diff=(n+1) } else x)
    with
        _-> (fun x line a b c -> if line.lineStart=0 then { x with min_height_diff=(n+1) } else x)

  in
    exceptions:= IntMap.add p f !exceptions

(* Garantit que la paragraphe p sera séparé du p-1 par au moins n pages *)
let pageBreak n p=
  let f=
    try
      let f0=IntMap.find p !exceptions in
        (fun x line a b c -> if line.lineStart=0 then { (f0 x line a b c) with min_page_diff=(n+1) } else x)
    with
        _-> (fun x line a b c -> if line.lineStart=0 then { x with min_page_diff=(n+1) } else x)

  in
    exceptions:= IntMap.add p f !exceptions

(* Centre les lignes d'un paragraphe. Il faut un optimiseur différent ici *)
let center p=
  let f=
    try
      let f0=IntMap.find p !exceptions in
        (fun x l a b c -> let f'= (f0 x l a b c) in
           if f'.measure >= b then
             { f' with measure=b; left_margin=f'.left_margin +. (f'.measure-.b)/.2. }
           else
             f')
    with
        _->(fun x _ a b c ->
              if x.measure >= b then
                { x with measure=b; left_margin=x.left_margin +. (x.measure-.b)/.2. }
              else
                x)
  in
    exceptions:= IntMap.add p f !exceptions


let measure (_:line)=150.
let parameters paragraphs figures last_parameters line (m0:float) (m1:float) (m2:float)=
  let p={ format=a4; lead=5.;
          measure= measure line;
          lines_by_page=if line.page_height <= 0 then 45 else last_parameters.lines_by_page;
          left_margin=(
            let space=(fst a4-.measure line)/.2. in
              if line.isFigure then (
                space+.(measure line -. (figures.(line.lastFigure).drawing_max_width
                                         +. figures.(line.lastFigure).drawing_min_width)/.2.)/.2.
              ) else space
          );
          local_optimization=0;
          min_page_diff=0;
          min_height_diff=1;
          allow_widows=false;
          allow_orphans=false
        }
  in
    try (IntMap.find line.paragraph !exceptions) p line m0 m1 m2 with Not_found -> p




(****************************************************************)

(* Fonctions auxiliaires qui produisent un document optimisablo à
   partir de l'arbre *)


let structNum path name=
  let n=match path with
      h::s->List.fold_left (fun x y -> x^"."^(string_of_int (y+1))) (string_of_int (h+1)) s
    | []->"0"
  in
    if List.length path <= 1 then
      [Scoped ((fun env->{ env with substitutions=(fun glyphs -> List.fold_left apply glyphs (
                                                     Fonts.select_features env.font [OldStyleFigures; SmallCapitals]
                                                   ));
                             size=sqrt (sqrt phi)*.env.size
                         }), [T (n ^ " " ^ (CM.uppercase name))])]
    else
      [add_features [OldStyleFigures] [T (n ^ " " ^ name)]]


let is_space c=c=' ' || c='\n' || c='\t'

let rec boxify env=function
    []->[]
  | (B b)::s->b::(boxify env s)
  | (T t)::s->(
      let rec cut_str i0 i result=
        if i>=String.length t then (
          if i0<>i then (
            if result<>[] then
              result @ (env.stdGlue :: (glyph_of_string env.substitutions env.positioning env.font env.size (String.sub t i0 (i-i0))))
            else
              glyph_of_string env.substitutions env.positioning env.font env.size (String.sub t i0 (i-i0))
          ) else result
        ) else (
          if is_space t.[i] then
            cut_str (i+1) (i+1) (
              if i0<>i then (
                if result<>[] then
                  result @ (env.stdGlue :: (glyph_of_string env.substitutions env.positioning env.font env.size (String.sub t i0 (i-i0))))
                else
                  glyph_of_string env.substitutions env.positioning env.font env.size (String.sub t i0 (i-i0))
              ) else result
            )
          else (
            cut_str i0 (i+1) result
          )
        )
      in
        (cut_str 0 0 []) @ (boxify env s)
    )
  | Scoped (env', p)::s->(boxify (env' env) p)@(boxify env s)

let flatten env0 str=

  let paragraphs=ref [] in
  let n=ref 0 in
  let add_paragraph p=
    incr n;
    paragraphs:=(Array.of_list (boxify env0 p))::(!paragraphs)
  in

  let rec flatten env path tree=
    tree.tree_paragraph<- !n;
    if path<>[] then (
      lineBreak 1 !n;
      lineBreak 1 (!n+1);
      add_paragraph (structNum path tree.name);
    ) else if tree.name<>"" then (
      lineBreak 3 (!n+1);
      center !n;
      add_paragraph [size 10. [T (tree.name)] ]
    );

    List.iter add_paragraph tree.content;

    IntMap.fold (fun a b _ -> flatten env (path@[a]) b) tree.children ()
  in
    flatten env0 [] str;
    Array.of_list (List.rev !paragraphs)

let rec make_struct positions tree=
  let (p,x,y)=positions.(tree.tree_paragraph) in
  let a=Array.of_list (List.map (fun (_,u)->make_struct positions u) (IntMap.bindings tree.children)) in
    { Drivers.name=tree.name;
      Drivers.page=p;
      Drivers.struct_x=x;
      Drivers.struct_y=y;
      Drivers.substructures=a }
