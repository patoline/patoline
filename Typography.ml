open Util
open Binary
open Constants
open Fonts
open Fonts.FTypes


let figures:drawingBox list ref=ref []

type environment={
  font:font;
  size:float;
  stdGlue:box;
  hyphenate:string->(string*string) array;
  substitutions:glyph_id list -> glyph_id list;
  positioning:glyph_ids list -> glyph_ids list;
}

type content=
    B of box
  | T of string
  | Scoped of (environment->environment)*(content list)

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

let size fsize t=
  Scoped ((fun env -> { env with
                          size=fsize;
                          stdGlue=Glue { glue_min_width= 2.*. fsize/.9.;
                                         glue_max_width= fsize/.2.;
                                         glue_nominal_width= fsize/.3.;
                                         glue_badness=knuth_h_badness (fsize/.3.) }
                      }), t)

type tree={
  name:string;
  content:content list list;
  children:tree IntMap.t;
  mutable tree_paragraph:int;
}

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

let empty={ name=""; content=[]; children=IntMap.empty; tree_paragraph= (-1) }

let cur=ref []
let str=ref empty

let next_key t=
  try
    fst (IntMap.max_binding t)+1
  with
      _ -> 0


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

let up ()=
  let rec up=function
      []->[]
    | [h]->[]
    | h::s-> h::up s
  in
    cur:=up !cur

let add_features f t=
  Scoped ((fun env->
             { env with substitutions=
                 (fun glyphs -> List.fold_left apply glyphs (
                    Fonts.select_features env.font f
                  ))
             }),
          t)

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


let defaultEnv=
  let f=loadFont "AGaramondPro-Regular.otf" in
  let fsize=5. in
    {
      font=f;
      size=5.;
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


let exceptions=ref IntMap.empty

let lineBreak p n=
  let f=
    try
      let f0=IntMap.find p !exceptions in
        (fun x line a b c -> if line.lineStart=0 then { (f0 x line a b c) with min_height_diff=(n+1) } else x)
    with
        _-> (fun x line a b c -> if line.lineStart=0 then { x with min_height_diff=(n+1) } else x)

  in
    exceptions:= IntMap.add p f !exceptions

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
  let p={ format=a4; lead=6.;
          measure= measure line;
          lines_by_page=
      if line.page_height <= 0 then 38 else last_parameters.lines_by_page;
          left_margin=
      (if line.isFigure then (
         20.+.(measure line -. (figures.(line.lastFigure).drawing_max_width +. figures.(line.lastFigure).drawing_min_width)/.2.)/.2.
       ) else 20.);
          local_optimization=0;
          min_page_diff=0;
          min_height_diff=1;
          allow_widows=false;
          allow_orphans=false
        }
  in
    try (IntMap.find line.paragraph !exceptions) p line m0 m1 m2 with Not_found -> p


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
      lineBreak !n 1;
      lineBreak (!n+1) 1;
      add_paragraph (structNum path tree.name);
    ) else if tree.name<>"" then (
      lineBreak (!n+1) 3;
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
