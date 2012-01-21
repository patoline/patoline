#define DEBUG_BOXES 1

open Drivers
open Binary
open Constants
open CamomileLibrary

type glyphBox= { contents:UTF8.t; glyph:Fonts.glyph; size: float; width:float; x0:float; x1:float; y0:float; y1:float }

type box=GlyphBox of glyphBox | Glue of (float*float*float)

let current_font=ref (Fonts.loadFont "AGaramondPro-Regular.otf")
let current_size=ref 4.

exception Impossible

let isGlue x=match x with Glue _->true | _->false

type line= { paragraph:int; lineStart:int; lineEnd:int; lastFigure:int; height:int;
             paragraph_height:int
           }

let print_line l=
#ifdef DEBUG_BOXES
  Printf.printf "{ paragraph=%d; lineStart=%d; lineEnd=%d; lastFigure=%d; height=%d }\n"
  l.paragraph l.lineStart l.lineEnd l.lastFigure l.height
#else
  ()
#endif

module LineMap=Map.Make (struct type t=line let compare=compare end)


type parameters={ format:float*float;
                  lead:float;
                  measure:float;
                  line_height:int }

let box_width comp=function
    GlyphBox y->y.width*.y.size/.1000.
  | Glue (a,_,c)->(a+.(c-.a)*.comp)

let lower_y=function
    GlyphBox y->y.y0*.y.size/.1000.
  | _->infinity

let upper_y=function
    GlyphBox y->y.y1*.y.size/.1000.
  | _-> -.infinity



let lineBreak parameters0 ?figures:(figures = [||]) lines=
  
  let get i j=lines.(i).(j) in
    
  let length_min=Array.create (Array.length lines) [||] in
  let length_nom=Array.create (Array.length lines) [||] in
  let length_max=Array.create (Array.length lines) [||] in
  let _=
    let bmin=ref 0. in
    let bnom=ref 0. in
    let bmax=ref 0. in
      for i=0 to Array.length lines-1 do
        length_min.(i)<-Array.create (Array.length (Array.get lines i)+1) 0.;
        length_nom.(i)<-Array.create (Array.length (Array.get lines i)+1) 0.;
        length_max.(i)<-Array.create (Array.length (Array.get lines i)+1) 0.;
        for j=0 to Array.length (Array.get lines i)-1 do
          length_min.(i).(j)<- !bmin;
          length_nom.(i).(j)<- !bnom;
          length_max.(i).(j)<- !bmax;
          (match get i j with
               Glue (a,b,c)->(bmin:= !bmin+.a;bnom:= !bnom+.b;bmax:= !bmax+.c)
             | GlyphBox x->(let w=x.width*.x.size/.1000. in bmin:= !bmin+.w;bnom:= !bnom+.w;bmax:= !bmax+.w));
        done;
        length_min.(i).(Array.length (Array.get lines i))<- !bmin;
        length_nom.(i).(Array.length (Array.get lines i))<- !bnom;
        length_max.(i).(Array.length (Array.get lines i))<- !bmax;
      done
  in

  let compression m p i j=
    let minLine=length_min.(p).(j) -. length_min.(p).(i) in
    let maxLine=length_max.(p).(j) -. length_max.(p).(i) in
      min 1. ((m-.minLine)/.(maxLine-. minLine))
      
  in
  let rec collide pi comp_i max_i pj comp_j max_j i xi j xj max_col=
    if (i>=max_i || j>=max_j) then
      max_col
    else
      let wi=box_width comp_i lines.(pi).(i) in
      let wj=box_width comp_j lines.(pj).(j) in
      let yi=lower_y lines.(pi).(i) in
      let yj=upper_y lines.(pj).(j) in
        if xi +.wi < xj+. wj then
          let max_col'=if not (is_infinite yj || is_infinite yi) then min max_col (yi-.yj) else max_col in
            collide pi comp_i max_i pj comp_j max_j (i+1) (xi+.wi) j xj max_col'
        else
          let max_col'=if not (is_infinite yj || is_infinite yi) then min max_col (yi-.yj) else max_col in
            collide pi comp_i max_i pj comp_j max_j i xi (j+1) (xj+.wj) max_col'
  in
  let makeLine node=
    let minLine=length_min.(node.paragraph).(node.lineEnd) -. length_min.(node.paragraph).(node.lineStart) in
    let maxLine=length_max.(node.paragraph).(node.lineEnd) -. length_max.(node.paragraph).(node.lineStart) in
    let compression=min 1. ((parameters0.measure-.minLine)/.(maxLine-. minLine)) in
    let rec makeLine i=
      if i>=node.lineEnd then [] else
        match get node.paragraph i with
            Glue (a,_,c)->(Glue (a, a+.compression*.(c-.a), c) :: makeLine (i+1))
          | x->(x::makeLine (i+1))
    in
      makeLine node.lineStart
  in


  let rec break parameters todo demerits=
   
    let badness pi i pj j=
      let length_nom_=length_nom.(pj).(j) -. length_nom.(pi).(i) in
      let bad=100.*.(if length_nom_ = parameters.measure then 0. else
                       if length_nom_ > parameters.measure then
                         (let length_min_=length_min.(pj).(j) -. length_min.(pi).(i) in
                            (length_nom_-. parameters.measure) /. length_min_)
                       else
                         (let length_max_=length_max.(pj).(j) -. length_max.(pi).(i) in
                            (parameters.measure -. length_nom_) /. length_max_)
                    )**3.
      in
        bad
    in
      (* A chaque etape, todo contient le dernier morceau de chemin qu'on a construit dans demerits *)
      if LineMap.is_empty todo then demerits else
      (
        let node,lastBadness=LineMap.min_binding todo in
        let todo'=ref (LineMap.remove node todo) in
          if node.paragraph >= Array.length lines then break parameters !todo' demerits else
            (
              (*Printf.printf "node : ";print_line node;*)
              (* On commence par chercher la première vraie boite après node *)
              let i=ref node.lineEnd in
              let demerits'=ref demerits in
              let register node nextNode badness=
                let reallyAdd ()=
                  todo':=LineMap.add nextNode badness !todo';
                  demerits':=LineMap.add nextNode (badness,node) !demerits'
                in
                  try
                    let bad,_=LineMap.find nextNode !demerits' in
                      if bad>badness then reallyAdd ()
                  with
                      Not_found->reallyAdd ()
              in

                while !i< Array.length (Array.get lines node.paragraph) &&
                  (match get node.paragraph !i with GlyphBox _->false | _->true) do
                  incr i
                done;

                (* Y a-t-il encore des boites dans ce paragraphe ? *)
                if !i>=Array.length (Array.get lines node.paragraph) then (

                  (* On s'apprete a changer de paragraphe.
                     - On ne peut pas le faire si on est une ou deux lignes avant la fin de la page
                     - Ni si on est juste une ligne apres le debut.
                     - Si on est a la fin de la page, il faut enlever une ligne.
                  *)
                  
                  if (node.height < parameters.line_height-3 || node.height=parameters.line_height-1) &&
                    (node.height >=1 || node.paragraph_height<=1) then

                      (let nextNode={ paragraph=node.paragraph+1; lastFigure=node.lastFigure;
                                      height=if node.height=parameters.line_height-1 then -1 else node.height+1;
                                      lineStart= 0; lineEnd= 0; paragraph_height= -1 }
                       in
                       let badness'=lastBadness in
                         register node nextNode badness');

                  break parameters !todo' !demerits'

                ) else (
                  
                  Printf.printf "i = %d\n" !i; flush stdout;
                  (* Ensuite, on cherche toutes les coupes possibles. Cas particulier : la fin du paragraphe. *)
                  let j=ref (!i+1) in
                  Printf.printf "%f %f\n" (length_min.(node.paragraph).(!j) -. length_min.(node.paragraph).(!i)) parameters.measure; flush stdout;
                    
                    while !j <= (Array.length (Array.get lines node.paragraph)) && 
                      (length_min.(node.paragraph).(!j) -. length_min.(node.paragraph).(!i)) <= parameters.measure do
                        
                      (if !j=Array.length (Array.get lines node.paragraph) ||
                         (length_max.(node.paragraph).(!j) -. length_max.(node.paragraph).(!i) >= parameters.measure && 
                            isGlue (get node.paragraph !j)) then
                           (let comp0=compression parameters.measure node.paragraph node.lineStart node.lineEnd in
                            let comp1=compression parameters.measure node.paragraph !i !j in
                            let v_distance= if node.height+1>=parameters.line_height then 0. else
                              collide node.paragraph comp0 node.lineEnd node.paragraph comp1 !j
                                node.lineStart 0. !i 0. infinity
                            in
                            let nextNode={ paragraph=node.paragraph; lastFigure=node.lastFigure;
                                           height=(node.height+1) mod parameters.line_height;
                                           lineStart= !i; lineEnd= !j;
                                           paragraph_height=node.paragraph_height+1 }
                            in
                              Printf.printf "collide : %f\n" v_distance;
                              print_line nextNode;
                              register node nextNode (lastBadness+.(badness node.paragraph !i node.paragraph !j)))
                      );
                      incr j
                    done;
                    break parameters !todo' !demerits'
                )
            )
      )
  in
  let todo=LineMap.singleton { paragraph=0; lineStart=0; lineEnd=0; lastFigure=(-1); height= -1;paragraph_height= -1 } 0. in
  let demerits=break parameters0 todo (LineMap.empty) in
    Printf.printf "demerits : done %d\n" (LineMap.cardinal demerits);
  let (b,(_,_)) = LineMap.max_binding demerits in
    print_line b;
  let rec makeLines node result=


#ifdef DEBUG_BOXES
    print_line node;
    for i=node.lineStart to node.lineEnd-1 do
      match get node.paragraph i with
          Glue _->Printf.printf " "
        | GlyphBox x->Printf.printf "%s" (x.contents)
    done;
    print_newline();

#endif


    try  
      let _,next=LineMap.find node demerits in
        makeLines next (node::result)
    with
        Not_found->if node.paragraph>0 || node.height>=0 then raise Impossible else result
  in

  let rec makePages p pages=match p with
      []->pages
    | node::s ->(
        let pages'=if node.height=0 || (match pages with []->true | _->false) then
          (Array.create parameters0.line_height [])::pages else pages in
        let first=List.hd pages' in
        
          if node.lineEnd > node.lineStart then
            first.(node.height)<-makeLine node;
          
          makePages s pages'
      )
  in
    Array.of_list (List.rev (makePages (makeLines b []) []))
