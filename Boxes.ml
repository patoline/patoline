open Drivers
open Binary
open Constants
open CamomileLibrary


type glyphBox= { contents:UTF8.t; glyph:Fonts.glyph; size: float; width:float }

type box=GlyphBox of glyphBox | Glue of (float*float*float)

let current_font=ref (Fonts.loadFont "AGaramondPro-Regular.otf")
let current_size=ref 4.

exception Impossible

let isGlue x=match x with Glue _->true | _->false

type line= { paragraph:int; lastLineStart:int; lastLineEnd:int;
             lineStart:int; lineEnd:int; lastFigure:int; height:int;
             paragraph_height:int
           }

let print_line l=
  Printf.printf "{ paragraph=%d; lastLineStart=%d; lastLineEnd=%d; lineStart=%d; lineEnd=%d; lastFigure=%d; height=%d }\n"
  l.paragraph l.lastLineStart l.lastLineEnd l.lineStart l.lineEnd l.lastFigure l.height

module LineMap=Map.Make (struct type t=line let compare=compare end)


type parameters={ format:float*float;
                  lead:float;
                  measure:float;
                  line_height:int }

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
             | GlyphBox x->(bmin:= !bmin+.x.width;bnom:= !bnom+.x.width;bmax:= !bmax+.x.width));
        done;
        length_min.(i).(Array.length (Array.get lines i))<- !bmin;
        length_nom.(i).(Array.length (Array.get lines i))<- !bnom;
        length_max.(i).(Array.length (Array.get lines i))<- !bmax;
      done
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

                      (let nextNode={ paragraph=node.paragraph+1; lastLineStart=0;
                                      lastLineEnd=0; lastFigure=node.lastFigure;
                                      height=if node.height=parameters.line_height-1 then -1 else node.height+1;
                                      lineStart= 0; lineEnd= 0; paragraph_height= -1 }
                       in
                       let badness'=lastBadness in
                         register node nextNode badness');
                  
                  break parameters !todo' !demerits'

                ) else (
                  
                  
                  (* Ensuite, on cherche toutes les coupes possibles. Cas particulier : la fin du paragraphe. *)
                  let j=ref (!i+1) in
                    while !j <= (Array.length (Array.get lines node.paragraph)) && 
                      (length_min.(node.paragraph).(!j) -. length_min.(node.paragraph).(!i)) <= parameters.measure do
                        
                      (if !j=Array.length (Array.get lines node.paragraph) ||
                         (length_max.(node.paragraph).(!j) -. length_max.(node.paragraph).(!i) >= parameters.measure && 
                            isGlue (get node.paragraph !j)) then
                           (let nextNode={ paragraph=node.paragraph; lastLineStart=node.lineStart;
                                           lastLineEnd=node.lineEnd; lastFigure=node.lastFigure;
                                           height=(node.height+1) mod parameters.line_height;
                                           lineStart= !i; lineEnd= !j;
                                           paragraph_height=node.paragraph_height+1 }
                            in
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
  let todo=LineMap.singleton { paragraph=0; lastLineStart=(-1); lastLineEnd=(-1);
                               lineStart=0; lineEnd=0; lastFigure=(-1); height= -1;paragraph_height= -1 } 0. in
  let demerits=break parameters0 todo (LineMap.empty) in
  let (b,(_,_)) = LineMap.max_binding demerits in
  let rec makeLines node result=
    (*************************************)
    print_line node;
    for i=node.lineStart to node.lineEnd-1 do
      match get node.paragraph i with
          Glue _->Printf.printf " "
        | GlyphBox x->Printf.printf "%s" (x.contents)
    done;
    print_newline();
    (*************************************)
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
            (
              let minLine=length_min.(node.paragraph).(node.lineEnd) -. length_min.(node.paragraph).(node.lineStart) in
              let maxLine=length_max.(node.paragraph).(node.lineEnd) -. length_max.(node.paragraph).(node.lineStart) in
              let compression=min 1. ((parameters0.measure-.minLine)/.(maxLine-. minLine)) in
              let rec makeLine i=
                if i>=node.lineEnd then [] else
                  match get node.paragraph i with
                      Glue (a,_,c)->(Glue (a, a+.compression*.(c-.a), c) :: makeLine (i+1))
                    | x->(x::makeLine (i+1))
              in
                first.(node.height)<-makeLine node.lineStart
            );
          
          makePages s pages'
      )
  in
    Array.of_list (List.rev (makePages (makeLines b []) []))
