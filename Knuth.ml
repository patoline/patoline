open Drivers
open Binary
open Constants
open CamomileLibrary
open Util
open FontsTypes

exception Impossible


type line= { paragraph:int; lineEnd:int; hyphenEnd:int; lastFigure:int }

let print_line l=
  Printf.printf "{ paragraph=%d; lineEnd=%d; hyphenEnd=%d; lastFigure=%d }\n"
  l.paragraph l.lineEnd l.hyphenEnd l.lastFigure

let rec print_box=function
    Glue _->Printf.printf " "
  | GlyphBox (_,x)->Printf.printf "%s" (x.contents)
  | Kerning x->print_box x.kern_contents
  | Hyphen x->Array.iter print_box x.hyphen_normal
  | _->Printf.printf "[]"

let print_text_line lines pi i hi j hj=
    (if hi>=0 then (match lines.(pi).(i) with
                        Hyphen x->Array.iter print_box (snd x.hyphenated.(hi))
                      | _->()));
    for k=i to j-1 do
      print_box (lines.(pi).(k))
    done;
    (if hj>=0 then (match lines.(pi).(j) with
                        Hyphen x->Array.iter print_box (fst x.hyphenated.(hj))
                      | _->()));
    print_newline()



module LineMap=Map.Make (struct type t=line let compare=compare end)

let rec print_graph file lines graph=
  let f=open_out file in
    Printf.fprintf f "digraph {\n";
    LineMap.iter (fun k (_,a)->
                    Printf.fprintf f "node_%d_%s [label=\""
                      k.paragraph (if k.lineEnd>=0 then string_of_int k.lineEnd else "x");
                    Printf.fprintf f "\"];\n";

                    Printf.fprintf f "node_%d_%s -> node_%d_%s\n"
                      a.paragraph (if a.lineEnd>=0 then string_of_int a.lineEnd else "x")
                      k.paragraph (if k.lineEnd>=0 then string_of_int k.lineEnd else "x")
                 ) graph;
    Printf.fprintf f "};\n";
    close_out f




let lineBreak parameters0 ?figures:(figures = [||]) lines=
  let get i j=lines.(i).(j) in
  let length_min=Array.create (Array.length lines) [||] in
  let length_max=Array.create (Array.length lines) [||] in
  let _=
    let bmin=ref 0. in
    let bmax=ref 0. in
      for i=0 to Array.length lines-1 do
        length_min.(i)<-Array.create (Array.length (Array.get lines i)+1) 0.;
        length_max.(i)<-Array.create (Array.length (Array.get lines i)+1) 0.;
        for j=0 to Array.length (Array.get lines i)-1 do
          length_min.(i).(j)<- !bmin;
          length_max.(i).(j)<- !bmax;
          let a,b=box_interval (get i j) in
            bmin:= !bmin+.a;
            bmax:= !bmax+.b
        done;
        length_min.(i).(Array.length (Array.get lines i))<- !bmin;
        length_max.(i).(Array.length (Array.get lines i))<- !bmax;
      done
  in

  let compression m p i hi j hj= if i<0 || j<0 then 0. else (
    let minLine=ref 0. in
    let maxLine=ref 0. in
      if hi>=0 then (
        match lines.(p).(i) with
            Hyphen x->let a,b=boxes_interval (snd x.hyphenated.(hi)) in
              (minLine:= !minLine+.a;
               maxLine:= !maxLine+.b)
          | _->());
      if hj>=0 then (
        match lines.(p).(j) with
            Hyphen x->let a,b=boxes_interval (fst x.hyphenated.(hj)) in
              (minLine:= !minLine+.a;
               maxLine:= !maxLine+.b)
          | _->());
      for k=(if hi>=0 then i+1 else i) to j-1 do
        let a,b=box_interval lines.(p).(k) in
          minLine := !minLine+.a;
          maxLine := !maxLine+.b
      done;
      (* if ((m-. !minLine)/.(!maxLine-. !minLine)) < 0. then Printf.printf "\n\n\n Negatif : %f %f %f\n\n\n" m !minLine !maxLine; *)
      max 0. (min 1. ((m-. !minLine)/.(!maxLine-. !minLine)))
  )
  in
  let rec break parameters todo demerits=
   
    let h_badness pi i hi j hj=
      let comp=compression parameters.measure pi i hi j hj in

      let bad=ref 0. in
        for k=i to j-1 do
          bad:= !bad +.
            (match lines.(pi).(i) with
                 Glue x->x.glue_badness (x.glue_min_width+.(x.glue_max_width-.x.glue_min_width)*.comp)
               | _->0.
            )
        done;
        !bad
    in
      
      (* A chaque etape, todo contient le dernier morceau de chemin qu'on a construit dans demerits *)
      if LineMap.is_empty todo then demerits else
      (
        let node,lastBadness=LineMap.min_binding todo in
        let todo'=ref (LineMap.remove node todo) in
          if node.paragraph >= Array.length lines then break parameters !todo' demerits else
            (
              (* On commence par chercher la première vraie boite après node *)
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
              let i0=ref node.lineEnd in
                while !i0<Array.length lines.(node.paragraph) && match lines.(node.paragraph).(!i0) with Glue _ -> true | _->false do
                  incr i0
                done;
              let i= !i0 in
                (* Y a-t-il encore des boites dans ce paragraphe ? *)
                if i>=Array.length (Array.get lines node.paragraph) then (
                  
                  (* On s'apprete a changer de paragraphe.
                     - On ne peut pas le faire si on est une ou deux lignes avant la fin de la page
                     - Ni si on est juste une ligne apres le debut.
                     - Si on est a la fin de la page, il faut enlever une ligne.
                  *)
                  
                  (let nextNode={ 
                     paragraph=node.paragraph+1; lastFigure=node.lastFigure;
                     hyphenEnd= -1;
                     lineEnd=0 }
                   in
                   let badness'=lastBadness in
                     register node nextNode badness');
                  
                  break parameters !todo' !demerits'

                ) else (
                  
                  (* Ensuite, on cherche toutes les coupes possibles. Cas particulier : la fin du paragraphe. *)
                  let rec break_next j sum_min sum_max=
                    let make_next_node hyphen=
                      let nextNode={ paragraph=node.paragraph;
                                     lastFigure=node.lastFigure;
                                     hyphenEnd= hyphen;
                                     lineEnd=j }
                      in
                      let bad=(lastBadness +. (h_badness node.paragraph i node.hyphenEnd j hyphen)) in
                        register node nextNode bad
                    in
                      
                      if j>=Array.length (lines.(node.paragraph)) then
                        (if sum_min<=parameters.measure then make_next_node (-1))
                      else(
                        match lines.(node.paragraph).(j) with
                            Hyphen x->(
                              for k=0 to Array.length x.hyphenated-1 do
                                let a,b=boxes_interval (fst x.hyphenated.(k)) in
                                  if sum_min+.a <= parameters.measure && sum_max+.b >= parameters.measure then
                                    make_next_node k
                              done;
                              let a,b=boxes_interval x.hyphen_normal in
                                break_next (j+1) (sum_min+. a) (sum_max+. b)
                            )
                          | _ when sum_min <= parameters.measure->(
                              (if sum_max >= parameters.measure then
                                 match lines.(node.paragraph).(j) with
                                     Glue _->make_next_node (-1)
                                   | _->());
                              let a,b=box_interval lines.(node.paragraph).(j) in
                                break_next (j+1) (sum_min+. a) (sum_max+. b)
                            )
                          | _->()
                      )
                  in
                    (if node.hyphenEnd >=0 then (
                       match lines.(node.paragraph).(node.lineEnd) with
                           Hyphen x->let a,b=boxes_interval (snd x.hyphenated.(node.hyphenEnd)) in
                             break_next (node.lineEnd+1) a b
                         | _->break_next i 0. 0.
                     ) else (break_next i 0. 0.));
                    break parameters !todo' !demerits'
                )
            )
      )
  in
  let todo=LineMap.singleton { paragraph=0; lineEnd= 0; hyphenEnd= -1;
                               lastFigure=(-1) } 0. in
  let demerits=break parameters0 todo (LineMap.empty) in

  print_graph "graph" lines demerits;
  let (b,(bad,_)) = LineMap.max_binding demerits in

    if b.paragraph<Array.length lines then
      (Printf.printf "impossible"; flush stdout; raise Impossible)
    else (
      let rec makeLines node result=
        try  
          let _,next=LineMap.find node demerits in
            makeLines next ((next.paragraph,next.lineEnd,next.hyphenEnd, node.lineEnd, node.hyphenEnd)::result)
        with
            Not_found->if node.paragraph>0 then raise Impossible else result
      in
      let makeLine pi i_ hi j_ hj y=
        let i0=ref i_ in
          while !i0 < Array.length lines.(pi) && (match lines.(pi).(!i0) with Glue _->true | _->false) do
            incr i0;
          done;

        let comp=compression parameters0.measure pi !i0 hi j_ hj in
        let rec makeLine boxes x i max_i line=if i<0 || max_i<0 then (0.,[]) else (
          
          if i>=max_i then (x,line) else
            match boxes.(i) with
                
                Glue g->let w=g.glue_min_width+.comp*.(g.glue_max_width-.g.glue_min_width) in
                  makeLine boxes (x+.w) (i+1) max_i line
                    
              | Kerning kbox as box -> makeLine boxes (x+.(box_width comp box)) (i+1) max_i
                  ((x+.kbox.kern_x0, y+.kbox.kern_y0, kbox.kern_contents)::line)
                    
              | Hyphen h->let (a,b)=makeLine h.hyphen_normal x 0 (Array.length h.hyphen_normal) line in
                  makeLine boxes a (i+1) max_i b
                    
         | box->makeLine boxes (x+.(box_width comp box)) (i+1) max_i ((x,y,box)::line)
        )
        in
        let u,v=(match lines.(pi).(!i0) with
                     Hyphen x when hi>=0->
                       let _,y=x.hyphenated.(hi) in
                       let u,v=makeLine y 0. 0 (Array.length y) [] in
                         makeLine lines.(pi) u (!i0+1) j_ v
                   | _->makeLine lines.(pi) 0. (!i0) j_ [])
        in
          if hj>=0 then match lines.(pi).(j_) with
              Hyphen x->let y,_=x.hyphenated.(hj) in snd (makeLine y u 0 (Array.length y) v)
            | _->v
          else
            v
      in
        
      let rec makePages p h pages=match p with
          []->pages
        | (pi,i,hi,j,hj)::s ->(
            let pages'=if h>=parameters0.lines_by_page || (match pages with []->true | _->false) then
              (Array.create parameters0.lines_by_page [])::pages else pages in
            let first=List.hd pages' in
              if j>i then
                first.(h) <- makeLine pi i hi j hj (float_of_int h *. parameters0.lead);
              
              makePages s (if h>=parameters0.lines_by_page then 0 else (h+1)) pages'
          )
      in
      let ln=makeLines b [] in
        Array.of_list (List.rev (makePages ln 0 []))
    )
