open Fonts.FTypes
open Fonts.Opentype
open Bezier
let f = try Sys.argv.(1) with _ -> "AldusLTStd-Roman.otf"
let initial_glyph = try int_of_string Sys.argv.(2) with _ -> 0

let font= loadFont f
let nb_glyphs = cardinal font
let round=Binary.round

let x=read_lookup font 4

let foi=float_of_int


let basic_draw xx0 yy0 glyph=
  let eps=0.1 in
    List.iter (fun l->
                 Array.iter (
                   fun (fx,fy)->
                     if Array.length fx=2 then (
                       let _=Graphics.wait_next_event [Graphics.Key_pressed] in
                         Graphics.moveto (xx0+round fx.(0)) (yy0+round fy.(0));
                         Graphics.lineto (xx0+round fx.(1)) (yy0+round fy.(1))
                     ) else (
                       let rec draw t x y=
                         if t<1. then (
                           let x'=eval fx t in
                           let y'=eval fy t in
                           let _=Graphics.wait_next_event [Graphics.Key_pressed] in
                             Graphics.moveto (xx0+round x) (yy0+round y);
                             Graphics.lineto (xx0+round x') (yy0+round y');
                             draw (t+.eps) x' y'
                         )
                       in
                         draw eps fx.(0) fy.(0)
                     )
                 ) l
              ) glyph


let draw_glyph xx0 yy0 glyph=
  let eps=0.01 in
  let minx=List.fold_left (List.fold_left (fun a (fx,_)->Array.fold_left min a fx)) infinity glyph in
  let maxx=List.fold_left (List.fold_left (fun a (fx,_)->Array.fold_left max a fx)) (-.infinity) glyph in
  let miny=List.fold_left (List.fold_left (fun a (_,fy)->Array.fold_left min a fy)) infinity glyph in
  let maxy=List.fold_left (List.fold_left (fun a (_,fy)->Array.fold_left max a fy)) (-.infinity) glyph in
  let lines=List.map (fun l->List.fold_right (fun (fx,fy) lines->
                                        if Array.length fx=2 then (
                                          (fx.(0),fy.(0),fx.(1),fy.(1))::lines
                                        ) else (
                                          let rec draw t x y lines0=
                                            if t<1. then (
                                              let x'=eval fx t in
                                              let y'=eval fy t in
                                                draw (t+.eps) x' y' ((x,y,x',y')::lines0)
                                            ) else (
                                              (x,y,fx.(Array.length fx-1),fy.(Array.length fy-1))::lines0
                                            )
                                          in
                                            draw eps fx.(0) fy.(0) lines
                                        )
                                     ) l []) glyph
  in
  let rec inter y lines i result=
    if i>=Array.length lines then result else (
      let (x0,y0,x1,y1) = lines.(i) in
        if (min y0 y1)>y || (max y0 y1)<y then (
          inter y lines (i+1) result
        ) else (
          if y=148. then (Printf.printf "%f %f %f %f\n" x0 y0 x1 y1; flush stdout);
          let result'=
            if y0=y then (
              let _,y0',_,_=lines.((i-1+Array.length lines) mod (Array.length lines)) in
              let y1'=ref y1 in
              let j=ref (i+1) in
                while !y1'=y0 && (!j mod (Array.length lines))<>i do
                  let _,y0',_,_=lines.(!j mod (Array.length lines)) in
                    y1':=y0';
                    incr j
                done;
                if (y0'-.y0)*.(!y1'-.y0) < 0. then x0::result else result
            ) else
              if y<>y1 then (x0+.(y-.y0)*.(x1-.x0)/.(y1-.y0))::result else result
          in
            inter y lines (i+1) result'
        )
    )
  in
    for y = int_of_float (floor miny) to int_of_float (ceil maxy) do
      let revLines=List.map (fun p->Array.of_list (List.rev p)) lines in
      let inters=List.concat (List.map (fun l-> inter (foi y) l 0 []) revLines) in
      let inter=List.sort compare inters in
        if y=148 then (List.iter (Printf.printf "%f ") inter; flush stdout);
      let rec remove_dupl=function
          []-> []
        | h::h'::s when h=h' -> remove_dupl (h'::s)
        | h::s-> h::(remove_dupl s)
      in
      let rec draw=function
          x0::x1::s -> (
            let g0=round (255.*.(x0-.floor x0)) in
            let g1=round (255.*.(ceil x1-.x1)) in
              Graphics.set_color Graphics.black;
              Graphics.moveto (xx0+(int_of_float (ceil x0))) (yy0+y);
              Graphics.lineto (xx0+(int_of_float (floor x1))) (yy0+y);
              Graphics.set_color (Graphics.rgb g0 g0 g0);
              Graphics.plot (xx0+(int_of_float (floor x0))) (yy0+y);
              Graphics.set_color (Graphics.rgb g1 g1 g1);
              Graphics.plot (xx0+(int_of_float (ceil x1))) (yy0+y);

            draw s
          )
        | _-> ()
      in draw (inter)
    done;
    let x=141 in
    let y=148 in
      Graphics.set_color Graphics.red;
      Graphics.moveto (xx0+x) yy0;
      Graphics.lineto (xx0+x) (yy0+500);
      Graphics.moveto xx0 (yy0+y);
      Graphics.lineto (xx0+800) (yy0+y)


let bezier3=Graphics.curveto

let _=
  let xx0=100 in
  let yy0=100 in
  let arr = Array.init nb_glyphs (fun i -> i) in
    Graphics.open_graph "";
    (* Graphics.auto_synchronize false; *)
    let rec show_glyphs i=
      let gl=loadGlyph font { empty_glyph with glyph_index=arr.(i mod (Array.length arr)) } in
      Graphics.clear_graph ();

      Graphics.set_color (Graphics.rgb 150 150 150);
      Graphics.moveto 5 5; Graphics.draw_string "'n': +1  'N': +50  'p': -1 'P': -50  'q': quit";
      Graphics.moveto 10 20; Graphics.draw_string (f ^ ", glyph " ^ (string_of_int (glyphNumber gl).glyph_index) ^ " / " ^ (string_of_int nb_glyphs) ^ " width : "^(string_of_float (glyphWidth gl)) );
      Graphics.set_color Graphics.black;

      (* Graphics.moveto 0 yy0;Graphics.lineto (Graphics.size_x()) yy0; *)
      let out=outlines gl in
        draw_glyph xx0 yy0 out;
        (* List.iter (fun (x,y)-> *)
        (*              Printf.printf "%f %f %f %f\n" x.(0) y.(0) x.(Array.length x-1) y.(Array.length y-1); *)
        (*              flush stdout; *)
        (*              if Array.length x = 2 then *)
        (*                (let (a,b)=x.(0),y.(0) in *)
        (*                 let (c,d)=x.(1),y.(1) in *)
        (*                   Graphics.moveto (xx0+round a) (yy0+round b); *)
        (*                   Graphics.lineto (xx0+round c) (yy0+round d)) *)
        (*              else *)
        (*                (let (a,b)=x.(0),y.(0) in *)
        (*                 let (c,d)=x.(1),y.(1) in *)
        (*                 let (e,f)=x.(2),y.(2) in *)
        (*                 let (g,h)=x.(3),y.(3) in *)
        (*                   Graphics.moveto (xx0+round a) (yy0+round b); *)
        (*                   bezier3 (xx0+round c, yy0+round d) (xx0+round e, yy0+round f) (xx0+round g, yy0+round h)); *)
        (*              let _=Graphics.wait_next_event [Graphics.Key_pressed] in () *)
        (*           ) out; *)
        Graphics.synchronize ();
        let s=Graphics.wait_next_event [Graphics.Key_pressed] in
          match s.Graphics.key with
             'q' -> exit 0
           | 'p' -> show_glyphs (Array.length arr + i-1)
           | 'P' -> show_glyphs (Array.length arr + i-50)
           | 'N' -> show_glyphs (Array.length arr + i+50)
           | ' ' | 'n' -> show_glyphs (i+1)
           | _ -> show_glyphs (i+1)
    in
      show_glyphs initial_glyph
