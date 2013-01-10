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
open Fonts.FTypes
open Fonts.Opentype
open Bezier
let f = try Sys.argv.(1) with _ -> "euler.otf"
let initial_glyph = try int_of_string Sys.argv.(2) with _ -> 0

let font= loadFont f
let nb_glyphs = cardinal font

let zoom=ref 1.
let round x=Binary.round x


let draw_glyph xx0 yy0 glyph=
  let _,miny,_,maxy, arr=glyph_roots glyph in
    for y=miny to miny + Array.length arr-1 do
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
      in draw arr.(y-miny)
    done

let draw_empty_glyph xx0 yy0 glyph=
  List.iter (
    List.iter (
      fun (a,b)->
        Graphics.moveto (xx0+round a.(0)) (yy0+round b.(0));
        if Array.length a=2 then
          Graphics.lineto (xx0+round a.(1)) (yy0+round b.(1))
        else (
          Graphics.curveto (xx0+round a.(1), yy0+round b.(1)) (xx0+round a.(2), yy0+round b.(2))
            (xx0+round a.(3), yy0+round b.(3))
        )
    )
  ) (glyph)


let _=
  let xx0=100 in
  let yy0=100 in
  let arr = Array.init nb_glyphs (fun i -> i) in
    Graphics.open_graph "";
    Graphics.auto_synchronize false;
    let rec show_glyphs empt i=
      let gl=loadGlyph font { empty_glyph with glyph_index=arr.(i mod (Array.length arr)) } in
        Graphics.clear_graph ();

        Graphics.set_color (Graphics.rgb 150 150 150);
        Graphics.moveto 5 5; Graphics.draw_string "'n': +1  'N': +50  'p': -1 'P': -50 'e' : empty '+': zoom '-': zoom  'q': quit";
        Graphics.moveto 10 20; Graphics.draw_string (f ^ ", glyph " ^ (string_of_int (glyphNumber gl).glyph_index) ^ " / "
                                                     ^ (string_of_int nb_glyphs) ^ " width : "^(string_of_float (glyphWidth gl))
                                                     ^ " y0 : "^(string_of_float (glyph_y0 gl))
                                                     ^ " y1 : "^(string_of_float (glyph_y1 gl))
                                                    );
        let out=List.map (List.map (fun (a,b)->Array.map (fun x->x*. !zoom) a, Array.map (fun x->x*. !zoom) b)) (outlines gl) in
          if empt then draw_empty_glyph xx0 yy0 out else
            draw_glyph xx0 yy0 out ;

            Graphics.synchronize ();
            let s=Graphics.wait_next_event [Graphics.Key_pressed] in
              match s.Graphics.key with
                  'q' -> exit 0
                | 'p' -> show_glyphs empt (Array.length arr + i-1)
                | 'P' -> show_glyphs empt (Array.length arr + i-50)
                | 'N' -> show_glyphs empt (Array.length arr + i+50)
                | 'e' -> show_glyphs (not empt) i
                | '+' -> (zoom:= !zoom+.0.1; show_glyphs empt i)
                | '-' -> (zoom:= !zoom-.0.1; show_glyphs empt i)
                | ' ' | 'n' -> show_glyphs empt (i+1)
                | _ -> show_glyphs empt (i+1)
    in
      show_glyphs true initial_glyph
