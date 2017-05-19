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

(* This file only contains printing functions and stuff that are only
   useful for debuging. *)

open UsualMake
open RawContent
open FTypes
open Box

let print_frame_struct (f,s as p) =
  let path = List.map fst s in
  let f = frame_top p in
  Printf.fprintf stderr "path: ";
  List.iter (fun i -> Printf.fprintf stderr "%d " i) path;
  Printf.fprintf stderr "\n";
  let rec fn ch f =
    Printf.fprintf ch "#%d [\n" (List.length f.frame_content);
    IntMap.iter (fun i f ->
      Printf.fprintf ch "%d:%a " i fn f) f.frame_children;
    Printf.fprintf ch "]\n"
  in
  fn stderr (fst f)

let layout_to_dot l file =
  let o=open_out file in
  let rec doit path lay=
    IntMap.iter (fun k a->
      if path==(List.map fst (snd l)) then(
        Printf.fprintf o "n%s[color=red];\n"
          (String.concat "_" (List.map string_of_int path))
      );
      Printf.fprintf o "n%s->n%s;\n"
        (String.concat "_" (List.map string_of_int path))
        (String.concat "_" (List.map string_of_int (k::path)));
      doit (k::path) a;
    ) lay.frame_children;
  in
  Printf.fprintf o "digraph {\n";
  doit [] (fst (frame_top l));
  Printf.fprintf o "}\n";
  close_out o


let sprint_linef l=
  Printf.sprintf "{ paragraph=%d; lineStart=%d; lineEnd=%d; hyphenStart=%d; hyphenEnd=%d; lastFigure=%d; height=%f; isFigure=%b; page=%d }"
    l.paragraph l.lineStart l.lineEnd l.hyphenStart l.hyphenEnd l.lastFigure l.height
    l.isFigure (frame_page l.layout)

let print_linef out l=Printf.fprintf out "%s\n" (sprint_linef l)
let print_line l=print_linef stderr l

let rec print_box chan=function
    GlyphBox x->Printf.fprintf chan "%s" (Fonts.glyphContents x.glyph)
  | Kerning x->print_box chan x.kern_contents
  | Glue _->Printf.fprintf chan " "
  | Drawing _->Printf.fprintf chan "[Drawing]"
  | Hyphen x->Array.iter (print_box chan) x.hyphen_normal
  | Marker m->Printf.fprintf chan "[Marker %s]" (print_marker m)
  | BeginFigure _->Printf.fprintf chan "[BeginFigure]"
  | FlushFigure _->Printf.fprintf chan "[FlushFigure]"
  | Parameters _ ->Printf.fprintf chan "[Parameters]"
  | Layout _ ->Printf.fprintf chan "[Layout]"
  | Empty ->()

and print_link () l = match l with
    Extern s -> Printf.sprintf "Extern %S" s
  | Intern s -> Printf.sprintf "Intern %S" s
  | Button (b,n) -> Printf.sprintf "Button %S %s" n
                               (match b with Click _ -> "Click"
                                           | Drag  _ -> "Drag"
                                           | Edit  _ -> "Edit")

and print_marker m=match m with
    Label l->Printf.sprintf "Label %s" l
  | FigureRef i->Printf.sprintf "FigureRef %d" i
  | Pageref s->Printf.sprintf "PageRef %S" s
  | Structure _->Printf.sprintf "Structure"
  | BeginLink s->Printf.sprintf "BeginLink %a" print_link s
  | EndLink->Printf.sprintf "EndLink"
  | AlignmentMark->Printf.sprintf "AlignmentMark"

let rec print_box_type chan=function
    GlyphBox _->Printf.fprintf chan "GlyphBox "
  | Kerning _->Printf.fprintf chan "Kerning "
  | Glue _->Printf.fprintf chan "Glue "
  | Drawing _->Printf.fprintf chan "Drawing "
  | Hyphen _->Printf.fprintf chan "Hyphen "
  | Marker _->Printf.fprintf chan "Marker "
  | BeginFigure _->Printf.fprintf chan "BeginFigure "
  | FlushFigure _->Printf.fprintf chan "FlushFigure "
  | Parameters _ ->Printf.fprintf chan "Parameters"
  | Layout _ ->Printf.fprintf chan "Layout"
  | Empty ->Printf.fprintf chan "Empty "

let print_text_line lines node=
  print_linef stderr node;
  for i=node.lineStart to node.lineEnd-1 do
    print_box stderr (lines.(node.paragraph).(i))
  done;
  Printf.fprintf stderr "\n"

open Document

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
