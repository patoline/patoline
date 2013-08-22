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
open Typography
open Typography.Document
open Typography.OutputCommon
open Typography.Util

open Typography.Box
open Typography.Fonts
open Typography.Fonts.FTypes
open CamomileLibrary
open Typography.Document
open HtmlFonts

module MathFonts=DefaultFormat.MathFonts
module MathsFormat=DefaultFormat.MathsFormat


type div={ div_children:page array; title:string }
and page=Div of div | Par of string list


module Format=functor (D:Document.DocumentStructure)->(
  struct

    module Default=DefaultFormat.Format(D)
    include (Default:
               (((module type of Default
                 with
                   module Output:=Default.Output)
                 with
                   module Make_theorem:=Default.Make_theorem)
                with
                  module TableOfContents:=Default.TableOfContents))



    module type Driver=sig
      val output':unit
    end


    module Output(M:Driver)=struct

      type output=unit
      let outputParams=()
      let output out_params structure defaultEnv file=
        let structure=Default.postprocess_tree structure in
        let prefix=file in
        let rec unlink_rec dir=
          if Sys.file_exists dir then (
            if Sys.is_directory dir then (
              Array.iter (fun x->unlink_rec (Filename.concat dir x)) (Sys.readdir dir);
              Unix.rmdir dir
            ) else (
              Unix.unlink dir
            )
          );
        in
        (try
           unlink_rec prefix;
           Unix.mkdir prefix 0o755;
         with
             _->());

        let fixable=ref false in
        let rec fix env n=
          let (env,_,_,_,_,_,_,pars,_,figures,_)=flatten env fixable structure in
          let env=reset_counters env in
          if !fixable && n>0 then
            fix env (n-1)
          else
            (env,pars,figures)
        in
        let (env,pars,figures)=fix defaultEnv 3 in
        let typeset_pars=
          (Array.map (fun x->draw_boxes env (Array.to_list x)) pars)
        in
        let cache=build_font_cache prefix typeset_pars in
        (*
        let rec fold_struct t=match t with
            Node n->
              List.concat (List.rev (IntMap.fold (fun k a x->fold_struct a::x) n.children []))
          | Paragraph p->
            List.map (fun c->match c with
                T (t,_)->Printf.sprintf "<span class=\"\">%s</span>" t
              | _->""
            ) p.par_contents
          | _->[]
        in
        *)
        let classname=ref (-1) in
        let span_open=ref false in
        let style_buffer=Rbuffer.create 1000 in
        let pages=Array.map (fun page->
          let buf=Rbuffer.create 1000 in
          let rec output_contents b=match b with
              GlyphBox g->(
                let cl=className cache g in
                if !classname<>cl || not !span_open then (
                  if !classname<>(-1) then (
                    Rbuffer.add_string buf "</span>";
                    span_open:=false
                  );
                  classname:=cl;
                  Rbuffer.add_string buf (Printf.sprintf "<span class=\"f%d\">" cl);
                  span_open:=true;
                );
                Rbuffer.add_string buf ((Fonts.glyphNumber g.glyph).glyph_utf8);
              )
            | Kerning x->output_contents x.kern_contents
            | Hyphen x->Array.iter output_contents x.hyphen_normal
            | Glue g->Rbuffer.add_string buf " "
            | Drawing d->(
              let i=SVG.images_of_boxes ~cache:cache ~css:"" prefix env [|[b]|] in
              Rbuffer.add_string buf i.(0)
            )
            | Marker (Label a)->(
              Rbuffer.add_string buf "<a name=\"";
              Rbuffer.add_string buf a;
              Rbuffer.add_string buf "\"></a>";
            )
            | Marker (BeginURILink a)->(
              Rbuffer.add_string buf "<a href=\"";
              Rbuffer.add_string buf a;
              Rbuffer.add_string buf "\">";
            )
            | Marker (BeginLink a)->(
              Rbuffer.add_string buf "<a href=\"#";
              Rbuffer.add_string buf a;
              Rbuffer.add_string buf "\">";
            )
            | Marker EndLink->(
              Rbuffer.add_string buf "</a>";
            )
            | _->(
              Printf.fprintf stderr "Box not shown: ";
              print_box stderr b;
              Printf.fprintf stderr "\n";flush stderr;
            )
          in
          Array.iter (fun par->
            Rbuffer.add_string buf "<p>";
            List.iter output_contents (Array.to_list par);
            if !span_open then (
              Rbuffer.add_string buf "</span>";
              span_open:=false
            );
            Rbuffer.add_string buf "</p>";
          ) page;
          buf
        ) [|pars|]
        in

        let st=open_out (Filename.concat prefix "style.css") in
        Rbuffer.output_buffer st style_buffer;
        Rbuffer.output_buffer st (SVG.make_defs prefix cache);
        Rbuffer.output_buffer st (SVG.make_defs ~output_fonts:true ~units:"mm" ~class_prefix:"f"
                                    prefix cache);
        close_out st;

        output_fonts cache;

        for i=0 to Array.length pages-1 do
          let out_file=open_out (Filename.concat prefix (Printf.sprintf "%d.html" i)) in
          Printf.fprintf out_file "<html><body>";
          Printf.fprintf out_file "<link rel=\"stylesheet\" href=\"style.css\">";
          Rbuffer.output_buffer out_file pages.(i);
          Printf.fprintf out_file "</body></html>";
          close_out out_file
        done;
        M.output'

    end
  end)
