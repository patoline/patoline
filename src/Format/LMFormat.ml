(*
  Copyright Tom Hirschowitz, Florian Hatat, Pierre-Etienne Meunier,
  Christophe Raffalli and others, 2012.

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
open Typography.Complete
open Typography.Fonts.FTypes
open Typography.Util
open Typography.Fonts
open Typography.Box
open Typography.Layout
open CamomileLibrary

let _=Random.self_init ()

let lmroman =
  [ Regular,
    (Lazy.lazy_from_fun
       (fun ()->
         (Fonts.loadFont (findFont "lmodern/lmroman10-regular.otf")),
          (fun x->x),
          (fun x->x),
          (fun x->x)),
    Lazy.lazy_from_fun
       (fun ()->
         (Fonts.loadFont (findFont "lmodern/lmroman10-italic.otf")),
          (fun x->x),
          (fun x->x),
          (fun x->x)));
    Bold,
    (Lazy.lazy_from_fun
       (fun ()->
         (Fonts.loadFont (findFont "lmodern/lmroman10-bold.otf")),
          (fun x->x),
          (fun x->x),
          (fun x->x)),
    Lazy.lazy_from_fun
       (fun ()->
         (Fonts.loadFont (findFont "lmodern/lmroman10-bolditalic.otf")),
          (fun x->x),
          (fun x->x),
          (fun x->x)));
    Caps,
    (simpleFamilyMember (fun ()->Fonts.loadFont (findFont
    "lmodern/lmromancaps10-regular.otf")),
     simpleFamilyMember (fun ()->Fonts.loadFont (findFont
     "lmodern/lmromancaps10-oblique.otf")));
  ]

let lmmono =
  [ Regular,
    (Lazy.lazy_from_fun
       (fun ()->
         (Fonts.loadFont (findFont "lmodern/lmmono10-regular.otf")),
          (fun x->x),
          (fun x->x),
          (fun x->x)),
    Lazy.lazy_from_fun
       (fun ()->
         (Fonts.loadFont (findFont "lmodern/lmmono10-italic.otf")),
          (fun x->x),
          (fun x->x),
          (fun x->x)));
    Bold,
    (Lazy.lazy_from_fun
       (fun ()->
         (Fonts.loadFont (findFont "lmodern/lmmono10-bold.otf")),
          (fun x->x),
          (fun x->x),
          (fun x->x)),
    Lazy.lazy_from_fun
       (fun ()->
         (Fonts.loadFont (findFont "lmodern/lmmono10-bolditalic.otf")),
          (fun x->x),
          (fun x->x),
          (fun x->x)));
    Caps,
    (simpleFamilyMember (fun ()->Fonts.loadFont (findFont
    "lmodern/lmmonocaps10-regular.otf")),
     simpleFamilyMember (fun ()->Fonts.loadFont (findFont
     "lmodern/lmmonocaps10-oblique.otf")));
  ]

module LMMath = struct
  include Euler

  open Document.Mathematical

  let italicsubst =
    List.map (fun x -> {x with glyph_index =
      if x.glyph_index >= 34 && x.glyph_index <= 49 then
        x.glyph_index + 3231 - 34 - 100
      else if x.glyph_index >= 56 && x.glyph_index <= 91 then
        x.glyph_index + 3263 - 56 - 110
      else x.glyph_index})

  let default_env = { Euler.default_env with
    mathsFont = Lazy.lazy_from_fun (fun () -> Fonts.loadFont (findFont
    "lmodern/lmmath.otf"));
    mathsSubst = italicsubst }

  let default_env2 = { default_env with
    delimiter_up_tolerance=0.3;
    delimiter_down_tolerance=0.15;
    op_tolerance = 0.95;
    op_limits_tolerance = 0.5;
  }
    
  let default=[|
    { default_env with mathsSubst=msubst (Lazy.force displaySubst) };
    { default_env with mathsSubst=msubst (Lazy.force displaySubst) };
    default_env2;
    default_env2;
    { default_env2 with mathsSize=2./.3. };
    { default_env2 with mathsSize=2./.3. };
    { default_env2 with mathsSize=4./.9. };
    { default_env2 with mathsSize=4./.9. }
	      |]

end

module MakeFormat (D:Document.DocumentStructure)
(Default : module type of DefaultFormat.Format(D)) =
  struct

    include Default

    let defaultEnv:environment=
      let f,str,subst,pos=selectFont lmroman Regular false in
      let hyphenate=
      	try
        let i=open_in_bin (findHyph "en.hdict") in
        let inp=input_value i in
          close_in i;
          (fun str->
             let hyphenated=Hyphenate.hyphenate inp str in
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
                 | h::s->(hyph s 0 h; pos));
      with
          File_not_found (f,p)->
	    (Printf.fprintf stderr "Warning : no hyphenation dictionary (%s not found). Path :\n" f;
                                  List.iter (Printf.fprintf stderr "%s\n") p;
                                  fun x->[||])
      in
      let replace_utf8 x y z=if String.length x>0 then (
        let buf=Buffer.create (String.length x) in
        let repl=UTF8.init 1 (fun _->UChar.chr y) in
        let rec add_it i=
          if not (UTF8.out_of_range z i) then (
            try
              let rec comp j=
                if UTF8.out_of_range x j then j else
                  if UTF8.out_of_range z (i+j) then raise Not_found else
                    if UTF8.look z (i+j) <> UTF8.look x j then raise Not_found else
                      comp (UTF8.next x j)
              in
              let j=comp 0 in
                Buffer.add_string buf repl;
                add_it (i+j)
            with
                Not_found->(
                  Buffer.add_string buf (String.sub z i (UTF8.next z i-i));
                  add_it (UTF8.next z i)
                )
          )
        in
          add_it 0;
          Buffer.contents buf
      ) else z
      in
      let fsize=3.8 in
      let feat= [ Opentype.standardLigatures ] in
      let loaded_feat=Fonts.select_features f [ Opentype.standardLigatures ] in
        {
          fontFamily=lmroman;
          fontMonoFamily=lmmono;
	  fontMonoRatio=1.0;
          fontItalic=false;
          fontAlternative=Regular;
          fontFeatures=feat;
          fontColor=OutputCommon.black;
          font=f;
          mathsEnvironment=LMMath.default;
	  mathStyle=Document.Mathematical.Text;
          word_substitutions=
            (fun x->List.fold_left (fun y f->f y) x
               [
                 replace_utf8 ("``") 8220;
                 replace_utf8 ("''") 8221
               ]
            );
          substitutions=(fun glyphs -> List.fold_left (fun a b->apply b a) (subst glyphs) loaded_feat);
          positioning=(fun x->pos (positioning f x));
          footnote_y=10.;
          size=fsize;
          lead=13./.10.*.fsize;
          normalMeasure=150.;
          normalLead=13./.10.*.fsize;
          normalLeftMargin=(fst a4-.150.)/.2.;
          normalPageFormat=Default.defaultEnv.normalPageFormat;
          par_indent = [Drawing { drawing_min_width= 4.0 *. phi;
                                  drawing_max_width= 4.0 *. phi;
                                  drawing_y0=0.;drawing_y1=0.;
                                  drawing_nominal_width= 4.0 *. phi;
                                  drawing_contents=(fun _->[]);
                                  drawing_badness=fun _-> 0. }];
          hyphenate=hyphenate;
          counters=List.fold_left (fun m (a,b)->StrMap.add a b m) StrMap.empty
            ["_structure",(-1,[0]);
             "_figure",(-1,[0]);
             "figure",(2,[0])];
          names=StrMap.empty;
          user_positions=UserMap.empty;
          new_page=Document.default_new_page a4;
	  show_boxes=false;
	  show_frames=false;
        }

end

module LMArticle = struct
  module Format (D : DocumentStructure) =
    FormatArticle.MakeFormat (D)
      (MakeFormat (D) (DefaultFormat.Format(D)))
end
