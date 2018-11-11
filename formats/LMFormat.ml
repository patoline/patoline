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
open Typography.Complete
open Patfonts
open FTypes
open Fonts
open Typography.Box
open DefaultFormat
open Patutil
open Unicodelib
open Extra
open Patoraw

let _=Random.self_init ()

let lmroman =
  [ Regular,
    (
      simpleFamilyMember (fun ()->Fonts.loadFont (findFont
            FontPattern.({family="Latin Modern Roman"; slant=Roman; weight=Regular})
      )),
      simpleFamilyMember (fun ()->Fonts.loadFont (findFont
            FontPattern.({family="Latin Modern Roman"; slant=Italic; weight=Regular})
      ))
    );
    Bold,
    (
      simpleFamilyMember (fun ()->Fonts.loadFont (findFont
            FontPattern.({family="Latin Modern Roman"; slant=Roman; weight=Bold})
      )),
      simpleFamilyMember (fun ()->Fonts.loadFont (findFont
            FontPattern.({family="Latin Modern Roman"; slant=Italic; weight=Bold})
      ))
    );
    Caps,
    (
      simpleFamilyMember (fun ()->Fonts.loadFont (findFont
            FontPattern.({family="Latin Modern Roman Caps"; slant=Roman; weight=Regular})
      )),
      simpleFamilyMember (fun ()->Fonts.loadFont (findFont
            FontPattern.({family="Latin Modern Roman Caps"; slant=Italic; weight=Regular})
      ))
    );
  ]

let lmmono =
  [ Regular,
    (
      simpleFamilyMember (fun ()->Fonts.loadFont (findFont
            FontPattern.({family="Latin Modern Mono"; slant=Roman; weight=Regular})
      )),
      simpleFamilyMember (fun ()->Fonts.loadFont (findFont
            FontPattern.({family="Latin Modern Mono"; slant=Italic; weight=Regular})
      ))
    );
    Bold,
    (
      simpleFamilyMember (fun ()->Fonts.loadFont (findFont
            FontPattern.({family="Latin Modern Mono"; slant=Roman; weight=Bold})
      )),
      simpleFamilyMember (fun ()->Fonts.loadFont (findFont
            FontPattern.({family="Latin Modern Mono"; slant=Italic; weight=Bold})
      ))
    );
    Caps,
    (
      simpleFamilyMember (fun ()->Fonts.loadFont (findFont
            FontPattern.({family="Latin Modern Mono Caps"; slant=Roman; weight=Regular})
      )),
      simpleFamilyMember (fun ()->Fonts.loadFont (findFont
            FontPattern.({family="Latin Modern Mono Caps"; slant=Italic; weight=Regular})
      ))
    );
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
    mathsFont = Lazy.from_fun (fun () -> Fonts.loadFont (findFont
    FontPattern.({family = "Latin Modern Math"; slant=Roman; weight=Regular})));
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

let lmEnv env=
  let f,str,subst,pos=selectFont lmroman Regular false in
  let fsize=3.8 in
  let feat= [ Opentype.standardLigatures ] in
  { env with
    fontFamily=lmroman;
    fontMonoFamily=lmmono;
    fontMonoRatio=1.0;
    fontItalic=false;
    fontAlternative=Regular;
    fontFeatures=feat;
    fontColor=Color.black;
    font=f;
    mathsEnvironment=LMMath.default;
    mathStyle=Document.Mathematical.Text;
    size=fsize;
    lead=13./.10.*.fsize;
    normalMeasure=150.;
    normalLead=13./.10.*.fsize;
    normalLeftMargin=(fst env.normalPageFormat-.150.)/.2.;
    normalPageFormat=env.normalPageFormat;
    par_indent = [Drawing { drawing_min_width= 4.0 *. phi;
                            drawing_max_width= 4.0 *. phi;
                            drawing_y0=0.;drawing_y1=0.;
                            drawing_nominal_width= 4.0 *. phi;
                            drawing_width_fixed = true;
                            drawing_adjust_before = false;
                            drawing_contents=(fun _->[]);
                            drawing_break_badness=0.;
                            drawing_states=[];
                            drawing_badness=fun _-> 0. }];
    adjust_optical_alpha=3.1416 /. 4.;
    adjust_optical_beta=0.2;
    adjust_epsilon=5e-2;
    adjust_min_space=1./.9.;
  }


module MakeFormat (D:Document.DocumentStructure)
(Default : module type of DefaultFormat.Format(D)) =
  struct

    include Default

    let defaultEnv:environment=lmEnv Default.defaultEnv

end
