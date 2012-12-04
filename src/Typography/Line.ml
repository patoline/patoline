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
type line= {
  paragraph:int;                        (** L'indice du paragraphe dans le tableau *)
  lastFigure:int;                       (** La dernière figure placée, initialement -1 *)
  lineStart:int;                        (** Le numéro de la boite de début dans le paragraphe *)
  lineEnd:int;                          (** Le numéro de la boite suivant la dernière boite de la ligne, ou, si la ligne est césurée, le numéro de la boite contenant la césure *)
  hyphenStart:int;
  hyphenEnd:int;
  isFigure:bool;
  height:float;
  paragraph_height:int;
  page_line:int;
  page:int;
  min_width:float;
  nom_width:float;
  max_width:float;
  line_y0:float;
  line_y1:float
}

let uselessLine=
  { paragraph=0; lineStart= -1; lineEnd= -1; hyphenStart= -1; hyphenEnd= -1; isFigure=false;
    lastFigure=(-1); height= 0.;paragraph_height= -1; page_line= -1; page= -1;
    min_width=0.;nom_width=0.;max_width=0.;line_y0=infinity;line_y1= -.infinity }

let sprint_linef l=
  Printf.sprintf "{ paragraph=%d; lineStart=%d; lineEnd=%d; hyphenStart=%d; hyphenEnd=%d; lastFigure=%d; height=%f; page=%d; isFigure=%b }"
    l.paragraph l.lineStart l.lineEnd l.hyphenStart l.hyphenEnd l.lastFigure l.height l.page
    l.isFigure

let print_linef out l=Printf.fprintf out "%s\n" (sprint_linef l)
let print_line l=print_linef stderr l

type parameters={ measure:float;
                  page_height:float;
                  left_margin:float;
                  local_optimization:int;
                  next_acceptable_height:line->parameters->line->parameters->float->float;
                  min_height_before:float;
                  min_height_after:float;
                  min_page_before:int;
                  min_page_after:int;
                  not_last_line:bool;
                  not_first_line:bool;
                  min_lines_before:int;
                  min_lines_after:int;
                  absolute:bool
                }

let default_params={ measure=0.;
                     page_height=0.;
                     left_margin=0.;
                     local_optimization=0;
                     next_acceptable_height=(fun _ _ h _ _->h.height);
                     min_height_before=0.;
                     min_height_after=0.;
                     min_page_before=0;
                     min_page_after=0;
                     not_last_line=false;
                     not_first_line=false;
                     min_lines_before=1;
                     min_lines_after=0;
                     absolute=false
                   }
