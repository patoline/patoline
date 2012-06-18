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
  max_width:float
}

let uselessLine=
  { paragraph=0; lineStart= -1; lineEnd= -1; hyphenStart= -1; hyphenEnd= -1; isFigure=false;
    lastFigure=(-1); height= 0.;paragraph_height= -1; page_line=0; page=0;
    min_width=0.;nom_width=0.;max_width=0. }

let sprint_linef l=
  Printf.sprintf "{ paragraph=%d; lineStart=%d; lineEnd=%d; hyphenStart=%d; hyphenEnd=%d; lastFigure=%d; height=%f; page=%d }"
    l.paragraph l.lineStart l.lineEnd l.hyphenStart l.hyphenEnd l.lastFigure l.height l.page

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
                  really_next_line:bool
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
                     really_next_line=true
                   }
