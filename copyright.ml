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

let license="(*
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
"

let _=
  for i=1 to Array.length Sys.argv-1 do
    let fi=open_in Sys.argv.(i) in
    try
      let c0=input_char fi in
      let c1=input_char fi in
      let str=String.create 1000 in
      let offset=
        if c0='(' && c1='*' then (
          let rec find_comment_end ls j=
            if j>=ls-1 then (
              let ls'=input fi str 0 (String.length str) in
              if ls'=0 then 0 else
                find_comment_end ls' 0
            ) else (
              if str.[j]='*' && str.[j+1]=')' then j+3 else
                find_comment_end ls (j+1)
            )
          in
          seek_in fi 0;
          let ls=input fi str 0 (String.length str) in
          find_comment_end ls 0
        ) else 0
      in
      Printf.fprintf stderr "%d\n" offset;
      seek_in fi offset;
      let fo_=Printf.sprintf "%s.tmp" Sys.argv.(i) in
      let fo=open_out fo_ in
      let _=output fo license 0 (String.length license) in
      let str=String.create 1000 in
      let rec copy ()=
        let x=input fi str 0 (String.length str) in
        if x>0 then (
          output fo str 0 x;
          copy ()
        )
      in
      copy ();
      close_in fi;
      close_out fo;
      Unix.rename fo_ Sys.argv.(i)
    with
        Unix.Unix_error (a,b,c)->Printf.fprintf stderr "%s\n" (Unix.error_message a)
      | End_of_file->close_in fi
  done
