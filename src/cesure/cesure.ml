open Str
open Typography.Hyphenate

let _=
  for i=1 to Array.length Sys.argv-1 do
    let patterns,hyphenations=
      let i=open_in_bin Sys.argv.(i) in
      let rec find_pats seek_patterns seek_hyph p e=
        let pos0=pos_in i in
        match (try Some (input_line i) with _->None) with
            None->p,e
          | Some str->(

            let str=try let i=String.index str '%' in String.sub str 0 i with Not_found->str in

            let reg=(Str.regexp "\\\\patterns{") in
            let exc=(Str.regexp "\\\\hyphenation{") in
            let regend=(Str.regexp "}") in
            if Str.string_match reg str 0 then (
              let pos1=Str.search_forward reg str 0 in
              seek_in i (pos0+pos1+String.length (Str.matched_string str));
              find_pats true false p e
            ) else (
              if Str.string_match exc str 0 then (
                let pos1=Str.search_forward exc str 0 in
                seek_in i (pos0+pos1+String.length (Str.matched_string str));
                find_pats false true p e
              ) else (
                let seek_patterns',seek_hyph',str=
                  if Str.string_match regend str 0 then (
                    let pos1=Str.search_forward regend str 0 in
                    seek_in i (pos0+pos1+String.length (Str.matched_string str));
                    false, false, String.sub str 0 pos1
                  ) else seek_patterns, seek_hyph, str
                in
                let next_p=
                  if seek_patterns then (
                    let s=List.filter (fun s->String.length s>0) (split (regexp "[\n\t ]") str) in
                    (* List.iter (Printf.fprintf stderr "pat %S\n") s; *)
                    s@p
                  ) else p
                in
                let next_e=
                  if seek_hyph then (
                    let s=List.filter (fun s->String.length s>0) (split (regexp "[\n\t ]") str) in
                    let ss=(List.map (Str.split (Str.regexp "-")) s) in
                    (* List.iter (fun s->Printf.fprintf stderr "hyph : ["; *)
                    (*   List.iter (Printf.fprintf stderr "%s ")s; *)
                    (*   Printf.fprintf stderr "]\n"; *)
                    (* ) ss; *)
                    ss@e
                  ) else e
                in
                find_pats seek_patterns' seek_hyph' next_p next_e
              )
            )
          )
      in
      let p,e=find_pats false false [] [] in
      close_in i;
      p,e
    in
    let tree0 = List.fold_left insert empty patterns in
    let tree = List.fold_left insert_exception tree0 hyphenations in
    let o=open_out ((try Filename.chop_extension Sys.argv.(i)
      with _->Sys.argv.(i))^".hdict")
    in
    output_value o tree;
    close_out o
  done
