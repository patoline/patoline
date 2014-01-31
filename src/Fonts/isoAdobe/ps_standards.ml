module IntMap = Map.Make (struct type t = int let compare = compare end)

let _ =
  let i = open_in Sys.argv.(1) in
  let rec make_iso sid encoding sids =
    let x = try Some (input_line i) with End_of_file -> None in
    match x with
      | None   -> (encoding, sids)
      | Some x -> (
          let li=(Str.regexp "\\([0-9A-F]+\\)[ \t]+[^ \t]*[ \t]+\\([^ \t]+\\)[ \t].*") in
          if Str.string_match li x 0
          then
            (let a=Str.matched_group 1 x in
             let b=Str.matched_group 2 x in
             let rec parse_hex s i n=if i>=String.length s then n else
                 if s.[i]>='A' && s.[i]<='F' then
                   parse_hex s (i+1) (n*16+int_of_char s.[i]-int_of_char 'A'+10)
                 else
                   parse_hex s (i+1) (n*16+int_of_char s.[i]-int_of_char '0')
             in
             let enc=parse_hex a 0 0 in
             make_iso (sid+1) (IntMap.add enc b encoding) (IntMap.add sid b sids))
          else make_iso sid encoding sids
      )
  in
  let encodings, sids = make_iso 1 IntMap.empty IntMap.empty in
  close_in i;

  let o = open_out Sys.argv.(2) in
  Printf.fprintf o "let name_of_unicode = [|";
  for i = 0 to 0xff do
    if i > 0 then Printf.fprintf o ";";
    Printf.fprintf o "%S" (try IntMap.find i encodings with Not_found -> "")
  done;
  Printf.fprintf o "|]\n";

  let sids = Array.of_list (List.map fst (IntMap.bindings encodings)) in
  Printf.fprintf o "let unicode_of_cid = [|";
  for i = 0 to Array.length sids - 1 do
    if i > 0 then Printf.fprintf o ";";
    Printf.fprintf o "%d" sids.(i)
  done;
  Printf.fprintf o "|]\n";
  let sids = Array.of_list (List.filter (fun x -> x <= 0xff)
                           ((List.map fst (IntMap.bindings encodings)))) in
  Printf.fprintf o "let ascii_of_cid = [|";
  for i = 0 to Array.length sids - 1 do
    if i > 0 then Printf.fprintf o ";";
    Printf.fprintf o "%d" sids.(i)
  done;
  Printf.fprintf o "|]\n";

(*
  Printf.fprintf o "let char_of_cid = [|";
  for i = 1 to (IntMap.cardinal encodings) - 1 do
    if i > 1 then Printf.fprintf o ";";
    Printf.fprintf o "%d" (try IntMap.find i encodings with _->0)
  done;
  Printf.fprintf o "|]\n";
  Printf.fprintf o "let cid_of_char=[|";
  let rev = IntMap.fold (fun k a m -> IntMap.add a k m) encodings IntMap.empty in
  for i = 0 to 0xff do
    if i > 0 then Printf.fprintf o ";";
    Printf.fprintf o "%d" (try IntMap.find i rev with _ -> 0)
  done;
  Printf.fprintf o "|]\n";
*)

  close_out o;
