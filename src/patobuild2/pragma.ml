type pragma = string * string option

let pragma_from_line : string -> pragma option = fun l ->
  let open String in
  let l = trim l in
  let len = length l in
  if len < 4 || sub l 0 2 <> "(*" || sub l (len - 2) 2 <> "*)" then None else
  let l = trim (sub l 2 (len - 4)) in
  let len = length l in
  if l.[0] <> '#' || len < 3 then None else
  if not (contains l ' ') then Some (String.sub l 1 (len - 1), None) else
  let i = index l ' ' in
  let k = String.sub l 1 (i-1) in
  let v = trim (String.sub l (i+1) (len-i-1)) in
  if contains v ' ' then None else Some (k, Some v)

let pragma_from_file : string -> pragma list = fun fn ->
  let ic = open_in fn in
  let ps = ref [] in
  begin
    try while true do
      let l = input_line ic in
      match pragma_from_line l with
      | None   -> ()
      | Some p -> ps := p :: !ps
    done with End_of_file -> ()
  end;
  close_in ic; !ps
