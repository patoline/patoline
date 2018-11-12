let _ =
  let normal = Str.regexp "\\([0-9]*\\)\t[^\t]*\t\\([0-9A-F]*\\)-\\([0-9A-F]*\\)" in
  let suite = Str.regexp "[ \t]*[^\t]*\t\\([0-9A-F]*\\)-\\([0-9A-F]*\\)" in
  let m0 = ref max_int in
  let m1 = ref 0 in
  let i = open_in Sys.argv.(1) in
  let rec read_all last ranges =
    try
      let l = input_line i in
      if Str.string_match normal l 0 then (
        let bit = Str.matched_group 1 l in
        let a = Scanf.sscanf (Str.matched_group 2 l) "%X" (fun x -> x) in
        let b = Scanf.sscanf (Str.matched_group 3 l) "%X" (fun x -> x) in
        m0 := min !m0 a;
        m1 := max !m1 b;
        read_all (int_of_string bit) ((int_of_string bit,a,b)::ranges)
      ) else (
        if Str.string_match suite l 0 && last >= 0 then (
          let a = Scanf.sscanf (Str.matched_group 1 l) "%X" (fun x -> x) in
          let b = Scanf.sscanf (Str.matched_group 2 l) "%X" (fun x -> x) in
          m0 := min !m0 a;
          m1 := max !m1 b;
          read_all last ((last,a,b)::ranges)
        ) else (
          read_all last ranges
        )
      )
    with End_of_file -> ranges
  in
  let ranges = List.rev (read_all (-1) []) in
  close_in i;

  let sorted = List.sort (fun (_,a,_) (_,b,_)->compare a b) ranges in
  let split n l =
    let fin = ref [] in
    let rec take n l =
      match l with
        | h::s when n>0 -> h::(take (n-1) s)
        | _             -> (fin:=l; [])
    in
    let x = take n l in
    (x, !fin)
  in

  let file = open_out Sys.argv.(2) in
  let rec make_program x y l =
    match l with
      |  []     -> ()
      | [a,b,c] ->
          let bit = (a mod 32) in
          let test =
            if b = x && c = y
            then ""
            else if b = x
            then Printf.sprintf "if k<=%d then " c
            else if c = y
            then Printf.sprintf "if k>=%d then " b
            else Printf.sprintf "if k>=%d && k<=%d then " b c
          in
          Printf.fprintf file "%su%d:= Int32.logor !u%d (%sl);\n"
            test (a/32) (a/32) (Int32.to_string (Int32.shift_left 1l bit))
      | l       ->
          let n = List.length l in
          let a, b = split (n/2) l in
          match b with
            | []         -> make_program x y a
            | (_,v,_)::_ -> (Printf.fprintf file "if k>=%d then (\n" v;
                             make_program v y b;
                             Printf.fprintf file ") else (\n";
                             make_program x v a;
                             Printf.fprintf file ")")
  in
  Printf.fprintf file "let unicode_range u0 u1 u2 u3 k=\n";
  make_program (-1) max_int sorted;
  Printf.fprintf file "\n";
  close_out file
