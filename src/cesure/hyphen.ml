open UChar
open UCharInfo

type ptree =
  | Node of string      * ptree UMap.t
  | Exce of string list * ptree UMap.t

let empty = Node ("", UMap.empty)

let find u m =
  try UMap.find u m
  with Not_found -> empty

let is_num c =
  c >= int_of_char '0' && c <= int_of_char '9'

let insert tree a =
  let breaks0 = Bytes.make (String.length a) '0' in
  let j = ref 0 in

  let rec fill_breaks i =
    if i < String.length a then
      let c = code (UTF8.look a i) in
      if is_num c then begin
        Bytes.set breaks0 (i- !j) (char_of_int (c - int_of_char '0'));
        incr j
      end;
      fill_breaks (UTF8.next a i)
  in
  fill_breaks 0;

  let breaks = Bytes.sub breaks0 0 (Bytes.length breaks0 - !j) in
  let breaks = Bytes.to_string breaks in

  let rec insert i tree =
    if i >= String.length a then
      match tree with
      | Node (_,t) -> Node (breaks, t)
      | _          -> tree (* Pattern is already an exception *)
    else if is_num (code (UTF8.look a i)) then
      insert (UTF8.next a i) tree
    else
      match tree with
      | Node (x,t) ->
        let tree' = find (UTF8.look a i) t in
        Node (x, UMap.add (UTF8.look a i) (insert (UTF8.next a i) tree') t)
      | Exce (x,t) ->
        let tree' = find (UTF8.look a i) t in
        Exce (x, UMap.add (UTF8.look a i) (insert (UTF8.next a i) tree') t)
  in
  insert 0 tree

let insert_exception tree a0 =
  let a = "." ^ (List.fold_left (^) "" a0) ^ "." in
  let rec insert i t =
    if i >= String.length a - 1 then
      match t with
      | Exce (_,_) -> t
      | Node (_,t) -> Exce (a0,t)
    else
      match t with
      | Exce (x,t) ->
        let t' = find (UTF8.look a i) t in
        Exce (x, UMap.add (UTF8.look a i) (insert (UTF8.next a i) t') t)
      | Node (x,t) ->
        let t' = find (UTF8.look a i) t in
        Node (x, UMap.add (UTF8.look a i) (insert (UTF8.next a i) t') t)
  in
  insert 0 tree

let rec dash_hyphen s i acc =
  if i >= String.length s then acc
  else begin
    let (j, next) =
      try
        let i' = String.index_from s i '-' in
        if i' >= String.length s - 1 then
          (String.length s, acc)
        else
          let s0 = String.sub s 0 (i' + 1) in
          let s1 = String.sub s (i' + 1) (String.length s - i' - 1) in
          (i' + 1, (s0,s1)::acc)
      with Not_found -> (String.length s, acc)
    in
    dash_hyphen s j next
  end

let hyphenate tree a0 =
  if UTF8.length a0 <= 4 then [] else
  let rec find_punct i =
    if i >= String.length a0 then i else
    match general_category (UTF8.look a0 i) with
    | Cc | Cf | Cn | Co | Cs (* | Mc | Me | Mn *)
    | Pc | Pd | Pe | Pf | Pi | Po | Ps | Zl | Zp | Zs -> i
    | _ -> find_punct (UTF8.next a0 i)
  in
  let first_punct = find_punct 0 in
  let l = dash_hyphen a0 0 [] in
  if List.length l >= 1 then l else
  let a = Bytes.create (first_punct+2) in
  String.blit a0 0 a 1 first_punct;
  Bytes.set a 0 '.';
  Bytes.set a (Bytes.length a - 1) '.';
  let a = Bytes.to_string a in
  let breaks = Bytes.make (String.length a + 1) (char_of_int 0) in

  let rec hyphenate i j t =
    if j <= String.length a then
      match t with
      | Exce (x,_) when i = 0 && j = String.length a - 1 -> ()
      | Exce (_,t) ->
        (try
           let t' = UMap.find (UTF8.look a j) t in
           hyphenate i (UTF8.next a j) t'
         with _ -> ())
      | Node (x,t) ->
        if String.length x > 0 then begin
          let rec fill_breaks k =
            Bytes.set breaks (i+k) (max (Bytes.get breaks (i+k)) x.[k]);
            fill_breaks (UTF8.next a (i+k)-i)
          in fill_breaks 0
        end;
        try
          let t' = UMap.find (UTF8.look a j) t in
          hyphenate i (UTF8.next a j) t'
        with _ -> ()
  in

  let rec hyphenate_word i =
    if i < String.length a then begin
      hyphenate i i tree;
      hyphenate_word (UTF8.next a i)
    end
  in
  hyphenate_word 0;

  let total = UTF8.length a in

  let rec make_hyphens j k =
    if j >= String.length a || j + 1 >= Bytes.length breaks || total - k < 6 then []
    else begin
      if (int_of_char (Bytes.get breaks (j + 1))) land 1 = 1 && k >= 3 then
        let j' = UTF8.next a j - 1 in
        (String.sub a0 0 j' ^ "-", String.sub a0 j' (String.length a0 - j'))
          :: make_hyphens (UTF8.next a j) (k + 1)
      else make_hyphens (UTF8.next a j) (k + 1)
    end
  in
  make_hyphens 1 0
