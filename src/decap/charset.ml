(*
  ======================================================================
  Copyright Christophe Raffalli & Rodolphe Lepigre
  LAMA, UMR 5127 - Université Savoie Mont Blanc

  christophe.raffalli@univ-savoie.fr
  rodolphe.lepigre@univ-savoie.fr

  This software contains implements a parser combinator library together
  with a syntax extension mechanism for the OCaml language.  It  can  be
  used to write parsers using a BNF-like format through a syntax extens-
  ion called pa_parser.

  This software is governed by the CeCILL-B license under French law and
  abiding by the rules of distribution of free software.  You  can  use,
  modify and/or redistribute it under the terms of the CeCILL-B  license
  as circulated by CEA, CNRS and INRIA at the following URL:

            http://www.cecill.info

  The exercising of this freedom is conditional upon a strong obligation
  of giving credits for everybody that distributes a software incorpora-
  ting a software ruled by the current license so as  all  contributions
  to be properly identified and acknowledged.

  As a counterpart to the access to the source code and rights to  copy,
  modify and redistribute granted by the  license,  users  are  provided
  only with a limited warranty and the software's author, the holder  of
  the economic rights, and the successive licensors  have  only  limited
  liability.

  In this respect, the user's attention is drawn to the risks associated
  with loading, using, modifying and/or developing  or  reproducing  the
  software by the user in light of its specific status of free software,
  that may mean that it is complicated  to  manipulate,  and  that  also
  therefore means that it is reserved  for  developers  and  experienced
  professionals having in-depth computer knowledge. Users are  therefore
  encouraged to load and test  the  software's  suitability  as  regards
  their requirements in conditions enabling the security of  their  sys-
  tems and/or data to be ensured and, more generally, to use and operate
  it in the same conditions as regards security.

  The fact that you are presently reading this means that you  have  had
  knowledge of the CeCILL-B license and that you accept its terms.
  ======================================================================
*)

type charset = int array

let used, mask, shift, size =
  match Sys.word_size with
  | 32 -> -1, 15, 4, 256 / 16
  | 64 -> -1, 31, 5, 256 / 32
  | _  -> assert false (* Cannot happen... *)

let empty_charset = Array.make size 0
let full_charset  = Array.make size used
 
let mem cs c =
  let i = Char.code c in
  cs.(i lsr shift) land (1 lsl (i land mask)) <> 0

let addq cs c =
  let i = Char.code c in
  cs.(i lsr shift) <- cs.(i lsr shift) lor (1 lsl (i land mask))

let add cs c =
  let i = Char.code c in
  let cs = Array.copy cs in
  cs.(i lsr shift) <- cs.(i lsr shift) lor (1 lsl (i land mask));
  cs

let delq cs c =
  let i = Char.code c in
  cs.(i lsr shift) <- cs.(i lsr shift) land (lnot (1 lsl (i land mask)))

let del cs c =
  let i = Char.code c in
  let cs = Array.copy cs in
  cs.(i lsr shift) <- cs.(i lsr shift) land (lnot (1 lsl (i land mask)));
  cs
  
let union cs1 cs2 = 
  Array.mapi (fun i x -> x lor cs2.(i)) cs1

let singleton =
  let tbl = Array.init 256 (fun i -> add empty_charset (Char.chr i)) in
  fun c -> tbl.(Char.code c)

let copy = Array.copy

let list_of_charset cs =
  let res = ref [] in
  for i = 0 to 255 do
    let c = Char.chr i in
    if mem cs c then res := Char.escaped c :: !res
  done;
  !res

let print_charset oc cs =
  if cs = full_charset then output_string oc "<FULL>"
  else begin
    output_string oc "{";
    let has_range min max =
      let has_all = ref true in
      for i = (Char.code min) to (Char.code max) do
        if not (mem cs (Char.chr i)) then has_all := false
      done;
      !has_all
    in
    let print_all min max =
      for i = min to max do
        if mem cs (Char.chr i) then
          output_string oc (Char.escaped (Char.chr i))
      done
    in
    let has_all_nums  = has_range '0' '9' in
    let has_all_upper = has_range 'A' 'Z' in
    let has_all_lower = has_range 'a' 'z' in
    (* Before character '0' *)
    print_all 0 (Char.code '0' - 1);
    (* Numbers. *)
    if has_all_nums then output_string oc "0-9"
    else print_all (Char.code '0') (Char.code '9');
    (* Before character 'A' *)
    print_all (Char.code '9' + 1) (Char.code 'A' - 1);
    (* Uppercase letters. *)
    if has_all_upper then output_string oc "A-Z"
    else print_all (Char.code 'A') (Char.code 'Z');
    (* Before character 'a' *)
    print_all (Char.code 'Z' + 1) (Char.code 'a' - 1);
    (* Lowercase letters. *)
    if has_all_lower then output_string oc "a-z"
    else print_all (Char.code 'a') (Char.code 'z');
    (* After character 'z'. *)
    print_all (Char.code 'z' + 1) 255;
    output_string oc "}"
  end

let print_raw_charset oc cs =
  output_string oc "{";
  for i = 0 to 255 do
    if mem cs (Char.chr i) then output_string oc (Char.escaped (Char.chr i))
  done;
  output_string oc "}"

type char_tree = Any | Leaf of charset | Node of char_tree array
