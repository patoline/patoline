open Unicodelib
open FTypes

type subst =
  { original_glyphs : int array;
    subst_glyphs    : int array
  }

type chain =
  { before : (int list) array;
    input  : (int list) array;
    after  : (int list) array
  }

type substitution = Alternative of int array
                  | Subst of subst
                  | Chain of chain
                  | Context of (int * (substitution list)) array

open Format
open Printf
let print_int_array x=
  open_box 0;
  Printf.printf "[| ";
  for i=0 to Array.length x-1 do
    (if i=Array.length x-1 then Printf.printf "%d" else Printf.printf "%d; ") (x.(i))
  done;
  Printf.printf " |]";
  close_box ()

let print_int_list x=
  open_box 0;
  Printf.printf "[ ";
  let rec print=function
    | []   -> printf "]"
    | [h]  -> printf "%d ]" h
    | h::s -> (printf "%d; " h; print s)
  in
  print x;
  close_box ()

let print_subst = function
  | Alternative x -> printf "Alternative ";
                     print_int_array x;
                     printf "\n"
  | Subst x       -> printf "Subst { original_glyphs=";
                     open_box 1;
                     print_int_array x.original_glyphs;
                     close_box();
                     printf "; subst_glyphs=";
                     open_box 1;
                     print_int_array x.subst_glyphs;
                     close_box();
                     printf " }\n"
  | Chain _       -> printf "Chain { ... }\n"
  | Context _     -> printf "Context { ... }\n"

let apply_ligature lig glyphs0 =
  let rec apply_lig i buffer glyphs =
    if i >= Array.length lig.original_glyphs
    then
      ({ glyph_utf8=UTF8.Buf.contents buffer;
         glyph_index=lig.subst_glyphs.(0) }::glyphs)
    else
      match glyphs with
        | [] -> []
        | h::s when (h.glyph_index=lig.original_glyphs.(i)) ->
            (UTF8.Buf.add_string buffer h.glyph_utf8;
             apply_lig (i+1) buffer s)
        | _  -> []
  in
  let rec apply_all buf l =
    match l with
      | []   -> []
      | h::s -> (UTF8.Buf.clear buf;
                 match apply_lig 0 buf l with
                   | [] -> h::(apply_all buf s)
                   | l' -> apply_all (UTF8.Buf.create 2) l')
  in apply_all (UTF8.Buf.create 2) glyphs0


let apply_subst x glyphs0 =
  if Array.length x.original_glyphs = Array.length x.subst_glyphs
  then
    begin
      let rec replace a i =
        if i>=Array.length x.subst_glyphs
        then a
        else match a with
               | []   -> []
               | h::s -> {h with glyph_index = x.subst_glyphs.(i)}::(replace s (i+1))
      in
      let rec matches a i =
        i >= Array.length x.original_glyphs ||
        (match a with
           | []   -> false
           | h::s -> h.glyph_index = x.original_glyphs.(i) && matches s (i+1))
      in
      let rec apply_all l =
        match l with
          | []                   -> []
          | _ when (matches l 0) -> apply_all (replace l 0)
          | h::s                 -> h::(apply_all s)
      in apply_all glyphs0
    end
  else if Array.length x.subst_glyphs = 1
  then
    apply_ligature x glyphs0
  else
    begin
      let rec matches a i=
        if i >= Array.length x.original_glyphs
        then 
          (Array.to_list (Array.map (fun u->{ glyph_utf8=""; glyph_index=u }) x.subst_glyphs))@a
        else
          match a with
            | [] -> []
            | h::s when h.glyph_index = x.original_glyphs.(i) -> matches s (i+1)
            | _  ->[]
      in
      let rec apply_all l =
        match l with
          | []   -> []
          | h::s -> (match matches l 0 with
                       | [] -> h::apply_all s
                       | x  -> apply_all x)
      in apply_all glyphs0
    end

let apply_alternative alt i glyphs0 =
  List.map (fun x -> if x.glyph_index = alt.(0)
                     then { x with glyph_index = alt.(i) }
                     else x) glyphs0

let apply subst glyphs0 = 
  match subst with
    | Alternative _ -> glyphs0
    | Subst x       -> apply_subst x glyphs0
    | _             -> glyphs0
