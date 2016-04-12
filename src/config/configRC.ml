module Data =
  struct
    exception Mismatch

    type _ atom_type =
      | Int    : int    atom_type
      | Float  : float  atom_type
      | String : string atom_type
      | Bool   : bool   atom_type

    type _ data_type =
      | Atom   : 'a atom_type -> 'a        data_type
      | List   : 'a atom_type -> 'a list   data_type
      | Option : 'a atom_type -> 'a option data_type

    type _ data_aux =
      | Field : 'a data_type * 'a -> _ data_aux
    type t = unit data_aux
    type data = t

    let of_atom   ty e = Field(Atom(ty)  , e)
    let of_list   ty l = Field(List(ty)  , l)
    let of_option ty o = Field(Option(ty), o)
    
    let of_int    = of_atom Int
    let of_float  = of_atom Float
    let of_string = of_atom String
    let of_bool   = of_atom Bool

    let to_atom : type a. a atom_type -> t -> a = fun ty d ->
      match (ty, d) with
      | (Int   , Field(Atom(Int)   , e)) -> e
      | (Float , Field(Atom(Float) , e)) -> e
      | (String, Field(Atom(String), e)) -> e
      | (Bool  , Field(Atom(Bool)  , e)) -> e
      | _                                -> raise Mismatch

    let to_list : type a. a atom_type -> t -> a list = fun ty d ->
      match (ty, d) with
      | (Int   , Field(List(Int)   , e)) -> e
      | (Float , Field(List(Float) , e)) -> e
      | (String, Field(List(String), e)) -> e
      | (Bool  , Field(List(Bool)  , e)) -> e
      | _                                -> raise Mismatch

    let to_option : type a. a atom_type -> t -> a option = fun ty d ->
      match (ty, d) with
      | (Int   , Field(Option(Int)   , e)) -> e
      | (Float , Field(Option(Float) , e)) -> e
      | (String, Field(Option(String), e)) -> e
      | (Bool  , Field(Option(Bool)  , e)) -> e
      | _                                  -> raise Mismatch

    let to_int    = to_atom Int
    let to_float  = to_atom Float
    let to_string = to_atom String
    let to_bool   = to_atom Bool
  end

open Data

let print_atom : type a. a atom_type -> out_channel -> a -> unit =
  fun ty oc e ->
    let open Printf in
    match ty with
    | Int    -> fprintf oc "%i" e
    | Float  -> fprintf oc "%f" e
    | String -> fprintf oc "%S" e
    | Bool   -> fprintf oc "%s" (if e then "true" else "false")

let print_data : type a. a data_type -> out_channel -> a -> unit =
  fun ty oc e ->
    let open Printf in
    match ty with
    | Atom(ty)   -> print_atom ty oc e
    | List(ty)   -> fprintf oc "[";
                    begin
                      match e with
                      | []    -> ()
                      | x::xs -> fprintf oc " %a" (print_atom ty) x;
                                 let f = fprintf oc " ; %a" (print_atom ty) in
                                 List.iter f xs
                    end;
                    fprintf oc " ]"
    | Option(ty) -> begin
                      match e with
                      | None    -> ()
                      | Some(e) -> print_atom ty oc e
                    end

let atom_of_string : type a. a atom_type -> string -> a = fun ty s ->
  let slen = String.length s in
  match ty with
  | Int    -> int_of_string s
  | Float  -> float_of_string s
  | String -> if slen > 0 && s.[0] = '"' && s.[slen-1] = '"' then
                String.sub s 1 (slen - 2)
              else
                raise (Failure "string_of_string")
  | Bool   -> bool_of_string s

let data_of_string : type a. a data_type -> string -> a = fun ty s ->
  let slen = String.length s in
  let split_string c s =
    let slen = String.length s in
    let seps = ref [] in
    for i = 0 to slen - 1 do
      if s.[i] = c then seps := i :: !seps
    done;
    let seps = -1 :: (List.rev (slen :: !seps)) in
    let rec intervals acc = function
      | i :: j :: is -> intervals ((i,j)::acc) (j :: is)
      | _            -> List.rev acc
    in
    List.map (fun (i,j) -> String.sub s (i+1) (j-i-1)) (intervals [] seps)
  in
  match ty with
  | Atom(ty)   -> atom_of_string ty s
  | List(ty)   -> if slen > 2 && s.[0] = '[' && s.[slen-1] = ']' then
                    let s = String.sub s 1 (slen - 2) in
                    let l = split_string ';' s in
                    let l = (* In case of empty list. *)
                      match l with
                      | [e] when String.trim e = "" -> []
                      | _                           -> l
                    in
                    List.map (fun e -> atom_of_string ty (String.trim e)) l
                  else
                    raise (Failure "list_of_string")
  | Option(ty) -> if s = "" then None else Some(atom_of_string ty s)

module M = Map.Make(String)

module Config =
  struct
    type t = Data.t M.t
    type config = t

    let get : string -> config -> Data.t = M.find

    let write oc cfg =
      let mlen = M.fold (fun k _ m -> max m (String.length k)) cfg 0 in
      let prnt k (Field(ft, e)) =
        let k = k ^ (String.make (mlen - String.length k) ' ') in
        Printf.fprintf oc "%s = %a\n%!" k (print_data ft) e
      in
      M.iter prnt cfg

    let read cfg ic =
      (* Reading into a list of (line_num, key, value) *)
      let line_num = ref 0 in
      let cont = ref [] in
      try
        while true do
          let l = input_line ic in
          let len = String.length l in
          incr line_num;
          if len > 0 && l.[0] <> '#' then
          try
            let e = String.index l '=' in
            let k = String.trim (String.sub l 0 e) in
            let v = String.trim (String.sub l (e+1) (len - e - 1)) in
            cont := (!line_num, k, v) :: !cont
          with Not_found ->
            Printf.eprintf "[WARNING] Invalid line %i (ignored)\n%!" !line_num
        done; assert false (* unreachable *)
      with End_of_file ->
      (* Handling fields: ignoring unspecified fields and parsing data. *)
      let rec handle_fields cfg = function
        | (l, k, v) :: rest ->
            begin
              try
                let Field (ft,_) = M.find k cfg in
                let data = Field(ft, data_of_string ft v) in
                let cfg = M.add k data cfg in
                handle_fields cfg rest
              with Not_found ->
                Printf.eprintf "[WARNING] Undefined field %s" k;
                Printf.eprintf " line %i (ignored)\n%!" l;
                handle_fields cfg rest
            end
        | []                -> cfg
      in
      handle_fields cfg !cont
  end

module type Spec =
  sig
    val name : string
    val spec : (string * Data.t) list
    val path : string * string list
  end

module Make(S : Spec) =
  struct
    include S

    let default = List.fold_left (fun m (k,v) -> M.add k v m) M.empty spec

    let print_default oc = Config.write oc default

    let write_default fn =
      let oc = open_out fn in print_default oc; close_out oc

    let read_config fn =
      let ic = open_in fn in
      let cfg = Config.read default ic in
      close_in ic; cfg

    let get_config () =
      let files = List.rev (fst path :: snd path) in
      let rec find = function
        | f :: fs -> (try read_config f with Sys_error _ -> find fs)
        | []      -> default
      in find files
  end
