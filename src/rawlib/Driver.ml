open RawContent

type page =
  { mutable size     : float * float (* Page size, couple (width, height).*)
  ; mutable contents : raw list }    (* Raw contents of the page. *)

let empty_page size = { size ; contents = [] }

let make_mage : float * float -> raw list -> page = fun size contents ->
  { size; contents }

type meta_field =
  | Contributor | Coverage | Creator | Date | Description | Format
  | Identifier | Language | Publisher | Relation | Rights | Source
  | Subject | Title | Type

type structure =
  { mutable name     : string
  ; mutable metadata : (meta_field * string) list
  ; mutable raw_name : raw list
  ; mutable tags     : (string * string) list
  ; mutable page     : int
  ; mutable struct_x : float
  ; mutable struct_y : float
  ; mutable children : structure array }

let empty_structure =
  { name     = ""
  ; raw_name = []
  ; metadata = []
  ; tags     = []
  ; page     = -1
  ; struct_x = 0.0
  ; struct_y = 0.0
  ; children = [||] }

module type OutputDriver = sig
  val output  : ?structure:structure -> page array       -> string -> unit
  val output' : ?structure:structure -> page array array -> string -> unit
end

let print_structure s = 
  let rec fn lvl s =
    for i = 0 to lvl do 
      Printf.printf " ";
    done;
    Printf.printf "%s\n" s.name;
    Array.iter (fn (lvl+1)) s.children
  in
  fn 0 s;
  flush stdout

let output_to_prime (output:(?structure:structure -> 'a array -> 'b -> 'c))
    ?(structure:structure=empty_structure) pages fileName =
  let n=Array.fold_left (fun n x->n+Array.length x) 0 pages in
  let pages'=Array.make n (empty_page (0.0,0.0)) in
  let corr=Array.init (Array.length pages)
    (fun i->Array.make (Array.length pages.(i)) 0)
  in
  let rec translate_page_numbers i j k=
    if i<Array.length pages then (
      if j<Array.length pages.(i) then (
        corr.(i).(j)<-k;
        translate_page_numbers i (j+1) (k+1)
      ) else translate_page_numbers (i+1) 0 k
    )
  in
  translate_page_numbers 0 0 0;
  let rec translate_pages i j k=
    if i<Array.length pages then (
      if j>=Array.length pages.(i) then
        translate_pages (i+1) 0 k
      else (
        pages'.(k)<-
          { pages.(i).(j) with
            contents=
              List.map (fun x->match x with
                  Link l->(
                    match l.link_kind with
                        Intern (a,b,c,d)->
                          if b>=0 && b<Array.length corr then
                            if Array.length corr.(b)>0 then
                              Link { l with link_kind=Intern (a,corr.(b).(0),c,d) }
                            else
                              x
                          else
                            x
                      | _->x
                  )
                | y->y
              ) (pages.(i).(j).contents)
          };
        translate_pages i (j+1) (k+1)
      )
    )
  in
  translate_pages 0 0 0;
  let rec translate_struct str=
    if str.page>=0 && str.page<Array.length corr then
      if Array.length corr.(str.page)>0 then
        str.page<-corr.(str.page).(0);
    Array.iter translate_struct str.children
  in
  translate_struct structure;
  output ~structure pages' fileName


let output_from_prime (output:(?structure:structure -> 'a array -> 'b -> 'c))
    ?(structure:structure=empty_structure) pages fileName =
   output ~structure (Array.map (fun x -> [|x|]) pages) fileName

let input_bin = ref None
let driver    = ref None
