open Str

module C=(*struct
  type 'a t=(char*'a) list
  let empty=[]
  let rec add a b c=match c with
      []->[(a,b)]
    | (u,_)::s when a=u->(u,b)::s
    | h::s->h::(add a b s)
  let singleton a b=[a,b]
  let rec find a b=match b with
      []->raise Not_found
    | (u,v)::s when u=a -> v
    | _::s->find a s
end*)New_map.Make (struct type t=char let compare=compare end)

type ptree=
  Node of (char array)*(ptree C.t)
  | Exception of (string list)*(ptree C.t)

let is_num c = c>='0' && c<='9'

let insert tree a=
  let breaks0=Array.make (String.length a) '0' in
  let j=ref 0 in
    for i=0 to String.length a-1 do
      if is_num a.[i] then breaks0.(!j)<-a.[i] else
        incr j
    done;
    let breaks=Array.sub breaks0 0 (!j+1) in
    let rec insert i tree=
      if i>=String.length a then Node (breaks, C.empty) else
        if is_num a.[i] then insert (i+1) tree else
          (match tree with
             Node (x,t)->
               (let tree'=try C.find a.[i] t with Not_found->Node ([||], C.empty) in
                  Node (x, C.add a.[i] (insert (i+1) tree') t))
             | Exception (x,t)->
                 (let tree'=try C.find a.[i] t with Not_found->Node ([||], C.empty) in
                    Exception (x, C.add a.[i] (insert (i+1) tree') t))
          )
    in
      insert 0 tree

let insert_exception tree a0=
  let a="."^(List.fold_left (^) "" a0)^"." in

  let rec insert i = function

      Exception (_,_) as t when i>=String.length a-1 -> t
    | Exception (x,t)->
        (
          let t'=try C.find a.[i] t with Not_found->Node ([||], C.empty) in
            Exception (x, C.add a.[i] (insert (i+1) t') t)
        )
    | Node (x,t) when i>=String.length a-1 -> Exception (a0,t)
    | Node (x,t)->
        (
          let t'=try C.find a.[i] t with Not_found->Node ([||], C.empty) in
            Node (x, C.add a.[i] (insert (i+1) t') t)
        )
  in
    insert 0 tree


exception Exp of (string list)

let hyphenate a0 tree =
  if String.length a0<=4 then [a0] else

    let a=String.create (String.length a0+2) in
    String.blit a0 0 a 1 (String.length a0);
      a.[0]<-'.';
      a.[String.length a-1]<-'.';
      let breaks=Array.create (String.length a+1) '0' in
      let rec hyphenate i j t=if j>=String.length a then () else match t with
        | Exception (x,_) when i=0 && j=String.length a-1->raise (Exp x)
        | Exception (_,t)->
            (
             try
               let t'=C.find a.[j] t in
                 hyphenate i (j+1) t'
             with
                 Not_found->())
        | Node (x,t) -> (
            if Array.length x>0 then (
              for k=0 to Array.length x-1 do
                breaks.(i+k)<-max breaks.(i+k) x.(k)
              done);
            try
              let t'=C.find a.[j] t in
                hyphenate i (j+1) t'
            with
                Not_found->()
          )
      in
        for i=0 to String.length a-1 do
          hyphenate i i tree;
        done;

        let rec make_hyphens i j=
          if j>=String.length a-2 then [String.sub a i (j-i+1)] else
            if (int_of_char breaks.(j+1)-int_of_char '0') mod 2 = 1 then
              (String.sub a i (j-i+1)) :: make_hyphens (j+1) (j+1)
            else
              make_hyphens i (j+1)
        in
          make_hyphens 1 1

let empty=Node ([||], C.empty)

(* let patterns= *)
(*   let i=open_in "patterns" in *)
(*   let str=String.create (in_channel_length i) in *)
(*     really_input i str 0 (in_channel_length i); *)
(*     let s=split (regexp "[\n\t ]") str in *)
(*       close_in i; *)
(*       s *)

(* let tree0 = List.fold_left insert (Node ([||],C.empty)) patterns *)

(* let tree = List.fold_left insert_exception tree0 *)
(*   [["as";"so";"ciate"]; ["as";"so";"ciates"]; ["dec";"li";"na";"tion"]; *)
(*    ["oblig";"a";"tory"]; ["phil";"an";"thropic"]; ["present"]; ["presents"]; *)
(*    ["project"]; ["projects"]; ["reci";"procity"]; ["re";"cog";"ni";"zance"]; *)
(*    ["ref";"or";"ma";"tion"]; ["ret";"ri";"bu";"tion";"ta";"ble"]] *)

(* let _= *)
(*   let o=open_out "dict_en" in *)
(*     output_value o tree; *)
(*     close_out o *)

(* let tree= *)
(*   let i=open_in "dict_en" in *)
(*   let inp=input_value i in *)
(*     close_in i; *)
(*     inp *)
(* let _=let str="associated" in hyphenate str tree *)
