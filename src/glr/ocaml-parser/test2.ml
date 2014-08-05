exception Invalid_arg of string * string
exception Invalid_arg2 = Invalid_arg

exception Piouf = Pervasives.Exit
exception Piaf = Not_found

type abstract

type bool_ = True
           | False

type 'a list_ = Nil
              | Cons of 'a * 'a list_

type 'a tree = Empty
             | Node of 'a * 'a tree * 'a tree

type inttree = int tree

type coord = { x : int ; y : int ; z : int }

type ('a, 'b) pierce = (('a -> 'b) -> 'a) -> 'a

type ('a, 'b) mod_pon = ('a -> 'b) -> 'a -> 'b

type +'a blop = Blop of 'a

type dummy = { mutable c : int; d : string -> bool }

