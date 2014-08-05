type abstract

type bool_ = True
           | False

type `a list_ = Nil
              | Cons of `a * `a list_

type `a tree = Empty
             | Node of `a * `a tree * `a tree

type inttree = int tree

type coord = { x : int ; y : int ; z : int }

type (`a, `b) pierce = ((`a -> `b) -> `a) -> `a

type (`a, `b) mod_pon = (`a -> `b) -> `a -> `b
