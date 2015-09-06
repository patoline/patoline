
open Classical

let classical (fib:int => int) n =
  if n <= 1 then
    1
  else
    let f1 = fib (n-1) in
    let f2 = fib (n-2) in
    (f1 + f2)

let n = 25
let _ = Printf.printf "fib(%d) = %d\n%!" n (?fib n?)

type ('a, 'b) sum = Inl of 'a | Inr of 'b

let print_list ch l = List.iter (Printf.fprintf ch "%d ") l

let print_sum f g ch = function Inl l -> Printf.fprintf ch "Inl(%a)%!" f l
                              | Inr l -> Printf.fprintf ch "Inr(%a)%!" g l

type 'a stream = unit => ('a * 'a stream)

let classical (init_aux:(int -> 'a) -> int -> 'a stream) f < n < () =
  let h = (!f n!) in
  let tail = fun () -> init_aux f < (!n+1!) < () in
  (h, tail)

let classical (init:(int -> 'a) -> 'a stream) f < () = init_aux f < 0 < ()

let classical (extract :int -> 'a stream => 'a list) n < s =
  if n = 0 then [] else
    let (h,t) = s () in
    let l = extract (n - 1) < t in
    (h :: l)

let classical (nats : int stream) () = init (!fun n -> n!) < ()

let classical (test1 : int => int list) n = extract n < nats

let test1 = Printf.printf "%a\n" print_list (?test1 15?)

let classical (extract_sum : int -> int -> ('a stream, 'b stream) sum =>  ('a list, 'b list) sum) n < m < s =
  match s with
  | Inl s -> let l = extract n < s in (!Inl l!)
  | Inr s -> let l = extract m < s in (!Inr l!)

let classical (tT : ('a, 'b) sum stream -> ('a * 'a stream) neg -> ('b * 'b stream) neg => 'c) s < k1 < k2 =
  let (a, t) = s () in
  match a with
  | Inl x1 -> [k1] (let t = fun () -> mu k1p -> tT t < k1p < k2 in (x1, t))
  | Inr x2 -> [k2] (let t = fun () -> mu k2p -> tT t < k1 < k2p in (x2, t))

let classical (infinite_tape : ('a, 'b) sum stream => ('a stream, 'b stream) sum) s =
  mu a -> let r1 = fun () -> mu k1 ->
		   [a] (let r2 = fun () -> mu k2 ->
				tT s < k1 < k2
		       in (!Inr r2!))
	  in (!Inl r1!)

let classical test2 n < m < f = extract_sum n < m < (infinite_tape (!init f!))

let f1 n = if n mod 7 <> 1 then begin Inl n end else begin Inr n end
let f2 n = if n mod 7 = 1 then begin Inl n end else begin Inr n end

let _ = Printf.printf "%a\n" (print_sum print_list print_list) (?test2 15 < 15 < f1 ?)
let _ = Printf.printf "%a\n" (print_sum print_list print_list) (?test2 150 < 15 < f1 ?)
let _ = Printf.printf "%a\n" (print_sum print_list print_list) (?test2 15 < 15 < f2 ?)
let _ = Printf.printf "%a\n" (print_sum print_list print_list) (?test2 15 < 150 < f2 ?)

type 'a color1 =  'a * 'a stream => ('a stream, 'a stream) sum

let classical (tT2 : 'a color1 -> 'a stream -> ('a * 'a stream) neg -> ('a * 'a stream) neg => 'c) f < s < k1 < k2 =
  let (a, t as s') = s () in
  let b = f s' in
  match b with
  | Inl t -> [k1] (let t' = (fun () -> mu k1p -> tT2 f < t < k1p < k2) in (a, t'))
  | Inr t -> [k2] (let t' = (fun () -> mu k2p -> tT2 f < t < k1 < k2p) in (a, t'))

let classical (infinite_tape2 : 'a color1 -> 'a stream => ('a stream, 'b stream) sum) f < s =
  mu a ->
    let r2 () = mu k2 ->
      [a] (let r1 () = mu k1 ->
	     tT2 f < s < k1 < k2
	   in (!Inl r1!))
    in (!Inr r2!)

type 'a colors = 'a list -> 'a stream => ('a stream, 'a stream) sum

let classical ramsey : (int -> 'a colors -> 'a stream => ('a stream, 'a stream) sum) k < color < s =
  if k = 1 then
    let (color1:'a color1) (n,s) = color [n] < s in
    infinite_tape2 color1 < s
  else
    let (color1:'a color1) (n,s) =
      let (color2:'a colors) l < s = color (n::l) < s in
      ramsey (k-1) < color2 < s
    in
    infinite_tape2 color1 < s

let rec hash l =
  match l with
  | [] -> 0
  | n::l -> (3571 * n + 1987 * hash l) mod 16384

let classical (color_hash : int list -> int stream => (int stream, int stream) sum) l < s =
  (*  let _ = (begin Printf.printf "%a => %d\n" print_list l ((hash l / 1024) mod 2) end) in*)
  if (hash l / 1024) mod 2 = 0 then (!Inl s!) else (!Inr s!)

let classical test3 n < m < k = extract_sum n < m < (ramsey k < color_hash < nats)

let _ =
  for i = 3 to 10 do
    Printf.printf "%a\n" (print_sum print_list print_list) (?test3 i < i < 2?)
  done

let _ =
  Printf.printf "%a\n" (print_sum print_list print_list) (?test3 7 < 7 < 4?)

let dot_prod (a,b) (a',b') = a*a' + b*b'
let vec (a,b) (c,d) = (c - a, d - b)
let vec_prod (a,b) (a',b') = b'*a - b*a'

let same_side x y u v =
    let s = vec x y in
    let a = vec_prod s (vec x u) and b = vec_prod s (vec x v) in
    a <> 0 && b <> 0 && (a > 0) = (b > 0)

let in_triangle x y z u =
    same_side x y z u &&
    same_side y z x u &&
    same_side z x y u

let convex x y z t =
    not (in_triangle x y z t) &&
    not (in_triangle y z t x) &&
    not (in_triangle z t x y) &&
    not (in_triangle t x y z)

type point = int * int

let classical (color_convex: point list -> point stream => (point stream, point stream) sum) [x;y;z;t] < s =
  if convex x y z t then (!Inl s!) else (!Inr s!)

let classical (spirale_aux: int -> point -> point stream) q < (n,p) < () =
  let t = if p < q then (!spirale_aux q (n-1,p+1)!) else (!spirale_aux (q+1) (p+1,0)!) in
    ((n,p), t)

let classical (spirale: point stream) = (!spirale_aux 0 (0,0)!)

let maximum = ref 0

let classical (random_aux: int -> point list -> point stream) size < prev < () =
  let p = (!if !maximum < size then maximum:=size;
	    Random.int (2*size + 1) - size,  Random.int (2*size + 1) - size!) in
  if List.mem p prev then
    random_aux size < prev < ()
  else
    let t = fun () -> random_aux (size+1) < (p::prev) < () in
    (p, t)

let classical (random: point stream) () = random_aux 0 < [] < ()

let rec print_point_list ch l =
  match l with
    [] -> print_newline ()
  | (i,j)::l ->
    Printf.fprintf ch "(%d, %d) %a" i j print_point_list l

let rec show_erdos verbose n p =
  Random.init 0;
  if n > p then (
    if verbose then
      print_point_list stdout (?extract (!maximum + 1) < random?))
  else (
    (match (?extract_sum n < 5 < (ramsey 4 < color_convex < random) ?)
     with
       Inl(l) ->
       Printf.printf "Convexe à %d sommets parmis %d points aléatoires\n" n !maximum;
       print_point_list stdout l
     | Inr _ -> assert false);
    show_erdos verbose (n+1) p)

let _ = show_erdos false 5 14
