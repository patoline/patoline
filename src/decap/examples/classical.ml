type empty = { all : 'a.'a }
type 'a neg = 'a -> empty
      
let run f =
  let res = ref None in
  let r x = res := Some x; raise Exit in
  Printf.printf "start\n%!";
  let _ = try f r; assert false with Exit -> () in
  match !res with
    None -> assert false
  | Some x -> x
