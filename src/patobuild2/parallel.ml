(* Number of threads used by compilation. *)
let nb_threads = ref 1

(* Thread-safe printing functions. Be careful to always FULLY APPLY them. *)
let m = Mutex.create ()
let fprintf : out_channel -> ('a, out_channel, unit) format -> 'a =
  fun och fmt ->
    let f _ _ = Mutex.lock m in
    Printf.kfprintf (fun _ -> Mutex.unlock m) och ("%a" ^^ fmt) f ()

let printf  : ('a, out_channel, unit) format -> 'a = fun fmt ->
  fprintf stdout fmt

let eprintf : ('a, out_channel, unit) format -> 'a = fun fmt ->
  fprintf stderr fmt

(* Parallel iteration function. *)
let iter : ('a -> unit) -> 'a list -> unit = fun f ls ->
  let m = Mutex.create () in
  let bag = ref ls in
  let rec thread_fun () =
    Mutex.lock m;
    match !bag with
    | t::ts -> bag := ts; Mutex.unlock m; f t; thread_fun ()
    | []    -> Mutex.unlock m; Thread.exit ()
  in
  let ths = Array.init !nb_threads (fun _ -> Thread.create thread_fun ()) in
  Array.iter Thread.join ths
