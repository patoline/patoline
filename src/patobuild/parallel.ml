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

module TaskBag =
  struct
    type 'a t =
      { mutex         : Mutex.t
      ; cond          : Condition.t
      ; eq            : 'a -> 'a -> bool
      ; mutable state : 'a list }

    let create : ('a -> 'a -> bool) -> 'a t = fun eq ->
      { mutex = Mutex.create ()
      ; cond  = Condition.create ()
      ; eq    = eq
      ; state = [] }

    let size : 'a t -> int = fun s -> List.length s.state

    let get : 'a t -> 'a list = fun s -> s.state

    let wait : 'a t -> 'a = fun s ->
      Mutex.lock s.mutex;
      while s.state = [] do
        Condition.wait s.cond s.mutex
      done;
      let task = List.hd s.state in
      s.state <- List.tl s.state;
      Mutex.unlock s.mutex; task

    let post : 'a t -> 'a list -> unit = fun s tasks ->
      Mutex.lock s.mutex;
      let insert tasks t =
        if List.exists (s.eq t) tasks then tasks else tasks @ [t]
      in
      s.state <- List.fold_left insert s.state tasks;
      Mutex.unlock s.mutex;
      List.iter (fun _ -> Condition.signal s.cond) tasks

    let raw_post : 'a t -> 'a list -> unit = fun s tasks ->
      Mutex.lock s.mutex; s.state <- s.state @ tasks; Mutex.unlock s.mutex;
      List.iter (fun _ -> Condition.signal s.cond) tasks

  end
