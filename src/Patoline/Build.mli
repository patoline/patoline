exception No_rule of string
val j : int ref
type known = Known of Mutex.t | Not_known of Mutex.t
val known : string -> known
type sem = {
  mut_value : Mutex.t;
  mutable value : int;
  mut_signal : Condition.t;
}
val sem_up : sem -> unit
val sem_down : sem -> unit
val sem_create : int -> sem
val sem_set : sem -> int -> unit
val sem : sem
val command : string -> int
type rule_t = Node of rule_t * rule_t | Leaf of (string -> bool)
val rules : rule_t ref
val macros : (string->string) Util.StrMap.t ref
val append_rule : (string -> bool) -> unit
val build_with_rule : (string -> bool) -> string -> unit
val build : string -> unit
