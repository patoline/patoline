(** Abstract representation of a permanent map. *)
type t

(** [new_map file] creates a new map file named [file]. If [file] already
    exists, then exception [Invalid_arg] is raised. *)
val new_map : string -> unit

(** [open_map file] opens a map file previously created using [new_map]. If
    [file] does not exist, the exception [Invalid_arg] is raised. The value
    returned is a map (i.e. an object of type [t]). *)
val open_map : string -> t

(** [close_map m] closes a map previously opened with [open_map]. This
    operation does not seem to be necessary, but it is probably a good idea
    to use it anyway. *)
val close_map : t -> unit

(** [add m k v] adds the value [v] with the key [k] to the map [m]. It is the
    user's responsibility not to add a value with an already existing key.
    Doing so will lead to an unspecified behaviour. If you are not sure
    whether a key is mapped or not, use the function [set]. *)
val add : t -> int -> 'a -> unit

(** [add_many m l] is equivalent to [List.iter (fun (k, v) -> add m k v) l]
    but it is a lot faster. On should prefer this function over add when
    adding more than a few entries. One should consider using the [compact]
    function after adding many entries as it might significantly reduce the
    size of the map file. *)
val add_many : t -> (int * 'a) list -> unit

(** [del m k] deletes the entry with the key [k] from the map [m] when it
    exists. *)
val del : t -> int -> unit

(** [set m k v] replaces the entry with the key [k] from the map [m] with the
    value [v]. This function behaves as [add] if no value with the key [k] is
    in the map. *)
val set : t -> int -> 'a -> unit

(** [get m k] retreave the value associated to the key [k] in the map [m]. If
    no value is mapped to [k], the exception [Not_found] is raised. *)
val get : t -> int -> 'a

(** [compact m] reduces the size of the given map if possible. *)
val compact : t -> unit
