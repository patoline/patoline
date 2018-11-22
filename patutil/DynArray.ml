(**
Dynamic arrays
 
A dynamic array behaves like an ordinary array, except that it can grow
on demand. It is implemented as a regular OCaml array, except that any
write attempt beyond the end of the array triggers the allocation of a
new array, whose size is twice the size of the current array. This new
array replaces the old one.
*)

(** Type of dynamic arrays. A dynamic array stores a defaut value of
 type ['a], which is used to initialize objects when expanding arrays.
 *)
type 'a t = {
  mutable data: 'a array;
  mutable length: int;
  default_value : 'a;
}

(** Creating a new array, which initially has [n] elements *)
let make n default_value =
  {
    data = Array.make n default_value;
    length = n;
    default_value = default_value;
  }

(** Retrieve the element stored at index [i]. Returns the array default
 value if index [i] is beyond the end of the array. *)
let get arr i =
  try arr.data.(i)
  with Invalid_argument(_) -> arr.default_value

(** Sets the value of the element at index [i], possibly resizing the
 array if needed. *)
let set arr i x =
  let n = Array.length arr.data in
  if i >= n then begin
    let new_arr = Array.make (2 * n) arr.default_value in
    Array.blit arr.data 0 new_arr 0 n;
    arr.data <- new_arr
  end;
  arr.data.(i) <- x;
  arr.length <- max arr.length (i+1)

(** Gets the actual number of elements stored in the array, which is
 always smaller than the number of available slots in memory. This
 always corresponds to [i+1] where [i] is the maximal index ever given
 to a call to {!val:set}. *)
let length arr =
  arr.length

(** Appends a new element after the last one *)
let append arr value =
  set arr (length arr) value

(** Reset the dynamic length to zero, without actually freeing slots. *)
let empty arr =
  arr.length <- 0

(** Returns the size of the actual OCaml array. This corresponds to the
 number of available slots which can be filled before {!val:set} needs
 to reallocate a new array. *)
let slots_length arr =
  Array.length arr.data
