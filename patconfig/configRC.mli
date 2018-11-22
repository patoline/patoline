(** This module provide a simple interface to a multi-purpose configuration
    files format. The file format is deliberatly kept very simple, and we do
    not provide any OCaml-specific features. A file consists in a list of
    named fields that can contain elements of a few base types. *)

module Data : sig
  (** The [Data] module provides an abstract interface for working with the
      value of configuration file fields. *)

  (** The exception [Mismatch] is raised when the user attempts to cast a
      piece of data to the wrong type using the functions bellow. *)
  exception Mismatch

  (** An enumerated type over the base types used in configuration files. *)
  type _ atom_type =
    | Int    : int    atom_type
    | Float  : float  atom_type
    | String : string atom_type
    | Bool   : bool   atom_type

  (** The type of the value of a configuration file field. *)
  type t

  type data = t

  (** Function for injecting OCaml types into the type [data]. *)

  val of_atom   : 'a atom_type -> 'a        -> t
  val of_list   : 'a atom_type -> 'a list   -> t
  val of_option : 'a atom_type -> 'a option -> t

  val of_int    : int    -> t
  val of_float  : float  -> t
  val of_string : string -> t
  val of_bool   : bool   -> t

  (** Function for reading OCaml from the type [data]. These functions can
      raise [Mismatch] if an attempt is made to read an element of [data]
      that do not have the right type. *)

  val to_atom   : 'a atom_type -> t -> 'a
  val to_list   : 'a atom_type -> t -> 'a list
  val to_option : 'a atom_type -> t -> 'a option

  val to_int    : t -> int
  val to_float  : t -> float
  val to_string : t -> string
  val to_bool   : t -> bool
end

module Config : sig
  (** The [Config] module provides an interface to configuration files. *)

  (** Type of a configuration. This is the abstract type corresponding to
      the contents of a configuration file. *)
  type t

  type config = t

  (** Lookup a field in a configuration. *)
  val get : string -> t -> Data.t

  (** Write a configuratio to an [out_channel]. *)
  val write : out_channel -> t -> unit

  (** Read a configuratio from an [in_channel], given a default value for
      the configuration. It is used for checking the type of fields, and
      for providing default values to missing fields. *)
  val read  : t -> in_channel -> t
end

module type Spec = sig
  (** The module type [Spec] is used as an argument to the [Make] functor.
      It is used to provide the specification of a configuration file. *)

  (** Name of the application. *)
  val name : string

  (** The actual specification of the configuration file. Each field is
      composed of a name (or key) and a default value. *)
  val spec : (string * Data.t) list

  (** The paths where to look for the configuration file on the file system.
      The first element corresponds to the main system-wide file, and the
      list then contains possible locations for files. Configuration files
      are looked for starting at the end of the list. *)
  val path : string * string list
end

module Make(S : Spec) : sig
  (** The [Make] functor is used for building a specific configuration
      system for an application. It provides higher-level interfaces to
      look for the configuration automatically and generating default
      configuration files. *)

  include Spec

  (** Default configuration for the system. It is used in case no config
      file is found in the locations specified in [path]. It is built
      directly from [spec]. *)
  val default : Config.t


  (** Write the default configuration to a channel. *)
  val print_default : out_channel -> unit

  (** Write the default configuration to a file. *)
  val write_default : string -> unit

  (** Read a configuration file that should have the same specification as
      the [default] module (that is the one defined by [spec]. *)
  val read_config : string -> Config.t

  (** Obtain the configuration of the system automatically by looking for
      configuration files using locations of [path]. If no configuration
      file is found, the default configuration is returned. *)
  val get_config : unit -> Config.t

  (** Write the current configuration to a file. *)
  val print_config : out_channel -> unit
end
