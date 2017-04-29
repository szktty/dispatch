module Time : sig

  type t

  val now : t

  val create : t -> int64 -> t

  val of_int : int -> t

  val to_int : t -> int

  val of_int64 : int64 -> t

  val to_int64 : t -> int64

end

module Block : sig

  type t = unit -> unit

  type flag =
    | Barrier
    | Detached
    | Assign_current
    | No_qos_class
    | Inherit_qos_class
    | Enforce_qos_class

  val create : ?flags:flag list -> f:t -> t

end

module Queue : sig

  (*
  type t

  val async : t -> Block.t -> unit
   *)

end
