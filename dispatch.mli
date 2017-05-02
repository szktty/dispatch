module Time : sig

  type t

  val now : t

  val create : t -> int64 -> t

  val of_int : int -> t

  val to_int : t -> int

  val of_int64 : int64 -> t

  val to_int64 : t -> int64

end

module Queue : sig

end
