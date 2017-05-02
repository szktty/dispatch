open Ctypes

type never_returns = unit

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

  type t

  (* adhoc: type of function to submit to a queue should be the following:
   *
   * type work = (unit -> unit)
   *
   * But a program is terminated if a function pointer which wraps
   * a OCaml function (unit -> unit) is given to dispatch queue functions. *)
  type work = (unit ptr -> unit)

  val current : unit -> t

  val global : unit -> t

  val create : string -> t

  val label : t -> string

  val async : t -> f:work -> unit

  val sync : t -> f:work -> unit

  val after : t -> f:work -> time:Time.t -> unit

  (*val once : Once.t -> f:work -> unit*)

  val forever : unit -> never_returns

end
