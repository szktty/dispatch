open Core.Std
open Ctypes
open Unsigned
open Foreign

(* TODO: struct Block_literal_1 for C blocks *)

module Primitive = struct

  module Time = struct

    type t = uint64

    let t = uint64_t

    let now = UInt64.of_int 0

    let dispatch_time =
      foreign "dispatch_time" (t @-> int64_t @-> returning t)

  end

  module Block = struct

    module Flags = struct

      type t = ulong

      let t = ulong

      let barrier = ULong.of_int 0x1
      let detached = ULong.of_int 0x2
      let assign_current = ULong.of_int 0x4
      let no_qos_class = ULong.of_int 0x8
      let inherit_qos_class = ULong.of_int 0x10
      let enforce_qos_class = ULong.of_int 0x20

    end

    type t = (unit typ -> unit typ) fn

    let t = funptr (void @-> returning void)

    let create =
      foreign "dispatch_block_create" (Flags.t @-> t @-> returning t)

  end

end

module Time = struct

  type t = Primitive.Time.t

  let now = Primitive.Time.now

  let create when_ delta =
    Primitive.Time.dispatch_time when_ delta

  let of_int = UInt64.of_int

  let to_int = UInt64.to_int

  let of_int64 = UInt64.of_int64

  let to_int64 = UInt64.to_int64

end

module Block = struct

  type t = unit -> unit

  type flag =
    | Barrier
    | Detached
    | Assign_current
    | No_qos_class
    | Inherit_qos_class
    | Enforce_qos_class

  let value_of_flag flag =
    let open Primitive.Block.Flags in
    match flag with
    | Barrier -> barrier
    | Detached -> detached
    | Assign_current -> assign_current
    | No_qos_class -> no_qos_class
    | Inherit_qos_class -> inherit_qos_class
    | Enforce_qos_class -> enforce_qos_class

  let create ?(flags=[]) ~f =
    let prim_flags = List.fold_left flags
        ~init:(ULong.of_int 0)
        ~f:(fun accu flag ->
            ULong.add accu (value_of_flag flag))
    in
    Primitive.Block.create prim_flags f

end

module Queue = struct
end
