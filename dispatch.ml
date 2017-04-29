open Core.Std
open Ctypes
open Unsigned
open Foreign

module Primitive = struct

  module Time = struct

    type dispatch_time = uint64

    let dispatch_time_t : uint64 typ = uint64_t

    let now = UInt64.of_int 0

    let dispatch_time =
      foreign "dispatch_time"
        (dispatch_time_t @-> int64_t @-> returning dispatch_time_t)

  end

end

module Time = struct

  type t = Primitive.Time.dispatch_time

  let now = Primitive.Time.now

  let create when_ delta =
    Primitive.Time.dispatch_time when_ delta

  let of_int = UInt64.of_int

  let to_int = UInt64.to_int

  let of_int64 = UInt64.of_int64

  let to_int64 = UInt64.to_int64

end
