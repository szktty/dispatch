open Core.Std
open Ctypes
open Unsigned
open Foreign

module Time = struct

  type t = UInt64.t

  let now = UInt64.of_int 0

  let create =
    foreign "dispatch_time"
      (uint64_t @-> int64_t @-> returning uint64_t)

end
