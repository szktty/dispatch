open Core.Std
open Ctypes
open Unsigned
open Foreign

let () =
  let now = Dispatch.Time.now in
  let now' = Dispatch.Time.create now (Int64.of_int 5) in
  Printf.printf "now = %d\n" (UInt64.to_int now);
  Printf.printf "now + 5 = %d\n" (UInt64.to_int now');
  ()

