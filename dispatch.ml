open Core.Std
open Ctypes
open Signed
open Unsigned
open Foreign

module Primitive = struct

  module Runtime = struct

    let class_ = ptr void

    let lookup_class =
      foreign "objc_lookUpClass" (string @-> returning class_)

    let class_name =
      foreign "class_getName" (class_ @-> returning string)

  end

  module Object = struct

    type t = unit ptr

    let t : t typ = ptr void

    let retain =
      foreign "dispatch_retain" (t @-> returning void)

    let release =
      foreign "dispatch_release" (t @-> returning void)

  end

  module Time = struct

    type t = uint64

    let t = uint64_t

    let now = UInt64.of_int 0

    let dispatch_time =
      foreign "dispatch_time" (t @-> int64_t @-> returning t)

  end

  module Function = struct

    type t = (unit ptr -> unit)

    let t : t typ = funptr (ptr void @-> returning void)

  end

  module Queue = struct

    module Priority = struct

      type t = long

      let t : t typ = long

      let high = Long.of_int 2
      let default = Long.of_int 0
      let low = Long.of_int (-2)
      let background = Long.of_int 0x8000 (* int16 min *)

    end

    module Attr = struct
    end

    module Qos = struct
    end

    module Once = struct

      type t = long

      let t : t typ = long

    end

    type t = unit ptr

    let t : t typ = ptr void

    let global_queue =
      foreign "dispatch_get_global_queue" (long @-> ulong @-> returning t)

    let create =
      foreign "dispatch_queue_create" (string @-> ptr void @-> returning t)

    let label =
      foreign "dispatch_queue_get_label" (t @-> returning string)

    let async_f =
      foreign "dispatch_async_f" (t @-> ptr void @-> Function.t @-> returning void)

    let sync_f =
      foreign "dispatch_sync_f" (t @-> ptr void @-> Function.t @-> returning void)

    let after_f =
      foreign "dispatch_after_f"
        (Time.t @-> t @-> ptr void @-> Function.t @-> returning void)

    let once_f =
      foreign "dispatch_once_f"
        (Once.t @-> t @-> ptr void @-> Function.t @-> returning void)

    let main =
      foreign "dispatch_main" (void @-> returning void)

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

module Queue = struct
end

let test_f _ =
  ()

let rec fib n =
  if n < 2 then n else fib (n - 2) + fib (n - 1)

let () =
  let queue = Primitive.Queue.global_queue Primitive.Queue.Priority.high ULong.zero in
  let label = Primitive.Queue.label queue in
  Printf.printf "queue label = %s\n" label;

  Printf.printf "begin create queue\n";
  flush_all ();
  let workers = ref [] in
  for i = 0 to 20 do
    Printf.printf "create queue %d\n" i;
    flush_all ();
    let queue = Primitive.Queue.create "test" null in
    workers := queue :: !workers
  done;
  Printf.printf "ready queue\n";
  flush_all ();

  for i = 0 to 10000 do
    List.iter !workers
      ~f:(fun worker ->
          Primitive.Queue.async_f worker null (fun _ -> ignore @@ fib 38);
          ignore @@ Unix.nanosleep 0.01;
        );
  done;

  flush_all ();
  Printf.printf "begin loop\n";
  flush_all ();
  Primitive.Queue.main ();
  ()

