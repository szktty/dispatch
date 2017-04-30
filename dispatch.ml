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

    let malloc_block_class =
      lookup_class "__NSMallocBlock__"

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

  module Block_literal = struct

    module Flags = struct

      type t = int32

      let t = int32_t

      let lshift n m = Int32.(Infix.(lsl) (of_int n) m)

      let block_has_copy_dispose = lshift 1 25

      let block_has_ctor = lshift 1 26

      let block_is_global = lshift 1 28

      let block_has_stret = lshift 1 29

      let block_has_signature = lshift 1 30

    end

    type s

    type t = s structure

    let t : t typ = structure "block_literal"

    let isa = field t "isa" (ptr void)

    let flags = field t "flags" int32_t

    let reserved = field t "reserved" int32_t

    let invoke = field t "invoke" (funptr (ptr void @-> returning void))

    let descriptor = field t "descriptor" (ptr void)

    let () = seal t

    let create ~(f:(unit -> unit)) () =
      let f = fun _ -> f () in (* unit ptr -> unit *)
      let str = make t in
      setf str isa Runtime.malloc_block_class;
      setf str flags (Signed.Int32.of_int 0);
      setf str flags (Signed.Int32.of_int (-1023410174));
      setf str invoke f;
      setf str descriptor null;
      str

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

    type t = Block_literal.t ptr

    let t : t typ = ptr Block_literal.t

    let create =
      foreign "dispatch_block_create" (Flags.t @-> t @-> returning t)

  end

  module Function = struct

    (* TODO: type t = *)
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

    type t = unit ptr

    let t : t typ = ptr void

    let global_queue =
      foreign "dispatch_get_global_queue" (long @-> ulong @-> returning t)

    let create =
      foreign "dispatch_queue_create" (string @-> ptr void @-> returning t)

    let label =
      foreign "dispatch_queue_get_label" (t @-> returning string)

    let async =
      foreign "dispatch_async" (t @-> Block.t @-> returning void)

    let async_f =
      foreign "dispatch_async_f" (t @-> ptr void @-> Function.t @-> returning void)

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

module Block = struct

  type t = Primitive.Block.t

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

  let create ?(flags=[]) ~(f:(unit -> unit)) () =
    let lit = addr @@ Primitive.Block_literal.create ~f () in
    let prim_flags = List.fold_left flags
        ~init:(ULong.of_int 0)
        ~f:(fun accu flag ->
            ULong.add accu (value_of_flag flag))
    in
    Primitive.Block.create prim_flags lit

end

module Queue = struct
end

let test_f _ =
  ()

let () =
  (*
  let ctrl = Gc.get () in
  ctrl.minor_heap_size <- 512000;
  ctrl.major_heap_increment <- 100;
   *)

  let _c = Primitive.Runtime.lookup_class "__NSMallocBlock__" in
  Printf.printf "ok\n";
  let name = Primitive.Runtime.class_name _c in
  Printf.printf "class name = %s\n" name;

  let queue = Primitive.Queue.global_queue Primitive.Queue.Priority.high ULong.zero in
  let label = Primitive.Queue.label queue in
  Printf.printf "queue label = %s\n" label;

  (*
  let block = Block.create ~f:(fun () -> Printf.printf "block executed!\n") () in
  Primitive.Queue.async queue block;
   *)

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

  (*Primitive.Queue.async_f queue null (fun _ -> Printf.printf "async_f ok\n");*)
  for i = 0 to 10000 do
    (*
    Printf.printf "i = %d\n" i;
    flush_all ();
     *)
    List.iter !workers
      ~f:(fun worker ->
          (*Printf.printf "queue %s async_f\n" (Primitive.Queue.label worker);*)
          (*Primitive.Queue.async_f worker null (fun _ -> ());*)
          (*Primitive.Queue.async_f worker null (fun _ -> ());*)
          Primitive.Queue.async_f worker null
            (fun _ ->
               for i = 0 to 1000 do
                 ()
               done;

               ());
          (*Primitive.Queue.async_f queue null (fun _ -> Printf.printf "async_f ok\n");*)
          ignore @@ Unix.nanosleep 0.0001;
        );
    (*Primitive.Queue.async_f queue null (fun _ -> Printf.printf "async_f ok\n");*)
    (*Primitive.Queue.async_f queue null (fun _ -> ());*)
    (*ignore @@ Unix.nanosleep 0.001;*)
  done;

  flush_all ();
  Printf.printf "begin loop\n";
  flush_all ();
  Primitive.Queue.main ();
  ()

