open Core.Std
open Ctypes
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

      let lshift n m = Signed.Int32.(Infix.(lsl) (of_int n) m)

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

let () =
  let _c = Primitive.Runtime.lookup_class "__NSMallocBlock__" in
  Printf.printf "ok\n";
  let name = Primitive.Runtime.class_name _c in
  Printf.printf "class name = %s\n" name;
  ()

