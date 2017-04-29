open Dispatch

let () =
  let now = Time.now in
  let now' = Time.create now (Int64.of_int 5) in
  Printf.printf "now = %d\n" (Time.to_int now);
  Printf.printf "now + 5 = %d\n" (Time.to_int now');

  let _block = Block.create ~f:(fun () -> Printf.printf "block ok\n") () in
  (* failed *)
  (*block ();*)
  ()

