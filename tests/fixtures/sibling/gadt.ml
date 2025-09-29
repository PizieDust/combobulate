(* -*- combobulate-test-point-overlays: ((1 outline 158) (2 outline 184) (3 outline 228)); eval: (combobulate-test-fixture-mode t); -*- *)

type _ term =
  | Int : int -> int term
  | Add : (int term * int term) -> int term
  | Bool : bool -> bool term
