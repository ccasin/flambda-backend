(* TEST
   expect;
*)

(* Basic tests for abstract kinds. No with-kind substutitions yet. *)

(* Test 1: You can have abstract kinds in signatures. *)
module type S = sig
  kind_ k
  type t : k
end

[%%expect{|
module type S = sig jkind_ k type t : k end
|}]
