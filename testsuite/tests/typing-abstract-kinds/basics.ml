(* TEST
   expect;
*)

(* Basic tests for abstract kinds. No with-kind substutitions yet. *)

(******************************************************)
(* Test 1: Abstract kinds allowed in sigs and structs *)

kind_ k

module type S = sig
  kind_ k
  type t : k
end

module M = struct
  kind_ k
  type t : k
end

module M' : S = M

[%%expect{|
happy
|}]

(****************************************************)
(* Test 2: Kind aliases allowed in sigs and structs *)

kind_ k = immutable_data

module type S = sig
  kind_ k = float64
  type t : k
end

module M = struct
  kind_ k = float64
  type t : k
end

module M' : S = M

[%%expect{|
happy
|}]

(****************************************************)
(* Test 3: Abstract kinds are no kind in particular *)

kind_ k
[%%expect{|
happy
|}]

type t : k = int
[%%expect{|
sad
|}]

type t : k = float#
[%%expect{|
sad
|}]

type t : k = int64#
[%%expect{|
sad
|}]

type t : k = #(float# * int64#)
[%%expect{|
sad
|}]

(************************************************)
(* Test 4: Abstract kinds are not representable *)

kind_ k

type t : k

let f (x : t) = x
[%%expect{|
sad
|}]

(***********************************************)
(* Test 5: Abstract kinds are a subkind of max *)

kind_ k
type t : k

type s : any = t
[%%expect{|
happy
|}]

type s : bits32 = t
[%%expect{|
sad
|}]

(******************************************************)
(* Test 6: kind aliases work like the kind they alias *)

kind_ k = value & value mod portable

type t : k

type s1 : value & value = t
type s2 : value & value mod portable = t
type s3 : any mod portable = t
type s4 : any = t

let require_portable (_x : t @ portable) = ()
let cross_portable : t -> unit = fun x -> require_portable x
[%%expect{|
happy
|}]

type s5 : value & value mod global = t
[%%expect{|
sad
|}]

let cross_local (x : t @ local) = x
[%%expect{|
sad
|}]

(******************************************************************)
(* Test 7: expanding through aliases takes the meet of the bounds *)

module A = struct
  kind_ ka = float64
end

module B = struct
  kind_ kb = A.ka mod portable
end

module C = struct
  kind_ kc = B.kb
end

module D = struct
  kind_ kd = C.kc mod global
end

type t : D.kd

type s1 : float64 mod portable global = t
type s2 : any mod portable global = t
[%%expect{|
happy
|}]

type s3 : any mod contended = t
[%%expect{|
sad
|}]

(**************************)
(* Test 8: no with bounds *)

type t

kind_ k = immutable_data with t
[%%expect{|
sad
|}]

(************************)
(* Test 9: no recursion *)

kind_ k_rec = k_rec
[%%expect{|
sad
|}]

(******************************************)
(* Test 10: you can shadow built-in kinds *)

module Spicy = struct
  kind_ value = float64 & float64

  type t : value = #(float# * float#)
end
[%%expect{|
happy
|}]

type t : Spicy.value = string
[%%expect{|
sad
|}]

(**************************************)
(* Test 11: Abstracting in signatures *)

(* You can abstract away an alias *)
module M : sig
  kind_ k

  type t : k

  val f : unit -> t
end = struct
  kind_ k = value

  type t = string

  let f () = "hi mom"
end
[%%expect{|
happy
|}]

(* But be careful - now we don't know enough to call f! *)
let _ = M.f ()
[%%expect{|
sad
|}]

(* If you expose the definition, you have to be exact. *)
module M : sig
  kind_ k = value
end = struct
  kind_ k = immediate
end
[%%expect{|
sad
|}]

module M : sig
  kind_ k = immediate
end = struct
  kind_ k = value
end
[%%expect{|
sad
|}]

(******************************)
(* Test 12: Recursive modules *)

(* You can use a kind from a recursive buddy *)
module rec M1 : sig
  kind_ k = value
end = struct
  kind_ k = value
end

and M2 : sig
  type t : M1.k
end = struct
  type t = string
end
[%%expect{|
happy
|}]

(* You can use it in a kind def too *)
module rec M1 : sig
  kind_ k = M2.k
end = struct
  kind_ k = M2.k
end

and M2 : sig
  kind_ k
end = struct
  kind_ k
end
[%%expect{|
happy
|}]

(* But no recursion *)
module rec M1 : sig
  kind_ k
end = struct
  kind_ k = M2.k
end

and M2 : sig
  kind_ k
end = struct
  kind_ k = M1.k
end
[%%expect{|
sad
|}]

(*********************************)
(* Test 13: Unused kind warnings *)

(* The warning exists *)
module M : sig end = struct
  kind_ k
end
[%%expect{|
sad
|}]

(* You can suppress it *)
module M : sig end = struct
  kind_ k [@@warning "-191"]
end
[%%expect{|
happy
|}]

module M : sig end = struct
  [@@@warning "-191"]
  kind_ k
end
[%%expect{|
happy
|}]

(**************************)
(* Test 14: Strengthening *)

module type S = sig
  module type T = sig kind_ k end
  module M : T
  module N : T
  module Q : T with M
end

(* Q.k is known to be M.k *)
module F(X : S) = struct
  type t : X.Q.k
  type s : X.M.k = t
end

(* N.k is not *)
module F(X : S) = struct
  type t : X.N.k
  type s : X.M.k = t
end


(* more things:

   deprecated attribute?
   serialization, missing cmi
   shapes?
*)
