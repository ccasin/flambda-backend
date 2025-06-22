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
kind_ k
module type S = sig kind_ k type t : k end
module M : sig kind_ k type t : k end
module M' : S
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
kind_ k = immutable_data
module type S = sig kind_ k = float64 type t : k end
module M : sig kind_ k = float64 type t : k end
module M' : S
|}]

(*************************************************************)
(* Test 3: Abstract kinds are no concrete kind in particular *)

kind_ k
[%%expect{|
kind_ k
|}]

type t : k = int
[%%expect{|
Line 1, characters 0-16:
1 | type t : k = int
    ^^^^^^^^^^^^^^^^
Error: The kind of type "int" is immediate
         because it is the primitive type int.
       But the kind of type "int" must be a subkind of k
         because of the definition of t at line 1, characters 0-16.
|}]

type t : k = float#
[%%expect{|
Line 1, characters 0-19:
1 | type t : k = float#
    ^^^^^^^^^^^^^^^^^^^
Error: The kind of type "float#" is float64 mod everything
         because it is the unboxed version of the primitive type float.
       But the kind of type "float#" must be a subkind of k
         because of the definition of t at line 1, characters 0-19.
|}]

type t : k = int64#
[%%expect{|
Line 1, characters 0-19:
1 | type t : k = int64#
    ^^^^^^^^^^^^^^^^^^^
Error: The kind of type "int64#" is bits64 mod everything
         because it is the unboxed version of the primitive type int64.
       But the kind of type "int64#" must be a subkind of k
         because of the definition of t at line 1, characters 0-19.
|}]

type t : k = #(float# * int64#)
[%%expect{|
Line 1, characters 0-31:
1 | type t : k = #(float# * int64#)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "#(float# * int64#)" is
         float64 mod everything & bits64 mod everything
         because it is an unboxed tuple.
       But the kind of type "#(float# * int64#)" must be a subkind of k
         because of the definition of t at line 1, characters 0-31.
|}]

(************************************************)
(* Test 4: Abstract kinds are not representable *)

kind_ k

type t : k

let f (x : t) = x
[%%expect{|
kind_ k
type t : k
Line 5, characters 6-13:
5 | let f (x : t) = x
          ^^^^^^^
Error: This pattern matches values of type "t"
       but a pattern was expected which matches values of type
         "('a : '_representable_layout_1)"
       The kind of t is k
         because of the definition of t at line 3, characters 0-10.
       But the kind of t must be representable
         because we must know concretely how to pass a function argument.
|}]

(***********************************************)
(* Test 5: Abstract kinds are a subkind of max *)

kind_ k
type t : k

type s : any = t
[%%expect{|
kind_ k
type t : k
type s = t
|}]

type s : bits32 = t
[%%expect{|
Line 1, characters 0-19:
1 | type s : bits32 = t
    ^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is k
         because of the definition of t at line 2, characters 0-10.
       But the kind of type "t" must be a subkind of bits32
         because of the definition of s at line 1, characters 0-19.
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
kind_ k = value mod portable & value mod portable
type t : k
type s1 = t
type s2 = t
type s3 = t
type s4 = t
val require_portable : t @ portable -> unit = <fun>
val cross_portable : t -> unit = <fun>
|}]

type s5 : value & value mod global = t
[%%expect{|
Line 1, characters 0-38:
1 | type s5 : value & value mod global = t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is k
         because of the definition of t at line 3, characters 0-10.
       But the kind of type "t" must be a subkind of
         value mod global & value mod global
         because of the definition of s5 at line 1, characters 0-38.
|}]

let does_not_cross_local (x : t @ local) : t @ global = x
[%%expect{|
Line 1, characters 56-57:
1 | let does_not_cross_local (x : t @ local) : t @ global = x
                                                            ^
Error: This value escapes its region.
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
module A : sig kind_ ka = float64 end
module B : sig kind_ kb = A.ka mod portable end
module C : sig kind_ kc = B.kb end
module D : sig kind_ kd = C.kc mod global end
type t : D.kd
type s1 = t
type s2 = t
|}]

type s3 : any mod contended = t
[%%expect{|
Line 1, characters 0-31:
1 | type s3 : any mod contended = t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is D.kd
         because of the definition of t at line 17, characters 0-13.
       But the kind of type "t" must be a subkind of any mod contended
         because of the definition of s3 at line 1, characters 0-31.
|}]

(**************************)
(* Test 8: no with bounds *)

type t

kind_ k = immutable_data with t
[%%expect{|
type t
Line 3, characters 30-31:
3 | kind_ k = immutable_data with t
                                  ^
Error: 'with' syntax is not allowed on a right mode.
|}]
(* XXX better error *)

(************************)
(* Test 9: no recursion *)

kind_ k_rec = k_rec
[%%expect{|
Line 1, characters 14-19:
1 | kind_ k_rec = k_rec
                  ^^^^^
Error: Unbound jkind "k_rec"
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
module M : sig kind_ k type t : k val f : unit -> t end
|}]

(* But be careful - now we don't know enough to call f! *)
let _ = M.f ()
[%%expect{|
Line 1, characters 8-14:
1 | let _ = M.f ()
            ^^^^^^
Error: This expression has type "M.t" but an expression was expected of type
         "('a : '_representable_layout_2)"
       The kind of M.t is M.k
         because of the definition of t at line 4, characters 2-12.
       But the kind of M.t must be representable
         because it's the type of a variable bound by a `let`.
|}]

(* If you expose the definition, you have to be exact. *)
module M : sig
  kind_ k = value
end = struct
  kind_ k = immediate
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   kind_ k = immediate
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig kind_ k = immediate end
       is not included in
         sig kind_ k = value end
       Kind declarations do not match:
         kind_ k = immediate
       is not included in
         kind_ k = value
       Their definitions are not equal.
|}]

module M : sig
  kind_ k = immediate
end = struct
  kind_ k = value
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   kind_ k = value
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig kind_ k = value end
       is not included in
         sig kind_ k = immediate end
       Kind declarations do not match:
         kind_ k = value
       is not included in
         kind_ k = immediate
       Their definitions are not equal.
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

[%%expect{|
happy
|}]

(**************************)
(* Test 13: Strengthening *)

module type S = sig
  module type T = sig kind_ k end
  module M : T
  module N : T
  module Q : T with M
end
[%%expect{|
module type S =
  sig
    module type T = sig kind_ k end
    module M : T
    module N : T
    module Q : sig kind_ k = M.k end
  end
|}]

(* Q.k is known to be M.k *)
module F(X : S) = struct
  type t : X.Q.k
  type s : X.M.k = t
end
[%%expect{|
module F : functor (X : S) -> sig type t : X.Q.k type s = t end
|}]

(* N.k is not *)
module F(X : S) = struct
  type t : X.N.k
  type s : X.M.k = t
end
[%%expect{|
Line 3, characters 2-20:
3 |   type s : X.M.k = t
      ^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is X.N.k
         because of the definition of t at line 2, characters 2-16.
       But the kind of type "t" must be a subkind of X.M.k
         because of the definition of s at line 3, characters 2-20.
|}]
