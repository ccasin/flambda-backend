(* TEST
   {
     expect;
   }
*)

(* NOTE: When adding tests to this file, consider updating
   [typing-layouts-products/typing_misc_unboxed_records.ml] *)

type 'a ref_u = { contents : 'a }

(* This test is based on testsuite/tests/typing-misc/records.ml *)

(* undefined labels *)
type t = {x:int;y:int};;
#{x=3;z=2};;
[%%expect{|
type 'a ref_u = { contents : 'a; }
type t = { x : int; y : int; }
Line 7, characters 6-7:
7 | #{x=3;z=2};;
          ^
Error: Unbound unboxed record field "z"
|}];;
fun #{x=3;z=2} -> ();;
[%%expect{|
Line 1, characters 10-11:
1 | fun #{x=3;z=2} -> ();;
              ^
Error: Unbound unboxed record field "z"
|}];;

(* mixed labels *)
#{x=3; contents=2};;
[%%expect{|
Line 1, characters 7-15:
1 | #{x=3; contents=2};;
           ^^^^^^^^
Error: The unboxed record field "contents" belongs to the type "'a ref_u#"
       but is mixed here with fields of type "t#"
|}];;

(* private types *)
type u = private {u:int};;
#{u=3};;
[%%expect{|
type u = private { u : int; }
Line 2, characters 0-6:
2 | #{u=3};;
    ^^^^^^
Error: Cannot create values of the private type "u#"
|}];;

(* Punning and abbreviations *)
module M = struct
  type t = {x: int; y: int}
end;;
[%%expect{|
module M : sig type t = { x : int; y : int; } end
|}];;

let f #{M.x; y} = x+y;;
let r () = #{M.x=1; y=2};;
let z () = f (r ());;
[%%expect{|
val f : M.t# -> int = <fun>
val r : unit -> M.t# = <fun>
val z : unit -> int = <fun>
|}];;

(* messages *)

let f (r: int) =
  match r with
  | #{ contents = 3 } -> ()
[%%expect{|
Line 3, characters 4-21:
3 |   | #{ contents = 3 } -> ()
        ^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type "int ref_u#"
       but a pattern was expected which matches values of type "int"
|}];;



(* bugs *)
type foo = { y: int; z: int };;
type bar = { x: int };;
let f (r: bar#) = (#{ r with z = 3 } : foo)
[%%expect{|
type foo = { y : int; z : int; }
type bar = { x : int; }
Line 3, characters 19-36:
3 | let f (r: bar#) = (#{ r with z = 3 } : foo)
                       ^^^^^^^^^^^^^^^^^
Error: This unboxed record expression should be boxed instead,
       the expected type is "foo"
|}];;

type foo = { x: int };;
let r : foo# = #{ ZZZ.x = 2 };;
[%%expect{|
type foo = { x : int; }
Line 2, characters 18-23:
2 | let r : foo# = #{ ZZZ.x = 2 };;
                      ^^^^^
Error: Unbound module "ZZZ"
|}];;

(ZZZ.X : int option);;
[%%expect{|
Line 1, characters 1-6:
1 | (ZZZ.X : int option);;
     ^^^^^
Error: Unbound module "ZZZ"
|}];;

(* PR#6608 *)
#{ true with contents = 0 };;
[%%expect{|
Line 1, characters 0-27:
1 | #{ true with contents = 0 };;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "bool" which is not a unboxed record type.
|}];;

type ('a, 'b) t = { fst : 'a; snd : 'b };;
let with_fst r fst = #{ r with fst };;
let #{ fst;  snd} = with_fst #{ fst=""; snd="" } 2;;
[%%expect{|
type ('a, 'b) t = { fst : 'a; snd : 'b; }
val with_fst : ('a, 'b) t# -> 'c -> ('c, 'b) t# = <fun>
val fst : int = 2
val snd : string = ""
|}];;

(* PR#7695 *)
type 'a t = { f : 'a; g : 'a };;
let x () = #{ f = 12; g = 43 };;
let foo () = let x = x () in #{x with f = "hola"};;
[%%expect{|
type 'a t = { f : 'a; g : 'a; }
val x : unit -> int t# = <fun>
Line 3, characters 29-49:
3 | let foo () = let x = x () in #{x with f = "hola"};;
                                 ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "string t#"
       but an expression was expected of type "int t#"
       Type "string" is not compatible with type "int"
|}]

(* PR#7696 *)
let r = #{ (assert false) with contents = 1 } ;;
[%%expect{|
Line 1, characters 8-45:
1 | let r = #{ (assert false) with contents = 1 } ;;
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 23 [useless-record-with]: all the fields are explicitly listed in this unboxed record:
the 'with' clause is useless.

Exception: Assert_failure ("", 1, 11).
|}]

type t = { f1 : int ; f2 : int }

let f () = #{ f1 = 0
        ; Coq__10.f2 = 0 }

[%%expect{|
type t = { f1 : int; f2 : int; }
Line 4, characters 10-20:
4 |         ; Coq__10.f2 = 0 }
              ^^^^^^^^^^
Error: Unbound module "Coq__10"
|}]

module Coq__11 = struct
  type t = { f1 : int ; f2 : int; f3 : int }
end

let f () = #{ f1 = 0
           ; Coq__10.f2 = 0
           ; Coq__11.f3 = 0 }

[%%expect{|
module Coq__11 : sig type t = { f1 : int; f2 : int; f3 : int; } end
Line 6, characters 13-23:
6 |            ; Coq__10.f2 = 0
                 ^^^^^^^^^^
Error: Unbound module "Coq__10"
Hint: Did you mean "Coq__11"?
|}]
