(* TEST
 expect;
*)

(* This file tests how unique_ and once_ are interpreated in signatures
   especially when currying is involved *)

(* When a [unique_] argument appears in a function type with multiple arguments,
return modes are implicitly once_ until the final argument. *)
type equ_fn = unit
constraint
'a -> unique_ 'b -> 'c -> 'd -> 'e
= 'a -> unique_ 'b -> once_ ('c -> once_ ('d -> 'e))
[%%expect{|
type equ_fn = unit
|}]

(* similar for once_ *)
type equ_fn = unit
constraint
'a -> once_ 'b -> 'c -> 'd -> 'e
= 'a -> once_ 'b -> once_ ('c -> once_ ('d -> 'e))
[%%expect{|
type equ_fn = unit
|}]

(* uniqueness of closures are by default aliased,
   regardless of anything; unique would be better
   except for some backward compatibility issues *)
type equ_fn = unit
constraint
'a -> unique_ 'b -> 'c -> 'd -> 'e
= 'a -> unique_ 'b -> unique_ once_ ('c -> unique_ once_ ('d -> 'e))
[%%expect{|
Lines 3-4, characters 0-68:
3 | 'a -> unique_ 'b -> 'c -> 'd -> 'e
4 | = 'a -> unique_ 'b -> unique_ once_ ('c -> unique_ once_ ('d -> 'e))
Error: The type constraints are not consistent.
       Type "'a -> 'b @ unique -> 'c -> 'd -> 'e" is not compatible with type
         "'a -> 'b @ unique -> ('c -> ('d -> 'e) @ unique once) @ unique once"
       Type "'b @ unique -> 'c -> 'd -> 'e" is not compatible with type
         "'b @ unique -> ('c -> ('d -> 'e) @ unique once) @ unique once"
|}]

type distinct_sarg = unit constraint unique_ int -> int = int -> int
[%%expect{|
Line 1, characters 37-68:
1 | type distinct_sarg = unit constraint unique_ int -> int = int -> int
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
       Type "int @ unique -> int" is not compatible with type "int -> int"
|}]
type distinct_sret = unit constraint int -> unique_ int = int -> int
[%%expect{|
Line 1, characters 37-68:
1 | type distinct_sret = unit constraint int -> unique_ int = int -> int
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
       Type "int -> int @ unique" is not compatible with type "int -> int"
|}]
type distinct_sarg_sret = unit constraint unique_ int -> int = unique_ int -> unique_ int
[%%expect{|
Line 1, characters 42-89:
1 | type distinct_sarg_sret = unit constraint unique_ int -> int = unique_ int -> unique_ int
                                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
       Type "int @ unique -> int" is not compatible with type
         "int @ unique -> int @ unique"
|}]
