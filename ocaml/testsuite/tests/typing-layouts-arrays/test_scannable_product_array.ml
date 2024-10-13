(* TEST
 readonly_files = "gen_u_array.ml test_gen_u_array.ml";
 modules = "${readonly_files}";
 include stdlib_upstream_compatible;
 flambda2;
 {
   flags = "-extension layouts_beta";
   bytecode;
 }
*)

(* CR layouts v7.1: The PR that adds middle- and back-end support for unboxed
   product arrays should move this test to beta and make it run in native
   code. *)

(* Test of compilation correctness for scannable product arrays. General
   array-of-unboxed-things typing tests go in [basics.ml] in this directory.
   Tests specific to product arrays go in
   [typing-layouts-products/product_arrays.ml]. *)

(* We somewhat arbitrarily pick a specific scannable product to do all our tests
   on.  We could imagine picking a bunch of different such products, but one
   reasonably complex one seems sufficient. *)
type boxed_element =
  (int * (float * bool) * (int64 * (int * float)) * int32 option)

module Element_ops = struct
  type t = boxed_element

  let of_int i =
    (i,
     (Float.of_int i, i mod 2 = 0),
     (Int64.of_int i, (i, Float.of_int i)),
     Some (Int32.of_int i))

  let add (i1, (f1, b), (i64, (i2, f2)), i32o)
        (i1', (f1', b'), (i64', (i2', f2')), i32o') =
    (i1 + i1',
     (f1 +. f1', b = b'),
     (Int64.add i64 i64', (i2 + i2', f2 +. f2')),
     (Option.bind i32o
        (fun i32 -> (Option.map (fun i32' -> Int32.add i32 i32') i32o')))
    )

  let sub (i1, (f1, b), (i64, (i2, f2)), i32o)
        (i1', (f1', b'), (i64', (i2', f2')), i32o') =
    (i1 - i1',
     (f1 -. f1', b = b'),
     (Int64.sub i64 i64', (i2 - i2', f2 -. f2')),
     (Option.bind i32o
        (fun i32 -> (Option.map (fun i32' -> Int32.sub i32 i32') i32o')))
    )

  let mul (i1, (f1, b), (i64, (i2, f2)), i32o)
        (i1', (f1', b'), (i64', (i2', f2')), i32o') =
    (i1 * i1',
     (f1 *. f1', b || b'),
     (Int64.mul i64 i64', (i2 * i2', f2 *. f2')),
     (Option.bind i32o
        (fun i32 -> (Option.map (fun i32' -> Int32.mul i32 i32') i32o')))
    )

  let neg (i1, (f1, b), (i64, (i2, f2)), i32o) =
    (-i1,
     (Float.neg f1, b),
     (Int64.neg i64, (-i2, Float.neg f2)),
     (Option.map Int32.neg i32o)
    )

  let max_val =
    (Int.max_int,
     (Float.max_float, true),
     (Int64.max_int, (Int.max_int, Float.max_float)),
     (Some Int32.max_int))

  let min_val =
    (Int.min_int,
     (Float.min_float, false),
     (Int64.min_int, (Int.min_int, Float.min_float)),
     None)

  let rand (i1, (f1, b), (i64, (i2, f2)), i32o) =
    (Random.full_int i1,
     (Random.float f1, b && Random.bool ()),
     (Random.int64 i64, (Random.full_int i2, Random.float f2)),
     (Option.map Random.int32 i32o))

  let compare (i1, (f1, b), (i64, (i2, f2)), i32o)
        (i1', (f1', b'), (i64', (i2', f2')), i32o') =
    let comparisons =
      [Int.compare i1 i1';
       Float.compare f1 f1';
       Bool.compare b b';
       Int64.compare i64 i64';
       Int.compare i2 i2';
       Float.compare f2 f2';
       Option.compare Int32.compare i32o i32o']
    in
    match List.find_opt ((<>) 0) comparisons with
    | None -> 0
    | Some i -> i

  let print (i1, (f1, b), (i64, (i2, f2)), i32o) =
    let i32o =
      match i32o with
      | None -> "None"
      | Some i -> Format.sprintf "Some %ld" i
    in
    Format.printf "(%d, (%f, %b), (%Ld, (%i, %f)), %s)"
      i1 f1 b i64 i2 f2 i32o
end

module Tuple_array : Test_gen_u_array.S = struct
  include Stdlib.Array
  type element_t = boxed_element


  type t = element_t array
  let map_to_array f a = map f a
  let map_from_array f a = map f a
  let max_length = Sys.max_array_length
  let equal = for_all2 (fun x y -> x = y)
  module I = Element_ops
end

module _ = Test_gen_u_array.Test (Tuple_array)

type unboxed_element =
  #(int * #(float * bool) * #(int64 * #(int * float)) * int32 option)

module UTuple_array0 :
  Gen_u_array.S0 with type element_t = unboxed_element
                  and type ('a : any) array_t = 'a array = struct
  type element_t = unboxed_element

  type ('a : any) array_t = 'a array

  type element_arg = unit -> element_t
  type t = element_t array
  let max_length = Sys.max_array_length
  external length : element_t array -> int = "%array_length"
  external get: element_t array -> int -> element_t = "%array_safe_get"
  let get t i = let a = get t i in fun () -> a
  external set: element_t array -> int -> element_t -> unit = "%array_safe_set"
  let set t i e = set t i (e ())
  external unsafe_get: element_t array -> int -> element_t = "%array_unsafe_get"
  let unsafe_get t i = let a = unsafe_get t i in fun () -> a
  external unsafe_set: element_t array -> int -> element_t -> unit =
    "%array_unsafe_set"
  let unsafe_set t i e = unsafe_set t i (e ())

  external makearray_dynamic : int -> element_t -> element_t array =
    "%makearray_dynamic"

  let unsafe_create : int -> element_t array =
    (* We don't actually have an uninitialized creation function for these, yet,
       so we just use [makearray_dynamic] (which is what we want to test anyway)
       with the zero element. *)
    let zero = #(0, #(0.0, false), #(0L, #(0, 0.0)), None) in
    fun i -> makearray_dynamic i zero

  external unsafe_blit :
    element_t array -> int -> element_t array -> int -> int -> unit =
    "caml_array_blit"
  (* XXX: We'll need to fill in a native-code blit here to enable the native
     code version of this test. *)

  let empty () = [||]
  let to_boxed #(i1, #(f1, b), #(i64, #(i2, f2)), i32o) =
    (i1, (f1, b), (i64, (i2, f2)), i32o)

  let compare_element x y =
    Element_ops.compare (to_boxed (x ())) (to_boxed (y ()))
end

module UTuple_array = Gen_u_array.Make (UTuple_array0)

module UTuple_array_boxed = Test_gen_u_array.Make_boxed (struct
    module M = UTuple_array
    module I = Element_ops
    module E = struct
      let to_boxed x =
        let #(i1, #(f1, b), #(i64, #(i2, f2)), i32o) = x () in
        (i1, (f1, b), (i64, (i2, f2)), i32o)
      let of_boxed (i1, (f1, b), (i64, (i2, f2)), i32o) () =
        #(i1, #(f1, b), #(i64, #(i2, f2)), i32o)
    end
  end)
module _ = Test_gen_u_array.Test (UTuple_array_boxed)
