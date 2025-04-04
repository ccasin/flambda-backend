(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
[@@@ocaml.warning "+4"]

open! Int_replace_polymorphic_compare

(* CSE for the AMD64 *)

open Arch

let of_simd_class (cl : Simd.operation_class) : Cfg_cse.op_class =
  match cl with
  | Pure -> Op_pure
  | Load { is_mutable = true } -> Op_load Mutable
  | Load { is_mutable = false } -> Op_load Immutable

class cfg_cse = object

  inherit Cfg_cse.cse_generic as super

  method! class_of_operation
  : Operation.t -> Cfg_cse.op_class
  = fun op ->
  match op with
    | Specific spec ->
    begin match spec with
    | Ilea _ | Isextend32 | Izextend32 -> Op_pure
    | Istore_int(_, _, is_asg) -> Op_store is_asg
    | Ioffset_loc(_, _) -> Op_store true
    | Ifloatarithmem _ -> Op_load Mutable
    | Ibswap _ -> super#class_of_operation op
    | Irdtsc | Irdpmc
    | Ilfence | Isfence | Imfence -> Op_other
    | Isimd op ->
      of_simd_class (Simd.class_of_operation op)
    | Isimd_mem (op,_addr) ->
      of_simd_class (Simd.Mem.class_of_operation op)
    | Ipause
    | Icldemote _
    | Iprefetch _ -> Op_other
      end
  | Move | Spill | Reload
  | Floatop _
  | Csel _
  | Reinterpret_cast _ | Static_cast _
  | Const_int _ | Const_float32 _ | Const_float _
  | Const_symbol _ | Const_vec128 _
  | Stackoffset _ | Load _ | Store _ | Alloc _
  | Intop _ | Intop_imm _ | Intop_atomic _
  | Name_for_debugger _ | Probe_is_enabled _ | Opaque
  | Begin_region | End_region | Poll | Dls_get
    -> super#class_of_operation op

end

let cfg_with_layout cfg_with_layout =
  (new cfg_cse)#cfg_with_layout cfg_with_layout
