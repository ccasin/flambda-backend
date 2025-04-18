(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Int_replace_polymorphic_compare
open Cmm


module V = Backend_var

module Raw_name = struct
  type t =
    | Anon
    | R
    | Var of V.t

  let create_from_var var = Var var

  let to_string t =
    match t with
    | Anon -> None
    | R -> Some "R"
    | Var var ->
      let name = V.name var in
      if String.length name <= 0 then None else Some name
end

type t =
  { mutable raw_name: Raw_name.t;
    stamp: int;
    typ: Cmm.machtype_component;
    mutable loc: location; }

and location =
    Unknown
  | Reg of int
  | Stack of stack_location

and stack_location =
    Local of int
  | Incoming of int
  | Outgoing of int
  | Domainstate of int

type reg = t

let dummy =
  { raw_name = Raw_name.Anon; stamp = 0; typ = Int; loc = Unknown; }

let currstamp = ref 0
let reg_list = ref([] : t list)
let hw_reg_list = ref ([] : t list)

let create ty =
  let r = { raw_name = Raw_name.Anon; stamp = !currstamp; typ = ty; loc = Unknown; } in
  reg_list := r :: !reg_list;
  incr currstamp;
  r

let createv tyv =
  let n = Array.length tyv in
  let rv = Array.make n dummy in
  for i = 0 to n-1 do rv.(i) <- create tyv.(i) done;
  rv

let createv_like rv =
  let n = Array.length rv in
  let rv' = Array.make n dummy in
  for i = 0 to n-1 do rv'.(i) <- create rv.(i).typ done;
  rv'

let clone r =
  let nr = create r.typ in
  nr.raw_name <- r.raw_name;
  nr

let at_location ty loc =
  let r = { raw_name = Raw_name.R; stamp = !currstamp; typ = ty; loc; } in
  hw_reg_list := r :: !hw_reg_list;
  incr currstamp;
  r

let typv rv =
  Array.map (fun r -> r.typ) rv

let anonymous t =
  match Raw_name.to_string t.raw_name with
  | None -> true
  | Some _raw_name -> false

let is_preassigned t =
  match t.raw_name with
  | R -> true
  | Anon | Var _ -> false

let is_unknown t =
  match t.loc with
  | Unknown -> true
  | Reg _ | Stack (Local _ | Incoming _ | Outgoing _ | Domainstate _) -> false

let name t =
  match Raw_name.to_string t.raw_name with
  | None -> ""
  | Some raw_name -> raw_name

let first_virtual_reg_stamp = ref (-1)

let is_stack t =
  match t.loc with
  | Stack _ -> true
  | Reg _ | Unknown -> false

let is_reg t =
  match t.loc with
  | Reg _ -> true
  | Stack _ | Unknown -> false

let reset() =
  (* When reset() is called for the first time, the current stamp reflects
     all hard pseudo-registers that have been allocated by Proc, so
     remember it and use it as the base stamp for allocating
     soft pseudo-registers *)
  if !first_virtual_reg_stamp = -1 then begin
    first_virtual_reg_stamp := !currstamp;
    assert (Misc.Stdlib.List.is_empty !reg_list) (* Only hard regs created before now *)
  end;
  currstamp := !first_virtual_reg_stamp;
  reg_list := []

let all_registers() = !reg_list
let num_registers() = !currstamp

let reinit_reg r =
  r.loc <- Unknown

let reinit() =
  List.iter reinit_reg !reg_list

module RegOrder =
  struct
    type t = reg
    let compare r1 r2 = r1.stamp - r2.stamp
  end

module Set = Set.Make(RegOrder)
module Map = Map.Make(RegOrder)
module Tbl = Hashtbl.Make (struct
    type t = reg
    let equal r1 r2 = r1.stamp = r2.stamp
    let hash r = r.stamp
  end)

let add_set_array s v =
  match Array.length v with
    0 -> s
  | 1 -> Set.add v.(0) s
  | n -> let rec add_all i =
           if i >= n then s else Set.add v.(i) (add_all(i+1))
         in add_all 0

let diff_set_array s v =
  match Array.length v with
    0 -> s
  | 1 -> Set.remove v.(0) s
  | n -> let rec remove_all i =
           if i >= n then s else Set.remove v.(i) (remove_all(i+1))
         in remove_all 0

let inter_set_array s v =
  match Array.length v with
    0 -> Set.empty
  | 1 -> if Set.mem v.(0) s
         then Set.add v.(0) Set.empty
         else Set.empty
  | n -> let rec inter_all i =
           if i >= n then Set.empty
           else if Set.mem v.(i) s then Set.add v.(i) (inter_all(i+1))
           else inter_all(i+1)
         in inter_all 0

let disjoint_set_array s v =
  match Array.length v with
    0 -> true
  | 1 -> not (Set.mem v.(0) s)
  | n -> let rec disjoint_all i =
           if i >= n then true
           else if Set.mem v.(i) s then false
           else disjoint_all (i+1)
         in disjoint_all 0

let set_of_array v =
  match Array.length v with
    0 -> Set.empty
  | 1 -> Set.add v.(0) Set.empty
  | n -> let rec add_all i =
           if i >= n then Set.empty else Set.add v.(i) (add_all(i+1))
         in add_all 0

let set_has_collisions s =
  let phys_regs = Hashtbl.create (Int.min (Set.cardinal s) 32) in
  Set.fold (fun r acc ->
    match r.loc with
    | Reg id ->
      if Hashtbl.mem phys_regs id then true
      else (Hashtbl.add phys_regs id (); acc)
    | Unknown | Stack _ -> acc) s false

let equal_stack_location left right =
  match left, right with
  | Local left, Local right -> Int.equal left right
  | Incoming left, Incoming right -> Int.equal left right
  | Outgoing left, Outgoing right -> Int.equal left right
  | Domainstate left, Domainstate right -> Int.equal left right
  | Local _, (Incoming _ | Outgoing _ | Domainstate _)
  | Incoming _, (Local _ | Outgoing _ | Domainstate _)
  | Outgoing _, (Local _ | Incoming _ | Domainstate _)
  | Domainstate _, (Local _ | Incoming _ | Outgoing _)->
    false

let equal_location left right =
  match left, right with
  | Unknown, Unknown -> true
  | Reg left, Reg right -> Int.equal left right
  | Stack left, Stack right -> equal_stack_location left right
  | Unknown, (Reg _ | Stack _)
  | Reg _, (Unknown | Stack _)
  | Stack _, (Unknown | Reg _) ->
    false

let same_phys_reg left right =
  match left.loc, right.loc with
  | Reg l, Reg r -> Int.equal l r
  | (Reg _ | Unknown | Stack _), _ -> false

let same_loc left right =
  (* CR-soon azewierzejew: This should also compare [reg_class] for [Stack
     (Local _)]. That's complicated because [reg_class] is definied in [Proc]
     which relies on [Reg]. *)
  equal_location left.loc right.loc

let same left right =
  Int.equal left.stamp right.stamp

let compare left right =
  Int.compare left.stamp right.stamp
