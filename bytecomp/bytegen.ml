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

(*  bytegen.ml : translation of lambda terms to lists of instructions. *)

open Misc
open Asttypes
open Primitive
open Lambda
open Switch
open Instruct
open Debuginfo.Scoped_location

(**** Label generation ****)

let label_counter = ref 0

let new_label () =
  incr label_counter;
  !label_counter

(**** Operations on compilation environments. ****)

let empty_env = { ce_stack = Ident.empty; ce_closure = Not_in_closure }

(* Add a stack-allocated variable *)

let add_var id pos env =
  { ce_stack = Ident.add id pos env.ce_stack; ce_closure = env.ce_closure }

let rec add_vars idlist pos env =
  match idlist with
  | [] -> env
  | id :: rem -> add_vars rem (pos + 1) (add_var id pos env)

(* Compute the closure environment *)

let rec add_positions entries pos_to_entry ~pos ~delta = function
  | [] -> entries, pos
  | id :: rem ->
    let entries = Ident.add id (pos_to_entry pos) entries in
    add_positions entries pos_to_entry ~pos:(pos + delta) ~delta rem

type function_definition =
  | Single_non_recursive
  | Multiple_recursive of Ident.t list

let closure_entries fun_defs fvs =
  let funct_entries, pos_end_functs =
    match fun_defs with
    | Single_non_recursive ->
      (* No need to store the function in the environment, but we still need to
         reserve a slot in the closure block *)
      Ident.empty, 3
    | Multiple_recursive functs ->
      add_positions Ident.empty (fun pos -> Function pos) ~pos:0 ~delta:3 functs
  in
  (* Note: [pos_end_functs] is the position where we would store the next
     function if there was one, and points after an eventual infix tag.
     Since that was the last function, we don't need the last infix tag
     and start storing free variables at [pos_end_functs - 1]. *)
  let all_entries, _end_pos =
    add_positions funct_entries
      (fun pos -> Free_variable pos)
      ~pos:(pos_end_functs - 1) ~delta:1 fvs
  in
  all_entries

(**** Examination of the continuation ****)

(* Return a label to the beginning of the given continuation.
   If the sequence starts with a branch, use the target of that branch
   as the label, thus avoiding a jump to a jump. *)

let label_code = function
  | Kbranch lbl :: _ as cont -> lbl, cont
  | Klabel lbl :: _ as cont -> lbl, cont
  | cont ->
    let lbl = new_label () in
    lbl, Klabel lbl :: cont

(* Return a branch to the continuation. That is, an instruction that,
   when executed, branches to the continuation or performs what the
   continuation performs. We avoid generating branches to branches and
   branches to returns. *)

let rec make_branch_2 lbl n cont = function
  | Kreturn m :: _ -> Kreturn (n + m), cont
  | Klabel _ :: c -> make_branch_2 lbl n cont c
  | Kpop m :: c -> make_branch_2 lbl (n + m) cont c
  | _ -> (
    match lbl with
    | Some lbl -> Kbranch lbl, cont
    | None ->
      let lbl = new_label () in
      Kbranch lbl, Klabel lbl :: cont)

let make_branch cont =
  match cont with
  | (Kbranch _ as branch) :: _ -> branch, cont
  | (Kreturn _ as return) :: _ -> return, cont
  | Kraise k :: _ -> Kraise k, cont
  | Klabel lbl :: _ -> make_branch_2 (Some lbl) 0 cont cont
  | _ -> make_branch_2 None 0 cont cont

(* Avoid a branch to a label that follows immediately *)

let branch_to label cont =
  match cont with
  | Klabel label0 :: _ when label = label0 -> cont
  | _ -> Kbranch label :: cont

(* Discard all instructions up to the next label.
   This function is to be applied to the continuation before adding a
   non-terminating instruction (branch, raise, return) in front of it. *)

let rec discard_dead_code = function
  | [] -> []
  | (Klabel _ | Krestart | Ksetglobal _) :: _ as cont -> cont
  | _ :: cont -> discard_dead_code cont

(* Check if we're in tailcall position *)

let rec is_tailcall = function
  | Kreturn _ :: _ -> true
  | Klabel _ :: c -> is_tailcall c
  | Kpop _ :: c -> is_tailcall c
  | _ -> false

(* Will this primitive result in an OCaml call which would benefit
   from the tail call optimization? *)

let preserve_tailcall_for_prim = function
  | Popaque _ | Psequor | Psequand | Pobj_magic _ | Prunstack | Pperform
  | Presume | Preperform
  | Pbox_float (_, _)
  | Punbox_float _
  | Pbox_vector (_, _)
  | Punbox_vector _ | Pbox_int _ | Punbox_int _ ->
    true
  | Pbytes_to_string | Pbytes_of_string | Parray_to_iarray | Parray_of_iarray
  | Pget_header _ | Pignore | Pgetglobal _ | Psetglobal _ | Pgetpredef _
  | Pmakeblock _ | Pmakefloatblock _ | Pmakeufloatblock _ | Pmakemixedblock _
  | Pmakelazyblock _ | Pfield _ | Pfield_computed _ | Psetfield _
  | Psetfield_computed _ | Pfloatfield _ | Psetfloatfield _ | Pduprecord _
  | Pufloatfield _ | Psetufloatfield _ | Pmixedfield _ | Psetmixedfield _
  | Pmake_unboxed_product _ | Punboxed_product_field _
  | Parray_element_size_in_bytes _ | Pccall _ | Praise _ | Pnot | Pnegint
  | Paddint | Psubint | Pmulint | Pdivint _ | Pmodint _ | Pandint | Porint
  | Pxorint | Plslint | Plsrint | Pasrint | Pintcomp _ | Poffsetint _
  | Poffsetref _ | Pintoffloat _
  | Pfloatofint (_, _)
  | Pfloatoffloat32 _ | Pfloat32offloat _
  | Pnegfloat (_, _)
  | Pabsfloat (_, _)
  | Paddfloat (_, _)
  | Psubfloat (_, _)
  | Pmulfloat (_, _)
  | Pdivfloat (_, _)
  | Pfloatcomp (_, _)
  | Punboxed_float_comp (_, _)
  | Pstringlength | Pstringrefu | Pstringrefs | Pcompare_ints
  | Pcompare_floats _ | Pcompare_bints _ | Pbyteslength | Pbytesrefu
  | Pbytessetu | Pbytesrefs | Pbytessets | Pmakearray _ | Pduparray _
  | Parraylength _ | Parrayrefu _ | Parraysetu _ | Pmakearray_dynamic _
  | Parrayblit _ | Parrayrefs _ | Parraysets _ | Pisint _ | Pisnull | Pisout
  | Pbintofint _ | Pintofbint _ | Pcvtbint _ | Pnegbint _ | Paddbint _
  | Psubbint _ | Pmulbint _ | Pdivbint _ | Pmodbint _ | Pandbint _ | Porbint _
  | Pxorbint _ | Plslbint _ | Plsrbint _ | Pasrbint _ | Pbintcomp _
  | Punboxed_int_comp _ | Pbigarrayref _ | Pbigarrayset _ | Pbigarraydim _
  | Pstring_load_16 _ | Pstring_load_32 _ | Pstring_load_f32 _
  | Pstring_load_64 _ | Pstring_load_128 _ | Pbytes_load_16 _ | Pbytes_load_32 _
  | Pbytes_load_f32 _ | Pbytes_load_64 _ | Pbytes_load_128 _ | Pbytes_set_16 _
  | Pbytes_set_32 _ | Pbytes_set_f32 _ | Pbytes_set_64 _ | Pbytes_set_128 _
  | Pbigstring_load_16 _ | Pbigstring_load_32 _ | Pbigstring_load_f32 _
  | Pbigstring_load_64 _ | Pbigstring_load_128 _ | Pbigstring_set_16 _
  | Pbigstring_set_32 _ | Pbigstring_set_f32 _ | Pfloatarray_load_128 _
  | Pfloat_array_load_128 _ | Pint_array_load_128 _
  | Punboxed_float_array_load_128 _ | Punboxed_float32_array_load_128 _
  | Punboxed_int32_array_load_128 _ | Punboxed_int64_array_load_128 _
  | Punboxed_nativeint_array_load_128 _ | Pfloatarray_set_128 _
  | Pfloat_array_set_128 _ | Pint_array_set_128 _
  | Punboxed_float_array_set_128 _ | Punboxed_float32_array_set_128 _
  | Punboxed_int32_array_set_128 _ | Punboxed_int64_array_set_128 _
  | Punboxed_nativeint_array_set_128 _ | Pbigstring_set_64 _
  | Pbigstring_set_128 _ | Pprobe_is_enabled _ | Pobj_dup | Pctconst _
  | Pbswap16 | Pbbswap _ | Pint_as_pointer _ | Patomic_exchange _
  | Patomic_compare_exchange _ | Patomic_compare_set _ | Patomic_fetch_add
  | Patomic_add | Patomic_sub | Patomic_land | Patomic_lor | Patomic_lxor
  | Patomic_load _ | Patomic_set _ | Pdls_get
  | Preinterpret_tagged_int63_as_unboxed_int64
  | Preinterpret_unboxed_int64_as_tagged_int63 | Ppoll | Ppeek _ | Ppoke _ ->
    false

(* Add a Kpop N instruction in front of a continuation *)

let rec add_pop n cont =
  if n = 0
  then cont
  else
    match cont with
    | Kpop m :: cont -> add_pop (n + m) cont
    | Kreturn m :: cont -> Kreturn (n + m) :: cont
    | Kraise _ :: _ -> cont
    | _ -> Kpop n :: cont

(* Add the constant "unit" in front of a continuation *)

let add_const_unit = function
  | (Kacc _ | Kconst _ | Kgetglobal _ | Kpush_retaddr _) :: _ as cont -> cont
  | cont -> Kconst const_unit :: cont

let rec push_dummies n k =
  match n with
  | 0 -> k
  | _ -> Kconst const_unit :: Kpush :: push_dummies (n - 1) k

(**** Merging consecutive events ****)

let copy_event ev kind info repr =
  { ev with
    ev_pos = 0;
    (* patched in emitcode *)
    ev_kind = kind;
    ev_info = info;
    ev_repr = repr
  }

let merge_infos ev ev' =
  match ev.ev_info, ev'.ev_info with
  | Event_other, info -> info
  | info, Event_other -> info
  | _ -> fatal_error "Bytegen.merge_infos"

let merge_repr ev ev' =
  match ev.ev_repr, ev'.ev_repr with
  | Event_none, x -> x
  | x, Event_none -> x
  | Event_parent r, Event_child r' when r == r' && !r = 1 -> Event_none
  | Event_child r, Event_parent r' when r == r' -> Event_parent r
  | _, _ -> fatal_error "Bytegen.merge_repr"

let merge_events ev ev' =
  let maj, min =
    match ev.ev_kind, ev'.ev_kind with
    (* Discard pseudo-events *)
    | Event_pseudo, _ -> ev', ev
    | _, Event_pseudo -> ev, ev'
    (* Keep following event, supposedly more informative *)
    | Event_before, (Event_after _ | Event_before) -> ev', ev
    (* Discard following events, supposedly less informative *)
    | Event_after _, (Event_after _ | Event_before) -> ev, ev'
  in
  copy_event maj maj.ev_kind (merge_infos maj min) (merge_repr maj min)

let weaken_event ev cont =
  match ev.ev_kind with
  | Event_after _ -> (
    match cont with
    | Kpush :: Kevent ({ ev_repr = Event_none } as ev') :: c -> (
      match ev.ev_info with
      | Event_return _ ->
        (* Weaken event *)
        let repr = ref 1 in
        let ev = copy_event ev Event_pseudo ev.ev_info (Event_parent repr)
        and ev' = copy_event ev' ev'.ev_kind ev'.ev_info (Event_child repr) in
        Kevent ev :: Kpush :: Kevent ev' :: c
      | _ ->
        (* Only keep following event, equivalent *)
        cont)
    | _ -> Kevent ev :: cont)
  | _ -> Kevent ev :: cont

let add_event ev = function
  | Kevent ev' :: cont -> weaken_event (merge_events ev ev') cont
  | cont -> weaken_event ev cont

(* Pseudo events are ignored by the debugger. They are only used for
   generating backtraces.

   We prefer adding this event here rather than in lambda generation
   1) there are many different situations where a Pmakeblock can
      be generated
   2) we prefer inserting a pseudo event rather than an event after
      to prevent the debugger to stop at every single allocation. *)
let add_pseudo_event loc modname c =
  if !Clflags.debug
  then
    let ev_defname = string_of_scoped_location ~include_zero_alloc:false loc in
    let ev =
      { ev_pos = 0;
        (* patched in emitcode *)
        ev_module = Compilation_unit.full_path_as_string modname;
        ev_loc = to_location loc;
        ev_defname;
        ev_kind = Event_pseudo;
        ev_info = Event_other;
        (* Dummy *)
        ev_typenv = Env.Env_empty;
        (* Dummy *)
        ev_typsubst = Subst.identity;
        (* Dummy *)
        ev_compenv = empty_env;
        (* Dummy *)
        ev_stacksize = 0;
        (* Dummy *)
        ev_repr = Event_none
      }
      (* Dummy *)
    in
    add_event ev c
  else c

(**** Compilation of a lambda expression ****)

type stack_info =
  { try_blocks : int list;
    (* list of stack size for each nested try block *)
    sz_static_raises : (int * (int * int * int list)) list;
    (* association staticraise numbers -> (lbl,size of stack, try_blocks *)
    max_stack_used : int ref
        (* Maximal stack size reached during the current function body *)
  }

let create_stack_info () =
  { try_blocks = []; sz_static_raises = []; max_stack_used = ref 0 }

(* association staticraise numbers -> (lbl,size of stack, try_blocks *)

let push_static_raise stack_info i lbl_handler sz =
  { stack_info with
    sz_static_raises =
      (i, (lbl_handler, sz, stack_info.try_blocks))
      :: stack_info.sz_static_raises
  }

let find_raise_label stack_info i =
  try List.assoc i stack_info.sz_static_raises
  with Not_found ->
    Misc.fatal_error ("exit(" ^ Int.to_string i ^ ") outside appropriated catch")

(* Will the translation of l lead to a jump to label ? *)
let code_as_jump stack_info l sz =
  match l with
  | Lstaticraise (i, []) ->
    let label, size, tb = find_raise_label stack_info i in
    if sz = size && tb == stack_info.try_blocks then Some label else None
  | _ -> None

(* Function bodies that remain to be compiled *)

type function_to_compile =
  { params : Ident.t list; (* function parameters *)
    body : lambda; (* the function body *)
    label : label; (* the label of the function entry *)
    entries : closure_entry Ident.tbl;
        (* the offsets for the free variables
           and mutually recursive functions *)
    rec_pos : int
  }
(* rank in recursive definition *)

let functions_to_compile = (Stack.create () : function_to_compile Stack.t)

(* Name of current compilation unit (for debugging events) *)

let compunit_name = ref Compilation_unit.dummy

let check_stack stack_info sz =
  let curr = stack_info.max_stack_used in
  if sz > !curr then curr := sz

(* Sequence of string tests *)

(* Translate a primitive to a bytecode instruction (possibly a call to a C
   function) *)

let comp_bint_primitive bi suff args =
  let pref =
    match bi with
    | Boxed_nativeint -> "caml_nativeint_"
    | Boxed_int32 -> "caml_int32_"
    | Boxed_int64 -> "caml_int64_"
  in
  Kccall (pref ^ suff, List.length args)

let indexing_primitive (index_kind : Lambda.array_index_kind) prefix =
  let suffix =
    match index_kind with
    | Ptagged_int_index -> ""
    | Punboxed_int_index Unboxed_int64 -> "_indexed_by_int64"
    | Punboxed_int_index Unboxed_int32 -> "_indexed_by_int32"
    | Punboxed_int_index Unboxed_nativeint -> "_indexed_by_nativeint"
  in
  prefix ^ suffix

let comp_primitive stack_info p sz args =
  check_stack stack_info sz;
  match p with
  | Pgetglobal cu -> Kgetglobal cu
  | Psetglobal cu -> Ksetglobal cu
  | Pgetpredef id -> Kgetpredef id
  | Pintcomp cmp -> Kintcomp cmp
  | Pcompare_ints -> Kccall ("caml_int_compare", 2)
  | Pcompare_floats Boxed_float64 -> Kccall ("caml_float_compare", 2)
  | Pcompare_floats Boxed_float32 -> Kccall ("caml_float32_compare", 2)
  | Pcompare_bints bi -> comp_bint_primitive bi "compare" args
  | Pfield (n, _ptr, _sem) -> Kgetfield n
  | Punboxed_product_field (n, _layouts) -> Kgetfield n
  | Parray_element_size_in_bytes _array_kind ->
    Kconst (Const_base (Const_int (Sys.word_size / 8)))
  | Pfield_computed _sem -> Kgetvectitem
  | Psetfield (n, _ptr, _init) -> Ksetfield n
  | Psetfield_computed (_ptr, _init) -> Ksetvectitem
  | Pfloatfield (n, _sem, _mode) -> Kgetfloatfield n
  | Psetfloatfield (n, _init) -> Ksetfloatfield n
  (* In bytecode, float#s are boxed.  So, we can use the existing float
     instructions for the ufloat primitives. *)
  | Pufloatfield (n, _sem) -> Kgetfloatfield n
  | Psetufloatfield (n, _init) -> Ksetfloatfield n
  | Pmixedfield (n, _, _sem) ->
    (* CR layouts: This will need reworking if we ever want bytecode
       to unbox fields that are written with unboxed types in the source
       language. *)
    (* Note, non-value mixed fields are always boxed in bytecode; they
       aren't stored flat like they are in native code.
    *)
    Kgetfield n
  | Psetmixedfield (n, _shape, _init) ->
    (* See the comment in the [Pmixedfield] case. *)
    Ksetfield n
  | Pduprecord _ -> Kccall ("caml_obj_dup", 1)
  | Pccall p -> Kccall (p.prim_name, p.prim_arity)
  | Pperform ->
    check_stack stack_info (sz + 4);
    Kperform
  | Pnegint -> Knegint
  | Paddint -> Kaddint
  | Psubint -> Ksubint
  | Pmulint -> Kmulint
  | Pdivint _ -> Kdivint
  | Pmodint _ -> Kmodint
  | Pandint -> Kandint
  | Porint -> Korint
  | Pxorint -> Kxorint
  | Plslint -> Klslint
  | Plsrint -> Klsrint
  | Pasrint -> Kasrint
  | Poffsetint n -> Koffsetint n
  | Poffsetref n -> Koffsetref n
  | Pintoffloat Boxed_float64 -> Kccall ("caml_int_of_float", 1)
  | Pfloatofint (Boxed_float64, _) -> Kccall ("caml_float_of_int", 1)
  | Pfloatoffloat32 _ -> Kccall ("caml_float_of_float32", 1)
  | Pfloat32offloat _ -> Kccall ("caml_float32_of_float", 1)
  | Pnegfloat (Boxed_float64, _) -> Kccall ("caml_neg_float", 1)
  | Pabsfloat (Boxed_float64, _) -> Kccall ("caml_abs_float", 1)
  | Paddfloat (Boxed_float64, _) -> Kccall ("caml_add_float", 2)
  | Psubfloat (Boxed_float64, _) -> Kccall ("caml_sub_float", 2)
  | Pmulfloat (Boxed_float64, _) -> Kccall ("caml_mul_float", 2)
  | Pdivfloat (Boxed_float64, _) -> Kccall ("caml_div_float", 2)
  | Pintoffloat Boxed_float32 -> Kccall ("caml_int_of_float32", 1)
  | Pfloatofint (Boxed_float32, _) -> Kccall ("caml_float32_of_int", 1)
  | Pnegfloat (Boxed_float32, _) -> Kccall ("caml_neg_float32", 1)
  | Pabsfloat (Boxed_float32, _) -> Kccall ("caml_abs_float32", 1)
  | Paddfloat (Boxed_float32, _) -> Kccall ("caml_add_float32", 2)
  | Psubfloat (Boxed_float32, _) -> Kccall ("caml_sub_float32", 2)
  | Pmulfloat (Boxed_float32, _) -> Kccall ("caml_mul_float32", 2)
  | Pdivfloat (Boxed_float32, _) -> Kccall ("caml_div_float32", 2)
  | Pstringlength -> Kccall ("caml_ml_string_length", 1)
  | Pbyteslength -> Kccall ("caml_ml_bytes_length", 1)
  | Pstringrefs -> Kccall ("caml_string_get", 2)
  | Pbytesrefs -> Kccall ("caml_bytes_get", 2)
  | Pbytessets -> Kccall ("caml_bytes_set", 3)
  | Pstringrefu -> Kgetstringchar
  | Pbytesrefu -> Kgetbyteschar
  | Pbytessetu -> Ksetbyteschar
  | Pstring_load_16 { index_kind; _ } ->
    Kccall (indexing_primitive index_kind "caml_string_get16", 2)
  | Pstring_load_32 { index_kind; _ } ->
    Kccall (indexing_primitive index_kind "caml_string_get32", 2)
  | Pstring_load_f32 { index_kind; _ } ->
    Kccall (indexing_primitive index_kind "caml_string_getf32", 2)
  | Pstring_load_64 { index_kind; _ } ->
    Kccall (indexing_primitive index_kind "caml_string_get64", 2)
  | Pbytes_set_16 { index_kind; _ } ->
    Kccall (indexing_primitive index_kind "caml_bytes_set16", 3)
  | Pbytes_set_32 { index_kind; _ } ->
    Kccall (indexing_primitive index_kind "caml_bytes_set32", 3)
  | Pbytes_set_f32 { index_kind; _ } ->
    Kccall (indexing_primitive index_kind "caml_bytes_setf32", 3)
  | Pbytes_set_64 { index_kind; _ } ->
    Kccall (indexing_primitive index_kind "caml_bytes_set64", 3)
  | Pbytes_load_16 { index_kind; _ } ->
    Kccall (indexing_primitive index_kind "caml_bytes_get16", 2)
  | Pbytes_load_32 { index_kind; _ } ->
    Kccall (indexing_primitive index_kind "caml_bytes_get32", 2)
  | Pbytes_load_f32 { index_kind; _ } ->
    Kccall (indexing_primitive index_kind "caml_bytes_getf32", 2)
  | Pbytes_load_64 { index_kind; _ } ->
    Kccall (indexing_primitive index_kind "caml_bytes_get64", 2)
  | Parraylength _ -> Kvectlength
  (* In bytecode, nothing is ever actually stack-allocated, so we ignore the
     array modes (allocation for [Parrayref{s,u}], modification for
     [Parrayset{s,u}]). *)
  | Parrayrefs (Pgenarray_ref _, index_kind, _)
  | Parrayrefs
      ( ( Paddrarray_ref | Pintarray_ref | Pfloatarray_ref _
        | Punboxedfloatarray_ref (Unboxed_float64 | Unboxed_float32)
        | Punboxedintarray_ref _ | Pgcscannableproductarray_ref _
        | Pgcignorableproductarray_ref _ ),
        (Punboxed_int_index _ as index_kind),
        _ ) ->
    Kccall (indexing_primitive index_kind "caml_array_get", 2)
  | Parrayrefs
      ( (Punboxedfloatarray_ref Unboxed_float64 | Pfloatarray_ref _),
        Ptagged_int_index,
        _ ) ->
    Kccall ("caml_floatarray_get", 2)
  | Parrayrefs
      ( ( Punboxedfloatarray_ref Unboxed_float32
        | Punboxedintarray_ref _ | Paddrarray_ref | Pintarray_ref
        | Pgcscannableproductarray_ref _ | Pgcignorableproductarray_ref _ ),
        Ptagged_int_index,
        _ ) ->
    Kccall ("caml_array_get_addr", 2)
  | Parraysets (Pgenarray_set _, index_kind)
  | Parraysets
      ( ( Paddrarray_set _ | Pintarray_set | Pfloatarray_set
        | Punboxedfloatarray_set (Unboxed_float64 | Unboxed_float32)
        | Punboxedintarray_set _ | Pgcscannableproductarray_set _
        | Pgcignorableproductarray_set _ ),
        (Punboxed_int_index _ as index_kind) ) ->
    Kccall (indexing_primitive index_kind "caml_array_set", 3)
  | Parraysets
      ( (Punboxedfloatarray_set Unboxed_float64 | Pfloatarray_set),
        Ptagged_int_index ) ->
    Kccall ("caml_floatarray_set", 3)
  | Parraysets
      ( ( Punboxedfloatarray_set Unboxed_float32
        | Punboxedintarray_set _ | Paddrarray_set _ | Pintarray_set
        | Pgcscannableproductarray_set _ | Pgcignorableproductarray_set _ ),
        Ptagged_int_index ) ->
    Kccall ("caml_array_set_addr", 3)
  | Parrayrefu (Pgenarray_ref _, index_kind, _)
  | Parrayrefu
      ( ( Paddrarray_ref | Pintarray_ref | Pfloatarray_ref _
        | Punboxedfloatarray_ref (Unboxed_float64 | Unboxed_float32)
        | Punboxedintarray_ref _ | Pgcscannableproductarray_ref _
        | Pgcignorableproductarray_ref _ ),
        (Punboxed_int_index _ as index_kind),
        _ ) ->
    Kccall (indexing_primitive index_kind "caml_array_unsafe_get", 2)
  | Parrayrefu
      ( (Punboxedfloatarray_ref Unboxed_float64 | Pfloatarray_ref _),
        Ptagged_int_index,
        _ ) ->
    Kccall ("caml_floatarray_unsafe_get", 2)
  | Parrayrefu
      ( ( Punboxedfloatarray_ref Unboxed_float32
        | Punboxedintarray_ref _ | Paddrarray_ref | Pintarray_ref
        | Pgcscannableproductarray_ref _ | Pgcignorableproductarray_ref _ ),
        Ptagged_int_index,
        _ ) ->
    Kgetvectitem
  | Parraysetu (Pgenarray_set _, index_kind)
  | Parraysetu
      ( ( Paddrarray_set _ | Pintarray_set | Pfloatarray_set
        | Punboxedfloatarray_set (Unboxed_float64 | Unboxed_float32)
        | Punboxedintarray_set _ | Pgcscannableproductarray_set _
        | Pgcignorableproductarray_set _ ),
        (Punboxed_int_index _ as index_kind) ) ->
    Kccall (indexing_primitive index_kind "caml_array_unsafe_set", 3)
  | Parraysetu
      ( (Punboxedfloatarray_set Unboxed_float64 | Pfloatarray_set),
        Ptagged_int_index ) ->
    Kccall ("caml_floatarray_unsafe_set", 3)
  | Parraysetu
      ( ( Punboxedfloatarray_set Unboxed_float32
        | Punboxedintarray_set _ | Paddrarray_set _ | Pintarray_set
        | Pgcscannableproductarray_set _ | Pgcignorableproductarray_set _ ),
        Ptagged_int_index ) ->
    Ksetvectitem
  | Parrayrefs (Punboxedvectorarray_ref _, _, _)
  | Parraysets (Punboxedvectorarray_set _, _)
  | Parrayrefu (Punboxedvectorarray_ref _, _, _)
  | Parraysetu (Punboxedvectorarray_set _, _) ->
    fatal_error "SIMD is not supported in bytecode mode."
  | Pctconst c ->
    let const_name =
      match c with
      | Big_endian -> "big_endian"
      | Word_size -> "word_size"
      | Int_size -> "int_size"
      | Max_wosize -> "max_wosize"
      | Ostype_unix -> "ostype_unix"
      | Ostype_win32 -> "ostype_win32"
      | Ostype_cygwin -> "ostype_cygwin"
      | Backend_type -> "backend_type"
      | Runtime5 -> "runtime5"
    in
    Kccall (Printf.sprintf "caml_sys_const_%s" const_name, 1)
  | Pisint _ -> Kisint
  | Pisout -> Kisout
  | Pbintofint (bi, _) -> comp_bint_primitive bi "of_int" args
  | Pintofbint bi -> comp_bint_primitive bi "to_int" args
  | Pcvtbint (src, dst, _) -> (
    match src, dst with
    | Boxed_int32, Boxed_nativeint -> Kccall ("caml_nativeint_of_int32", 1)
    | Boxed_nativeint, Boxed_int32 -> Kccall ("caml_nativeint_to_int32", 1)
    | Boxed_int32, Boxed_int64 -> Kccall ("caml_int64_of_int32", 1)
    | Boxed_int64, Boxed_int32 -> Kccall ("caml_int64_to_int32", 1)
    | Boxed_nativeint, Boxed_int64 -> Kccall ("caml_int64_of_nativeint", 1)
    | Boxed_int64, Boxed_nativeint -> Kccall ("caml_int64_to_nativeint", 1)
    | (Boxed_int32 | Boxed_int64 | Boxed_nativeint), _ ->
      fatal_error "Bytegen.comp_primitive: invalid Pcvtbint cast")
  | Pnegbint (bi, _) -> comp_bint_primitive bi "neg" args
  | Paddbint (bi, _) -> comp_bint_primitive bi "add" args
  | Psubbint (bi, _) -> comp_bint_primitive bi "sub" args
  | Pmulbint (bi, _) -> comp_bint_primitive bi "mul" args
  | Pdivbint { size = bi } -> comp_bint_primitive bi "div" args
  | Pmodbint { size = bi } -> comp_bint_primitive bi "mod" args
  | Pandbint (bi, _) -> comp_bint_primitive bi "and" args
  | Porbint (bi, _) -> comp_bint_primitive bi "or" args
  | Pxorbint (bi, _) -> comp_bint_primitive bi "xor" args
  | Plslbint (bi, _) -> comp_bint_primitive bi "shift_left" args
  | Plsrbint (bi, _) -> comp_bint_primitive bi "shift_right_unsigned" args
  | Pasrbint (bi, _) -> comp_bint_primitive bi "shift_right" args
  | Pbintcomp (_, Ceq) | Punboxed_int_comp (_, Ceq) -> Kccall ("caml_equal", 2)
  | Pbintcomp (_, Cne) | Punboxed_int_comp (_, Cne) ->
    Kccall ("caml_notequal", 2)
  | Pbintcomp (_, Clt) | Punboxed_int_comp (_, Clt) ->
    Kccall ("caml_lessthan", 2)
  | Pbintcomp (_, Cgt) | Punboxed_int_comp (_, Cgt) ->
    Kccall ("caml_greaterthan", 2)
  | Pbintcomp (_, Cle) | Punboxed_int_comp (_, Cle) ->
    Kccall ("caml_lessequal", 2)
  | Pbintcomp (_, Cge) | Punboxed_int_comp (_, Cge) ->
    Kccall ("caml_greaterequal", 2)
  | Pbigarrayref (_, n, Pbigarray_float32_t, _) ->
    Kccall ("caml_ba_float32_get_" ^ Int.to_string n, n + 1)
  | Pbigarrayset (_, n, Pbigarray_float32_t, _) ->
    Kccall ("caml_ba_float32_set_" ^ Int.to_string n, n + 2)
  | Pbigarrayref (_, n, _, _) -> Kccall ("caml_ba_get_" ^ Int.to_string n, n + 1)
  | Pbigarrayset (_, n, _, _) -> Kccall ("caml_ba_set_" ^ Int.to_string n, n + 2)
  | Pbigarraydim n -> Kccall ("caml_ba_dim_" ^ Int.to_string n, 1)
  | Pbigstring_load_16 { unsafe = _; index_kind } ->
    Kccall (indexing_primitive index_kind "caml_ba_uint8_get16", 2)
  | Pbigstring_load_32 { unsafe = _; mode = _; index_kind } ->
    Kccall (indexing_primitive index_kind "caml_ba_uint8_get32", 2)
  | Pbigstring_load_f32 { unsafe = _; mode = _; index_kind } ->
    Kccall (indexing_primitive index_kind "caml_ba_uint8_getf32", 2)
  | Pbigstring_load_64 { unsafe = _; mode = _; index_kind } ->
    Kccall (indexing_primitive index_kind "caml_ba_uint8_get64", 2)
  | Pbigstring_set_16 { unsafe = _; index_kind } ->
    Kccall (indexing_primitive index_kind "caml_ba_uint8_set16", 3)
  | Pbigstring_set_32 { unsafe = _; index_kind } ->
    Kccall (indexing_primitive index_kind "caml_ba_uint8_set32", 3)
  | Pbigstring_set_f32 { unsafe = _; index_kind } ->
    Kccall (indexing_primitive index_kind "caml_ba_uint8_setf32", 3)
  | Pbigstring_set_64 { unsafe = _; index_kind } ->
    Kccall (indexing_primitive index_kind "caml_ba_uint8_set64", 3)
  | Pbswap16 -> Kccall ("caml_bswap16", 1)
  | Pbbswap (bi, _) -> comp_bint_primitive bi "bswap" args
  | Pint_as_pointer _ -> Kccall ("caml_int_as_pointer", 1)
  | Pbytes_to_string -> Kccall ("caml_string_of_bytes", 1)
  | Pbytes_of_string -> Kccall ("caml_bytes_of_string", 1)
  | Parray_to_iarray -> Kccall ("caml_iarray_of_array", 1)
  | Parray_of_iarray -> Kccall ("caml_array_of_iarray", 1)
  | Pget_header _ -> Kccall ("caml_get_header", 1)
  | Pobj_dup -> Kccall ("caml_obj_dup", 1)
  | Patomic_load _ -> Kccall ("caml_atomic_load", 1)
  | Patomic_set _ | Patomic_exchange _ -> Kccall ("caml_atomic_exchange", 2)
  | Patomic_compare_exchange _ -> Kccall ("caml_atomic_compare_exchange", 3)
  | Patomic_compare_set _ -> Kccall ("caml_atomic_cas", 3)
  | Patomic_fetch_add -> Kccall ("caml_atomic_fetch_add", 2)
  | Patomic_add -> Kccall ("caml_atomic_add", 2)
  | Patomic_sub -> Kccall ("caml_atomic_sub", 2)
  | Patomic_land -> Kccall ("caml_atomic_land", 2)
  | Patomic_lor -> Kccall ("caml_atomic_lor", 2)
  | Patomic_lxor -> Kccall ("caml_atomic_lxor", 2)
  | Pdls_get -> Kccall ("caml_domain_dls_get", 1)
  | Ppoll -> Kccall ("caml_process_pending_actions_with_root", 1)
  | Pisnull -> Kccall ("caml_is_null", 1)
  | Pstring_load_128 _ | Pbytes_load_128 _ | Pbytes_set_128 _
  | Pbigstring_load_128 _ | Pbigstring_set_128 _ | Pfloatarray_load_128 _
  | Pfloat_array_load_128 _ | Pint_array_load_128 _
  | Punboxed_float_array_load_128 _ | Punboxed_float32_array_load_128 _
  | Punboxed_int32_array_load_128 _ | Punboxed_int64_array_load_128 _
  | Punboxed_nativeint_array_load_128 _ | Pfloatarray_set_128 _
  | Pfloat_array_set_128 _ | Pint_array_set_128 _
  | Punboxed_float_array_set_128 _ | Punboxed_float32_array_set_128 _
  | Punboxed_int32_array_set_128 _ | Punboxed_int64_array_set_128 _
  | Punboxed_nativeint_array_set_128 _ | Pbox_vector _ | Punbox_vector _ ->
    fatal_error "SIMD is not supported in bytecode mode."
  | Preinterpret_tagged_int63_as_unboxed_int64 ->
    if not (Target_system.is_64_bit ())
    then
      Misc.fatal_error
        "Preinterpret_tagged_int63_as_unboxed_int64 can only be used on 64-bit \
         targets";
    Kccall ("caml_reinterpret_tagged_int63_as_unboxed_int64", 1)
  | Preinterpret_unboxed_int64_as_tagged_int63 ->
    if not (Target_system.is_64_bit ())
    then
      Misc.fatal_error
        "Preinterpret_unboxed_int64_as_tagged_int63 can only be used on 64-bit \
         targets";
    Kccall ("caml_reinterpret_unboxed_int64_as_tagged_int63", 1)
  | Pmakearray_dynamic (kind, locality, With_initializer) -> (
    if List.compare_length_with args 2 <> 0
    then
      fatal_error
        "Bytegen.comp_primitive: Pmakearray_dynamic takes two arguments for \
         [With_initializer]";
    (* CR layouts v4.0: This is "wrong" for unboxed types. It should construct
       blocks that can't be marshalled. We've decided to ignore that problem in
       the short term, as it's unlikely to cause issues - see the internal arrays
       epic for out plan to deal with it. *)
    (match kind with
    | Punboxedvectorarray _ ->
      fatal_error "SIMD is not supported in bytecode mode."
    | Pgenarray | Pintarray | Paddrarray | Punboxedintarray _ | Pfloatarray
    | Punboxedfloatarray _ | Pgcscannableproductarray _
    | Pgcignorableproductarray _ ->
      ());
    match locality with
    | Alloc_heap -> Kccall ("caml_make_vect", 2)
    | Alloc_local -> Kccall ("caml_make_local_vect", 2))
  | Parrayblit { src_mutability = _; dst_array_set_kind } ->
    (match dst_array_set_kind with
    | Punboxedvectorarray_set _ ->
      fatal_error "SIMD is not supported in bytecode mode."
    | Pgenarray_set _ | Pintarray_set | Paddrarray_set _
    | Punboxedintarray_set _ | Pfloatarray_set | Punboxedfloatarray_set _
    | Pgcscannableproductarray_set _ | Pgcignorableproductarray_set _ ->
      ());
    Kccall ("caml_array_blit", 5)
  | Pmakearray_dynamic (_, _, Uninitialized) ->
    Misc.fatal_error
      "Pmakearray_dynamic Uninitialized should have been translated to \
       Pmakearray_dynamic Initialized earlier on"
  (* The cases below are handled in [comp_expr] before the [comp_primitive] call
     (in the order in which they appear below),
     so they should never be reached in this function. *)
  | Prunstack | Presume | Preperform | Pignore | Popaque _ | Pobj_magic _ | Pnot
  | Psequand | Psequor | Praise _ | Pmakearray _ | Pduparray _
  | Pfloatcomp (_, _)
  | Punboxed_float_comp (_, _)
  | Pmakeblock _ | Pmake_unboxed_product _ | Pmakefloatblock _
  | Pmakeufloatblock _ | Pmakemixedblock _ | Pmakelazyblock _
  | Pprobe_is_enabled _ | Punbox_float _
  | Pbox_float (_, _)
  | Punbox_int _ | Pbox_int _ ->
    fatal_error "Bytegen.comp_primitive"
  | Ppeek _ | Ppoke _ ->
    fatal_error "Bytegen.comp_primitive: Ppeek/Ppoke not supported in bytecode"

let is_immed n = immed_min <= n && n <= immed_max

let is_nontail = function
  | Rc_nontail -> true
  | Rc_normal | Rc_close_at_apply -> false

module Storer = Switch.Store (struct
  type t = lambda

  type key = lambda

  let compare_key = Stdlib.compare

  let make_key = Lambda.make_key
end)

(* Compile an expression.
   The value of the expression is left in the accumulator.
   env = compilation environment
   exp = the lambda expression to compile
   sz = current size of the stack frame
   cont = list of instructions to execute afterwards
   Result = list of instructions that evaluate exp, then perform cont. *)

(* CR dkalinichenko: this error happens because we run tests
   under [flambda_backend/tests] with the boot compiler instead of the
   final compiler. Run them using the final compiler.*)
(* We cannot use the [float32] or [or_null] types in the compiler. *)
external is_boot_compiler : unit -> bool = "caml_is_boot_compiler"

external float32_of_string : string -> Obj.t = "caml_float32_of_string"

let rec contains_float32s_or_nulls = function
  | Const_base (Const_float32 _ | Const_unboxed_float32 _) | Const_null -> true
  | Const_block (_, fields) -> List.exists contains_float32s_or_nulls fields
  | Const_mixed_block _ ->
    Misc.fatal_error "[Const_mixed_block] not supported in bytecode."
  | _ -> false

let rec translate_float32s_or_nulls stack_info env cst sz cont =
  match cst with
  | Const_base (Const_float32 f | Const_unboxed_float32 f) ->
    let i = float32_of_string f in
    Kconst (Const_base (Const_int32 (Obj.obj i)))
    :: Kccall ("caml_float32_of_bits_bytecode", 1)
    :: cont
  | Const_null ->
    Kconst (Const_base (Const_int 0))
    :: Kccall ("caml_int_as_pointer", 1)
    :: cont
  | Const_block (tag, fields) as cst when contains_float32s_or_nulls cst ->
    let fields = List.map (fun field -> Lconst field) fields in
    let cont = Kmakeblock (List.length fields, tag) :: cont in
    comp_args stack_info env fields sz cont
  | Const_mixed_block _ ->
    Misc.fatal_error "[Const_mixed_block] not supported in bytecode."
  | _ as cst -> Kconst cst :: cont

and comp_expr stack_info env exp sz cont =
  check_stack stack_info sz;
  match exp with
  | Lvar id | Lmutvar id -> (
    try
      let pos = Ident.find_same id env.ce_stack in
      Kacc (sz - pos) :: cont
    with Not_found -> (
      let not_found () =
        fatal_error ("Bytegen.comp_expr: var " ^ Ident.unique_name id)
      in
      match env.ce_closure with
      | Not_in_closure -> not_found ()
      | In_closure { entries; env_pos } -> (
        match Ident.find_same id entries with
        | Free_variable pos -> Kenvacc (pos - env_pos) :: cont
        | Function pos -> Koffsetclosure (pos - env_pos) :: cont
        | exception Not_found -> not_found ())))
  | Lconst cst when is_boot_compiler () ->
    translate_float32s_or_nulls stack_info env cst sz cont
  | Lconst cst -> Kconst cst :: cont
  | Lapply { ap_func = func; ap_args = args; ap_region_close = rc } ->
    let nargs = List.length args in
    if is_tailcall cont && not (is_nontail rc)
    then
      comp_args stack_info env args sz
        (Kpush
        :: comp_expr stack_info env func (sz + nargs)
             (Kappterm (nargs, sz + nargs) :: discard_dead_code cont))
    else if nargs < 4
    then
      comp_args stack_info env args sz
        (Kpush
        :: comp_expr stack_info env func (sz + nargs) (Kapply nargs :: cont))
    else
      let lbl, cont1 = label_code cont in
      Kpush_retaddr lbl
      :: comp_args stack_info env args (sz + 3)
           (Kpush
           :: comp_expr stack_info env func
                (sz + 3 + nargs)
                (Kapply nargs :: cont1))
  | Lsend (kind, met, obj, args, rc, _, _, _) ->
    assert (kind <> Cached);
    let nargs = List.length args + 1 in
    let getmethod, args' =
      if kind = Self
      then Kgetmethod, met :: obj :: args
      else
        match met with
        | Lconst (Const_base (Const_int n)) -> Kgetpubmet n, obj :: args
        | _ -> Kgetdynmet, met :: obj :: args
    in
    if is_tailcall cont && not (is_nontail rc)
    then
      comp_args stack_info env args' sz
        (getmethod :: Kappterm (nargs, sz + nargs) :: discard_dead_code cont)
    else if nargs < 4
    then comp_args stack_info env args' sz (getmethod :: Kapply nargs :: cont)
    else
      let lbl, cont1 = label_code cont in
      Kpush_retaddr lbl
      :: comp_args stack_info env args' (sz + 3)
           (getmethod :: Kapply nargs :: cont1)
  | Lfunction { params; body; loc } ->
    (* assume kind = Curried *)
    let cont = add_pseudo_event loc !compunit_name cont in
    let lbl = new_label () in
    let fv = Ident.Set.elements (free_variables exp) in
    let entries = closure_entries Single_non_recursive fv in
    let to_compile =
      { params = List.map (fun p -> p.name) params;
        body;
        label = lbl;
        entries;
        rec_pos = 0
      }
    in
    Stack.push to_compile functions_to_compile;
    comp_args stack_info env
      (List.map (fun n -> Lvar n) fv)
      sz
      (Kclosure (lbl, List.length fv) :: cont)
  | Llet (_, _k, id, arg, body) | Lmutlet (_k, id, arg, body) ->
    comp_expr stack_info env arg sz
      (Kpush
      :: comp_expr stack_info
           (add_var id (sz + 1) env)
           body (sz + 1) (add_pop 1 cont))
  | Lletrec (decl, body) ->
    let ndecl = List.length decl in
    let fv =
      Ident.Set.elements (free_variables (Lletrec (decl, lambda_unit)))
    in
    let rec_idents = List.map (fun { id } -> id) decl in
    let entries = closure_entries (Multiple_recursive rec_idents) fv in
    let rec comp_fun pos = function
      | [] -> []
      | { def = { params; body } } :: rem ->
        let lbl = new_label () in
        let to_compile =
          { params = List.map (fun p -> p.name) params;
            body;
            label = lbl;
            entries;
            rec_pos = pos
          }
        in
        Stack.push to_compile functions_to_compile;
        lbl :: comp_fun (pos + 1) rem
    in
    let lbls = comp_fun 0 decl in
    comp_args stack_info env
      (List.map (fun n -> Lvar n) fv)
      sz
      (Kclosurerec (lbls, List.length fv)
      :: comp_expr stack_info
           (add_vars rec_idents (sz + 1) env)
           body (sz + ndecl) (add_pop ndecl cont))
  | Lprim ((Popaque _ | Pobj_magic _), [arg], _) ->
    comp_expr stack_info env arg sz cont
  | Lprim
      ( ( Pbox_float ((Boxed_float64 | Boxed_float32), _)
        | Punbox_float (Boxed_float64 | Boxed_float32) ),
        [arg],
        _ ) ->
    comp_expr stack_info env arg sz cont
  | Lprim ((Pbox_int _ | Punbox_int _), [arg], _) ->
    comp_expr stack_info env arg sz cont
  | Lprim (Pignore, [arg], _) ->
    comp_expr stack_info env arg sz (add_const_unit cont)
  | Lprim (Pnot, [arg], _) ->
    let newcont =
      match cont with
      | Kbranchif lbl :: cont1 -> Kbranchifnot lbl :: cont1
      | Kbranchifnot lbl :: cont1 -> Kbranchif lbl :: cont1
      | _ -> Kboolnot :: cont
    in
    comp_expr stack_info env arg sz newcont
  | Lprim (Psequand, [exp1; exp2], _) -> (
    match cont with
    | Kbranchifnot lbl :: _ ->
      comp_expr stack_info env exp1 sz
        (Kbranchifnot lbl :: comp_expr stack_info env exp2 sz cont)
    | Kbranchif lbl :: cont1 ->
      let lbl2, cont2 = label_code cont1 in
      comp_expr stack_info env exp1 sz
        (Kbranchifnot lbl2
        :: comp_expr stack_info env exp2 sz (Kbranchif lbl :: cont2))
    | _ ->
      let lbl, cont1 = label_code cont in
      comp_expr stack_info env exp1 sz
        (Kstrictbranchifnot lbl :: comp_expr stack_info env exp2 sz cont1))
  | Lprim (Psequor, [exp1; exp2], _) -> (
    match cont with
    | Kbranchif lbl :: _ ->
      comp_expr stack_info env exp1 sz
        (Kbranchif lbl :: comp_expr stack_info env exp2 sz cont)
    | Kbranchifnot lbl :: cont1 ->
      let lbl2, cont2 = label_code cont1 in
      comp_expr stack_info env exp1 sz
        (Kbranchif lbl2
        :: comp_expr stack_info env exp2 sz (Kbranchifnot lbl :: cont2))
    | _ ->
      let lbl, cont1 = label_code cont in
      comp_expr stack_info env exp1 sz
        (Kstrictbranchif lbl :: comp_expr stack_info env exp2 sz cont1))
  | Lprim (Praise k, [arg], _) ->
    comp_expr stack_info env arg sz (Kraise k :: discard_dead_code cont)
  | Lprim (Paddint, [arg; Lconst (Const_base (Const_int n))], _) when is_immed n
    ->
    comp_expr stack_info env arg sz (Koffsetint n :: cont)
  | Lprim (Psubint, [arg; Lconst (Const_base (Const_int n))], _)
    when is_immed (-n) ->
    comp_expr stack_info env arg sz (Koffsetint (-n) :: cont)
  | Lprim (Poffsetint n, [arg], _) when not (is_immed n) ->
    comp_expr stack_info env arg sz
      (Kpush :: Kconst (Const_base (Const_int n)) :: Kaddint :: cont)
  | Lprim ((Pmakefloatblock _ | Pmakeufloatblock _), args, loc) ->
    (* In bytecode, float# is boxed, so we can treat these two primitives the
       same. *)
    let cont = add_pseudo_event loc !compunit_name cont in
    comp_args stack_info env args sz (Kmakefloatblock (List.length args) :: cont)
  | Lprim (Pmakemixedblock (tag, _, shape, _), args, loc) ->
    (* There is no notion of a mixed block at runtime in bytecode. Further,
       source-level unboxed types are represented as boxed in bytecode, so
       no ceremony is needed to box values before inserting them into
       the (normal, unmixed) block.
    *)
    let total_len = Array.length shape in
    let cont = add_pseudo_event loc !compunit_name cont in
    comp_args stack_info env args sz
      (Kmake_faux_mixedblock (total_len, tag) :: cont)
  | Lprim (Pmakearray (kind, _, _), args, loc) -> (
    let cont = add_pseudo_event loc !compunit_name cont in
    match kind with
    (* arrays of unboxed types have the same representation
       as the boxed ones on bytecode *)
    | Pintarray | Paddrarray | Punboxedintarray _
    | Punboxedfloatarray Unboxed_float32
    | Pgcscannableproductarray _ | Pgcignorableproductarray _ ->
      comp_args stack_info env args sz (Kmakeblock (List.length args, 0) :: cont)
    | Pfloatarray | Punboxedfloatarray Unboxed_float64 ->
      comp_args stack_info env args sz
        (Kmakefloatblock (List.length args) :: cont)
    | Punboxedvectorarray _ ->
      fatal_error "SIMD is not supported in bytecode mode."
    | Pgenarray ->
      if args = []
      then Kmakeblock (0, 0) :: cont
      else
        comp_args stack_info env args sz
          (Kmakeblock (List.length args, 0)
          :: Kccall ("caml_make_array", 1)
          :: cont))
  | Lprim (Presume, args, _) ->
    let nargs = List.length args - 1 in
    assert (nargs = 3);
    if is_tailcall cont
    then (
      (* Resumeterm itself only pushes 2 words, but perform adds another *)
      check_stack stack_info 3;
      comp_args stack_info env args sz
        (Kresumeterm (sz + nargs) :: discard_dead_code cont))
    else (
      (* Resume itself only pushes 2 words, but perform adds another *)
      check_stack stack_info (sz + nargs + 3);
      comp_args stack_info env args sz (Kresume :: cont))
  | Lprim (Prunstack, args, _) ->
    let nargs = List.length args in
    assert (nargs = 3);
    if is_tailcall cont
    then (
      (* Resumeterm itself only pushes 2 words, but perform adds another *)
      check_stack stack_info 3;
      Kconst const_unit :: Kpush
      :: comp_args stack_info env args (sz + 1)
           (Kresumeterm (sz + nargs) :: discard_dead_code cont))
    else (
      (* Resume itself only pushes 2 words, but perform adds another *)
      check_stack stack_info (sz + nargs + 3);
      Kconst const_unit :: Kpush
      :: comp_args stack_info env args (sz + 1) (Kresume :: cont))
  | Lprim (Preperform, args, _) ->
    let nargs = List.length args - 1 in
    assert (nargs = 2);
    check_stack stack_info (sz + 3);
    if is_tailcall cont
    then
      comp_args stack_info env args sz
        (Kreperformterm (sz + nargs) :: discard_dead_code cont)
    else fatal_error "Reperform used in non-tail position"
  | Lprim (Pmakearray_dynamic (kind, locality, Uninitialized), [len], loc) ->
    (* Use a dummy initializer to implement the "uninitialized" primitive *)
    let init =
      match kind with
      | Pgenarray | Paddrarray | Pintarray | Pfloatarray
      | Pgcscannableproductarray _ ->
        Misc.fatal_errorf
          "Array kind %s should have been ruled out by the frontend for \
           %%makearray_dynamic_uninit"
          (Printlambda.array_kind kind)
      | Punboxedfloatarray Unboxed_float32 ->
        Lconst (Const_base (Const_float32 "0.0"))
      | Punboxedfloatarray Unboxed_float64 ->
        Lconst (Const_base (Const_float "0.0"))
      | Punboxedintarray Unboxed_int32 -> Lconst (Const_base (Const_int32 0l))
      | Punboxedintarray Unboxed_int64 -> Lconst (Const_base (Const_int64 0L))
      | Punboxedintarray Unboxed_nativeint ->
        Lconst (Const_base (Const_nativeint 0n))
      | Punboxedvectorarray _ ->
        fatal_error "SIMD is not supported in bytecode mode."
      | Pgcignorableproductarray ignorables ->
        let rec convert_ignorable (ign : Lambda.ignorable_product_element_kind)
            =
          match ign with
          | Pint_ignorable -> Lconst (Const_base (Const_int 0))
          | Punboxedfloat_ignorable Unboxed_float32 ->
            Lconst (Const_base (Const_float32 "0.0"))
          | Punboxedfloat_ignorable Unboxed_float64 ->
            Lconst (Const_base (Const_float "0.0"))
          | Punboxedint_ignorable Unboxed_int32 ->
            Lconst (Const_base (Const_int32 0l))
          | Punboxedint_ignorable Unboxed_int64 ->
            Lconst (Const_base (Const_int64 0L))
          | Punboxedint_ignorable Unboxed_nativeint ->
            Lconst (Const_base (Const_nativeint 0n))
          | Pproduct_ignorable ignorables ->
            let fields = List.map convert_ignorable ignorables in
            Lprim (Pmakeblock (0, Immutable, None, alloc_heap), fields, loc)
        in
        convert_ignorable (Pproduct_ignorable ignorables)
    in
    comp_expr stack_info env
      (Lprim
         ( Pmakearray_dynamic (kind, locality, With_initializer),
           [len; init],
           loc ))
      sz cont
  | Lprim (Pmakearray_dynamic (_, _, Uninitialized), _, _loc) ->
    Misc.fatal_error "Pmakearray_dynamic takes one arg when [Uninitialized]"
  | Lprim
      ( Pduparray (kind, mutability),
        [Lprim (Pmakearray (kind', _, m), args, _)],
        loc ) ->
    assert (kind = kind');
    comp_expr stack_info env
      (Lprim (Pmakearray (kind, mutability, m), args, loc))
      sz cont
  | Lprim (Pduparray _, [arg], loc) ->
    let prim_obj_dup =
      Lambda.simple_prim_on_values ~name:"caml_obj_dup" ~arity:1 ~alloc:true
    in
    comp_expr stack_info env (Lprim (Pccall prim_obj_dup, [arg], loc)) sz cont
  | Lprim (Pduparray _, _, _) ->
    Misc.fatal_error "Bytegen.comp_expr: Pduparray takes exactly one arg"
  (* Integer first for enabling further optimization (cf. emitcode.ml)  *)
  | Lprim (Pintcomp c, [arg; (Lconst _ as k)], _) ->
    let p = Pintcomp (swap_integer_comparison c) and args = [k; arg] in
    let nargs = List.length args - 1 in
    comp_args stack_info env args sz
      (comp_primitive stack_info p (sz + nargs - 1) args :: cont)
  | Lprim (Pfloatcomp (Boxed_float64, cmp), args, _)
  | Lprim (Punboxed_float_comp (Unboxed_float64, cmp), args, _) ->
    let cont =
      match cmp with
      | CFeq -> Kccall ("caml_eq_float", 2) :: cont
      | CFneq -> Kccall ("caml_neq_float", 2) :: cont
      | CFlt -> Kccall ("caml_lt_float", 2) :: cont
      | CFnlt -> Kccall ("caml_lt_float", 2) :: Kboolnot :: cont
      | CFgt -> Kccall ("caml_gt_float", 2) :: cont
      | CFngt -> Kccall ("caml_gt_float", 2) :: Kboolnot :: cont
      | CFle -> Kccall ("caml_le_float", 2) :: cont
      | CFnle -> Kccall ("caml_le_float", 2) :: Kboolnot :: cont
      | CFge -> Kccall ("caml_ge_float", 2) :: cont
      | CFnge -> Kccall ("caml_ge_float", 2) :: Kboolnot :: cont
    in
    comp_args stack_info env args sz cont
  | Lprim (Pfloatcomp (Boxed_float32, cmp), args, _)
  | Lprim (Punboxed_float_comp (Unboxed_float32, cmp), args, _) ->
    let cont =
      match cmp with
      | CFeq -> Kccall ("caml_eq_float32", 2) :: cont
      | CFneq -> Kccall ("caml_neq_float32", 2) :: cont
      | CFlt -> Kccall ("caml_lt_float32", 2) :: cont
      | CFnlt -> Kccall ("caml_lt_float32", 2) :: Kboolnot :: cont
      | CFgt -> Kccall ("caml_gt_float32", 2) :: cont
      | CFngt -> Kccall ("caml_gt_float32", 2) :: Kboolnot :: cont
      | CFle -> Kccall ("caml_le_float32", 2) :: cont
      | CFnle -> Kccall ("caml_le_float32", 2) :: Kboolnot :: cont
      | CFge -> Kccall ("caml_ge_float32", 2) :: cont
      | CFnge -> Kccall ("caml_ge_float32", 2) :: Kboolnot :: cont
    in
    comp_args stack_info env args sz cont
  | Lprim (Pmakeblock (tag, _mut, _, _), args, loc) ->
    let cont = add_pseudo_event loc !compunit_name cont in
    comp_args stack_info env args sz (Kmakeblock (List.length args, tag) :: cont)
  | Lprim (Pmakelazyblock Lazy_tag, args, loc) ->
    let cont = add_pseudo_event loc !compunit_name cont in
    comp_args stack_info env args sz
      (Kmakeblock (List.length args, Config.lazy_tag) :: cont)
  | Lprim (Pmakelazyblock Forward_tag, args, loc) ->
    let cont = add_pseudo_event loc !compunit_name cont in
    comp_args stack_info env args sz
      (Kmakeblock (List.length args, Obj.forward_tag) :: cont)
  | Lprim (Pmake_unboxed_product _, args, loc) ->
    let cont = add_pseudo_event loc !compunit_name cont in
    comp_args stack_info env args sz (Kmakeblock (List.length args, 0) :: cont)
  | Lprim (Pfloatfield (n, _, _), args, loc) ->
    let cont = add_pseudo_event loc !compunit_name cont in
    comp_args stack_info env args sz (Kgetfloatfield n :: cont)
  | Lprim (p, args, _) ->
    let nargs = List.length args - 1 in
    comp_args stack_info env args sz
      (comp_primitive stack_info p (sz + nargs - 1) args :: cont)
  | Lstaticcatch (body, (i, vars), handler, _, _) ->
    let vars = List.map fst vars in
    let nvars = List.length vars in
    let branch1, cont1 = make_branch cont in
    let r =
      if nvars <> 1
      then
        (* general case *)
        let lbl_handler, cont2 =
          label_code
            (comp_expr stack_info
               (add_vars vars (sz + 1) env)
               handler (sz + nvars) (add_pop nvars cont1))
        in
        let stack_info =
          push_static_raise stack_info i lbl_handler (sz + nvars)
        in
        push_dummies nvars
          (comp_expr stack_info env body (sz + nvars)
             (add_pop nvars (branch1 :: cont2)))
      else
        (* small optimization for nvars = 1 *)
        let var = match vars with [var] -> var | _ -> assert false in
        let lbl_handler, cont2 =
          label_code
            (Kpush
            :: comp_expr stack_info
                 (add_var var (sz + 1) env)
                 handler (sz + 1) (add_pop 1 cont1))
        in
        let stack_info = push_static_raise stack_info i lbl_handler sz in
        comp_expr stack_info env body sz (branch1 :: cont2)
    in
    r
  | Lstaticraise (i, args) -> (
    let cont = discard_dead_code cont in
    let label, size, tb = find_raise_label stack_info i in
    let cont = branch_to label cont in
    let rec loop sz tbb =
      if tb == tbb
      then add_pop (sz - size) cont
      else
        match tbb with
        | [] -> assert false
        | try_sz :: tbb ->
          add_pop (sz - try_sz - 4) (Kpoptrap :: loop try_sz tbb)
    in
    let cont = loop sz stack_info.try_blocks in
    match args with
    | [arg] ->
      (* optim, argument passed in accumulator *)
      comp_expr stack_info env arg sz cont
    | _ -> comp_exit_args stack_info env args sz size cont)
  | Ltrywith (body, id, handler, _kind) ->
    let branch1, cont1 = make_branch cont in
    let lbl_handler = new_label () in
    let body_cont =
      Kpoptrap :: branch1 :: Klabel lbl_handler :: Kpush
      :: comp_expr stack_info
           (add_var id (sz + 1) env)
           handler (sz + 1) (add_pop 1 cont1)
    in
    let stack_info =
      { stack_info with try_blocks = sz :: stack_info.try_blocks }
    in
    let l = comp_expr stack_info env body (sz + 4) body_cont in
    Kpushtrap lbl_handler :: l
  | Lifthenelse (cond, ifso, ifnot, _kind) ->
    comp_binary_test stack_info env cond ifso ifnot sz cont
  | Lsequence (exp1, exp2) ->
    comp_expr stack_info env exp1 sz (comp_expr stack_info env exp2 sz cont)
  | Lwhile { wh_cond; wh_body } ->
    let lbl_loop = new_label () in
    let lbl_test = new_label () in
    Kbranch lbl_test :: Klabel lbl_loop :: Kcheck_signals
    :: comp_expr stack_info env wh_body sz
         (Klabel lbl_test
         :: comp_expr stack_info env wh_cond sz
              (Kbranchif lbl_loop :: add_const_unit cont))
  | Lfor { for_id; for_from; for_to; for_dir; for_body } ->
    let lbl_loop = new_label () in
    let lbl_exit = new_label () in
    let offset = match for_dir with Upto -> 1 | Downto -> -1 in
    let comp = match for_dir with Upto -> Cgt | Downto -> Clt in
    comp_expr stack_info env for_from sz
      (Kpush
      :: comp_expr stack_info env for_to (sz + 1)
           (Kpush :: Kpush :: Kacc 2 :: Kintcomp comp :: Kbranchif lbl_exit
          :: Klabel lbl_loop :: Kcheck_signals
           :: comp_expr stack_info
                (add_var for_id (sz + 1) env)
                for_body (sz + 2)
                (Kacc 1 :: Kpush :: Koffsetint offset :: Kassign 2 :: Kacc 1
               :: Kintcomp Cne :: Kbranchif lbl_loop :: Klabel lbl_exit
                :: add_const_unit (add_pop 2 cont))))
  | Lswitch (arg, sw, _loc, _kind) ->
    let branch, cont1 = make_branch cont in
    let c = ref (discard_dead_code cont1) in
    (* Build indirection vectors *)
    let store = Storer.mk_store () in
    let act_consts = Array.make sw.sw_numconsts 0
    and act_blocks = Array.make sw.sw_numblocks 0 in
    (match sw.sw_failaction with
    (* default is index 0 *)
    | Some fail -> ignore (store.act_store () fail)
    | None -> ());
    List.iter
      (fun (n, act) -> act_consts.(n) <- store.act_store () act)
      sw.sw_consts;
    List.iter
      (fun (n, act) -> act_blocks.(n) <- store.act_store () act)
      sw.sw_blocks;
    (* Compile and label actions *)
    let acts = store.act_get () in
    (*
      let a = store.act_get_shared () in
      Array.iter
        (function
          | Switch.Shared (Lstaticraise _) -> ()
          | Switch.Shared act ->
              Printlambda.lambda Format.str_formatter act ;
              Printf.eprintf "SHARE BYTE:\n%s\n" (Format.flush_str_formatter ())
          | _ -> ())
        a ;
*)
    let lbls = Array.make (Array.length acts) 0 in
    for i = Array.length acts - 1 downto 0 do
      let lbl, c1 =
        label_code (comp_expr stack_info env acts.(i) sz (branch :: !c))
      in
      lbls.(i) <- lbl;
      c := discard_dead_code c1
    done;
    (* Build label vectors *)
    let lbl_blocks = Array.make sw.sw_numblocks 0 in
    for i = sw.sw_numblocks - 1 downto 0 do
      lbl_blocks.(i) <- lbls.(act_blocks.(i))
    done;
    let lbl_consts = Array.make sw.sw_numconsts 0 in
    for i = sw.sw_numconsts - 1 downto 0 do
      lbl_consts.(i) <- lbls.(act_consts.(i))
    done;
    comp_expr stack_info env arg sz (Kswitch (lbl_consts, lbl_blocks) :: !c)
  | Lstringswitch (arg, sw, d, loc, kind) ->
    comp_expr stack_info env
      (Matching.expand_stringswitch loc kind arg sw d)
      sz cont
  | Lassign (id, expr) -> (
    try
      let pos = Ident.find_same id env.ce_stack in
      comp_expr stack_info env expr sz (Kassign (sz - pos) :: cont)
    with Not_found -> fatal_error "Bytegen.comp_expr: assign")
  | Levent (lam, lev) -> (
    let ev_defname =
      string_of_scoped_location ~include_zero_alloc:false lev.lev_loc
    in
    let event kind info =
      { ev_pos = 0;
        (* patched in emitcode *)
        ev_module = Compilation_unit.full_path_as_string !compunit_name;
        ev_loc = to_location lev.lev_loc;
        ev_kind = kind;
        ev_defname;
        ev_info = info;
        ev_typenv = Env.summary lev.lev_env;
        ev_typsubst = Subst.identity;
        ev_compenv = env;
        ev_stacksize = sz;
        ev_repr =
          (match lev.lev_repr with
          | None -> Event_none
          | Some ({ contents = 1 } as repr) when lev.lev_kind = Lev_function ->
            Event_child repr
          | Some ({ contents = 1 } as repr) -> Event_parent repr
          | Some repr when lev.lev_kind = Lev_function -> Event_parent repr
          | Some repr -> Event_child repr)
      }
    in
    match lev.lev_kind with
    | Lev_before ->
      let c = comp_expr stack_info env lam sz cont in
      let ev = event Event_before Event_other in
      add_event ev c
    | Lev_function ->
      let c = comp_expr stack_info env lam sz cont in
      let ev = event Event_pseudo Event_function in
      add_event ev c
    | Lev_pseudo ->
      let c = comp_expr stack_info env lam sz cont in
      let ev = event Event_pseudo Event_other in
      add_event ev c
    | Lev_after ty ->
      let preserve_tailcall =
        match lam with
        | Lprim (prim, _, _) -> preserve_tailcall_for_prim prim
        | Lapply { ap_region_close = rc; _ } | Lsend (_, _, _, _, rc, _, _, _)
          ->
          not (is_nontail rc)
        | _ -> true
      in
      if preserve_tailcall && is_tailcall cont
      then (* don't destroy tail call opt *)
        comp_expr stack_info env lam sz cont
      else
        let info =
          match lam with
          | Lapply { ap_args = args } -> Event_return (List.length args)
          | Lsend (_, _, _, args, _, _, _, _) ->
            Event_return (List.length args + 1)
          | Lprim (_, args, _) -> Event_return (List.length args)
          | _ -> Event_other
        in
        let ev = event (Event_after ty) info in
        let cont1 = add_event ev cont in
        comp_expr stack_info env lam sz cont1)
  | Lifused (_, exp) -> comp_expr stack_info env exp sz cont
  | Lregion (exp, _) -> comp_expr stack_info env exp sz cont
  | Lexclave exp -> comp_expr stack_info env exp sz cont

(* Compile a list of arguments [e1; ...; eN] to a primitive operation.
   The values of eN ... e2 are pushed on the stack, e2 at top of stack,
   then e3, then ... The value of e1 is left in the accumulator. *)

and comp_args stack_info env argl sz cont =
  comp_expr_list stack_info env (List.rev argl) sz cont

and comp_expr_list stack_info env exprl sz cont =
  match exprl with
  | [] -> cont
  | [exp] -> comp_expr stack_info env exp sz cont
  | exp :: rem ->
    comp_expr stack_info env exp sz
      (Kpush :: comp_expr_list stack_info env rem (sz + 1) cont)

and comp_exit_args stack_info env argl sz pos cont =
  comp_expr_list_assign stack_info env (List.rev argl) sz pos cont

and comp_expr_list_assign stack_info env exprl sz pos cont =
  match exprl with
  | [] -> cont
  | exp :: rem ->
    comp_expr stack_info env exp sz
      (Kassign (sz - pos)
      :: comp_expr_list_assign stack_info env rem sz (pos - 1) cont)

(* Compile an if-then-else test. *)

and comp_binary_test stack_info env cond ifso ifnot sz cont =
  let cont_cond =
    if ifnot = Lconst const_unit
    then
      let lbl_end, cont1 = label_code cont in
      Kstrictbranchifnot lbl_end :: comp_expr stack_info env ifso sz cont1
    else
      match code_as_jump stack_info ifso sz with
      | Some label ->
        let cont = comp_expr stack_info env ifnot sz cont in
        Kbranchif label :: cont
      | None -> (
        match code_as_jump stack_info ifnot sz with
        | Some label ->
          let cont = comp_expr stack_info env ifso sz cont in
          Kbranchifnot label :: cont
        | None ->
          let branch_end, cont1 = make_branch cont in
          let lbl_not, cont2 =
            label_code (comp_expr stack_info env ifnot sz cont1)
          in
          Kbranchifnot lbl_not
          :: comp_expr stack_info env ifso sz (branch_end :: cont2))
  in
  comp_expr stack_info env cond sz cont_cond

(**** Compilation of a code block (with tracking of stack usage) ****)

let comp_block env exp sz cont =
  let stack_info = create_stack_info () in
  let code = comp_expr stack_info env exp sz cont in
  let used_safe = !(stack_info.max_stack_used) + Config.stack_safety_margin in
  if used_safe > Config.stack_threshold
  then
    Kconst (Const_base (Const_int used_safe))
    :: Kccall ("caml_ensure_stack_capacity", 1)
    :: code
  else code

(**** Compilation of functions ****)

let comp_function tc cont =
  let arity = List.length tc.params in
  let ce_stack, _last_pos =
    add_positions Ident.empty Fun.id ~pos:arity ~delta:(-1) tc.params
  in
  let env =
    { ce_stack;
      ce_closure = In_closure { entries = tc.entries; env_pos = 3 * tc.rec_pos }
    }
  in
  let cont = comp_block env tc.body arity (Kreturn arity :: cont) in
  if arity > 1
  then Krestart :: Klabel tc.label :: Kgrab (arity - 1) :: cont
  else Klabel tc.label :: cont

let comp_remainder cont =
  let c = ref cont in
  (try
     while true do
       c := comp_function (Stack.pop functions_to_compile) !c
     done
   with Stack.Empty -> ());
  !c

(**** Compilation of a lambda phrase ****)

let reset () =
  label_counter := 0;
  compunit_name := Compilation_unit.dummy;
  Stack.clear functions_to_compile

let compile_gen ?modulename ~init_stack expr =
  reset ();
  (match modulename with Some name -> compunit_name := name | None -> ());
  Fun.protect ~finally:reset (fun () ->
      let init_code = comp_block empty_env expr init_stack [] in
      if Stack.length functions_to_compile > 0
      then
        let lbl_init = new_label () in
        Kbranch lbl_init :: comp_remainder (Klabel lbl_init :: init_code), false
      else init_code, true)

let compile_implementation modulename expr =
  fst (compile_gen ~modulename ~init_stack:0 expr)

let compile_phrase expr = compile_gen ~init_stack:1 expr
