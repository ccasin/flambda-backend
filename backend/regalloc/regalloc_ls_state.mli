[@@@ocaml.warning "+a-30-40-41-42"]

open! Regalloc_ls_utils
module DLL = Flambda_backend_utils.Doubly_linked_list

type t

val for_fatal : t -> Interval.t DLL.t * ClassIntervals.t array

val make : stack_slots:Regalloc_stack_slots.t -> last_used:InstructionId.t -> t

val update_intervals : t -> Interval.t Reg.Tbl.t -> unit

val iter_intervals : t -> f:(Interval.t -> unit) -> unit

val fold_intervals : t -> f:('a -> Interval.t -> 'a) -> init:'a -> 'a

val release_expired_intervals : t -> pos:int -> unit

val active : t -> reg_class:int -> ClassIntervals.t

val active_classes : t -> ClassIntervals.t array

val stack_slots : t -> Regalloc_stack_slots.t

val get_and_incr_instruction_id : t -> InstructionId.t

val invariant_intervals : t -> Cfg_with_infos.t -> unit

val invariant_active : t -> unit
