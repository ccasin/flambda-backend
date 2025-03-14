@@ portable
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Stephen Dolan, University of Cambridge                 *)
(*             Gabriel Scherer, projet Partout, INRIA Paris-Saclay        *)
(*                                                                        *)
(*   Copyright 2017-2018 University of Cambridge.                         *)
(*   Copyright 2020 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Atomic references.

  See {{!examples} the examples} below.
  See 'Memory model: The hard bits' chapter in the manual.

    @since 4.12
*)

(** An atomic (mutable) reference to a value of type ['a]. *)
type !'a t : value mod portable contended

(** Create an atomic reference. *)
val make : 'a -> 'a t @@ nonportable
[@@alert unsafe_multidomain "Use [Atomic.Safe.make]."]

(** Create an atomic reference that is alone on a cache line. It occupies 4-16x
    the memory of one allocated with [make v].

    The primary purpose is to prevent false-sharing and the resulting
    performance degradation. When a CPU performs an atomic operation, it
    temporarily takes ownership of an entire cache line that contains the
    atomic reference. If multiple atomic references share the same cache line,
    modifying these disjoint memory regions simultaneously becomes impossible,
    which can create a bottleneck. Hence, as a general guideline, if an atomic
    reference is experiencing contention, assigning it its own cache line may
    enhance performance.

    CR ocaml 5 all-runtime5: does not support runtime4 *)

val make_contended : 'a -> 'a t @@ nonportable
[@@alert unsafe_multidomain "Use [Atomic.Safe.make_contended]."]

(** Get the current value of the atomic reference. *)
val get : 'a t -> 'a @@ nonportable
[@@alert unsafe_multidomain "Use [Atomic.Safe.get]."]

(** Set a new value for the atomic reference. *)
external set : 'a t -> 'a -> unit @@ nonportable = "%atomic_set"
[@@alert unsafe_multidomain "Use [Atomic.Safe.set]."]

(** Set a new value for the atomic reference, and return the current value. *)
external exchange : 'a t -> 'a -> 'a @@ nonportable = "%atomic_exchange"
[@@alert unsafe_multidomain "Use [Atomic.Safe.exchange]."]

(** [compare_and_set r seen v] sets the new value of [r] to [v] only
    if its current value is physically equal to [seen] -- the
    comparison and the set occur atomically. Returns [true] if the
    comparison succeeded (so the set happened) and [false]
    otherwise. *)
external compare_and_set : 'a t -> 'a -> 'a -> bool @@ nonportable = "%atomic_cas"
[@@alert unsafe_multidomain "Use [Atomic.Safe.compare_and_set]."]

(** [compare_exchange r seen v] sets the new value of [r] to [v] only
    if its current value is physically equal to [seen] -- the comparison
    and the set occur atomically. Returns the previous value. *)
external compare_exchange : 'a t -> 'a -> 'a -> 'a @@ nonportable = "%atomic_compare_exchange"
[@@alert unsafe_multidomain "Use [Atomic.Safe.compare_exchange]."]

(** [fetch_and_add r n] atomically increments the value of [r] by [n],
    and returns the current value (before the increment). *)
val fetch_and_add : int t -> int -> int

(** [add r i] atomically adds [i] onto [r]. *)
val add : int t -> int -> unit

(** [sub r i] atomically subtracts [i] onto [r]. *)
val sub : int t -> int -> unit

(** [logand r i] atomically bitwise-ands [i] onto [r]. *)
val logand : int t -> int -> unit

(** [logor r i] atomically bitwise-ors [i] onto [r]. *)
val logor : int t -> int -> unit

(** [logxor r i] atomically bitwise-xors [i] onto [r]. *)
val logxor : int t -> int -> unit

(** [incr r] atomically increments the value of [r] by [1]. *)
val incr : int t -> unit @@ portable

(** [decr r] atomically decrements the value of [r] by [1]. *)
val decr : int t -> unit @@ portable

(** Submodule containing non-backwards-compatible functions which enforce thread safety
    via modes. *)
module Safe : sig @@ portable
  (** Like {!make}, but is safe to call in the presence of multiple domains.

      The provided value must be [portable] as atomics can freely cross between domains.
  *)
  val make : 'a @ portable contended -> 'a t

  (** Like {!make_contended}, but is safe to call in the presence of multiple domains.

      The provided value must be [portable] as atomics can freely cross between domains.
  *)
  val make_contended : 'a @ portable contended -> 'a t

  (** Like {!get}, but is safe to call in the presence of multiple domains.

      The resulting value must be [contended] as the atomics can freely cross between
      domains, so the value may come from another domain. *)
  val get : 'a t -> 'a @ portable contended

  (** Like {!set}, but is safe to call in the presence of multiple domains.

      The provided value must be [portable] as atomics can freely cross between domains.
  *)
  external set : 'a t -> 'a @ portable contended -> unit = "%atomic_set"

  (** Like {!exchange}, but is safe to call in the presence of multiple domains.

      The provided value must be [portable] and the resulting value must be [contended]
      as atomics can freely cross between domains. *)
  external exchange
    : 'a t -> 'a @ portable contended -> 'a @ portable contended = "%atomic_exchange"

  (** Like {!compare_and_set}, but is safe to call in the presence of multiple domains.

      The provided values must be [portable] as atomics can freely cross between domains.
  *)
  external compare_and_set :
    'a t -> 'a @ portable contended -> 'a @ portable contended -> bool = "%atomic_cas"

  (** Like {!compare_exchange}, but is safe to call in the presence of multiple domains.

      The provided values must be [portable] and the resulting value must be [contended]
      as atomics can freely cross between domains. *)
  external compare_exchange :
    'a t -> 'a @ portable contended -> 'a @ portable contended -> 'a @ portable contended
    = "%atomic_compare_exchange"
end

(** {1:examples Examples}

    {2 Basic Thread Coordination}

    A basic use case is to have global counters that are updated in a
    thread-safe way, for example to keep some sorts of metrics
    over IOs performed by the program. Another basic use case is to coordinate
    the termination of threads in a given program, for example when one thread
    finds an answer, or when the program is shut down by the user.

    Here, for example, we're going to try to find a number whose hash
    satisfies a basic property. To do that, we'll run multiple threads which
    will try random numbers until they find one that works.

    Of course the output below is a sample run and will change every time
    the program is run.

    {[
    (* use for termination *)
    let stop_all_threads = Atomic.make false

    (* total number of individual attempts to find a number *)
    let num_attempts = Atomic.make 0

    (* find a number that satisfies [p], by... trying random numbers
       until one fits. *)
    let find_number_where (p:int -> bool) =
      let rand = Random.State.make_self_init() in
      while not (Atomic.get stop_all_threads) do

        let n = Random.State.full_int rand max_int in
        ignore (Atomic.fetch_and_add num_attempts 1 : int);

        if p (Hashtbl.hash n) then (
          Printf.printf "found %d (hash=%d)\n%!" n (Hashtbl.hash n);
          Atomic.set stop_all_threads true; (* signal all threads to stop *)
        )
      done;;


    (* run multiple domains to search for a [n] where [hash n <= 100] *)
    let () =
      let criterion n = n <= 100 in
      let threads =
        Array.init 8
          (fun _ -> Domain.spawn (fun () -> find_number_where criterion))
      in
      Array.iter Domain.join threads;
      Printf.printf "total number of attempts: %d\n%!"
        (Atomic.get num_attempts) ;;

    - : unit = ()
    found 1651745641680046833 (hash=33)
    total number of attempts: 30230350
    ]}

    {2 Treiber Stack}

    Another example is a basic
    {{: https://en.wikipedia.org/wiki/Treiber_stack} Treiber stack}
    (a thread-safe stack) that can be safely shared between threads.

    Note how both [push] and [pop] are recursive, because they attempt to
    swap the new stack (with one more, or one fewer, element) with the old
    stack.
    This is optimistic concurrency: each iteration of, say, [push stack x]
    gets the old stack [l], and hopes that by the time it tries to replace
    [l] with [x::l], nobody else has had time to modify the list. If the
    [compare_and_set] fails it means we were too optimistic, and must try
    again.

    {[
    type 'a stack = 'a list Atomic.t

    let rec push (stack: _ stack) elt : unit =
      let cur = Atomic.get stack in
      let success = Atomic.compare_and_set stack cur (elt :: cur) in
      if not success then
        push stack elt

    let rec pop (stack: _ stack) : _ option =
      let cur = Atomic.get stack in
      match cur with
      | [] -> None
      | x :: tail ->
        let success = Atomic.compare_and_set stack cur tail in
        if success then Some x
        else pop stack

    # let st = Atomic.make []
    # push st 1
    - : unit = ()
    # push st 2
    - : unit = ()
    # pop st
    - : int option = Some 2
    # pop st
    - : int option = Some 1
    # pop st
    - : int option = None
    ]}
  *)
