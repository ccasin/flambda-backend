(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt     *)
(*                                                                        *)
(*   Copyright 1995 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib

type t : value mod portable contended
external create: unit -> t @@ portable = "caml_ml_condition_new"
external wait: t -> Mutex.t -> unit @@ portable = "caml_ml_condition_wait"
external signal: t -> unit @@ portable = "caml_ml_condition_signal"
external broadcast: t -> unit @@ portable = "caml_ml_condition_broadcast"
