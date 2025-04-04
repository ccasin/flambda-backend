# 2 "marshal.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib

[@@@ocaml.flambda_o3]

type extern_flags =
    No_sharing
  | Closures
  | Compat_32

(* note: this type definition is used in 'runtime/debugger.c' *)

external to_channel: ('a : value_or_null)
  . out_channel -> 'a -> extern_flags list -> unit @@ portable
  = "caml_output_value"
external to_bytes: ('a : value_or_null)
  . 'a -> extern_flags list -> bytes @@ portable
  = "caml_output_value_to_bytes"
external to_string: ('a : value_or_null)
  . 'a -> extern_flags list -> string @@ portable
  = "caml_output_value_to_string"
external to_buffer_unsafe:
      ('a : value_or_null)
      . bytes -> int -> int -> 'a -> extern_flags list -> int @@ portable
    = "caml_output_value_to_buffer"

let to_buffer buff ofs len v flags =
  if ofs < 0 || len < 0 || ofs > Bytes.length buff - len
  then invalid_arg "Marshal.to_buffer: substring out of bounds"
  else to_buffer_unsafe buff ofs len v flags

(* The functions below use byte sequences as input, never using any
   mutation. It makes sense to use non-mutated [bytes] rather than
   [string], because we really work with sequences of bytes, not
   a text representation.
*)

external from_channel: ('a : value_or_null)
  . in_channel -> 'a @@ portable
  = "caml_input_value"
external from_bytes_unsafe: ('a : value_or_null)
  . bytes -> int -> 'a @@ portable
  = "caml_input_value_from_bytes"
external data_size_unsafe: ('a : value_or_null)
  . bytes -> int -> int @@ portable
  = "caml_marshal_data_size"

let header_size = 16

let data_size buff ofs =
  if ofs < 0 || ofs > Bytes.length buff - header_size
  then invalid_arg "Marshal.data_size"
  else data_size_unsafe buff ofs
let total_size buff ofs = header_size + data_size buff ofs

let from_bytes buff ofs =
  if ofs < 0 || ofs > Bytes.length buff - header_size
  then invalid_arg "Marshal.from_bytes"
  else begin
    let len = data_size_unsafe buff ofs in
    if ofs > Bytes.length buff - (header_size + len)
    then invalid_arg "Marshal.from_bytes"
    else from_bytes_unsafe buff ofs
  end

let from_string buff ofs =
  (* Bytes.unsafe_of_string is safe here, as the produced byte
     sequence is never mutated *)
  from_bytes (Bytes.unsafe_of_string buff) ofs
