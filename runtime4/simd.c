/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                      Max Slater, Jane Street                           */
/*                                                                        */
/*   Copyright 2023 Jane Street Group LLC                                 */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <string.h>
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/simd.h"

#define Max_array_wosize                   (Max_wosize)
#define Max_custom_array_wosize            (Max_wosize - 1)

// SIMD is only supported on 64-bit targets
#define Max_unboxed_vec128_array_wosize    (Max_custom_array_wosize / 2)

#if defined(ARCH_SSE2) || defined(__ARM_NEON)

CAMLexport value caml_copy_vec128(simd_float32x4_t v) {
    value res = caml_alloc_small(2, Abstract_tag);
    Store_vec128_val(res, v);
    return res;
}

CAMLexport value caml_copy_vec128i(simd_int128_t v) {
    value res = caml_alloc_small(2, Abstract_tag);
    Store_vec128_vali(res, v);
    return res;
}

CAMLexport value caml_copy_vec128d(simd_float64x2_t v) {
    value res = caml_alloc_small(2, Abstract_tag);
    Store_vec128_vald(res, v);
    return res;
}

/* Defined in array.c */

CAMLextern int caml_unboxed_array_no_polymorphic_compare(value v1, value v2);
CAMLextern intnat caml_unboxed_array_no_polymorphic_hash(value v);
CAMLextern void caml_unboxed_array_serialize(value v, uintnat* bsize_32, uintnat* bsize_64);
CAMLextern uintnat caml_unboxed_array_deserialize(void* dst);

CAMLexport struct custom_operations caml_unboxed_vec128_array_ops = {
  "_unboxed_vec128_array",
  custom_finalize_default,
  caml_unboxed_array_no_polymorphic_compare,
  caml_unboxed_array_no_polymorphic_hash,
  caml_unboxed_array_serialize,
  caml_unboxed_array_deserialize,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value caml_unboxed_vec128_vect_blit(value a1, value ofs1, value a2,
                                             value ofs2, value n) {
    // Need to skip the custom_operations field
    memmove((simd_poly128_t *)((uintnat *)a2 + 1) + Long_val(ofs2),
            (simd_poly128_t *)((uintnat *)a1 + 1) + Long_val(ofs1),
            Long_val(n) * sizeof(simd_poly128_t));
    return Val_unit;
}

static value caml_make_unboxed_vec128_vect0(value len, int local)
{
  /* This is only used on 64-bit targets. */

  mlsize_t num_elements = Long_val(len);
  if (num_elements > Max_unboxed_vec128_array_wosize)
    caml_invalid_argument("Array.make");

  /* [num_fields] does not include the custom operations field. */
  mlsize_t num_fields = num_elements * 2;

  if (local)
    return caml_alloc_custom_local(&caml_unboxed_vec128_array_ops,
      num_fields * sizeof(value), 0, 0);
  else
    return caml_alloc_custom(&caml_unboxed_vec128_array_ops,
      num_fields * sizeof(value), 0, 0);
}

CAMLprim value caml_make_unboxed_vec128_vect(value len)
{
  return caml_make_unboxed_vec128_vect0(len, 0);
}

CAMLprim value caml_make_local_unboxed_vec128_vect(value len)
{
  return caml_make_unboxed_vec128_vect0(len, 1);
}

CAMLprim value caml_make_unboxed_vec128_vect_bytecode(value len) {
  caml_failwith("SIMD is not supported on this platform.");
}

#else

CAMLprim value caml_unboxed_vec128_vect_blit(value a1, value ofs1, value a2,
                                             value ofs2, value n) {
  caml_failwith("SIMD is not supported on this platform.");
}

CAMLprim value caml_make_unboxed_vec128_vect(value len) {
  caml_failwith("SIMD is not supported on this platform.");
}

CAMLprim value caml_make_local_unboxed_vec128_vect(value len) {
  caml_failwith("SIMD is not supported on this platform.");
}

CAMLprim value caml_make_unboxed_vec128_vect_bytecode(value len) {
  caml_failwith("SIMD is not supported on this platform.");
}

#endif // defined(ARCH_SSE2) || defined(__ARM_NEON)
