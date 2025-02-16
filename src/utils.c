/* This file is released under the terms of an MIT-like license.
   See the attached LICENSE file.
   Copyright (C) 2000-2025 LexiFi                                    */

#define CAML_INTERNALS

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/major_gc.h>
#include <caml/minor_gc.h>
#include <caml/gc_ctrl.h>
#include <stdint.h>

#ifdef _MSC_VER
#include <intrin.h>
#pragma intrinsic(__rdtsc)
#endif

CAMLprim int64_t caml_highres_clock_native(value unit)
{
#if defined(__GNUC__)
#if defined(__aarch64__)
  uint64_t v;
  __asm__ __volatile__("mrs %0, cntvct_el0" : "=r"(v));
  return v;
#elif defined(__x86_64__)
  uint32_t hi = 0, lo = 0;
  __asm__ __volatile__("rdtsc" : "=a"(lo), "=d"(hi));
  return ((int64_t)lo) | (((int64_t)hi) << 32);
#else
  return 0;
#endif
#elif defined(_MSC_VER)
  int64_t v;
  v = __rdtsc();
  return v;
#else
  return 0;
#endif
}

CAMLprim value caml_highres_clock(value unit)
{
  return caml_copy_int64(caml_highres_clock_native(unit));
}

CAMLprim int64_t allocated_bytes_native(value v)
{
  double minwords = caml_stat_minor_words
                    + (double) (caml_young_alloc_end - caml_young_ptr);
  double prowords = caml_stat_promoted_words;
  double majwords = caml_stat_major_words + (double) caml_allocated_words;

  return (int64_t) ((minwords + majwords - prowords) * sizeof(value));
}

CAMLprim value allocated_bytes(value v)
{
  return caml_copy_int64(allocated_bytes_native(v));
}

CAMLprim int64_t allocated_bytes_major_native(value v)
{
  double majwords = caml_stat_major_words + (double) caml_allocated_words;

  return (int64_t) (majwords * sizeof(value));
}

CAMLprim value allocated_bytes_major(value v)
{
  return caml_copy_int64(allocated_bytes_major_native(v));
}
