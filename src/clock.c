/* This file is released under the terms of an MIT-like license.
   See the attached LICENSE file.
   Copyright 2016 by LexiFi.                                         */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <stdint.h>

CAMLprim value caml_highres_clock(value unit)
{
#if defined(_MSC_VER)
    int64_t v;
    v = __rdtsc();
    return caml_copy_int64(v);
#elif defined(__GNUC__)
    uint32_t hi = 0, lo = 0;
    __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
    return caml_copy_int64( ((int64_t)lo)|( ((int64_t)hi)<<32 ));
#else
    return 0;
#endif
}

