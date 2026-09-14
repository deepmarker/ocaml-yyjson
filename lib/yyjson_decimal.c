/* Decimal strings read in place.

   Market data sends prices and sizes as JSON strings holding a plain decimal
   ("62817.99"), and a consumer that needs them exact wants the mantissa and
   exponent rather than a double. Reading one through ml_yyjson_get_str copies
   it into an OCaml string before anything can parse it; this parses the bytes
   where yyjson already holds them.

   The grammar is deliberately narrow: an optional sign, digits with at most
   one point, and nothing else -- no exponent, no whitespace.

   The result is one tagged OCaml int, mantissa << 5 | (exponent + 31), for a
   mantissa within +/-(2^57 - 1) and an exponent in [-31, 0]. That int is
   Jsondec.Decimal.t's immediate form as it stands, so the stub is noalloc,
   needs no out-parameter, and the decoder calling it allocates nothing
   either. Anything else -- not a string, not such a decimal, or one that only
   fits boxed -- returns Min_long, whose mantissa would be -2^57, outside the
   range. Jsondec then parses the copied string, which boxes it or refuses it.

   Trailing zeros after the point are the only digits whose dropping keeps the
   value, and the rule for them is Jsondec.Decimal.of_string's: kept if they
   fit the immediate form, dropped if that makes it fit. The two must agree on
   every string, which test_jsondec checks.

   Two alternatives were measured behind the earlier (int64 * int) contract
   and lost. Writing the exponent into an int ref took 7.8 ns and 10 words a
   field against 7.2 ns and 8 words for a packed int. Building the option here
   in C allocated the same 8 words but took 15.5 ns, the cost of a C call that
   registers GC roots. fast_float's scanner (ffc.h) was slower than this loop
   on prices of ordinary length, which are too short for its eight-digit fast
   path. */

#include <stdint.h>
#include <stddef.h>
#include <caml/mlvalues.h>
#include <yyjson.h>

#define EXPONENT_BITS 5
#define MAX_FRACTION 31
#define MAX_MANTISSA ((((uint64_t)1) << 57) - 1)
#define SAFE_DIGITS 17
#define NOT_PACKED Min_long

static intnat packed_decimal(const char *s, size_t n) {
  size_t i = 0;
  int negative = 0;
  if (n > 0 && (s[0] == '-' || s[0] == '+')) {
    negative = s[0] == '-';
    i = 1;
  }
  uint64_t mantissa = 0;
  int fraction = 0; /* digits after the point folded into the mantissa */
  int zeros = 0;    /* zeros after the point, not folded in yet */
  int point = 0;
  int digits = 0;
  for (; i < n; i++) {
    char c = s[i];
    if (c == '.') {
      if (point) return NOT_PACKED;
      point = 1;
    } else if (c == '0' && point) {
      zeros++;
      digits++;
    } else if (c >= '0' && c <= '9') {
      uint64_t d = (uint64_t)(c - '0');
      digits++;
      /* [digits] counts every digit folded in so far, so up to seventeen of
         them the mantissa is below 10^17 < 2^57 and cannot overflow: the
         checks only run on the long decimals that can. */
      for (; zeros > 0; zeros--) {
        if (digits > SAFE_DIGITS && mantissa > MAX_MANTISSA / 10) return NOT_PACKED;
        mantissa *= 10;
        fraction++;
      }
      if (digits > SAFE_DIGITS && mantissa > (MAX_MANTISSA - d) / 10) return NOT_PACKED;
      mantissa = mantissa * 10 + d;
      fraction += point;
    } else {
      return NOT_PACKED;
    }
  }
  if (digits == 0) return NOT_PACKED;
  uint64_t with_zeros = mantissa;
  int zeros_fit = fraction + zeros <= MAX_FRACTION;
  for (int z = 0; zeros_fit && z < zeros; z++) {
    if (with_zeros > MAX_MANTISSA / 10) zeros_fit = 0;
    else with_zeros *= 10;
  }
  if (zeros_fit) {
    mantissa = with_zeros;
    fraction += zeros;
  } else if (fraction > MAX_FRACTION) {
    return NOT_PACKED;
  }
  intnat m = negative ? -(intnat)mantissa : (intnat)mantissa;
  return (intnat)((uintnat)m << EXPONENT_BITS) | (intnat)(MAX_FRACTION - fraction);
}

static intnat packed_of_val(yyjson_val *val) {
  const char *s = yyjson_get_str(val);
  if (s == NULL) return NOT_PACKED;
  return packed_decimal(s, yyjson_get_len(val));
}

CAMLprim value ml_yyjson_get_packed_decimal(value doc, value v) {
  (void)doc;
  return Val_long(packed_of_val(Ptr_val(v)));
}

/* Element [i] of an array of exactly [length] elements, read without the
   OCaml side building a handle for the array or the element: a depth level
   is two of these, and a book carries dozens of levels per message. Any
   mismatch -- not an array, another length, an index out of range -- is
   NOT_PACKED, like a decimal that does not pack, and the caller takes its
   general path to find out which. */
CAMLprim value ml_yyjson_get_packed_decimal_at(value doc, value v, value length, value i) {
  (void)doc;
  yyjson_val *arr = Ptr_val(v);
  intnat n = Long_val(length);
  intnat idx = Long_val(i);
  if (!yyjson_is_arr(arr) || n < 0 || yyjson_arr_size(arr) != (size_t)n) return Val_long(NOT_PACKED);
  if (idx < 0 || idx >= n) return Val_long(NOT_PACKED);
  return Val_long(packed_of_val(yyjson_arr_get(arr, (size_t)idx)));
}
