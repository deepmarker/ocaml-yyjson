/* Decimal strings read in place.

   Market data sends prices and sizes as JSON strings holding a plain decimal
   ("62817.99"), and a consumer that needs them exact wants the mantissa and
   exponent rather than a double. Reading one through ml_yyjson_get_str copies
   it into an OCaml string before anything can parse it; this parses the bytes
   where yyjson already holds them.

   The grammar is deliberately narrow: an optional sign, digits with at most
   one point, between one and eighteen digits in all, and nothing else -- no
   exponent, no whitespace. Eighteen digits keeps the mantissa below 2^62, so
   it comes back as a tagged OCaml int with nothing allocated. A value that is
   not a string, or not such a decimal, returns Max_long, which no mantissa can
   equal. The exponent is written into the int ref passed in; storing an
   immediate there needs no write barrier, so the stub is noalloc.

   fast_float's scanner (ffc.h) was measured against this loop behind the same
   contract and was slower on prices of ordinary length, which are too short
   for its eight-digit fast path to engage. */

#include <stdint.h>
#include <stddef.h>
#include <caml/mlvalues.h>
#include <yyjson.h>

#define MAX_DIGITS 18

static intnat decimal(const char *s, size_t n, intnat *exponent) {
  size_t i = 0;
  int negative = 0;
  if (n > 0 && (s[0] == '-' || s[0] == '+')) {
    negative = s[0] == '-';
    i = 1;
  }
  uint64_t mantissa = 0;
  intnat exp = 0;
  int digits = 0;
  int point = 0;
  for (; i < n; i++) {
    char c = s[i];
    if (c >= '0' && c <= '9') {
      if (++digits > MAX_DIGITS) return Max_long;
      mantissa = mantissa * 10 + (uint64_t)(c - '0');
      exp -= point;
    } else if (c == '.' && !point) {
      point = 1;
    } else {
      return Max_long;
    }
  }
  if (digits == 0) return Max_long;
  *exponent = exp;
  return negative ? -(intnat)mantissa : (intnat)mantissa;
}

CAMLprim value ml_yyjson_get_decimal(value doc, value v, value exponent) {
  (void)doc;
  yyjson_val *val = Ptr_val(v);
  const char *s = yyjson_get_str(val);
  if (s == NULL) return Val_long(Max_long);
  intnat e = 0;
  intnat m = decimal(s, yyjson_get_len(val), &e);
  if (m != Max_long) Field(exponent, 0) = Val_long(e);
  return Val_long(m);
}
