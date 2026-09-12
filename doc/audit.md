# Audit — 2026-09-12

A correctness and performance audit of this binding, made on 2026-09-12. All
findings have been fixed; see [Status](#status) at the end. This document is
kept as the record of what was wrong, why, and how each defect was
demonstrated, so the regression tests in `test/test.ml` have something to point
back to.

Versions: the audit began against yyjson **0.12.0**, which the binding pinned
by asserting the version literal in `test/test.ml`. The system package was
upgraded to **0.13.0** (released 2026-09-08) while the audit was in progress,
and the binding now targets 0.13.0.

That upgrade is itself instructive: the only thing that noticed was a hardcoded
version literal in a test. The flag tables, which *do* depend on the header,
would have shifted silently. The version test now compares the header the stubs
were compiled against (`YYJSON_VERSION_HEX`) with the library linked at run
time, so a header/library skew fails loudly instead of quietly changing what
every flag means.

Every finding below was reproduced with a scratch harness linked against the
installed library, not inferred from reading.

## Summary

The binding is fast in the right places — it holds a `yyjson_doc` in a custom
block, represents values as unboxed tagged pointers into that document, and
offers a direct accessor API (`obj_get`, `obj_get_string`, `array_values`,
`int64_value`) that avoids materialising a second DOM.

The defects clustered in two places:

1. **The generic `view` path** — the one `Json_encoding.Make` uses — silently
   corrupted strings containing NUL and integers above 2^53, and inverted the
   sign of integers above 2^62.
2. **The flag tables and the mutable API**, which had never been exercised:
   read flags were off by one, mutable documents were never reclaimed, and one
   exposed mutable accessor segfaulted on a type mismatch.

Nothing here was a slow path needing a rewrite; these were small, local fixes.

## Correctness findings

### 1. Every read flag from `Allow_inf_and_nan` onward was off by one

`lib/common.ml` enumerated the read flags but omitted
`YYJSON_READ_ALLOW_COMMENTS` (`1 << 3`). Every subsequent constructor was
therefore shifted down one bit position:

| `ReadFlag.t` constructor | Bit sent | Flag actually enabled |
|---|---|---|
| `Insitu` | `1 << 0` | `INSITU` (correct) |
| `Stop_when_done` | `1 << 1` | `STOP_WHEN_DONE` (correct) |
| `Allow_trailing_commas` | `1 << 2` | `ALLOW_TRAILING_COMMAS` (correct) |
| `Allow_inf_and_nan` | `1 << 3` | `ALLOW_COMMENTS` |
| `Number_as_raw` | `1 << 4` | `ALLOW_INF_AND_NAN` |
| `Allow_invalid_unicode` | `1 << 5` | `NUMBER_AS_RAW` |
| `Bignum_as_raw` | `1 << 6` | `ALLOW_INVALID_UNICODE` |
| `Allow_bom` | `1 << 7` | `BIGNUM_AS_RAW` |
| — | `1 << 8` | `ALLOW_BOM` (unreachable) |

Reproduction:

```
of_string ~flags:[Allow_inf_and_nan] "1e999"    => FAIL "number is infinity..."
of_string ~flags:[Allow_inf_and_nan] "[1] //c"  => OK   (comments were enabled)
of_string ~flags:[Number_as_raw]     "1e999"    => OK   float inf
of_string ~flags:[Allow_invalid_unicode] "123"  => parses as RAW, view asserts
```

The failure was silent: each flag was a request for lenient parsing that was
not granted, with a different leniency granted instead. `ALLOW_BOM` could not
be selected at all.

`WriteFlag` was checked exhaustively against the header — all eight values were
correct.

The enum also did not model the flags added by 0.12.0 (`ALLOW_EXT_NUMBER`,
`ALLOW_EXT_ESCAPE`, `ALLOW_EXT_WHITESPACE`, `ALLOW_SINGLE_QUOTED_STR`,
`ALLOW_UNQUOTED_KEY`, `JSON5`) or 0.13.0 (`WRITE_LOWERCASE_HEX`).

### 2. `view` and `string_value` truncated strings at an embedded NUL

`ml_yyjson_get_str` in `lib/yyjson_stubs.c` copied the string with
`caml_copy_string`, which stops at the first NUL, discarding the length yyjson
already knows. `ml_yyjson_obj_get_string` did it correctly, with
`caml_alloc_initialized_string` and `yyjson_get_len`. Object **keys** had the
same defect, in both `ml_yyjson_obj_iter` and `ml_yyjson_mut_obj_iter`.

Reproduction, on `{"k":"a\u0000b"}`:

```
obj_get_string root "k"           => Some "a\000b"  (len 3)   correct
string_value member               => Some "a"       (len 1)   truncated
view member                       => `String "a"    (len 1)   truncated
view of {"a\u0000b":1}       => key "a"        (len 1)   truncated
```

The existing test covered only `obj_get_string`, the path that works. Every
`Json_encoding` destruct goes through `view`, i.e. through the broken path.

### 3. `view` corrupted integers above 2^53 and inverted them above 2^62

`lib/yyjson.ml` routed non-real numbers through `get_int`, whose stub
(`ml_yyjson_get_sint_int`) is `Val_long` of an `int64` — a silent truncation to
OCaml's 63-bit integer — and then converted to float.

| JSON input | `view` | `int64_value` |
|---|---|---|
| `9007199254740993` | `9007199254740992.` | correct |
| `4611686018427387905` | `-4.6116860184273879e+18` | correct |
| `9223372036854775807` | `-1.` | correct |
| `18446744073709551615` | `-1.` | `-1` (see finding 4) |
| `-9223372036854775808` | `0.` | correct |

64-bit identifiers, sequence numbers and nanosecond timestamps live in exactly
this range.

Note that a `Json_repr.Repr` view can only represent a number as `` `Float ``,
so precision above 2^53 cannot be preserved through `view` at all. The fix
removes the sign inversion and the 63-bit truncation; code that needs exact
large integers must use `int64_value`/`uint64_value` and the direct API.

### 4. `int64_value` wrapped on unsigned integers above `INT64_MAX`

`int64_value` accepted any non-`Real` number and called `yyjson_get_sint`,
which reinterprets a `SUBTYPE_UINT` payload as signed: `18446744073709551615`
came back as `-1`. yyjson stores every non-negative integer with subtype UINT,
so the branch has to distinguish a UINT whose high bit is set.

### 5. `Mutable.get_string` on a non-string value segfaulted

`yyjson_get_str` and `yyjson_mut_get_str` return `NULL` when the value is not a
string; `caml_copy_string(NULL)` then dereferences it. Confirmed: the process
died with SIGSEGV (exit 139). `lib/mutable.mli` exposes `get_string` with no
type check, so this was reachable from safe OCaml. `Yyjson.get_string` had the
same stub defect, but every caller inside the binding guards it with a
`get_type` test first.

### 6. Mutable documents were never reclaimed

`yyjson_mut_doc_ops` had a `custom_finalize_default` finalizer — a no-op —
where the immutable `yyjson_doc_ops` correctly installs `doc_free`. A
`Mutable.doc` therefore leaked its whole arena unless the caller explicitly
called `Mutable.free`.

Reproduction: allocate 100k mutable docs, dropping each reference, then
`Gc.full_major (); Gc.compact ()`:

```
before: VmRSS  6012 kB
after:  VmRSS 85744 kB      (unrecovered)
```

The block was also allocated with `caml_alloc_custom(..., 0, 1)`, declaring
zero GC pressure, so nothing provoked collection even once a finalizer existed.

### 7. `view` raised `Assert_failure` on `RAW` values

`view` asserted on `Raw` and `ErrInvalid`. `RAW` values are produced by
`NUMBER_AS_RAW` and `BIGNUM_AS_RAW`, both reachable — and per finding 1,
`NUMBER_AS_RAW` was what `Allow_invalid_unicode` actually selected. An
`assert false` in a decode path is a crash, not an error.

### 8. `Insitu` was unsafe as exposed

`YYJSON_READ_INSITU` makes yyjson **write into the input buffer** and requires
it to be padded with at least `YYJSON_PADDING_SIZE` (4) bytes beyond the
declared length. The binding guaranteed neither: `of_string` passes an
immutable OCaml string, and neither entry point reserves padding. The flag was
correctly numbered, so it was reachable. It should either stay out of the
public enum, or be offered through a dedicated entry point that owns a padded
bigstring.

### 9. Latent: the write stubs discarded the length they computed

`ml_yyjson_write_opts`, `ml_yyjson_val_write_opts`, `ml_yyjson_mut_write_opts`
and `ml_yyjson_mut_val_write_opts` all obtained `len` from yyjson and then
called `caml_copy_string`, re-deriving the length with `strlen`. This was only
a wasted scan — JSON output escapes NUL — but one `ALLOW_INVALID_UNICODE`-style
change away from being a truncation bug, and it sits on the path taken by any
caller that re-serialises parsed values.

## Performance findings

These are reasoned from the code and not profiled; they are listed in expected
order of impact.

### A. The `with_check_doc*` wrappers de-optimised every accessor

`lib/yyjson.ml` (and the equivalents in `lib/mutable.ml`) rebound each external
through a partial application:

```ocaml
let get_type = with_check_doc1 get_type
```

This forces the `[@@noalloc]` external to be eta-expanded into a closure, so
each call becomes a generic `caml_apply2` and two indirect calls, plus an
out-of-line C call to `is_doc_null`. It is paid once per value visited, on
every accessor. Explicit, inlinable functions restore the direct external call:

```ocaml
let[@inline] get_type doc va =
  if is_doc_null doc then raise Mutable.Doc_is_null else get_type_unsafe doc va
```

Better still, hoist the null check to the parse boundary so hot accessors do
not carry it at all.

### B. `Store_field` on immediate values

`va` is `[@@immediate]` (`Val_ptr p = p + 1`), but the iteration stubs stored
it with `Store_field`, invoking `caml_modify` and its write barrier for a value
that can never point into the heap. Plain `Field(x, i) = ...` is correct and
free, and this is inside the array and object iteration loops.

### C. `view` is the wrong shape for `Json_encoding`

`view` materialises every child as a boxed `{doc; va}` record, builds a list
per array and an association list per object — which `Json_encoding.destruct`
then scans linearly for each field, making object decoding quadratic in field
count on top of a full second DOM.

Callers that care about throughput should use the direct accessor API
(`obj_get`, `obj_get_string`, `array_values`, `int64_value`, `uint64_value`)
and skip `view` entirely. This finding is not fixed: `view` exists to satisfy
`Json_repr.Repr`, and that interface is what makes it slow.

### D. `of_file` declared no GC pressure

`ml_yyjson_read_file` used `caml_alloc_custom(..., 0, 1)` rather than
`caml_alloc_custom_mem` with the document size, as the buffer path correctly
does. File-loaded documents accumulated without provoking collection.

### E. yyjson 0.13.0 offers allocation-free writing

0.13.0 adds `write_buf()` — writing JSON into a caller-supplied buffer with no
allocation — which would suit any path that re-serialises a parsed value, where
each write currently costs a `malloc` in yyjson plus an OCaml string copy plus
a `free`. The binding does not yet expose it. 0.13.0 also fixes "integer
truncation when parsing extremely large numbers".

## Status

Fixed on 2026-09-12, in the same pass as this audit. Every reproduction above
is now a regression test in `test/test.ml` (`read flag mapping`, `raw view`,
`embedded NUL`, `integer range`, `mutable doc`), and the suite passes.

| # | Finding | Fix |
|---|---|---|
| 1 | Read flags off by one | `ReadFlag` renumbered against the header, `Allow_comments` added, plus the 0.12/0.13 extension flags. `No_read_flag`/`NoWriteFlag` dropped (a 0 in a list is meaningless); `WriteFlag.LowercaseHex` added. |
| 2 | NUL truncation | `ml_yyjson_get_str`, both `obj_iter` key copies and `mut_strcpy` now carry the length yyjson already knows. |
| 3 | `view` integer corruption | `view` reads numbers through `yyjson_get_num`, correct for sint, uint and real alike. The 63-bit `get_int` binding is deleted. |
| 4 | `int64_value` u64 wrap | Rejects a UINT above `Int64.max_int`; new `uint64_value` returns the bit pattern. |
| 5 | `get_string` segfault | Both stubs raise `Failure` on a type mismatch instead of dereferencing NULL. |
| 6 | Mutable doc leak | Real finalizer installed, allocated with `caml_alloc_custom_mem`. |
| 7 | `assert false` on `Raw` | Raises `Unexpected_type of json_typ`. |
| 8 | Unsafe `Insitu` | Removed from `ReadFlag`, with the reason recorded in `lib/common.ml`. |
| 9 | Writers discarding `len` | All four write stubs use it. |
| A | Accessor de-optimisation | `with_check_doc*` combinators replaced by explicit `let[@inline]` wrappers. |
| B | `Store_field` on immediates | Plain `Field` assignment in the three iteration loops. |
| D | `of_file` GC pressure | `caml_alloc_custom_mem` with the document's read size. |

Not done, in rough priority order:

- **C** — `view` is inherently the slow path; use the direct accessor API.
  Since this audit the direct API has gained `obj_cursor`/`cursor_get` for
  single-pass member reads and `arr_fold`/`arr_length` for allocation-free
  array traversal, which is what a decoder should be built on.
- **E** — expose 0.13.0's `write_buf()`.
- The `json_subtyp` constructors (`NoneFalseUint`, `TrueSintNoesc`) are named
  after yyjson's bit encoding rather than their meaning, which is why the
  UINT/SINT confusion in findings 3 and 4 was easy to write and hard to see.
  Worth splitting per type.
- Nothing exercises the bigstring parse path, `pos`/`len` slicing, or
  concurrent document lifetimes.
