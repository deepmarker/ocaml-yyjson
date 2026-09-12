# ocaml-yyjson

OCaml bindings to [yyjson](https://github.com/ibireme/yyjson), a fast C JSON
parser.

The binding wraps a `yyjson_doc` in a custom block and represents each value as
an unboxed tagged pointer into that document, so walking a parsed document
allocates nothing beyond the OCaml values you actually extract.

## Requirements

yyjson **0.13.0 or later**, installed as a system library — the stubs
`#include <yyjson.h>` and link with `-lyyjson`. On Arch, `pacman -S yyjson`.

The header and the linked library must be the same version: yyjson's read and
write flags are bit positions, so a skew silently changes what every flag
means. The test suite checks this by comparing `YYJSON_VERSION_HEX` (compiled
against) with `yyjson_version()` (linked at run time).

## Two APIs

### Direct accessors — the fast path

Read only the fields you need, straight out of the parsed document:

```ocaml
let doc = Yyjson.of_string {|{"s":"BTCUSDT","E":1769040135865,"p":["1.5","2.0"]}|} in
let root = Yyjson.value_of_doc doc in
let symbol = Yyjson.obj_get_string root "s" in
let ts = Option.bind (Yyjson.obj_get root "E") Yyjson.int64_value in
let levels = Option.bind (Yyjson.obj_get root "p") Yyjson.array_values in
Yyjson.free_doc doc
```

- `obj_get`, `obj_get_string`, `array_values`
- `string_value`, `int64_value`, `uint64_value`

`int64_value` and `uint64_value` read integers exactly, without going through a
float. `int64_value` returns `None` for an unsigned value above
`Int64.max_int`; `uint64_value` returns its bit pattern for those.

### Reading many fields, or long arrays

`obj_get` restarts the search for each member, so reading an *n*-field record
costs *n* searches. When the fields are known ahead of time — a decoder, hand
written or generated — a cursor reads them in a single pass instead, resuming
where the previous lookup stopped. Keys that are absent, or out of order, are
still handled correctly; the scan wraps around.

```ocaml
let cursor = Option.get (Yyjson.obj_cursor root) in
let symbol = Yyjson.cursor_get cursor "s" in
let event_time = Yyjson.cursor_get cursor "E" in
```

`arr_fold` walks an array in place, without building the intermediate array
that `array_values` returns, and `arr_length` sizes a result up front. Both
step by container offset, so they stay O(1) per element on arrays of arrays,
where indexed access degrades to a linear search.

```ocaml
(* [None] if [levels] is not an array. *)
let count : int option = Yyjson.arr_fold levels ~init:0 ~f:(fun n _ -> n + 1)
```

### `view` — generic, for `Json_repr` interop

`view` produces the polymorphic-variant JSON view that
[`json-data-encoding`](https://gitlab.com/nomadic-labs/json-data-encoding)'s
`Json_encoding.Make` consumes. It is convenient but not cheap, and it is lossy
for integers:

- it materialises every child as a boxed value, and every object as an
  association list that `destruct` then scans linearly per field;
- `Json_repr`'s view has no integer case, so numbers come back as `` `Float ``
  and integers above 2^53 are rounded.

Use the direct accessors where throughput or exact integers matter.

## Lifetimes

A `va` is a pointer into its document; it must not outlive it. The `value`
record (`{doc; va}`) pairs the two so the document stays reachable while any
value derived from it does.

Documents are freed when collected. `free_doc` frees one eagerly; using any
value from a freed document raises `Doc_is_null` rather than crashing.

`Yyjson.Mutable` builds documents for serialisation; its documents are likewise
GC-reclaimed, with `Mutable.free` for eager release.

## Flags

`ReadFlag.t` and `WriteFlag.t` map one-to-one onto `YYJSON_READ_*` and
`YYJSON_WRITE_*`. `YYJSON_READ_INSITU` is deliberately not exposed: it makes
yyjson write into the input buffer and requires `YYJSON_PADDING_SIZE` bytes of
slack past the declared length, neither of which this binding can promise for
an OCaml string.

## Build and test

```sh
make          # dune build @install @runtest
dune runtest  # tests only
```

## Documentation

[`doc/audit.md`](doc/audit.md) — a correctness and performance audit of this
binding from 2026-09-12, with a reproduction for each defect it found. All of
them are fixed and covered by regression tests; it is kept as the record of
what was wrong and why, and it doubles as a description of the sharp edges
(integer precision through `view`, value lifetimes, flag/version skew).

## License

ISC. See [LICENSE.md](LICENSE.md).
