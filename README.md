# sml-lz4

[![CI](https://github.com/sjqtentacles/sml-lz4/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-lz4/actions/workflows/ci.yml)

Pure [Standard ML](https://en.wikipedia.org/wiki/Standard_ML) implementation of
the **LZ4 block format** — compression and decompression — with **zero
dependencies**. Builds and tests cleanly under both [MLton](http://mlton.org/)
and [Poly/ML](https://www.polyml.org/).

## Overview

[LZ4](https://github.com/lz4/lz4) is a fast LZ77-family compression codec. This
library implements its *block format* (the raw compressed payload, without the
LZ4 frame header/checksums):

- **Decompressor** — parses a sequence of LZ4 *sequences*. Each sequence carries
  a token byte (high nibble = literal length, low nibble = `match length − 4`),
  optional length-extension bytes, the literal bytes, a 2-byte little-endian
  back-reference offset, and the match length. Matches are copied byte-by-byte
  so overlapping copies (the classic run-length trick) work correctly. A block
  ends with a literals-only sequence (the *last-literals* rule).
- **Compressor** — a greedy matcher backed by a hash table of 4-byte sequences.
  It emits valid LZ4 block data that this library's own decompressor round-trips
  exactly. It does **not** aim to match the reference encoder byte-for-byte (the
  format permits many valid encodings).

Everything operates on `Word8Vector.vector`.

## Why

A small, readable, dependency-free LZ4 codec is handy for embedding compression
into SML programs, for teaching the LZ77 idea, and as a reference for the LZ4
block layout. The decompressor is written to the spec first (it is the
well-specified half), and the compressor is validated purely by round-tripping.

## Install

Using [smlpkg](https://github.com/diku-dk/smlpkg):

```sh
smlpkg add github.com/sjqtentacles/sml-lz4
smlpkg sync
```

Then add the library to your MLB file:

```
$(SML_LIB)/basis/basis.mlb
lib/github.com/sjqtentacles/sml-lz4/sources.mlb
```

## Usage

```sml
(* Round-trip a byte vector *)
val original   = Byte.stringToBytes "hello hello hello world"
val compressed = Lz4.compress original
val restored   = Lz4.decompress compressed

val () =
  if restored = original
  then print "round-trip OK\n"
  else print "mismatch!\n"
```

The signature exposes:

```sml
signature LZ4 =
sig
  exception Lz4                                   (* malformed input *)
  val compress   : Word8Vector.vector -> Word8Vector.vector
  val decompress : Word8Vector.vector -> Word8Vector.vector
end
```

`decompress` raises `Lz4` on malformed or truncated block data (e.g. a
back-reference offset of zero, an offset that points before the start of the
output, or truncated literal/offset bytes).

## Example

`make example` builds and runs [`examples/demo.sml`](examples/demo.sml), which
compresses and round-trips a repetitive string and a low-repetition string,
and shows `decompress` raising `Lz4` on truncated input (output is
byte-identical under MLton and Poly/ML):

```
LZ4 block codec over a repetitive string:
  original length     = 59 bytes
  compressed length   = 26 bytes
  round-trip matches  = true

Decompressing malformed input:
  Lz4 raised, as expected, on malformed input

LZ4 block codec over a low-repetition string:
  original length     = 21 bytes
  compressed length   = 23 bytes
  round-trip matches  = true
```

## Testing

The test suite (strict TDD: written before the implementation) covers
hand-constructed LZ4 blocks decoded to known bytes, malformed-input errors, and
round-trip properties over empty input, sub-min-match inputs, long literal runs
(length-extension bytes), long matches, repetitive data, and pseudo-random
incompressible bytes.

```sh
make test        # MLton
make test-poly   # Poly/ML
```

Both compilers report `26 passed, 0 failed`.

## License

MIT.
