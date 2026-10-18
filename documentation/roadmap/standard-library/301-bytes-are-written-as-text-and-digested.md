# Bytes are written as text and digested

**Not refined yet.** This specification reserves the binary-to-text encodings of RFC 4648 over `Bytes` and the standard digests and checksums of a byte string. It is not an implementation plan. No module of `/std` consumes it yet: its first consumer would be `http` writing credentials or answering a WebSocket upgrade, or a program checking what it downloaded.

## What is missing

- **`Bytes` has no text encoding.** `/std` writes one hexadecimal digit at a time (`Char/hex_digit`) and reads one four ways: `Char/of_hex_digit` over a `Char`, which `Json/decode` and `Html/parse` call; `Toml/scan/hex_val` over a code point; `http/Url`'s `hex_value` over a `Byte`, with the bound its percent-decoding needs; and `Flt/grammar`'s `hex_value` over a whole `Str`. Nothing writes or reads a byte string as hexadecimal, base32 or base64.
- **`/std` has no digest a peer can check.** `Hash/digest` (`curios-std/src/Hash.crs`) is eight bytes of three polynomial lanes, for dedup, cache keys and bloom filters, and its own documentation says it is not stable across compiler versions and not adversarially resistant. No SHA-1, SHA-2 or CRC-32 is computed anywhere in `/std`.

## What is known

- **The encodings are one standard.** [RFC 4648](https://www.rfc-editor.org/rfc/rfc4648) defines base16, base32 and base64 with their URL-safe alphabets, and in §3.5 the canonical form a strict decoder holds an encoding to.
- **The protocols `/std` already speaks ask for them.** HTTP's Basic scheme sends `user:password` in base64 ([RFC 7617](https://www.rfc-editor.org/rfc/rfc7617)); a WebSocket server answers with the base64 of the SHA-1 of the client's key and a fixed GUID ([RFC 6455 §4.2.2](https://www.rfc-editor.org/rfc/rfc6455#section-4.2.2)); a `data:` URL carries base64 ([RFC 2397](https://www.rfc-editor.org/rfc/rfc2397)); a JSON Web Signature writes its parts in base64url ([RFC 7515](https://www.rfc-editor.org/rfc/rfc7515)); and gzip ([RFC 1952](https://www.rfc-editor.org/rfc/rfc1952)), zip and PNG check what they carry with a CRC-32.
- **The digests are FIPS 180-4's.** SHA-1 and the SHA-2 family ([FIPS 180-4](https://csrc.nist.gov/pubs/fips/180-4/upd1/final)) work in 32- and 64-bit words with additions modulo the word, rotations and shifts.
- **Peers split.** Go ships [`encoding/base64`](https://pkg.go.dev/encoding/base64), `encoding/hex`, `crypto/sha256` and `hash/crc32`; Python ships [`base64`](https://docs.python.org/3/library/base64.html), [`hashlib`](https://docs.python.org/3/library/hashlib.html) and `zlib.crc32`; Haskell's `base` and Rust's standard library leave all of them to packages.

## Refinement

- **The word.** A digest's every step is a 32- or 64-bit operation, past the `2^30` a `Nat` is held unboxed in. The question is [a random sequence replays from its seed](300-a-random-sequence-replays-from-its-seed.md)'s, settled there once for both.
- **Where they live.** Lowercase namespaces of functions over `Bytes`, and whether the four hexadecimal readers above become callers of one.
- **Which digests.** SHA-256 for integrity, SHA-1 only where a protocol requires it, CRC-32 for the formats that carry one; and whether a digest is computed in Curios or answered by a host row, which every host would then answer.
- **Whole or streamed.** A digest of one `Bytes`, or one fed piece by piece from an `Io/Reader`, so that a large body is digested as it arrives.
- **Decoding.** What a decoder refuses — padding, alphabet, the non-canonical encodings RFC 4648 §3.5 names — and whether it answers an `Option` or a refusal with its position.
- **Comparison.** Whether comparing two digests takes time independent of where they differ, which a message authentication code would need and a checksum does not.

It retires when `Bytes` reads and writes RFC 4648's three encodings, `/std` computes the digests and checksum the refinement chooses, and the hexadecimal readers of `/std` are what it settles.
