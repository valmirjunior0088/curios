# Bytes are compressed and expanded by DEFLATE

**Not refined yet.** It waits on [Bytes are written as text and digested](300-bytes-are-written-as-text-and-digested.md)'s CRC-32, which gzip's trailer carries. RFC 1951's DEFLATE inside RFC 1952's gzip is HTTP's universal content coding and the wrapper of every `.gz` and `.tar.gz` a program downloads; a reply sent `Content-Encoding: gzip`, or a compressed file, is bytes no `/std` reader opens.

## What it reserves

`inflate` and `deflate` over `Bytes`, gzip's framing with its CRC-32 checked, and the HTTP client asking for and undoing the coding.

## Peers

Go's `compress/flate` and `compress/gzip`, Python's `zlib` and `gzip`, Java's `java.util.zip` and Zig's `std.compress.flate` ship both directions; Rust leaves it to `flate2`.

## Open questions

- **In Curios or as a host row**: the question 300 leaves open of a digest, asked again of a codec whose loop is bit-level.
- **How much of DEFLATE's search an encoder makes**: stored, fixed or dynamic Huffman blocks.
- **Whether `http` asks for gzip unprompted.**

## Stages

Once decided: inflate, held to a corpus another implementation wrote; gzip's framing; deflate, its output inflating back and read by another implementation; the client's coding.
