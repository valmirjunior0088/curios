//! What certifies UTF-8, where text may be cut and where a search lands in it, and what a `Str` literal costs. The `Str` surface and its decompositions are `/std/Str`'s own tests', and the JSON codec `/std/Json`'s.

mod at_tests;
mod search_tests;
mod str_tests;
mod utf8_tests;
