# Foreign wire contract

**Assumes.** A `foreign` declaration's type is drawn from the wire grammar, so an embedder cannot supply an inhabitant of a proposition

**Status.** **probed** at the surface, where the grammar refuses, and separately **probed from Core** with a forged ABI row — the half no surface program reaches. The contract is representational rather than checked: a builtin is an identity whose row is the roster's and cannot be written on the term, a declared row's signature is a closed enum of wire types with no case for a nominal type, so no forgery can *say* its result is a proposition, and typing constructs a host call's type through `curios_core::foreign_signature` rather than reading one off the term. `tests::perimeter::a_foreign_declaration_is_confined_to_wire_types` is the surface half; `curios-cert`'s `recheck/foreign_tests.rs` the Core half, a forged declared row and a builtin held at another row's type both refused. Null result
