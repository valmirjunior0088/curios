# Theory: findings

- **`Term::match_ambient` is a public constructor only tests call** (`curios-core/src/term.rs`): the elaborator builds `MatchResult::Ambient` directly, and every caller is a `curios-cert` test. Fix: move it to `curios_core::test_support`, or have the elaborator build through it. Check: `curios-cert`'s tests build with the feature on. Size: quick.
