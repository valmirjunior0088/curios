# Findings

- **Three test files hold more than 25 tests.** `.claude/rules/rust.md` splits a module past 25 tests by subject, a file to a theme, and three theme files are past it: `curios-cert/src/recheck/adoption_tests.rs` (27), `curios-cert/src/kernel/infer/tests/typing.rs` (27) and `curios-prelude/src/restore/tests.rs` (26). Each is one theme already, so each splits into narrower ones.
