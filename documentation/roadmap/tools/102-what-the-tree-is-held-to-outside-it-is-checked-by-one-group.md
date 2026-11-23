# What the tree is held to outside it is checked by one group

**Adopted by the user on 2026-11-22; its next step is the decision of its two open questions, then stage 1, a ready delivery; it extends the fetching [`xtask`'s README](../../../xtask/README.md#a-tool-a-step-runs-is-fetched-at-its-digest-and-named-by-the-step) states.** A check of the tree against something it does not own — a standard's corpus, or a revision published elsewhere — is a step of `conformance`, a group apart from the suite as `workspace caches` is, which the handoff requires and a workflow job runs.

## What is wrong today

- **Two corpora are run by nothing.** At `3ea89cfbf`, `Json/decode` answers all 318 of JSONTestSuite's cases as the suite asks — the 95 to read read, the 188 to refuse refused, the 35 left to a parser 11 read and 24 refused — and at `2e44199cb` `Toml/decode` refuses all 509 of `toml-test`'s invalid TOML 1.0 documents and reads all 208 of its valid ones; only `Json/tests`'s number cases run, by hand-copied names.
- **The checks against what the tree does not own run where it cannot be told apart.** Unicode's grapheme cases run inside the suite (`curios-prelude/tests/std.rs`'s `every_conformance_case_segments_through_the_library_as_the_database_states`, and `curios-unicode`'s own), and the Zed pins' check inside the extension's tests (`editors/zed`'s `every_pinned_rev_publishes_this_grammar`), each a check of an outside fact among the tree's own.

## Goal

- `xtask/tasks/conformance.toml`, a group of four steps: `unicode`, the grapheme cases, and UAX #15's once normalization lands; `json`; `toml`; and `zed`, the pins publishing these grammars.
- A corpus is a `tools` entry with one row for every host and the digest of its unpacked files, named by the step that reads it as `{.tools.<name>}`, a directory where a tool names an executable.
- `json` and `toml` are Curios programs, `curios run conformance/<suite>.crs -- {corpus}`, which list the corpus, decode each case and exit nonzero naming every case answered otherwise, a valid TOML document's value held to the tagged JSON beside it.
- The grapheme test and the pin check leave the suite and the extension's tests for their steps.

## Settled

- **A corpus is downloaded, not vendored.** It is pinned at a commit and checked by the digest of its unpacked files, since the archive a forge generates at a commit is not byte-stable where its contents are: Go's module hash (`h1:`, a digest of the file tree) and Nix's `fetchFromGitHub` check what was unpacked for the same reason.
- **A corpus's check is written in Curios**, a program over `File`, `fs` and the codec it holds, exercising `/std` as a program does.

**Rejected.** The corpora vendored into the tree, a second copy of what a commit already pins. A crate of their own whose build script fetches them, where `xtask` fetches what a step names. The archive's bytes digested, which a forge's recompression breaks. The checks as Rust tests the cases are handed to.

## Open questions

- **Where `unicode` reads `GraphemeBreakTest.txt`**: `curios-unicode`'s pinned copy, or a pin of its own, the database's version stated once either way.
- **Whether the moved checks become Curios programs too.**

## Stages

**1. The group, with `zed` and `unicode` moved.** Check: each step passes, and fails where its check is made to: a pin moved off its grammar, a grapheme case altered.

**2. Corpus entries in the `tools` table.** A host-independent entry and a digest of an unpacked tree. Check: a corpus fetched, kept and refused where a file of it is changed.

**3. `json`.** Check: the step passes, and fails naming a case `Json/decode` is made to answer otherwise.

**4. `toml`.** Check: the same, a valid document's value among what it holds.

## Verification and retirement

Each stage's check, then `cargo xtask workspace lint`, the jig's lint, the suite, `cargo xtask docs build` and `cargo xtask handoff`'s new requirement. Once stage 4 lands, this file is deleted.
