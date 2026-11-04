# The kernel's suites write their modules as Core text

**Not refined yet.** This specification reserves the move of `curios-cert`'s hand-built fixtures to Core text, which [the soundness board](../../design/soundness/the-soundness-board.md) asks of a held attack and `curios-cert`'s README states for one the elaborator refuses first. No fixture is chosen and no stage is drawn; it waits on the split under *Refinement*.

## What is missing

`curios-cert`'s suites build what they put to the kernel from `curios-core`'s constructors: 49 of its test files call them, 2,266 `Term::` calls at `99015c677` (counted), and one, `recheck::positivity_tests`, writes Core text read with nothing in scope (`recheck::test_support::core_text_verdicts`). A module spelled in constructors is read only as Rust and rots with the representation it spells, which is how the board's hand-built witnesses rotted before they were written as Core text.

## Refinement

Which fixtures move turns on what each hands the kernel. A module put to the walk, `fixture_verdicts` over definitions and an entry, is a held attack the rule places as Core text. A term handed to a function below the walk's entry — reduction, conversion, a memo — is an attack below a checker's entry, a Rust test of the crate whose function it calls, where Core text may still spell the term and need not. Deciding the second kind, and whether a fixture shared by many suites, `recheck::test_support`'s module builders among them, moves with its first consumer or at once, is what refines this specification into stages.
