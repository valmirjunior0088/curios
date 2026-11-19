# A value every program runs at its start is reported where it is declared

**Not refined yet.** This specification reserves a lint for a module-level value whose statements still compute once its unit is erased. It is not an implementation plan: it is one more lint, decided by the stage that establishes its last fact and kept in that stage's record ([A lint is an exact finding read off the compilation](../../design/tools/a-lint-is-an-exact-finding-read-off-the-compilation.md)) — erasure, here — and waits on the refinement below.

## What is missing

A statement that does not finish where its unit is erased runs when a program starts, in every program its unit is in scope of, read or not ([A top-level value is computed where its unit is compiled](../../design/compilation/a-top-level-value-is-computed-where-its-unit-is-compiled.md)), and nothing says so where the value is declared. A program `mod Side; /std/print("done")`, whose module `Side` declares `let forever(n: Nat) -> Nat = forever(n + 1);` and `let hidden: Nat = forever(0);`, compiles to an entry that is that loop and prints nothing; with `pub let incoming: Io(Io/Chunk) = Io/read(/std/Proc/stdin, 1024);` in `Side` instead, the erased unit still applies `/std/Io/read` at its top level (`curios wonder --stage anf`), which that program's optimizer drops, its `cps-optm` rung the bytes of the same program with `Side` declaring a `Nat` in its place, at `00627ea06`.

## What is known

- **No value of `/std` is linked into every program so.** The erased rung of the one-line program `/std/print("ok")` (`curios wonder --stage anf`) holds only the items its entry reaches: the library's recursive parsers are functions of none, as its effects that take no argument are, so a program that names none of them links none of them.
- **One suite notices such a value and none names it.** `curios`'s `tests::codegen::structural::loop_` hold that a small program allocates no environment of its own.
- **The finding is exact, and of a stage no lint reads yet.** Each lint is decided by the stage that establishes its last fact — the lowering and elaboration today — and gathered where a unit is made (`Unit::new`), each always on, with nothing to configure ([A lint is an exact finding read off the compilation](../../design/tools/a-lint-is-an-exact-finding-read-off-the-compilation.md)). Whether a value's statements finish where its unit is erased is a fact of the unit's erased forms, with no false positive either, and erasure decides no lint yet.

## Refinement

- The rule as erasure states it: which statements of an erased value still compute, an application, an allocation and a recursion among them, and whether a value every program's optimizer drops is reported.
- Where erasure keeps the lint it decides, beside the lowering's and the records' elaboration files, and how it travels with its unit; and what `wonder` and the server show of a unit erasure has not reached.
- Whether its kind names itself beside the rule erasure states, or joins the closed enum every kind is a variant of today (`LintKind`, `curios-core/src/lint.rs`): the uncertain finding of a lint's identity in [the tools findings](000-findings.md).
- What the report says the remedy is. A lint is fixed by changing the program, and the change here is a function in the value's place or a value that finishes where its unit is compiled.

It retires when the two programs above are reported, a value finished where its unit is erased is not, and no value of `/std` is.
