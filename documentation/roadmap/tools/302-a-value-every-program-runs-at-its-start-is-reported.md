# A value every program runs at its start is reported where it is declared

**Not refined yet.** This specification reserves a fifth lint, for a module-level value whose statements still compute once its unit is erased. It is not an implementation plan.

## What is missing

A statement that does not finish where its unit is erased runs when a program starts, in every program its unit is in scope of, read or not ([A top-level value is computed where its unit is compiled](../../design/compilation/a-top-level-value-is-computed-where-its-unit-is-compiled.md)), and nothing says so where the value is declared. A program `mod Side; /std/print("done")`, whose module `Side` declares `let forever(n: Nat) -> Nat = forever(n + 1);` and `let hidden: Nat = forever(0);`, compiles to an entry that is that loop and prints nothing; with `pub let incoming: Io(Io/Chunk) = Io/read(/std/Process/stdin, 1024);` in `Side` instead, the erased unit still applies `/std/Io/read` at its top level (`curios wonder stage anf`), which that program's optimizer drops, its `cont-optm` rung the bytes of the same program with `Side` declaring a `Nat` in its place, at `00627ea06`.

## What is known

- **No value of `/std` is linked into every program so.** The erased rung of the one-line program `/std/print("ok")` (`curios wonder stage anf`) holds only the items its entry reaches: the library's recursive parsers are functions of none, as its effects that take no argument are, so a program that names none of them links none of them.
- **One suite notices such a value and none names it.** `curios`'s `tests::codegen::structural::loop_tests` hold that a small program allocates no environment of its own.
- **The finding is exact, and decided later than a lint is today.** The four lints are zeros read off name resolution, decided in `into_core`, always on, with nothing to configure ([A lint is an exact finding read off the compilation](../../design/tools/a-lint-is-an-exact-finding-read-off-the-compilation.md)). Whether a value's statements finish where its unit is erased is a fact of the unit's erased forms, with no false positive either, and known two stages after names resolve.

## Refinement

- The rule as erasure states it: which statements of an erased value still compute, an application, an allocation and a recursion among them, and whether a value every program's optimizer drops is reported.
- Where the finding is decided and how it travels with its unit, since a lint is filed with the lowered unit and this one is read off the erased one; and what `wonder diagnostics` and the server show of a unit erasure has not reached.
- What the report says the remedy is. A lint is fixed by changing the program, and the change here is a function in the value's place or a value that finishes where its unit is compiled.

It retires when the two programs above are reported, a value finished where its unit is erased is not, and no value of `/std` is.
