# A value every program runs at its start is reported where it is declared

**Not refined yet.** This specification reserves a fifth lint, for a module-level value whose statements still compute once its unit is erased. It is not an implementation plan.

## What is missing

A statement that does not finish where its unit is erased runs when a program starts, in every program its unit is in scope of, read or not ([A top-level value is computed where its unit is compiled](../../design/compilation/a-top-level-value-is-computed-where-its-unit-is-compiled.md)), and nothing says so where the value is declared. A program `mod Side; /std/print("done")`, whose module `Side` declares `let forever(n: Nat) -> Nat = forever(n + 1);` and `let hidden: Nat = forever(0);`, compiles to an entry that is that loop and prints nothing; with `let incoming: Async(Io/Chunk) = Async/Read/read(Io/stdin, 1024);` in `Side` instead, the entry allocates two environments for a value nothing reads.

## What is known

- **Four values of `/std` are linked into every program so.** Each still applies something at its top level in the erased rung of the one-line program `/std/print("ok")` (`curios wonder stage ersd`): the two recursive parsers `/std/Json/decode/decode` and `/std/Html/parse/content`, and the two `Io` values `/std/fs/cwd` and `/std/Serial/list`. That program's optimizer drops all four; one that reaches a recursion the optimizer cannot see return, or that allocates, stays.
- **One suite notices such a value and none names it.** `curios`'s `tests::codegen::structural::loop_tests` hold that a small program allocates no environment of its own.
- **The finding is exact, and decided later than a lint is today.** The four lints are zeros read off name resolution, decided in `into_core`, always on, with nothing to configure ([A lint is an exact finding read off the compilation](../../design/tools/a-lint-is-an-exact-finding-read-off-the-compilation.md)). Whether a value's statements finish where its unit is erased is a fact of the unit's erased forms, with no false positive either, and known two stages after names resolve.

## Refinement

- The rule as erasure states it: which statements of an erased value still compute, an application, an allocation and a recursion among them, and whether a value every program's optimizer drops is reported.
- Where the finding is decided and how it travels with its unit, since a lint is filed with the lowered unit and this one is read off the erased one; and what `wonder diagnostics` and the server show of a unit erasure has not reached.
- What the report says the remedy is. A lint is fixed by changing the program, and the change here is a function in the value's place or a value that finishes where its unit is compiled.
- What becomes of the four values of `/std`: the two `Io` values, which a function in their place respells at every use, and the two parsers, whose recursion runs through the value, so that a function in its place builds the parser again at each call.

It retires when the two programs above are reported, a value finished where its unit is erased is not, and no value of `/std` is.
