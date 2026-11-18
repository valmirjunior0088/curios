# A test answers what it found, and its runner presents it

Adopted: a failing test tells its reader which check failed, what that check compared and the path of steps that reached it, because the test describes what it found as data and the runner, which owns every line a reader sees, presents it. Today the description is a string the test formats itself and prints on standard error, the runner learns a `bool` and the bytes the test wrote, and `Test/assert` says "the condition was false" and nothing of what it checked. Settled with the user on 2026-11-18, replacing the tools finding that asked where a failed assertion was written.

The next step is refining stage 1's one open design: how a test's verdict crosses to its runner.

## What is known

- **`Test` is a monad over outcomes** (`curios-prelude/std/Test.crs`): `Test(A)` is `success(A)`, `failed(Failure)` or `action(Io(Test(A)))`, `bind` skipping its continuation past a failure. A `Failure` is a message and a list of contexts, `Test/context` prepending a label and `Test/all` labelling a failing case by its index.
- **The test formats its own failure.** `message` joins the contexts and the message, and `answer` prints the result on standard error; run in order outside the runner, `report` prints `<name>: failed` and the message on standard output.
- **The runner learns one `bool` and some bytes.** `curios test` runs each test in an instantiation of its own, `[argv0, index]` its arguments (`curios/src/test_runner.rs`); the program's answer, `Io(Bool)`, is read natively as `TestEnd::Answered` (`curios-native/src/engine.rs`); `TestHost` frames what the test writes on its standard streams under the test's node (`curios/src/test_runner/host.rs`), and one printer draws the tree, the same in a terminal and in a browser's transcript.
- **Most checks already carry their values; the rest carry nothing.** Counted on 2026-11-18, `/std`'s 320 tests write 636 `Test/equal`, which spells both sides through `Spell`, and 106 `Test/assert`, which spells neither side, having none.

## Decided

- **The test answers a verdict.** What a test program answers is a record of what it found: the path of steps it went through, and each failed check with its statement and both sides spelled as Curios writes them. It crosses as the answer a test program already gives its runner, with no host row added for it, and the native engine and the browser runner read it alike.
  - Rejected: events on a channel `TestHost` holds, each step and check as it happens, as Go's `test2json`, TAP and libtest's JSON events report: they tell where a test that trapped or timed out stood, which an answer cannot, and are left to a specification of their own should traps and timeouts need it.
- **The runner presents.** The one printer `curios test` draws its tree with renders a verdict — its steps, its failed checks, their spelled sides — and `/std`'s `Test` prints nothing of its own.
- **A check states itself.** A check carries what it compared, `Test/equal` its two sides as today; a condition with no sides to show is checked under a label its author writes, so no check is anonymous.
  - Rejected: a source place, a line and a column, as what identifies a check: it says nothing of why the check failed and moves with every edit, and is needed only where a check has no identity of its own.
  - Rejected: an implicit parameter the elaborator fills with the call's place, as Scala's `sourcecode`, GHC's `HasCallStack`, Swift's `#fileID` and `#line` defaults and C++'s `source_location` do: a filling rule of the language's for what a report needs.
  - Rejected: a place literal written at each check, as OCaml's `__POS__` and Alcotest's `~pos` are: a burden on every author, silent where forgotten.
  - Rejected: a test program whose calls answering a `Test` are wrapped in a placed context, as pytest rewrites its assertions.
- **Steps, and every failure.** `Test/context` becomes a named step, its label part of the path a verdict reports; `Test/all` reports every independent failure rather than the first, while `bind` keeps short-circuiting the steps that depend on one another — a validation where checks are independent, a monad where they are not.
- **Left to specifications of their own**, should they be wanted: expect tests, whose expected output stands in the source and whose runner offers to rewrite it with what it got, as Jane Street's `ppx_expect` and `insta` do; seeds the runner hands a property test and replays a failing one by; and checks the compiler decides, a closed check proved at compile time.

## Stages

1. **The verdict and its crossing.** The record a test answers and how it reaches both runners, the runner rendering today's message and contexts from it, `report` and `answer` printing nothing. Not refined: the record's shape; its crossing, the answer a `bool` the engine reads today — a text `Spell` writes and the runner reads back, or a host row the test hands a value through; and how the tree lays a verdict out. Check: a failing `Test/equal` and a failing table case rendered by the runner, natively and in a browser, from the verdict alone; a passing test unchanged. One commit.
2. **Checks that state themselves.** A labelled condition in `Test/assert`'s place, and `/std`'s 106 uses rewritten with their labels. Not refined: the combinator's name, and whether `Test/assert` keeps its name with a label. Check: no check of `/std` anonymous; a failing labelled condition reported with its label. One commit.
3. **Every independent failure.** `Test/all` reports each failing case, `bind` still stopping at the first. Check: a table with two failing cases reporting both, by their indices. One commit.
4. **Named steps.** `Test/context` restated as a step whose label the verdict's path holds. Not refined: the step combinator's name. Check: a failure two steps deep reported under both labels. One commit.

## Verification

`cargo xtask workspace lint` and the stage's tests by name at each stage, `curios test` over `/std` at stages 1 and 2, and the whole workspace's tests once stage 4 has landed.

## Retirement

Once stage 4 lands, `curios-prelude`'s `Test` documentation holds the combinators, `documentation/usage.md`'s `test` section what a run reports, and `curios/src/test_runner.rs`' module documentation how the verdict is presented, and this file is deleted.
