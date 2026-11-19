# A test answers what it found, and its runner presents it

Adopted (the user, 2026-11-18): a failing test tells its reader which check failed, what that check compared and the steps that reached it, because the test answers what it found as data and the runner, which draws every line of the transcript, presents it. A test answers what it found and its runner draws it beneath the test's verdict ([A test is a declared description, and a proof is a `let`](../../design/tools/a-test-is-a-declared-description-and-a-proof-is-a-let.md)); what is left is what a test can say. `Test/assert` says "the condition was false" and nothing of what it checked, and no step but a table's case index joins the path a failure is reported under.

The next step is stage 1, a delivery.

## What the tree holds

- **`Test` is a monad over outcomes** (`curios-prelude/std/Test.crs`): `Test(A)` is `success(A)`, `failed(Failure)` or `action(Io(Test(A)))`, `bind` skipping its continuation past a failure. A `Failure` is the steps that reached a check, its statement and the values it compared behind their labels; `Test/main` answers a list of them, and the runner draws every one the list holds. `bind` stops at a test's first failure, `Test/all` runs each of its cases and gathers every failure under a step `case N`, and the private `context` prepends a step.
- **The checks**: `Test/equal` states `not equal` beside its `expected` and `obtained` values; `Test/assert` states `the condition was false`; `Test/fail(message)` states its message, a lifted error the error shown, and `Test/timeout` `timed out` beside its deadline.
- **The counts**, at `368b8ab82`: `/std` declares 320 tests and writes `Test/equal` 640 times, `Test/assert` 107 and `Test/all` 137, of which 132 are written lists of checks and 5 build theirs from a table; outside `/std`, `Test/assert` 86 and `Test/equal` 24 times.

## Peers

**What Curios takes.**

- **A path of named steps.** Go names a subtest by "the name of the top-level test and the sequence of names passed to Run" ([`testing`](https://pkg.go.dev/testing)); GoogleTest's `SCOPED_TRACE` adds its message to "every failure" inside its scope ([advanced](https://google.github.io/googletest/advanced.html)).
- **A check states itself.** Alcotest's `check` takes a label beside the two values it compares, and prints them through the testable's own printer ([Alcotest](https://github.com/mirage/alcotest)) — `Test/equal`'s `Spell` and `Eql`; Gleam's `assert … as "message"` replaces the generic "Assertion failed" ([tour](https://tour.gleam.run/advanced-features/bool-assert)).

**What Curios avoids.**

- **A place as a check's identity**, and **a compiler that takes the asserted expression apart**: pytest's rewriting, Swift's `#expect` macro, ExUnit's and Gleam's `assert`, Catch2's decomposition and Roc's `expect` each give the compiler a rule for what a report needs, and Go's `t.Helper` exists because a place in a helper names the helper. Rejected below.

## Decided

Settled with the user on 2026-11-18:

- **A check states itself**: `Test/equal` carries its two sides; a condition with no sides to show is checked under a label its author writes, so no check is anonymous.
- **Steps**: a named step's label joins the path a verdict reports.
- **Left to specifications of their own**, should they be wanted: expect tests, seeds a runner replays a property by, and checks the compiler decides.

## Stages

1. **Checks that state themselves.** `Test/assert(label, condition)`, the label first, its statement the label; `/std`'s 107 uses and the 86 outside it rewritten with their labels, `documentation/language.md`'s example among them. Check: no check of `/std` anonymous; a failing labelled condition reported by its label. The site's cheatsheet example follows at the next release. One commit.
2. **Named steps.** `Test/step(label, test)`, its label prepended to the path of each failure its test reports. Check: a failure two steps deep reported under both labels. One commit.

## Verification

`cargo xtask workspace lint` after `cargo xtask format`, with each stage's tests by name; `curios test` over `/std` at stage 1; the whole workspace's tests once stage 2 has landed.

## Rejected

- **A source place, a line and a column, as what identifies a check**: it says nothing of why the check failed and moves with every edit.
- **An implicit parameter the elaborator fills with the call's place**, as Scala's `sourcecode`, GHC's `HasCallStack`, Swift's `#fileID` and C++'s `source_location` do, and **the asserted expression taken apart by the compiler**, as pytest, Swift, ExUnit, Gleam, Catch2 and Roc do: a filling rule of the language's for what a report needs.
- **A place literal written at each check**, OCaml's `__POS__`: a burden on every author, silent where forgotten.
- **A case of a table named by its spelled input**, Python's `subTest(i=i)`: of `/std`'s 137 `Test/all`, 132 are written lists of checks, which no input names; a step names a case where it helps.

## Retirement

Once stage 2 lands, `curios-prelude`'s `Test` documentation holds the combinators and the test decision what they settle, and this file is deleted.
