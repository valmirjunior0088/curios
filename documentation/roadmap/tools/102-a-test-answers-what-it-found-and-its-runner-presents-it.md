# A test answers what it found, and its runner presents it

Adopted (the user, 2026-11-18): a failing test tells its reader which check failed, what that check compared and the steps that reached it, because the test answers what it found as data and the runner, which draws every line of the transcript, presents it. A test answers what it found, each check states itself, and its runner draws every failure beneath the test's verdict ([A test is a declared description, and a proof is a `let`](../../design/tools/a-test-is-a-declared-description-and-a-proof-is-a-let.md)); what is left is the path. No step but a table's case index joins the path a failure is reported under, so an author cannot name the part of a test a failure stands in.

The next step is stage 1, a delivery.

## What the tree holds

- **A failure carries its steps** (`curios-prelude/std/Test.crs`): `Failure`'s `steps`, the outermost first, which the runner draws ahead of its statement, `case 2: not equal`. The private `context` prepends one to each failure a test reports, and `Test/all` alone calls it, with `case N`.

## Peers

- **A path of named steps.** Go names a subtest by "the name of the top-level test and the sequence of names passed to Run" ([`testing`](https://pkg.go.dev/testing)); GoogleTest's `SCOPED_TRACE` adds its message to "every failure" inside its scope ([advanced](https://google.github.io/googletest/advanced.html)).

## Decided

Settled with the user on 2026-11-18:

- **Steps**: a named step's label joins the path a verdict reports.
- **Left to specifications of their own**, should they be wanted: expect tests, seeds a runner replays a property by, and checks the compiler decides.

## Stages

1. **Named steps.** `Test/step(label, test)`, its label prepended to the path of each failure its test reports: today's `context`, public, and `Test/all` through it. Check: a failure two steps deep reported under both labels. One commit.

## Verification

`cargo xtask workspace lint` after `cargo xtask format`, with the stage's tests by name, and the whole workspace's tests once it has landed.

## Rejected

- **A case of a table named by its spelled input**, Python's `subTest(i=i)`: of `/std`'s 137 `Test/all`, 132 are written lists of checks, which no input names; a step names a case where it helps.

## Retirement

Once stage 1 lands, `curios-prelude`'s `Test` documentation holds the combinators and the test decision what they settle, and this file is deleted.
