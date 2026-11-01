# A browser test run is answered by whatever opens its page

`curios test` runs a subject's tests natively and nowhere else: `test` takes no `--target` (`curios/src/cli.rs`), where `run` and `compile` do. A test that reaches a `foreign` declaration a `[foreign.browser]` gateway answers is therefore refused natively unless a `[foreign.native]` module stands in for the gateway, and the seam itself — the gateway answering the program's rows in a browser — is exercised by nothing the toolchain runs. The gateway is tested from JavaScript with the program mocked, the program natively with the gateway mocked, and the two meet only when someone opens the page and looks ([`documentation/usage.md`](../../usage.md), "Each side is tested with its own tools").

It has no prerequisite in another specification. The next step is a ready delivery, the stages below. [A delivery may be a Curios root](306-a-delivery-may-be-a-curios-root.md) states its "the same tests on both targets" check against this one.

## What is missing

- **`test` has no target.** `Mode::Test` carries a subject, `--filter` and the elaboration flags; `main.rs` compiles each subject through `TestSubject::compile`, loads it with `bindings_of` and runs each test through `Compiled::run_test` (`curios/src/testing.rs`). Nothing builds a test program for a browser, and `--port` has no meaning beside `test`.
- **The page carries no verdict out.** The harness's `run` resolves `{stdout, stderr, exitCode, trap}`, and of a program that answers rather than suspends it says "what it yields, which nothing here reads" (`curios-browser/src/harness.mjs`); native reads the same reference as `TestEnd::Answered(bool)` (`curios-native/src/engine.rs`). The loader's `start` resolves to what `run` resolves to and writes the output to the page's element and the console; nothing posts it anywhere.
- **The server answers `GET` alone.** `curios/src/serve.rs` serves the page's files from memory and refuses everything else, so nothing a page learns can reach the command that serves it.

## What this builds on

- **`run --target browser` is the build and the serve.** `built` (`curios/src/browser.rs`) makes the page `compile --target browser` writes, and `Served` answers it on `127.0.0.1` at port 8000 unless `--port` names another, `Host` checked, every reply `Cache-Control: no-store` and `Connection: close`, printing `↳ Serving   http://127.0.0.1:8000/<name>/?arg=…` and opening no browser ([`documentation/usage.md`](../../usage.md), "Serving a page").
- **A test program picks its test by argument.** `Test/main` runs the test its second argument indexes and every test in order when it has none (`curios-prelude/std/Test.crs`); native passes `[argv0, index]` and instantiates the program once per test, "so one test's effects, traps and exits never reach another" (`TestRun::run`, `curios/src/testing.rs`). The page turns each `arg=` of its query into an argument after the page's name (`argumentsOf`, `curios-browser/templates/index.js`).
- **A test program is target-agnostic.** It is the subject's compilation with `EntryTail::Test { under }` in place of the final term (`curios-pipeline/src/compile.rs`); `payload_of` already takes the target and files a browser payload under `Runs::Browser` (`curios/src/pipeline.rs`). The records a report prints — each test's path and body — are read off the compiled program (`TestRecord`).
- **The exit is a tri-state** (`documentation/usage.md`, "Exit codes"): `0` when every selected test passed, `1` on any that failed, trapped, exited or could not be built, `2` for goals.
- **Both JavaScript suites run under Node.** `curios-browser/src/page.test.mjs` has the released compiler write a page and imports its loader, calling `start` with the program's bytes; `harness.test.mjs` runs the bundle's `compile` and `run` against scripted hooks ([`curios-browser/README.md`](../../../curios-browser/README.md)). The README's rejection of a headless browser and a driver for the harness's own suite stands: this specification launches nothing.

## Prior art

- **web-test-runner's manual mode** "Starts test runner in manual testing mode. Ignores browsers option and prints manual testing URL" ([CLI and configuration](https://modern-web.dev/docs/test-runner/cli-and-configuration/)).
- **testem** prints, in development mode, a URL that "is a normal page in Chrome, Firefox, Safari, etc."; its page "talks to the Testem server through a hidden iframe"; only `testem ci` launches browsers, and a job "relies on the exit code to fail" ([README](https://github.com/testem/testem/blob/master/README.md)).
- **Karma** serves a `client.html` that "connects back to the server via websockets"; a browser joins by visiting the URL or by being launched, and results come back as events reporters subscribe to ([How it works](https://karma-runner.github.io/5.0/intro/how-it-works.html)).
- **jasmine-browser-runner** has `serve`, for "the browser of your choice", beside `runSpecs`, which drives one configured browser and exits nonzero on a failure ([Using Jasmine in the browser](https://jasmine.github.io/setup/browser.html)).
- **wasm-bindgen-test** drives a browser through WebDriver, and with `NO_HEADLESS=1` instead starts "a local server that you can visit in your Web browser of choice" ([browsers](https://github.com/wasm-bindgen/wasm-bindgen/blob/main/guide/src/wasm-bindgen-test/browsers.md)).
- **Dart's `package:test`** runs browser tests "in hidden iframes" and pauses for the developer with `--pause-after-load` ([README](https://github.com/dart-lang/test/blob/master/pkgs/test/README.md)).

The runners that serve a URL for any browser report to a terminal but end with no exit code, and the ones that end with an exit code launch a browser. One mode here does both, because the selected tests are known before serving, so "every test has a verdict" is a condition the command can wait for.

## Decisions

Settled 2026-11-01.

- **No browser is launched or assumed.** The command prints the address and waits; whatever opens it runs the tests — a person, a headless browser a script starts against the printed port, a device reaching the machine. Rejected: WebDriver and a driver binary, a dependency and a download for what any browser does by visiting a page; launching a named browser, a later convenience and not this specification; Node or jsdom in place of a browser, one with no DOM and the other a second DOM.
- **One iframe per test.** The runner opens `/<subject>/index.html?arg=<index>` once per selected test, in the order the records state. A fresh iframe is a fresh realm — a fresh document and fresh instances of every gateway module — which is the browser's form of native's one instantiation per test. Rejected: one page running every test, where a trap or an exit in one ends the rest (`run_all`); one loader instance reused across tests, which shares gateway module state between them.
- **The page reports by `POST`.** The loader, when it finds itself framed, posts `start`'s outcome and the entry's verdict to its parent; the runner `POST`s each test's report to `/report` under the token the runner page carries, and shows the same line in the page. Rejected: a websocket, a dependency for a one-way, bounded report; the command polling the page; reading the browser's console.
- **The run is complete when every selected test has one verdict.** The command then stops serving and exits with the tri-state. A report for a test already reported is ignored, and a rerun is the command run again, the rule `run --target browser` already states: "nothing reloads". Rejected: serving on after the report; a second run inside one serve, whose semantics would have to be defined; a timeout, since a page may be opened an hour later and the signal that stops the command is the timeout.
- **A subject's page is the ordinary page.** `/<subject>/` holds what `compile --target browser` writes for the subject's test program and nothing else, deployable as any page is; the runner at `/` is rendered when the command serves and written nowhere. Rejected: a test mode in the loader; a runner written into the page's directory, which "is the page whole".
- **`no-store` stays.** Each iframe fetches its subject's `index.wasm` again over loopback, and a changed program keeps its one meaning: run the command again.
- **A report is admitted under the run's token alone**, from loopback, under the `Host` check the server already makes; the token is minted per run and rendered into the runner page. Rejected: an unauthenticated route, which any page the machine's browser loads could file verdicts through.
- **The terminal shows what native shows**, from the same inputs — the verdict, the exit code or trap, the guest's output: one line per test, path then `passed`, `failed`, `trapped` or `exited N`, a failure's report indented beneath it with the test's body as written, `↳ Testing` and `↳ Tested` around each subject, and the final tally. The runner page shows the same lines.
- **`--port` and `--filter` as they are.** `--port` as `run`'s, refused beside the native target; `--filter` as native's, a prefix matching no test an error naming it; a reached row no gateway answers refused before anything is served, as `compile` refuses it.

## Open questions

- **A subject with nothing selected.** Native prints its compile line and nothing more; the runner has nothing to open for it. Lean: the terminal as native, the runner listing the subject with no tests.
- **One address for every subject.** Lean: one runner at `/` walking the library's tests and then each executable's, native's order; the alternative is one address per subject, each printed as `run` prints its one.
- **Streaming output.** Whether the loader posts a test's output as it streams or whole with the verdict. Lean: whole, the console carrying lines meanwhile.

## Stages

### 1. The page hands back a verdict

One commit.

- `run` resolves `held` beside `exitCode` and `trap`: the entry's reference read as native reads it, `true` or `false` for a test program and `null` for one whose entry answers no `Bool`.
- The loader's `start`, when `window.parent !== window`, posts `{stdout, stderr, exitCode, trap, held}` to its parent after `run` resolves, and keeps writing the element and the console as it does.
- Check: a `harness.test.mjs` case compiling a program holding a `test` declaration through the bundle and reading `held` from `run`; a `page.test.mjs` case starting a written loader under a stubbed parent and reading the posted message.

### 2. `test` builds a subject for the browser

One commit.

- `Mode::Test` takes `--target` and `--port`; `--port` beside `native` is refused as `run` refuses it.
- `TestSubject::compile` for the browser produces the subject's test program through Binaryen alone, as `payload_of` does for `Runs::Browser`, links its rows through `linked_of`, and lays the subject out as a page through `curios_browser::page`, refusing what `compile` refuses — a reached row with no browser table named with the row and the table.
- Check: a case under `curios/src/tests/` building a test subject for the browser and asserting its page files; one asserting the refusal for a row stating only `[foreign.native]`; one asserting `--port` beside the native target refused.

### 3. The runner, the report route and the terminal

One commit.

- `Served` carries the runner at `/`, rendered from a template in `curios-browser/templates/` with the subjects, each selected test's index, path and body, and the run's token; each subject's page stands at `/<subject>/`.
- `serve.rs` answers `POST /report` with a JSON body naming the subject, the test's index and the posted outcome, under the token; a report lacking the token or naming a test already reported is answered and ignored.
- The command prints each line as its report arrives, the per-subject and final tallies, stops serving once every selected test has a verdict, and exits with the tri-state.
- Check: `curios/src/serve/tests.rs` cases — a report without the token refused, a duplicate ignored, completion ending the serve; an end-to-end case starting the command on a port, fetching `/`, playing each report as the runner would and reading the terminal's lines and the exit code; a `page.test.mjs` case reading the token and the records off a rendered runner.

### 4. Documentation

One commit.

- `documentation/usage.md`: the `test` section and the surface table gain `--target` and `--port`; "Exit codes" states that `test --target browser` serves until its report is complete; "The page" revises "Each side is tested with its own tools" to name the seam's test.
- `curios-browser/README.md` gains the decision that a test page reports to the command that serves it, with the rejections above.
- [A foreign row states a delivery for each target](../../design/tools/a-foreign-row-states-a-delivery-for-each-target.md) gains, in its rationale, that the seam is tested by `test --target browser`.

## Verification

`cargo xtask workspace lint` and `cargo xtask workspace format`; the named `curios` tests; `cargo xtask compiler test` for the page suite and `cargo xtask bundle test` for the harness's. By hand, once: a package with a gateway and a test that reaches its row, `curios test --target browser`, the address opened in a browser, the terminal reporting each test and the command exiting with the tally's code.

## Retirement

The specification is deleted when the stages land, `documentation/usage.md` owning the command, `curios-browser/README.md` the decisions, and the open questions settled in them or filed as findings.
