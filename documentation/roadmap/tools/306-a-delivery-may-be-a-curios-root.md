# A delivery may be a Curios root

A `[[foreign]]` row delivers a WebAssembly module to the native target and a JavaScript gateway to a browser ([A foreign row states a delivery for each target](../../design/tools/a-foreign-row-states-a-delivery-for-each-target.md)), and nothing else. A declaration a gateway answers has, natively, no answer but a module in another language — built freestanding, importing nothing, taking scalars and byte strings alone and answering at once (`curios-native/src/plugin.rs`, [Foreign calls past scalars and byte strings](../runtime/300-foreign-calls-past-scalars.md)). A row declared `-> List(Bytes)` or taking a `Handle` has no native stand-in at all. So a program that reaches a browser row is tested natively through a `.wasm` written for the purpose or not at all, and a package cannot give such a row a native answer in the language the rest of it is written in, though the language can state the answer's type exactly and the checkers can hold it to it. This revises the decision above, whose rationale makes a `.wasm` "standing in for a gateway" the way code that reaches a row is tested natively.

It has no prerequisite in another specification; its "the same tests on both targets" check is [A browser test run is answered by whatever opens its page](305-a-browser-test-run-is-answered-by-whatever-opens-its-page.md)'s command. The next step is a ready delivery, the stages below.

## What is missing

- **A delivery is a file the compiler cannot read.** `Delivery { module, hash }` names a module (`curios-package/src/manifest.rs`); the native table "names the WebAssembly module the native target compiles and links", `bindings_of` compiles each with Cranelift and `plugin_bindings` holds its exports to the rows (`curios/src/link.rs`, `curios-native/src/plugin.rs`). The browser table names a gateway, read by nothing but a browser.
- **A module's ceiling is the wire into linear memory.** `crossing` refuses a `Handle`, a `List` and an `Option`; `Plugin::of` refuses a module that imports anything; a module answers at once, so the paths a promise-answering gateway takes — a reply that comes later, a call dropped meanwhile — run natively through no delivery.
- **A root the manifest names is a library or an executable.** `Program` is loose or `Scope::Declared { home, units, declares }`, the units "its own library last, everything that depends on before that" (`curios-package/src/selection.rs`); a file is placed by the chain that reaches it from `lib.crs`, `exe.crs` or an `[[executables]]` row, and nothing places one by a `[[foreign]]` row.
- **Nothing reads a unit's declaration but an importer.** A `[foreign.answers]` entry names an export of a module or a gateway; no table names a Curios declaration, and the link reads no unit's names.
- **No point above a call's statement survives optimization.** Probed at `a2ac62b97`: for `foreign ping(Nat) -> {}; let early: Io({}) = ping(7); early`, `wonder stage anf` shows `early` calling `/ping`, which calls the hoisted `io//ping/step`; `wonder stage anf-optm` shows `ffi//ping(7, …)` inline in the entry inside a fresh copy of the description, the declaration and `step` both gone. Replacing the declaration's body or `step` would leave such a copy calling the wire.

## What this builds on

- **A row's types are its whole contract, and who answers is decided at link.** "Nothing in the program says which" ([A host operation is a foreign declaration whose types are its whole contract](../../design/runtime/a-host-operation-is-a-foreign-declaration-whose-types-are-its-whole-contract.md)); each target's table says what that target links.
- **A Curios function carries a declaration's type exactly.** Probed at `a2ac62b97` with `wonder diagnostics -`: `let answer_f(n: Nat) -> Io(Nat) = Io/pure(n + 1)` is admitted at `(Nat) -> Io(Nat)`, the type of `foreign f(Nat) -> Nat`; `let answer_gone(@A: Type, code: Byte) -> Io(A) = /std/Process/exit(code)` at `(@A: Type, Byte) -> Io(A)`, the type of `foreign gone(Byte) -> Never`; a function answering `Io(Int)` is refused with `inferred: (n: Nat) -> Io(Int)  expected: (Nat) -> Io(Nat)`; and `Io/pure(?)` at `Io(A)` leaves a goal `? : A` with nothing in scope to fill it, so an answer to a row that does not return has to end the program as the row does.
- **A foreign call is never reduced and `Io` has no eliminator** ([Effects are descriptions and the carrier has no eliminator](../../design/theory/effects-are-descriptions-and-the-carrier-has-no-eliminator.md)), so what answers a row decides a run's behavior and never a proof: `let e: Eq()(f(1 + 1), f(2)) = Eq/refl()` holds by congruence of the opaque declaration, whoever answers `f`.
- **Visibility is the whole rule.** A unit may name the prefixes its `declares` lists, "its package's declared dependencies, its own library, and `/std`", and "the root's kind only chooses the wording" (`curios-text/src/into_core.rs`); a `RootKind` "is not a privilege tier" (`curios-utilities/src/mount.rs`). A mount is one segment, mounts are pairwise disjoint — "no mount can lie beneath another, and the entry's own `mod json` claims `/json` against a mounted package of that name exactly as a second mount would" — and `Scoped`'s shadowing, the registries' duplicate keys and the `ffi` namespace rest on it; the empty prefix is the entry's, and at most one mount may claim it.
- **A root's files are placed by its chain.** "Roots that sit in one directory share it: the package's directory holds the library's modules and those of every executable beside the manifest alike", a file two chains reach is refused, and an executable with modules of its own takes a directory to itself, `tools/serve/serve.crs` beside `tools/serve/*.crs` (`curios-package/src/selection.rs`, [`documentation/usage.md`](../../usage.md)).
- **A claim is held where the unit is.** A `[foreign.answers]` entry naming no declaration is refused where the library compiles, against the rows the unit holds (`RootSource::answering`, `curios-text/src/root_source.rs`).
- **The link appends fragments and interns rows.** A declaration's erased form is a fragment; a program is the fragments it reaches appended under fresh identities, a foreign row interned once however many fragments spell it (`curios-anf/src/link.rs`). A call of a row is one `Rhs::Foreign` inside the description `lane_call` builds, and a call of a row that does not return one `Terminator::Halt` (`curios-anf/src/node.rs`, `curios-erase/src/lower/crossing.rs`).
- **Payloads are per target**, `Runs::Native` and `Runs::Browser`, and a payload's record names every unit of its compilation ([Reused payloads](../../design/soundness/reused-payloads.md)).

## Prior art

- **Kotlin's `expect`/`actual`.** Actuals live in platform source sets and "typically contain an implementation using platform-specific libraries"; "every expected declaration in the common source set has a matching actual declaration in every platform-specific source set", a missing one a compile error; interfaces are preferred over expected classes because fakes are easier to substitute ([expect/actual](https://kotlinlang.org/docs/multiplatform/multiplatform-expect-actual.html)).
- **dune's virtual libraries** "delay the selection of concrete implementations until linking an executable", and it is "impossible to link more than one implementation for the same virtual library in one executable" ([virtual libraries](https://jbuilder.readthedocs.io/en/latest/virtual-libraries.html)).
- **MoonBit** maps files to backends in the package manifest: "the smallest unit of conditional compilation is a file", and an unlisted file "will be compiled under all conditions" ([package configuration](https://docs.moonbitlang.com/en/latest/toolchain/moon/package.html)).
- **Agda's `COMPILE` pragmas** attach a per-backend implementation to a postulate, and "at type-checking time the functions are still treated as postulates" ([FFI](https://agda.readthedocs.io/en/v2.6.1/language/foreign-function-interface.html)); **Gleam** uses an external where one exists for the target and the function's Gleam body otherwise ([fallbacks](https://tour.gleam.run/advanced-features/external-gleam-fallbacks/)).
- **Lean's `implemented_by`** substitutes an implementation that "is never verified to match the original definition", which "allows a false proof through `native_decide`" ([ImplementedByAttr](https://lean-lang.org/doc/api/Lean/Compiler/ImplementedByAttr.html)) — the failure a delivery here cannot cause.
- **Ports and adapters.** A port "identifies a purposeful conversation", an adapter "converts the API definition to the signals needed by that device", and the application is "developed and tested in isolation from its eventual run-time devices and databases" ([Cockburn](https://alistair.cockburn.us/hexagonal-architecture/)). A `foreign` declaration is a port; a module, a gateway and a Curios root are adapters; a target table says which each target plugs in.
- **Fakes against mocks.** Fakes "actually have working implementations" with a shortcut; mocks are "pre-programmed with expectations" and verify behavior ([Fowler](https://martinfowler.com/articles/mocksArentStubs.html)). A delivery is a fake.

## Decisions

Settled 2026-11-01.

- **A target table's `path` may name a `.crs`, and the kind is the extension.** `.wasm` is a module, `.crs` a Curios root, `.js` a gateway in the browser table; a kind a table does not take is refused where the manifest is read. A `.crs` takes no `hash`, being the package's own source, and one written for it is refused saying so; `hash` stays on delivered files. Rejected: a key per kind; the kind read off the file's bytes, a rule nobody can read off the manifest.
- **The root is a unit of the package, mounted at the row's `name`, compiled over the dependencies and the library, in no other unit's scope.** The row's name is "a single legal identifier" already, and the root names itself by it in its own `use` lines, as every unit with modules names itself absolutely, relative paths searching no ancestor ([`documentation/syntax.md`](../../syntax.md)). Nothing imports it: it is in the program's link and in no unit's `units`, so a `use` into it from the entry fails as a name that does not exist. Rejected: a loose file, which sees `/std` alone and so has neither modules nor the library; a module of the library, shipped to every consumer and nameable by the library it answers; a package — its own name, dependencies, rows and pin, and an umbrella to hold it as a member; a mount under the package, `/<package>/native/<row>`, which mounts cannot be; a mount under a keyword segment, `/<package>/foreign/<row>`, which the grammar refuses in a path and so the root could not write for itself (probed: `mod foreign;` and `use /std/foreign/{Nat};` are both refused); the empty prefix, which the entry holds and under which two deliveries' private names would collide.
- **A row is named as a package is.** A row's name is a one-segment claim in the compilation's flat namespace, beside the packages' and the entry's top-level modules, and a collision is refused at link naming both claimants. `documentation/usage.md` states the rule beside `name`.
- **One root per row, placed as a root.** Its files are its chain's; a root with modules takes a directory to itself; a file two chains reach is refused as today; a root with a final term is refused naming the `[[executables]]` row that would run it. A root may stand in either target table or both.
- **An answer is a `pub` declaration of the root, named below it.** A `[foreign.answers]` value for a Curios root is a path from the root, `query` the root file's own declaration and `fakes/query` one in its `mod fakes`, and is held to its declaration's type exactly where the delivery compiles, the library being among its predecessors. Rejected: a check at link alone, after `lint` has passed the package.
- **The splice is the statement.** At the ANF link, before optimization, every `Rhs::Foreign` and every `Terminator::Halt` of the row, in whatever function holds one, becomes the forcing of the answer's description over the statement's operands, the lane operands beside them unused; a `Halt` so replaced returns what the force answers, by type a suspension or nothing. The answer's `Io` composes with the program's: it may suspend, wait and be cancelled, and no token or copy stands between them. Rejected: replacing the declaration's `let` or the hoisted `step`, both inlined away (probed above); replacing the `ffi` import, whose lane carries host tokens a guest suspension cannot travel in.
- **An answer is held to the wire's numeric limits as every answerer is.** A `Nat` or `Int` operand past what the wire carries, and a result past it, stop the program as they would at the real row, so a native test fails where the gateway's wire would.
- **An answer that reaches its own row is refused at link.** Where the code reachable from an answer — through calls and closure references — holds a statement of a row the same delivery answers, the link refuses naming the chain; the check is conservative, and a dynamically unreachable path is refused with the rest.
- **`lint` and `test` read every Curios delivery a manifest names, whatever the target.** A browser-table root is compiled on a machine that opens no browser, so the unused target's answer cannot rot; `test` takes each delivery as a subject after the executables, its own `test` declarations run as any unit's are.
- **A delivery is a fake, not a mock.** It cannot tell a test what it was called with; a test that verifies interactions is written against operations passed as values, and `documentation/usage.md` says so where it describes a Curios delivery.

## Open questions

- **Where the delivery stands in the fold.** Each unit's scope is its predecessors, so a delivery stands after the library and outside the entry's resolution: after the entry in the sequence, or before it and left out of its scope. Settled by reading `curios-pipeline`'s fold and `Program`'s units in stage 2.
- **How the wire limit is kept at a splice.** The refusals the emitter inserts at a real call (`NatWire`, `IntWire`, a byte past 255) stated once and applied at the splice, or a narrowing the splice inserts. Settled in stage 3.
- **`document`.** Whether a delivery's declarations get pages; they are the link's interface, not a reader's. Lean: no.
- **A dependency only a delivery needs** lands in the package's `[dependencies]`, compiled for every consumer. A dependency table scoped to a delivery is a later refinement; nothing here depends on it.
- **The report naming the answerer.** `↳ Linking` could name, for each row the program reaches, what answered it.
- **A row every stated target answers in Curios** is a function with extra steps; a lint could say so.
- **A consumer answering a dependency's row.** A package speaks for its own declarations, so a dependency shipping a gateway and no native delivery is refused natively with no fix on the consumer's side; the consumer's recourse is `test --target browser`. Kept as the rule; filed if a consumer asks.
- **`pin foreign` for a `.crs`.** It writes the path and no hash; whether it describes the root's row-shaped public declarations, as it lists a module's exports, needs a compile it does not make today.

## Stages

### 1. The manifest takes a Curios root

One commit.

- `[foreign.native]` and `[foreign.browser]` read a `.crs` `path` as a delivery of its own kind, no `hash` taken and one written refused; `curios pin foreign … --path x.crs` writes the path alone; `curate` fetches nothing for it.
- Check: `curios-package`'s manifest and pin tests — a `.crs` in either table, a `.crs` with a hash refused, an extension no table takes refused.

### 2. The root is placed and compiled

One commit.

- `selection.rs` places a file under a delivery root's chain as under an executable's; `Program` carries the target's delivery roots beside `units`, each compiled as a unit over the dependencies and the library at the row's mount, in no other unit's scope; a root with a final term refused; two claims of one one-segment name refused naming both.
- The answers check: each `[foreign.answers]` value of a Curios root names a `pub` declaration of the root whose type is its declaration's, refused otherwise with both types.
- Check: cases in `curios/src/tests/packages.rs` — a delivery root compiled and its files placed, `wonder diagnostics` on one of its modules, a wrong answer type refused, a `use` into the root from the entry refused as unbound, a row named as a dependency package refused.

### 3. The splice

One commit.

- The ANF link replaces every `Rhs::Foreign` and `Terminator::Halt` of a row a Curios root answers with the forcing of the answer's description, before `anf-optm`; the reachability refusal; the wire limit at the splice.
- Check: `curios-anf` link tests for the rewrite on a statement in a nested function, on a `Halt`, and on a fragment the evaluator folded; end to end under `curios/src/tests/host/` — an answer that returns, one that suspends on a deadline, one for a `Never` row that prints before exiting, a top-level value built from the row, an inlined call, a `Nat` past the wire refused, and a self-reaching answer refused naming the chain.

### 4. `lint` and `test` read every delivery

One commit.

- `lint` compiles every Curios root a manifest names for any target; `test` takes each as a subject after the executables, under `--filter` as any unit.
- Check: a package with a browser-table root linted and tested on the native target, its own tests reported under its mount.

### 5. Documentation

One commit.

- `documentation/usage.md`: the manifest's `[[foreign]]` section states a Curios root, its mount, its placement, the rule for naming rows and the fake-not-mock sentence; `test`'s section lists deliveries among a package's subjects; "Each side is tested with its own tools" no longer names a `.wasm` as the stand-in.
- [A foreign row states a delivery for each target](../../design/tools/a-foreign-row-states-a-delivery-for-each-target.md) is rewritten: a delivery is a module, a gateway or a Curios root, with the rationale and rejections above, the `.wasm` stand-in rationale replaced; `curios-anf/README.md` gains the splice's decision.
- `documentation/syntax.md` is untouched — no grammar changes — and [Foreign calls past scalars and byte strings](../runtime/300-foreign-calls-past-scalars.md) stands, its ceiling a module's.

## Verification

`cargo xtask workspace lint` and `cargo xtask workspace format`; the named `curios-package`, `curios-anf` and `curios` tests; `curios-prelude`'s corpus tests, since the link changes. Then the check this specification shares with [305](305-a-browser-test-run-is-answered-by-whatever-opens-its-page.md): one package whose row has a gateway and a Curios root, its tests passing under `curios test` and under `curios test --target browser`.

## Retirement

The specification is deleted when the stages land, the contracts owned by `documentation/usage.md`, `curios-package/README.md` and `curios-anf/README.md`, the rationale by the rewritten decision, and the open questions settled there or filed as findings.
