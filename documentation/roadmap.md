# Roadmap

The one list of Curios's work: every capability landed or pending, and every place the code falls short of a rule the documents state, each open line linked to the spec a contributor starts from.

The areas follow [`design/`](design)'s subjects. Each opens with its open work, then lists what has landed, one line per capability; a landed line's detail is its design decision's, its crate's `README.md`'s and its tests'.

An open line says what is missing or wrong today, and where, and links its spec under [`roadmap/`](roadmap): one directory per area once the area holds two files, and a lone spec loose in `roadmap/` itself, its line directly under this description, before any area. Directories and specs carry their index in this file's order, renumbered when an item lands or is inserted. An area opens with its findings, `00-findings.md`: small fixes and possible bugs, each naming where, what is wrong, the fix, its check and its size, worked as one pass; an entry marked uncertain says what is not yet known, and is investigated before its fix is taken. Its specs follow, those where the code breaks a rule a document states first, then capabilities and costs, refined before unrefined, and last those waiting for a consumer. A spec states its context from the code, its goal, the decisions already settled and the questions still open, stages each with its own check, its verification, and its retirement. A spec not yet refined is marked "Not refined yet" and says what is known and what it waits on. A design decision states the intended rule, so where the code falls short the gap is an open line here, never a caveat there.

When an item lands, its contracts go to the owning rustdoc, `README.md` and tests, its rationale and rejected alternatives to a design decision or the crate's `README.md`, and its line here becomes a checked summary; once nothing references the spec, the spec is deleted.

## Soundness

- [ ] [Findings](roadmap/01-soundness/00-findings.md) — the elaborator's conversion fires eta whatever the goal type, the two checkers part on unit eta where a program reaches it, and seven spellings the strict-positivity entry calls attacked have no fixture
- [ ] [The two checkers' conversion held to each other](roadmap/01-soundness/01-conversion-held-across-checkers.md) — not refined yet; their conversion meets only where the corpus sends both, and their recurrence keys and untyped child positions differ
- [ ] [The certifier confirms what it skips](roadmap/01-soundness/02-the-certifier-confirms-what-it-skips.md) — not refined yet; an item under a name already in scope is not judged, and the mount disjointness that keeps one from arriving is checked in `curios-text`
- [ ] [Checked evidence and trusted reasoning](roadmap/01-soundness/03-checked-evidence.md) — not refined yet; certificate transport and stronger restrictions on trusted implementations, beginning once [the relational layer](roadmap/04-arithmetic/08-relational-layer.md) has a consumer
- [x] [Totality of everything erasure deletes](design/soundness/totality-of-the-erased-program.md): nothing reachable from a type and nothing at a proposition is partial, decided per recursive group by size-change termination, so no closed term inhabits `/std/Bool/False`
- [x] [An independent kernel re-checks what the elaborator accepts](design/soundness/an-independent-kernel-re-checks-what-the-elaborator-accepts.md), the trusted base being `curios-cert` and the layer both checkers share
- [x] [The soundness perimeter](design/soundness/the-soundness-perimeter.md): every rule that can admit a term, graded probed, argued or auditable, with its fixtures
- [x] The checkers agree on what they accept: one rule records a case equation for both, and a solution, a candidate or a bound's fill is judged under the refinements it was born under
- [x] The certifier files its own verdicts: each unit carries its record of every definition's totality and of what judging it read, and [a group's calls are the ones the kernel types](../curios-cert/README.md#a-groups-calls-are-the-ones-the-kernel-types)
- [x] [A reduction step costs what it builds](design/soundness/a-reduction-step-costs-what-it-builds.md), deterministic across machines, with every memo cleared where the budget is restored

## Types

- [ ] [Findings](roadmap/02-types/00-findings.md) — the kernel refuses to classify a universe instance over a bodiless scheme
- [ ] [A universe level only a parameter's type mentions is irrelevant](roadmap/02-types/01-irrelevant-universe-levels.md) — both checkers compare a nominal type's levels for equality, so `!` holds its region at the level of a nominal action it binds
- [ ] [A universe level settled before its evidence is in](roadmap/02-types/02-levels-settled-before-their-evidence.md) — not refined yet; a generic declaration dispatching through a witness declared later settles at its least levels, and two instances' levels are identified where unfolding alone would decide
- [ ] [A subsumption blocked on a metavariable waits as a subsumption](roadmap/02-types/03-blocked-subsumption.md) — not refined yet; the elaborator hands it to conversion, refusing what the relation admits
- [ ] [Flex–flex problems with distinct heads](roadmap/02-types/04-flex-flex-intersection.md) — not refined yet; `?0(x) ~ ?1(x)` parks undecided, with no intersection
- [ ] [Strict positivity through a type-former parameter](roadmap/02-types/05-positivity-through-type-formers.md) — not refined yet; `induct Mu(F : (Type) -> Type)` is refused, since its body cannot say how `F` uses its argument
- [ ] [K-like reduction](roadmap/02-types/06-k-reduction.md) — not refined yet; a relevant match on a stuck `Eq` proof does not reduce; waits for a program that needs it
- [x] Π- and Σ-types with eta for both, named tuple fields, and `let` bindings recursive by their body, with `let … and …;` groups
- [x] [An implicit, cumulative universe hierarchy whose levels settle by where they came from](design/types/a-universe-level-is-implicit-cumulative-and-settles-by-where-it-came-from.md), polymorphic per declaration
- [x] [`Prop`, strict, proof-irrelevant and definitionally K](design/types/prop-is-strict-proof-irrelevant-and-definitionally-k.md), irrelevance asked before either side is reduced
- [x] [Plicity as part of function identity](design/types/plicity-is-part-of-function-identity.md): implicit binders, inserted lambda binders, and [one parameter group per call](design/types/a-call-fills-one-parameter-group.md)
- [x] [Subsumption as a relation](design/types/subsumption-is-a-relation-not-a-traversal-order.md), decided structurally in both checkers
- [x] Inductive families: constructor registry and dependent eliminators, indexed families, [coverage by index inversion](design/types/an-arm-is-checked-in-a-context-specialized-by-index-inversion.md), the large-elimination guard, and [strict positivity modulo polarity](design/types/strict-positivity-modulo-polarity.md)
- [x] [A motive is a term](design/types/a-motive-is-a-term-not-a-grammar.md), and an omitted one is the expected type, specialized per arm with no convoy
- [x] Bidirectional elaboration: pattern unification over metavariable spines, re-validation in checking mode, postponement on a blocked conversion, right-biased imitation for flex-apply, and projections and matches that wait on a stuck head's metavariable

## Surface

- [ ] [Findings](roadmap/03-surface/00-findings.md) — a leading byte-order mark refused as an invisible character, and the parser naming commitment twice and overstating its public surface
- [ ] [Typed patterns](roadmap/03-surface/01-typed-patterns.md) — a wildcard beside a concrete pattern is refused in any but the first column, coverage is not checked against the scrutinee's constructors, and a redundant arm is not reported
- [x] `struct` and `induct` declarations with independent nominal and representation visibility; structure, concept and witness groups
- [x] [Privacy scoped to a subtree](design/surface/privacy-is-scoped-to-a-subtree.md), with sealed representations and an exact private-item-in-public-interface audit
- [x] [Concepts resolved with global coherence](design/surface/concepts-resolve-with-global-coherence.md): one witness per key — a type's head, a tuple's shape, a partially applied constructor's stuck head — the orphan rule, decreasing premises, higher-kinded parameters, laws, associated types and superclass edges
- [x] [Syntax forms closed, their meaning extended by witness](design/surface/syntax-forms-are-closed-semantics-extend-by-witness.md): every operator and `!` dispatches through a `/std` concept, and `Lift` embeds one monad in another, never chained
- [x] [A witness body may be written by the compiler](design/surface/a-witness-body-may-be-written-by-the-compiler.md), for `Spell`, `Eql`, `Ord` and `Hash`
- [x] [A literal is realized by its expected type](design/surface/a-literal-is-realized-by-its-expected-type.md): numerals, characters as numerals, strings and block strings, packed `b[…]` and `x[…]`, and signed non-finite floats
- [x] Pattern matching: nested, tuple and struct patterns compiled as a matrix over several scrutinees, the intrinsic match families, a final `_` default, and `choose` with refutable bind arms
- [x] Sugar kept verbatim for printing: signature and field-function sugar, postfix `!`, projections, spreads in lists, packed literals and structure updates, trailing commas, and irrefutable destructuring binders

## Arithmetic

- [ ] [What conversion still decides by spelling or by cap](roadmap/04-arithmetic/01-decided-by-spelling-or-cap.md) — not refined yet; refinement lookup turns on a scrutinee's spelling and the checkers part over it, atoms pair by hash, Boolean agreement stops at a cap, and the law grid refuses parity as a clash, map fusion and a symbolic shift's exponent law
- [ ] [Declared operations](roadmap/04-arithmetic/02-declared-operations.md) — `min`, `max`, `abs` and `sign` are library functions conversion sees unfolded, `pow` is no operation, the float identities that hold for every bit pattern are refused, and the families `Nat` declares are undecided at `Int`
- [ ] [Numeric laws](roadmap/04-arithmetic/03-numeric-laws.md) — `Divides` lacks its converse bridge, no `gcd` a type may mention exists, `Int` lacks multiplicative cancellation and its signed scale, and `Flt`'s `ord` is not proved a total order
- [ ] [Exact rationals and their laws](roadmap/04-arithmetic/04-exact-rationals.md) — no canonical rational library, executable binary64 conversions, exact decimals or library proofs
- [ ] [Alternate floating-point exception handling](roadmap/04-arithmetic/05-flt-exception-handling.md) — IEEE 754's remaining §8 policies, and per-operation substitution and recording
- [ ] [Correctly rounded elementary functions](roadmap/04-arithmetic/06-flt-elementary-functions.md) — the §9.2 functions are absent, and fuel and exhaustion need a contract
- [ ] [Proofs of rational–binary64 conversion](roadmap/04-arithmetic/07-binary64-conversion-proofs.md) — not refined yet; the boundary theorems need a specified connection to the primitive model, and a `/sys/Flt` fold trusts the Rust model rather than the Curios twin
- [ ] [Relational facts decided in conversion, justified by checked evidence](roadmap/04-arithmetic/08-relational-layer.md) — not refined yet; waits for a consumer needing a relational fact by conversion rather than by proof
- [x] The intrinsic carriers: `Bool`; `Byte`; [`Nat` and `Int` unbounded at run time, an i31 until they outgrow it](design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md); packed `Bits` and `Bytes` with O(1) windows and their bitwise vocabulary; `List`
- [x] [`Flt` specified by a model the runtime conforms to](design/arithmetic/flt-is-specified-by-a-model-and-the-runtime-conforms.md): IEEE 754-2019 binary64 over every bit pattern, the five directions and `fma`, exceptions as values and the environment as a monad, decimal and hexadecimal text in every direction, and §9.4's reductions and §9.5's augmented operations
- [x] [A partial primitive is totalized or states its domain](design/arithmetic/a-partial-primitive-is-totalized-by-a-canonical-extension-or-it-states-its-domain.md), the bound reaching Core for the kernel to re-check
- [x] [The carriers' algebra stays in conversion](design/arithmetic/the-carriers-algebra-stays-in-conversion.md): laws stated once over abstract atoms, one conversion chain for both checkers, and a law grid generated from each operation's declared families and audited
- [x] [A law is decided where it neither respells nor invents](design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md): the sum normal form, Euclid's identity, a comparison split by sign, the `Nat`–`Int` embedding, a shift as a coefficient, a position through a window, a map by the identity, and Boolean agreement by truth table
- [x] [A bound is a decided proposition discharged by reduction](design/arithmetic/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md), filled on retry once its proposition is known
- [x] [A bound that follows from the facts in scope is proved by the elaborator](design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md), by linear arithmetic with a quotient's bounds, subtraction's cases and products, in an ordinary term both checkers recheck
- [x] Certified division with remainder and divisibility (`/std/Nat/div_mod`, `/std/Nat/Divides`), and `Int`'s order carried from `Nat` along the embedding

## Effects

- [ ] [Findings](roadmap/05-effects/00-findings.md) — a channel's nonblocking faces merge the outcomes the concurrency decision keeps apart
- [ ] [Foreign calls past scalars and byte strings](roadmap/05-effects/01-foreign-calls-past-scalars.md) — not refined yet; a `Handle`, a `List` and several results at once are each refused where a plugin's signature is read
- [x] [Effects are descriptions, and the carrier has no eliminator](design/effects/effects-are-descriptions-and-the-carrier-has-no-eliminator.md): `Io` built by `pure` and `bind`, forced once by the entry point
- [x] [A fallible operation returns `Try`, and `!` lifts along declared edges](design/effects/a-fallible-operation-returns-try-and-bang-lifts-along-declared-edges.md), with `Result` error first and its own monad
- [x] [A host operation has one contract, checked at both ends](design/effects/a-host-operation-has-one-contract-checked-at-both-ends.md): each row typed in `curios-abi`, read by `/sys` as a `Result` or an `Option`, held to its row by the native adapter and by the guest, and conformed across the native, mock, plugin and browser hosts
- [x] [Only a fiber waits](design/effects/only-a-fiber-waits.md): every peer-facing handle non-blocking, single-attempt writes and an explicit flush, write-once cells, bounded channels and level waiting in the guest heap
- [x] Structured concurrency in `/std/Async`: fibers and tasks, `race`, `select` and `join_all`, `sleep` and `timeout`, scoped resources, and deadlock detection
- [x] Host capabilities: terminal with raw mode, files and the filesystem over `Path`, clock and randomness, process IO and subprocesses, TCP with TLS, and serial ports
- [x] Foreign functions: `foreign` declarations answered by a WebAssembly module a package names and pins, linked by `run` and `test`, and carried inside a `compile`d executable

## Lowering

- [ ] [Findings](roadmap/06-lowering/00-findings.md) — a `Nat` shift by a count of `2³²` or more computing a different number, a sequence that long answering a wrong length or stopping on a bare trap, a flag licensing a deletion no pass performs while dead calls stand, a merge path perhaps unreachable, a specialization key that reads any literal as a tag, and an assertion restating the verifier
- [ ] [What unbounded `Nat` and `Int` still cost at run time](roadmap/06-lowering/01-unbounded-nat-costs.md) — not refined yet; a field is a reference, a chain boxes between steps, and the fast path tests a tag per operand
- [ ] [Contification of a function with several return contexts](roadmap/06-lowering/02-multi-site-contification.md) — not refined yet; such a function stays a function, and nothing downstream contifies it
- [x] [WebAssembly-GC is the only target](design/lowering/webassembly-gc-is-the-only-target.md), serialized by `curios-wasm` with text round-tripped against the binary writer, and optimized closed-world by Binaryen
- [x] The erased IR: flat, verified arenas; erasure as transcription; behaviour-summary pruning, partial evaluation and monoid rebasing; one lowering into continuations
- [x] The continuation IR: a pre-closure CPS graph with delayed closure conversion, an interprocedural optimizer, SCC specialization, a dataflow substrate for unboxed scalars, return through several continuations, and structured control flow by SCC condensation
- [x] Value representation: [a variant collapses when nothing needs to distinguish it](design/lowering/a-variant-collapses-when-nothing-needs-to-distinguish-it.md), [a field is declared at the carrier its shape names](design/lowering/a-field-is-declared-at-the-carrier-its-shape-names.md), [a value costs when it is kept, not when it is named](design/lowering/a-value-costs-when-it-is-kept-not-when-it-is-named.md), and a closure carries its code as a table index
- [x] [A lowering names the elimination it performs](design/lowering/a-lowering-names-the-elimination-it-performs.md), and [a refusal is a panic the emitter renders](design/lowering/a-refusal-is-a-panic-the-emitter-renders.md)

## Tools

- [ ] [Findings](roadmap/07-tools/00-findings.md) — a test that exits early counted by its exit code, a formatter that moves comments onto the wrong line, refusals that print internal paths, lack a location, read one fault five ways or point elsewhere, a report spelling names through imported modules in full, test instruments that pass with their defect present, measurement readings the code has moved past, CLI refusals that come late or at the wrong code, a documentation build that succeeds in silence, a benchmark cross-check that compares nothing, a corpus nothing holds to the formatter, CLI migration shims and dead page styles
- [ ] [Questions file what they compile](roadmap/07-tools/01-questions-file-what-they-compile.md) — `lint` and the `wonder` queries file nothing, so each invocation compiles every unit no build has filed again, and a server session starts cold
- [ ] [Profiling in the budget's own units](roadmap/07-tools/02-profiling-in-budget-units.md) — not refined yet; a profile reports durations rather than the budget's machine-independent units, and counts no priced site
- [ ] [A binary reader for `curios-wasm`](roadmap/07-tools/03-wasm-binary-reader.md) — not refined yet; the binary side is checked only by the engine's acceptance
- [x] [A diagnostic spells what its reader can write](design/tools/a-diagnostic-spells-what-its-reader-can-write.md): spans across every stage, names as resolution reaches them, written goals reporting their scope and verified candidate fits
- [x] [An argument names one subject, and each command states what it accepts](design/tools/an-argument-names-one-subject-and-each-command-states-what-it-accepts.md): `run`, `compile`, `document`, `test`, `curate`, `pin`, `new`, `lint`, `format`, `wonder` and `profile`
- [x] `curios wonder`: diagnostics, tests, a declaration's fate in the optimizer and any pipeline rung, over the command line and a language server
- [x] Editor support: a tree-sitter grammar, and Zed and VS Code extensions over `wonder server`
- [x] [A printer states each fact once, where it is bound](design/tools/a-printer-states-each-fact-once-where-it-is-bound.md), with one layout engine and a canonical formatter verified by reparse
- [x] [A lint is an exact finding read off the compilation](design/tools/a-lint-is-an-exact-finding-read-off-the-compilation.md): four, always on
- [x] [A test is a declared description, and a proof is a `let`](design/tools/a-test-is-a-declared-description-and-a-proof-is-a-let.md)
- [x] [A library is documented for its consumers, from the compilation that builds it](design/tools/a-library-is-documented-for-its-consumers-from-the-compilation-that-builds-it.md)
- [x] Profiling through `curios-profile`: `--profile` writes a stream as it runs, `curios profile` reads it back, and `cargo x profile` builds and folds one
- [x] Distribution: CI, tag-triggered releases for Linux and macOS, a checksum-verified installer, and a browser playground
- [x] The language reference, the command-line reference, and cross-language benchmarks against six other languages

## Architecture

- [ ] [Findings](roadmap/08-architecture/00-findings.md) — the kernel's conversion inferring a type it should look up, a filter and a checker-shared bound each written twice, two printers that hand-roll their frames, an unused dependency of the certifier, a stack segment taken twice, an error path nothing reaches, names of a retired root, a test-only constructor in production, and history in a build script an edit would rebuild
- [ ] [A shared term costs its size](roadmap/08-architecture/01-shared-term-costs.md) — settlement is a sixth of `/std`'s elaboration, `capture` loses sharing, `shift`, `release` and the kernel's typing walk an open term per path, and a sum is flattened afresh on every read
- [ ] [One environment, and every read recorded](roadmap/08-architecture/02-one-environment.md) — the item graph is computed three times, the elaborator threads state from item to item, and every compile re-seeds its whole scope
- [ ] [A compilation is a graph of item tasks](roadmap/08-architecture/03-item-tasks.md) — nothing the compiler holds can cross a thread, so a compilation occupies one core
- [ ] [Size cliffs](roadmap/08-architecture/04-size-cliffs.md) — not refined yet; elaboration is not linear in `let` depth, the parser buys its depth with stack, and every binding gets a fresh local
- [x] [A module is a compilation unit, and the prelude is an environment](design/architecture/a-module-is-a-compilation-unit-and-the-prelude-is-an-environment.md): units folded over a dependency order, every edge declared in a manifest, and a package named `std` compiled as the standard library
- [x] Crate boundaries: a pipeline with no back end, a launcher with no Cranelift or Binaryen, the shared analyses apart from the certifier, and the emitter out of the prelude build
- [x] The prelude built once per compiler build, archived and certified, and restored with no source fallback
- [x] Packages: manifests and discovery, exactly pinned dependencies in a content-addressed store, and `curios pin` deriving a row's hash from the delivery
- [x] [Cached verdicts](design/soundness/admission/cached-verdicts.md) and [reused payloads](design/soundness/admission/reused-payloads.md), and [a stored unit is a baseline for an item-level recompile](design/architecture/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md)
- [x] A unit carries no identity another compilation could mint, and nothing branches on a name's spelling
- [x] [Depth is bought with stack, not with hand-rolled frames](design/architecture/depth-is-bought-with-stack-not-with-hand-rolled-frames.md)
- [x] Compile cost: per-node memoization bounded by written binder nesting, a closed fold on the shared machine, a type-level concatenation that copies nothing, a string literal checked once per use, and the certifier measured by item and judgment
- [x] [One crate is the authority for one external concern](design/architecture/one-crate-is-the-authority-for-one-external-concern.md), and [every gate step catches what no other step does](design/architecture/every-gate-step-catches-what-no-other-step-does.md)

## Standard library

- [ ] [Findings](roadmap/09-standard-library/00-findings.md) — `Json` accepts a raw control character; a URL's port, `query_pairs` and `Flt`'s `div_mod` answer what their standard or contract does not; a terminal session reads a failed read as silence; no inventory says which defaults are specified; `Html`'s documentation overstates what it reads; `Int` has no `Map` key and nothing keeps it out; and two definitions could say more with what the elaborator proves
- [ ] [HTTP messages as RFC 9110 and RFC 9112 frame them](roadmap/09-standard-library/01-http-framing.md) — HTTP neither reads nor writes a message as RFC 9110 and RFC 9112 frame it, and refuses a head's opaque octets where they are data
- [ ] [Text read as text, and numbers by each format's grammar](roadmap/09-standard-library/02-text-read-as-text.md) — `Json` accepts `01` and writes infinity as `null`, the text formats walk the bytes of input that began as text, and `Flt`'s readers cut text where a grammar belongs
- [ ] [A certified sort and an `Ord`-keyed tree](roadmap/09-standard-library/03-certified-sort-and-ord-tree.md) — not refined yet; `sort` is pinned by properties rather than proved, and `Map` is keyed only through `Bytes` encodings
- [x] Foundations: `/std/Bool`'s `True`, `False` and `Holds`, `Eq` and `Ordering`, `Option` and `Result`, `State`, `Try`, `Io/Error` and `Path`
- [x] Collections: `List` and its helpers, `Vec` counting its list in its type, and `Map`, a canonical crit-bit trie over `Bytes` keys
- [x] Text: proof-carrying UTF-8 `Str` addressed by proved byte positions, certified `Char`, parser combinators, typed format strings, and decimal conversions that round-trip
- [x] Formats: `Json` over binary64, `Toml` 1.0.0, and `Html` as a tree
- [x] Applications: an HTTP client and server over TCP and `Async`, command-line interfaces, and terminal programs with widgets
- [x] [Explicit invariants](../curios-prelude-archive/README.md#std-invents-no-value-where-a-proof-belongs-and-text-is-addressed-by-position): decoding and encoding carry their certificates, indices carry their bounds, and a host's facts arrive first-order

## Runtime

- [x] Execution on a shared, GC-enabled Wasmtime engine, from precompiled `.cwasm` compiled across threads
- [x] A slim launcher embedded in the compiler, and self-contained executables carrying their foreign modules
- [x] [The heap is sized ahead of its churn](../curios-runtime/README.md#the-heap-is-sized-ahead-of-its-churn)
