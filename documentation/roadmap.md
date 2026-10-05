# Roadmap

The one list of Curios's work: every capability landed or pending, and every place the code falls short of a rule the documents state, each open line linked to the spec a contributor starts from.

The areas follow [`design/`](design)'s subjects. Each opens with its open work, then lists what has landed, one line per capability; a landed line's detail is its design decision's, its crate's `README.md`'s and its tests'.

An open line says what is missing or wrong today, and where, and links its spec under [`roadmap/`](roadmap): one directory per area, and a spec about the workspace as a whole loose in `roadmap/` itself, its line right after these opening paragraphs, before any area, with the workspace's other lines. Directories and specs carry their index in this file's order. An area's specs run those where the code breaks a rule a document states first, then capabilities and costs, refined before unrefined, and last those waiting for a consumer. A spec states its context from the code, its goal, the decisions already settled and the questions still open, stages each with its own check, its verification, and its retirement. A spec not yet refined is marked "Not refined yet" and says what is known and what it waits on. A design decision states the intended rule, so where the code falls short the gap is an open line here, never a caveat there.

An area's directory opens with its findings, `00-findings.md`, and `roadmap/00-findings.md` holds those of the workspace as a whole; findings have no line here, and are small fixes and possible bugs, worked as one pass, each a bullet led by the finding in bold and naming where, what is wrong, the fix, its check and its size. An entry marked uncertain says what is not yet known, and is investigated before its fix is taken. The commit that fixes a finding deletes its entry.

When an item lands, its contracts go to the owning rustdoc, `README.md` and tests, its rationale and rejected alternatives to a design decision or the crate's `README.md`, and its line here becomes a checked summary; once nothing references the spec, the spec is deleted.

- [x] Crate boundaries: a pipeline with no back end, a launcher with no Cranelift or Binaryen, the shared analyses apart from the certifier, and the emitter out of the prelude build
- [x] [One crate is the authority for one external concern](design/one-crate-is-the-authority-for-one-external-concern.md), and [every gate step catches what no other step does](design/every-gate-step-catches-what-no-other-step-does.md)
- [x] CI: on every push to `main`, the gate's steps on Ubuntu and the runtime's tests on macOS as well

## Soundness

- [ ] [The certifier confirms what it skips](roadmap/01-soundness/02-the-certifier-confirms-what-it-skips.md) — a declaration under a name already in scope is passed over whatever it is, a registry entry among them live and unchecked, and what keeps one from arriving is checked outside `curios-cert`
- [ ] [Checked evidence and trusted reasoning](roadmap/01-soundness/03-checked-evidence.md) — no procedure in the certifier's closure is classified against the grade, no step of the gate holds the closure, and reasoning found outside the kernel has no evidence the certifier checks, which opens with [the relational layer](roadmap/04-arithmetic/08-relational-layer.md)
- [x] [Conversion is one relation in both checkers](design/soundness/conversion-is-one-relation-in-both-checkers.md): every rule of conversion held to its laws — reversed, chained, substituted and under every child a term former holds — with each checker asked by itself and near misses among the rows; one order of rules, one set of typed positions and one reading of a goal's type in both; and two neutrals decided by the shape of their type wherever eta cannot be fired
- [x] [Totality of everything erasure deletes](design/soundness/totality-of-the-erased-program.md): nothing reachable from a type and nothing at a proposition is partial, decided per recursive group by size-change termination, so no closed term inhabits `/std/Bool/False`
- [x] [An independent kernel re-checks what the elaborator accepts](design/soundness/an-independent-kernel-re-checks-what-the-elaborator-accepts.md), the trusted base being `curios-cert` and the layer both checkers share
- [x] [The soundness board](design/soundness/the-soundness-board.md): every rule that can admit a term, graded probed, argued or auditable, with its fixtures
- [x] The checkers agree on what they accept: one rule records a case equation for both, and a solution, a candidate or a bound's fill is judged under the refinements it was born under
- [x] The certifier files its own verdicts: each unit carries its record of every definition's totality and of what judging it read, and [a group's calls are the ones the kernel types](../curios-cert/README.md#a-groups-calls-are-the-ones-the-kernel-types)
- [x] [A reduction step costs what it builds](design/soundness/a-reduction-step-costs-what-it-builds.md), deterministic across machines, with every memo cleared where the budget is restored

## Theory

- [ ] [A universe level settled before its evidence is in](roadmap/02-theory/02-levels-settled-before-their-evidence.md) — identifying two instances' levels refuses a bare former passed as a family, and `!` through a transformer over one at two decided levels, where its eta-expansion is accepted, at a cost across `/std` not yet measured, and a signature's level that a witness's side condition bounds settles at zero, so a function over a family that carries a type cannot be handed one above it
- [ ] [A subsumption blocked on a metavariable waits as a subsumption](roadmap/02-theory/03-blocked-subsumption.md) — the elaborator hands a pair blocked on a metavariable to conversion as an equation, and solves a metavariable met in one by equality, so one call is admitted or refused by the order of its arguments
- [ ] [Flex–flex problems with distinct heads](roadmap/02-theory/04-flex-flex-intersection.md) — not refined yet; `?0(x) ~ ?1(x)` parks undecided, with no intersection
- [ ] [Strict positivity through a type-former parameter](roadmap/02-theory/05-positivity-through-type-formers.md) — not refined yet; `induct Mu(F : (Type) -> Type)` is refused, since its body cannot say how `F` uses its argument
- [ ] [K-like reduction](roadmap/02-theory/06-k-reduction.md) — not refined yet; a relevant match on a stuck `Eq` proof does not reduce; waits for a program that needs it
- [x] Π- and Σ-types with eta for both, named tuple fields, and `let` bindings recursive by their body, with `let … and …;` groups
- [x] [An implicit, cumulative universe hierarchy whose levels settle by where they came from](design/theory/a-universe-level-is-implicit-cumulative-and-settles-by-where-it-came-from.md), polymorphic per declaration, with [a nominal type's levels compared by its family's variance](design/soundness/conversion/a-nominal-types-levels-compare-by-variance.md) in both checkers, so `!` sequences actions whatever level their payloads sit at
- [x] [`Prop`, strict, proof-irrelevant and definitionally K](design/theory/prop-is-strict-proof-irrelevant-and-definitionally-k.md), irrelevance asked before either side is reduced
- [x] [Plicity as part of function identity](design/theory/plicity-is-part-of-function-identity.md): implicit binders, inserted lambda binders, and [one parameter group per call](design/theory/a-call-fills-one-parameter-group.md)
- [x] [Subsumption as a relation](design/theory/subsumption-is-a-relation-not-a-traversal-order.md), decided structurally in both checkers
- [x] Inductive families: constructor registry and dependent eliminators, indexed families, [coverage by index inversion](design/theory/an-arm-is-checked-in-a-context-specialized-by-index-inversion.md), the large-elimination guard, and [strict positivity modulo polarity](design/theory/strict-positivity-modulo-polarity.md)
- [x] [A motive is a term](design/theory/a-motive-is-a-term-not-a-grammar.md), and an omitted one is the expected type, specialized per arm with no convoy
- [x] Bidirectional elaboration: pattern unification over metavariable spines, re-validation in checking mode, postponement on a blocked conversion, right-biased imitation for flex-apply, and projections and matches that wait on a stuck head's metavariable
- [x] [Effects are descriptions, and the carrier has no eliminator](design/theory/effects-are-descriptions-and-the-carrier-has-no-eliminator.md): `Io` built by `pure` and `bind`, forced once by the entry point

## Surface

- [ ] [Typed patterns](roadmap/03-surface/01-typed-patterns.md) — a wildcard beside a concrete pattern is refused in any but the first column, coverage is not checked against the scrutinee's constructors, and a redundant arm is not reported
- [ ] [Plicity on a telescope's members](roadmap/03-surface/02-plicity-on-a-telescopes-members.md) — a `use` member is refused on a constructor's payload and on a structure's field, `@` on a concept's field, and any mark on a tuple type's field
- [x] [A hidden member takes no position](design/theory/a-hidden-member-takes-no-position.md), on data as on a function: a constructor's payload and a structure's field declare `@`, left out where the value is built, where an arm opens it and where a position reads it, and a fact wherever it is in scope
- [x] `struct` and `induct` declarations with independent nominal and representation visibility; structure, concept and witness groups
- [x] [Privacy scoped to a subtree](design/surface/privacy-is-scoped-to-a-subtree.md), with sealed representations and an exact private-item-in-public-interface audit
- [x] [Concepts resolved with global coherence](design/surface/concepts-resolve-with-global-coherence.md): one witness per key — a type's head, a tuple's shape, a partially applied constructor's stuck head — the orphan rule, decreasing premises, higher-kinded parameters, laws, associated types and superclass edges
- [x] [Syntax forms closed, their meaning extended by witness](design/surface/syntax-forms-are-closed-semantics-extend-by-witness.md): every operator and `!` dispatches through a `/std` concept, and `Lift` embeds one monad in another, never chained
- [x] [A witness body may be written by the compiler](design/surface/a-witness-body-may-be-written-by-the-compiler.md), for `Spell`, `Eql`, `Ord` and `Hash`
- [x] [A literal is realized by its expected type](design/surface/a-literal-is-realized-by-its-expected-type.md): numerals, characters as numerals, strings and block strings, packed `b[…]` and `x[…]`, and signed non-finite floats
- [x] Pattern matching: nested, tuple and struct patterns compiled as a matrix over several scrutinees, the intrinsic match families, a final `_` default, and `choose` with refutable bind arms
- [x] Sugar kept verbatim for printing: signature and field-function sugar, postfix `!`, projections, spreads in lists, packed literals and structure updates, trailing commas, and irrefutable destructuring binders

## Arithmetic

- [ ] [Declared operations](roadmap/04-arithmetic/02-declared-operations.md) — `min`, `max`, `abs` and `sign` are library functions conversion sees unfolded, `pow` is no operation, the float identities that hold for every bit pattern are refused, and the families `Nat` declares are undecided at `Int`
- [ ] [Numeric laws](roadmap/04-arithmetic/03-numeric-laws.md) — `Divides` lacks its converse bridge, no `gcd` a type may mention exists, `Int` lacks multiplicative cancellation and its signed scale, and `Flt`'s `ord` is not proved a total order
- [ ] [Exact rationals and their laws](roadmap/04-arithmetic/04-exact-rationals.md) — no canonical rational library, executable binary64 conversions, exact decimals or library proofs
- [ ] [Alternate floating-point exception handling](roadmap/04-arithmetic/05-flt-exception-handling.md) — IEEE 754's remaining §8 policies, and per-operation substitution and recording
- [ ] [Correctly rounded elementary functions](roadmap/04-arithmetic/06-flt-elementary-functions.md) — the §9.2 functions are absent, and fuel and exhaustion need a contract
- [ ] [Proofs of rational–binary64 conversion](roadmap/04-arithmetic/07-binary64-conversion-proofs.md) — not refined yet; the boundary theorems need a specified connection to the primitive model, and a `/sys/Flt` fold trusts the Rust model rather than the Curios twin
- [ ] [Relational facts decided in conversion, justified by checked evidence](roadmap/04-arithmetic/08-relational-layer.md) — not refined yet; waits for a consumer needing a relational fact by conversion rather than by proof
- [ ] [What the carriers' algebra leaves undecided](roadmap/04-arithmetic/09-left-undecided-by-the-carriers-algebra.md) — not refined yet; Boolean agreement stops at a cap, a metavariable is solved through the algebra only where one equation is linear in it, the law grid refuses parity as a clash, map fusion and a symbolic shift's exponent law, and an arm's equation leaves a question inside a question, a form naming a binder its scrutinee does not and an inverted index unanswered; each waits for a consumer
- [x] The intrinsic carriers: `Bool`; `Byte`; [`Nat` and `Int` unbounded at run time, an i31 until they outgrow it](design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md); packed `Bits` and `Bytes` with O(1) windows and their bitwise vocabulary; `List`
- [x] [`Flt` specified by a model the runtime conforms to](design/arithmetic/flt-is-specified-by-a-model-and-the-runtime-conforms.md): IEEE 754-2019 binary64 over every bit pattern, the five directions and `fma`, exceptions as values and the environment as a monad, decimal and hexadecimal text in every direction, and §9.4's reductions and §9.5's augmented operations
- [x] [A partial primitive is totalized or states its domain](design/arithmetic/a-partial-primitive-is-totalized-by-a-canonical-extension-or-it-states-its-domain.md), the bound reaching Core for the kernel to re-check
- [x] [The carriers' algebra stays in conversion](design/arithmetic/the-carriers-algebra-stays-in-conversion.md): laws stated once over abstract atoms, one conversion chain for both checkers, and a law grid generated from each operation's declared families and audited
- [x] [A law is decided where it neither respells nor invents](design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md): the sum normal form, Euclid's identity, a comparison split by sign, the `Nat`–`Int` embedding, a shift as a coefficient, a position through a window, a map by the identity, and Boolean agreement by truth table
- [x] [A term is one where conversion says so](design/arithmetic/a-term-is-one-where-conversion-says-so.md): a pair's atoms classed by each checker's own conversion, a commutative operation's operands paired and never compared by position, the one metavariable an equation is linear in solved by exact division, an implicit with two solutions refused, a mismatch saying what was not compared, an arm's equation answering the terms conversion holds equal to its scrutinee by one rule in both checkers and following the solution its arm is checked under, a stuck fold taken again over the atoms conversion holds one, and totality reading terms by a reduction that asks conversion nothing
- [x] [A bound is a decided proposition discharged by reduction](design/arithmetic/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md), filled on retry once its proposition is known
- [x] [A bound that follows from the facts in scope is proved by the elaborator](design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md), by linear arithmetic with a quotient's bounds, subtraction's cases and products, in an ordinary term both checkers recheck
- [x] Certified division with remainder and divisibility (`/std/Nat/div_mod`, `/std/Nat/Divides`), and `Int`'s order carried from `Nat` along the embedding

## Compilation

- [x] [Every stage but reduction costs the graph it is handed](design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md): a read visits a node once, a rebuild keeps the graph, a judgment is remembered while what it read stands, a print is bounded, and the kernel binds a `let`, so a chain of `let`s each naming the one before it twice compiles in its size
- [ ] [A declaration is a function of what it reads](roadmap/05-compilation/02-a-declaration-is-a-function-of-what-it-reads.md) — a proof the elaborator writes follows the order the lowering gives a unit's declarations, a declaration's vectors and totality are written after the items on one shared budget, the item graph is computed three times over names, and every compile re-seeds all its predecessors
- [ ] [A compilation is a graph of item tasks](roadmap/05-compilation/03-item-tasks.md) — nothing the compiler holds can cross a thread, so a compilation occupies one core
- [ ] [Size cliffs](roadmap/05-compilation/04-size-cliffs.md) — not refined yet; elaboration is not linear in `let` depth, the parser buys its depth with stack, and every binding gets a fresh local
- [ ] [What unbounded `Nat` and `Int` still cost at run time](roadmap/05-compilation/05-unbounded-nat-costs.md) — not refined yet; a field is a reference, a chain boxes between steps, and the fast path tests a tag per operand
- [ ] [Contification of a function with several return contexts](roadmap/05-compilation/06-multi-site-contification.md) — not refined yet; such a function stays a function, and nothing downstream contifies it
- [ ] [A binary reader for `curios-wasm`](roadmap/05-compilation/07-wasm-binary-reader.md) — not refined yet; the binary side is checked only by the engine's acceptance
- [x] [A module is a compilation unit, and the prelude is an environment](design/compilation/a-module-is-a-compilation-unit-and-the-prelude-is-an-environment.md): units folded over a dependency order, every edge declared in a manifest, and a package named `std` compiled as the standard library
- [x] The prelude built once per compiler build, archived and certified, and restored with no source fallback
- [x] Packages: manifests and discovery, exactly pinned dependencies in a content-addressed store, and `curios pin` deriving a row's hash from the delivery
- [x] [Cached verdicts](design/soundness/admission/cached-verdicts.md) and [reused payloads](design/soundness/admission/reused-payloads.md), and [a stored unit is a baseline for an item-level recompile](design/compilation/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md)
- [x] A unit carries no identity another compilation could mint, and nothing branches on a name's spelling
- [x] [Depth is bought with stack, not with hand-rolled frames](design/compilation/depth-is-bought-with-stack-not-with-hand-rolled-frames.md)
- [x] Compile cost: per-node memoization bounded by written binder nesting, a closed fold on the shared machine, a type-level concatenation that copies nothing, a string literal checked once per use, and the certifier measured by item and judgment
- [x] [WebAssembly-GC is the only target](design/compilation/webassembly-gc-is-the-only-target.md), serialized by `curios-wasm` with text round-tripped against the binary writer, and optimized closed-world by Binaryen
- [x] The erased IR: flat, verified arenas; erasure as transcription; behaviour-summary pruning, partial evaluation and monoid rebasing; one lowering into continuations
- [x] The continuation IR: a pre-closure CPS graph with delayed closure conversion, an interprocedural optimizer, SCC specialization, a dataflow substrate for unboxed scalars, return through several continuations, and structured control flow by SCC condensation
- [x] Value representation: [a variant collapses when nothing needs to distinguish it](design/compilation/a-variant-collapses-when-nothing-needs-to-distinguish-it.md), [a field is declared at the carrier its shape names](design/compilation/a-field-is-declared-at-the-carrier-its-shape-names.md), [a value costs when it is kept, not when it is named](design/compilation/a-value-costs-when-it-is-kept-not-when-it-is-named.md), and a closure carries its code as a table index
- [x] [A lowering names the elimination it performs](design/compilation/a-lowering-names-the-elimination-it-performs.md), and [a refusal is a panic the emitter renders](design/compilation/a-refusal-is-a-panic-the-emitter-renders.md)

## Runtime

- [ ] [Foreign calls past scalars and byte strings](roadmap/06-runtime/01-foreign-calls-past-scalars.md) — not refined yet; a `Handle`, a `List` and several results at once are each refused where a plugin's signature is read
- [x] Execution on a shared, GC-enabled Wasmtime engine, from precompiled `.cwasm` compiled across threads
- [x] A slim launcher embedded in the compiler, and self-contained executables carrying their foreign modules
- [x] [The heap is sized ahead of its churn](../curios-runtime/README.md#the-heap-is-sized-ahead-of-its-churn)
- [x] [A host operation has one contract, checked at both ends](design/runtime/a-host-operation-has-one-contract-checked-at-both-ends.md): each row typed in `curios-abi`, read by `/sys` as a `Result` or an `Option`, held to its row by the native adapter and by the guest, and conformed across the native, mock, plugin and browser hosts
- [x] Host capabilities: terminal with raw mode, files and the filesystem over `Path`, clock and randomness, process IO and subprocesses, TCP with TLS, and serial ports
- [x] Foreign functions: `foreign` declarations answered by a WebAssembly module a package names and pins, linked by `run` and `test`, and carried inside a `compile`d executable

## Tools

- [x] [A command compiles only as far as its answer needs, and a project keeps what it compiled](design/tools/a-command-compiles-only-as-far-as-its-answer-needs-and-a-project-keeps-what-it-compiled.md): `lint` and the `wonder` queries file what they compile where the disk holds what it was compiled from, whole or over a baseline, in the slot and with the bytes a build files there, and say so where the store cannot be written
- [ ] A question about a program checks its entry every time — an entry is no unit (`Fold::check`), so nothing a question compiles of it is filed and no payload a build filed answers one
- [ ] A build compiles a moved unit whole where a question compiles it over a baseline — `Verdicts`' own `Cache::baseline` answers none
- [ ] [Profiling in the budget's own units](roadmap/07-tools/02-profiling-in-budget-units.md) — not refined yet; a profile reports durations rather than the budget's machine-independent units, and counts no priced site
- [x] [A diagnostic spells what its reader can write](design/tools/a-diagnostic-spells-what-its-reader-can-write.md): spans across every stage, names as resolution reaches them, written goals reporting their scope and verified candidate fits
- [x] [An argument names one subject, and each command states what it accepts](design/tools/an-argument-names-one-subject-and-each-command-states-what-it-accepts.md): `run`, `compile`, `document`, `test`, `curate`, `pin`, `new`, `lint`, `format`, `wonder` and `profile`
- [x] `curios wonder`: diagnostics, tests, a declaration's fate in the optimizer and any pipeline rung, over the command line and a language server
- [x] Editor support: a tree-sitter grammar, and Zed and VS Code extensions over `wonder server`
- [x] [A printer states each fact once, where it is bound](design/tools/a-printer-states-each-fact-once-where-it-is-bound.md), with one layout engine and a canonical formatter verified by reparse
- [x] [A lint is an exact finding read off the compilation](design/tools/a-lint-is-an-exact-finding-read-off-the-compilation.md): four, always on
- [x] [A test is a declared description, and a proof is a `let`](design/tools/a-test-is-a-declared-description-and-a-proof-is-a-let.md)
- [x] [A library is documented for its consumers, from the compilation that builds it](design/tools/a-library-is-documented-for-its-consumers-from-the-compilation-that-builds-it.md)
- [x] Profiling through `curios-profile`: `--profile` writes a stream as it runs, `curios profile` reads it back, and `cargo xtask profile` builds and folds one
- [x] Distribution: tag-triggered releases for Linux and macOS, a checksum-verified installer, and a browser playground
- [x] The language reference, the command-line reference, and a benchmark bench recording what Curios costs against Rust, natively and on WebAssembly

## Standard library

- [ ] [HTTP messages as RFC 9110 and RFC 9112 frame them](roadmap/08-standard-library/01-http-framing.md) — HTTP neither reads nor writes a message as RFC 9110 and RFC 9112 frame it, and refuses a head's opaque octets where they are data
- [ ] [Text read as text, and numbers by each format's grammar](roadmap/08-standard-library/02-text-read-as-text.md) — `Json` accepts `01` and writes infinity as `null`, the text formats walk the bytes of input that began as text, and `Flt`'s readers cut text where a grammar belongs
- [x] `Map(K, V)` and `Set(K)`: keys held and handed back at their own type, each built and read under the `Key(K)` dictionary its type names
- [ ] [A certified sort and an `Ord`-keyed tree](roadmap/08-standard-library/03-certified-sort-and-ord-tree.md) — not refined yet; `sort` is pinned by properties rather than proved, and `Map` orders its keys by their encodings rather than by `Ord`
- [x] Foundations: `/std/Bool`'s `True`, `False` and `Holds`, `Eq` and `Ordering`, `Option` and `Result`, `State`, `Try`, `Io/Error` and `Path`
- [x] Collections: `List` and its helpers, `Vec` counting its list in its type, and `Map`, a canonical crit-bit trie over `Bytes` keys
- [x] Text: proof-carrying UTF-8 `Str` addressed by proved byte positions, certified `Char`, parser combinators, typed format strings, and decimal conversions that round-trip
- [x] Formats: `Json` over binary64, `Toml` 1.0.0, and `Html` as a tree
- [x] Applications: an HTTP client and server over TCP and `Async`, command-line interfaces, and terminal programs with widgets
- [x] [Explicit invariants](../curios-text/README.md#std-invents-no-value-where-a-proof-belongs-and-text-is-addressed-by-position): decoding and encoding carry their certificates, indices carry their bounds, and a host's facts arrive first-order
- [x] [A fallible operation returns `Try`, and `!` lifts along declared edges](design/standard-library/a-fallible-operation-returns-try-and-bang-lifts-along-declared-edges.md), with `Result` error first and its own monad
- [x] [Only a fiber waits](design/standard-library/only-a-fiber-waits.md): every peer-facing handle non-blocking, single-attempt writes and an explicit flush, write-once cells, bounded channels and level waiting in the guest heap
- [x] [A `Bits` encoding is injective by its extent](design/standard-library/a-bits-encoding-is-injective-by-its-extent.md): one byte per bit, so a run's extent is its length and no field beside the bytes states it
- [x] Structured concurrency in `/std/Async`: fibers and tasks, `race`, `select` and `join_all`, `sleep` and `timeout`, scoped resources, and deadlock detection
