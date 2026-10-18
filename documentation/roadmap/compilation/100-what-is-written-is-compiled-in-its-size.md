# What is written is compiled in its size

A program accepted at one size is refused, or never answers, at a larger one, where three documents state it should not be:

- [Every stage but reduction costs the graph it is handed](../../design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md): "a compilation ends — accepted, or refused for its budget — in time proportional to what was written plus what the budget counted".
- [Depth is bought with stack, not with hand-rolled frames](../../design/compilation/depth-is-bought-with-stack-not-with-hand-rolled-frames.md): "a term's acceptance should not depend on the host's stack".
- `curios/src/compile.rs`'s contract: "emitting a module that does not validate is the compiler's own broken contract rather than any program's fault".

## What is wrong

The figures were taken at `28bcb9aba` with `target/release/curios`, on standard input, by the protocol below. Depths, local counts and refusals are counted. Times are timed: CPU seconds, one reading each, on a machine other sessions held at load averages from 14 to 75. They show a slope and are no value to hold. Peak resident memory is beside each, as the load does not move it.

### Elaboration pays the scope a judgment stands in

A judgment reads its scope through walks of the scope, so what each judgment costs grows with every binder above it.

- **Spines.** An operator mints three metavariables. Each carries a spine with one entry per local in scope (`Frames::identity_snapshot`, rebuilt whenever the locals changed since the last mint), and every walk that meets it visits each entry: its hash, a solve that assumes the birth telescope again (`Convert::solve`), and the capture where a block or a lambda closes (`Term::let_block`).
- **Frame walks per step.** `reduce_within` asks on every step whether any frame holds a refinement (`has_scrutinee_refinements`, `any_refinements_registered`), and each asks every frame. The frame store walks its stack in eighteen places (`curios-elab/src/context/frames.rs`).
- **Held frames.** Parking a problem holds the stack, rebuilt in its depth after any write (`Frames::hold`). A birth's frames are compared whole (`stands_on`, `reads_the_refinements_of`).

| Shape | Readings | Growth |
| --- | --- | --- |
| `let x{i} = x{i-1} + 1;` in one body | 1 000 → 5.0 s, 1.1 GB; 2 000 → 18.7 s, 4.3 GB | n² |
| `let _ = /std/print("x")!;` in one `Io` block | 100 → 1.4 s, 140 MB; 200 → 8.5 s, 387 MB; 500 → no answer in 60 s, 1.37 GB | ~n³ |
| `f(x0, …, x{n-1}) = x0 + … + x{n-1}` | 500 → 8.0 s, 429 MB; 1 000 → 69 s, 1.56 GB | ~n³ |
| `(x0) => … => x0`, n deep | 200 → 0.17 s, 65 MB; 800 → 1.7 s, 216 MB | n^1.6 |

The same chain written with `Nat/add(x{i-1}, 1)` mints nothing and takes 0.19 s at 1 000, as does `let x{i} = x{i-1}` (0.14 s). An `Io` block is the program nearly every author writes: each statement binds its continuation's parameter, so the scope grows a binder, a frame and a spine entry a line.

### A rebuild is done again per recorded term, and a level is zonked along its chain

- **The settle zonks afresh.** `Context::settle_checked` (`curios-elab/src/context.rs`) zonks each term `record_checked` collected through its own `zonk(self, term)`, each with an empty memo. A deep expression records a term per level, and the k-th holds the k beneath it. `1 + 1 + … + 1` at 1 000 and 2 000 operators takes 1.1 s at 415 MB and 3.5 s at 1.49 GB, all four samples in this walk.
- **The totality obligations zonk afresh.** `zonked` (`curios-elab/src/totality.rs`) zonks each distinct recorded type with an empty memo, so nested types are zonked once for each type they are nested in.
- **A spine is zonked for nothing.** A solved metavariable's zonk zonks every entry of its spine (`zonk_level`, `curios-elab/src/zonk.rs`), even where the solution names no birth binder and the spine is never read.
- **A universe level is zonked along its chain.** `UniverseSolver::zonk` (`curios-elab/src/universe_solver.rs`) follows a solved level metavariable to its solution with no memo and no path compression. Nested list literals, n deep, each `List` minting a level solved to the next, take 0.73 s at 200, 4.5 s at 400 and 63.6 s at 800, at 91, 198 and 621 MB, with three samples in four in this walk.

### A declaration is read whole for one fact

A type former's sort is read off its whole declaration instantiated at the occurrence's universes (`Context::instantiate_induct_decl_at`, every constructor's telescope with it). The totality obligations ask it of each recorded type (`proof_position` → `Sort::of`), and a constructor's application asks it of the one constructor it applies (`elaborate/aggregate.rs`, `into_ersd/aggregate.rs`). An `induct` of n constructors matched once takes 0.77 s at 500 and 9.6 s at 2 000, at 92 and 576 MB, with three samples in three in this walk. [A compile costs what it reads](../../design/compilation/a-compile-costs-what-it-reads.md) reads a declaration through its facets, and this read takes all of them for one.

### The parser and erasure take depth from the thread's stack

Nothing under `curios-text/src/parse/` or `curios-parse` grows the stack, so a parse runs on whatever thread called it, and a level of call nesting costs the parser twelve native frames and 6.7 KB. On the 8 MiB main thread:

- nested calls overflow between 1 125 and 1 250 levels;
- right-nested `+` overflows between 1 000 and 1 500;
- nested parentheses overflow between 1 750 and 2 000 on release, and between 2 000 and 3 000 on debug, both at `opt-level = 3`.

With the main stack raised to 256 MiB, erasure overflows next, at about 8 250 levels. `Lowering::walk` → `erase_apply` → `slots` recurses with no `recurse`, about 4 KB a level, inside the one 32 MiB segment `erase_unit` takes through `grown`, and dies as a bare `SIGSEGV`. The surface tree has no iterative `Drop`, where `curios-core`'s `Term` has one (`impl Drop for Node`). Lowering's own walk (`Lowerer::term` → `subterm`) brackets nothing either, inside its one segment, so a curried arrow ten thousand parameters long overflows it.

### The emitted module leaves the engine's envelope

The JavaScript embedding states [implementation limits](https://webassembly.github.io/spec/js-api/#limits), and Wasmtime's validator states most of the same (`wasmparser`'s `limits.rs`):

- 50 000 locals and 1 000 parameters and results to a function;
- 10 000 fields to a struct;
- 7 654 321 bytes to a function body;
- 100 000 data segments;
- in the browsers alone, 10 000 operands to `array.new_fixed`.

What the emitter does at those limits:

- **A local per value.** `curios-emit` gives every value a fresh local (`ExprEmitter::declare_local`), five to a `Nat` `+`, codegen's own scratch among them. `curios/src/compile.rs` validates before Binaryen, which is where locals would merge. 10 100 additions in one function panic `` `into_wasm` emitted a module that does not validate: too many locals ``, and the browser path (`curios-js`) runs no Binaryen at all.
- **A struct per structure, field for field.** A structure of 10 001 fields panics `` `into_wasm` emitted a module that does not validate: struct fields size is out of bounds ``.
- **Not reached today.** 500 string literals emit 17 data segments, a 500-element list literal emits `array.new_fixed` of at most 10 operands, and the emitted subtype depth is 1. A function of 1 001 parameters called once compiles. An escaping function of more than 1 000 parameters, and a body past 7 654 321 bytes, are untried.

### Binaryen pays superlinearly for a long function

A Binaryen `version_130` replay of the module `curios` hands `BinaryenModuleReadWithFeatures`, at `curios-binaryen`'s settings:

- **`precompute-propagate`.** After `simplify-locals` has sunk a dependent chain of n `+` into one expression, it takes 10.0 s at 150 and 62.9 s at 300, while either pass alone takes milliseconds. The whole default pipeline with that one pass skipped (`BinaryenAddPassToSkip`) takes 2.25 s and 4.74 s, on a module whose baseline is 0.34 s.
- **`simplify-locals-nostructure`.** On a 2 000-element list built from a run-time value, it takes 6.8 s of a 10 s pipeline the skip above does not shorten.
- **A match's arms are not Binaryen's.** A match of 2 000 literal arms spends 1.3 s of a 6.7 s compile in Binaryen; the counted slopes below place the rest in the Cont lowering and its optimizer.

[Precompute-propagate's introduction](https://github.com/WebAssembly/binaryen/pull/1179) already kept it to higher levels for its compile time. The [LLVM WebAssembly backend](https://android.googlesource.com/platform/external/opencl/llvm-project/+/cd46354dbd10) counts excess locals among what makes it "easy to hit engine limitations of 50K locals" and code "slower to compile to native code".

### The counted slopes reach every stage

`growth_slopes`' reading over the compiler of `65741c434`, printed in its documentation, holds five growths linear in every stage: definitions, strings, a list literal, parentheses and a string literal. The others reach a stage that grows as its square or steeper. Besides the elaboration, erasure and Binaryen costs above:

- **Lowering**: a structure's fields, a telescope's parameters, and nested matches, `let`s and lambdas (slopes 3.4 to 4.0 in `lower`).
- **Erasure**: nested list literals (3.8 in `erase`), besides the constructors above.
- **The Cont lowering**: a list built at run time, a match's arms, nested calls and a chain of operators either way (3.7 to 4.0 in `cont`).
- **The Cont optimizer**: a match's arms, a telescope's parameters, a tuple, a structure's fields, an `Io` block and nested lambdas (3.3 to 4.6 in `cont-optm`).
- **Cranelift**: a long function's bytes (3.1 to 4.1 in `cranelift`).

## Goal

Doubling a program along any axis an author can grow it at most doubles what every stage counts, and changes no verdict: length, width, nesting depth, the scope a term stands in, the size of a literal and the size of a function. Depth is bounded by memory alone, and the emitted module is inside the engine's envelope at every size. What holds it is counted, as the rule it restores is.

## Settled

- **The instrument is a growth suite, beside the towers.**
  - *Plain fixtures* (`curios`'s `tests::growth`) compile each shape at a size past today's cliff, as `tests::towers` compiles each tower at sixty lines.
  - *Counted slopes* (`growth_slopes`, among the instrumented tests) compile each shape of the protocol's grid at n, 2n and 4n through the whole pipeline, parse to Cranelift. Per stage they count instructions, node looks (`take_looks`), and allocations and bytes (`curios-profile`'s counting allocator). A slope is the second difference over the first, so what every compile pays whatever its size cancels: two is linear and four quadratic, and a held growth's every slope stays under three.
  - This is Swift's [scale-test](https://forums.swift.org/t/ongoing-work-related-to-compilation-performance/7085), which fits the growth of compiler counters across sizes, and GHC's [allocation-counted performance tests](https://gitlab.haskell.org/ghc/ghc/issues/12758), read through counters Curios keeps.
- **Instructions are a fourth instrument.** `curios-profile` counts the user-space instructions of the thread that opens its counter and of the threads it starts afterwards (`perf_event_open`), reading none where the kernel refuses. Looks and bytes cannot see a walk that allocates nothing, or Binaryen, which is C++. The system call is the one `unsafe` it adds, behind one safe type, with `libc` named in `curios-profile`'s manifest alone.
- **A judgment does not pay its scope.**
  - The frames are a persistent scope: a pushed frame refers to the frames beneath it and carries what they hold, so the questions a step asks are answered in constant time or a persistent map's lookup, and a held scope is a pointer.
  - A metavariable's spine is the persistent vector of its birth scope's locals, minted and passed over without being walked, and spelled out only where an unsolved metavariable is read at another scope.
  - A solved metavariable is substituted before a binder closes over the term holding it.
  - A walk passes over what it cannot change: a capture, a subterm whose locals are all older than the binders it closes, which the newest local a node keeps tells, as `reach` tells a loose index; a materialization, a subterm that holds no metavariable; and a node's facts are read off its spine's, never off its entries.
  - This is [Lean 4's](https://raw.githubusercontent.com/leanprover/lean4/master/src/Lean/MetavarContext.lean) design: a metavariable holds "the local context containing the free variables that the mvar is permitted to depend upon", and the application to them is built on demand (`mkMVarApp`), over a `LocalContext` of a `PersistentHashMap` and a `PersistentArray`. Rocq paid the other way: identity evar instances, "an explicit substitution that has the size of the context", were "the source of the next large wave of performance issues" ([#12526](https://github.com/rocq-prover/rocq/issues/12526)) until [#16442](https://github.com/rocq-prover/rocq/pull/16442) made "bunches of identity instances … a O(1) constructor".
- **A rebuild is done once per pass.**
  - One `Zonk` serves a settle, and one node memo serves both totality obligations, as the memo's rule already reads ("a node is zonked once per pass").
  - A spine is zonked only where the solution reads a birth binder.
  - The universe zonk keeps a solved level's zonked solution once it holds no unsolved metavariable, for as long as the journal keeps the solutions it rests on.
- **A fact is read alone.** An instantiated declaration is read by facet — its sort, its arity, one constructor — and only the facet asked is instantiated.
- **Depth is bought with stack in the parser and in erasure.** `recurse` stands at each memoized nonterminal of `curios-text`'s grammar, the points every recursion of the grammar re-enters, with `grown` where `run_parser` begins and `recurse` at erasure's `Lowering::walk`. [rustc](https://doc.rust-lang.org/stable/nightly-rustc/src/rustc_data_structures/stack.rs.html) places the same bracket at its recursion points, its parser among them ([#128422](https://github.com/rust-lang/rust/issues/128422)).
- **The emitter allocates locals by liveness.** A value whose live range has ended hands its local to the next value of the same type, every local alike, as [LLVM's `WebAssemblyRegColoring`](https://llvm.org/docs/doxygen/WebAssemblyRegColoring_8cpp_source.html) merges registers whose lifetimes do not interfere. A straight-line function's locals are then bounded by what is live, on both paths.
- **A wide shape spills past the engine's limits.**
  - A struct type past 10 000 fields keeps the first 9 999 and nests the rest in a trailing struct.
  - A signature past 1 000 parameters or results passes the overflow as a trailing struct.
  - Acceptance stays independent of size.
- **`curios-binaryen` holds Binaryen to the rule.** It is Binaryen's authority ([One crate is the authority for one external concern](../../design/one-crate-is-the-authority-for-one-external-concern.md)): a pass the growth suite refuses is skipped by name with its reason stated, and reported upstream with its reduced module. Constant propagation is the continuation optimizer's in any case ([`curios-binaryen`'s README](../../../curios-binaryen/README.md)).

## Stages

Each stage begins by attributing the growths its slopes name, by a debugger's samples of the stage, and ends with those rows held in `growth_slopes`.

1. **The instrument.** The instruction count in `curios-profile`; `tests::growth`'s programs; `growth_slopes` over the grid, holding the seven growths linear today. Check: `growth_slopes` by hand; `curios-profile`'s tests.
2. **Depth.** The parser's, lowering's and erasure's brackets, the surface tree's drop if a fixture needs it, and each further overflow the depth fixtures meet. Check: parentheses and an arrow at 10 000 levels on a test thread; nested calls join them with stage 7, whose lexical scopes make them quadratic in their depth (59 s at 10 000, every sample in `curios-cont`'s `lexical_scopes`).
3. **One rebuild per pass.** The settle's `Zonk`, the totality obligations' memo, the spine read where it is used, solved metavariables substituted before a close, and the universe zonk's memo. Check: a left-leaning and a right-nested `+` of 10 000 operators and 1 000 nested lists elaborate in `tests::growth`; their elaboration rows held, the stages after it being stage 7's and stage 10's.
4. **A fact read alone.** `InductDecl::at`'s facets through every reader of one, in both checkers: the kernel's handle instantiates its arity and its sort, and a constructor where one is read. Besides, the facts a reader reads beside a declaration: an arm retypes the locals past the base frame alone, entailment reads its hole's binders there too, and erasure copies the one constructor's row it reads, builds a family's row in one pass and finds a match's arms by their tags once. Check: 4 000 constructors compile in near-linear time; the row's memory is stage 6's.
5. **The scope by reference.** The persistent frames first, then the spine, prototyped on a scratch copy and measured before it lands. Besides, each walk that paid a scope or a body it could not change: a reduct kept by the names it read absent, a solution judged in the scope it was born in, a node's facts read off its spine, a capture passing over what holds only older locals, a materialization over what holds no metavariable, the proof-binder marking entering a block's binders once, and a block closing its bindings over one lookup of its binders. Check: a 6 000-binding `let` chain, a 2 000-statement `Io` block, 1 000 parameters summed and 2 000 nested lambdas in `tests::growth`; their elaboration rows held, the nested lambdas' erasure being stage 6's.
6. **Lowering and erasure.** A telescope, a structure's fields and nested matches, `let`s and lambdas lowered in their size, and nested list literals erased in theirs. Landed: lowering resolves a name through an index of its scope and binds a telescope's prefixes a binder at a time, a `let` in tail position continues its block, a telescope and a block close over one lookup of their binders, a curried type's erasability is answered once for every tail of it, and a term's levels are zonked through the solver's memo. Remaining:
   - An item's erasure imports a family's whole row, every constructor at its width, so a unit of n constructor definitions holds n rows of n (2.1 GB at 4 000). An importing fragment holding only the constructors it reads needs a constructor's position stated where `curios-ersd` now reads it off its family's list, which linking maps by.
   - `InductDecl::constructor`, `constructor_index` and `declares` scan the cases for a tag, where an index answers in constant time.
   - A nesting of n list literals types each level at a term the size of the nesting below it, and the closed machine, the kernel's sorts, totality's proof positions and the solver each copy one per level: at 800 levels they allocate 614, 286, 192 and 193 MB, each fourfold per doubling.

   Check: their rows held.
7. **The Cont lowering and its optimizer.** A function's lexical scope shared with the one it is nested in, where `lexical_scopes` (`curios-cont/src/verify.rs`) copies it, so every function holds the set of every function above it; a match's arms, a list built at run time, a tuple, a structure's fields, a telescope and a chain lowered and optimized in their size. Check: their rows held; nested calls at 10 000 levels join the depth fixture.
8. **Locals by liveness.** The coloring pass over each emitted function. Check: 10 100 `+` in one function compiles and runs; `curios-std`'s corpus.
9. **Wide shapes.** The envelope fixtures, then the spill. Check: each crossing runs.
10. **Binaryen and Cranelift.** The long-function shapes retaken after stage 8; each Binaryen pass still refused skipped, stated in `curios-binaryen`'s README, and its report prepared; what Cranelift pays for a long function attributed, and taken where `curios-cranelift` configures it or the emitted function shapes it. Check: every row held; `wasm-optm` sizes over `programs/` before and after.

## Protocol

One program a shape and size, on standard input. Each body reads the argument count, `let a = /std/List/len(/std/Process/args()!)`, so nothing is a closed term, and prints one `Nat`. The front end runs as `wonder diagnostics -` and the whole as `run -`, each under a memory cap and a time cap. The grid, at two sizes a factor of two or four apart:

- **Length**: a `let` chain over `+`, the same over `Nat/add`, n top-level definitions, n top-level functions each calling the last, n string definitions, and an `Io` block of n statements.
- **Width**: a list literal, a list built from `a`, a match of n literal arms, n parameters, a tuple, a structure of n fields, and an `induct` of n constructors.
- **Depth**: nested calls, parentheses, right-nested `+`, matches, lambdas, singleton lists, parenthesized `let`s and a curried arrow.
- **Scope**: n parameters summed.
- **Size**: one string literal of n characters.

A Binaryen reading replays the bytes `curios` hands `BinaryenModuleReadWithFeatures`, dumped by a debugger at that call, through `BinaryenModuleOptimize` or `BinaryenModuleRunPasses`, linked against the cached `libbinaryen.a`, with `BinaryenAddPassToSkip` for a skip.

## Rejected

- **A spine over the bound variables alone**, `let`-defined locals left out as [elaboration-zoo](https://github.com/AndrasKovacs/elaboration-zoo)'s pruning mask leaves them: an `Io` block's binders are lambdas', so its scope grows all the same.
- **A spine linear in the scope, as [smalltt](https://github.com/AndrasKovacs/smalltt) keeps it**, "usually linear in the size of the local scope": that is the cliff.
- **A nesting limit, as [clang's `-fbracket-depth`](https://lists.llvm.org/pipermail/llvm-bugs/2021-April/091875.html)**: acceptance would follow the host's stack, which the depth decision rejects.
- **Validating after Binaryen, for its merged locals**: Binaryen aborts on a module it cannot read, and the browser path runs none.
- **A pass list of Curios's own in place of Binaryen's default**: the list would have to follow every version's, where a skip names only what the suite refuses.
- **Timing the suite**: the readings above are a machine's load as much as the compiler's.

## Retirement

- [Every stage but reduction costs the graph it is handed](../../design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md) gains that a judgment does not pay its scope, and its counted fixtures gain the growth suite.
- [Depth is bought with stack, not with hand-rolled frames](../../design/compilation/depth-is-bought-with-stack-not-with-hand-rolled-frames.md) names the parser and erasure.
- `curios-profile`'s README states four instruments.
- `curios-emit`'s README states local allocation and the spill, and `curios-binaryen`'s states its skips.
- What cites this specification is updated, and it is deleted.
