# Identity is declared once, and a unit is compiled once

This is the Curios way to do packages and projects: how a program depends on code it does not contain, and how a unit compiled by one compilation is consumed by another so that depending on N packages does not cost N elaborations per build. The two halves are one document because the coupling runs both ways — packages ship source, so the boundary is unusable at scale without the cache, and the cache's location and keys are facts about what a project is.

## The four laws

Every decision below derives from one of these, and cites it.

1. **Declaration decides; location never does.** Modules exist because a header declares `mod`, artifacts because the manifest declares them, members because the umbrella enumerates them. A file nothing names is inert, wherever it sits. *One exception, taken deliberately and argued where it is stated: a package has a library exactly when `lib.crs` sits beside its manifest.*
2. **Identity is declared exactly once, by its owner.** A package names itself, and the filesystem spells structure, never names. Nothing positional — no identity meaningful only in the compilation that assigned it — is ever stored.
3. **Membership organizes; dependency compiles.** The membership tree places stores and scopes tooling; only declared dependencies order compilation. Neither implies the other.
4. **A refusal fires early and names both parties.** Conflicts, collisions, cycles, and missing obligations are diagnosed before elaboration, never surfaced as an unbound name or a conversion failure holding no span.

## What is already in place

The compiler no longer knows structurally that there is *a prelude* and *a program*. A compilation is a set of units folded over a dependency order; this specification builds on that vocabulary rather than restating it.

| Thing | What it is |
| --- | --- |
| `curios_unit::Unit` | One compiled unit: its resolution state, its elaborated `Module`, its erased arena, and its binder floor |
| `curios_unit::Scope` | Every predecessor, borrowed in dependency order — never merged |
| `curios_base::Mount` | A claimed prefix and its `RootKind`; lookup is longest-match, and mount sets are pairwise disjoint |
| `curios_text::UnitSource` | The seam a unit arrives through: a resolver, plus the entrypoint the one unit that has one carries. `curios_text::RootSource` is that resolver — one base per mount, a base being a directory or a tree already in memory |
| `curios_pipeline::compile_units` | The fold — each source compiled against the base and everything before it, judged by the kernel between elaboration and erasure |
| `curios_core::validate_stored_identities` | The refusal at the one seam a unit is written today |
| `--unit <DIR>` | Repeatable, argument order *is* dependency order; survives beneath everything as the already-resolved form a manifest entry becomes — a package directory, whose own manifest declares the prefix it mounts at |

Three properties are enforced rather than assumed: the orphan rule fires between two ordinary units and not between two modules of one unit; two units claiming one prefix is refused at mount; every unit's `foreign` rows reach the returned store, unioned, disjoint by mount. Two further facts are structural and this design leans on them. An executable is an `Entrypoint` — a module *plus a tail expression* — while a library is a `Module` with neither, so the artifact distinction below is enforced by type, not convention. And `curios_text::RootSource` resolves every header's children out of that header's stem directory, which is the layout rule this document states; a package's library header is the one anchor it does not derive, taking its directory as an argument instead.

## The manifest

A manifest is `curios.toml`: declarative TOML, no code escape. Lake ended up making declarative TOML the default with its Lean DSL as a rarely-needed escape, and Swift's code-as-manifest is the documented failure mode — unanalyzable without execution. If a computed configuration is ever genuinely needed, that is a new decision, not a latent capability.

Every manifest is in exactly one of two modes, and the modes are mutually exclusive — a manifest that declares both refuses to parse:

- **A package** — a namespace for definitions: `name`, `[dependencies]`, `[[executables]]`, optional `default`.
- **An umbrella** — a namespace for packages: nameless, `members`, `[catalog]`.

A manifest is optional. A bare `.crs` file compiles with no manifest, no dependencies, and no project — exactly as today. `curios.lock` is reserved as a name even though a manifest of exact pins already is a lockfile.

There is no privilege field in either mode and there never will be one: a mounted package is `Ordinary` structurally — the loader hands `RootKind::Ordinary` to `insert_root` and the manifest parser has no path to that argument — so no package can exempt itself from the orphan rule in the file its own author writes. **Reinstate only if** a package must ever reference an internal root, and split reach from exemption before doing it.

The manifest, the governance walk, the resolver, and the store are one subsystem with one owner: **`curios-package`**, a new crate beside `curios-pipeline` and below `curios`, with the CLI's subcommands as thin wrappers over it. The pipeline stays pure and never learns it exists; `curios-web` never touches it. It is also the workspace's only TOML dependency — the pattern `curios-archive` sets for rkyv — because `/std`'s TOML codec is a guest library and cannot serve the driver.

### A package names itself

> **A package declares its own canonical name, the name is its mount prefix, and every consumer refers to it by that name.**

If a consumer chose the name, a diamond dependency would compile twice under two prefixes and its types would become two nominally distinct families spelled the same — the interoperation failure that declining version coexistence exists to prevent. Package-chosen naming is what makes a diamond share instead of duplicate (law 2).

Names are **multi-segment from day one**: `name = "myorg/json"` mounts `/myorg/json`. Each segment is a legal Curios identifier, so no dashes, Unicode alphanumerics admitted, and a segment spelling a keyword (`let`, `mod`, `struct`, …) is refused at manifest parse — exactly the segments a path refuses, read from the one list `curios-base` states. Single-segment names stay legal for local work. Every first-generation flat registry — npm, PyPI, crates.io — is a documented squatting and typosquatting regret, and every second-generation one made namespacing mandatory (JSR scopes, Packagist vendors, Go domains); the separator is reserved before anything exists because retrofitting one costs an ecosystem migration. The name is self-declared, never conferred: an umbrella contributes nothing to any member's name, so reorganizing a tree renames nothing (law 2).

**A name is an atom: its segments are spelling, not structure.** `myorg` in `myorg/json` denotes nothing on its own, exactly as the scope in a JSR name denotes nothing importable. Reference resolution finds the owning mount by longest match over a reference's leading segments — `Mount::owning`'s algorithm, promoted from ownership queries to name resolution — and resolves the remainder inside that unit; a single-segment mount resolves exactly as it does today, so the atom rule subsumes the current mechanism rather than replacing it. What it sharpens is disjointness: with prefixes of different lengths, pairwise distinctness is no longer enough, because an entry's `mod myorg` beside a mount at `/myorg/json` makes `myorg/json/parse` ambiguous. **No claimed prefix may lie within another claimed subtree, and the entry's top-level module names claim their stems for this purpose** — a refusal naming both claimants, the same shape as the mount collision (law 4).

### The library and the executables

A package has a library when `lib.crs` sits beside its manifest, and none when it does not. There is no `library` key and no `lib` path: a library is not an artifact a package opts into, it is the package's own body, and a package of nothing but programs has none to declare — writing a vestigial empty file to say so would be ceremony, not a declaration.

**This is the one place location decides, and law 1 has an exception here on purpose.** What makes it safe is that the failure is loud: a library that stops existing takes its whole mount with it, so every reference in every dependent stops resolving at once, at the first compile. The reasoning does not extend to executables — a default executable that stops existing fails by silently not being there, with nothing to notice — which is why those stay enumerated and why an `exe.crs` convention is refused below. A header that fails to *parse* is still a refusal; only its absence is an answer.

Executables are enumerated, never discovered:

```toml
name = "myorg/json"

[[executables]]
name = "serve"                 # runs serve.crs beside the manifest

[[executables]]
name = "bench"
path = "tools/bench.crs"       # explicit path overrides the default
```

An executable's `name` is a single legal identifier; the default path is `<name>.crs` in the package root and `path` overrides it. Deriving a path from a declared name is not disk discovery — declaration still decides existence — and disk discovery is the refused thing: Cargo's target auto-discovery accumulated enough misfires to grow `autobins`/`autolib` off-switches, and explicit targets are its escape hatch from convention. Here explicit is the only mode. `default = "<name>"` selects which executable a bare `curios run` means when there is more than one. An executable compiles against the package's full scope — its library and its dependencies — as the entry, owning the empty prefix.

**Rejected: a target abstraction.** The library is not one target among several — it *is* the package, the thing the name names and the mount serves; executables are entry roots compiled onto it, outside its namespace. Swift PM's products/targets split exists so one package can vend several libraries, which one-prefix-per-manifest deliberately rules out.

## The layout: the tree reads as the namespace

The package directory is the object; its on-disk name is free; identity lives only in the manifest (law 2).

```text
json/                ← directory name is free; identity is `name = "myorg/json"`
  curios.toml
  lib.crs            ← library header: /myorg/json's own definitions, plus `mod parse`, `mod render`
  parse.crs          ← /myorg/json/parse
  parse/
    lexer.crs        ← /myorg/json/parse/lexer
  render.crs         ← /myorg/json/render
  serve.crs          ← executable "serve": an entry, not a module of the package
  serve/
    cli.crs          ← serve's private module
```

**The resolution rule, stated once:** `mod x` declared in a namespace's header resolves to `x.crs` in that namespace's directory; every header's namespace directory is its stem directory — except the package's library header, whose namespace directory is the manifest's. The exception is forced by law 2: directory names are semantics-free, so no stem could mark the package root; the manifest names that namespace, so the manifest anchors it. A file's stem never enters any qualified name — `lib` appears in no path a consumer can spell.

Consequences, owned here:

- **The package root has one stem space.** `lib`, executable names, and module names enumerated by `lib.crs` all claim stems in the manifest's directory; a double claim — an executable named `parse` beside `mod parse` — is a refusal naming both claimants, the same shape as the mount collision (law 4).
- **An unenumerated `.crs` file is inert.** Not compiled, not a member, wherever it sits — the scratch-file freedom, preserved by construction. `curate` reports it ("`parse2.crs` sits in the package and nothing names it") rather than the compiler guessing. Python's src-layout doctrine exists to approximate this property by directory convention; enumeration delivers it structurally.
- **Bare files stem-nest too.** A bare `scratch.crs` handed to `run` is its own header: `mod util` reads `scratch/util.crs`. This is a breaking change to the entry's current first-level sibling rule, taken deliberately: it closes the recorded entry-versus-mounted resolution defect, makes one rule govern every file in the language, and never lets declaring a file change what its `mod`s mean. Migration is mechanical file moves, applied to the corpus in M1.
- **Disk mirrors the namespace below the root.** `/myorg/json/parse/lexer` lives at `parse/lexer.crs` inside the package directory — a qualified name's tail and its path are the same string; only the name's head maps to the manifest instead of to a spelled directory.

**Rejected: a root file named after the package** (`json/json.crs`). It makes the filesystem restate the declaration — the name spelled in the manifest, the root file, and the stem directory, three spellings to keep synchronized — and produces Python's `foo/foo/` stutter. **Rejected: per-directory index files** (`mod.rs`, `__init__.py`): the repository's own Rust conventions refused the pattern, and the fixed header here is one thin file per package, not one per directory.

## A dependency names a resolver and an exact pin

Every dependency row is a table — there is no string shorthand, because revisions are opaque strings and `json = "umbrella"` colliding with a revision literally spelled `umbrella` is the kind of ambiguity a grammar should make unrepresentable. Each row carries a `source` naming its resolver, plus the fields that resolver requires:

```toml
[dependencies]
json       = { source = "member" }                                              # a live member of my umbrella
toml_parse = { source = "catalog" }                                             # the umbrella's [catalog] row
http       = { source = "git", url = "https://…", rev = "abc123", hash = "…" }  # pinned directly
tools      = { source = "path", path = "../tools" }                             # live sibling, umbrella-less
```

- **`rev` is the fetch instruction** — the thing a remote can be asked for. It is opaque to the compiler: compared for equality, never interpreted, so the compiler needs no notion of a registry, a version scheme, or a VCS.
- **`hash` is the acceptance criterion and the store key** — this toolchain's own hash over the materialized source tree, transport-independent. It is what makes a pin exact when tags move, what "same revision" actually rests on when upstream force-pushes, and the key the shared store files everything under, uniformly across source kinds. The scheme is named here because a hash outlives any implementation: `c1:` prefixes SHA-256 over the delivered tree's regular files, sorted by relative path, each contributing its path and its contents — permissions and timestamps do not exist for it, and a symlink in a delivered tree is refused. A future scheme is `c2:`, and the prefix is what lets both verify during a transition. This is Go's `go.mod`-version/`go.sum`-hash split with its `h1:` dirhash, and Zig's `url`+`hash` pair — the ecosystems that thought hardest about registry-less integrity landed on both columns and a versioned scheme. Nobody hand-writes a hash: `curate` computes it on first materialization and the wrong-or-missing hash's refusal states the correct one.
- Fetchable sources require both `rev` and `hash`; `member`, `catalog`, and `path` forbid both, because live code has no pin. Making the acceptance of a full commit SHA stand in for `hash` would require interpreting `rev` (refused: opacity), pin integrity to SHA-1, and fork the store's keying per source kind.
- **`path` exists for umbrella-less siblings** — the shape Elixir's community calls a poncho project — with the recorded consequence that a path cannot travel if publishing ever exists, which is Cargo's position too.

Exact pins mean no requirement language and no solver; a manifest of exact pins is its own lockfile. The order the fold consumes is a topological sort over declared dependencies; a cycle is refused. A conflict — two units in one compilation graph pinning different `rev` or `hash` for one canonical name — is refused naming both dependents and both pins, before any of the three elaborates (law 4). The named successor for selection is Go's minimal version selection, which needs only a total order on revisions and *produces* the pins this design already consumes — an additive layer, not a redesign.

A source is a resolver, never an assumed filesystem: `curios-web` compiles with no filesystem at all and supplies every module body inline, and it keeps compiling. That constraint is what keeps fetching genuinely separable — a resolver turns an identifier into a module tree, and that is the entire contract.

## The umbrella

An umbrella is a nameless manifest that enumerates packages: those it contains, and those it refers to.

```toml
members = ["json", "http_client", "tools/cli"]   # packages inside the tree: live, unpinned

[catalog]                                        # shared rows members reference: pinned, hashed
toml_parse = { source = "git", url = "https://…", rev = "abc123", hash = "…" }
```

- **`members` enumerates by path**, and the paths may point deep (`"tools/cli"`), which delivers deep directory organization with flat manifest structure. **Umbrellas do not nest.** Every mature toolchain converged on flat workspaces — Cargo forbids nesting, uv's nested members misbehave, `go.work` is a flat list — and the one ecosystem that shipped deep shared-everything umbrellas watched its community retreat to plain sibling projects. Nothing archived depends on flatness, so lifting it later is additive; refusing it now is what keeps governance a single-step question.
- **The umbrella contributes nothing to any name** (law 2). Its tree is the membership graph, not the dependency graph (law 3): enumerating a member creates no dependency edge, and members depend on each other only by declaring it.
- **A member's canonical name is always answered by the live member.** There is no override or patch table; `source = "member"` in a dependent's row resolves through the governing umbrella to the enumerated package, live, unpinned. `source = "catalog"` resolves to the umbrella's `[catalog]` row. Each marker names exactly the umbrella-side list that answers it — `members` answers `"member"`, `[catalog]` answers `"catalog"` — and each mismatch is its own refusal naming both sides: a `"member"` row whose name no governing umbrella enumerates, a `"catalog"` row absent from the catalog, a `"catalog"` row whose name *is* a member, and a direct row — fetchable or `path` — pinning a name the governing umbrella enumerates, because the live member is the only answer for that name inside its tree. The last two catch a promotion that half-happened, in either direction. The stated cost of these exact ties: promoting a catalog entry to a live member edits each consuming member's marker, and the mismatch refusal is what makes the edit impossible to forget.
- **A catalog row does not fetch anything by itself.** The real dependency graph is the union of what members' `[dependencies]` reference; `curate` materializes that, and a catalog entry no member references is a reconcile report, not a download. Activation lives in the package that names it (law 1). Catalog rows name fetchable sources or `path`, never `member` or `catalog`.
- **Pin agreement is reported, not forced.** Members referencing the catalog cannot drift by construction. Members pinning the same name directly can, and that drift is a `curate` report; the hard refusal fires only where two disagreeing graphs meet in one compilation. Cargo's workspace-wide forced agreement is also what makes its staged migrations painful; here the catalog is the carrot and the meet-point refusal the backstop.

**Rejected: auto-propagating umbrella dependencies into members** — the umbrella injecting edges nobody declared is the implicit coupling that soured Elixir on umbrellas, and it would make member manifests lie when read alone. **Rejected: a dual-role manifest** that is both package and umbrella — it adds no expressive power (an umbrella with that package as first member is one directory away) and it is the overload behind Cargo's virtual-versus-root-manifest warts. **Rejected: umbrella-conferred name prefixes** — position-derived identity is the consumer-chosen-prefix duplication failure arriving one level up, and it would make reorganizing a monorepo a renaming event.

## The store

`.curios/` sits beside the governing root — the umbrella's manifest when the invocation is governed by one, the package's otherwise — and it is the only generated directory in the tree: member directories hold user files and nothing else. Above it sits the shared content-addressed cache, keyed by the same `hash` column the manifest carries, so two projects pinning one revision materialize and compile it once. This is the two-layer shape every modern toolchain converged on — pnpm's content-addressable store under per-project virtual stores, uv's cache under venvs, Go's module cache, Zig's global cache — and content-derived keys are what make the upper layer shareable at all; a path-keyed store could only ever have been local. The store holds three families, and each gets its own subtree rather than sharing one namespace:

```text
.curios/
  bin/    myorg/json/serve      what `curios compile` emits
  src/    c1/<digest>/          materialized source trees, keyed by their manifest `hash`
  unit/   <key>/                compiled units, keyed by the terms and the certifier B3 states
```

Separated because the alternative re-invites the collision the nesting below removes: a hash needs transforming to sit in a directory name at all, `c1:<digest>` most naturally becoming `c1/<digest>`, and a package legitimately named `c1` would then land on top of it. Collision-freedom by construction is not something to be half-way about.

**Where a built executable lands, decided:** `<governing root>/.curios/bin/<package name>/<executable name>`. Nested by the package's own name, which is the one identity in a compilation that cannot collide (law 2) — so two members of one umbrella may both declare `serve` and no refusal, no reconcile check and no test is needed for it. Nesting also keeps the path *stable*: under a flat layout the same package's binary would move when it joined an umbrella, since only the governing root changed. A name is already a path, so its segments nest exactly as the layout rule maps `/myorg/json/parse/lexer` onto `parse/lexer.crs`. The leaf is the executable's declared name rather than the package's, so a bare `compile` and a `compile <name>` never disagree about the filename for one program. All of it sits under `.curios/` because that directory is the only generated one in the tree and a built binary is generated; the cost is that a shipped program comes out of a directory people gitignore wholesale, which is `target/`'s shape and nobody minds. **The file form is the exception, and has to be:** `curios compile hello.crs` has no project, hence no governing root, no package name and no store, so it writes `./hello` — the same declared-artifact-versus-bare-file split as everywhere else.

`curios curate` is the store's tool, and it does two jobs:

- **Materialize** — resolve every referenced pin to bytes, verify each against its `hash`, place the results where the resolver finds them, and drop what nothing references any longer. With exact pins there is nothing to solve, only to realize.
- **Reconcile** — report a catalog entry no member references, and a `.crs` file nothing enumerates. Both are answerable from the manifests and the tree, which is all this command reads.

**The other two reports belong to a build, not to `curate`** (decided while implementing). A dependency declared but never named by any module, and a name resolved against no declaration, both need to know which prefixes a unit actually resolved against — which is not a new compiler seam, because `Term::free_vars` and `Mount::owning` already answer it over a finished `Module`. What they need is a *compilation*, and `curate` performs none. Making a fetch command pay for an elaboration to produce a hint inverts the cost; at the end of a `run` or a `compile` the answer is already there for free. Two rules come with it: the prelude's own mounts are excluded, because every compilation depends on them implicitly and no manifest names them; and the reading is taken from the **Core** module rather than the erased one, since a dependency used only in a type that gets erased is still used.

**`curate` is the toolchain's only network actor.** Opacity is a compiler property — the compiler compares `rev` and never interprets it — and the tool is exactly where interpretation belongs, because something must run a transport to turn a `rev` into bytes. This is a decision rather than a concession, because acceptance is by `hash`: any transport may deliver the bytes, an untrusted one included, and a delivery that fails the hash is refused regardless of who fetched it. A separate fetcher layered above `curate` would double the tooling for zero integrity gain. The compiler itself never fetches.

## The command line

`curios run` has three forms, and dispatch between them is lexical, never probed — an argument ending in `.crs` or containing a path separator is a file, anything else is an executable name, and the two spaces cannot overlap because executable names are identifiers:

- **`curios run`** — the governing package's default executable: the sole one, or the one `default` names; otherwise a refusal listing the candidates. No reserved names, no magic filenames.
- **`curios run <name>`** — the governing package's declared executable of that name.
- **`curios run <file>.crs`** — a bare file, standalone, *everywhere* — today's semantics, unconditionally. No manifest ever captures a file argument.

`curios compile` takes the same three forms, for the same reason: what a bare invocation means inside a package should not depend on which subcommand asked. Where its output lands is stated with the store.

**Discovery, decided:** only the no-argument and name forms walk — upward from the working directory to the nearest package manifest, which is the governing package; the walk then continues upward, and an umbrella governs the invocation only if it enumerates that package, directly or through a path. Enumeration bounds the walk (law 1), which is exactly the ambiguity Cargo's unconditional walk never resolved. A file argument triggers no walk at all, so project scope is reachable only through declared artifacts — the scratch-file hazard is not mitigated but unconstructible, at the price of one `[[executables]]` line when a scratch program wants the library. `--manifest` is the explicit override for scripting. The governed forms materialize what the manifest references before compiling, through the same machinery `curate` exposes standalone — the uv and Cargo convergence — and a refuse-instead flag for CI is additive, later.

`curios new` comes last, after the machinery that reads a manifest and acts on it exists. `--unit <DIR>` survives beneath all of this as the already-resolved form: a location holding a manifest, which is what a dependency becomes once its `rev` has been resolved to verified bytes. It names no prefix, because by then the package's own manifest is in the directory and the prefix is that manifest's to declare (law 2).

## The caching half

**It does not introduce verdict caching. It removes Cargo from underneath the one that already exists.** The prelude is a cached unit today — `verdicts_from` skips an item every one of whose declared names the environment already answers for — and what makes that sound is that the only crate handing the image out is one whose build script walked it with the kernel first. Cargo supplies four things there: storage (`OUT_DIR` and `include_bytes!`), the key (a schema constant and a source fingerprint), invalidation (the build script's dependency graph), and enforcement (a crate that does not compile). Three are engineering. The fourth is a change to what the compiler believes, and it is stated in B3 rather than inherited.

### The rule a stored unit is checked against

> **A unit may be stored only if it carries no positional identity** (law 2).

Measured against the stored prelude — 1091 items, 1107 definitions, release build:

| Identity | In a stored unit | Established by |
| --- | --- | --- |
| Term metavariable | none — zonking substitutes every solution and refuses an unsolved hole | `validate_stored_identities` |
| Universe metavariable | none — a level holding one is not closed over its declaration's parameters | `validate_bound_universes` |
| Free local binder | none — `derived_binder_floor` over items and registries is **0**, against a lowering watermark of 6748 | `validate_stored_identities` |
| Witness | none — every one is scoped to a mount its own unit claims | `validate_stored_identities`, since B1 |

All four classes are now refused rather than three. Of the monotonic counters, the ones that remain leave watermarks, which combine by maximum and cannot alias; the witness counter was the one minting dense identities that reached a stored unit, and it is gone. One further predecessor-dependence must be named beside them: a unit's lowering copies the *cumulative universe-seed table* from its last predecessor, so a stored unit's bytes depend on its predecessors — covered so long as B3's key covers the closure, and stated here so nobody discovers it as a surprise. The precondition that was load-bearing and unenforced — at most one unit in a compilation is restored from storage, and it always sits first — is what B1 removed.

The fold changes shape, and this is the whole of it:

```rust
// curios-pipeline, with this half in place. Compare `compile_units` today.
let mut units: Vec<Unit> = Vec::new();
let mut globals = Globals::default();

for source in sources {                              // dependency order
    let unit = match store.unit(source.key()) {      // key: its content, and the certifier
        Some(unit) => unit,                          // already judged; nothing re-runs
        None => {
            let elaborated = elaborate_unit(Scope::over(&units), source, budget)?;
            judge(&elaborated, &globals)?;           // curios-cert
            store.put_unit(source.key(), elaborated)? // refuses a positional identity
        }
    };
    globals.mount(&unit);
    units.push(unit);
}

// One erased artifact for the whole prefix, keyed on the ordered set above — never per unit.
let prefix = store
    .prefix(units.keys())
    .unwrap_or_else(|| store.put_prefix(units.keys(), erase_prefix(&units, budget)?))?;

// The entry is what you are editing, so it is never cached: it erases onto the prefix, as today.
let ersd = erase_onto(prefix, &entry, budget)?;
```

### B1 — a witness is identified by its mount

`Global::Witness(WitnessId)` is minted from one program-global counter, and it is the only name in a stored unit that carries no prefix. Two units elaborated in separate compilations both mint from zero, and aliasing one would silently rebind a coherence-table entry. The identity gains its declaring mount: a pair — mount and ordinal — is disjoint by the same argument mount disjointness carries everywhere else. The production surface is three files — the mint in `curios-text`'s `into_core`, the counter beside `fresh_binder`, and the variant with its `Display` in `curios-core` — and the archive schema bumps. It is also what makes a unit cacheable at all: a witness identity seeded at `witness_floor` takes different values depending on where the unit sat, so the witness counter is the only thing tying a stored unit to its position, and a per-mount ordinal is what lets B3's key be content-derived rather than content-and-position. `PreparedPrelude::witness_floor` becomes vestigial once each mount numbers its own.

*Must not change:* what any program means — a witness is anonymous and reached only through resolution. *Verified by:* the full gate over a corpus that runs identically, and the prelude re-certifying at 0 refusals against the bumped schema. **Rejected: renumbering witnesses as a unit is restored** — `cnum_map`, refused again.

### B3 — what replaces Cargo, and what the compiler starts believing

The key and the enforcement are one question — *what makes a cached verdict unforgeable and unstale* — and answering it turns the verdict from a build artifact into a recorded claim. That is not a reason to refuse it; it is a reason to write it down. **A cached verdict is a rule that admits, so it earns an entry in [SOUNDNESS.md](../../SOUNDNESS.md) — its assumption, its grade, and the evidence behind it — and no unit's verdict is cached before that entry exists.**

The key says *these terms, this certifier* — never a path, never a timestamp. An **over-broad** key invalidates too much and costs time (Cargo's crate-granularity, which made a kernel edit re-elaborate the standard library until `curios-analysis` was split out). An **imprecise** key fails to invalidate, and a verdict surviving a change it should not have survived *admits* — the only soundness question of the two. The terms half is the same content fingerprint the store is keyed by. The certifier half is derived, not remembered: the prelude's build script already hashes authored sources into an `env!`, and the same mechanism over `curios-cert` and `curios-analysis` yields a certifier fingerprint. The limit, stated beside the mechanism: a source fingerprint moves when those sources move, and a dependency bump changes what the certifier decides without touching them — so either the key covers that closure or it is conservative by construction.

### B4 — the erased artifact is keyed on the prefix, not on the unit

Re-erasing one unit costs **608 ms**, measured over the stored prelude in release, against a ~680 ms release compile of a one-line program — so a dependant cannot re-erase its predecessors per compile. It does not follow that each unit's erased form is stored alone: two independently erased arenas both number from zero, so per-unit erased artifacts need a relocation pass, which is `cnum_map` once more. Store the erased artifact against the **ordered set of predecessors** — today's mechanism unchanged, because the prelude *is* that set while there is one unit. Core and verdict cache per unit, where elaboration's cost is; the erased prefix caches per dependency set. Adding a dependency pays one erasure; compiling under an unchanged set pays none. This is what keeps "there is no link step at the erased level" true, and it is why `curios_unit::Unit`'s erased half is documented as provisional: it moves off the unit and onto the prefix.

### B6 — what never caches, and what only looks like it

**Genuinely program-wide:** witness coherence and the visibility fixed point. A coherence violation is only visible where two units meet, and `Audiences::compute` runs over the union of scope and unit. Neither is decidable inside a unit, so neither caches. **Stable under extension, and so cacheable exactly when the key covers the predecessors:** strict positivity, declaration sizing, and concept-registry validation — mounts are disjoint and units are ordered, so nothing later can add a constructor to an earlier unit's inductive. Decide each rather than the group. Either way the win is bounded rather than removed, because per-item typing is the expensive part. The entry never caches: it is what you are editing, and it erases onto the prefix.

## Out of scope

- **Version coexistence** — a conflict is a refusal, and everything above depends on it staying one.
- **Selecting versions, fetching, publishing, and a registry** — minimal version selection and a fetcher are additive layers that produce and deliver what this design consumes.
- **Parallelising elaboration** — the shared monotonic counters are a serialization point by design.
- **A third visibility level** — package-privacy is subtree containment, which the audience model already expresses; the module tree and the package boundary coincide here, so `pub(crate)`'s reason to exist does not arise.
- **The prelude as a package** — every compilation depends on it implicitly, no manifest names it, and its privileged mounts stay the compiler's own.
- **The archive as a stable interchange format** — it is scoped to one compiler build, and packages ship source.
- **Incrementality within a unit** — per-declaration fingerprinting answers a question about editing your own code that nothing here asks; this document needs a unit reused whole or recompiled whole.
- **Surface syntax** — `use /foo/Bar` already reaches a mounted prefix; [SYNTAX.md](../../SYNTAX.md) is untouched and no `.crs` file spells a dependency.

## Prior art

- **Cargo** — took: workspace-inherited rows (RFC 2906) as `[catalog]`'s precedent, explicit targets. Refused: target auto-discovery, dual-role root manifests, workspace-forced pin agreement, the crate-granularity cache key.
- **Go** — took: the `go.mod`/`go.sum` split of resolution from integrity, MVS as the named successor, domain-namespaced naming's lesson. Refused: nothing it offers here.
- **uv and pnpm** — took: the two-layer store (shared content-addressed cache under per-project materialization), flat workspaces with one lock's coherence argument; pnpm's catalogs are `[catalog]`'s nearest ancestor.
- **Zig** — took: exact pins plus content hashes with no registry as a working existence proof, and the tool-computes-the-hash flow. Its transitive-hash defect is the caution B3's closure rule answers.
- **JSR** — took: namespaced names mandatory from day one, because every flat first-generation registry is a squatting regret.
- **Swift PM** — took: executability as a declared statement of purpose (SE-0294 retired inferring it from `main.swift`). Refused: the products/targets split, via one-prefix-per-manifest.
- **Lake** — took: declarative TOML as the only manifest mode.
- **Elixir** — took: the poncho lesson — explicit path dependencies beat implicit umbrella coupling — as the reason members declare their edges and `path` exists.
- **Gleam** — its convention of self-prefixing modules under `src/<name>/` is what mounts enforce structurally; kept as the validation that prefix-as-identity is the load-bearing piece.
- **Coq** — mounts come from `-Q dir Lib`; refused the consumer naming the library, because Coq does not key coherence on the prefix and this design does.
- **Lean** — stores pre-elaborated environments per module; its trust posture — a stored verdict believed on the strength of the file it came in — is what B3 declines to inherit silently.
- **GHC** — the home/external split arrives as the provenance of a unit, cached or live; its per-declaration interface fingerprints are the granularity argument B3 leans on.
- **rustc** — assigns crate numbers per compilation and pays `cnum_map` to remap them on load; every identity decision here is downstream of refusing that.

## Milestones

The B identities are kept from the predecessor document because commits and the task list reference them; C1 and C2 are recorded as M1 and M2's prior names. Renumbering an identity to match a position is the mistake this design exists to refuse.

- **M1** *(was C1)* — the package manifest and the resolver, in the new `curios-package` crate: parse `curios.toml`'s package mode; generalize `RootSource` to one base per mount and replace both `UnitSource` arms with the resolver its documentation promises; implement the layout rule, including deleting `load_unit`'s stem-prepend, moving the bare-file base to its stem directory, migrating the corpus, and refusing stem collisions; thread real package names into the mount-collision diagnostic; and resolve names against mounts by longest match with the prefix-containment refusal, which is the whole of multi-segment support.
- **M2** *(was C2)* — the graph: dependency order from declared dependencies; cycle and conflict refusals; the umbrella mode with `members` and `[catalog]`; the `member`/`catalog` markers and their mismatch refusals; the governance walk; the `run` trichotomy.
- **M3** — the store: `.curios/`, the shared content-addressed cache, hash verification on materialization, and `curate`'s materialize and reconcile jobs.
- **M4** — the caching half, in order: B1, B3, B4, B6.
- **M5** — `curios new` scaffolding, over machinery that already works.

### Status

Updated as each piece lands, and deleted with this document. What the code corrected is recorded beside what it implemented, because a specification nobody amends is a specification nobody read.

- **M1, the manifest** — landed. `curios-package` exists, holding `curios.toml` and the `c1:` scheme; both modes parse and are mutually exclusive, a name is refused where no path could spell it, every dependency row is a table whose source's fields are required and whose other fields are refused by name, executables are enumerated against the package root's one stem space, and a hash is verified for scheme and shape where it is written rather than where it is used. The umbrella *mode* parses here too — it is one file and one parser, so splitting it would have meant writing it twice — while everything the umbrella *means* stays M2.
  - *Corrected:* the reserved segments are `let match choose rec mod use pub end false true induct struct foreign`, and not `Type`, `concept`, `satisfy` or `and`, which this document named as examples. Those four are grammar words the parser reads through `parse_keyword`, but the surface grammar admits all of them as path segments, so refusing them in a manifest would refuse a mount that resolves. The manifest refuses exactly what a path refuses, against one list rather than a copy of it — which is why the two spelling rules moved down to `curios-base`, beside the `Qualifier` whose segments they govern, rather than being exported from the lexer.
  - *Landed with it:* a `rev` is a `String` and a hash is a `TreeHash`. Opacity is a property of what the compiler does with a revision — compare it — and a newtype restating that would be mechanism over data; the hash's newtype earns itself by validating a scheme.
- **M1, the resolver** — landed. `RootSource` is one base per mount, and a base is a directory or a tree already in memory; lookup is longest-match over the claimed prefixes. `UnitSource`'s two arms are gone, and with them `PreludeModules`, which was a resolver over supplied bodies all along — what survives of the distinction is the one genuine difference, that an executable carries a tail expression and owns the empty prefix.
  - *Corrected:* the two discovery walks were not two configurations of one walk, they were one walk twice. Unifying them needed exactly one new idea — that the compilation root's items are the entry's own and nothing otherwise — and everything else fell out: the same three passes (`discover`, `seed`, `process_items`) each lost their arm-match for a filter over the unit's own non-root mounts.
  - *Also landed:* `load_unit` no longer materializes a mounted unit's tree eagerly. This document's premise that "a mounted unit reaches this stage as an already-materialized tree" was true of the code and false of the requirement — eager materialization was there because the mounted arm had no loader, not because it needed one. A mounted module now reports a parse failure with the span-carrying diagnostic the entry's modules always got, and `--unit` reads nothing until discovery asks.
- **M1, the layout** — landed. Every header's namespace directory is its stem directory, stated in `curios_text::RootSource` and obeyed by the entry too, so a bare `main.crs` declaring `mod util` reads `main/util.crs`. `curios-package`'s `layout` module owns the one exception and the stem space it implies: a package's library header sits beside its manifest, its namespace *is* the manifest's directory, and `lib`, every module that header enumerates, and every executable compiled from a file directly inside the root all claim stems there — twice is a refusal naming both claimants.
  - *Corrected, and decided with the author:* `--unit` takes a **package directory**, not a header file, and takes no prefix at all. This document called the flag "a mount and a location"; a resolved manifest entry is a directory holding `curios.toml` and `lib.crs`, and once the manifest is in it the prefix is the package's to declare (law 2) — a prefix on the flag would be the consumer naming the package, which is the very thing package-chosen naming exists to prevent. That reading is also the only one under which "the mounted loader differs only by prepending the root file's stem, and closing that gap is a deletion" is literally true: the base stops being `<dir>/<stem>` and becomes the directory named.
  - *Corrected:* the corpus needed no migration. Every `.crs` file in the tree that declares a file-backed module is a prelude source, and those are supplied by qualifier rather than resolved by layout; the three programs and two benchmarks declare none. What moved was one `curios-text` fixture, rewritten to assert the new rule rather than merely survive it — it now writes a decoy sibling that resolution must *not* find.
- **M1, multi-segment names** — landed. A name is an atom: resolution finds the owning mount by longest match, a head names the namespace it heads rather than the whole prefix, and the namespaces a multi-segment prefix implies are registered from every mount in the compilation rather than only the unit's own — so two packages sharing a leading segment stay both reachable instead of the later one shadowing the earlier. The mount collision generalized to non-disjointness in either direction, and it names both prefixes, which by law 2 is naming both packages.
  - *Corrected:* "no claimed prefix may lie within another claimed subtree" cannot be checked over the mount table alone, because the entry mounts the empty prefix and *every* name lies within that. The root takes no part in the relation — that it contains everything is what makes it the root — and what the entry contributes instead is its top-level `mod` declarations, each claiming its own stem. That reading was already implicit in this document's own example (`mod myorg` beside `/myorg/json`); stating it is what made the check writable.
  - *Also landed:* a supplied root claims a whole `Qualifier` rather than one segment. Nothing else could express a multi-segment package handed over already parsed, which is the shape the archive builder and every embedder use.
- **M2, the graph and the command line** — landed. Dependency order is a depth-first walk over declared edges with the governing package last; the governance walk finds the nearest package manifest above the working directory and then the umbrella that enumerates it, or none; the four marker mismatches are four refusals; and `run`'s three forms dispatch lexically, with a file argument captured by no manifest anywhere.
  - *Corrected:* a location has to be compared *canonically*, not as written. `../left/../base` and `../right/../base` are one package, and a diamond that read them as two would compile its point twice and hand out two nominally distinct families spelled the same — the exact failure package-chosen naming exists to prevent, arriving through the filesystem instead of through a prefix. The diamond test is what caught it.
  - *Also landed:* a row keyed by a name the package it points at does not declare is refused. This document states that a consumer refers to a package by the name it declares; the refusal that makes that true was not written down, and without it a `path` row can rename a package after all.
  - *Deferred with its reason:* the conflict refusal over **pins** is implemented and untested. Two dependents disagreeing about one name is checked from the rows themselves, before anything is located, so it fires whether or not either has been materialized — but a fetchable row cannot be located at all until the store exists, so no test can reach a second one. Its live counterpart, two dependents resolving one name to two directories, is tested now. **The pin conflict's test lands with M3.**
  - *Reported, not fixed:* `curios compile` still takes a path and only a path. This document specifies the trichotomy for `run` and says nothing about `compile`, so `curios run` now works with no argument inside a package while `curios compile` does not — a gap a user will find before a reader of this document does. It is one decision, not an oversight to fix quietly: whether `compile` shares `run`'s dispatch, and what `curios compile` with no argument should emit when a package declares several executables.
  - *Corrected, and decided with the author:* the library is no longer mandatory. This document said `name` obligates `lib.crs` and its absence is a refusal, which makes an executable-only package write a vestigial empty file to say it has no body. The stated justification — "there is no `library` key because the name is the declaration" — only holds if every package has a library, so its premise was false. A `lib` key was offered as the law-abiding fix and declined: either the file is there or it is not. Law 1 therefore carries one argued exception, recorded where it applies, and the argument for it is exactly the one that refuses the same move for executables.
  - *Named, not renamed:* the crate is `curios-package`. `curios-project` was this document's word and the author's first pick; `curios-package` was chosen over it on the second look, and over `curios-manifest`, which under-describes a crate that will own a content-addressed store and the toolchain's only network actor.
- **M3, the store and `curate`** — landed. `.curios/` holds its three families in separate subtrees; `c1:` hashes a delivered tree; `curate` materializes to a fixed point and reconciles; a `git` dependency resolves to the store when it is there and names `curate` when it is not; and `curios compile` writes into `bin/<package>/<name>`. The transport is `git`, shelled out to — a second implementation of the protocol vendored here would buy nothing the hash does not already guarantee, while owning an authentication story the installed one handles.
  - *Stated because the scheme has to and this document did not:* **both halves of each file are length-framed.** Without it a file `ab` holding `c` and a file `a` holding `bc` feed the digest identical bytes — two different trees, one store key. It is a wire-visible property of `c1:`, not an implementation detail, and it has its own test.
  - *Decided while implementing:* `.git` is removed before a delivered tree is hashed or placed. A fresh clone's object store differs run to run, so leaving it in makes the criterion unreproducible and the store key meaningless. The scheme cannot state this — it hashes whatever it is handed — so it is a fact about what `curate` hands it.
  - *Decided while implementing:* fetch, verify, **then** place. The tree is hashed where it lands temporarily and moved into the store only once accepted, so an interrupted fetch cannot leave a directory the store would later read as verified. Losing a placement race costs only the work: content-addressed means the tree already there is the same tree.
  - *Deferred, with its reason:* two of reconcile's four reports. "A dependency declared but never named by any module" and "a name resolved against no declaration" both need the compiler's mount table, which knows precisely which prefixes a unit resolved against but hands that over to nothing. The two answerable from manifests alone — a catalog entry no member references, and a `.crs` file nothing enumerates — are done. **The handover is the work those two are waiting on**, and it is a compiler-side seam rather than anything in this crate.
  - *Tested further than expected:* the fetch path is exercised end to end against a `file://` remote this machine serves, so init, fetch, checkout, dropping the repository metadata, hashing and placing are all covered without a network. What remains unexercised is authentication against a *remote* host over https or ssh. `curios-project` was this document's word and the author's first pick; `curios-package` was chosen over it on the second look, and over `curios-manifest`, which under-describes a crate that will own a content-addressed store and the toolchain's only network actor.
- **M4-B1, a witness is identified by its mount** — landed. `WitnessId` is the declaring mount and an ordinal within it, the seeded floor is *deleted* rather than left vestigial, and the display is mount-qualified. The prelude re-certified at **1091 items, zero refusals** under the bumped schema, and no program changed — which is the prediction this document made and the reason it held: a witness is anonymous and reached only through resolution, so what backs the identity was never observable.
  - *Confirmed by accident:* two of this workspace's own diagnostics already spelled a witness `sys/witness@0` while the identity behind it was a bare counter rendering as `witness0`. The doc comments were describing the design rather than the code, and B1 makes them true.
  - *The refusal grew, and asks a different question than the other two.* This document parked the storage check here — "witness identities are the fourth class, are not scoped to their mount yet, and are where this refusal grows when they are" — and the shape it grew into is not the obvious one. A metavariable or a free local is disqualifying **anywhere in a module's terms**; a witness *reference* is not, because a stored unit legitimately names witnesses its predecessors declared, scoped to their mounts. Every unit compiled against `/std` does. So the check is over `Module::witnesses` — what this module **declares** — and deliberately not through the position walk. Reading it off the terms would refuse every unit in the workspace.

## Tests

- **The diamond:** two packages depending on one package at the same pin compile it once, and a witness declared in it resolves identically through both.
- **The conflict:** two dependents pinning different `rev` or `hash` of one canonical name is refused naming both dependents and both pins, before any of the three elaborates.
- **The cycle:** a dependency cycle is refused; a manifest claiming a prefix another manifest in the graph claims is the mount collision, now naming real packages; and a prefix lying within another claimed subtree — an entry's `mod myorg` beside a mount at `/myorg/json` — is the containment refusal, naming both claimants.
- **The bare file:** a `.crs` file with no manifest compiles with no dependencies and no project, and its modules stem-nest.
- **The layout:** the library header resolves children as manifest-directory siblings while every other header stem-nests; a stem double-claim is refused naming both claimants; an unenumerated file is inert and reported by reconcile.
- **The resolver:** a source that is not a directory resolves, so the format cannot quietly assume a filesystem — the property that keeps `curios-web` compiling.
- **The hash:** a delivered tree verifies under `c1:`, a tampered byte is refused stating the computed hash, and a symlink in a delivered tree is refused.
- **The umbrella:** an umbrella governs only what it enumerates; a `member` row with no governing umbrella, a `catalog` row absent from the catalog, a `catalog` row naming a member, and a direct pin of a member's name are four distinct refusals; a catalog row no member references materializes nothing.
- **The dispatch:** `run`'s three forms dispatch lexically, and a file argument is never captured by any manifest.
- **The storage check** refuses an unscoped witness once B1 gives "scoped" a meaning; it already refuses a free local and a metavariable of either kind.
- **Two units elaborated in separate compilations**, each declaring witnesses, resolve to their own — the collision B1 removes, written as the fixture that would have caught it.
- **A cached unit and a freshly elaborated one produce the same program**, and changing either half of the key — the terms or the certifier — invalidates.

## Retirement criteria

Before this specification is deleted: both manifest modes parse and are mutually exclusive; a package's name is its mount, multi-segment names included; the layout rule is implemented with the bare-file break migrated; dependency order comes from declarations, with cycle and conflict refusals naming their parties; a source column names a resolver, with the filesystem as one implementation and `curios-web` still compiling; the `member` and `catalog` markers resolve through a governing umbrella with all four mismatch refusals; `run`'s three forms dispatch lexically and discovery walks only for declared artifacts; the store lives at `.curios/` under the shared content-addressed cache with hashes verified under the `c1:` scheme on materialization; `curate` fetches, materializes, and reconciles as the toolchain's only network actor; no stored unit carries a positional identity, witnesses included, with the check running at every seam a unit is written; and every cached verdict carries its [SOUNDNESS.md](../../SOUNDNESS.md) entry with a grade and evidence.

**The appendix is not deleted with this file.** Its measurements are the only record of how they were taken, and its findings outlive the work that turned them up.

## Appendix — measurements and adjacent findings

### Measurements

Every figure this document leans on, with its date, its **profile**, and how to retake it. Two items in a predecessor document were designed against unattributed numbers and both were wrong: a 471 ms eager restore that is 34.4 ms, and parallel certification's estimated 60–70 s win over an operation that takes 11.8 s. A number in prose with no method decays quietly and is then designed against, which is what this section exists to stop.

**The probe is in-tree.** It is `stored_prelude_measurements` in `curios-prelude-archive`, ignored because a measurement with an opinion is an assertion, and it takes every figure below in one run:

```sh
cargo test --release --package curios-prelude-archive -- --ignored --nocapture stored_prelude_measurements
```

`--release` is not a footnote: these are figures over the *stored* image, and a debug build measures a different program.

Taken **2026-08-10**, **release**, immediately after the erased arena gained its compaction pass — which is why two of them moved:

| What | Measured | Against |
| --- | --- | --- |
| Cold restore — bytecheck, then deserializing the prepared Text state, the Core and the erased prefix | 34.1–35.3 ms | 34.4 ms — confirmed |
| Erased-prefix clone, taken once per compile | 2.0–2.1 ms, mean of 100 | 1.4 ms — see below |
| Re-erasing one whole unit over the stored Core | 661–682 ms | 608 ms — up, partly spread |
| Certifying one whole unit — `recheck_module_verdicts` from an empty environment | 11.9–12.0 s, 0 refusals | 11.8 s — **confirmed** |

Shape, same run: 1091 items and 1107 definitions; 75 witnesses; 31 inductives, 47 structures, 14 concepts; `derived_binder_floor` **0**, against a lowering watermark of 6748. (The witness *inventory* B1 turns on — identities dense at 0..74, 34 of them referenced from terms — is a term walk the probe does not do. Recorded 2026-08-09; the count is unchanged.)

Ranges, not points, because these are two runs and the spread between them is part of what the figure is: the re-erasure alone moved 21 ms between consecutive readings, which is half of what separates it from the number it replaces. Nobody should read "608 to 682" as a 12% regression on that evidence.

**The clone's old figure was a single sample, and that was the defect.** Taken once again the same way it read 3.7 ms, which would have looked like a 2.6× regression from the compaction pass; averaged over 100 it is 2.0 ms. At one-to-four milliseconds the noise band swallows the signal, so the probe averages this one and reports the others once — which is the difference between a number and a reading. The 1.4 ms it replaces was never wrong so much as never repeated.

**The elaboration figures need no probe.** `curios-prelude-archive`'s build script already wraps its whole run in `curios_profile::capture` and writes per-span times, call counts, retained and allocated bytes, allocation counts, and every `sample!` magnitude to `OUT_DIR/profile.tsv`, announcing the path and the peak RSS in a build warning. Retaking them is one command:

```sh
cargo build --release --package curios-prelude --features profile
```

Profile is part of that command rather than a footnote: Cargo builds a build script in the profile of the build that triggers it, so a dev iteration loop and a release measurement are not comparable. Alternating the two also evicts one archive with the other, so a measuring session and a testing session pay for each other.

**Two inherited figures are now confirmed, and then superseded — in one day.** They were carried as *undated, profile unrecorded*: "469 s of a ~570 s prelude build in elaboration, and 204 s of that in universe finalization". Retaken **2026-08-10, release**, they were right: `elaborate_and_zonk_module` at **474.5 s** and `universe::finalize` at **204.6 s**. Solver work in the same session then moved them to **288.4 s** and **92.4 s** — the level substitution rebuilding its input per atom, and both totality obligations re-zonking a recorded type per checked term rather than per distinct type.

That is this section's own thesis arriving faster than expected. A number is not wrong because it was badly taken; it is wrong because the code moved. **Any figure here older than the last change to the pass it measures is a claim about history**, which is why the date and the profile sit beside each one.

The certification figure was this section's live example — 11.8 s taken before the kernel's level substitution changed, and the kernel calls it — and the retake it was owed says **12.0 s**. Unchanged. That is worth stating rather than quietly swapping, because the doubt was correct to raise and the answer is that nothing moved: B3 and B4 may design against roughly twelve seconds, and the reason to believe it is a command anyone can run rather than a number somebody remembers.

### Findings whose triggers fire inside this specification

**The `O(scope)` per-compile prologues.** Erasure projects the whole predecessor Core and re-seeds the elaboration context with every one of its definitions, and `Globals::of` copies every registry and builds a map of every definition — both on every compile, today, with one predecessor. Read from the code and **not measured**. Recorded because this specification multiplies each by the number of dependencies, and because measuring before designing is what removed three items from the caching half.

**Parallel per-item certification.** Split the certifier's walk into a serial define-all phase and a parallel check-all phase, one `Kernel` per item over a shared read-only environment, verdicts sorted by item index for determinism. Per-item kernels settle binder identity without arithmetic: each is seeded at the same derived floor, above every identity in the module. A shared counter is ruled out — nondeterministic under work stealing, and the archive must stay byte-reproducible. Any parallelism must be feature-gated native-only, because `curios-web` compiles `curios-cert` to `wasm32-unknown-unknown`, which has no threads.

*Declined on measurement, not merely parked.* The original estimate cannot be right, because certifying a whole unit takes 11.8 s and nothing can save 60 s of it. What the measurement changes is not the size of the prize but who pays: B3 caches a verdict against its terms and its certifier, so a dependency is certified once when it is stored and never again while both hold. Spending concurrency **inside the trusted base** — where *parallel verdicts equal serial verdicts* becomes something to prove — to speed up a once-per-dependency cost is the wrong trade. **Revisit if** first-build latency for a dependency, or a compiler upgrade re-certifying every cached dependency at once, becomes the complaint; and try narrowing what an upgrade invalidates before reaching for threads, since that is sequential and outside the trusted base.

**Incrementality within a unit.** Not declined on the merits — a different objective. This document needs a unit reused whole or recompiled whole; per-declaration fingerprinting, which is GHC's model, answers a question about editing your own code that nothing here asks.
