# curios-package

What a Curios package is, and everything that reads one: the `curios.toml` manifest, the walk that decides which manifest governs an invocation, the resolver that turns a declared dependency into a module tree, and the store the results are filed in. It sits beside the compiler boundary rather than under it — `curios-pipeline` folds its stages over whatever scope it is handed, and deciding that scope is a product's job. The four laws the crate enforces are stated below, where its comments cite them by number; the subsystem's invariants and refusal discipline belong to the crate rustdoc, and the command-line and manifest reference to [usage.md](../documentation/usage.md).

## Design

### A compiler version belongs to the package or its enumerating umbrella

**Decision.** The effective `compiler` version is mandatory, and it is the one compiler the package is written for. A standalone package declares it; an umbrella declares it once and every enumerated member inherits it without a local override. The existing governance walk decides membership, so a directory beneath an umbrella is not enough. Each dependency resolves its own owner, and is held to its own version as the package that reaches it is. The version is a bare SemVer version, and the running compiler must be that version exactly, by SemVer precedence, checked before compilation or cached execution; it downloads no toolchain. The field's syntax and diagnostics are [the manifest reference](../documentation/usage.md#the-manifest)'s.

**Rationale.** Missing metadata cannot say which compiler a package is written for, and a parser error inside source another compiler reads does not identify that mismatch. A package is checked by the compiler that built it and by no other: a later one may refuse it, or read it otherwise, and nothing in the package says which, so the version it states is the one known to hold. Membership already owns project-wide configuration, while a package outside that membership owns its own contract. Requiring the declaration at that owner gives every package one requirement and every refusal the manifest somebody can edit. The existing compiler identity still keys cached verdicts: a release version cannot identify a developing compiler's implementation.

**Rejected.** An optional field leaves the requirement unknown. A bare version read as a minimum admits every later compiler on the word of an author who ran none of them. A range or an upper bound states more than its author checked, every version inside it. A dependency exempt from the check reads one field two ways, by where the package stands. A per-member override duplicates the umbrella's declaration. Inheriting from a consuming package gives a dependency a requirement its author never declared.

### The four laws

1. **Declaration decides; location does not.** Modules exist because a header declares `mod`, artifacts because the manifest declares them, members because the umbrella enumerates them — a file nothing names is inert, wherever it sits. The two exceptions are a package's own `lib.crs` and `exe.crs`, whose presence beside the manifest is their declaration, for the reason `layout.rs` states.
2. **Identity is declared exactly once, by its owner.** A package names itself, every consumer refers to it by that name, and the filesystem spells structure, never names; no identity meaningful only in the compilation that assigned it is ever stored.
3. **Membership organizes; dependency compiles.** The umbrella's tree decides where the store goes and what a marker may resolve to; only declared dependencies order compilation, and neither implies the other. Declared dependencies also bound it: a unit may name the prefixes its manifest declares and `/std`, and nothing else — a transitive dependency is in the fold, and writing its prefix is refused by name.
4. **A refusal fires early and names both parties.** Conflicts, collisions, cycles and missing obligations are diagnosed before elaboration, against the file somebody wrote, never surfaced as an unbound name or a conversion failure holding no span.

### A module answers its own package's declarations

**Decision.** A `[[foreign]]` row's `[foreign.answers]` is keyed by declaration, each named from the package's own root, plain and relative as an executable's `path` is, beside the export that answers it; the qualified name is made of the key and the package's name where the manifest is read. A path written in full is refused there, and so is a declaration a second row answers: a row states one export a declaration by its table's shape, and the rows of one manifest are held apart where they are read. The claims travel with the library's unit, and the fold holds each to a `foreign` that unit declares, for a unit compiled or restored alike; a unit's stored record holds the claims it was compiled under, so a stored unit, and a program's stored payload, which compiles none, is taken only under the claims the manifest makes now. A package with no library has none to claim. A row no claim names is the host's to answer.

**Rationale.** Who may answer a row is its declarer's to say (law 2), and a claim spelled as a qualified name can name any row of the compilation: a dependency's manifest could answer another package's row, or one of the standard library's, with a module of its own. Spelled from the package's root, a claim on anything else cannot be written, so no later stage holds a claim to its claimant. Whether a claim names a declaration is asked of the rows the unit holds, the compilation's own answer, where a walk over the manifest would predict a qualified name from a source position.

**Rejected.** A table keyed by export, under which one module's two exports, or two rows, can claim one declaration, and what refuses them is a check where the modules are linked; a qualified path checked against the claimant's prefix, a check where a spelling suffices; ownership decided where modules are linked, after every unit is compiled and with no mount in hand; a claim on a program's own row, which is one program's private name and no declaration of the package — a program reaches a row its package's module answers through the library, as it reaches everything else of the package; the link name written on the declaration, which puts how a row is answered into the source that only names it.

### A pin is derived by one command, and checked everywhere else

**Decision.** `curios pin` is the one command that writes a manifest, and the one place a hash is derived from a delivery; every other reader accepts a delivery only against the hash its row states, whichever transport delivered it. Adding a row and re-pinning one are one act. `member` and `catalog` rows, which point at no location, are not the command's to write. What a pin delivered is filed in the store under the digest it turns out to have, and a fetch is described — its size, its kind, its exports, and that nothing verified it — before its pin is written. `curate` and `pin` reach the network through programs already on the machine: `git` for a package, and `curl` or `wget` for a module. A `[[foreign]]` row states a `hash` whether its module is carried or fetched.

**Rationale.**

- **Computing the hash is the pin's point**, so the command that derives one cannot check one; everywhere else there is a hash written down to check against.
- **A row that did not exist is one with nothing to keep**, so adding and re-pinning are one act. A marker names no location: there is nothing to deliver and no hash to derive, and a word in the manifest is less to type than a command.
- **Filing a delivery under its own digest is sound**, since the digest is computed from it: the entry holds bytes that hash to its name, every reader hashes what it takes out again, and the `curate` after a pin finds it rather than fetching it twice.
- **A re-pin is the one moment a hash stops being a check and becomes a claim**, so what arrived is described before anyone believes it.
- **Acceptance is by hash, so any transport may deliver the bytes**, a mirror no weaker than the origin, and the transport already installed is the right one.
- **A carried module only looks covered by the tree hash its consumer pinned.** It is when its package came from git, but the entry package is hashed against nothing and `path` and `member` dependencies are unpinned, which is where a locally built module lives.

**Rejected.** A separate `add`, a second spelling of the same act; writing `member` and `catalog` rows from the command line; a network client vendored into the compiler; a `hash` for fetched modules alone.

### The store goes where a manifest governs, and nowhere it was not pointed

**Decision.** Everything generated lands in one `.curios/` beside the governing manifest, the umbrella's where one governs, so members share it. `CURIOS_CACHE` moves the half that says nothing about which project asked, and has no default. A source's hash scheme is a directory of its own.

**Rationale.** A toolchain that writes into a home directory nobody pointed it at does what the person who ran it did not ask for. A scheme of its own lets a successor stand beside `c1` rather than replace it.

**Rejected.** A default cache directory divined from the environment.

### `new` starts a package whole

**Decision.** `curios new` writes the manifest, `lib.crs`, `exe.crs` and a `.gitignore` for `.curios/`, and no flag asks for fewer.

**Rationale.** A package may be a library alone or a program alone, which deleting either file says, but starting one is not the moment to be asked: a flag asking would be answerable only by someone who already knows what the two are.

**Rejected.** A flag that starts only the library, or only the program.

### A stored unit is reused whole, and an edited one is compiled over it

**Decision.** A unit is filed whole and reused whole. An edited unit is compiled over the unit the store holds, by a build as by a question, keeping every declaration whose reads the edit did not move, through the `Cache` seam `curios-verdict` implements over this crate's store ([A stored unit is a baseline for an item-level recompile](../documentation/design/compilation/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md)); nothing here recognizes a declaration. A dependency's verdict is cached against the compiler that reached it and verified against the files it was compiled from, so it is certified once when stored ([Cached verdicts](../documentation/design/soundness/cached-verdicts.md)).

**Rejected.** Parallel per-item certification — a serial define-all phase, then one `Certifier` per item over a shared read-only environment, verdicts sorted by item index. It spends concurrency inside the trusted base, where parallel verdicts equal serial verdicts becomes a thing to prove, to speed a cost paid once per dependency; any parallelism would also be native-only, since `curios-js` compiles `curios-cert` to a target without threads. Narrowing what a compiler upgrade invalidates is sequential, outside the trusted base, and comes first. `curios-prelude`'s `stored_prelude_measurements` retakes what certifying a unit costs.

### One subsystem, one owner

**Decision.** The manifest, the walk that decides which manifest governs an invocation, the resolver and the store are one crate. `curios-pipeline` never learns it exists: it folds its stages over whatever scope it is handed, and deciding that scope is a product's job, as supplying the syntax registry already is. `curios` is the product that decides it, over this crate; `curios-js` never touches it.

**Rationale.** The four are faces of a single question — what is in this compilation, and where did each part come from — so one crate answers it rather than four seams through the driver. `curios-js` reaching none of it is the constraint that keeps a resolver from quietly assuming a filesystem.
