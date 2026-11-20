# A cabinet holds one object a unit, and what its projects reach

A unit's place in the cabinet is addressed by the compiler that filed it (`unit_slot`, `entry_slot`, `payload_slot`, `curios-package/src/cabinet.rs`), and a payload's by the engine besides. So every compiler build files a new place for every unit it compiles, and nothing removes the places no compiler files at any more. `unit_slot`'s `///` says the address is "what bounds the cabinet — a project has as many slots as it has units, forever", which holds for one compiler. A unit a project no longer has keeps its place as well, and a layout a move leaves behind stays whole, since no compiler knows it.

Measured on 2026-11-20:

- The curios-papers project's cabinet holds 148M in the layout `d1b828364` and `e35c93f80` moved away from, which no compiler reads. Of its nine verdicts, filed on 11-13, 11-15 and 11-17 by three compiler builds, six were dead before the move.
- The compilers of `dc1f25fa4` and `19d53645f`, which differ in the CPS optimizer alone, file `/std`'s test unit as one byte-identical pack of 88,879,924 bytes beside index files that differ, and the cabinet holds the pack twice, at two addresses.

The words are uneven besides. *Slot* names this place and, in the back end, a row's field position (102 files there, 37 in the cabinet's crates). *Object* names a content-addressed piece of a stored unit where a compiler's *object* is the compiled unit itself. *Verdict* names a checker's judgment, a test's outcome, and the cabinet's crate, handle, family and file. `curios-verdict`'s `//!` still calls a unit's place a one-file slot, which it has not been since `e35c93f80`.

The next step is stage 1.

## Decided

Settled by the user on 2026-11-20.

- **The cabinet's words.** A unit as the cabinet keeps it is an *object*, as a compiler's compiled unit is (C's `.o`, OCaml's `.cmo`, Rocq's `.vo`, Lean's `.olean`): a directory `objects/<address>/` holding its `unit` file, its record and its index, beside its packs. A content-addressed piece of an object is a *blob*, as git's, restic's and Bazel's are, and a *pack* is a file of blobs. *Slot* is left to the back end's field positions, and *verdict* to a checker's judgment. `.claude/rules/documentation.md`'s vocabulary table states each.
- **An object's address belongs to `curios-verdict`.** `unit_slot`, `entry_slot`, `payload_slot` and the two schema tags move from `curios-package`, `curios-verdict` being their only reader, beside the record they pair with ("A slot is addressed, and a hit is verified"). `curios-package`'s "One subsystem, one owner" is restated to what it owns: where `.curios/` and its cabinet are, its families' paths, the package's own families, and the compiler's identity.
- **The compiler is named by the record, not the address.** An object is addressed by what its unit is: its mounts or its executable, what it declares, its kind, and the archived root it stands in for. Its record names the compiler whose verdict it holds. A read by any other compiler is a miss, neither a hit nor a baseline, since a baseline's verdicts are believed too ([Cached verdicts](../../design/soundness/cached-verdicts.md)). A compiler change therefore replaces the object, and its filing writes only the blobs the address lacks. A payload is addressed by its package, executable and target, and names its compiler and engine in its frame. Two compilers alternating on one project each recompile the unit at every switch, and the blobs they share stay written. Peers: rustc's incremental cache writes the compiler's version into its files' header, discards a mismatch and overwrites it ([`file_format.rs`](https://github.com/rust-lang/rust/blob/master/compiler/rustc_incremental/src/persist/file_format.rs)), and Ninja keeps one result per output. The peers that key by compiler each need a second mechanism: Go a clock ([`cache.go`](https://go.dev/src/cmd/go/internal/cache/cache.go)), ccache and sccache a size cap, Cargo's target directory an outside tool ([cargo-sweep](https://github.com/holmgr/cargo-sweep)).
- **No clock.** A filing keeps the blobs its record and the record it replaces name, and drops every other, at every filing. Two writers alternating at one address, two checkouts sharing a cabinet or two compilers, lose nothing, and a third's blobs go at the next filing. The packs' geometric merge, already counted rather than timed, bounds what the dropping rewrites. A filer naming a blob that a filing beside it drops stays a miss and never a belief. This revises [What no slot names is collected five days after a collection first finds it so](../../../curios-verdict/README.md#what-no-slot-names-is-collected-five-days-after-a-collection-first-finds-it-so): its grace and its daily interval served those alternating writers, and its rejection of a time renewed by reads held of blobs, never of addresses.
- **What a project no longer reaches goes.** A filing in a project's own cabinet keeps the objects of its fold's units in both kinds, the sources its lock pins and the payloads of its executables, and takes away every other object. An entry of `.curios/` that no family of the running compiler holds is taken away too, so a layout a later move leaves behind is collected without a migration written for it.

## Open

- **A shared cabinet.** A run against `CURIOS_CABINET` knows its own project's roots and no other's. Either each project's run files what it reaches under a name for its root, and a collection keeps the union of the projects whose root still exists, as Nix keeps a closure while its root link resolves ([gcroots](https://nixos.org/manual/nix/stable/package-management/garbage-collector-roots)); or a shared cabinet is collected within each address alone, the third decision not holding there.

## Stages

1. **The words, and the address's owner.** The renames, and the three addresses with their schema tags moved into `curios-verdict`, with no behaviour moved.
2. **The compiler into the record.** The addresses lose the compiler, and the payload's loses the engine; the record and the payload's frame name them, and a read by another compiler is a miss. Both schema tags advance.
3. **No clock.** `Packs::collect` runs at every filing and keeps what the new and the replaced record name; `GRACE`, the interval and the `collection` file go, and the decision is restated.
4. **What a project reaches.** The collection from a project's roots, the unknown entries of `.curios/`, and the shared cabinet as *Open* settles.

## Verification

- Stage 2: a cabinet two compiler identities file one unit into in turn holds one object and each pack once, and another compiler's object is no hit and no baseline, each mutation-checked.
- Stage 3: alternating filings of two texts of one unit keep both texts' blobs, and a third text's filing drops the first's.
- Stage 4: a module deleted from a project, and an entry of `.curios/` no family holds, are gone after the next filing; the curios-papers cabinet, filed by the compiler of this stage, holds its project's objects and nothing else.

## Retirement

When stage 4 lands, its decisions live in `curios-verdict/README.md` and `curios-package/README.md`, and this file is deleted.
