# A cabinet holds one object a unit, and what its projects reach

An object is collected within its own address alone (`Packs::collect`, `curios-verdict/src/packs.rs`), and nothing removes an object no project reaches any more: a unit a project no longer has keeps its object, an executable a manifest dropped keeps its payload, and a layout a move leaves behind stays whole, since no compiler knows it.

Counted on 2026-11-20: the curios-papers project's cabinet holds 148M in the layout `d1b828364` and `e35c93f80` moved away from, which no compiler reads. Of the nine units filed there, on 11-13, 11-15 and 11-17 by three compiler builds, six were dead before the move.

The next step is stage 4.

## Decided

Settled by the user on 2026-11-20.

- **What a project no longer reaches goes.** A filing in a project's own cabinet keeps the objects of every unit the project reaches, and takes away every other object: every package its governing root holds, each member an umbrella enumerates or the one package, each one's library and executables in both kinds, and what each reaches, since a run's fold reaches its own package's graph alone (`order`, `curios-package/src/graph.rs`). It keeps the payloads of those executables and of their test subjects, and the sources and foreign modules their manifests pin. An entry of `.curios/` or of its cabinet that no family of the running compiler holds is taken away too, so a layout a later move leaves behind is collected without a migration written for it.

## Open

- **A shared cabinet.** A run against `CURIOS_CABINET` knows its own project's roots and no other's. Either each project's run files what it reaches under a name for its root, and a collection keeps the union of the projects whose root still exists, as Nix keeps a closure while its root link resolves ([gcroots](https://nixos.org/manual/nix/stable/package-management/garbage-collector-roots)); or a shared cabinet is collected within each address alone, the last decision not holding there.

## Stages

4. **What a project reaches.** The collection from a project's roots, the unknown entries of `.curios/` and of its cabinet, and the shared cabinet as *Open* settles.

## Verification

- Stage 4: a package an umbrella stops enumerating, an executable a manifest drops, and an entry of `.curios/` no family holds are gone after the next filing, while a sibling member's objects stay; the curios-papers cabinet, filed by the compiler of this stage, holds its project's objects and nothing else.

## Retirement

When stage 4 lands, its decisions live in `curios-verdict/README.md` and `curios-package/README.md`, and this file is deleted.
