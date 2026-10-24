# A reader pays for what it reads of a declaration

[Every stage but reduction costs the graph it is handed](../../design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md) has a declaration read a fact at a time — "an instantiated declaration is read by facet — its sort, its arity, one constructor, one field — and only the facet asked is instantiated" — and a compilation end in time proportional to what was written plus what the budget counted; [an item imports what it reads of another's row](../../../curios-ersd/README.md#an-item-imports-what-it-reads-of-anothers-row) so that no row is copied once for each declaration that reads it. Two readers may still take a row whole where they read a fact of it, so that `k` readers of a row of width `w` cost `w·k`, and each is an investigation (*What is not yet known*): a call of one method of a wide concept, and a read of a wide structure's last field. A row is a family's constructors, a structure's or a tuple type's fields, a function type's domains, or an erased schema.

## What is wrong

Counted over the commit that types a slot without copying its type, with a release compiler built `--features profile` in a scratch copy, by the protocol under *Measuring*. A reader's cost is the difference between 16 readers and one, divided by 15, in kilobytes allocated, at a row of width 1,000 and of width 4,000.

| Reader of a row of width `w` | Elaboration | Kernel | Erasure |
| --- | ---: | ---: | ---: |
| A call of one method of a concept of `w` methods | 207–227 → 287–340 | 53 → 60 | 40 → 33 |
| A function reading the last field of a structure of `w` fields | 107 → 160 | 33 → 27 | 20 → 20 |
| *Control:* a function's parameter typed by a closed tuple alias of `w` fields, projected at `x.0` | 93 → 100 | 33 → 27 | 20 → 20 |
| *Control:* a function's unused parameter typed by a closed function-type alias of `w` domains | 87 → 87 | 27 → 27 | 20 → 20 |
| *Control:* a construction of the last constructor of a family of `w` | 67 → 67 | 20 → 20 | 20 → 20 |
| *Control:* two types of one family of `w` constructors compared | 273 → 280 | 47 → 40 | 27 → 27 |
| *Control:* a function's unused parameter typed by a structure of `w` fields | 93 → 93 | 33 → 27 | 13 → 13 |
| *Control:* a match writing one arm and a catch-all over a family of `w` constructors | 140 → 133 | 33 → 40 | 33 → 33 |
| *Control:* `Eq/refl()` of two constructions of one constructor of a parameterized family of `w` | 1,033 → 1,033 | 300 → 300 | 60 → 60 |

The fold states a stage's bytes to a tenth of a megabyte, so a row moves by 7 KB a reader with nothing grown. A method call's elaboration allocates differently between two runs of one program, as no other probe's does, so its row gives the least and the most of four runs.

## What is known

- **The peers read a synonym's kind and a constructor's tag without the rest.** GHC reads a type synonym's kind eagerly and its right-hand side through a thunk forced where it is needed, and a data type's constructor argument types lazily, "in the hope that we may never poke on those argument types" ([`TcIface`](https://downloads.haskell.org/ghc/8.0.1/docs/html/libraries/ghc-8.0.1/src/TcIface.html)); Rocq stores a constant's relevance on it, computed once where the constant is typed (`const_relevance`, [`constant_typing.ml`](https://github.com/rocq-prover/rocq/blob/master/kernel/constant_typing.ml)); Lean's IR constructor carries its index and its field counts and nothing of its family (`CtorInfo`, [`Basic.lean`](https://github.com/leanprover/lean4/blob/master/src/Lean/Compiler/IR/Basic.lean)), and its interpreter takes the IR "directly from the elab_environment", a declaration by name ([`ir_interpreter.cpp`](https://github.com/leanprover/lean4/blob/master/src/library/ir_interpreter.cpp)).

## What is not yet known

- **What a method call pays its concept.** One call of one method of a concept of 4,000 methods costs the elaborator 287 to 340 KB and 6,446 to 6,999 allocations over four runs, against 207 to 227 KB and 3,458 to 3,681 at 1,000, about an allocation for each method, and the kernel and erasure the same at both widths, counted with the figures above; what it allocates differs between two runs of one program. At `7c61c1a15` the growth stood in the allocations of `Context::standing_on` itself, a parked problem's retry, with no weak-head value filed as a key, so it is not the witness the call resolves, a structure of `w` methods. Settled by: gdb samples of the calls' elaboration at a concept of many methods, the cost attributed; a cost left is a stage here, and none left drops the item.
- **What elaboration pays a field read of a wide structure.** A function reading the last field of a structure of 4,000 fields costs the elaborator 160 KB and 1,593 allocations against 107 KB and 1,498 at 1,000, a few allocations that grow by about 18 bytes a field, and the kernel and erasure the same at both widths, counted with the figures above. Settled by: the read's elaboration attributed by spans; a cost left is a stage here, and none left drops the item.

## Stages

None is left: an investigation that finds a cost adds one.

## Measuring

Each probe is a program on standard input. A reader probe declares the row and `k` functions reading it, ending in `()`, under `wonder stage ersd -` or `wonder diagnostics -`: a tuple alias `pub let T: Type = {Nat, …}` read as `pub let f{i}(x: T) -> Nat = x.0;` or not at all; a function-type alias `pub let F: Type = (Nat, …) -> Nat;` read by `pub let g{i}(_f: F) -> Nat = 0;`; a structure `pub struct S: pub Type { f0: Nat, … }` read by `pub let g{i}(s: S) -> Nat = s.f{w-1};` or by `pub let g{i}(_s: S) -> Nat = 0;`; a match `match b | c{w-1}() => 1 | _ => 0 end` over the family; `Eq/refl()` at `Eq()(Wide/c{w-1}(Nat/add(i, 1)), Wide/c{w-1}(i + 1))` over `pub induct Wide(A: Type): pub Type` of `w` constructors `c{i}(A)`; a call `C/m{w-1}(x)` of a concept of `w` methods `m{i}(A) -> A`, satisfied at `Nat`; a construction `Big/c{w-1}()`; and, the control, `pub let g{i}(x: Wide(Nat)) -> Wide(Nat) = x;`. Run each under `--profile <stream>`, fold it with `curios profile <stream>`, and read a stage's allocated bytes and allocations off the rows `elaborate_and_zonk_with_prelude`, `recheck_module` and `erase_unit` with an empty group, a declaration's off `declaration` and `certify_declaration` with its group; a reader's cost is the difference between `k` = 16 and `k` = 1 divided by 15. Counts of what a walk materialized come from temporary `curios_profile::sample!` sites in a scratch copy of the same commit: each `TupleType` `Visit::adopt` rebuilds and `Term::unwrap_or_clone` clones, and each tuple a sort walk visits; a temporary `profile!` span around `Sharing::share` and `ClosedMemo::key` counts consing. Bytes are read off a build of the commit with no temporary site: a temporary span adds allocations of its own to the stage it stands in, by an amount the code does not decide. An alias's certification at 4,000 costs the same beside one reader as beside sixteen with no temporary site, and grew by 1.6 MB from one to sixteen under a span opened at each hash-consing call. A stored unit's bytes are a library package of the declarations, judged once by `wonder diagnostics lib.crs`, its `.curios/objects` summed. Every figure is counted; none needs a quiet machine.

## Verification and retirement

Each stage holds its own growth shape under `growth_slopes`, whose every slope stays under three where the shape grows a row's width and its readers together. A stage that moves stored bytes states them and holds verdicts, erased results and executable artifacts equal; one that moves none holds `/std`'s images byte-identical at one path. A landed stage moves its contract to its owner, and this specification keeps the rest. Retire it once both investigations are settled and nothing references this file.
