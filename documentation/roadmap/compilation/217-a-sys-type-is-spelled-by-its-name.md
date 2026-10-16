# A `/sys` type is spelled by its name

**Not refined yet.** This specification reserves spelling a `/sys` declaration's type by the carriers' names, as a program's is, where it holds the intrinsic nodes today. It is not an implementation plan; it waits on the measurement named under *Refinement*, which says whether it is worth its cost.

## What is missing

`projection` (`curios-text/src/sys_module/projected.rs`) reads each type core's table states back as the node it is, and the roster builds `Nat`, `Byte` and `Bool` the same way (`curios-text/src/sys_module/helpers.rs`), so `/sys/Cell/fill` returns `Io(Bool)` over the nodes `IoType` and `BoolType`, where a program's `let f(b: Bool) -> Bool` elaborates to `(b: /sys/Bool/Bool) -> /sys/Bool/Bool`: the two meet by unfolding the name. Nothing is refused for it, and since `5b0ea16f9` no page shows it, the printer writing a node by its `/sys` name as a report does.

## What is known

- **One read-back was tried and taken out**, on top of `5b0ea16f9`: a type former read back by name — a node whose signature produces a sort, written by `curios_core::intrinsic_path` over its operands, a bound left the node its row states, the three helpers written alike. The prelude elaborated and certified and the `curios-text` tests passed with one test restated, but `elaborate_and_zonk_with_prelude` took 52,486 to 52,727 ms over three runs of `cargo xtask profile-prelude elaboration` against 52,346 to 52,431 ms without it (medians 52,672 and 52,401), and `/sys`'s image grew from 821,108 to 940,758 bytes; certification's `recheck_module` took 16,826 to 17,138 ms against 16,793 to 17,355. The documentation record came out the same size, 899,357 bytes; the pages were not compared.

## Refinement

- **The measurement it waits on.** Whether a program's elaboration gains, where its types meet `/sys`'s, what the prelude's lost: a program typed against many `/sys` operations, elaborated with the read-back and without, and `profile-prelude`'s two halves, three runs each way. The read-back is kept only where that program elaborates faster by more than the prelude's 0.5% loss.
