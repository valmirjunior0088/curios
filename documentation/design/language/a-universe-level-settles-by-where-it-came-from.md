# A universe level settles by where it came from

**Decision.** Every universe metavariable the elaborator mints carries a provenance — *chosen*, a `Type` written in an input position; *occurrence*, the instance level a use of a universe-polymorphic name mints; *inferred*, every other — and declaration finalization (`UniverseSolver::finalize`) settles a level by it before generalizing what is left:

- an occurrence's level settles at its recorded floor: its principal lower bound, or zero where a constant bounded it from below. The store discharges `0 ≤ u` as trivially true, so the floor is recorded as it is discharged, at insertion or by substitution. A level nothing bounds from below — an unapplied former's, a family's own sort — stays;
- a chosen level bounded only from above, by one other chosen level, is that level, so a concept's method level is its family's domain;
- a level neither the type nor the body mentions settles, after the occurrences;
- a still-deferred witness goal is made ground: its levels settle, and so does every level a settlement lands on, chosen ones included;
- a result sort is read as determined only where the declaration's terminal is a sort.

Where two levels are identified, the member of greater provenance represents the class. With an occurrence where its argument is, a written type lands in the sort its reduct does, and the kernel types a type as written rather than its reduct.

**Rationale.** Both checkers size a tuple type or a Π from its parts' reducts, where an occurrence's instance is gone, while the elaborator generalized an occurrence's level as interface: `pair_of(x: List(Nat)) -> Io({List(Nat), List(Nat)})` carried a parameter per occurrence, each above the `Type 0` its `Io` was sized at, and the kernel certified it only because it typed reducts. Typing the spelling refused 46 `/std` declarations, every one that class, and none admitted `False`: a written level overstates its reduct's and never understates it. Rocq settles the same levels the same way. Its minimization touches "only the fresh universes generated for each global application", never "a user-written anonymous Type" (`dev/doc/universes.md`); `choose_canonical` keeps a rigid universe as a class's representative over a flexible one; an inferred `Set ≤ u` is kept apart so minimization can read it; and Agda under `--cumulativity` instantiates a level bounded only from below to the join of its bounds. The tying is Lean's `Pure (f : Type u → Type v)` declaring `pure {α : Type u}`, and Rocq's manual writing `monad@{i}` with `unit : forall (A : Type@{i}), A -> m A`: generalized apart, `Monad`'s method levels were three more parameters, a witness pinned them at zero, and `!` was refused at a large payload. A deferred goal resolves after its declaration's scheme has closed, so the witness it finds can only be pinned to what the goal fixes; the least levels are the one assignment every witness polymorphic in them, or fixed at zero, answers. `/std` went from 4 472 universe parameters to 815 and from 771 polymorphic declarations to 450; the kernel types a written type with no refusal and certifies the library faster than it reduced first, 29.1 s against 32.3 s.

**What it leaves.** A generic declaration that dispatches through a witness declared later in its unit is pinned at its least levels (`tests::universes::a_goal_deferred_past_its_declaration_settles_at_its_least_levels`); the scheduler orders an item after the witnesses it reaches by operator or method name, and not yet after those its `!` reaches. And `!` holds its region at the level of the action it binds (`tests::concepts::monad_tests::a_bang_holds_its_region_at_a_lower_nominal_actions_level`), since both checkers compare a nominal type's universe levels for equality — the fork [Cumulative inductive types are an open fork](cumulative-inductive-types-are-an-open-fork.md) holds open.

**Rejected.**
- *Reconstructing a level's role at finalization* — an occurrence flag, a record of discharged constant bounds, the written levels an occurrence was aliased to. Which member of an identified pair survives was arbitrary, so settling an occurrence settled a choice with it.
- *Rocq's atomic-bound condition* for minimizing: Curios's levels are algebraic where Rocq's are not, and it left occurrences whose lower bound is a maximum floating above their families.
- *Keeping method levels generic and solving their disjunctions harder*: a witness's scheme carried maxima over them, and a use relating that domain to one level met a disjunction that closure could only guess — a cost growing with every concept.
- *Minimizing only the occurrences of definitions that unfolding erases*, since conversion observes those levels.
- *Equating an occurrence's level with its argument's at application*: incomplete under cumulativity where one level bounds several arguments.
- *Requiring a witness before its use*, as Lean and Rocq do: it breaks `syntax.md`'s promise and `/std`'s module order.
- *Checking witness heads before any body*, as GHC checks instance heads: a Curios witness's scheme depends on its body — `Lift(Io, Io)` has one level because its body identifies its two sides.
- *Fixing a scheme from the signature before the body*, as Lean does for a theorem's header: a body could not narrow it without level syntax.
- *McBride's displacement* (Favonia, Angiuli and Mullanix, POPL 2023): one level per definition displaced at each use sets cumulativity aside, and would replace the solver and the kernel's entailment.
- *Sizing by the written spelling in both checkers*: it keeps every floating level and pays for it at the size condition.
- *Resolving the `/sys` formers to their intrinsics*: aliases and nominal occurrences float the same way.
- *Declaring the reduct the truth*: the kernel would keep accepting a declared type it never typed.
- *Settling only the levels a deferred goal names*: `tcp/Socket/close_raising` kept `A`'s level, the constraint its late witness needed reached no scheme, and the kernel refused it.
- *Retrying a declaration's own deferred goals before its scheme closes*: a goal defers only when no witness answers its key, and none registers while one declaration elaborates.
- *Elaborating a missing witness on demand*, pausing the declaration that needs it: item elaboration would have to be re-entrant.
- *Holding a declaration's scheme open until its goals resolve*: the items between it and its witness would take its levels rather than instances of them.

**Measured by** `curios-prelude-archive`'s ignored `universe_parameter_census`, which prints every `/std` definition's parameter count, and `kernel_disagreements`, which walks `/std` through the kernel and prints each refusal; `curios`'s `tests::universes` holds the settled schemes.
