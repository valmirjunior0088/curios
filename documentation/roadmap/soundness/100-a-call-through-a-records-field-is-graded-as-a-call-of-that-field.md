# A call through a record's field is graded as a call of that field

**The benefit.** A recursion through a record of functions descends where the function the record holds descends, so a witness whose method recurses on its argument is total and a type may reach it: a derived `Eql`, `Ord` or `Spell` on any recursive type, the first witnesses a program over a tree derives, a hand-written `satisfy` whose method calls itself on a payload, and a struct of handlers a recursive walker is handed. A recursion through a record's field the member's own body applies descends ([Totality of the erased program](../../design/soundness/totality-of-the-erased-program.md), *A call is read off a path*); one through a record handed to another definition is still refused wherever a type reaches it. The project has decided to adopt it.

The next step is stage 2.

## What is missing

**A record handed to a definition that applies a field of its parameter is graded as fresh.** A derived `Eql` on `induct Rose | rose(Nat, List(Rose))` compares the children with `List`'s witness, handing it the witness being defined (`use /witness~/std/ops/Eql(/Rose)`), and `/std/List/eql` applies the method off that parameter, `x == y` being `e.0(x, y)`. A summary states how a definition applies each parameter (`group_applications`, `curios-analysis`), and the summary walk reads a parameter's field applied as a use it does not follow, so `List/eql` publishes nothing for its witness parameter, and the recursion through it is graded as fresh, in both checkers alike. Recursive data whose children sit in a container, a document tree or a syntax tree, takes this shape, and so does a structure of handlers handed to a walker over a list.

## What this builds on

- **Summaries of how a definition applies its parameters** (`group_applications`, `handed`, `binder_bounds`, `curios-analysis`), filed in each checker's record and read across units, with a parameter used any way the walk does not follow publishing nothing.
- **A callback handed bare.** A member handed at a summarized parameter is graded as the calls the summary's applications make of it (`record_handed`, `curios-cert/src/certifier/calls.rs`), which is how a record handed at one is graded here, field by field.
- **Both checkers judge through the shared layer.** The elaborator's walk and the certifier's call recording read calls the same way, and a change on one side alone is a disagreement the board's probe shows.

## What the peers do

- **Lean** leaves a recursive instance to its user: the method is a function defined first and the instance names it (`instance : DecidableEq Term := Term.deq`), and an instance whose method resolves back to itself meets the chicken-and-egg loop its users describe ([mutually recursive instances](https://leanprover-community.github.io/archive/stream/270676-lean4/topic/mutually.20recursive.20instances.html)). Its derived `BEq` on a nested inductive is a `partial` definition, opaque to proofs, and deriving `DecidableEq` or `LawfulBEq` on one has long been unsupported ([DecidableEq and List](https://leanprover-community.github.io/archive/stream/270676-lean4/topic/DecidableEq.20and.20List.20in.20inductive.20type.html), [BEq for a nested inductive](https://leanprover-community.github.io/archive/stream/113488-general/topic/Defining.20.60BEq.60.20for.20nested.20inductive.20type.html)); its users prefer structural recursion to well-founded encodings because the latter reduce poorly in `decide`.
- **Rocq**'s guard condition reads through a callee's fixpoint where the function the callee is handed is a uniform parameter, the precedent the totality decision cites for callbacks; a container's witness argument is such a parameter, so a summary per field is that precedent read through a record.
- **Agda**'s termination checker gives record projections and copatterns treatment of their own and lets a record constructor be inlined for it ([record types](https://agda.readthedocs.io/en/v2.7.0/language/record-types.html), [records and termination](https://lists.chalmers.se/pipermail/agda/2014/007319.html)): reading through a record needs explicit care and does not fall out of the rest. Its sized types, the type-based route, have admitted proofs of ⊥ ([2020](https://lists.chalmers.se/pipermail/agda/2020/012327.html)).
- **Idris 2** does not see a recursion behind a higher-order function, and its users reach for `covering` or `assert_total` ([totality](https://idris-community.github.io/idris2-tutorial/Tutorial/Folds/Totality.html)).
- **Higher-order size-change** handles closures by a whole-program flow analysis ([Sereni and Jones](https://www.cs.ox.ac.uk/techreports/oucl/rr-04-20.html)); a summary a definition publishes is its modular counterpart.

What is taken: Rocq's reading through a uniform parameter and Agda's explicit treatment of projections, both as summaries a definition publishes; terms left as written, so reduction is as fast in a type as before. What is set aside: anything that makes the method another term, `partial` above all, and every escape a user writes.

## Settled

- **A call is read off a path**, as the totality decision states it: its callees the parts of a member it reaches, a member's parts its leading lambdas and each field lambda of the record under them, and every part of a record it lets escape (`group_parts`, `part_calls`). A parameter's path, `(e.1.0)(y)`, is read the same way.
- **A summary states how a definition applies each field of a parameter**, beside how it applies the parameter itself, under the rule it states now: every use the walk sees — the field applied, or the parameter handed on unchanged at its own place — or the position summarizes nothing.
- **A record handed at a summarized parameter is graded field by field.** A member, a member applied to its parameters, or a record literal handed where a definition applies a field of its parameter makes the calls of its own field at that path that the summary's applications grade, as a member handed bare makes its calls now.
- **By structure, not by provenance.** A record is any struct or tuple of functions; nothing asks whether it came from a `satisfy`, so a struct of handlers is graded as a witness is.
- **The terms are left as elaborated.** No method is lifted into a definition of its own, no name is minted for one, and nothing a report shows or a type reduces changes.
- **Rejected.** Lifting a method, and marking a derived witness partial, are the totality decision's. A `derive` that writes container-specific mutual helpers, Lean's route: it covers derived witnesses alone, and only the containers it knows. Unfolding a callee at its call site, as a guard condition does: a declaration's verdict would read its callee's body rather than what the callee published, against [A declaration is a function of what it reads](../../design/compilation/a-declaration-is-a-function-of-what-it-reads.md). Sized types: the type-based route the peers' record shows admitting `False`.

## Stages

2. **Summaries per field, and records handed at them.** A summary states the applications of each field of a parameter, and a record handed at one is graded field by field. Check: a derived `Eql` on `Rose`, and a user's struct of handlers handed to a recursive walker over a list, reached by a type, pass both checkers; a container applying the method at its own argument, and a record handed where the summary records nothing, refused by both; the record's format raised where a summary's archived form moves.

What this waits on beyond its stages: a container whose operation is no recursive group, `Option`'s `eql`, publishes nothing until [A call in a callback is graded where its callee applies it](302-a-call-in-a-callback-is-graded-where-its-callee-applies-it.md) summarizes a definition that is no recursive group, so a recursion through one is graded as fresh until that stage lands.

## Verification

`cargo xtask workspace lint` and `cargo xtask format` after each stage, with the stage's tests by name, `cargo xtask workspace test` and `cargo xboard report`, since both change what the trusted base admits. A soundness hunt follows stage 2 before this specification retires, its charter the premise this specification adds: that the path a summary records is the path the call reaches.

## Retirement

Once both stages have landed, the totality decision states that a call is read off a path and a record handed at a summarized parameter is graded field by field, `curios-cert`'s README its call recording, and nothing references this specification, it is deleted.
