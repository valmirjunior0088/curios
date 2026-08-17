//! End-to-end coverage for strict positivity modulo polarity.
//!
//! The check exists to make `induct` and `struct` sound: an inductive declaration claims its functor has an initial algebra, and without the gate `induct Bad | c(f : (Bad) -> False) end` inhabits `False`. The lattice itself is unit-tested in `curios-analysis/src/positivity/tests.rs`; these check what a *user* can observe.
//!
//! Every shape here is one the standard library already relies on. The prelude build exercises them through the from-scratch elaboration path, so it fails loudly on a regression; these run the same shapes through the prelude-replay path a user program actually takes, where the analysis sees only the user suffix and reads the prelude's polarity vectors back from the archive.

use {
    super::{run, run_text},
    curios_runtime::MockHost,
};

fn rejected(source: &str) {
    let (system, _io) = MockHost::builder().build();
    assert!(
        run_text(source, system).is_err(),
        "expected the declaration to be rejected",
    );
}

/// [`rejected`], naming the rule that must do the rejecting.
///
/// A bare `is_err` passes on a typo in the fixture, which is worth guarding wherever the shape under test is one a future relaxation of `occurrences` could plausibly start admitting.
fn rejected_by(source: &str, diagnostic: &str) {
    let (system, _io) = MockHost::builder().build();
    let error = run_text(source, system).expect_err("expected the declaration to be rejected");
    assert!(
        error.contains(diagnostic),
        "rejected, but not by '{diagnostic}':\n{error}",
    );
}

// A description of an `X` is the delayed `X` its thunk erasure makes it, so `X` under `Io` stays strictly positive — the same reading as `List`, and for a stronger reason: `bind` never hands back the result except inside another `Io`, and there is no eliminator that does. This is the rule a suspension whose continuation is computed by performing an effect rests on.
#[test]
fn an_io_payload_is_a_strictly_positive_occurrence() {
    let source = r#"
        use /std/{Nat, Io};

        induct Step(A : Type) : pub Type
        | done(A)
        | more(next : Io(Step(A)))
        end

        let machine : Step(Nat) = Step/more(Io/pure(Step/done(7)));

        match machine
        | done(_) => /std/print("no")
        | more(_) => /std/print("suspended")
        end
        "#;
    assert_eq!(run(source), b"suspended");
}

// The rule most likely to be implemented wrongly, and the one that would take the whole concurrency stack with it. A nullary `() -> X` is an *empty* parameter list, so nothing flips and `X` stays strictly positive — a zero-argument function is a thunk of its result. Read as "an occurrence under an arrow is negative", this rejects `/std/Async`.
#[test]
fn a_nullary_arrow_codomain_is_a_strictly_positive_payload() {
    let source = r#"
        use /std/{Nat};

        induct Susp(A : Type) : pub Type
        | now(A)
        | later(() -> Susp(A))
        end

        let delayed : Susp(Nat) = Susp/later(() => Susp/now(7));

        match delayed
        | now(_) => /std/print("no")
        | later(_) => /std/print("thunked")
        end
        "#;
    assert_eq!(run(source), b"thunked");
}

// `/std/Pause` and `/std/Async` in miniature: two declarations in one `and` block, each reaching the other, with the recursive occurrence behind a thunk. The group boundary lives on `Item::Rec` and not on the registry entry, so this passes only because the occurrence relation is closed transitively rather than by asking who shares a declaration block.
#[test]
fn a_mutually_recursive_group_reaching_itself_through_a_thunk_is_admitted() {
    let source = r#"
        induct Step : Type
        | halt()
        | again(next : () -> Machine)
        and Machine : Type
        | done()
        | more(step : Step, rest : () -> Machine)
        end

        match Machine/more(Step/halt(), () => Machine/done())
        | done() => /std/print("no")
        | more(_, _) => /std/print("mutual")
        end
        "#;
    assert_eq!(run(source), b"mutual");
}

// `Json/arr(List(Json))`: a list is a finite product of its element, so the intrinsic is covariant and a strict occurrence stays strict. Without polarity on intrinsics this shape is rejected outright.
#[test]
fn recursion_through_a_covariant_intrinsic_is_admitted() {
    let source = r#"
        use /std/{List};

        induct Tree : pub Type
        | leaf()
        | node(kids : List(Tree))
        end

        let branch : Tree = Tree/node([Tree/leaf()]);

        match branch
        | leaf() => /std/print("no")
        | node(_) => /std/print("listed")
        end
        "#;
    assert_eq!(run(source), b"listed");
}

// `Json/obj(List({Str, Json}))`: polarity has to travel through an anonymous tuple type as well as the intrinsic, since a product is as positive as its factors.
#[test]
fn recursion_through_an_anonymous_tuple_is_admitted() {
    let source = r#"
        use /std/{List, Nat};

        induct Doc : pub Type
        | atom(Nat)
        | fields(List({Nat, Doc}))
        end

        match Doc/fields([])
        | atom(_) => /std/print("no")
        | fields(_) => /std/print("tupled")
        end
        "#;
    assert_eq!(run(source), b"tupled");
}

// `/std/Toml`, whose recursion travels `Toml → Map(Toml) → struct field → Option → Node`. This is the shape that makes "modulo polarity" load-bearing on day one rather than a later refinement: a check that only recognizes recursive occurrences in immediate payload positions rejects it.
#[test]
fn recursion_through_a_struct_into_a_parameterized_inductive_is_admitted() {
    let source = r#"
        use /std/{Nat, Option};

        induct Node(V : Type) : pub Type
        | leaf(key : Nat, value : V)
        | fork(zero : Node(V), one : Node(V))
        end

        struct Table(V : Type) : pub Type {
            size : Nat,
            root : Option(Node(V)),
        }

        induct Value : pub Type
        | num(Nat)
        | table(Table(Value))
        end

        match Value/table(Table { size = 0, root = Option/none() })
        | num(_) => /std/print("no")
        | table(_) => /std/print("three hops")
        end
        "#;
    assert_eq!(run(source), b"three hops");
}

// `Node(V)` mentions itself, so its own parameter vector is an input to computing that vector. A single ordered pass reads `V` as `Unused` and then composes `Value → Table → Node` through a parameter it wrongly believes is never inspected; only running the two fixpoints to stability gets `Strict`.
#[test]
fn a_self_mentioning_parameter_needs_the_fixpoint_to_settle() {
    let source = r#"
        use /std/{Nat};

        induct Rose(A : Type) : pub Type
        | tip(A)
        | fork(left : Rose(A), right : Rose(A))
        end

        induct Holder : pub Type
        | held(Rose(Holder))
        | empty()
        end

        match Holder/held(Rose/tip(@Holder, Holder/empty()))
        | held(_) => /std/print("settled")
        | empty() => /std/print("no")
        end
        "#;
    assert_eq!(run(source), b"settled");
}

// `syn/Str`'s `Utf8`, which recurses at an index computed from its own payload. Indices are walked opaquely — an inductive is not uniform in them — so what has to survive is the *payload* occurrence, not the index.
#[test]
fn an_indexed_family_recursing_at_a_computed_index_is_admitted() {
    let source = r#"
        use /std/{Nat};

        induct Run : (n : Nat) -> pub Type
        | stop() : (0)
        | more(@m : Nat, rest : Run(m)) : (m + 1)
        end

        let two : Run(2) = Run/more(Run/more(Run/stop()));

        match two : (k, r) => /std/Io({})
        | more(@_, _) => /std/print("indexed")
        end
        "#;
    assert_eq!(run(source), b"indexed");
}

// An inductive's *index binder types* describe the family's arity, not its carrier, so they contribute no polarity of their own — `Eq(@A : Type) : (x : A, y : A)` is `Strict` in `A` because `refl(@z : A)` has an `A` payload. Walking `x : A` on top of that costs the vector its precision and rejects this declaration, which is sound.
//
// Nothing is lost by skipping them: a declaration cannot reach itself there. `induct Foo : (x : Foo) -> Type` does not elaborate, because `x : Foo` requires `Foo` to already be a type and it is a family until applied to the very index being declared.
#[test]
fn recursion_beside_a_propositional_equality_over_the_declaration_is_admitted() {
    let source = r#"
        use /std/{Eq};

        induct Wit : pub Type
        | base()
        | tied(a : Wit, b : Wit, p : Eq(a, b))
        end

        match Wit/tied(Wit/base(), Wit/base(), Eq/refl())
        | base() => /std/print("no")
        | tied(_, _, _) => /std/print("equated")
        end
        "#;
    assert_eq!(run(source), b"equated");
}

// The skip above is a *reachability* claim, and this is the half of it that was only ever asserted. `Split::of` walks constructor payloads and struct fields and nothing else; if an index binder type could name the declaration being declared, a negative occurrence there would go unseen. It cannot, and the reason is kinding rather than positivity: `x : Foo` needs `Foo` to be a type, and `Foo` is a family until it is applied to the very index this declaration is introducing. The elaborator refuses it before the positivity pass runs at all, which is what makes skipping the position safe rather than lucky.
#[test]
fn a_self_reference_in_an_index_binder_type_does_not_elaborate() {
    rejected_by(
        r#"
        induct Foo : (x : Foo) -> pub Type
        end

        /std/print("unreachable")
        "#,
        "type mismatch",
    );
}

// The way *around* that kinding refusal, and the reason it closes too. A second declaration can carry the negative occurrence in its index domain — `B : (f : (A) -> False) -> Type` is well-kinded, because `A` is already a type — so the position the walk skips does name a declaration in the group, and the occurrence relation never records the `B → A` edge.
//
// Nothing follows, because an index domain must be *inhabited* to be used. Every constructor of `B` has to state a target of type `(A) -> False`, and the only way to have one is to bind it — at which point it is a payload, and the payload walk is exactly what sees it. The rejection below names `mk(f)`, a stored binder, not the index domain that motivated it.
//
// Probed rather than closed: the analysis already refused this when it was written. What the fixture pins is that the two skipped index positions cannot be reached around, so a future relaxation that made `B`'s payload readable — or that dropped `@f` from `mk` — would have to answer this test rather than silently inherit the skip.
#[test]
fn an_index_domain_over_the_declaration_is_reachable_only_by_storing_its_witness() {
    rejected_by(
        r#"
        use /syn/{False};

        induct A : pub Type
        | mk(@f : (A) -> False, b : B(f))
        and B : (f : (A) -> False) -> pub Type
        | unit(@g : (A) -> False) : (g)
        end

        /std/print("unreachable")
        "#,
        "is not strictly positive",
    );
}

// The other skipped index position, and the one that stays admitted: a constructor's index *targets*. `labelled` ignores the telescope's terminal, so the recursive occurrence in `c`'s target is never walked and `Bad`'s diagonal comes out `Unused`.
//
// That is sound for the same reason as the binder types, and it is worth stating because the declaration looks alarming: an index is not stored. `c()` carries nothing, and eliminating a `Bad(t)` refines the motive's `t` to the target — which makes `(Bad(Nat)) -> False` a *goal* the arm must discharge, never a value the match hands back. The declaration is consistent, and `Bad(Nat)` is simply an empty type, no constructor targeting that index.
//
// It is here as the counterweight to the two rejections above: the skip is not a hole to be plugged by walking index positions, and a change that started walking them would reject this.
#[test]
fn a_recursive_occurrence_in_an_index_target_is_admitted() {
    let source = r#"
        use /std/{Nat};
        use /syn/{False};

        induct Bad : (t : Type) -> pub Type
        | c() : ((Bad(Nat)) -> False)
        end

        /std/print("indexed")
        "#;
    assert_eq!(run(source), b"indexed");
}

// `struct Waker : Type { () -> {} }` inside `/std/Pause`'s `park` payload: a genuine negative position, and sound only because `Waker` mentions neither member of the group. The check must reject on the *path back*, not on the mere presence of a negative occurrence.
#[test]
fn a_negative_occurrence_of_an_unrelated_type_is_admitted() {
    let source = r#"
        struct Waker : pub Type { () -> {} }

        induct Event : pub Type
        | idle()
        | park((Waker) -> {})
        end

        match Event/idle()
        | idle() => /std/print("unrelated")
        | park(_) => /std/print("no")
        end
        "#;
    assert_eq!(run(source), b"unrelated");
}

// A declaration reaching itself through a *type-former parameter* is refused, and this pins that it is refused rather than assumed.
//
// `curios-analysis/src/positivity.rs` names this obligation deliberately out of scope: `F` is a binder with no known polarity, so `Mu` cannot be checked from its own body, and discharging it properly needs an inferred per-binder obligation in a side store. Out of scope leaves open which way the analysis falls, and only one way is safe — an unknown former must read as `Mixed`, never as `Strict`. Nothing in the corpus takes a type-former parameter on an `induct`, so nothing exercised the choice, and a later improvement to `occurrences` that taught it to see through `F(Mu(F))` would flip it silently.
//
// What the wrong direction costs is Curry's paradox with no recursion in sight. Admitting `Mu` lets it be instantiated at a negative former — `let Neg(X : Type) -> Type = (X) -> False` — and `Mu(Neg)`'s constructor is then `fix : ((Mu(Neg)) -> False) -> Mu(Neg)`, the negative occurrence this gate exists to forbid, spelled through a parameter instead of directly. `out(m) = match m | fix(f) => f end` and `delta(m) = out(m)(m)` give `delta(Mu/fix(delta)) : False`.
//
// Verified as refused rather than as a closed hole: this run found the rule already holding, and the diagnostic is asserted so the fixture cannot pass on an unrelated failure. Its control is `a_type_former_parameter_not_recursed_through_is_admitted`, which shows the refusal is about self-reference through an unknown former and not a blanket ban on higher-kinded parameters — a ban would take `/std`'s `Monad`-shaped abstractions with it.
#[test]
fn a_declaration_recursing_through_a_type_former_parameter_is_refused() {
    rejected_by(
        r#"
        induct Mu(F : (Type) -> Type) : pub Type
        | fix(F(Mu(F)))
        end

        /std/print("unreachable")
        "#,
        "is not strictly positive",
    );
}

// The control for the fixture above: a type-former parameter is legal, and applying it is legal. What is refused there is the declaration reaching *itself* through a former whose polarity nothing knows — so the diagonal of the occurrence relation is what decides, exactly as the acceptance predicate says. `Apply` never appears inside `F(A)`, so its diagonal stays `Unused` however `F` behaves.
#[test]
fn a_type_former_parameter_not_recursed_through_is_admitted() {
    let source = r#"
        induct Apply(F : (Type) -> Type, A : Type) : pub Type
        | wrap(F(A))
        end

        induct Box(A : Type) : pub Type
        | put(A)
        end

        let boxed : Apply(Box, /std/Nat) = Apply/wrap(Box/put(7));

        match boxed
        | wrap(_) => /std/print("applied")
        end
        "#;
    assert_eq!(run(source), b"applied");
}

// The whole reason the gate exists. `Bad` is not the initial algebra of any functor — the payload is a function *out of* `Bad` — and admitting it hands back an eliminator that inhabits `False` in four lines with no recursion.
#[test]
fn a_negative_occurrence_is_rejected() {
    rejected(
        r#"
        use /syn/{False};

        induct Bad : pub Type
        | c(f : (Bad) -> False)
        end

        /std/print("unreachable")
        "#,
    );
}

// Positive but not strictly positive: two arrows, so the sign flips back. This records the impredicative-`Prop` decision as a test rather than as prose — with an impredicative `Prop` and a universe hierarchy both present, the Coquand–Paulin construction applies, so the merely-positive relaxation other systems allow is not available here.
#[test]
fn a_positive_but_not_strictly_positive_occurrence_is_rejected() {
    rejected(
        r#"
        use /syn/{False};

        induct Bad2 : pub Type
        | c(f : ((Bad2) -> False) -> False)
        end

        /std/print("unreachable")
        "#,
    );
}

// The composition case, and the one a check without polarity vectors would miss entirely: `Sink` is contravariant in its parameter, so `Trap`'s payload puts `Trap` left of an arrow one indirection away. Nothing about `Trap`'s own constructor looks negative — the rejection comes from `Sink`'s vector.
#[test]
fn a_negative_occurrence_borrowed_through_another_declaration_is_rejected() {
    rejected(
        r#"
        use /std/{Nat};

        induct Sink(A : Type) : pub Type
        | drain(f : (A) -> Nat)
        end

        induct Trap : pub Type
        | caught(Sink(Trap))
        end

        /std/print("unreachable")
        "#,
    );
}

// A cycle whose negative step is in the *other* member. Neither declaration is negative on its own inspection, and the group boundary is not on the registry entry, so this is caught only by closing the occurrence relation transitively.
#[test]
fn a_negative_cycle_through_a_mutual_group_is_rejected() {
    rejected(
        r#"
        use /syn/{False};

        induct Left : Type
        | wrap(Right)
        and Right : Type
        | back(f : (Left) -> False)
        end

        /std/print("unreachable")
        "#,
    );
}

// A struct is checked on the same footing as an inductive: it is a nominal record, so a field that consumes the record it belongs to is the same unsoundness wearing different syntax.
#[test]
fn a_negative_struct_field_is_rejected() {
    rejected(
        r#"
        use /syn/{False};

        struct Consume : pub Type {
            run : (Consume) -> False,
        }

        /std/print("unreachable")
        "#,
    );
}

// `Cell` is invariant — it is read *and* written — so nothing recursive may travel through one, even though the occurrence looks like a plain payload.
#[test]
fn recursion_through_an_invariant_intrinsic_is_rejected() {
    rejected(
        r#"
        use /std/{Cell};

        induct Knot : pub Type
        | tie(Cell(Knot))
        end

        /std/print("unreachable")
        "#,
    );
}
