//! Where an occurrence stands in an index rather than a payload.
//!
//! An inductive is not uniform in its indices, so nothing composes through one and the walk leaves them opaque. What these pin is the boundary that leaves: which index positions still admit a recursive occurrence, and which are not reachable at all.

use {super::test_support::*, crate::tests::run};

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
