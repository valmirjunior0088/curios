//! Where an occurrence stands in an index rather than a payload.
//!
//! An inductive is not uniform in its indices, so nothing composes through one and the walk leaves them opaque. What these pin is the boundary that leaves: which index positions still admit a recursive occurrence, and which are not reachable at all.

use {super::test_support::*, crate::test_support::*, curios_elab::Error};

// A family that recurses at an index computed from its own payload. Indices are walked opaquely — an inductive is not uniform in them — so what has to survive is the *payload* occurrence, not the index.
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
// Nothing is lost by skipping them: a declaration cannot reach itself there, by its own name or by a member of its group's (below). `induct Foo : (x : Foo) -> Type` does not elaborate, because `x : Foo` requires `Foo` to already be a type and it is a family until applied to the very index being declared.
#[test]
fn recursion_beside_a_propositional_equality_over_the_declaration_is_admitted() {
    let source = r#"
        use /std/{Eq};

        induct Wit : pub Type
        | base()
        | tied(a : Wit, b : Wit, p : Eq()(a, b))
        end

        match Wit/tied(Wit/base(), Wit/base(), Eq/refl())
        | base() => /std/print("no")
        | tied(_, _, _) => /std/print("equated")
        end
        "#;
    assert_eq!(run(source), b"equated");
}

// The skip above is a *reachability* claim, and this pins its first half. `Split::of` walks constructor payloads and struct fields and nothing else; if an index binder type could name the declaration being declared, a negative occurrence there would go unseen. It cannot, and the reason is not positivity: a declaration is in scope from its own signature on and never inside it, so the elaborator refuses the name before the positivity pass runs at all, and were it in scope kinding would refuse it, `x : Foo` needing `Foo` to be a type where `Foo` is a family until it is applied to the very index this declaration is introducing. That is what makes skipping the position safe rather than lucky.
#[test]
fn a_self_reference_in_an_index_binder_type_does_not_elaborate() {
    refused(
        r#"
        induct Foo : (x : Foo) -> pub Type
        end

        /std/print("unreachable")
        "#,
    )
    .by_elaborator_in(
        "/Foo",
        |error| matches!(error, Error::UnboundVariable { term, .. } if pinned(term) == "/Foo"),
    );
}

// The reachability claim's second half, held by a rule rather than by kinding: a second declaration of the group is well-kinded in an index domain, `A` being a type already, so the position the walk skips could name the group, and a family's parameters and indices name nothing its group declares. `S`, indexed by `W` beside a `W` whose `wrap` holds an `S`, is refused, as is `Ty`, whose parameter is a `Ctx` beside a `Ctx` whose `ext` holds a `Ty`, and `B`, whose index domain `(A) -> False` carries a negative occurrence of `A` beside an `A` whose `mk` holds a `B`, the occurrence the payload walk sees only once `mk` stores its witness. A family indexed by a family that holds it is induction-induction, which no decision admits. `curios-cert`'s `recheck::positivity_tests` puts the kernel to the same rule under this name, with the pair declared apart too.
#[test]
fn a_family_is_indexed_by_nothing_that_holds_it() {
    for (source, declaration, holder) in [
        (
            r#"
            induct W: pub Prop
            | base()
            | wrap(x: W, s: S(x))
            and S: (W) -> pub Prop
            | mk(v: W): (v)
            end

            /std/print("unreachable")
            "#,
            "/S",
            "/W",
        ),
        (
            r#"
            induct Ctx: pub Type
            | empty()
            | ext(g: Ctx, a: Ty(g))
            and Ty(g: Ctx): pub Type
            | unit()
            end

            /std/print("unreachable")
            "#,
            "/Ty",
            "/Ctx",
        ),
        (
            r#"
            use /std/Bool/{False};

            induct A: pub Type
            | mk(@f: (A) -> False, b: B(f))
            and B: (f: (A) -> False) -> pub Type
            | unit(@g: (A) -> False): (g)
            end

            /std/print("unreachable")
            "#,
            "/B",
            "/A",
        ),
    ] {
        refused(source).by_elaborator_in(declaration, |error| {
            matches!(error, Error::IndexedByItsGroup { name, member } if *name == global(declaration) && *member == global(holder))
        });
    }
}

// The other skipped index position, and the one that stays admitted: a constructor's index *targets*. `labelled` ignores the telescope's terminal, so the recursive occurrence in `c`'s target is never walked and `Bad`'s diagonal comes out `Unused`.
//
// That is sound for the same reason as the binder types, and it is worth stating because the declaration looks alarming: an index is not stored. `c()` carries nothing, and eliminating a `Bad(t)` refines the motive's `t` to the target — which makes `(Bad(Nat)) -> Bool` a *goal* the arm must discharge, never a value the match hands back. The declaration is consistent, and `Bad(Nat)` is simply an empty type, no constructor targeting that index.
//
// It is here as the counterweight to the two rejections above: the skip is not a hole to be plugged by walking index positions, and a change that started walking them would reject this.
#[test]
fn a_recursive_occurrence_in_an_index_target_is_admitted() {
    let source = r#"
        use /std/{Nat, Bool};
        
        induct Bad : (t : Type) -> pub Type
        | c() : ((Bad(Nat)) -> Bool)
        end

        /std/print("indexed")
        "#;
    assert_eq!(run(source), b"indexed");
}
