//! The subsumption relation as a surface program reaches it: cumulative codomains, invariant domains.
//!
//! The rule is `documentation/design/theory/subsumption-is-a-relation-not-a-traversal-order.md`. Checking a λ against a Π pushes the comparison to the leaves, where a head-only rule suffices and no Π being subsumed is formed; passing a *name* forms it, which is what these compile.
//!
//! Both checkers decide the same relation, and `curios_cert`'s `kernel::infer::sort_tests` puts the same two propositions to the kernel under these names. Rename both or neither.

use {
    super::test_support::*,
    crate::tests::{error, run, typecheck},
};

// The direction the language needs. `Bool/Holds` is `(b : Bool) -> Prop`, handed to a slot wanting `(Bool) -> Type`; the corpus fixture `/big_nat`'s `canonical_of_is_trimmed` passes it to `Eq/subst` exactly so.
#[test]
fn a_function_types_codomain_is_cumulative() {
    let source = r#"
        use /std/{Bool, print};
        use /std/Bool/{True, Holds};

        let apply(motive: (Bool) -> Type, b: Bool) -> Type =
            motive(b);

        let witnessed: apply(Holds, true) =
            True/qed();

        let _ = witnessed;
        print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// The other half of the same fork, and the one that can be wrong in the admitting direction: a covariant domain would hand a function that takes only propositions a type. Asserted separately from the codomain fixture because invariance is a conjunction of refusals — a relation drifted to covariance still refuses the other direction.
#[test]
fn a_function_types_domain_is_invariant() {
    let wider_domain = r#"
        use /std/{print};
        use /std/Bool/{Holds};

        let takes(motive: (Prop) -> Type) -> Type =
            motive(Holds(true));

        let given(t: Type) -> Type =
            t;

        let _bad: Type = takes(given);
        print("")
        "#;
    let report = error(wider_domain);
    assert!(
        report.contains("type mismatch")
            && report.contains("inferred: (t: Type) -> Type")
            && report.contains("expected: (Prop) -> Type"),
        "{report}"
    );

    let narrower_domain = r#"
        use /std/{Bool, print};

        let takes(motive: (Type) -> Type) -> Type =
            motive(Type);

        let given(p: Prop) -> Type =
            p;

        let _bad: Type = takes(given);
        print("")
        "#;
    let report = error(narrower_domain);
    assert!(report.contains("type mismatch"), "{report}");
}

// The control. A relation that had stopped descending into function types would satisfy neither fixture above while the head rule stayed green, and a relation that refused every function type would satisfy the domain fixture alone.
#[test]
fn the_head_rules_still_decide_a_bare_sort() {
    let source = r#"
        use /std/{print};
        use /std/Bool/{Holds};

        let holds: Type =
            Holds(true);

        let _ = holds;
        print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// A level entailment rather than a conversion. A member of an `and` group used at a type one level up needs `1 ≤ u` of the group's own instance, a group being monomorphic in its universes, and the elaborator records exactly that in the scheme it generalizes; the kernel's entailment reaches the hypotheses before it decides a level's *constant* part structurally, since a parameter ranges over every natural when nothing is assumed and a hypothesis is what puts a floor under it.
#[test]
fn a_group_member_is_used_a_level_up_by_its_sibling() {
    assert_eq!(
        typecheck(A_GROUP_MEMBER_IS_USED_A_LEVEL_UP_BY_ITS_SIBLING),
        Ok(())
    );
}

#[test]
fn the_same_pair_declared_apart_certifies() {
    assert_eq!(typecheck(THE_SAME_PAIR_DECLARED_APART_CERTIFIES), Ok(()));
}

// A rung that waits does not decide the codomain. The domain `Bool` against `dom(c)` waits on `c`, which the witness after it solves; walked as an equation once the rung blocked, the pair compared `Prop` with `Type`.
#[test]
fn a_codomain_is_cumulative_past_a_rung_that_waits() {
    let source = r#"
        use /std/{Bool, Nat, Eq, print};

        let is(b: Bool) -> Prop = Bool/Holds(b);
        let dom(c: Bool) -> Type = match c | true => Bool | false => Nat end;
        let app(@c: Bool, f: (dom(c)) -> Type, w: Eq()(c, true)) -> Nat = 0;

        let inferred: Nat = app(is, Eq/refl());
        let written: Nat = app(@true, is, Eq/refl());

        let _ = inferred + written;
        print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// A subsumption that waits resumes as one. `Prop` against `sort_of(c)` waits on `c`; resumed as an equation, the retry compared `Prop` with `Type`.
#[test]
fn a_subsumption_that_waits_resumes_as_one() {
    let source = r#"
        use /std/{Bool, Nat, Eq, print};

        let is(b: Bool) -> Prop = Bool/Holds(b);
        let sort_of(b: Bool) -> Type = match b | true => Type | false => Prop end;
        let pick(@c: Bool, f: (Bool) -> sort_of(c), w: Eq()(c, true)) -> Nat = 0;

        let inferred: Nat = pick(is, Eq/refl());
        let written: Nat = pick(@true, is, Eq/refl());

        let _ = inferred + written;
        print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// A rung that waits on a family applied to its binder: the first argument's domain waits on `F`, which the second solves by abstraction, and the codomain is `Prop` below `Type` meanwhile.
#[test]
fn a_rung_waits_on_a_family_its_sibling_solves() {
    let source = r#"
        use /std/{Bool, Nat, print};

        let is(b: Bool) -> Prop = Bool/Holds(b);
        let app(@F: (Nat) -> Type, f: (F(0)) -> Type, g: (n: Nat) -> F(n)) -> Nat = 0;

        let inferred: Nat = app(is, (n) => true);

        let _ = inferred;
        print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// What is no sort and no function type is the equation it was, the spine rule with it: `Handler(true)` against `Handler(c)` solves `c` by the two calls' spines, a function type against a type that waits on `c`. And a sort reached only by forcing a recursive call is read as the kernel reads it.
#[test]
fn a_pair_that_is_no_sort_and_no_function_type_is_the_equation_it_was() {
    let source = r#"
        use /std/{Bool, Nat, print};

        let Handler(c: Bool) -> Type = match c | true => (Nat) -> Nat | false => (Bool) -> Bool end;
        let make(@c: Bool, h: Handler(c)) -> Nat = 0;
        let given: Handler(true) = (n) => n;

        let tower(n: Nat) -> Type = match n | 0 => Type | m + 1 => tower(m) end;
        let held: tower(2) = Bool/Holds(true);

        let _ = make(given);
        let _ = held;
        print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// A sort an implicit is left to is no first argument's: `S` is a level of its own over both, in either order, in a call, a list literal and a match's arms.
#[test]
fn a_sort_left_to_an_implicit_is_held_by_no_argument() {
    let source = r#"
        use /std/{Bool, Nat, List, print};

        let both(@S: Type, f: (Bool) -> S, g: (Bool) -> S) -> Nat = 0;
        let fam(b: Bool) -> Type = Nat;
        let fam1(b: Bool) -> Type = Type;

        let high_first: Nat = both(fam1, fam);
        let low_first: Nat = both(fam, fam1);

        let listed_high_first = [Type, Nat];
        let listed_low_first = [Nat, Type];

        let arms(b: Bool) -> Nat =
            let high = match b | true => Type | false => Nat end;
            let low = match b | true => Nat | false => Type end;
            0;

        let _ = high_first + low_first + arms(true);
        let _ = listed_high_first;
        let _ = listed_low_first;
        print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// A declaration keeps no argument order in its scheme: `outer` bounds its caller's level by nothing `both` was not asked, so a family two levels up passes whichever way the body wrote the call.
#[test]
fn a_declaration_keeps_no_argument_order_in_its_scheme() {
    let source = r#"
        use /std/{Bool, Nat, Eq, print};

        let both(@S: Type, f: (Bool) -> S, g: (Bool) -> S) -> Nat = 0;
        let fam1(b: Bool) -> Type = Type;
        let fam2(b: Bool) -> Type = {K: Type, same: Eq()(K, Type)};

        let family_last(f: (Bool) -> Type) -> Nat = both(fam1, f);
        let family_first(f: (Bool) -> Type) -> Nat = both(f, fam1);
        let written(f: (Bool) -> Type) -> Nat = both(@Type, fam1, f);

        let _ = family_last(fam2) + family_first(fam2) + written(fam2);
        print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// A proposition met first decides nothing a type met after it would have decided: in a call, a two-group call, a list literal, a match's arms, and against the expectation.
#[test]
fn a_proposition_met_first_holds_no_later_type_to_it() {
    let source = r#"
        use /std/{Bool, Nat, List, Option, Eq, print};

        let both(@S: Type, f: (Bool) -> S, g: (Bool) -> S) -> Nat = 0;
        let is(b: Bool) -> Prop = Bool/Holds(b);
        let fam(b: Bool) -> Type = Nat;

        let prop_first: Nat = both(is, fam);
        let type_first: Nat = both(fam, is);

        let grouped_prop_first: Prop = Eq()(Bool/Holds(true), Nat);
        let grouped_type_first: Prop = Eq()(Nat, Bool/Holds(true));

        let listed_prop_first = [Bool/Holds(true), Nat];
        let listed_type_first = [Nat, Bool/Holds(true)];

        let expected: Option(Type) = Option/some(Bool/Holds(true));

        let arms(b: Bool) -> Nat =
            let prop_arm_first = match b | true => Nat | false => Bool/Holds(b) end;
            let type_arm_first = match b | true => Bool/Holds(b) | false => Nat end;
            0;

        let _ = prop_first + type_first + arms(true);
        let _ = listed_prop_first;
        let _ = listed_type_first;
        let _ = expected;
        print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// Where nothing raises it, a proposition met first is the proposition: a call whose every argument is one answers at `Prop`, and stands where a `Prop` is wanted.
#[test]
fn a_proposition_nothing_raises_stays_a_proposition() {
    let source = r#"
        use /std/{Bool, Nat, print};

        let pick(@S: Type, x: S, y: S) -> S = x;
        let id(@A: Type, a: A) -> A = a;

        let both_props: Prop = pick(Bool/Holds(true), Bool/Holds(false));
        let through: Prop = id(Bool/Holds(true));
        let inferred = pick(Bool/Holds(true), Bool/Holds(false));
        let held: Prop = inferred;

        let _ = 0;
        print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// Each term awaits what it minted and no longer. A structure literal reads its fields past a proposition a parameter or a hidden field it left out met first; a call inside a list literal settles its own implicit before the list reads its next element; and a lambda argument that needs the implicit's sort is checked once the call has settled it.
#[test]
fn a_term_awaits_what_it_minted_until_its_parts_are_read() {
    let source = r#"
        use /std/{Bool, Nat, List, print};

        struct Twin(S: Type): pub Type { a: S, b: S }
        struct Pack: pub Type { @S: Type, a: S, b: S }
        let id(@A: Type, a: A) -> A = a;
        let through(@S: Type, x: S, k: (S) -> Nat) -> Nat = k(x);

        let twin_prop_first = Twin { a = Bool/Holds(true), b = Nat };
        let twin_type_first = Twin { a = Nat, b = Bool/Holds(true) };
        let twin_of_props: Twin(Prop) = Twin { a = Bool/Holds(true), b = Bool/Holds(false) };
        let pack_prop_first = Pack { a = Bool/Holds(true), b = Nat };
        let nested_first = [id(Bool/Holds(true)), Nat];
        let nested_last = [Nat, id(Bool/Holds(true))];

        let _ = twin_prop_first;
        let _ = twin_type_first;
        let _ = twin_of_props;
        let _ = pack_prop_first;
        let _ = nested_first;
        let _ = nested_last;
        let _ = through(Bool/Holds(true), (p) => 0);
        print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// What waited decides nothing the parts did not: a `Type` among them makes the implicit a `Type`, so the call does not stand where the proposition's sort is wanted invariantly, and a part that is no sort at all is refused as it was.
#[test]
fn a_metavariable_that_waited_is_what_its_parts_made_it() {
    let invariant = r#"
        use /std/{Bool, Nat, Option, print};

        let pick(@S: Type, x: S, y: S) -> Option(S) = Option/some(x);
        let _bad: Option(Prop) = pick(Bool/Holds(true), Nat);
        print("")
        "#;
    let report = error(invariant);
    assert!(
        report.contains("type mismatch")
            && report.contains("inferred: Option(Type)")
            && report.contains("expected: Option(Prop)"),
        "{report}"
    );

    let no_sort = r#"
        use /std/{Bool, Nat, print};

        let same(@S: Type, x: S, y: S) -> Nat = 0;
        let _bad: Nat = same(Bool/Holds(true), 5);
        print("")
        "#;
    let report = error(no_sort);
    assert!(report.contains("type mismatch"), "{report}");
}
