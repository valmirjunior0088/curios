//! The subsumption relation as a surface program reaches it: cumulative codomains, invariant domains, and no proposition where a type is wanted.
//!
//! The rule is `documentation/design/theory/subsumption-is-a-relation-not-a-traversal-order.md`. Checking a λ against a Π pushes the comparison to the leaves, where a head-only rule suffices and no Π being subsumed is formed; passing a *name* forms it, which is what these compile.
//!
//! Both checkers decide the same relation, and `curios_cert`'s `kernel::infer::sort_tests` puts the same two propositions to the kernel under these names. Rename both or neither.

use {
    super::test_support::*,
    crate::tests::{error, run, typecheck},
};

// The direction the language needs. Two predicates, one a level above the other, are handed to slots that share one sort: under the binder each codomain is below it, whichever is read first. A predicate is no family of types, though: `Bool/Holds` is `(b : Bool) -> Prop`, and a slot wanting `(Bool) -> Type` refuses it.
#[test]
fn a_function_types_codomain_is_cumulative() {
    let source = r#"
        use /std/{Bool, print};
        use /std/Bool/{Holds};

        let both(@S: Type, f: (Bool) -> S, g: (Bool) -> S) -> Bool = true;
        let is(b: Bool) -> Prop = Holds(b);
        let over_types(b: Bool) -> Prop = (A: Type) -> Holds(b);

        let small_first: Bool = both(is, over_types);
        let large_first: Bool = both(over_types, is);

        let _ = small_first && large_first;
        print("ok")
        "#;

    assert_eq!(run(source), b"ok");

    let a_predicate_as_a_family = r#"
        use /std/{Bool, print};
        use /std/Bool/{True, Holds};

        let apply(motive: (Bool) -> Type, b: Bool) -> Type =
            motive(b);

        let witnessed: apply(Holds, true) =
            True/qed();

        let _ = witnessed;
        print("")
        "#;
    let report = error(a_predicate_as_a_family);
    assert!(
        report.contains("type mismatch")
            && report.contains("inferred: (b: Bool) -> Prop")
            && report.contains("expected: (Bool) -> Type"),
        "{report}"
    );
}

// The other half of the same fork: a domain is compared by conversion, so a function over types is none over propositions, and the other way round. Asserted separately from the codomain fixture because invariance is a conjunction of refusals.
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
            Bool;

        let _bad: Type = takes(given);
        print("")
        "#;
    let report = error(narrower_domain);
    assert!(report.contains("type mismatch"), "{report}");
}

// The control. A relation that had stopped descending into function types would satisfy neither fixture above while the head rule stayed green, and a relation that refused every function type would satisfy the domain fixture alone. At the head a proposition is held at `Prop` and a type at `Type`, and a proposition stands where no `Type` is wanted.
#[test]
fn the_head_rules_still_decide_a_bare_sort() {
    let source = r#"
        use /std/{Nat, print};
        use /std/Bool/{Holds};

        let holds: Prop =
            Holds(true);
        let number: Type =
            Nat;

        let _ = holds;
        let _ = number;
        print("ok")
        "#;

    assert_eq!(run(source), b"ok");

    let a_proposition_at_a_type = r#"
        use /std/{print};
        use /std/Bool/{Holds};

        let holds: Type =
            Holds(true);

        let _ = holds;
        print("")
        "#;
    let report = error(a_proposition_at_a_type);
    assert!(
        report.contains("type mismatch")
            && report.contains("inferred: Prop")
            && report.contains("expected: Type"),
        "{report}"
    );
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

// A rung that waits does not decide the codomain. The domain `Bool` against `dom(c)` waits on `c`, which the witness after it solves, and each codomain is below the sort the two slots share meanwhile; walked as an equation once the rung blocked, the smaller predicate read second is refused at the level the larger one left.
#[test]
fn a_codomain_is_cumulative_past_a_rung_that_waits() {
    let source = r#"
        use /std/{Bool, Nat, Eq, print};

        let is(b: Bool) -> Prop = Bool/Holds(b);
        let over_types(b: Bool) -> Prop = (A: Type) -> Bool/Holds(b);
        let dom(c: Bool) -> Type = match c | true => Bool | false => Nat end;
        let app(@c: Bool, @S: Type, f: (dom(c)) -> S, g: (dom(c)) -> S, w: Eq()(c, true)) -> Nat = 0;

        let large_first: Nat = app(over_types, is, Eq/refl());
        let small_first: Nat = app(is, over_types, Eq/refl());
        let written: Nat = app(@true, over_types, is, Eq/refl());

        let _ = large_first + small_first + written;
        print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// A rung that waits on a family applied to its binder: the first argument's domain waits on `F`, which the last solves by abstraction, and each codomain is below the sort the two slots share meanwhile.
#[test]
fn a_rung_waits_on_a_family_its_sibling_solves() {
    let source = r#"
        use /std/{Bool, Nat, print};

        let is(b: Bool) -> Prop = Bool/Holds(b);
        let over_types(b: Bool) -> Prop = (A: Type) -> Bool/Holds(b);
        let app(@F: (Nat) -> Type, @S: Type, f: (F(0)) -> S, h: (F(0)) -> S, g: (n: Nat) -> F(n)) -> Nat = 0;

        let large_first: Nat = app(over_types, is, (n) => true);
        let small_first: Nat = app(is, over_types, (n) => true);

        let _ = large_first + small_first;
        print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// What is no sort and no function type is the equation it was, the spine rule with it: `Handler(true)` against `Handler(c)` solves `c` by the two calls' spines, a function type against a type that waits on `c`. And a sort reached only by forcing a recursive call is read as the kernel reads it, a type held at the `Type` the call comes to.
#[test]
fn a_pair_that_is_no_sort_and_no_function_type_is_the_equation_it_was() {
    let source = r#"
        use /std/{Bool, Nat, print};

        let Handler(c: Bool) -> Type = match c | true => (Nat) -> Nat | false => (Bool) -> Bool end;
        let make(@c: Bool, h: Handler(c)) -> Nat = 0;
        let given: Handler(true) = (n) => n;

        let tower(n: Nat) -> Type = match n | 0 => Type | m + 1 => tower(m) end;
        let held: tower(2) = Nat;

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

// No proposition stands where a type is wanted, so one implicit holds no proposition beside a type, whichever is read first: in a call, a two-group call, a list literal, a match's arms, a structure literal's parameter and its hidden field, a call inside a list literal, and against the expectation.
#[test]
fn a_proposition_and_a_type_under_one_implicit_are_refused_in_either_order() {
    let declarations = r#"
        use /std/{Bool, Nat, List, Option, Eq, print};

        struct Twin(S: Type): pub Type { a: S, b: S }
        struct Pack: pub Type { @S: Type, a: S, b: S }
        let both(@S: Type, f: (Bool) -> S, g: (Bool) -> S) -> Nat = 0;
        let pick(@S: Type, x: S, y: S) -> Option(S) = Option/some(x);
        let id(@A: Type, a: A) -> A = a;
        let is(b: Bool) -> Prop = Bool/Holds(b);
        let fam(b: Bool) -> Type = Nat;
        "#;

    for mixed in [
        ": Nat = both(is, fam)",
        ": Nat = both(fam, is)",
        ": Prop = Eq()(Bool/Holds(true), Nat)",
        ": Prop = Eq()(Nat, Bool/Holds(true))",
        " = [Bool/Holds(true), Nat]",
        " = [Nat, Bool/Holds(true)]",
        ": Option(Type) = Option/some(Bool/Holds(true))",
        "(b: Bool) -> Nat = let arm = match b | true => Nat | false => Bool/Holds(b) end; 0",
        "(b: Bool) -> Nat = let arm = match b | true => Bool/Holds(b) | false => Nat end; 0",
        " = Twin { a = Bool/Holds(true), b = Nat }",
        " = Twin { a = Nat, b = Bool/Holds(true) }",
        " = Pack { a = Bool/Holds(true), b = Nat }",
        " = [id(Bool/Holds(true)), Nat]",
        " = [Nat, id(Bool/Holds(true))]",
        ": Option(Prop) = pick(Bool/Holds(true), Nat)",
    ] {
        let report = error(&format!("{declarations}\nlet _mixed{mixed};\nprint(\"\")"));
        assert!(
            report.contains("type mismatch") && report.contains("Prop") && report.contains("Type"),
            "{mixed}: {report}"
        );
    }
}

// A proposition an implicit meets makes it a sort of propositions: a call whose every argument is one answers at `Prop` and stands where a `Prop` is wanted, a structure literal's parameter and a list literal's element are that sort, and a lambda argument that needs the implicit's sort is checked once the call has it.
#[test]
fn propositions_under_one_implicit_make_it_a_sort_of_propositions() {
    let source = r#"
        use /std/{Bool, Nat, List, print};

        struct Twin(S: Type): pub Type { a: S, b: S }
        let pick(@S: Type, x: S, y: S) -> S = x;
        let id(@A: Type, a: A) -> A = a;
        let through(@S: Type, x: S, k: (S) -> Nat) -> Nat = k(x);

        let both_props: Prop = pick(Bool/Holds(true), Bool/Holds(false));
        let passed: Prop = id(Bool/Holds(true));
        let inferred = pick(Bool/Holds(true), Bool/Holds(false));
        let held: Prop = inferred;
        let twin_of_props: Twin(Prop) = Twin { a = Bool/Holds(true), b = Bool/Holds(false) };
        let listed: List(Prop) = [id(Bool/Holds(true)), Bool/Holds(false)];

        let _ = twin_of_props;
        let _ = listed;
        let _ = through(Bool/Holds(true), (p) => 0);
        print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// A part that is no sort at all is refused as it was.
#[test]
fn a_sort_and_a_value_under_one_implicit_are_refused() {
    let no_sort = r#"
        use /std/{Bool, Nat, print};

        let same(@S: Type, x: S, y: S) -> Nat = 0;
        let _bad: Nat = same(Bool/Holds(true), 5);
        print("")
        "#;
    let report = error(no_sort);
    assert!(report.contains("type mismatch"), "{report}");
}

// Whether a type is a proposition is one fact of it under every case: a result declared at a sort its caller states is one sort, so a match that answers a proposition in one case and a type in another is refused, as it is at `Type`.
#[test]
fn a_type_that_is_a_proposition_in_one_case_is_at_no_sort_left_to_a_caller() {
    let source = r#"
        use /std/{Nat, Bool};

        induct Two: pub Prop
        | a()
        | b()
        end

        let F(c: Bool) -> Sort = match c | true => Two | false => Nat end;

        /std/print("")
        "#;

    rejected_by(
        source,
        "this decides a `Sort` its declaration leaves to a caller as `Prop`",
    );
}

// A definition generic in a sort stands at `Type` at one use, where matching a proposition of two constructors into it would tell its proofs apart. The elaborator refuses the definition for deciding its sort; the kernel, handed it all the same, refuses it at the assignment that states `Type`.
#[test]
fn a_proposition_is_not_matched_into_a_sort_left_to_a_caller() {
    let source = r#"
        induct Two: pub Prop
        | a()
        | b()
        end

        let pick(@A: Sort, t: Two, x: A, y: A) -> A = match t | a() => x | b() => y end;

        /std/print("")
        "#;

    rejected_by(source, "it can only be `Prop`");
}
