//! The subsumption relation as a surface program reaches it: cumulative codomains, invariant domains, and no proposition where a type is wanted.
//!
//! The rule is `documentation/design/theory/subsumption-is-a-relation-not-a-traversal-order.md`. Checking a λ against a Π pushes the comparison to the leaves, where a head-only rule suffices and no Π being subsumed is formed; passing a *name* forms it, which is what these compile.
//!
//! Both checkers decide the same relation, and `curios_cert`'s `kernel::infer::tests::sort` puts the same two propositions to the kernel under these names. Rename both or neither.

use {super::test_support::*, crate::test_support::*, curios_elab::Error};

// The direction the language needs. Two predicates, one a level above the other, are handed to slots that share one sort: under the binder each codomain is below it, whichever is read first. A predicate is no family of types, though: `Bool/Holds` is `(b : Bool) -> Prop`, and a slot wanting `(Bool) -> Type` refuses it. The refusal's control hands `apply` a family of types.
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

    control(
        r#"
        use /std/{Bool, Nat, print};
        use /std/Bool/{True, Holds};

        let apply(motive: (Bool) -> Type, b: Bool) -> Type =
            motive(b);

        let witnessed: apply((b: Bool) => Nat, true) =
            3;

        let _ = witnessed;
        print("")
        "#,
        b"",
    )
    .refused(&[
        ("apply((b: Bool) => Nat, true) =\n            3;", "apply(Holds, true) =\n            True/qed();"),
    ])
    .by_elaborator_in("/witnessed", |error| {
        matches!(error, Error::TypeMismatch { inferred, expected, .. } if pinned(inferred) == "(b: Bool) -> Prop" && pinned(expected) == "(Bool) -> Type")
    });
}

// The other half of the same fork: a domain is compared by conversion, so a function over types is none over propositions, and the other way round. Asserted separately from the codomain fixture because invariance is a conjunction of refusals. Each refusal's control gives the domain the slot states.
#[test]
fn a_function_types_domain_is_invariant() {
    control(
        r#"
        use /std/{Nat, print};
        use /std/Bool/{Holds};

        let takes(motive: (Prop) -> Type) -> Type =
            motive(Holds(true));

        let given(t: Prop) -> Type =
            Nat;

        let _bad: Type = takes(given);
        print("")
        "#,
        b"",
    )
    .refused(&[
        ("let given(t: Prop) -> Type =\n            Nat;", "let given(t: Type) -> Type =\n            t;"),
    ])
    .by_elaborator_in("/_bad", |error| {
        matches!(error, Error::TypeMismatch { inferred, expected, .. } if pinned(inferred) == "(t: Type) -> Type" && pinned(expected) == "(Prop) -> Type")
    });

    control(
        r#"
        use /std/{Bool, print};

        let takes(motive: (Type) -> Type) -> Type =
            motive(Type);

        let given(p: Type) -> Type =
            Bool;

        let _bad: Type = takes(given);
        print("")
        "#,
        b"",
    )
    .refused(&[("let given(p: Type)", "let given(p: Prop)")])
    .by_elaborator_in("/_bad", |error| matches!(error, Error::TypeMismatch { .. }));
}

// The control. A relation that had stopped descending into function types would satisfy neither fixture above while the head rule stayed green, and a relation that refused every function type would satisfy the domain fixture alone. At the head a proposition is held at `Prop` and a type at `Type`, and a proposition stands where no `Type` is wanted. The refusal's control states the proposition at `Prop`.
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

    control(
        r#"
        use /std/{print};
        use /std/Bool/{Holds};

        let holds: Prop =
            Holds(true);

        let _ = holds;
        print("")
        "#,
        b"",
    )
    .refused(&[
        ("let holds: Prop =", "let holds: Type ="),
    ])
    .by_elaborator_in("/holds", |error| {
        matches!(error, Error::TypeMismatch { inferred, expected, .. } if pinned(inferred) == "Prop" && pinned(expected) == "Type")
    });
}

// A type that is a sort or a function type only under an arm's case is subsumed there as the one the case makes it: under `k(0) = true`, `S(k(0))` is `Type`, so `Bool` stands at it and `Holds(true)` does not, and `H(k(0))` is a function type into `Type`, so a family of small types stands at it and a predicate does not. `curios-cert`'s `kernel::infer::tests::sort` puts the same propositions to the kernel under this name. Each refusal's control is a type, or a family of types, at the arm.
#[test]
fn what_an_arms_case_makes_a_sort_or_a_function_type_is_subsumed_as_one_there() {
    let declarations = r#"
        use /std/{Bool, Nat, print};
        use /std/Bool/{Holds};

        let S(b: Bool) -> Type = match b | true => Type | false => Nat end;
        let H(b: Bool) -> Type = match b | true => (Nat) -> Type | false => Nat end;
        "#;

    let admitted = r#"
        let small(_: Nat) -> Type = Bool;
        let sort(k: (Nat) -> Bool) -> Nat = match k(0) | true => let _: S(k(0)) = Bool; 0 | false => 0 end;
        let family(k: (Nat) -> Bool) -> Nat = match k(0) | true => let _: H(k(0)) = small; 0 | false => 0 end;

        let _ = sort((_) => true) + family((_) => true);
        print("ok")
        "#;
    assert_eq!(run(&format!("{declarations}{admitted}")), b"ok");

    for (at, declared, edits, inferred_as, expected_as) in [
        (
            "/_sort",
            "let _sort(k: (Nat) -> Bool) -> Nat = match k(0) | true => let _: S(k(0)) = Bool; 0 | false => 0 end;",
            [("= Bool; 0", "= Holds(true); 0")],
            "Prop",
            "Type",
        ),
        (
            "/_family",
            "let is(_: Nat) -> Type = Bool;
            let _family(k: (Nat) -> Bool) -> Nat = match k(0) | true => let _: H(k(0)) = is; 0 | false => 0 end;",
            [("let is(_: Nat) -> Type = Bool;", "let is(_: Nat) -> Prop = Holds(true);")],
            "(Nat) -> Prop",
            "(Nat) -> Type",
        ),
    ] {
        control(&format!("{declarations}\n{declared}\nprint(\"\")"), b"").refused(&edits).by_elaborator_in(at, |error| {
            matches!(error, Error::TypeMismatch { inferred, expected, .. } if pinned(inferred) == inferred_as && pinned(expected) == expected_as)
        });
    }
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
        use /std/Bool/{Holds};

        let is(b: Bool) -> Prop = Holds(b);
        let over_types(b: Bool) -> Prop = (A: Type) -> Holds(b);
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
        use /std/Bool/{Holds};

        let is(b: Bool) -> Prop = Holds(b);
        let over_types(b: Bool) -> Prop = (A: Type) -> Holds(b);
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

// No proposition stands where a type is wanted, so one implicit holds no proposition beside a type, whichever is read first: in a call, a two-group call, a list literal, a match's arms, a structure literal's parameter and its hidden field, a call inside a list literal, and against the expectation. Each refusal's control is the same construction over two types or two propositions.
#[test]
fn a_proposition_and_a_type_under_one_implicit_are_refused_in_either_order() {
    let declarations = r#"
        use /std/{Bool, Nat, List, Option, Eq, print};
        use /std/Bool/{Holds};

        struct Twin(S: Type): pub Type { a: S, b: S }
        struct Pack: pub Type { @S: Type, a: S, b: S }
        let both(@S: Type, f: (Bool) -> S, g: (Bool) -> S) -> Nat = 0;
        let pick(@S: Type, x: S, y: S) -> Option(S) = Option/some(x);
        let id(@A: Type, a: A) -> A = a;
        let is(b: Bool) -> Prop = Holds(b);
        let fam(b: Bool) -> Type = Nat;
        "#;

    for (sound, edit, at, inferred_as, expected_as) in [
        (
            ": Nat = both(fam, fam)",
            ("both(fam, fam)", "both(is, fam)"),
            "/_mixed",
            "(b: Bool) -> Type",
            "(Bool) -> Prop",
        ),
        (
            ": Nat = both(fam, fam)",
            ("both(fam, fam)", "both(fam, is)"),
            "/_mixed",
            "(b: Bool) -> Prop",
            "(Bool) -> Type",
        ),
        (
            ": Prop = Eq()(Holds(true), Holds(false))",
            ("Eq()(Holds(true), Holds(false))", "Eq()(Holds(true), Nat)"),
            "/_mixed",
            "Type",
            "Prop",
        ),
        (
            ": Prop = Eq()(Holds(false), Holds(true))",
            ("Eq()(Holds(false), Holds(true))", "Eq()(Nat, Holds(true))"),
            "/_mixed",
            "Prop",
            "Type",
        ),
        (
            " = [Holds(true), Holds(false)]",
            ("[Holds(true), Holds(false)]", "[Holds(true), Nat]"),
            "entry",
            "Type",
            "Prop",
        ),
        (
            " = [Holds(false), Holds(true)]",
            ("[Holds(false), Holds(true)]", "[Nat, Holds(true)]"),
            "entry",
            "Prop",
            "Type",
        ),
        (
            ": Option(Type) = Option/some(Nat)",
            ("Option/some(Nat)", "Option/some(Holds(true))"),
            "/_mixed",
            "/sys/Option(Prop)",
            "/sys/Option(Type)",
        ),
        (
            "(b: Bool) -> Nat = let arm = match b | true => Nat | false => Bool end; 0",
            ("| false => Bool end", "| false => Holds(b) end"),
            "/_mixed",
            "Type",
            "Prop",
        ),
        (
            "(b: Bool) -> Nat = let arm = match b | true => Bool | false => Nat end; 0",
            ("match b | true => Bool", "match b | true => Holds(b)"),
            "/_mixed",
            "Prop",
            "Type",
        ),
        (
            " = Twin { a = Bool, b = Nat }",
            ("a = Bool, b = Nat", "a = Holds(true), b = Nat"),
            "entry",
            "Type",
            "Prop",
        ),
        (
            " = Twin { a = Nat, b = Bool }",
            ("a = Nat, b = Bool", "a = Nat, b = Holds(true)"),
            "entry",
            "Prop",
            "Type",
        ),
        (
            " = Pack { a = Bool, b = Nat }",
            ("Pack { a = Bool", "Pack { a = Holds(true)"),
            "entry",
            "Type",
            "Prop",
        ),
        (
            " = [id(Bool), Nat]",
            ("[id(Bool), Nat]", "[id(Holds(true)), Nat]"),
            "entry",
            "Type",
            "Prop",
        ),
        (
            " = [Nat, id(Bool)]",
            ("[Nat, id(Bool)]", "[Nat, id(Holds(true))]"),
            "entry",
            "Prop",
            "Type",
        ),
        (
            ": Option(Prop) = pick(Holds(true), Holds(false))",
            ("pick(Holds(true), Holds(false))", "pick(Holds(true), Nat)"),
            "/_mixed",
            "Type",
            "Prop",
        ),
    ] {
        control(&format!("{declarations}\nlet _mixed{sound};\nprint(\"\")"), b"").refused(&[edit]).by_elaborator_in(at, |error| {
            matches!(error, Error::TypeMismatch { inferred, expected, .. } if pinned(inferred) == inferred_as && pinned(expected) == expected_as)
        });
    }
}

// A proposition an implicit meets makes it a sort of propositions: a call whose every argument is one answers at `Prop` and stands where a `Prop` is wanted, a structure literal's parameter and a list literal's element are that sort, and a lambda argument that needs the implicit's sort is checked once the call has it.
#[test]
fn propositions_under_one_implicit_make_it_a_sort_of_propositions() {
    let source = r#"
        use /std/{Bool, Nat, List, print};
        use /std/Bool/{Holds};

        struct Twin(S: Type): pub Type { a: S, b: S }
        let pick(@S: Type, x: S, y: S) -> S = x;
        let id(@A: Type, a: A) -> A = a;
        let through(@S: Type, x: S, k: (S) -> Nat) -> Nat = k(x);

        let both_props: Prop = pick(Holds(true), Holds(false));
        let passed: Prop = id(Holds(true));
        let inferred = pick(Holds(true), Holds(false));
        let held: Prop = inferred;
        let twin_of_props: Twin(Prop) = Twin { a = Holds(true), b = Holds(false) };
        let listed: List(Prop) = [id(Holds(true)), Holds(false)];

        let _ = twin_of_props;
        let _ = listed;
        let _ = through(Holds(true), (p) => 0);
        print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// A part that is no sort at all is refused as it was. The control's two parts are both propositions.
#[test]
fn a_sort_and_a_value_under_one_implicit_are_refused() {
    control(
        r#"
        use /std/{Bool, Nat, print};
        use /std/Bool/{Holds};

        let same(@S: Type, x: S, y: S) -> Nat = 0;
        let _bad: Nat = same(Holds(true), Holds(false));
        print("")
        "#,
        b"",
    )
    .refused(&[("same(Holds(true), Holds(false))", "same(Holds(true), 5)")])
    .by_elaborator_in("/_bad", |error| matches!(error, Error::TypeMismatch { .. }));
}

// Whether a type is a proposition is one fact of it under every case: a result declared at a sort its caller states is one sort, so a match that answers a proposition in one case and a type in another is refused, as it is at `Type`. The control's two cases are both types, at the `Type` they decide.
#[test]
fn a_type_that_is_a_proposition_in_one_case_is_at_no_sort_left_to_a_caller() {
    control(
        r#"
        use /std/{Nat, Bool};

        induct Two: pub Prop
        | a()
        | b()
        end

        let F(c: Bool) -> Type = match c | true => Bool | false => Nat end;

        /std/print("")
        "#,
        b"",
    )
    .refused(&[(
        "-> Type = match c | true => Bool |",
        "-> Sort = match c | true => Two |",
    )])
    .by_elaborator_in("/F", |error| {
        matches!(error, Error::SortDecidedTwice { prop: true, .. })
    });
}

// The same fact through a codomain the argument computes rather than a `Sort` left to a caller: `F(c) : S(c)` is a proposition at `true` and a type at `false`, so `F` is admitted, and what keeps a type one sort under every case is that `F(c)` is no type while `c` is open. At `true` two values are one by irrelevance, read off `F`'s declaration at the argument, and at `false` they stay apart. `curios-cert`'s `kernel::convert::tests::irrelevance` puts the two comparisons to the kernel under this name. The refusals derive from the two values one at `true`, and from a binder at `S(c)`, which is a type wherever `c` is open.
#[test]
fn a_type_whose_sort_its_argument_computes_is_a_proposition_only_where_the_argument_decides_one() {
    control(
        r#"
        use /std/{Nat, Bool, Eq, print};
        use /std/Bool/{True};

        let S(c: Bool) -> Type = match c | true => Prop | false => Type end;
        let F(c: Bool) -> S(c) = match c: (d) => S(d) | true => True | false => Nat end;

        let one(x: F(true), y: F(true)) -> Eq()(x, y) = Eq/refl();
        print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("let one(x: F(true), y: F(true))", "let two(x: F(false), y: F(false))"),
    ])
    .by_elaborator_in("/two", |error| {
        matches!(error, Error::TypeMismatch { inferred, expected, .. } if pinned(inferred) == "/std/Eq/Eq(Nat)(x, x)" && pinned(expected) == "/std/Eq/Eq(Nat)(x, y)")
    });

    control(
        r#"
        use /std/{Nat, Bool, print};
        use /std/Bool/{True};

        let S(c: Bool) -> Type = match c | true => Prop | false => Type end;
        let F(c: Bool) -> S(c) = match c: (d) => S(d) | true => True | false => Nat end;

        let either(c: Bool, x: S(c)) -> Bool = c;
        print("")
        "#,
        b"",
    )
    .refused(&[
        ("x: S(c)", "x: F(c)"),
    ])
    .by_elaborator_in("/either", |error| {
        matches!(error, Error::TypeMismatch { inferred, expected, .. } if pinned(inferred) == "/S(c)" && pinned(expected) == "Type")
    });
}

// A definition generic in a sort stands at `Type` at one use, where matching a proposition of two constructors into it would tell its proofs apart. The elaborator refuses the definition for deciding its sort; the kernel, handed it all the same, refuses it at the assignment that states `Type`. The control states `Prop`, into which a proposition is matched.
#[test]
fn a_proposition_is_not_matched_into_a_sort_left_to_a_caller() {
    control(
        r#"
        induct Two: pub Prop
        | a()
        | b()
        end

        let pick(@A: Prop, t: Two, x: A, y: A) -> A = match t | a() => x | b() => y end;

        /std/print("")
        "#,
        b"",
    )
    .refused(&[("@A: Prop", "@A: Sort")])
    .by_elaborator_in("/pick", |error| {
        matches!(
            error,
            Error::SortDecided {
                prop: true,
                at_decider: true
            }
        )
    });
}

// A proposition carries only proofs, so a field at a sort left to a caller is one the structure decides: at `Type` two of its values would be one with their fields apart. The elaborator refuses the structure for deciding its sort; `curios-cert`'s `recheck::sort_tests` refuses it at the assignment that states `Type`, under this name. The control states `Prop`.
#[test]
fn a_proposition_holds_no_field_at_a_sort_left_to_its_caller() {
    control(
        r#"
        struct W(A: Prop): pub Prop {
            a: A,
        }

        /std/print("")
        "#,
        b"",
    )
    .refused(&[("W(A: Prop)", "W(A: Sort)")])
    .by_elaborator_in("/W", |error| {
        matches!(
            error,
            Error::SortDecided {
                prop: true,
                at_decider: true
            }
        )
    });
}

// Two values of a sort left to a caller are one only where it states `Prop`, so a definition holding any two of them one is true at `Prop` alone, and used at `Type` it would carry `Holds(true)` to `Holds(false)`. The elaborator checks the declaration once with the sort unknown, where no two values are one for being proofs. `curios-cert`'s `recheck::assignment_tests` puts the definition, and the same as a group, to the kernel under this name. Rename both or neither. The control states `Prop`, where two values are one.
#[test]
fn two_values_of_a_sort_left_to_a_caller_are_not_one() {
    control(
        r#"
        let same(@A: Prop, x: A, y: A, G: (A) -> Prop, g: G(x)) -> G(y) = g;

        /std/print("")
        "#,
        b"",
    )
    .refused(&[("@A: Prop", "@A: Sort")])
    .by_elaborator_in("/same", |error| matches!(error, Error::TypeMismatch { .. }));
}

// Two instances of one family apart in a sort are two types, whatever else they share: `Ph` takes no parameter, and its one payload holds the irrelevance a proposition grants, so a value built at `Prop` and read at `Type` would hand `Eq()(false, true)` over. `curios-cert`'s `recheck::sort_tests` puts two such instances to the kernel under this name. The control reads the value at the `Prop` it was built at.
#[test]
fn two_instances_of_a_family_apart_in_a_sort_are_two_types() {
    control(
        r#"
        use /std/{Eq, Bool};
        use /std/Bool/{True};

        induct Ph: pub Type
        | mk(g: (A: Sort, x: A, y: A) -> Eq()(x, y))
        end

        let v: Ph = Ph/mk((A: Prop, x: A, y: A) => Eq/refl());

        let apply(p: Ph) -> Eq()(True/qed(), True/qed()) =
            match p | mk(g) => g(True, True/qed(), True/qed()) end;

        let leak: Eq()(True/qed(), True/qed()) = apply(v);

        /std/print("")
        "#,
        b"",
    )
    .refused(&[
        ("        use /std/Bool/{True};\n", ""),
        ("-> Eq()(True/qed(), True/qed()) =", "-> Eq()(false, true) ="),
        ("g(True, True/qed(), True/qed())", "g(Bool, false, true)"),
        ("let leak: Eq()(True/qed(), True/qed()) = apply(v);", "let leak: Bool/False = Bool/false_neq_true(apply(v));"),
    ])
    .by_elaborator_in("/leak", |error| {
        matches!(error, Error::TypeMismatch { inferred, expected, .. } if pinned_with_instances(inferred) == "/Ph.{Prop; ?u}" && pinned_with_instances(expected) == "/Ph.{Type; ?u}")
    });
}

// A proposition at a family's parameter that wants a type. `curios_cert`'s `recheck::proposition_tests` puts the module to the kernel under this name. The control's parameter is a type.
#[test]
fn a_proposition_is_no_parameter_a_type_is_wanted_at() {
    control(
        r#"
        use /std/{Bool};
        use /std/Bool/{False};

        pub induct Holder(A: Type): Type
        | hold(a: A)
        end

        pub let held: Type = Holder(Bool);

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("Holder(Bool)", "Holder(False)")])
    .by_elaborator_in("/held", |error| matches!(error, Error::TypeMismatch { .. }));
}

// A sort left to a caller is one each use states, so what a declaration claims of a value at it holds at both: `Box(A)` has one inhabitant at a proposition and two at `Bool`, and `generic`, holding any two boxes one, would carry `Holds(true)` to `Holds(false)` at `Type`. The control is the same claim made at `Prop`. `curios-cert`'s `recheck::shape_tests` puts the route to the kernel under this name, refused at the assignment that states `Type`.
#[test]
fn a_struct_at_a_sort_left_to_a_caller_has_one_inhabitant_only_at_prop() {
    control(
        r#"
        use /std/{Bool};
        use /std/Bool/{Holds, True, False};

        struct Box(A: Sort): pub Type { a: A }

        let generic(@A: Prop, x: Box(A), y: Box(A), G: (Box(A)) -> Prop, g: G(x)) -> G(y) = g;

        let _held: Holds(true) = generic(Box { a = True/qed() }, Box { a = True/qed() }, (z: Box(Holds(true))) => Holds(true), True/qed());

        /std/print("1")
        "#,
        b"1",
    )
    .refused(&[
        ("@A: Prop", "@A: Sort"),
        ("let _held: Holds(true) = generic(Box { a = True/qed() }, Box { a = True/qed() }, (z: Box(Holds(true))) => Holds(true), True/qed());", "let _forged: False = generic(Box { a = true }, Box { a = false }, (z: Box(Bool)) => Holds(z.a), True/qed());"),
    ])
    .by_elaborator_in("/generic", |error| matches!(error, Error::TypeMismatch { .. }));
}
