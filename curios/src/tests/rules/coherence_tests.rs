//! One witness per key: duplicates, orphans, and the standard library's witnesses that a user program may not shadow.

use {
    super::test_support::*, crate::test_support::*, curios_common::Qualifier, curios_elab::Error,
};

// Registering two witnesses for the same `(concept, head)` key is a coherence error (global uniqueness) — independent of, and checked alongside, the orphan rule below. The control declares the witness once.
#[test]
fn duplicate_witness_is_an_error() {
    control(
        r#"
        use /std/{Nat, Str};
        pub concept Show(A : Type) : pub Type {
            show(A) -> Str
        }
        satisfy Show(Nat) {
            show(n) = Nat/to_str(n)
        }
        let n : Nat = 1;
        /std/print(Show/show(n))
        "#,
        b"1",
    )
    .refused(&[
        ("        let n : Nat = 1;", "        satisfy Show(Nat) {\n            show(n) = Nat/to_str(n)\n        }\n        let n : Nat = 1;"),
    ])
    .by_elaborator_in("/witness~/Show(Nat)#1", |error| {
        matches!(error, Error::DuplicateWitness { concept, key, first, second, .. } if *concept == global("/Show") && pinned_key(key) == "Nat" && *first == Qualifier::empty() && *second == Qualifier::empty())
    });
}

// The declaring module of a nested-module witness renders as that module, which the entry-module cases above cannot distinguish from a bug that always reports the root. The control declares the witness once.
#[test]
fn duplicate_witness_reports_its_declaring_module() {
    control(
        r#"
        mod M
            pub concept C(A : Type) : pub Type {
                f(A) -> A
            }
            pub induct T : pub Type
            | t()
            end
            satisfy C(T) {
                f(x) = x
            }
        end
        /std/Io/pure(())
        "#,
        b"",
    )
    .refused(&[
        ("            satisfy C(T) {\n                f(x) = x\n            }\n", "            satisfy C(T) {\n                f(x) = x\n            }\n            satisfy C(T) {\n                f(x) = x\n            }\n"),
    ])
    .by_elaborator_in("/M/witness~/M/C(/M/T)#1", |error| {
        matches!(error, Error::DuplicateWitness { concept, key, first, second, .. } if *concept == global("/M/C") && pinned_key(key) == "/M/T" && *first == Qualifier::from(["M"]) && *second == Qualifier::from(["M"]))
    });
}

// Two *different* declaring modules, the branch the same-module collapse above must not swallow: with one witness in each, the report names both. `/B` owns neither the concept nor the type, so the orphan rule would refuse it too — the duplicate insert is reached first only because `/A` declared the pair, and the fixture keeps `/B` importing them rather than declaring its own. The control's `/B` declares nothing.
#[test]
fn a_duplicate_across_two_modules_names_both() {
    control(
        r#"
        mod A
            pub induct T : pub Type
            | t()
            end
            pub concept C(X : Type) : pub Type {
                f(X) -> X
            }
            satisfy C(T) {
                f(x) = x
            }
        end
        mod B
            use /A/{T, C};
        end
        /std/Io/pure(())
        "#,
        b"",
    )
    .refused(&[
        ("            use /A/{T, C};\n", "            use /A/{T, C};\n            satisfy C(T) {\n                f(x) = x\n            }\n"),
    ])
    .by_elaborator_in("/B/witness~/A/C(/A/T)", |error| {
        matches!(error, Error::DuplicateWitness { concept, key, first, second, .. } if *concept == global("/A/C") && pinned_key(key) == "/A/T" && *first == Qualifier::from(["A"]) && *second == Qualifier::from(["B"]))
    });
}

// The orphan rule: a witness may be declared only where the concept it witnesses, or a type in its key, is already declared. `Ord` and `Bool` are both `/std`/`/sys`-owned and the entry program owns neither, so `Ord(Bool)` (not already witnessed anywhere in the standard library) is rejected. The control keys the same witness on a type the entry program owns.
#[test]
fn orphan_witness_is_rejected() {
    control(
        r#"
        use /std/{Bool, Ord, Ordering};
        use /std/ops/{Eql};
        induct Mine : pub Type | m() end
        satisfy Eql(Mine) { .. }
        satisfy Ord(Mine) {
            ord(a, b) = Ordering/eq()
        }
        let n : Bool = true;
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("satisfy Ord(Mine)", "satisfy Ord(Bool)"),
    ])
    .by_elaborator_in("/witness~/std/Ord/Ord(Bool)", |error| {
        matches!(error, Error::OrphanWitness { concept, key, witness } if *concept == global("/std/Ord/Ord") && pinned_key(key) == "Bool" && *witness == Qualifier::empty())
    });
}

// The user's most natural attempt at incoherence, and the one the fixtures above leave out. `orphan_witness_is_rejected` deliberately picks `Ord(Bool)`, a pair the standard library does *not* witness, which leaves out an entry program re-declaring a witness the prelude already holds. `/std/Bool` witnesses `Show(Bool)`, and the answer must be a refusal — otherwise a program could silently replace a standard-library instance at every site that resolves it, which is exactly the incoherence "one witness per key, program-wide" exists to exclude.
//
// The orphan rule is what refuses it, and the ordering is deliberate: `register_witness` checks orphanhood before the duplicate-key insert, because "not allowed to declare this at all" is the more fundamental violation than "and it also collides". Coherence is not resting on that ordering, though, and this is the part worth recording. The replay path re-registers *every* prefix witness through the same `register_witness` — orphan check and duplicate insert alike — rather than trusting the archive, so the prelude's keys are already in the map a user's declaration is inserted into. Both barriers are live and each would refuse this alone; the fixture pins the one a user actually reaches. The control keys the same witness on a type the entry program owns.
#[test]
fn a_standard_library_witness_cannot_be_shadowed() {
    control(
        r#"
        use /std/{Bool, Show, Str};
        induct Mine : pub Type | m() end
        satisfy Show(Mine) {
            show(b) = "shadow"
        }
        /std/print(Show/show(Mine/m()))
        "#,
        b"shadow",
    )
    .refused(&[
        ("satisfy Show(Mine)", "satisfy Show(Bool)"),
        ("Show/show(Mine/m())", "Show/show(true)"),
    ])
    .by_elaborator_in("/witness~/std/Show/Show(Bool)", |error| {
        matches!(error, Error::OrphanWitness { concept, key, witness } if *concept == global("/std/Show/Show") && pinned_key(key) == "Bool" && *witness == Qualifier::empty())
    });
}

// The sanctioned counterpart: a user's own type is legal to `satisfy` a standard-library concept for, since the declaring root (the entry program) owns the key's type even though it doesn't own the concept.
#[test]
fn witness_for_a_locally_owned_type_is_not_an_orphan() {
    let source = r#"
        use /std/{Nat, Str, Show};
        pub struct Wrapper : pub Type { inner : Nat }
        satisfy Show(Wrapper) {
            show(w) = Nat/to_str(w.inner)
        }
        let w : Wrapper = Wrapper { inner = 7 };
        /std/print(Show/show(w))
        "#;

    assert_eq!(run(source), b"7");
}

// The single-occupancy rule holds through head keying: two parametric witnesses over the same family collide on the family's head, whatever their prefix arguments. The control declares the parametric witness alone.
#[test]
fn parametric_witnesses_over_one_family_still_collide() {
    control(
        r#"
        use /std/{Nat, Str, Monad};
        induct Box(S : Type, A : Type) : Type
        | wrap(A)
        end
        satisfy (@S : Type) => Monad((A : Type) => Box(S, A)) {
            pure(@A, a) = Box/wrap(a),
            bind(@A, @B, m, f) =
                match m : (_) => Box(S, B)
                | wrap(a) => f(a)
                end,
        }
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("        /std/print(\"ok\")", "        satisfy Monad((A : Type) => Box(Str, A)) {\n            pure(@A, a) = Box/wrap(a),\n            bind(@A, @B, m, f) =\n                match m : (_) => Box(Str, B)\n                | wrap(a) => f(a)\n                end,\n        }\n        /std/print(\"ok\")"),
    ])
    .by_elaborator_in("/witness~/std/Monad/Monad(/Box)#1", |error| {
        matches!(error, Error::DuplicateWitness { .. })
    });
}

// A name for a type is read through to the type before it keys a witness, so a second witness spelled through the name meets the first at the type's own key: were the name a key of its own, one goal at `Nat` would have two witnesses answering it, and which ran would follow which spelling resolution met. The control declares the one through the name alone, which answers the goal at `Nat`.
#[test]
fn a_witness_spelled_through_a_name_for_a_type_collides_with_the_types_own() {
    control(
        r#"
        use /std/{Nat, Str};
        concept Named(A : Type) : pub Type { name() -> Str }
        let N : Type = Nat;
        satisfy Named(N) { name() = "alias" }
        /std/print(Named/name(@Nat))
        "#,
        b"alias",
    )
    .refused(&[
        ("        let N : Type = Nat;", "        satisfy Named(Nat) { name() = \"nat\" }\n        let N : Type = Nat;"),
    ])
    .by_elaborator_in("/witness~/Named(Nat)#1", |error| {
        matches!(error, Error::DuplicateWitness { concept, key, .. } if *concept == global("/Named") && pinned_key(key) == "Nat")
    });
}

// A group's members register on their signatures before any body elaborates, so a member whose key another member already holds is refused as any duplicate is. The control declares the first alone.
#[test]
fn a_group_member_with_a_taken_key_is_a_duplicate() {
    control(
        r#"
        use /std/{Nat, Str};
        pub concept Show(A : Type) : pub Type {
            show(A) -> Str
        }
        induct A : pub Type | a(Nat) end
        satisfy Show(A) { show(x) = "first", }
        /std/print(Show/show(A/a(1)))
        "#,
        b"first",
    )
    .refused(&[
        ("satisfy Show(A) { show(x) = \"first\", }", "satisfy Show(A) { show(x) = \"first\", }\n        and Show(A) { show(x) = \"second\", }"),
    ])
    .by_elaborator_in("/witness~/Show(/A)", |error| {
        matches!(error, Error::DuplicateWitness { concept, key, .. } if *concept == global("/Show") && pinned_key(key) == "/A")
    });
}

// A shape is a key like any other, so two witnesses of one shape collide exactly as two of one name do — and the report spells the key by its shape, field types elided, because the key does not carry them. The control declares the shape's witness once.
#[test]
fn a_duplicate_tuple_shape_is_refused() {
    control(
        r#"
        use /std/{Nat, Bool, Str};
        pub concept Tag(A: Type): pub Type {
            tag(A) -> Str,
        }
        satisfy Tag({Nat, Bool}) {
            tag(t) = "one",
        }
        /std/print(Tag/tag((1, true)))
        "#,
        b"one",
    )
    .refused(&[
        ("            tag(t) = \"one\",\n        }\n", "            tag(t) = \"one\",\n        }\n        satisfy Tag({Nat, Bool}) {\n            tag(t) = \"two\",\n        }\n"),
    ])
    .by_elaborator_in("/witness~/Tag({_, _})#1", |error| {
        matches!(error, Error::DuplicateWitness { concept, key, .. } if *concept == global("/Tag") && pinned_key(key) == "{_, _}")
    });
}

// A tuple shape is owned by no module, as an intrinsic former is, so it contributes nothing to ownership: an entry program may not key a witness on one for a concept `/std` declares. Two independent packages each declaring `Show({Nat, Bool})` would otherwise collide at link with neither in the wrong. The control keys the same witness for a concept the entry program declares.
#[test]
fn a_tuple_witness_for_a_standard_concept_is_an_orphan() {
    control(
        r#"
        use /std/{Show, Nat, Bool, Str};
        pub concept Display(A: Type): pub Type {
            show(A) -> Str,
        }
        satisfy Display({Nat, Bool}) {
            show(t) = "mine",
        }
        /std/print(Display/show((1, true)))
        "#,
        b"mine",
    )
    .refused(&[
        ("satisfy Display({Nat, Bool})", "satisfy Show({Nat, Bool})"),
        ("Display/show((1, true))", "Show/show((1, true))"),
    ])
    .by_elaborator_in("/witness~/std/Show/Show({_, _})", |error| {
        matches!(error, Error::OrphanWitness { concept, key, .. } if *concept == global("/std/Show/Show") && pinned_key(key) == "{_, _}")
    });
}

// The other half of the rule: the shape contributes nothing, but a concept the entry root declares does, so a program writes tuple witnesses for its own concepts freely. `a_concept_resolves_on_a_tuple_value` in `shape_tests` is the same admission seen from the resolution side.
#[test]
fn a_tuple_witness_for_an_entry_concept_registers() {
    let source = r#"
        use /std/{Nat, Bool, Str};
        pub concept Tag(A: Type): pub Type {
            tag(A) -> Str,
        }
        satisfy Tag({Nat, Bool}) {
            tag(t) = "mine",
        }
        let z: {Nat, Bool} = (1, true);
        /std/print(Tag/tag(z))
        "#;

    assert_eq!(run(source), b"mine");
}

// A witness head written with a list holding nothing is the application `Show()`, so it is refused by the call's own arity report rather than read as the bare concept. The control writes the head's argument.
#[test]
fn a_witness_head_with_nothing_written_is_the_application() {
    control(
        r#"
        use /std/{Nat, Str};
        concept Show(A : Type) : pub Type { show(A) -> Str }
        satisfy Show(Nat) { show(n : Nat) = "" }
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("satisfy Show(Nat)", "satisfy Show()")])
    .by_elaborator_in("/witness~0", |error| {
        matches!(
            error,
            Error::WrongNumberOfArguments {
                expected: 1,
                got: 0,
                ..
            }
        )
    });
}
