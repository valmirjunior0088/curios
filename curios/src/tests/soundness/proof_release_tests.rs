//! The erasure obligation: a partial value may not reach a proof, at any head a program can spell.

use crate::tests::run;

use super::test_support::*;

// The second route (T) cannot see, and the one that needs no `exit` at all. `forge` is an ordinary partial *value* at a `Type`-sorted carrier — nothing about `Box` or its type mentions a partial definition — and the certificate escapes through an arm binder, so `boom`'s body reaches `forge` without naming a partial type anywhere.
#[test]
fn a_partial_carrier_releasing_a_proof_is_rejected() {
    rejected_as_a_proof(
        r#"
        induct Box : pub Type
        | box(p : /std/Bool/False)
        end

        let forge(n : /std/Nat) -> Box = forge(n + 1);

        let boom : /std/Bool/False =
            match forge(0)
            | box(p) => p
            end;

        /std/print("unreachable")
        "#,
    );
}

// The tests below cover (V)'s *argument* rule: a proof handed to a `Prop`-declared parameter of a definition that is not itself a proof. Nothing above them catches these — the definition-level rule needs a `Prop`-sorted declared type, and every offender here sits inside a `Nat`-valued function, so no seed reaches it by name.
//
// Each is keyed to one shape of application *head*, because the rule can only fire where the head's type can be synthesized: it reads the parameter telescope off that type to learn which parameters are propositions. A head shape sort synthesis cannot answer for is a silent hole rather than a rejection, which is why the coverage is enumerated by shape and not by one representative program.

// A universe-polymorphic head. `@A : Type` generalizes the definition, so the call site's head is a universe instance rather than a plain name — the most common shape in the language, and the widest of this set: without the rule, a one-line helper with no `match`, no data type, and no recursion but the forged proof's own would pass.
#[test]
fn a_proof_at_a_polymorphic_head_is_rejected() {
    rejected_as_a_proof(
        r#"
        let ignore(@A : Type, x : A, p : /std/Bool/False) -> A = x;

        let leak() -> /std/Nat = ignore(0, let b : /std/Bool/False = b; b);

        /std/print(/std/Nat/to_str(leak()))
        "#,
    );
}

// A match arm binder as the head. An arm binder's type lives in the eliminated constructor's telescope rather than in the arm scope, so opening the arm without consulting the declaration leaves the binder untyped — and the scrutinee's own type is a recursive group member wrapping the inductive, not the inductive itself, so naming the declaration takes an unfolding step.
#[test]
fn a_proof_at_an_arm_binder_head_is_rejected() {
    rejected_as_a_proof(
        r#"
        induct Holder : pub Type
        | hold(f : (/std/Bool/False) -> /std/Nat)
        end

        let make() -> Holder = Holder/hold((p) => 0);

        let leak(h : Holder) -> /std/Nat =
            match h
            | hold(f) => f(let b : /std/Bool/False = b; b)
            end;

        /std/print(/std/Nat/to_str(leak(make())))
        "#,
    );
}

// An intrinsic fold binder as the head. `List`'s cons arm takes its element type from the carrier rather than from any declaration, so this is a different source of binder types than the inductive arm above and fails independently.
#[test]
fn a_proof_at_a_fold_binder_head_is_rejected() {
    rejected_as_a_proof(
        r#"
        let apply_it(fs : /std/List((/std/Bool/False) -> /std/Nat)) -> /std/Nat =
            match fs
            | [] => 0
            | [head, ..tail] => head(let b : /std/Bool/False = b; b)
            end;

        /std/print(/std/Nat/to_str(apply_it([])))
        "#,
    );
}

// A nominal structure projection as the head. A structure's field types come from its declaration, instantiated at the head's universes and then at its parameters — two steps a tuple projection needs neither of.
#[test]
fn a_proof_at_a_struct_projection_head_is_rejected() {
    rejected_as_a_proof(
        r#"
        struct Api : pub Type {
            take : (/std/Bool/False) -> /std/Nat,
        }

        let api : Api = Api { take = (p) => 7 };

        let leak() -> /std/Nat = api.take(let b : /std/Bool/False = b; b);

        /std/print(/std/Nat/to_str(leak()))
        "#,
    );
}

// The same projection route as a user would actually write it: concept dispatch projects a method out of a resolved witness dictionary, so the head of `Sink/drain` is a structure projection reached through resolution rather than through a written `.field`.
#[test]
fn a_proof_at_a_concept_method_head_is_rejected() {
    rejected_as_a_proof(
        r#"
        pub concept Sink(A : Type) : pub Type {
            drain(x : A, p : /std/Bool/False) -> /std/Nat,
        }

        satisfy Sink(/std/Nat) {
            drain(x, p) = x,
        }

        let leak() -> /std/Nat = Sink/drain(5, let b : /std/Bool/False = b; b);

        /std/print(/std/Nat/to_str(leak()))
        "#,
    );
}

// A call back into a group from inside another group's projected member. `Walk::walk` gives a *member reference* — a `rec` node whose tail selects one member — an arm above the general `rec` one, so that a self-reference cannot send the walk into the bodies it is already inside; `RecGroup::member_body` materializes each self-reference as a projection carrying the whole group, so descending would regenerate those bodies without end. That arm answers for a projection of *this* group alone: a projection of a different group falls to the general arm, because an inner group is classified on its own but its bodies may still call this group, and such a call is a real edge of this group's call graph.
//
// Answered by the member-reference arm instead, the call back into `f` from inside the projected `g` would be invisible, and so would `g`'s own call site inside `f`: each group would close to no call at all and be classified `Total` while `f(0)` diverges through `g`, a closed inhabitant of `False` — `f`'s declared type is a proposition, so (V) is the whole defence and it reads the engine's verdict. The same loop with the inner group removed, `rec f(n : Nat) -> False = f(n);`, is refused either way, which places the rule in the walk rather than in the obligation.
#[test]
fn a_proof_looping_through_a_projected_inner_group_is_rejected() {
    rejected_as_a_proof(
        r#"
        use /std/{Nat, Str};
        use /std/Bool/{False};

        let f(n : Nat) -> False =
            (let g(m : Nat) -> False = f(m); g)(n);

        /std/print(match f(0) : (_) => Str end)
        "#,
    );
}

// The accepting side of that same descent, and the reason the rule is not "a group whose body mentions a projection is partial". `outer` descends on its own parameter, and its arm projects a foreign group whose bodies the walk enters. Nothing in `keep` calls back, so entering it must find no edge and leave both groups total — `outer` by its own `outer(p)`, `keep` by having no recursive call at all. Both types are propositions, so a spurious edge in either is a rejection rather than a silent loss of precision.
#[test]
fn a_proof_projecting_an_inner_group_that_does_not_call_back_is_accepted() {
    let source = r#"
        use /std/{Nat};
        use /std/Bool/{True};

        let outer(n : Nat) -> True =
            match n
            | 0 => True/qed()
            | p + 1; _ => (let keep(t : True) -> True = t; keep)(outer(p))
            end;

        let proved : True = outer(3);

        /std/print("kept")
        "#;
    assert_eq!(run(source), b"kept");
}

#[test]
fn a_partial_value_reaching_a_type_through_an_argument_is_rejected() {
    rejected_as_a_type(&format!(
        "{SHAPE}\n let ignore(@A : Type, x : Nat) -> Nat = x;\n\n let _ = ignore(@Shape(inf), 5);\n /std/Io/pure(())"
    ));
}

// The four fixtures below probe the *erasure premise*: that everything erasure deletes lies within a term one of the obligations covers.
//
// Erasure deletes more than types and proofs. Each of these sites drops a whole construct, arguments included, and those arguments are ordinary values — neither a type nor a proof, so nothing seeds them directly. What makes that safe is containment rather than coverage: the *enclosing* term is a proof position, and the reachability closure walks into it. Each fixture therefore hides a partial `Nat` computation inside one deleted construct, and each must be rejected for reaching it.

// `erase_apply`'s proof-valued callee: the application collapses to the unit constant, discarding every argument unevaluated.
#[test]
fn a_partial_argument_to_an_erased_call_is_still_reached() {
    let source = r#"
        use /std/{Nat};
        use /std/Bool/{True};

        let spin(n : Nat) -> Nat = spin(n);

        let mk_proof(n : Nat) -> True = True/qed();

        let use_it(n : Nat) -> Nat =
            let witness : True = mk_proof(spin(0));
            n;

        /std/print(Nat/to_str(use_it(5)))
        "#;
    rejected_as_a_proof(source);
}

// `is_proof_constructor`: a `Prop` family's constructor is the one direct call erasure drops whole, on a predicate that never consults `is_erasable`.
#[test]
fn a_partial_argument_to_a_proof_constructor_is_still_reached() {
    let source = r#"
        use /std/{Nat};

        let spin(n : Nat) -> Nat = spin(n);

        induct Tagged : pub Prop
        | tag(n : Nat)
        end

        let use_it(n : Nat) -> Nat =
            let witness : Tagged = Tagged/tag(spin(0));
            n;

        /std/print(Nat/to_str(use_it(5)))
        "#;
    rejected_as_a_proof(source);
}

// An erasable scrutinee: the elimination reduces to its single live arm and the scrutinee is never emitted.
#[test]
fn a_partial_erased_scrutinee_is_still_reached() {
    let source = r#"
        use /std/{Nat};
        use /std/Bool/{True};

        let spin(n : Nat) -> Nat = spin(n);

        let mk(n : Nat) -> True = True/qed();

        let use_it(n : Nat) -> Nat =
            match mk(spin(0))
            | qed() => n
            end;

        /std/print(Nat/to_str(use_it(5)))
        "#;
    rejected_as_a_proof(source);
}

// A proof bound by `let`: the binding is a kept slot, filled with a stand-in, and its value is never walked — whatever form the value takes, so this one is an elimination rather than a call.
#[test]
fn a_partial_scrutinee_under_an_erased_binding_is_still_reached() {
    let source = r#"
        use /std/{Nat};
        use /std/Bool/{True};

        let spin(n : Nat) -> Nat = spin(n);

        let use_it(n : Nat) -> Nat =
            let witness : True =
                match spin(0)
                | 0 => True/qed()
                | _ => True/qed()
                end;
            n;

        /std/print(Nat/to_str(use_it(5)))
        "#;
    rejected_as_a_proof(source);
}

// (V) is seeded where elaboration *settles* a term, so its coverage argument rests on every `Prop`-typed term in the accepted module having been settled. A metavariable solution is the one way a term reaches the module without that: witness resolution fills the slot rather than elaborating a written argument. Here the witness of a `Prop`-sorted concept — a proof — is partial, and it arrives entirely by resolution. If solutions were outside what the seeding sees, this program would compile.
#[test]
fn a_partial_proof_cannot_arrive_through_witness_resolution() {
    let source = r#"
        use /std/{Nat};
        use /std/Bool/{True};

        concept Trivial(A : Type) : pub Prop {
            fact(A) -> True,
        }

        satisfy Trivial(Nat) {
            fact(n) = let loop : True = loop; loop,
        }

        let needs_witness(@A : Type, use Trivial(A), x : A) -> Nat = 0;

        /std/print(Nat/to_str(needs_witness(5)))
        "#;
    rejected_as_a_proof(source);
}
