//! The erasure obligation: a partial value may not reach a proof, at any head a program can spell.

//! End-to-end coverage for the two totality obligations.
//!
//! Erasure deletes types and it deletes `Prop`-sorted proofs, and both must be total: a divergent type breaks type formation, a divergent proof proves anything. What erasure *keeps* may diverge freely, which is why every rejection here is of a position and never of a definition.
//!
//! The size lattice and the classifier are unit-tested in `curios-analysis/src/totality/tests.rs`; these check what a user can observe, through the prelude-replay path a real program takes — where the analysis sees only the user suffix and reads the prelude's verdicts back from the archive.
//!
//! Each rejection asserts the *diagnostic*, not merely that compilation failed. A soundness test that accepts any error is worthless: a typo in the fixture would pass it while the hole stayed open.

use super::super::run;

use super::test_support::*;

// The second route (T) cannot see, and the one that needs no `exit` at all. `forge` is an ordinary partial *value* at a `Type`-sorted carrier — nothing about `Box` or its type mentions a partial definition — and the certificate escapes through an arm binder, so `boom`'s body reaches `forge` without naming a partial type anywhere.
#[test]
fn a_partial_carrier_releasing_a_proof_is_rejected() {
    rejected_as_a_proof(
        r#"
        induct Box : pub Type
        | box(p : /std/False)
        end

        let forge(n : /std/Nat) -> Box = forge(n + 1);

        let boom : /std/False =
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

// A universe-polymorphic head. `@A : Type` generalizes the definition, so the call site's head is a universe instance rather than a plain name — the most common shape in the language, and the widest hole of this set: it admitted a one-line helper with no `match`, no data type, and no recursion but the forged proof's own.
#[test]
fn a_proof_at_a_polymorphic_head_is_rejected() {
    rejected_as_a_proof(
        r#"
        let ignore(@A : Type, x : A, p : /std/False) -> A = x;

        let leak() -> /std/Nat = ignore(0, let b : /std/False = b; b);

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
        | hold(f : (/std/False) -> /std/Nat)
        end

        let make() -> Holder = Holder/hold((p) => 0);

        let leak(h : Holder) -> /std/Nat =
            match h
            | hold(f) => f(let b : /std/False = b; b)
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
        let apply_it(fs : /std/List((/std/False) -> /std/Nat)) -> /std/Nat =
            match fs
            | [] => 0
            | [head, ..tail] => head(let b : /std/False = b; b)
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
            take : (/std/False) -> /std/Nat,
        }

        let api : Api = Api { take = (p) => 7 };

        let leak() -> /std/Nat = api.take(let b : /std/False = b; b);

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
            drain(x : A, p : /std/False) -> /std/Nat,
        }

        satisfy Sink(/std/Nat) {
            drain(x, p) = x,
        }

        let leak() -> /std/Nat = Sink/drain(5, let b : /std/False = b; b);

        /std/print(/std/Nat/to_str(leak()))
        "#,
    );
}

// The size-change engine never opened the group that called back. `Walk::walk` gives a *member reference* — a `rec` node whose tail selects one member — an arm above the general `rec` one, so that a self-reference cannot send the walk into the bodies it is already inside; `RecGroup::member_body` materializes each self-reference as a projection carrying the whole group, so descending would regenerate those bodies without end. That arm answered for every projection and not only the group's own: a projection of a *different* group fell into it, matched no branch, and returned, leaving that group's member bodies unwalked. The general arm below is there for exactly the case it thereby skipped — an inner group is classified on its own, but its bodies may still call *this* group, and such a call is a real edge of this group's call graph.
//
// So a call back into `f` from inside the projected `g` was invisible, and so was `g`'s own call site inside `f`. Each group closed to no call at all, and a group with no recursive call is accepted — both classified `Total` while `f(0)` diverges through `g`, which is a closed inhabitant of `False`: `f`'s declared type is a proposition, so (V) is the whole defence and it read the engine's verdict.
//
// Verified while the hole was open: the program compiled, the compile-path kernel recheck raised no verdict, and `False/absurd` on the forged proof erased to an `unreachable` the runtime trapped on. The same loop with the inner group removed — `rec f(n : Nat) -> False = f(n);` — was refused throughout, which is what places the defect in the walk rather than in the obligation.
#[test]
fn a_proof_looping_through_a_projected_inner_group_is_rejected() {
    rejected_as_a_proof(
        r#"
        use /std/{Nat, Str, False};

        let f(n : Nat) -> False =
            (let g(m : Nat) -> False = f(m); g)(n);

        /std/print(match f(0) : (_) => Str end)
        "#,
    );
}

// The accepting side of that same descent, and the reason the fix is not "a group whose body mentions a projection is partial". `outer` descends on its own parameter, and its arm projects a foreign group whose bodies the walk now enters. Nothing in `keep` calls back, so entering it must find no edge and leave both groups total — `outer` by its own `outer(p)`, `keep` by having no recursive call at all. Both types are propositions, so a spurious edge in either is a rejection rather than a silent loss of precision.
#[test]
fn a_proof_projecting_an_inner_group_that_does_not_call_back_is_accepted() {
    let source = r#"
        use /std/{Nat, True};

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

// The three fixtures below probe the *erasure premise*: that everything erasure deletes lies within a term one of the obligations covers.
//
// Erasure deletes more than types and proofs. Each of these sites drops a whole construct, arguments included, and those arguments are ordinary values — neither a type nor a proof, so nothing seeds them directly. What makes that safe is containment rather than coverage: the *enclosing* term is a proof position, and the reachability closure walks into it. Each fixture therefore hides a partial `Nat` computation inside one deleted construct, and each must be rejected for reaching it.

// `erase_apply`'s proof-valued callee: the application collapses to the unit constant, discarding every argument unevaluated.
#[test]
fn a_partial_argument_to_an_erased_call_is_still_reached() {
    let source = r#"
        use /std/{Nat, True};

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
        use /std/{Nat, True};

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

// (V) is seeded where elaboration *settles* a term, so its coverage argument rests on every `Prop`-typed term in the accepted module having been settled. A metavariable solution is the one way a term reaches the module without that: witness resolution fills the slot rather than elaborating a written argument. Here the witness of a `Prop`-sorted concept — a proof — is partial, and it arrives entirely by resolution. If solutions were outside what the seeding sees, this program would compile.
#[test]
fn a_partial_proof_cannot_arrive_through_witness_resolution() {
    let source = r#"
        use /std/{Nat, True};

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
