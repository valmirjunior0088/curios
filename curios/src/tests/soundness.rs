//! End-to-end coverage for the two totality obligations.
//!
//! Erasure deletes types and it deletes `Prop`-sorted proofs, and both must be
//! total: a divergent type breaks type formation, a divergent proof proves
//! anything. What erasure *keeps* may diverge freely, which is why every
//! rejection here is of a position and never of a definition.
//!
//! The size lattice and the classifier are unit-tested in
//! `curios-elab/src/totality/tests.rs`; these check what a user can observe,
//! through the prelude-replay path a real program takes — where the analysis
//! sees only the user suffix and reads the prelude's verdicts back from the
//! archive.
//!
//! Each rejection asserts the *diagnostic*, not merely that compilation failed.
//! A soundness test that accepts any error is worthless: a typo in the fixture
//! would pass it while the hole stayed open.

use {super::run, curios_runtime::MockHost};

fn rejected(source: &str) {
    let (system, _io) = MockHost::builder().build();
    let error =
        crate::run_text(source, system).expect_err("expected the erased position to be rejected");
    assert!(
        error.contains("not known to terminate") || error.contains("does not terminate"),
        "rejected, but not by the totality gate:\n{error}",
    );
}

/// As [`rejected`], and by **(V)** rather than by (T).
///
/// Which gate fires is the whole claim of a (V)-only fixture: (T) runs first,
/// so a fixture that accidentally put a partial definition in a type position
/// would pass `rejected` while proving nothing about proof positions.
fn rejected_as_a_proof(source: &str) {
    let (system, _io) = MockHost::builder().build();
    let error =
        crate::run_text(source, system).expect_err("expected the proof position to be rejected");
    assert!(
        error.contains("is a proof position"),
        "rejected, but not as a proof position:\n{error}",
    );
}

/// As [`rejected`], and by **(T)** rather than by (V).
///
/// The mirror of [`rejected_as_a_proof`]: a fixture that means to pin a type
/// position proves nothing if a stray proof position is what actually fired.
fn rejected_as_a_type(source: &str) {
    let (system, _io) = MockHost::builder().build();
    let error =
        crate::run_text(source, system).expect_err("expected the type position to be rejected");
    assert!(
        error.contains("is a type position"),
        "rejected, but not as a type position:\n{error}",
    );
}

/// The negative functor every exploit below is built from. It is *accepted* —
/// strict positivity asks whether a declaration reaches itself, and `Sink`
/// never does. Only tying `A` back to `Sink(A)` is dangerous, and no `induct`
/// can express that.
const SINK: &str = r#"
    induct Sink(A : Type) : pub Type
    | sink(f : (A) -> /std/False)
    end
"#;

// The route no analysis of *value*-level partiality can see. The exploit has
// no value-level `rec`, no `exit`, and no negative occurrence in any
// declaration: a lambda, a constructor application, and one application. The
// knot is tied entirely by `rec Bad : Type`, which `check_positivity` never
// looks at, because a `rec` is neither an `induct` nor a `struct`.
#[test]
fn a_type_level_rec_cannot_tie_the_negative_knot() {
    rejected(&format!(
        r#"
        {SINK}

        rec Bad : Type = Sink(Bad);

        let delta(x : Bad) -> /std/False =
            match x
            | sink(g) => g(x)
            end;

        let boom : /std/False = delta(Sink/sink(delta));

        /std/print("unreachable")
        "#
    ));
}

// The same knot behind a projection. A rule keyed on "the `rec` member is
// itself sort-valued" misses this: `P`'s type is a tuple, and only `P.0` is a
// type.
#[test]
fn a_partial_type_behind_a_projection_is_still_a_type() {
    rejected(&format!(
        r#"
        {SINK}

        rec P : {{Type, /std/Nat}} = (Sink(P.0), 0);

        let delta(x : P.0) -> /std/False =
            match x
            | sink(g) => g(x)
            end;

        let boom : /std/False = delta(Sink/sink(delta));

        /std/print("unreachable")
        "#
    ));
}

// Why (T) has to be the aggressive reading. Nothing here is a partial type
// former: `Shape` is structurally recursive and total, and `inf` is an
// ordinary partial *value* of an ordinary data type. The dangerous type exists
// only because a total function was applied to a bottom argument, so a rule
// that looks at what a type-level eliminator scrutinizes does not see it.
#[test]
fn a_total_type_function_applied_to_a_partial_value_is_rejected() {
    rejected(&format!(
        r#"
        {SINK}

        induct F : pub Type
        | stop()
        | more(rest : F)
        end

        rec Shape(f : F) -> Type =
            match f
            | stop() => /std/False
            | more(rest) => Sink(Shape(rest))
            end;

        rec inf : F = F/more(inf);

        let delta(x : Shape(inf)) -> /std/False =
            match x
            | sink(g) => g(x)
            end;

        let boom : /std/False = delta(Sink/sink(delta));

        /std/print("unreachable")
        "#
    ));
}

// The entrypoint's trailing expression is not a top-level item — lowering
// leaves it in `Module::body`. An obligation seeded only from `items` sees
// nothing here. The only items below are `Sink` and `consume`, and neither is
// partial; the offending `rec` lives entirely inside the final term.
#[test]
fn the_entrypoint_expression_is_not_a_blind_spot() {
    rejected(&format!(
        r#"
        {SINK}

        let consume(u : {{}}) -> {{}} = u;

        consume(
            let forge(x : (rec Bad : Type = Sink(Bad); Bad)) -> {{}} = ();
            ()
        )
        "#
    ));
}

// The shape that used to abort the compiler rather than reject the program.
// Reducing `Bad` rebuilds an arrow whose domain is `Bad`, so elaborating the
// first *use* overflowed the stack — long before any whole-module gate could
// run. This is what the local form at `rec` elaboration exists for, and the
// only claim it makes is that the compiler answers instead of dying.
#[test]
fn a_type_level_rec_through_an_arrow_is_diagnosed_not_aborted() {
    rejected(
        r#"
        rec Bad : Type = (Bad) -> /std/False;

        let delta(x : Bad) -> /std/False = x(x);

        let boom : /std/False = delta(delta);

        /std/print("unreachable")
        "#,
    );
}

// The same shape as a *local* `rec`. `Item::Rec` and `Subterm::Rec` are
// separate elaboration paths, and the second one still aborted after the first
// was fixed.
#[test]
fn a_local_type_level_rec_through_an_arrow_is_diagnosed_too() {
    rejected(
        r#"
        let consume(u : {}) -> {} = u;

        consume(
            let forge(x : (rec Bad : Type = (Bad) -> /std/False; Bad)) -> {} = ();
            ()
        )
        "#,
    );
}

// The first route (T) cannot see. `Never` is `Type`-sorted precisely so that
// `proc/exit` is retained where it is written — but eliminating it into a
// *proposition* puts the never-returning call back in a position erasure
// deletes, so the exit does not fire and `boom` is a closed inhabitant of
// `False`. No type here reaches anything partial: the offender is reached only
// through a term whose declared type is `Prop`-sorted.
#[test]
fn an_exit_eliminated_into_a_proposition_is_rejected() {
    rejected_as_a_proof(
        r#"
        use /std/{proc};

        let forge() -> /std/False =
            match proc/exit(1) : (_) => /std/False
            end;

        let boom : /std/False = forge();

        /std/print("unreachable")
        "#,
    );
}

// The second route (T) cannot see, and the one that needs no `exit` at all.
// `forge` is an ordinary partial *value* at a `Type`-sorted carrier — nothing
// about `Box` or its type mentions a partial definition — and the certificate
// escapes through an arm binder, so `boom`'s body reaches `forge` without
// naming a partial type anywhere.
#[test]
fn a_partial_carrier_releasing_a_proof_is_rejected() {
    rejected_as_a_proof(
        r#"
        induct Box : pub Type
        | box(p : /std/False)
        end

        rec forge(n : /std/Nat) -> Box = forge(n + 1);

        let boom : /std/False =
            match forge(0)
            | box(p) => p
            end;

        /std/print("unreachable")
        "#,
    );
}

// The tests below cover (V)'s *argument* rule: a proof handed to a
// `Prop`-declared parameter of a definition that is not itself a proof.
// Nothing above them catches these — the definition-level rule needs a
// `Prop`-sorted declared type, and every offender here sits inside a
// `Nat`-valued function, so no seed reaches it by name.
//
// Each is keyed to one shape of application *head*, because the rule can only
// fire where the head's type can be synthesized: it reads the parameter
// telescope off that type to learn which parameters are propositions. A head
// shape sort synthesis cannot answer for is a silent hole rather than a
// rejection, which is why the coverage is enumerated by shape and not by one
// representative program.

// A universe-polymorphic head. `@A : Type` generalizes the definition, so the
// call site's head is a universe instance rather than a plain name — the most
// common shape in the language, and the widest hole of this set: it admitted a
// one-line helper with no `match`, no data type, and no recursion but the
// forged proof's own.
#[test]
fn a_proof_at_a_polymorphic_head_is_rejected() {
    rejected_as_a_proof(
        r#"
        let ignore(@A : Type, x : A, p : /std/False) -> A = x;

        let leak() -> /std/Nat = ignore(0, rec b : /std/False = b; b);

        /std/print(/std/Nat/to_str(leak()))
        "#,
    );
}

// A match arm binder as the head. An arm binder's type lives in the eliminated
// constructor's telescope rather than in the arm scope, so opening the arm
// without consulting the declaration leaves the binder untyped — and the
// scrutinee's own type is a recursive group member wrapping the inductive, not
// the inductive itself, so naming the declaration takes an unfolding step.
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
            | hold(f) => f(rec b : /std/False = b; b)
            end;

        /std/print(/std/Nat/to_str(leak(make())))
        "#,
    );
}

// A primitive fold binder as the head. `Lst`'s cons arm takes its element type
// from the carrier rather than from any declaration, so this is a different
// source of binder types than the inductive arm above and fails independently.
#[test]
fn a_proof_at_a_fold_binder_head_is_rejected() {
    rejected_as_a_proof(
        r#"
        let apply_it(fs : /std/Lst((/std/False) -> /std/Nat)) -> /std/Nat =
            match fs
            | [] => 0
            | [head, ..tail] => head(rec b : /std/False = b; b)
            end;

        /std/print(/std/Nat/to_str(apply_it([])))
        "#,
    );
}

// A nominal structure projection as the head. A structure's field types come
// from its declaration, instantiated at the head's universes and then at its
// parameters — two steps a tuple projection needs neither of.
#[test]
fn a_proof_at_a_struct_projection_head_is_rejected() {
    rejected_as_a_proof(
        r#"
        struct Api : pub Type {
            take : (/std/False) -> /std/Nat,
        }

        let api : Api = Api { take = (p) => 7 };

        let leak() -> /std/Nat = api.take(rec b : /std/False = b; b);

        /std/print(/std/Nat/to_str(leak()))
        "#,
    );
}

// The same projection route as a user would actually write it: concept
// dispatch projects a method out of a resolved witness dictionary, so the
// head of `Sink/drain` is a structure projection reached through resolution
// rather than through a written `.field`.
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

        let leak() -> /std/Nat = Sink/drain(5, rec b : /std/False = b; b);

        /std/print(/std/Nat/to_str(leak()))
        "#,
    );
}

// General recursion is untouched wherever erasure keeps it. `collatz` cannot
// pass any size-change checker — it recurses on a computed quotient, and
// whether it terminates at all is an open problem — and it runs anyway,
// because its result is a `Nat` the program actually consumes.
#[test]
fn a_partial_definition_stays_usable_in_a_program() {
    let source = r#"
        use /std/{Nat};

        rec collatz(n : Nat, steps : Nat) -> Nat =
            choose
            | n <= 1 => steps
            | n % 2 == 0 => collatz(n / 2, steps + 1)
            | _ => collatz(3 * n + 1, steps + 1)
            end;

        /std/print(Nat/to_str(collatz(6, 0)))
        "#;
    assert_eq!(run(source), b"8");
}

// (T) is seeded two ways, because neither reaches what the other does. The walk
// finds types where they are *written* — annotations, motives, telescopes, and
// the body of any definition whose type ends in a sort. It has no case for an
// application's arguments, so a type handed to a function is written nowhere it
// looks. The settle records find those, because a term whose own type is a sort
// is a type wherever it appears.
//
// The argument form is the one that reaches this gate at all. Written as an
// annotation, `Shape(inf)` is *forced* while the binder elaborates and the step
// budget stops it there — fail-closed, but before any whole-module pass runs.
// Passed as a type argument it is never forced, so elaboration succeeds and only
// the gate is left. That form was accepted until the settle records were added.
//
// Nothing here is `Prop`-typed, so (V) cannot fire and (T) is on its own:
// `Shape` is total, descending structurally on `F`, and `inf` is an ordinary
// partial value of a data type.
// The whole-module gate runs post-zonk — after elaboration has already done the
// type-level reduction (T) exists to make safe. A type that reaches a partial
// definition and happens to be *productive* survives elaboration and the gate
// rejects it. One that is not productive spins until the step budget dies, and
// the program used to be refused for running out of a resource no amount of
// would have helped. Refusing the written type up front is what makes both
// shapes report the same thing, which is the thing that is actually wrong.
//
// `Shape(inf)` here reduces to itself with no progress; the productive sibling
// is `a_total_type_function_applied_to_a_partial_value_is_rejected`.
#[test]
fn a_non_productive_type_level_loop_is_diagnosed_not_exhausted() {
    rejected_as_a_type(&format!(
        "{SHAPE}\n let here(x : Shape(inf)) -> Nat = 0;\n\n 0"
    ));
}

#[test]
fn a_partial_value_reaching_a_type_through_an_argument_is_rejected() {
    rejected_as_a_type(&format!(
        "{SHAPE}\n let ignore(@A : Type, x : Nat) -> Nat = x;\n\n ignore(@Shape(inf), 5)"
    ));
}

/// A *total* type-level function and a *partial* value of ordinary data type.
/// `Shape` descends structurally on `F`; nothing here is a partial type former,
/// which is why only the aggressive reading of (T) rejects the pair at all.
const SHAPE: &str = r#"
    use /std/{Nat};

    induct F : pub Type
    | stop()
    | more(rest : F)
    end

    rec Shape(f : F) -> Type =
        match f
        | stop() => Nat
        | more(rest) => Shape(rest)
        end;

    rec inf : F = F/more(inf);
"#;

// The three fixtures below probe the *erasure premise*: that everything erasure
// deletes lies within a term one of the obligations covers.
//
// Erasure deletes more than types and proofs. Each of these sites drops a whole
// construct, arguments included, and those arguments are ordinary values —
// neither a type nor a proof, so nothing seeds them directly. What makes that
// safe is containment rather than coverage: the *enclosing* term is a proof
// position, and the reachability closure walks into it. Each fixture therefore
// hides a partial `Nat` computation inside one deleted construct, and each must
// be rejected for reaching it.

// `erase_apply`'s proof-valued callee: the application collapses to the unit
// constant, discarding every argument unevaluated.
#[test]
fn a_partial_argument_to_an_erased_call_is_still_reached() {
    let source = r#"
        use /std/{Nat, True};

        rec spin(n : Nat) -> Nat = spin(n);

        let mk_proof(n : Nat) -> True = True/qed();

        let use_it(n : Nat) -> Nat =
            let witness : True = mk_proof(spin(0));
            n;

        /std/print(Nat/to_str(use_it(5)))
        "#;
    rejected_as_a_proof(source);
}

// `is_proof_constructor`: a `Prop` family's constructor is the one direct call
// erasure drops whole, on a predicate that never consults `is_erasable`.
#[test]
fn a_partial_argument_to_a_proof_constructor_is_still_reached() {
    let source = r#"
        use /std/{Nat};

        rec spin(n : Nat) -> Nat = spin(n);

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

// An erasable scrutinee: the elimination reduces to its single live arm and the
// scrutinee is never emitted.
#[test]
fn a_partial_erased_scrutinee_is_still_reached() {
    let source = r#"
        use /std/{Nat, True};

        rec spin(n : Nat) -> Nat = spin(n);

        let mk(n : Nat) -> True = True/qed();

        let use_it(n : Nat) -> Nat =
            match mk(spin(0))
            | qed() => n
            end;

        /std/print(Nat/to_str(use_it(5)))
        "#;
    rejected_as_a_proof(source);
}

// (V) is seeded where elaboration *settles* a term, so its coverage argument
// rests on every `Prop`-typed term in the accepted module having been settled.
// A metavariable solution is the one way a term reaches the module without
// that: witness resolution fills the slot rather than elaborating a written
// argument. Here the witness of a `Prop`-sorted concept — a proof — is partial,
// and it arrives entirely by resolution. If solutions were outside what the
// seeding sees, this program would compile.
#[test]
fn a_partial_proof_cannot_arrive_through_witness_resolution() {
    let source = r#"
        use /std/{Nat, True};

        concept Trivial(A : Type) : pub Prop {
            fact(A) -> True,
        }

        satisfy Trivial(Nat) {
            fact(n) = rec loop : True = loop; loop,
        }

        let needs_witness(@A : Type, use Trivial(A), x : A) -> Nat = 0;

        /std/print(Nat/to_str(needs_witness(5)))
        "#;
    rejected_as_a_proof(source);
}

// The corpus shapes the design is keyed to, exercised through the replay path
// rather than the prelude build. `BigNat`'s arithmetic rests on `add/raw`,
// which descends on *either* of two `Bits` depending on the arm — a shape only
// match refinement can see — and `Fmt/print`'s result type is
// `format_type_with(parse(s))`, a type-level `rec` over a parsed format.
#[test]
fn the_prelude_shapes_a_user_program_leans_on_still_elaborate() {
    let source = r#"
        use /std/{BigNat, Fmt};

        Fmt/print("%-%")(BigNat/to_str(BigNat/of_nat(99999999) + BigNat/of_nat(1)))(3)
        "#;
    assert_eq!(run(source), b"100000000-3");
}
