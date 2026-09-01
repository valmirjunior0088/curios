//! What postpones until a sibling pins it, and what an unannotated binding infers.

use {
    crate::*,
    curios_text::{Entrypoint, RootSource},
};

use super::test_support::*;

#[test]
fn entrypoint_type_is_used_as_expected_type() {
    // A `Str` literal, because a numeral no longer serves: `0` realizes at an expected `Bool` since numerals became the packed literals' constant-atom spelling, so the mismatch needs a shape no expectation can absorb.
    let entrypoint =
        r#""zero""#.parse::<Entrypoint>().unwrap().with_type("/std/Bool".parse().unwrap());

    let error = compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |_| {},
    )
    .map_err(String::from)
    .unwrap_err();

    assert!(error.contains("type mismatch"));
}

#[test]
fn an_entrypoint_type_may_apply_a_type_former() {
    // The annotation is elaborated before it becomes the expectation (`elaborate_module_suffix`), so an application of a type former reduces to the intrinsic it denotes. Left raw it reached conversion as an `Apply` that no unfolding could reconcile with the inferred `Intrinsic::ListType`, and the mismatch was reported between two spellings of one type — `List Nat` against `List(Nat)`.
    let source = r#"
        use /std/{List, Nat};
        [1]
    "#;

    assert!(compile(source, Some("/std/List(/std/Nat)")).is_ok());
}

#[test]
fn a_surviving_conversion_reports_postponement_naming_its_blockers() {
    // `f`'s implicit domain meets `(Nat) -> Option(?X)` with `?X` never pinned — and minted under the lambda's own binder, so the embedded-metavariable guard's containment exemption cannot commit the candidate and it postpones. The goal parks and survives the drain. The report must say the conversion was postponed — not that the types rigidly mismatched — and name the blockers it watched. (A bare `f(Option/none())` no longer serves: `?X` is then contained in the implicit's own scope, the forced solution commits, and the honest residue is the uninferred implicit itself.)
    let source = r#"
        use /std/{Nat, Option, Io};

        let f(@A: Type, a: A) -> {} = ();

        let stuck: {} = f((n: Nat) => Option/none());

        Io/pure(())
    "#;
    let error = compile(source, None).map(|_| ()).unwrap_err();
    assert!(
        error.contains("cannot decide a postponed conversion"),
        "unexpected report: {error}"
    );
    assert!(error.contains("never solved"), "unexpected report: {error}");
    assert!(!mentions_metavar_id(&error), "id leaked: {error}");
}

#[test]
fn a_conversion_parked_under_refinements_notes_the_dependence() {
    // `Scalar/below`'s proof checks against `Below(?c, 0xD800)` — a match stuck on the unsolved index — and parks under the `code < 0xD800` arm's refinement. Nothing ever pins `?c`, so the survivor's report must say the goal was postponed and note the refinement dependence.
    let source = r#"
        use /std/{Nat, Option, True, Char, Io};
        use /std/Char/{Scalar};

        let f(code: Nat) -> {} =
            match code < 0xD800
            | true =>
                let s = Scalar/below(True/qed());
                ()
            | false => ()
            end;

        Io/pure(())
    "#;
    let error = compile(source, None).map(|_| ()).unwrap_err();
    assert!(
        error.contains("cannot decide a postponed conversion"),
        "unexpected report: {error}"
    );
    assert!(
        error.contains("match-arm refinements"),
        "unexpected report: {error}"
    );
}

#[test]
fn a_metavariable_blocked_match_comparison_parks_until_the_index_lands() {
    // The Item 2 acceptance shape: the proof argument is checked before anything pins `@b`, against `Nat/Lt(0, Bytes/len(?b))` — a match stuck on the metavariable. The goal must park and discharge once the witness argument solves `?b`, in either argument order.
    let source = r#"
        use /syn/Str/{Scan, Utf8};
        use /std/{Nat, Byte, Bytes, True, Io};

        let proof_first(@b: Bytes, nz: Nat/Lt(0, Bytes/len(b)), w: Utf8(Scan/lead(), b)) -> {} = ();

        let call(h: Byte, t: Bytes, valid: Utf8(Scan/lead(), x[h, ..t])) -> {} =
            proof_first(True/qed(), valid);

        Io/pure(())
    "#;
    assert!(compile(source, None).is_ok());
}

#[test]
fn a_packed_literal_decomposes_against_its_folded_spine() {
    // The packed-literal view's acceptance shape, distilled from `BigNat/succ.crs`: `raw(b[])` folds to the literal `b[1]`, so recovering the injectivity lemma's implicits needs `append(b[], ?h) ≡ b[1]` and the concat suffix against the same literal — the length-directed decomposition, since no shape congruence relates `Bin` to `BinAppend`/`BinConcat`.
    let source = r#"
        use /std/{Bool, Bits, Eq, False, True, Io};
        use /std/Bool/{false_neq_true};

        let head_of(x: Bits) -> Bool =
            match x | b[] => false | b[h, .._] => h end;

        let cons_inj_head(
            @h1: Bool,
            @t1: Bits,
            @h2: Bool,
            @t2: Bits,
            p: Eq(b[h1, ..t1], b[h2, ..t2]),
        ) -> Eq(h1, h2) =
            match p: (s, t, q) => Eq(head_of(s), head_of(t)) | refl(@z) => Eq/refl() end;

        let raw(x: Bits) -> Bits =
            match x
            | b[] => b[1]
            | b[h, ..t] => match h | true => b[0, ..raw(t)] | false => b[1, ..t] end
            end;

        let probe(zt: Bits, p: Eq(raw(b[]), raw(b[true, ..zt]))) -> False =
            false_neq_true(Eq/sym(cons_inj_head(p)));

        Io/pure(())
    "#;
    assert!(compile(source, None).is_ok());
}

#[test]
fn a_dependent_result_action_auto_lifts_through_bang() {
    // `Cell/new : (@T: Type, x: T) -> Io(Cell(T))` names its binder in the result, so the auto-lift oracle can key it only by opening the declared telescope before reading the head. The `!` must insert the `Lift(Io, Async)` embedding without an explicit `lift(...)`.
    let source = r#"
        use /std/{Async, Cell, Nat, Io};

        let fiber: Async(Nat) =
            let c = Cell/new(7)!;
            let n = Cell/get(c)!;
            Async/pure(n);

        Io/pure(())
    "#;
    assert!(compile(source, None).is_ok());
}

#[test]
fn a_list_element_lambda_body_solves_against_the_element_metavariable() {
    // The inference spec's defect (a), same root cause as defect (b): checked as a list element, `map`'s result metavariable reaches the lambda body's conversion still spelling solved metavariables whose spines carry out-of-scope binders, and the unmaterialized scope check refused the ground `Option(A)`.
    let source = r#"
        use /std/{Async, Option, Nat};

        let probe(@A: Type, body: Async(A)) -> Async({Nat, Option(A)}) =
            Async/select([Async/map(body, (a) => Option/some(a))]);

        /std/Io/pure(())
    "#;
    assert!(compile(source, None).is_ok());
}

#[test]
fn a_solved_metavariable_in_a_candidate_does_not_strand_the_wake_cascade() {
    // The inference spec's defect (b): `c`'s element type is pinned only by the later `Cell/set`, and the chain back to the `Cell/new` argument runs through solved metavariables whose spines carry the continuation binder. `solve` must materialize committed solutions before its scope analysis, or the ground candidate is refused for a name that only rides a solved spine.
    let source = r#"
        use /std/{Cell, Option, Io, Nat};
        let probe: Io({}) =
            let c = Cell/new(Option/none())!;
            let _ = Cell/set(c, Option/some(1))!;
            Io/pure(());
        probe
    "#;
    assert!(compile(source, None).is_ok());
}

#[test]
fn a_conversion_held_up_by_a_goal_and_an_implicit_still_reports_postponement() {
    // The diversion above applies only when written goals are *all* that holds a conversion up. Here the goal's congruence equation rides under a lambda binder — `f`'s implicit meets `(n: Nat) -> Eq(?f(k), ?f(7))` whose metavariables were minted under that binder, so the containment exemption cannot commit the candidate and it postpones — and the survivor watches the goal and the implicit both, reporting as a postponement naming each by what it is: never by an id, which is elaboration state the reader cannot decode. The sides show every solved metavariable beside the open one — `?(k)`, not `?(?)` — because display materializes tolerantly.
    let source = r#"
        use /std/{Nat, Eq};
        let f(@A : Type, a : A) -> {} = ();
        let stuck(k : Nat, h : Eq(k, 7)) -> {} = f((n : Nat) => Eq/cong(?, h));
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(
        error.contains("cannot decide a postponed conversion"),
        "unexpected error: {error}"
    );
    assert!(
        error.contains("Eq(@?, ?(k), ?(7))"),
        "unexpected error: {error}"
    );
    assert!(
        error.contains("never solved: a written goal `?`, the implicit argument 'A' of"),
        "unexpected error: {error}"
    );
    assert!(!mentions_metavar_id(&error), "id leaked: {error}");
}

#[test]
fn lambda_argument_postpones_until_a_sibling_pins_its_domain() {
    // `with((pair) => pair.0, xs)`: the inserted implicit `?A` is the lambda's domain *and* `xs`'s element type, but `xs : List(?A)` is checked after the lambda. Elaboration must postpone the lambda (its domain is an unsolved metavar, and its body projects `pair.0`) until `xs` pins `?A`, then re-check it. Guards the lambda-domain arm of `blocked_on_metavar`; without it this fails "projected from a non-tuple". Checked at the type-check level — the inference is the point, not lowering. (`with` is local: the std maps take their collection first, which would pin `?A` before the lambda and vacate the scenario.)
    let source = r#"
        use /std/{List};
        use /std/{Nat};
        let with(@A : Type, @B : Type, f : (A) -> B, xs : List(A)) -> List(B) =
            List/map(xs, f);
        let first(xs : List({ Nat, Nat })) -> List(Nat) =
            with((pair) => pair.0, xs);
        0
    "#;

    assert!(typecheck(source, Some("/std/Nat")).is_ok());
}

#[test]
fn empty_array_postpones_until_a_sibling_pins_its_element_type() {
    // `pick([], cat)`: the inserted implicit `?A` is the empty array's type *and* `combine`'s domain. The empty-array literal `[]` borrows its element type from the expected (check-only intro), so against the bare metavar `?A` it cannot elaborate. Elaboration must postpone it until the sibling `cat` grounds `?A := List(?T)`, then re-check — at which point the `List(Nat)` result pins `?T`. Exercises the array arm of `blocked_on_metavar`; without it this fails "type mismatch" eagerly. `cat` is declared here rather than taken from `/std` because concatenation is literal syntax, which cannot be passed as a value.
    let source = r#"
        use /std/{List};
        use /std/{Nat};
        let cat(@T : Type, a : List(T), b : List(T)) -> List(T) = [..a, ..b];
        let pick(@A : Type, fallback : A, combine : (A, A) -> A) -> A =
            combine(fallback, fallback);
        let go : List(Nat) =
            pick([], cat);
        0
    "#;

    assert!(typecheck(source, Some("/std/Nat")).is_ok());

    // With no sibling to ground the element type and no result type to pin it, the postponed `[]` re-checks against a bare metavar and is rejected — graceful degradation, no new acceptance.
    let unpinned = r#"
        use /std/{List};
        let id(@A : Type, x : A) -> A = x;
        let bad = id([]);
        0
    "#;

    assert!(typecheck(unpinned, Some("/std/Nat")).is_err());
}

#[test]
fn continuation_postpones_until_the_result_type_pins_its_codomain() {
    // A `!` region whose tail is `Parse/pure((x, x))` — a *bare tuplecheckable only against a known tuple type. The expected type reaches the tail solely through each bind's result metavar `?B`, which the turnaround solves *after* the continuation is checked. Elaboration must postpone the continuation lambda (its codomain `M(?B)` carries a result metavar) until `expect` grounds `?B` against the concrete `Parse({ Byte, Byte })`, then re-check it. Guards the codomain arm of `blocked_on_metavar`; without it the tail fails "introduced a tuple where the expected type is not a tuple type".
    let source = r#"
        use /std/{Parse};
        use /std/{Byte};
        let pair : Parse({ Byte, Byte }) =
            let x = Parse/any_byte!;
            Parse/pure((x, x));
        0
    "#;

    assert!(typecheck(source, Some("/std/Nat")).is_ok());

    // The `expected_ground` gate: with no concrete result type to pin `?B`, the codomain stays a metavar, the continuation is *not* postponed, and the bare tuple is rejected — graceful degradation, no new acceptance. The unpinned region is a typeless local `let`'s body, which is where a region's type is still inferred: the entrypoint tail is always checked now.
    let unpinned = r#"
        use /std/{Parse};
        let bad =
            let x = Parse/any_byte!;
            Parse/pure((x, x));
        0
    "#;

    assert!(typecheck(unpinned, Some("/std/Nat")).is_err());
}

#[test]
fn closure_returning_a_bare_projection_lowers() {
    // A closure whose body *is* a tuple projection (`(pair) => pair.0`), handed to a higher-order function over an empty array, never constructs a tuple anywhere in the module — yet lowering must still emit the arity-1 tuple type the projection reads through. The wasm `Table` sizes its tuple types from the max arity it sees; scanning only tuple *constructions* missed this projection-only arity and panicked "`Table` lacks tuple type for arity `1`". Guards folding projection (`index + 1`) and prealloc arities into that scan.
    let source = r#"
        use /std/{List};
        use /std/{Nat};
        let mapped : List(Nat) = List/map(@{ Nat, Nat }, @Nat, [], (pair) => pair.0);
        /std/print(/std/Nat/to_str(List/len(mapped)))
    "#;

    assert!(compile(source, None).is_ok());
}

#[test]
fn typeless_let_infers_a_literal_body() {
    // A local `let` with no type annotation infers the body's type (`Nat` here) and lowers end-to-end.
    let source = r#"
        let n = 5;
        n
    "#;

    assert!(compile(source, Some("/std/Nat")).is_ok());
}

#[test]
fn typeless_let_binds_an_annotated_closure() {
    // The composite feature: a typeless local `let` binds an annotated closure. The closure's type is synthesized from its annotation (Infer-mode `elaborate_func`), the let's type is inferred from it, and `f(5)` checks and lowers all the way to wasm.
    let source = r#"
        use /std/{Nat};
        let f = (x : Nat) => x;
        f(5)
    "#;

    assert!(compile(source, Some("/std/Nat")).is_ok());
}

#[test]
fn closure_annotation_must_match_the_expected_domain() {
    // In checking position the param annotation is verified against the expected function type's domain — a wrong annotation is a type mismatch.
    let source = r#"
        use /std/{Nat, Bool};
        let f : (Nat) -> Nat = (x : Bool) => x;
        f(5)
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(error.contains("mismatch"), "unexpected error: {error}");
}

#[test]
fn bare_typeless_let_closure_cannot_be_inferred() {
    // Without an annotation there is nothing to infer the domain from, so a typeless `let` binding a bare closure is a `cannot`-infer error.
    let source = r#"
        let f = (x) => x;
        f
    "#;

    let error = compile(source, None).unwrap_err();

    assert!(error.contains("cannot"), "unexpected error: {error}");
}

#[test]
fn typecheck_accepts_a_well_typed_program() {
    // The fast path stops after `elaborate → zonk`; a well-typed program passes without running erase/cont/optimize/wasm.
    assert!(typecheck("/std/print(/std/Nat/to_str(0))", None).is_ok());
}
