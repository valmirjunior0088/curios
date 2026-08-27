//! Inserting, overriding and saturating implicit arguments, and the surplus that is rejected.

use super::test_support::*;

#[test]
fn arguments_can_all_be_supplied_explicitly() {
    // Every implicit slot can be overridden positionally with a call-site `@` — including an inductive constructor's parameters, which are implicit by default — and the fully-supplied call compiles end-to-end.
    let source = r#"
        use /std/{Nat};
        induct Opt(A : Type) : Type
        | some(A)
        | none()
        end
        let id(@T : Type, x : T) -> T = x;
        match Opt/some(@Nat, id(@Nat, 1)) : (_) => Nat
        | some(value) => value
        | none() => 0
        end
    "#;

    compile(source, Some("/std/Nat")).unwrap();
}

#[test]
fn argument_is_inserted_and_inferred() {
    // An `@`-marked inductive parameter makes the constructor's type argument implicit, so the call site writes no holes at all.
    let source = r#"
        use /std/{Nat};
        induct Opt(A : Type) : Type
        | some(A)
        | none()
        end
        match Opt/some(1) : (_) => Nat
        | some(value) => value
        | none() => 0
        end
    "#;

    compile(source, Some("/std/Nat")).unwrap();
}

#[test]
fn interleaved_implicit_with_partial_override() {
    // `T` is overridden positionally with `@`, `U` (interleaved after an explicit binder) is inferred from `y`.
    let source = r#"
        use /std/{Nat, Bytes};
        let second(@T : Type, x : T, @U : Type, y : U) -> U = y;
        std/Bytes/len(second(@Nat, 1, /std/Str/to_bytes("abc")))
    "#;

    compile(source, Some("/std/Nat")).unwrap();
}

#[test]
fn argument_queues_are_order_insensitive() {
    // The two queues are matched independently: an `@`-argument fills the first unfilled implicit binder no matter where it sits among the plain arguments.
    let at_first = r#"
        use /std/{Nat, Bytes};
        let second(@T : Type, x : T, @U : Type, y : U) -> U = y;
        std/Bytes/len(second(@Nat, 1, /std/Str/to_bytes("abc")))
    "#;
    let at_last = r#"
        use /std/{Nat, Bytes};
        let second(@T : Type, x : T, @U : Type, y : U) -> U = y;
        std/Bytes/len(second(1, /std/Str/to_bytes("abc"), @Nat))
    "#;

    compile(at_first, Some("/std/Nat")).unwrap();
    compile(at_last, Some("/std/Nat")).unwrap();
}

#[test]
fn trailing_implicit_is_pinned_by_the_expected_type() {
    // The proof-argument shape: the implicit trails every explicit binder and is mentioned only in the result type, so nothing but the result-directed turnaround can pin it.
    let source = r#"
        use /std/{Nat};
        induct Opt(A : Type) : Type
        | some(A)
        | none()
        end
        let nothing(n : Nat, @T : Type) -> Opt(T) = Opt/none(@T);
        let r : Opt(Nat) = nothing(0);
        match r : (_) => Nat
        | some(value) => value
        | none() => 9
        end
    "#;

    compile(source, Some("/std/Nat")).unwrap();
}

#[test]
fn all_implicit_telescope_saturates_and_retargets() {
    // The curried `bind` shape: `(@A, @B) -> (M A, A -> M B) -> M B`. Applying it directly to plain arguments saturates the all-implicit telescope with fresh metavariables and re-targets the arguments at the next telescope — both through a direct call and the `!` sugar (which sequences through the user's `Monad(Id)` witness).
    let source = r#"
        use /std/{Nat, Monad};
        induct Id(A : Type) : Type
        | wrap(A)
        end
        let bind : (@A : Type, @B : Type) -> (Id(A), (A) -> Id(B)) -> Id(B) =
            (@A, @B) => (m, f) =>
                match m : (_) => Id(B)
                | wrap(x) => f(x)
                end;
        satisfy Monad(Id) {
            pure(@A, x) = Id/wrap(x),
            bind(@A, @B, m, f) = bind(@A, @B)(m, f)
        }
        let direct = bind(Id/wrap(1), (x) => Id/wrap(Nat/succ(x)));
        -- The lambda body is its own region root: the `!` sequences inside
        -- it instead of hoisting into the entrypoint tail (which returns a
        -- bare `Nat`, not an `Id`). The annotation is what names the region's
        -- monad: a `!` reads it from the region's type and never infers it
        -- from the action, so an inference-position region is refused.
        let sugared_block : ({}) -> Id(Nat) =
            (_) =>
                let v = Id/wrap(3)!;
                Id/wrap(v);
        let sugared = sugared_block(());
        match sugared : (_) => Nat
        | wrap(value) =>
            match direct : (_) => Nat
            | wrap(other) => Nat/add(value, other)
            end
        end
    "#;

    compile(source, Some("/std/Nat")).unwrap();
}

#[test]
fn uninferred_implicit_names_the_binder_and_function() {
    // Nothing mentions `T` outside the binder itself, so unification can never pin it; the report must name the hole, not a bare metavar id.
    let source = r#"
        use /std/{Nat};
        let cast(x : Nat, @T : Type) -> Nat = x;
        cast(5)
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(
        error.contains("implicit argument 'T' of '/cast' was not inferred"),
        "unexpected error: {error}"
    );
}

#[test]
fn surplus_implicit_arguments_are_rejected() {
    let source = r#"
        use /std/{Nat};
        let id(@T : Type, x : T) -> T = x;
        id(@Nat, @Nat, 1)
    "#;

    let error = compile(source, None).unwrap_err();

    assert!(
        error.contains("2 '@' argument(s) but the function has only 1 implicit parameter(s)"),
        "unexpected error: {error}"
    );
}

#[test]
fn bare_polymorphic_function_inserts_implicits_in_value_position() {
    // Passing a bare `cat : (@T, List T, List T) -> List T` where an explicit `(List Nat, List Nat) -> List Nat` is expected: the check turnaround (`insert_implicits_on_check`) inserts the implicit `@T` and eta-expands over the explicit binders, so no hand-written `(l, r) => cat(l, r)` wrapper is needed. Lowers end-to-end — the eta-expansion is an ordinary closure over a saturated call.
    let source = r#"
        use /std/{List};
        use /std/{Nat};
        let cat(@T : Type, a : List(T), b : List(T)) -> List(T) = [..a, ..b];
        let pairwise(f : (List(Nat), List(Nat)) -> List(Nat), a : List(Nat)) -> List(Nat) =
            f(a, a);
        let result : List(Nat) = pairwise(cat, [1]);
        /std/print(/std/Nat/to_str(List/len(result)))
    "#;

    assert!(compile(source, None).is_ok());
}

#[test]
fn polymorphic_value_assignment_keeps_its_implicit() {
    // The guard arm: when the *expected* type also leads with an implicit binder, implicit-eta must not fire — the polymorphic function is assigned as-is, implicit intact, and stays applicable at a chosen instance. Without the expected-not-implicit gate this would wrongly eta-expand and fail to convert against the implicit-leading annotation.
    let source = r#"
        use /std/{List};
        use /std/{Nat};
        let cat(@T : Type, a : List(T), b : List(T)) -> List(T) = [..a, ..b];
        let g : (@T : Type, List(T), List(T)) -> List(T) = cat;
        let result : List(Nat) = g(@Nat, [1], [2]);
        /std/print(/std/Nat/to_str(List/len(result)))
    "#;

    assert!(compile(source, None).is_ok());
}
