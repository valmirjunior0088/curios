//! A `!` region's monad: read from its expected type, waited for where it has yet to come, refused where it is no monad or never comes, and a sequencing left stranded.

use {
    crate::tests::{error, run, typecheck},
    curios_utilities::fill,
};

/// An action whose head is not a monad at all is called out as such — suggesting an edge that could never be declared would be a trap.
#[test]
fn a_non_monad_action_is_called_out() {
    let source = r#"
        use /std/{Io, Nat, print};
        pub struct Box(A: Type): pub Type {
            A,
        }
        pub let main() -> Io({}) =
            let b: Box(Nat) = Box { 1 };
            let _ = b!;
            print("unreachable");
        main()
        "#;

    let error = typecheck(source).expect_err("a non-monad action must refuse");
    assert!(
        error.contains("Box is not a monad"),
        "expected the non-monad refinement, got: {error}"
    );
}

/// A monad-polymorphic action (`Monad/pure`) has no rigid monad of its own: the oracle abstains, the region pins the flex hole as it would with no lift at all, and nothing is wrapped.
#[test]
fn a_flex_action_still_sequences_without_lifting() {
    let source = r#"
        use /std/{Monad, Io, Nat, print};
        pub let main() -> Io({}) =
            let n = Monad/pure(65)!;
            print(Nat/to_str(n));
        main()
        "#;

    assert_eq!(run(source), b"65");
}

/// Strict postponement: a bang in an inference-position region is refused rather than letting the action elect the monad. The parameter is annotated, so the region's monad is the one thing left undetermined.
#[test]
fn a_bang_in_an_inference_position_region_is_refused() {
    let source = r#"
        use /std/{Str, print};
        let f = (s: Str) => print(s)!;
        f("never")
        "#;

    let error = typecheck(source).expect_err("an inference-position bang must refuse");
    assert!(
        error.contains("the monad of this region was never determined"),
        "expected an inference refusal, got: {error}"
    );
}

/// A region whose type is an implicit nothing solves is refused as the inference-position one is, at its `!`: the sequencing waits under the one placeholder it parked with, so the item's drain meets no progress and ends.
#[test]
fn a_region_whose_type_never_comes_is_refused_at_its_bang() {
    let source = r#"
        use /std/{Io, print};
        let ignore(@T: Type, program: () -> T) -> Io({}) = print("x");
        ignore(() =>
            let _ = print("a")!;
            print("b"))
        "#;

    let error = typecheck(source).expect_err("a region nothing states must refuse");
    assert!(
        error.contains("the monad of this region was never determined"),
        "expected the region's refusal, got: {error}"
    );
    assert!(
        error.contains("let _ = print(\"a\")!;"),
        "expected the refusal at the `!`, got: {error}"
    );
}

/// A region waits for its type where a part read after it states one: the arm beside it solves the implicit both are checked at, and the sequencing is then read in that monad.
#[test]
fn a_region_waits_for_the_arm_beside_it_to_state_its_type() {
    let source = r#"
        use /std/{Nat, Io, print};
        let ignore(@T: Type, program: () -> T) -> Io({}) = print("x");
        let n: Nat = 3;
        ignore(() =>
            match n
            | 0 =>
                let _ = print("a")!;
                print("b")
            | _ => print("more")
            end)
        "#;

    assert_eq!(run(source), b"x");
}

/// A region whose type is known and is no monad names that type, rather than reporting the wrapper's unsolved hole.
///
/// The rigid twin of the refusal above. `elaborate_bang` reads the monad from the region, and when a *rigid* region yields none its head applies to nothing, so `?M(?B)` can never meet it — left to the wrapper's own inference it would report `no witness of Monad(?) found`, blaming a premise of `/std/Monad/bind`, a call the author never wrote, and printing a hole where the region's type is already in hand. `documentation/syntax.md` writes the first of these programs out with the intended reading beside it.
#[test]
fn a_bang_in_a_region_that_is_no_monad_names_the_region_type() {
    for (source, region) in [
        (
            r#"
        use /std/{Str, Bool, print};
        let probe(tag : Str, r : Bool) -> Bool =
            let _ = print(tag)!;
            r;
        /std/Io/pure(())
        "#,
            "Bool",
        ),
        (
            r#"
        use /std/{Nat, List, print};
        let f(xs : List(Nat)) -> Nat = List/fold(xs, 0, (x, acc) => print("x")!);
        /std/Io/pure(())
        "#,
            "Nat",
        ),
        // A proof's region, which is a proposition: nothing is sequenced into a proof.
        (
            r#"
        use /std/{Io};
        use /std/Bool/{True, Holds};
        let _sequenced: Holds(true) = (let _ = Io/pure(1)!; True/qed());
        /std/Io/pure(())
        "#,
            "True",
        ),
    ] {
        let error = typecheck(source).expect_err("a non-monadic region must refuse");
        assert!(
            error.contains(&format!(
                "this region's type is {region}, which is no monad"
            )),
            "{error}"
        );
        assert!(!error.contains("Monad/bind"), "{error}");
    }
}

/// Values and functions receive the same type-directed sequencing diagnosis, placed at the written action.
#[test]
fn values_and_functions_report_the_same_invalid_sequencing_types() {
    for (binding, refusal) in [
        (
            "let n: Nat = Io/pure(3)!;",
            "this region's type is Nat, which is no monad",
        ),
        (
            "let cell: Cell({}) = Cell/new()!;",
            "this region's type is Cell({}), which is no monad",
        ),
        (
            "let xs: List(Nat) = Io/pure([1])!;",
            "no witness of /std/Monad/Lift(Io, List) found",
        ),
    ] {
        let function = binding.replacen(':', "() ->", 1);
        for declaration in [binding, &function] {
            let source = fill!(
                "use /std/{Nat, List, Cell, Io};\n${declaration}\n()\n",
                declaration
            );
            let error = typecheck(&source).expect_err("sequencing needs a suitable monad");
            assert!(error.contains(refusal), "{error}");
            assert!(error.contains(declaration), "{error}");
            assert!(error.contains('^'), "{error}");
            assert!(!error.contains("Monad/bind"), "{error}");
        }
    }
}

/// A region whose monad is a telescope binder still sequences: the imitation reads a variable head, so the refusal above cannot fire on one.
#[test]
fn a_bang_sequences_in_a_region_whose_monad_is_a_parameter() {
    let source = r#"
        use /std/{Nat, Monad};
        let f(@M : (Type) -> Type, use Monad(M), a : M(Nat)) -> M(Nat) =
            let x = a!;
            Monad/pure(x + 1);
        /std/Io/pure(())
        "#;

    typecheck(source).expect("a parameterized monad region sequences");
}

#[test]
fn a_long_sequencing_chain_compiles_and_runs() {
    // `inline_known_calls` prunes each inlined callee's dead body between sweeps; left standing, its call sites would keep counting, callees would turn multi-site, and a `!` chain's continuations would duplicate exponentially — 16 sequenced actions would not finish compiling.
    let mut source = String::from("use /std/{Str, Io, print};\nlet main() -> Io({}) =\n");
    for _ in 0..24 {
        source.push_str("let _ = print(\"x\")!;\n");
    }
    source.push_str("print(\"done\\n\");\nmain()");
    assert_eq!(run(&source), format!("{}done\n", "x".repeat(24)).as_bytes());
}

/// A `!` reads its region's monad from a type whose *value slot* is still open, which is what a lambda checked against a caller's signature always has.
///
/// The region here is `Try(Io, Io/Error, ?)`: rigid head, rigid context arguments, and only the slot the `!` itself solves left open. Refusing it as `which is no monad` would be wrong twice — the type is one whose monad is plainly `(X) => Try(Io, Io/Error, X)`, and what would be missing is the reading, not the monad — and annotating the enclosing result type, which changes nothing about the monad, would make the identical program compile. `/std/File/with` and `/std/tcp/Socket/with` are spelled exactly this way.
#[test]
fn a_bang_reads_its_monad_where_only_the_value_slot_is_open() {
    let source = r#"
        use /std/{Try, Io, Nat, Str};
        let take(@A: Type, body: (Nat) -> Try(Io, Io/Error, A)) -> Try(Io, Io/Error, A) =
            body(1);
        let entry() -> /std/Io({}) =
            let outcome = Try/run(take((n) =>
                let a = Try/pure(@Io, @Io/Error, n)!;
                Try/pure(a + 1)))!;
            match outcome
            | success(v) => /std/print(Nat/to_str(v))
            | failure(_) => /std/print("error")
            end;
        entry()
        "#;
    assert_eq!(run(source), b"2");
}

/// The two refusals the reading above must not have swallowed: a region whose type can never name a monad, and one whose type is never determined at all.
#[test]
fn a_region_that_names_no_monad_is_still_refused() {
    let rigid = error(
        r#"
        use /std/{Bool, Str, print};
        let probe(tag: Str, r: Bool) -> Bool =
            let _ = print(tag)!;
            r;
        /std/print("x")
        "#,
    );
    assert!(rigid.contains("no monad"), "unexpected error: {rigid}");

    let undetermined = error(
        r#"
        use /std/{Try, Io, Nat};
        let entry() -> /std/Io({}) =
            let go = Try/run((() => let a = Try/pure(@Io, @Io/Error, 1)!; Try/pure(a + 1))())!;
            /std/print("ok");
        entry()
        "#,
    );
    assert!(
        undetermined.contains("never determined"),
        "unexpected error: {undetermined}"
    );
}

/// A sequencing stranded in a region that is no monad names the monad the region would have to be, and leaves the result its continuation never produced as a goal: a blank is no term, so the report writes none.
#[test]
fn a_stranded_sequencing_leaves_its_result_a_goal() {
    let stranded = error(
        r#"
        use /std/{Nat, Monad, Str};
        let g(@M: (Type) -> Type, use Monad(M), a: M(Str)) -> Nat =
            Monad/bind(a, (_) => Monad/pure(1));
        /std/print("x")
        "#,
    );
    assert!(
        stranded.contains("this sequencing needs a region whose type is M(?)"),
        "unexpected error: {stranded}"
    );
}
