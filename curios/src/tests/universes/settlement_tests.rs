//! Which of a declaration's levels its callers choose, and where every other one lands.

use {super::test_support::universe_parameters, crate::tests::run};

// A universe parameter is minted only from a declaration's *interface* — its type and the registry signatures a use site instantiates. A level reachable only through the body is minimized to a constant instead (`UniverseSolver::finalize`'s `internal` set, which no occurrence could choose a value for), so `carrier`, whose sole sort occurrence is the `Type` it stores, is monomorphic, while `holder`, whose level is tied to a `Type`-sorted parameter, generalizes.
//
// That asymmetry is what shuts [Case equations and their key](../../../documentation/design/soundness/case-equations-and-their-key.md)'s still-open elaborator-side copy against *source*. The defect needs two occurrences of one definition at differing instances whose values differ by level, and only a level carried into a payload can make a value differ — `Type u` embedded in a term is the entry's own counterexample. Such a level is body-only, so it never becomes a parameter, so no occurrence can choose one and the pair has no surface spelling. The entry records that nothing in the corpus spells it; what this pins is the stronger claim that nothing *can*, which is the half that does not decay when the corpus changes.
//
// The counts are asserted rather than a printout matched, because the parameter count is the thing the argument turns on and a printer is free to render it differently.
#[test]
fn a_body_carried_level_is_minimized_rather_than_generalized() {
    let source = r#"
        use /std/{Nat, Str};
        induct Box : pub Type
        | wrap(Type)
        end
        let carrier : Box = Box/wrap(Type);
        let holder(@A : Type) -> Box = Box/wrap(A);
        /std/print("both")
        "#;

    let parameters = universe_parameters(source);

    assert_eq!(
        parameters.get("/carrier"),
        Some(&0),
        "a body-carried level became a parameter a use site can choose: {parameters:?}",
    );
    assert_eq!(
        parameters.get("/holder"),
        Some(&1),
        "an interface level stopped generalizing, so the control no longer separates the two: {parameters:?}",
    );
}

// **A concept's method level is its family's domain.** `ret`'s `@A: Type` is bounded only by the domain of `M`, and cumulativity already lets a caller pass anything smaller, so it is that domain: `Mon` takes its family's domain and codomain and no parameter per method. Generalized apart, a method level is one an implementation may answer at below its family's domain, which would leave `Result`'s `Monad` witness taking `Type 0` payloads only. `Two` is the control: its `pick` is bounded by two families' domains, which name no one level to be, so it keeps a level of its own.
#[test]
fn a_method_level_bounded_by_one_domain_is_that_domain() {
    let source = r#"
        concept Mon(M: (Type) -> Type): pub Type {
            ret(@A: Type, A) -> M(A),
        }
        concept Two(M: (Type) -> Type, N: (Type) -> Type): pub Type {
            pick(@A: Type, M(A)) -> N(A),
        }
        /std/print("tied")
        "#;

    let parameters = universe_parameters(source);

    assert_eq!(
        parameters.get("/Mon"),
        Some(&2),
        "a method level bounded by its family's domain alone was generalized apart from it: {parameters:?}",
    );
    assert_eq!(
        parameters.get("/Two"),
        Some(&5),
        "a method level bounded by two domains was identified with one of them: {parameters:?}",
    );
}

// **A witness leaves its method levels to its callers.** `Mon`'s `ret` quantifies `@A: Type` bounded above by the domain of the family `M`, and that level is the family's domain (`a_method_level_bounded_by_one_domain_is_that_domain`), which the witness's `(A: Type) => Box(A)` leaves to its caller. At a level of its own, the witness's goal would name it only as an argument of its occurrence of `Mon`, where it counts as a result sort and goes to zero, so `ret` through the witness would take `Type 0` payloads only and refuse `Mon/ret(Nat)` at `Box(Type)` as a `Type` strictly below itself. `small` is the control: the same witness at a small payload.
#[test]
fn a_witness_leaves_its_method_levels_to_its_callers() {
    let source = r#"
        use /std/{Nat};
        concept Mon(M: (Type) -> Type): pub Type {
            ret(@A: Type, A) -> M(A),
        }
        induct Box(A: Type): pub Type
        | box(A)
        end
        satisfy Mon((A: Type) => Box(A)) {
            ret(@_, a) = Box/box(a),
        }
        let small: Box(Nat) = Mon/ret(3);
        let large: Box(Type) = Mon/ret(Nat);
        match large | box(_) => /std/print("large") end
        "#;

    assert_eq!(run(source), b"large");
}

// **An occurrence sits where its argument does.** `List(Nat)` checks `Nat : Type 0` against the level its occurrence of `List` mints, which records nothing a store keeps; generalized as interface where it stands in a signature, that level would give `pair_of` a parameter per occurrence, each above the `Type 0` both checkers size the enclosing tuple and `Io` by — a tuple type is sized from its parts' reducts, where the occurrence is gone. `UniverseSolver::finalize` settles an occurrence's level at its recorded floor instead, which is its argument's level, so the written type sits where its reduct does. `Tree` holds a list of itself and settles the same way rather than generalizing the list's level above the family's own. `keep` is the control: its `A` is a binder a caller chooses, and the occurrence of `List` over it settles at `A`'s level, which stays the one parameter.
#[test]
fn an_occurrence_level_settles_at_its_argument_rather_than_generalizing() {
    let source = r#"
        use /std/{List, Nat, Io};
        induct Tree : pub Type
        | leaf(Nat)
        | node(List(Tree))
        end
        let pair_of(x: List(Nat)) -> Io({List(Nat), List(Nat)}) = Io/pure((x, x));
        let keep(@A: Type, x: List(A)) -> List(A) = x;
        /std/print("settled")
        "#;

    let parameters = universe_parameters(source);

    assert_eq!(
        parameters.get("/pair_of"),
        Some(&0),
        "an occurrence's level was generalized above its argument's: {parameters:?}",
    );
    assert_eq!(
        parameters.get("/Tree"),
        Some(&0),
        "a family's occurrence of a former over itself was generalized: {parameters:?}",
    );
    assert_eq!(
        parameters.get("/keep"),
        Some(&1),
        "a level a caller chooses stopped generalizing, so the control no longer separates the two: {parameters:?}",
    );
}

// **A declaration's levels do not follow where its witnesses are written.** `rewrap`'s `!` asks for `Monad(Box)`, and `same` reaches `Eql(Box(A))` and `Spell(Box(A))` through the `use` premises of a function another unit declares. A goal whose witness the unit declares and has not elaborated voids its item's attempt, which is made again once the witness has: so each goal resolves while its declaration's levels are open, `A` stays the caller's, and a use at `Type` is accepted with the witness written before the declaration or after it. Settled at its least levels instead, a declaration written before its witness would offer `A` at one level, and `big` would be refused.
#[test]
fn a_declarations_levels_do_not_follow_where_its_witnesses_are_written() {
    let through_bang = (
        "let rewrap(@A: Type, b: Box(A)) -> Box(A) = let a = b!; Box { value = a };",
        "satisfy Monad(Box) { pure(@_, a) = Box { value = a }, bind(@_, @_, m, f) = f(m.value) }",
        "let big: Box(Type) = rewrap(Box { value = Nat });",
        "/rewrap",
    );
    let through_a_premise = (
        "let same(@A: Type, b: Box(A)) -> Test = Test/equal(b, b);",
        r#"satisfy (@A: Type) => Eql(Box(A)) { eql(_, _) = true, neq(_, _) = false, }
           satisfy (@A: Type) => Spell(Box(A)) { spell(_) = "box", }"#,
        "let big: Test = same(Box { value = Nat });",
        "/same",
    );
    let program = |first: &str, second: &str, use_: &str| {
        format!(
            r#"
            use /std/{{Monad, Nat, Spell, Test}};
            use /std/ops/{{Eql}};
            pub struct Box(A: Type): pub Type {{ value: A }}
            {first}
            {second}
            {use_}
            /std/print("settled")
            "#
        )
    };

    for (declaration, witnesses, use_, name) in [through_bang, through_a_premise] {
        let before = universe_parameters(&program(declaration, witnesses, use_));
        let after = universe_parameters(&program(witnesses, declaration, use_));

        assert_eq!(
            before.get(name),
            Some(&1),
            "a declaration written before its witness lost the level its caller chooses: {before:?}",
        );
        assert_eq!(
            before, after,
            "where a witness is written decided a declaration's levels"
        );
    }
}

// A level that occurs only in a declaration's result sort is one no use site can choose, and `finalize_definition` minimizes it rather than minting a parameter for it. The group path generalizes the same signatures, so the same rule must hold there: a `rec` returning a type, and a `rec` proof whose family level sits only in its result, take no more parameters than the `let` beside them. Without this a definition's scheme would depend on which path elaborated it — a fact no reader can see once a group is decided by whether a body names itself.
#[test]
fn a_rec_result_sort_level_is_minimized_like_a_let_s() {
    let source = r#"
        use /std/{Nat, Eq};
        induct N : pub Type | z() | s(N) end
        let f(n: N) -> Type = N;
        let g(n: N) -> Type =
            match n
            | z() => N
            | s(m) => g(m)
            end;
        let count(n: N) -> Nat =
            match n
            | z() => 0
            | s(m) => count(m) + 1
            end;
        let count_self(n: N) -> Eq()(count(n), count(n)) =
            match n
            | z() => Eq/refl()
            | s(m) => Eq/refl()
            end;
        /std/print("same")
        "#;

    let parameters = universe_parameters(source);

    assert_eq!(
        parameters.get("/f"),
        Some(&0),
        "the let's result-sort level became a parameter: {parameters:?}",
    );
    assert_eq!(
        parameters.get("/g"),
        Some(&0),
        "the rec's result-sort level was generalized where the let's was minimized: {parameters:?}",
    );
    assert_eq!(
        parameters.get("/count_self"),
        Some(&0),
        "the recursive proof's family level was generalized: {parameters:?}",
    );
}

// **A type's spelling carries a universe its value need not.** `Pick(true)` is `Nat`, in `Type 0`, but a call to `Pick` is typed by `Pick`'s codomain, `Type 1`, which the other arm's quantifier over `Type` needs. `three`'s type is that call, so `same`'s `@A` meets it as written; solved by that spelling, `A` would sit in `Type 1`, where `Nat`'s `Eql` witness, at `Type 0`, then asks it to be strictly below itself. The solver commits the reduct instead, which names the type at its own universe.
#[test]
fn a_type_named_through_a_higher_universe_solves_at_its_own() {
    let source = r#"
        use /std/{Bool, Nat};
        use /std/ops/{Eql};

        let Pick(b: Bool) -> Type =
            match b
            | true => Nat
            | false => (A: Type) -> A
            end;
        let same(@A: Type, use Eql(A), x: A, y: A) -> Bool = x == y;
        let three: Pick(true) = 3;

        /std/print(Bool/to_str(same(three, 3)))
        "#;

    assert_eq!(run(source), b"true");
}

// **A level a witness's side condition bounds settles at zero.** `Value` carries a type, so its level is its caller's, and `one` and `plain` keep it. `all` has both a local definition over `Value` and a `Monad` witness resolved in its body: the witness requires the error type's level to be at most the payload's, the error type's is floored at zero and still open when the signature's occurrences settle, and the payload's level takes it as its bound and follows it to zero. Following a bound down is what settles `Io(List(Str))` at zero, so the order is not the defect; what would tell the two apart is where each bound came from. The roadmap's "A universe level settled before its evidence is in" holds the gap, and this pins the loss so that closing it flips a fixture.
#[test]
fn a_level_bounded_through_a_witnesses_side_condition_settles_at_zero() {
    let source = r#"
        use /std/{Str, List, Result};
        pub struct Value: pub Type { A: Type, read: (Str) -> Result(Str, A) }
        let all(v: Value, xs: List(Str)) -> Result(Str, List(v.A)) =
            let read_with(w: Value, s: Str) -> Result(Str, w.A) =
                Result/map_failure(w.read(s), (reason) => Str/concat("bad: ", reason));
            List/traverse(xs, (s) => read_with(v, s));
        let one(v: Value, xs: List(Str)) -> Result(Str, List(v.A)) =
            let read_with(w: Value, s: Str) -> Result(Str, w.A) =
                Result/map_failure(w.read(s), (reason) => Str/concat("bad: ", reason));
            match xs
            | [] => Result/success([])
            | [s, .._] => Result/map_success(read_with(v, s), (a) => [a])
            end;
        let plain(v: Value, xs: List(Str)) -> Result(Str, List(v.A)) =
            List/traverse(xs, (s) => v.read(s));
        /std/print("settled")
        "#;

    let parameters = universe_parameters(source);

    assert_eq!(
        parameters.get("/all"),
        Some(&0),
        "a level bounded through a witness's side condition kept its caller's choice, so the gap this pins has closed: {parameters:?}",
    );
    assert_eq!(
        parameters.get("/one"),
        Some(&1),
        "a local definition alone took the level, so the control no longer separates the two: {parameters:?}",
    );
    assert_eq!(
        parameters.get("/plain"),
        Some(&1),
        "a witness alone took the level, so the control no longer separates the two: {parameters:?}",
    );
}
