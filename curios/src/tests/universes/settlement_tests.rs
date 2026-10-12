//! Which of a declaration's levels its callers choose, and where every other one lands.

use {
    super::test_support::{definition_variances, signatures, universe_parameters},
    crate::tests::{run, typecheck},
};

// A universe parameter is minted only from a declaration's *interface* — its type and the registry signatures a use site instantiates. A level reachable only through the body is minimized to a constant instead (`UniverseSolver::finalize`'s `internal` set, which no occurrence could choose a value for). So `carrier`'s one parameter is the level its result holds, `Box`'s own, which the family is invariant in and a caller may need higher, and the `Type` it stores is no second one; `holder` takes that one beside the level of its `Type`-sorted parameter.
//
// That is no bound on what a value carries. A parameter's own sort reaches a payload through an implicit argument, `lifted` storing `Type u` for its `S: Type u`, so its vector reads both of its levels held where `holder`'s reads its parameter's irrelevant, and two instances of `lifted` at one argument are two values: the pair [Case equations and their key](../../../documentation/design/soundness/case-equations-and-their-key.md) keeps a key's levels for.
//
// The counts are asserted rather than a printout matched, because the parameter count is the thing the argument turns on and a printer is free to render it differently.
#[test]
fn a_body_carried_level_is_minimized_rather_than_generalized() {
    let source = r#"
        use /std/{Nat, Str};
        induct Box : pub Type
        | wrap(Type)
        end
        induct Any : pub Type
        | any(@A: Type, a: A)
        end
        let carrier : Box = Box/wrap(Type);
        let holder(@A : Type) -> Box = Box/wrap(A);
        let lifted(S : Type) -> Any = Any/any(S);
        /std/print("both")
        "#;

    let parameters = universe_parameters(source);

    assert_eq!(
        parameters.get("/carrier"),
        Some(&1),
        "a body-carried level became a parameter a use site can choose, or the level its result holds stopped being one: {parameters:?}",
    );
    assert_eq!(
        parameters.get("/holder"),
        Some(&2),
        "an interface level stopped generalizing, so the control no longer separates the two: {parameters:?}",
    );

    let held = definition_variances(source);

    assert_eq!(
        held.get("/holder").map(String::as_str),
        Some("*="),
        "a value that stores its parameter came to hold that parameter's sort, or stopped holding its family's level: {held:?}",
    );
    assert_eq!(
        held.get("/lifted").map(String::as_str),
        Some("=="),
        "a value that stores its parameter's sort stopped holding it: {held:?}",
    );
}

// **A family's level is its callers' where a parameter is typed at it or an instance holds it, and no other is.** `Tree`'s payload names `List` over the family itself, at a level nothing but the family's own sort bounds. `List` is irrelevant in it, so no two instances of `Tree` would differ in it, and it is minimized where its group closes: `Tree` takes no level, and neither does a signature that names it. `Wrap` keeps the level its parameter is typed at, and `Kept` the one its field holds.
#[test]
fn a_family_takes_no_level_its_instances_cannot_differ_in() {
    let source = r#"
        use /std/{Str, List};
        pub induct Tree: pub Type | leaf(Str) | node(List(Tree)) end
        pub induct Wrap(A: Type): pub Type | wrap(a: A, rest: List(Wrap(A))) end
        pub struct Kept: pub Type { A: Type }
        pub let leaves(t: Tree) -> List(Tree) = [t];
        /std/print("sorted")
        "#;

    let parameters = universe_parameters(source);
    for (name, count) in [("/Tree", 0), ("/leaves", 0), ("/Wrap", 1), ("/Kept", 1)] {
        assert_eq!(
            parameters.get(name),
            Some(&count),
            "a family took a level no instance of it holds, or lost one that types a parameter or that an instance holds: {parameters:?}",
        );
    }
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

// **A level a signature holds where conversion compares it stays its caller's, whatever its body bounds it by.** `Value` carries a type and is invariant in its level, so `v`'s is the declaration's parameter. `all` has both a local definition over `Value` and a `Monad` witness resolved in its body: the witness requires the error type's level to be at most the payload's, which leaves the payload's level a lower bound, and an occurrence nothing holds follows such a bound down, as `Io(List(Str))` follows its payload to zero. A level held by its position does not: the bound is the declaration's constraint. `one` and `plain`, each with one of the two, are the controls.
#[test]
fn a_level_a_signature_holds_stays_its_callers_through_a_bound_from_the_body() {
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

    for name in ["/all", "/one", "/plain"] {
        assert_eq!(parameters.get(name), Some(&1), "{name}: {parameters:?}");
    }
}

// **A result keeps its level where conversion compares it.** A tuple type's field, a former's argument and a level a family is invariant in are compared for equality, so no rule lifts a `small()` settled at its floor to the type `big()` answers at. Each is the declaration's parameter with its floor as a constraint, as the same type written as a parameter's is in `takes`, and a function answering either instantiates both at one level.
#[test]
fn a_result_at_a_compared_position_keeps_its_level() {
    let value = "pub struct Value: pub Type { A: Type, read: (Str) -> Result(Str, A) }";
    let rows = [
        ("", "{A: Type, a: A}", "(Nat, 0)", "(Type, Nat)"),
        ("", "List(Type)", "[Nat]", "[Type]"),
        (
            value,
            "Value",
            "Value { A = Nat, read = (_) => Result/success(0) }",
            "Value { A = Type, read = (_) => Result/success(Nat) }",
        ),
    ];
    let program = |declarations: &str, type_: &str, small: &str, big: &str, pick: &str| {
        format!(
            r#"
            use /std/{{Bool, Nat, Str, List, Result}};
            {declarations}
            pub let small() -> {type_} = {small};
            pub let big() -> {type_} = {big};
            pub let takes(p: {type_}) -> Nat = 0;
            {pick}
            /std/print("settled")
            "#
        )
    };

    for (declarations, type_, small, big) in rows {
        let parameters = universe_parameters(&program(declarations, type_, small, big, ""));
        for name in ["/small", "/big", "/takes"] {
            assert_eq!(
                parameters.get(name),
                Some(&1),
                "{name}: {type_}: {parameters:?}"
            );
        }

        let pick = format!(
            "pub let pick(b: Bool) -> {type_} = match b | true => small() | false => big() end;"
        );
        assert_eq!(
            typecheck(&program(declarations, type_, small, big, &pick)),
            Ok(()),
            "{type_}"
        );
    }
}

// **A type-valued definition's body level is its callers' where its body holds it.** `Holder`'s body names `Value` at an occurrence of its own, which nothing in the source ties to its parameter's. `Value` is invariant in its level, so an occurrence of `Holder` unfolds to a record that holds it, and it is `Holder`'s second parameter; `hold` builds that record from its argument, where the two are one level, and takes a `Value` whose `A` is `Type`. `Types` keeps the level of the `Type` it lists, and the recursive `Tower` those of its two occurrences of `Value`, `top` following it.
#[test]
fn a_type_valued_definitions_body_level_is_its_callers() {
    let program = |use_: &str| {
        format!(
            r#"
            use /std/{{Str, Nat, List, Result}};
            pub struct Value: pub Type {{ A: Type, read: (Str) -> Result(Str, A) }}
            pub let Holder(v: Value) -> Type = {{held: Value, tag: Nat}};
            pub let hold(v: Value) -> Holder(v) = (held = v, tag = 0);
            pub let Types: Type = List(Type);
            pub let Tower(n: Nat) -> Type = match n | 0 => Value | k + 1 => {{head: Value, rest: Tower(k)}} end;
            pub let top(v: Value) -> Tower(0) = v;
            {use_}
            /std/print("held")
            "#
        )
    };

    let parameters = universe_parameters(&program(""));
    for (name, count) in [
        ("/Holder", 2),
        ("/hold", 1),
        ("/Types", 1),
        ("/Tower", 2),
        ("/top", 2),
    ] {
        assert_eq!(
            parameters.get(name),
            Some(&count),
            "a level a type-valued definition's body holds stopped being its callers', or one it does not hold became theirs: {parameters:?}",
        );
    }

    let large = r#"let large: Value = Value { A = Type, read = (_) => Result/success(Nat) };
        pub let held: Holder(large) = hold(large);"#;
    assert_eq!(typecheck(&program(large)), Ok(()));
}

// **The library's instances.** `/std/Cli` describes and parses a spec at any level: `help` prints a spec whose reader answers a type, and `parse` walks it, its private `Claim` alias keeping the levels of the `Arg` its body names. The stock readers keep their level, a `Value` being invariant in it, so a spec holds `nat` beside a reader of a type.
#[test]
fn the_command_line_parser_takes_a_spec_at_any_universe_instance() {
    let program = |body: &str| {
        format!(
            r#"
            use /std/{{Str, Nat, List, Result, Option, Io}};
            use /std/Cli/{{Value, Arg, Cli, Outcome, reader, option, nat, parse, help}};
            let large: Value = reader(@Type, "T", (_) => Result/success(Nat));
            let spec: List(Arg) = [option("t", large, "a type")];
            {body}
            "#
        )
    };

    let described = program(
        r#"let cli: Cli = Cli/leaf("probe", "a probe", Option/none(), spec, (_) => Io/pure(()));
        /std/print(help([], cli))"#,
    );
    let printed = String::from_utf8(run(&described)).expect("help prints text");
    assert!(printed.contains("--t <T>"), "got: {printed}");

    let mixed = program(
        r#"pub let mixed: List(Arg) = [option("n", nat, "a number"), option("t", large, "a type")];
        /std/print("mixed")"#,
    );
    assert_eq!(typecheck(&mixed), Ok(()));

    let parsed = program(
        r#"pub let parsed: Outcome(spec) = parse(spec, [], Option/none());
        /std/print("parsed")"#,
    );
    assert_eq!(typecheck(&parsed), Ok(()));
}

// **A weak equation decides no level a signature holds.** Two instances of a family meet at a position it is irrelevant in wherever a small action is sequenced into a larger region or an arm answers at an instance of its own, and the pair is joined where the declaration closes. Joined to a level the signature holds, it would decide that level for every caller. `both`'s arms each answer at their own constructor's level: joined through the declared result they make `Kind`'s two levels one, which a caller holding a `Kind` at two cannot pass. `counted`'s region is a `State` at its payload's level and the action it sequences sits at zero: the two joined hold `A` to zero, and `counted` at a type is refused. Each such pair is dropped, and the occurrence it would have joined settles from its own bounds.
#[test]
fn a_weak_equation_decides_no_level_a_signature_holds() {
    let arms = r#"
        use /std/{Str, Result};
        pub struct Value: pub Type { A: Type, read: (Str) -> Result(Str, A) }
        pub induct Kind: pub Type
        | one(v: Value)
        | two(v: Value)
        end
        let Carrier(k: Kind) -> Type = match k | one(v) => v.A | two(v) => v.A end;
        let read_hoisted(v: Value, s: Str) -> Result(Str, v.A) = v.read(s);
        pub let both(k: Kind, s: Str) -> Result(Str, Carrier(k)) =
            match k
            | one(v) => read_hoisted(v, s)
            | two(v) => read_hoisted(v, s)
            end;
        /std/print("arms")
        "#;
    let parameters = universe_parameters(arms);
    assert_eq!(parameters.get("/both"), Some(&2), "{parameters:?}");

    let region = r#"
        use /std/{Nat, State};
        let tick: State(Nat, {}) = State/modify((n) => n + 1);
        pub let counted(@A: Type, a: A) -> State(Nat, A) =
            let _ = tick!;
            State/pure(a);
        pub let at_type: State(Nat, Type) = counted(Nat);
        /std/print("region")
        "#;
    assert_eq!(typecheck(region), Ok(()));
    let parameters = universe_parameters(region);
    assert_eq!(parameters.get("/counted"), Some(&1), "{parameters:?}");
}

// **A level nothing holds settles at its least value.** An occurrence's level that stands only where conversion compares nothing is no caller's to choose. `parameters` answers an `Option` of a record through a local definition answering the same, each `Option`'s level bounded by the other's alone, and the witness of `Source` over `Io` names a bare `Io` at a level nothing bounds from below. Each settles at zero, and no parameter is carried for it. Nothing reads such a level, so a region at a larger one is still sequenced through the witness.
#[test]
fn a_level_nothing_holds_settles_at_its_least_value() {
    let source = r#"
        use /std/{Nat, Io, Monad, List, Option};
        pub concept Source(M: (Type) -> Type, S: Type): pub Type {
            pull(s: S) -> M(Nat),
        }
        satisfy Source(Io, Nat) {
            pull(n) = Io/pure(n),
        }
        pub let drained(@M: (Type) -> Type, @S: Type, use Monad(M), use Source(M, S), s: S) -> M(Nat) =
            Source/pull(s);
        pub let large: Io(Type) =
            let _n = drained(3)!;
            Io/pure(Nat);
        pub let parameters(n: Nat) -> Option({nums: List(Nat), final: Nat}) =
            let go(j: Nat) -> Option({nums: List(Nat), final: Nat}) =
                match j
                | 0 => Option/none()
                | m + 1 => go(m)
                end;
            go(n);
        /std/print("least")
        "#;

    let parameters = universe_parameters(source);
    for name in ["/parameters", "the witness in the entry module"] {
        assert_eq!(parameters.get(name), Some(&0), "{name}: {parameters:?}");
    }
    assert_eq!(run(source), b"least");
}

// **A declaration under two premises over one carrier is judged alike by both checkers.** `both` builds a `Map` under its `Key(K)` premise and sorts the map's keys under its `Ord(K)` one, each premise a witness with levels of its own over the one `K`.
#[test]
fn a_declaration_under_a_key_and_an_order_over_one_carrier_is_judged_alike() {
    let source = r#"
        use /std/{Nat, List, Map, Ord};
        pub let both(@K: Type, use Map/Key(K), use Ord(K), keys: List(K)) -> List(K) =
            let m: Map(K, Nat) = Map/of(List/map(keys, (k: K) => (k, 0)));
            List/sort(Map/keys(m));
        /std/print("both")
        "#;

    assert_eq!(run(source), b"both");
}

// **A type former sits where its parts are typed, which is where the kernel types it.** A function type or a tuple type over a part whose written type is above its reduct's — a closed match typed by its motive, a call typed by its codomain, an associated type named through a witness at a level of its own — is sized from the part as typing read it. Sized from the part's reduct it sat a level short, and the kernel refused the sort the elaborator declared: `Over` at `Type` over a domain typed at its witness's level, and `listed`'s `List` at zero over a record of one.
#[test]
fn a_type_former_is_sized_by_its_parts_as_they_are_written() {
    let source = r#"
        use /std/{Nat, Bool, Str, List};
        let over_match(A: Type, B: Type) -> Type =
            (match true: (_) => Type | true => A | false => B end) -> Nat;
        let pair_match(A: Type, B: Type) -> Type =
            {held: match true: (_) => Type | true => A | false => B end, n: Nat};
        let Pick(b: Bool) -> Type = match b | true => Nat | false => (A: Type) -> A end;
        let over_call() -> Type = (Pick(true)) -> Nat;
        pub concept Sized(T: Type): pub Type { Carrier: Type }
        satisfy Sized(Nat) { Carrier = Str }
        pub let Over: Type = (x: Sized/Carrier(@Nat)) -> Nat;
        pub let listed(xs: List({a: Sized/Carrier(@Nat), n: Nat})) -> Nat = 0;
        /std/print("sized")
        "#;

    assert_eq!(run(source), b"sized");
}

// A sort an implicit is left to closes as the `Type` written at it does. A call that leaves the implicit out elaborates to the scheme its written twin has, in a signature over two type binders, in a signature over closed types, and in a body that answers a type: the sort is a level of its own over what it met, with the provenance of the level it copies, where solved by the equation it was the first argument's sort.
#[test]
fn a_sort_left_to_an_implicit_closes_as_the_type_written_there() {
    let source = r#"
        use /std/{Eq, Nat, Bool};
        let lemma_inferred(A: Type, B: Type, p: Eq()(A, B)) -> Nat = 0;
        let lemma_written(A: Type, B: Type, p: Eq(@Type)(A, B)) -> Nat = 0;
        let stated_inferred(p: Eq()(Nat, Bool)) -> Nat = 0;
        let stated_written(p: Eq(@Type)(Nat, Bool)) -> Nat = 0;
        let answered_inferred: Prop = Eq()(Nat, Bool);
        let answered_written: Prop = Eq(@Type)(Nat, Bool);
        /std/print("twins")
        "#;

    let signatures = signatures(source);
    let parameters = universe_parameters(source);

    for twin in ["lemma", "stated", "answered"] {
        let (inferred, written) = (format!("/{twin}_inferred"), format!("/{twin}_written"));
        assert_eq!(
            signatures.get(&inferred),
            signatures.get(&written),
            "{signatures:?}"
        );
        assert_eq!(
            parameters.get(&inferred),
            parameters.get(&written),
            "{parameters:?}"
        );
    }
    assert_eq!(
        parameters.get("/lemma_inferred"),
        Some(&1),
        "{parameters:?}"
    );
}

// An empty list literal is typed before anything says what it holds, so its type is a list of an open element, and each implicit that type is handed to, `id`'s and then `first`'s, is sized from it there. The open element is read at the level its own sort states, a bound the solver keeps once the element is known. Read at zero, each implicit closed at zero under an element at the declaration's own level, and the kernel refused a call the elaborator had admitted.
#[test]
fn a_type_over_an_open_element_is_sized_by_the_level_the_element_is_born_at() {
    let source = r#"
        use /std/{Nat, List, Str};
        let id(@A: Type, a: A) -> A = a;
        let first(@A: Type, x: A, pin: A) -> A = x;
        let second(@A: Type, pin: A, x: A) -> A = x;
        let before(@T: Type, x: T) -> List(T) = first(id([]), [x]);
        let after(@T: Type, x: T) -> List(T) = second([x], id([]));
        let bound(@T: Type, x: T) -> List(T) =
            let empty = id([]);
            first(empty, [x]);
        /std/print(Nat/to_str(List/len(before(1)) + List/len(after("a")) + List/len(bound(Type))))
        "#;

    assert_eq!(run(source), b"0");
}
