//! Where a type is sized: a type former by its parts as written, a field and a parameter at their written type's level, an open element at its birth, and a stuck match, a recursive group and a cycle of levels at what they hold.

use {
    crate::{test_support::*, tests::universes::test_support::*},
    curios_common::fill,
};

/// A recursive predicate over a list of any element type, its list type the one the hole `list` names, beside a predicate it is called with: a call's implicit written makes the list type a declaration states meet the one the callee states as two calls of one global, which joins the declaration's instance to the callee's, held where conversion compares it.
const EVERY_OVER: &str = "pub let Wrap(T: Type) -> Type => List(T);
let every(@T: Type, l: ${list}(T), pred: (T) -> Bool) -> Bool =>
    match l
    | [] => true
    | [h, ..t] => match pred(h) | true => every(t, pred) | false => false end
    end;
let small(n: Nat) -> Bool => n < 10;";

// **A type's spelling carries a universe its value need not.** `Pick(true)` is `Nat`, in `Type 0`, but a call to `Pick` is typed by `Pick`'s codomain, `Type 1`, which the other arm's quantifier over `Type` needs. `three`'s type is that call, so `same`'s `@A` meets it as written; solved by that spelling, `A` would sit in `Type 1`, where `Nat`'s `Eql` witness, at `Type 0`, then asks it to be strictly below itself. The solver commits the reduct instead, which names the type at its own universe.
#[test]
fn a_type_named_through_a_higher_universe_solves_at_its_own() {
    let source = r#"
        use /std/{Bool, Nat};
        use /std/ops/{Eql};

        let Pick(b: Bool) -> Type =>
            match b
            | true => Nat
            | false => (A: Type) -> A
            end;
        let same(@A: Type, use Eql(A), x: A, y: A) -> Bool => x == y;
        let three: Pick(true) = 3;

        /std/print(Bool/to_str(same(three, 3)))
        "#;

    assert_eq!(run(source), b"true");
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
        fill!(
            r#"
            use /std/{Bool, Nat, Str, List, Result};
            ${declarations}
            pub let small() -> ${type_} => ${small};
            pub let big() -> ${type_} => ${big};
            pub let takes(p: ${type_}) -> Nat => 0;
            ${pick}
            /std/print("settled")
            "#,
            declarations,
            type_,
            small,
            big,
            pick
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
            "pub let pick(b: Bool) -> {type_} => match b | true => small() | false => big() end;"
        );
        assert_eq!(
            typecheck(&program(declarations, type_, small, big, &pick)),
            Ok(()),
            "{type_}"
        );
    }
}

// **A type former sits where its parts are typed, which is where the certifier types it.** A function type or a tuple type over a part whose written type is above its reduct's — a closed match typed by its goal, a call typed by its codomain, an associated type named through a witness at a level of its own — is sized from the part as typing read it. Sized from the part's reduct it sat a level short, and the certifier refused the sort the elaborator declared: `Over` at `Type` over a domain typed at its witness's level, and `listed`'s `List` at zero over a record of one.
#[test]
fn a_type_former_is_sized_by_its_parts_as_they_are_written() {
    let source = r#"
        use /std/{Nat, Bool, Str, List};
        let over_match(A: Type, B: Type) -> Type =>
            (match true | true => A | false => B end) -> Nat;
        let pair_match(A: Type, B: Type) -> Type =>
            {held: match true | true => A | false => B end, n: Nat};
        let Pick(b: Bool) -> Type => match b | true => Nat | false => (A: Type) -> A end;
        let over_call() -> Type => (Pick(true)) -> Nat;
        pub concept Sized(T: Type): pub Type { Carrier: Type }
        satisfy Sized(Nat) { Carrier = Str }
        pub let Over: Type = (x: Sized/Carrier(@Nat)) -> Nat;
        pub let listed(xs: List({a: Sized/Carrier(@Nat), n: Nat})) -> Nat => 0;
        /std/print("sized")
        "#;

    assert_eq!(run(source), b"sized");
}

// A sort an implicit is left to closes as the `Type` written at it does. A call that leaves the implicit out elaborates to the scheme its written twin has, in a signature over two type binders, in a signature over closed types, and in a body that answers a type: the sort is a level of its own over what it met, with the provenance of the level it copies, where solved by the equation it was the first argument's sort.
#[test]
fn a_sort_left_to_an_implicit_closes_as_the_type_written_there() {
    let source = r#"
        use /std/{Eq, Nat, Bool};
        let lemma_inferred(A: Type, B: Type, p: Eq()(A, B)) -> Nat => 0;
        let lemma_written(A: Type, B: Type, p: Eq(@Type)(A, B)) -> Nat => 0;
        let stated_inferred(p: Eq()(Nat, Bool)) -> Nat => 0;
        let stated_written(p: Eq(@Type)(Nat, Bool)) -> Nat => 0;
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

// An empty list literal is typed before anything says what it holds, so its type is a list of an open element, and each implicit that type is handed to, `id`'s and then `first`'s, is sized from it there. The open element is read at the level its own sort states, a bound the solver keeps once the element is known. Read at zero, each implicit closed at zero under an element at the declaration's own level, and the certifier refused a call the elaborator had admitted.
#[test]
fn a_type_over_an_open_element_is_sized_by_the_level_the_element_is_born_at() {
    let source = r#"
        use /std/{Nat, List, Str};
        let id(@A: Type, a: A) -> A => a;
        let first(@A: Type, x: A, pin: A) -> A => x;
        let second(@A: Type, pin: A, x: A) -> A => x;
        let before(@T: Type, x: T) -> List(T) => first(id([]), [x]);
        let after(@T: Type, x: T) -> List(T) => second([x], id([]));
        let bound(@T: Type, x: T) -> List(T) =>
            let empty = id([]);
            first(empty, [x]);
        /std/print(Nat/to_str(List/length(before(1)) + List/length(after("a")) + List/length(bound(Type))))
        "#;

    assert_eq!(run(source), b"0");
}

// A domain is sized at the level typing read it at, which is where the certifier types it, and not at its reduct's. `List(Nat)` is written at the level of `/sys/List`'s instance and reduces to a node at `0`, and `Wrap(Nat)` is written at the level of `Wrap`'s; with the call's implicit written, that instance is the struct's own parameter, and read off the reduct the field stood above the struct's sort, which the certifier refused. Each struct certifies, its implicit written and left out.
#[test]
fn a_field_is_sized_at_the_level_its_written_type_stands_at() {
    for list in ["List", "Wrap"] {
        for implicit in ["@Nat, ", ""] {
            let every = fill!(EVERY_OVER, list);
            let source = fill!(
                r#"use /std/{Nat, List, Bool};
use /std/Bool/{Holds};

${every}

pub struct G: Type {
    chars: ${list}(Nat),
    @ok: Holds(every(${implicit}chars, small)),
}

/std/print("sized")"#,
                every,
                list,
                implicit,
            );

            assert_eq!(typecheck(&source), Ok(()), "{list}, `{implicit}`");
        }
    }
}

// A parameter is sized as a field is: a struct's at the level its former's type was elaborated at, and a family's at the level each constructor's telescope reads it again at.
#[test]
fn a_parameter_is_sized_at_the_level_its_written_type_stands_at() {
    for implicit in ["@Nat, ", ""] {
        let every = fill!(EVERY_OVER, list = "List");
        let source = fill!(
            r#"use /std/{Nat, List, Bool};
use /std/Bool/{Holds};

${every}

pub struct S(xs: List(Nat)): Type {
    @ok: Holds(every(${implicit}xs, small)),
}

pub induct I(xs: List(Nat)): Type
| made(Holds(every(${implicit}xs, small)))
end

/std/print("sized")"#,
            every,
            implicit,
        );

        assert_eq!(typecheck(&source), Ok(()), "`{implicit}`");
    }
}

// **A body's cycle of levels closes at its least solution.** A `!` region's monad is read at one sort for its domain and codomain, the `Try` witness bounds the payload's level by the base's, and a payload above level zero bounds the domain from below: `max(u, v) ≤ v`, `v ≤ u` and `1 ≤ v`, where neither level has a principal bound while the other is open. The floors are read round after round, so the pair settles at one and `read` takes no parameter. Floored once from zero, the two were generalized as parameters nothing could choose, and the program's entry, which generalizes nothing, was refused for a level it could not close.
#[test]
fn a_bodys_cycle_of_levels_closes_at_its_least_solution() {
    let source = r#"
        use /std/{Try, Io, Str, Bool, Nat};
        let probe() -> Try(Io, Str, Bool) => Try/pure(true);
        pub let read() -> Try(Io, Str, {}) =>
            let _ = probe()!;
            let _ = Try/pure(Nat)!;
            Try/pure(());
        read()
        "#;
    assert_eq!(typecheck(source), Ok(()));
    let parameters = universe_parameters(source);
    assert_eq!(parameters.get("/read"), Some(&0), "{parameters:?}");
}

// **A stuck `match` holds the levels its arms reduce to.** `M` holds a proposition `Good` computes by a `match` on `M`'s own field, which no instance decides: read through its arms, opened and forced, `List/length`'s instance in the `some` arm is the `#ListLen` it reduces to and holds no level, so `M` keeps the one level a family stating the same would, and `Good`'s own vector holds none. Read closed, the instance held its level, and `M` took a second one, invariant. The control's arm answers a `Type`, which holds the level it stands at.
#[test]
fn a_stuck_match_holds_the_levels_its_arms_reduce_to() {
    let source = r#"
        use /std/{Nat, List, Option, Bool};
        pub let Good(@V: Type, r: Option(List(V))) -> Prop =>
            match r | none() => Bool/True | some(l) => Bool/Holds(List/length(l) < 9) end;
        pub struct M(V: Type): pub Type { root: Option(List(V)), @ok: Good(root) }
        pub let Sized(@V: Type, r: Option(List(V))) -> Type =>
            match r | none() => {} | some(_) => Type end;
        /std/print("held")
        "#;

    // Run whole, the program is certified too, which recomputes `M`'s vector and refuses an irrelevance it would deny.
    assert_eq!(run(source), b"held");
    let parameters = universe_parameters(source);
    assert_eq!(parameters.get("/M"), Some(&1), "{parameters:?}");
    let families = family_variances(source);
    assert_eq!(
        families.get("/M").map(String::as_str),
        Some("*"),
        "{families:?}"
    );
    let definitions = definition_variances(source);
    assert!(
        !definitions["/Good"].contains('='),
        "a level only a closed arm held: {definitions:?}"
    );
    assert!(
        definitions["/Sized"].contains('='),
        "a level an arm answers at: {definitions:?}"
    );
}

// **A recursive group holds the levels its unfolding holds.** `Held` holds a proposition `Ok` and `All` compute together, each meeting the other on its path: read by the least fixpoint of the group's unfolding, the list `match`'s element type `Rose(V)` is an annotation that holds no level, so `Held` keeps the one level a family stating the same would, irrelevant; `B` over the recursive `Balanced` alike. Read with every level the group mentions held, each took a second level, invariant. `Ok`, `All` and `Balanced` file their one level irrelevant, read off each member's projection, so the elaborator compares two instances of one as the certifier compares the projections they unfold to.
#[test]
fn a_recursive_group_holds_the_levels_its_unfolding_holds() {
    let source = r#"
        use /std/{List, Bool};
        pub induct Rose(V: Type): pub Type
        | leaf(v: V)
        | node(kids: List(Rose(V)))
        end
        pub let Ok(@V: Type, t: Rose(V)) -> Prop =>
            match t | leaf(_) => Bool/True | node(kids) => All(kids) end
        and All(@V: Type, l: List(Rose(V))) -> Prop =>
            match l | [] => Bool/True | [x, ..rest] => {Ok(x), All(rest)} end;
        pub struct Held(V: Type): pub Type { t: Rose(V), @ok: Ok(t) }
        pub induct Tree(V: Type): pub Type
        | tip(v: V)
        | fork(l: Tree(V), r: Tree(V))
        end
        pub let Balanced(@V: Type, t: Tree(V)) -> Prop =>
            match t | tip(_) => Bool/True | fork(l, r) => {Balanced(l), Balanced(r)} end;
        pub struct B(V: Type): pub Type { t: Tree(V), @ok: Balanced(t) }
        /std/print("held")
        "#;

    // Run whole, the program is certified too, which recomputes each vector and refuses an irrelevance it would deny.
    assert_eq!(run(source), b"held");
    let parameters = universe_parameters(source);
    let families = family_variances(source);
    for family in ["/Held", "/B"] {
        assert_eq!(parameters.get(family), Some(&1), "{family}: {parameters:?}");
        assert_eq!(
            families.get(family).map(String::as_str),
            Some("*"),
            "{family}: {families:?}"
        );
    }
    let definitions = definition_variances(source);
    for definition in ["/Ok", "/All", "/Balanced"] {
        assert_eq!(
            definitions.get(definition).map(String::as_str),
            Some("*"),
            "{definition}: {definitions:?}"
        );
    }
}

// **A recursive member that answers no type holds no level of its group in what it answers.** `every` is `/std/List/all`'s shape: a recursion over a list answering a `Bool`, so its projection mentions its group's levels and its answer holds none. Read off its group's body as written beneath its own binders, where its call to itself is the group's own local and carries no level — the least fixpoint of the group's unfolding conversion reads two projections by, with nothing reduced — its vector claims each level irrelevant, so `every_cons`, which names `every` at two levels, settles them at their least values and declares no more parameters than `every` does. Read as written, the projection held every level of its group, and `every_cons` kept both.
#[test]
fn a_recursive_member_answering_no_type_holds_no_level_of_its_group() {
    let source = r#"
        use /std/{Bool, List};
        pub let every(@T: Type, l: List(T), pred: (T) -> Bool) -> Bool =>
            match l
            | [] => true
            | [h, ..t] => match pred(h) | true => every(t, pred) | false => false end
            end;
        pub let every_cons(
            @T: Type,
            pred: (T) -> Bool,
            h: T,
            t: List(T),
            first: Bool/Holds(pred(h)),
            rest: Bool/Holds(every(t, pred)),
        ) -> Bool/Holds(every([h, ..t], pred)) =>
            match pred(h) | true => rest | false => match first end end;
        /std/print("every")
        "#;

    let parameters = universe_parameters(source);
    assert_eq!(
        parameters.get("/every_cons"),
        parameters.get("/every"),
        "a declaration naming a recursive member at two levels kept a level the member's answer holds none of: {parameters:?}",
    );
}
