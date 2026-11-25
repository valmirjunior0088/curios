//! How a group is judged and computes in a type: a local recursion and an applied call in a type, a family read as a relation, and what a judgment inside a group reads of its members — by value, withheld, typed at a sibling, in a signature and in a slot.

use {crate::test_support::*, curios_common::fill};

/// A local recursive definition computes in a type stated in the function that declares it, as a top-level one does anywhere and a `let`-bound value does in its tail: both checkers read a member as its group there. `_held` leaves `Eq/refl`'s implicit to be solved, to a term that spells `halve` by value, and `_one_body` puts two definitions of one body to each other. The certifier typed a group's tail over members that stood for nothing, and refused each of the five where the elaborator accepted it.
#[test]
fn a_local_recursive_definition_computes_in_a_type_where_it_is_declared() {
    let source = r#"
        use /std/{Nat, Eq};
        let through(n : Nat) -> Nat =
            let go(k : Nat) -> Nat = match k | 0 => 0 | p + 1 => go(p) + 2 end;
            let halve(k : Nat) -> Nat = match 0 < k | true => halve(k / 2) + 1 | false => 0 end;
            let again(k : Nat) -> Nat = match 0 < k | true => again(k / 2) + 1 | false => 0 end;
            let _closed : Eq()(go(3), 6) = Eq/refl();
            let _one_step : Eq()(go(n + 1), go(n) + 2) = Eq/refl();
            let _guarded : Eq()(halve(4), 3) = Eq/refl();
            let _held : Eq()(halve(n) + 1, 1 + halve(n)) = Eq/refl();
            let _one_body : Eq()(halve(n), again(n)) = Eq/refl();
            go(n);
        /std/print(Nat/to_str(through(3)))
        "#;

    assert_eq!(run(source), b"6");
}

/// A recursive definition whose result is a function computes when applied past its own parameters, at the top level, locally, and as the loop `Nat/descend` runs when its family is a function: `f(3)(1)` is the call `f(3)` applied to what is left, and a demand for its value unfolds that call rather than stopping at the outer application. A symbolic argument past the parameters rides along, so the last claim is a law rather than a sample.
#[test]
fn a_recursive_call_applied_past_its_parameters_computes_in_a_type() {
    let source = r#"
        use /std/{Nat, Eq};
        use /std/Bool/{True};
        let top(n : Nat) -> (Nat) -> Nat =
            match n | 0 => (x) => x | p + 1 => (x) => top(p)(x) + 1 end;
        let nested(n : Nat) -> (Nat) -> Nat =
            let go(m : Nat) -> (Nat) -> Nat = match m | 0 => (x) => x | p + 1 => (x) => go(p)(x) + 1 end;
            go(n);
        let measured(n : Nat, k : Nat) -> Nat =
            Nat/descend(
                (x : Nat) => x,
                (_ : Nat) => (k : Nat) -> Nat,
                (x, ih) => (k) => match x | 0 => k | p + 1 => ih(p, True/qed())(k) + 1 end,
                n)(k);
        let _top : Eq()(top(3)(1), 4) = Eq/refl();
        let _nested : Eq()(nested(3)(1), 4) = Eq/refl();
        let _measured : Eq()(measured(3, 10), 13) = Eq/refl();
        let _symbolic(k : Nat) -> Eq()(top(2)(k), k + 2) = Eq/refl();
        /std/print(Nat/to_str(measured(3, 10)))
        "#;

    assert_eq!(run(source), b"13");
}

/// `Eq(@Nat)` is the relation `(Nat, Nat) -> Prop` with no lambda around it: an indexed family at its parameters is a family of its own, so it stands where a relation is expected and reflexivity inhabits its diagonal. Both checkers see the partial application, since the certifier re-checks the program.
#[test]
fn an_indexed_family_at_its_parameters_is_a_relation() {
    let source = r#"
        use /std/{Nat, Eq};
        let diagonal(R: (Nat, Nat) -> Prop) -> Prop = (n: Nat) -> R(n, n);
        let _reflexive: diagonal(Eq(@Nat)) = (n) => Eq/refl();
        /std/print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// A `rec` group whose member is reached through a *dependent* index family, at both the top level and inside a function.
//
// A member's name is defined to its slot while the group is being checked, so reduction turns a recursive reference into that slot and a committed solution can carry one. `RecItem::try_new` captures member *names* into the group's binder, so a slot reaching that point is not something the capture can bind. Both substitution walks spell a filled slot as its member's name, which is the capturable thing the group's binder is waiting for, and `elaborate_rec` materializes before it closes so the name lands where the capture still binds it; expanded to the member's body instead, the slot would meet the same solution in that body and the walk would never end — `recurse` answers a deepening walk by asking the allocator for stack, so the compilation would die by exhausting memory rather than refusing anything.
#[test]
fn a_rec_group_over_a_dependent_family_closes_without_expanding_its_own_members() {
    let source = r#"
        use /std/{Nat, Str, List};

        induct Shape: pub Type
        | leaf() | node(a: List(Shape))
        end

        induct Sizes: (Shape, Nat) -> pub Type
        | one(h: Nat): (Shape/leaf(), h)
        | many(@ps: List(Shape), @h: Nat, col: Column(ps, h)): (Shape/node(ps), h)
        | fitted(@s: Shape, @h0: Nat, h: Nat, inner: Sizes(s, h0)): (s, h)
        and Column: (List(Shape), Nat) -> pub Type
        | stop(): ([], 0)
        | part(
            @s: Shape,
            @rest: List(Shape),
            @h1: Nat,
            @h2: Nat,
            head: Sizes(s, h1),
            tail: Column(rest, h2),
          ): ([s, ..rest], h1 + h2)
        end

        let split(s: Shape, h: Nat) -> Sizes(s, h) =
            match s
            | leaf() => Sizes/one(h)
            | node(ps) =>
                let made = split_column(ps, h);
                Sizes/fitted(h, Sizes/many(made.col))
            end
        and split_column(ps: List(Shape), h: Nat) -> {th: Nat, col: Column(ps, th)} =
            match ps
            | [] => (th = 0, col = Column/stop())
            | [child, ..rest] =>
                let below = split_column(rest, h);
                (th = h + below.th, col = Column/part(split(child, h), below.col))
            end;

        let local(root: Shape, avail: Nat) -> Nat =
            let inner(s: Shape, h: Nat) -> Sizes(s, h) =
                match s
                | leaf() => Sizes/one(h)
                | node(ps) =>
                    let made = inner_column(ps, h);
                    Sizes/fitted(h, Sizes/many(made.col))
                end
            and inner_column(ps: List(Shape), h: Nat) -> {th: Nat, col: Column(ps, th)} =
                match ps
                | [] => (th = 0, col = Column/stop())
                | [child, ..rest] =>
                    let below = inner_column(rest, h);
                    (th = h + below.th, col = Column/part(inner(child, h), below.col))
                end;
            avail;

        /std/print(Nat/to_str(local(Shape/leaf(), 7)))
        "#;

    assert_eq!(run(source), b"7");
}

// A member not known to terminate stands for nothing in its group's bodies, as the certifier checks them: no judgment inside a body reads a member withheld, its own body or a sibling's, written before it or after it, at the top level or locally. `f` and `r` call themselves at `0`, nothing below their argument, and `h` closes a cycle with `g` that does not descend, so each is withheld at its turn and its bound stays undischarged. The elaborator once opened a member's slot into the body it was filled with, an earlier sibling's at once and its own or a later one's on retry, and so filled each bound here — each refused by the certifier.
#[test]
fn no_judgment_inside_a_group_reads_a_member_withheld_from_it() {
    let bounded = |member: &str, groups: &str| {
        let error = error(&fill!(
            r#"
            use /std/{Nat, List};
            ${groups}
            /std/print("")
            "#,
            groups,
        ));
        assert!(
            error.contains(&fill!(
                "nothing discharged `/std/Bool/Holds(${member}(0) < List/length(@Nat, [1, 2, 3]))`",
                member,
            )) && !error.contains("certifier refusal"),
            "{error}"
        );
    };

    bounded(
        "f",
        "let f(n: Nat) -> Nat = match n | 0 => 0 | _ + 1 => List/get([1, 2, 3], f(0)) end;",
    );
    bounded(
        "h",
        "let g(n: Nat) -> Nat = match n | 0 => 0 | _ + 1 => List/get([1, 2, 3], h(0)) end
        and h(n: Nat) -> Nat = match n | 0 => 0 | k + 1 => g(k) end;",
    );
    bounded(
        "r",
        "let outer(m: Nat) -> Nat =
            let r(n: Nat) -> Nat = match n | 0 => 0 | _ + 1 => List/get([1, 2, 3], r(0)) end;
            r(m);",
    );
}

// Which member of a cycle that does not descend is withheld follows the order the members are written in: `g`'s turn finds it known, its calls reaching only `h`, which stands for nothing, and `h`'s closes the cycle. So `h`'s body reads `g(0)` by value, and it is `0`, in a bound and in a proposition, both checkers alike.
#[test]
fn a_member_known_before_its_cycle_closes_is_read_by_value() {
    let source = |read: &str| {
        fill!(
            r#"
            use /std/{Nat, List};
            use /std/Bool/{True, Holds};
            let need(n: Nat, p: Holds(n == 0)) -> Nat = n;
            let g(n: Nat) -> Nat = match n | 0 => 0 | k + 1 => h(k) end
            and h(n: Nat) -> Nat = match n | 0 => 0 | _ + 1 => ${read} end;
            /std/print(Nat/to_str(h(1)))
            "#,
            read,
        )
    };

    assert_eq!(run(&source("List/get([1, 2, 3], g(0))")), b"1");
    assert_eq!(run(&source("need(g(0), True/qed())")), b"0");
}

// A member known to terminate is read by value in its group's bodies, as its tail reads it, both checkers alike: `T` calls itself below its argument, so its turn finds it known, `T(2)` is `Nat` where `val`'s body states it, and `f`'s binder, which its body restates at `T(0)` from its signature written as a function or as a lambda, is the `Nat` the signature reads. With the bodies reading `T` as standing for nothing, the elaborator refused the annotation and the function, and the certifier the lambda.
#[test]
fn a_member_typed_at_a_sibling_reads_it_by_value() {
    let program = |group: &str| {
        fill!(
            r#"
            use /std/{Nat};
            let outer(n: Nat) -> Nat =
                let T(m: Nat) -> Type = match m | 0 => Nat | k + 1 => T(k) end
                ${group}
            /std/print(Nat/to_str(outer(41)))
            "#,
            group,
        )
    };

    for group in [
        "and val: Nat = (() =>
            let x: T(2) = n;
            x)();
        val + 1;",
        "and f(x: T(0)) -> T(0) = x;
        f(n) + 1;",
        "and f: (x: T(0)) -> T(0) = (x) => x;
        f(n) + 1;",
    ] {
        assert_eq!(run(&program(group)), b"42", "{group}");
    }
}

// The type a body is checked at reads its group as the certifier reads it: `val`'s type `T(2)` unfolds `T`, known to terminate, through its own recursion to `Nat`, both where `val`'s body is checked at it and where a sibling's body reads `val` at it. Read with `T` standing for nothing, `T(2)` stopped at `T(1)` and both were refused.
#[test]
fn a_group_signature_reads_its_members_bodies() {
    let source = r#"
        use /std/{Nat};
        let outer(taint: Nat) -> Nat =
            let T(n: Nat) -> Type = match n | 0 => Nat | k + 1 => T(k) end
            and val: T(2) = taint
            and after(n: Nat) -> Nat = match n | 0 => val + 1 | k + 1 => after(k) end;
            after(1);
        /std/print(Nat/to_str(outer(2)))
        "#;

    assert_eq!(run(source), b"3");
}

// A group's signatures are sorted with its members opaque, so `val: T(2)` is read at a stuck `T(2)` whose sort is `Type`; its bodies are checked with the members known to terminate unfolding, where `T(2)` reduces through `T`. The two readings cannot disagree on which sort `T(2)` is: `T`'s result sort holds every arm to `Type`, so an arm that is a proposition is refused where `T`'s body is checked, and no member's type folds to a `Prop` the opaque reading took for a `Type`. Refused by the elaborator, not the certifier.
#[test]
fn a_group_member_type_cannot_fold_to_a_proposition() {
    let error = error(
        r#"
        use /std/{Nat};
        use /std/Bool/{True, Holds};
        let T(n: Nat) -> Type = match n | 0 => Holds(true) | k + 1 => T(k) end
        and val: T(2) = True/qed();
        /std/print("")
        "#,
    );
    assert!(
        error.contains("inferred: `Prop`")
            && error.contains("expected: `Type`")
            && !error.contains("certifier refusal"),
        "{error}"
    );
}

// A recursive slot is the one solution the occurs check does not hold, and a signature reads a member through its slot, so a body reading a sibling at its signature, or an implicit solved from one, names a slot inside the solution that fills it. A walk through solutions stops at a filled slot, which stands for its member; walked into, each program here overflowed the elaborator's stack. An implicit met at a type headed by a filled slot is solved with the type's reduct, which names no member, so `id`'s `@A` is `(Nat) -> Nat` and the first program runs.
#[test]
fn a_slot_reached_from_its_own_solution_is_judged() {
    let program = |groups: &str, tail: &str| {
        fill!(
            r#"
            use /std/{Nat};
            let id(@A: Type, a: A) -> A = a;
            ${groups}
            /std/print(Nat/to_str(${tail}))
            "#,
            groups,
            tail,
        )
    };
    let refused = |groups: &str, tail: &str, diagnostic: &str| {
        let error = error(&program(groups, tail));
        assert!(
            error.contains(diagnostic) && !error.contains("certifier refusal"),
            "{error}"
        );
    };

    assert_eq!(
        run(&program(
            "let outer(m: Nat) -> Nat =
            let T(n: Nat) -> Type = match n | 0 => (Nat) -> Nat | k + 1 => T(k) end
            and val: T(2) = id((x) => x + 1);
            val(m);",
            "outer(3)",
        )),
        b"4"
    );
    refused(
        "let outer(m: Nat) -> Nat =
            let T(n: Nat) -> Type = match n | 0 => (Nat) -> Nat | k + 1 => T(k) end
            and val: T(2) = id(@T(2), m);
            val(m);",
        "outer(3)",
        "expected: `(Nat) -> Nat`",
    );
    refused(
        "let T(n: Nat) -> Type = match n | 0 => (Nat) -> Nat | k + 1 => let _w = val; T(k) end
        and val: T(2) = id((x) => x + 1);",
        "val(3)",
        "is a type position\n│  it does not terminate on every input",
    );
}

/// Two members of one group that hand each other a constructor's fields convert where one unfolding of each meets the same two again. `first_in([T/node(kids), ..r])` and `first(T/node(kids))` unfold to `first(T/node(kids))` and `first_in(kids)`, the same two heads, and the first of those still takes its match apart, so the pair is unfolded again rather than apart; the closed pair, `first_in([T/node([])])` and `first(T/node([]))`, walks the same way to `0` on both sides. Both checkers read a pair met again by the same test.
///
/// Mutation-checked: with every pair met again apart, in either checker, both are refused.
#[test]
fn two_members_met_again_where_one_takes_a_constructor_apart_convert() {
    let program = |claim: &str| {
        fill!(
            r#"
            use /std/{Nat, List, Eq};
            induct T: Type | nil() | leaf(n: Nat) | node(kids: List(T)) end
            let first(t: T) -> Nat =
                match t | nil() => 0 | leaf(n) => n | node(kids) => first_in(kids) end
            and first_in(l: List(T)) -> Nat =
                match l
                | [] => 0
                | [h, ..r] => match h | nil() => first_in(r) | leaf(_) => first(h) | node(_) => first(h) end
                end;
            ${claim}
            /std/print("")
            "#,
            claim
        )
    };

    assert_eq!(
        typecheck(&program(
            "let open(kids: List(T), r: List(T)) -> Eq()(first_in([T/node(kids), ..r]), first(T/node(kids))) = Eq/refl();"
        )),
        Ok(())
    );
    assert_eq!(
        typecheck(&program(
            "let closed() -> Eq()(first_in([T/node([])]), first(T/node([]))) = Eq/refl();"
        )),
        Ok(())
    );
}
