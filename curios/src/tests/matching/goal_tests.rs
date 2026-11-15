//! The goal a match is checked at: the expected type with the case in its scrutinee's place, over a variable, an expression and a free monoid, and the sort a goal nothing states is given by its arms.

use {crate::tests::test_support::*, curios_common::fill};

// A `| _ =>` catch-all on an indexed family: the enumerated arms are checked at their own case target indices and the default at the scrutinee's actual ones.
#[test]
fn a_default_arm_is_allowed_on_an_indexed_family() {
    let source = r#"
        use /std/{Nat};
        induct Vec(T : Type) : (n : Nat) -> pub Type
        | nil() : (0)
        | cons(@n : Nat, head : T, tail : Vec(T)(n)) : (n + 1)
        end
        let head_or(@T : Type, @n : Nat, v : Vec(T)(n), fallback : T) -> T =
            match v
            | cons(@m, x, xs) => x
            | _ => fallback
            end;
        let v : Vec(Nat)(2) = Vec/cons(8, Vec/cons(9, Vec/nil()));
        /std/print(Nat/to_str(head_or(v, 0)))
        "#;

    assert_eq!(run(source), b"8");
}

// === Over a variable =========================================================
//
// A match over a variable scrutinee takes the expected type as its result and checks each arm against it with the scrutinee and its variable indices standing for the arm's case. A hypothesis whose type mentions the scrutinee — `d : Utf8(s, b)` under `match s`, over a state-indexed validity family — therefore needs no convoy: no family is closed over `s`, so nothing has to be typed under a binder that `d`'s type does not name, and nothing hides the shape from the size-change walk as a synthesized convoy would.
#[test]
fn a_hypothesis_typed_by_the_scrutinee_needs_no_convoy() {
    let source = r#"
        use /std/{Nat, Byte, Bytes, Eq, Str};
        use /std/Str/{Scan, step};

        induct Utf8: (Scan, Bytes) -> Prop
        | stop(): (Scan/lead(), x[])
        | more(c: Byte, st: Scan, t: Bytes, rest: Utf8(step(c, st), t)): (st, x[c, ..t])
        end

        let dv(s : Scan, @b : Bytes, d : Utf8(s, b)) -> Eq()(0, 0) =
            match s
            | lead() => match d | stop() => Eq/refl() | more(c, _, t, rest) => dv(step(c, Scan/lead()), rest) end
            | cont(_, _, _) => match d | more(c, _, _, rest) => dv(step(c, s), rest) end
            | bad() => match d | more(c, _, _, rest) => dv(step(c, Scan/bad()), rest) end
            end;

        /std/print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// === Over an expression ======================================================
//
// A match over an expression scrutinee takes its goal as a variable's does: the goal as written, its syntactic occurrences of the expression replaced by the case, and the arm's refinement reducing every occurrence the goal reaches only by unfolding. A family solved by occurrence abstraction would see the goal reduced at its root instead, where an occurrence spelled through a `let` escapes the abstraction while the application the refinement is keyed on has been unfolded away — refusing `classify`'s shape, a defined guard behind a `let` after an earlier guard.
#[test]
fn a_match_over_an_expression_reaches_a_guard_behind_a_let() {
    let source = r#"
        use /std/{Nat, Bool, Byte};
        use /std/Bool/{True, Holds};
        let ladder(c : Byte) -> Bool =
            let m = Byte/to_nat(c);
            match m < 1 | true => true | false => match Nat/in_range(m, 3, 5) | true => true | false => true end end;
        let climbs(c : Byte) -> Holds(ladder(c)) =
            match Byte/to_nat(c) < 1
            | true => True/qed()
            | false => match Nat/in_range(Byte/to_nat(c), 3, 5) | true => True/qed() | false => True/qed() end
            end;
        let aliased(n : Nat) -> Bool =
            let m = n;
            match Nat/in_range(m, 0, 1) | true => true | false => match Nat/in_range(m, 3, 5) | true => true | false => true end end;
        let climbs_aliased(n : Nat) -> Holds(aliased(n)) =
            match Nat/in_range(n, 0, 1)
            | true => True/qed()
            | false => match Nat/in_range(n, 3, 5) | true => True/qed() | false => True/qed() end
            end;
        /std/print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// An occurrence the goal reaches only by unfolding a definition is reduced by the arm's refinement, since no spelling shows it to replace.
#[test]
fn a_match_over_an_expression_reaches_an_occurrence_behind_a_definition() {
    let source = r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, Holds};
        let below(n : Nat) -> Bool = match n < 5 | true => true | false => n < 10 end;
        let bounded(n : Nat, h : Holds(n < 10)) -> Holds(below(n)) =
            match n < 5
            | true => True/qed()
            | false => h
            end;
        /std/print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// A goal that writes the expression is specialized per arm: each arm is checked with the case in the comparison's place.
#[test]
fn a_match_over_an_expression_specializes_a_goal_that_writes_it() {
    let source = r#"
        use /std/{Nat, Bool};
        let pick(n : Nat) -> match n < 5 | true => Nat | false => Bool end =
            match n < 5
            | true => 3
            | false => true
            end;
        /std/print(Nat/to_str(pick(2)))
        "#;

    assert_eq!(run(source), b"3");
}

// The goal is still the goal: an arm whose goal does not hold at its case is refused.
#[test]
fn a_match_over_an_expression_refuses_an_arm_whose_goal_fails() {
    let source = r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, Holds};
        let below(n : Nat) -> Bool = match n < 5 | true => true | false => n < 10 end;
        let unbounded(n : Nat) -> Holds(below(n)) =
            match n < 5
            | true => True/qed()
            | false => True/qed()
            end;
        /std/print("ok")
        "#;

    let error = error(source);
    assert!(error.contains("type mismatch"), "unexpected error: {error}");
}

// === Case splits over a free monoid ==========================================
//
// A case split takes its goal as a `Bool` match does: a goal holding a proof about the scrutinee — which a family closed over the scrutinee, and not over the proof, cannot state — needs no convoy.
#[test]
fn a_case_split_whose_goal_holds_a_proof_about_its_scrutinee_needs_no_convoy() {
    let source = r#"
        use /std/{Nat, Bytes, List, Eq};
        use /std/Bool/{Holds};
        let at_bytes(a : Bytes, i : Nat, q : Holds(i < Bytes/len(a)))
            -> Eq()(Bytes/get(a, i, @q), Bytes/get(a, i, @q)) =
            match a | x[] => Eq/refl() | x[_, .._] => Eq/refl() end;
        let at_list(@T : Type, xs : List(T), i : Nat, q : Holds(i < List/len(xs)))
            -> Eq()(List/get(@T, xs, i, @q), List/get(@T, xs, i, @q)) =
            match xs | [] => Eq/refl() | [_, .._] => Eq/refl() end;
        let byte_of(n : Nat, q : Holds(n < 256)) -> Eq()(Nat/to_byte(n, @q), Nat/to_byte(n, @q)) =
            match n | 0 => Eq/refl() | p + 1 => Eq/refl() end;
        /std/print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

/// An arm's goal is the match's goal with the case in its scrutinee's place, an elimination of that scrutinee inside the goal among it: in the `false` arm the goal's own `match` is one on `false`, whose `true` arm holds a proof that rested on its guard. A `match` inside the arm stores that goal as its result, and the certifier types no arm a literal scrutinee never takes. The second program eliminates a call, whose occurrence the goal writes.
#[test]
fn an_arms_goal_that_eliminates_its_scrutinee_is_a_result_a_match_may_store() {
    let variable = r#"
        use /std/{Nat, Bool, proved};
        use /std/Bool/{True, Holds};
        let g(c : Bool, @ok : Holds(c)) -> Nat = 1;
        let f(c : Bool, k : Nat) -> Holds((match c | true => g(c, @True/qed()) | false => 0 end) <= 1) =
            match c
            | true => proved()
            | false => match k | 0 => True/qed() | _ + 1 => True/qed() end
            end;
        /std/print("held")
        "#;
    let call = r#"
        use /std/{Nat, Bool, proved};
        use /std/Bool/{True, Holds};
        let g(c : Bool, @ok : Holds(c)) -> Nat = 1;
        let f(p : (Nat) -> Bool, n : Nat, k : Nat) -> Holds((match p(n) | true => g(p(n), @True/qed()) | false => 0 end) <= 1) =
            match p(n)
            | true => proved()
            | false => match k | 0 => True/qed() | _ + 1 => True/qed() end
            end;
        /std/print("held")
        "#;

    assert_eq!(run(variable), b"held");
    assert_eq!(run(call), b"held");
}

// === A goal nothing states ===================================================

// A match where nothing is expected takes a metavariable as its goal, and the sort of what it answers is a quality nothing has stated. A proposition of two constructors is matched into a proposition alone, so its arms decide: where they are proofs the result is a proposition and the match stands.
#[test]
fn a_match_nothing_expects_over_a_proposition_is_decided_by_the_proofs_in_its_arms() {
    let source = r#"
        use /std/Bool/{True};
        induct Two: Prop | a() | b() end
        let chosen(t: Two) -> True =
            let r = match t | a() => True/qed() | b() => True/qed() end;
            r;
        let _ : True = chosen(Two/a());
        /std/print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// Where its arms are data the same match takes a proposition into data, and is refused as that elimination rather than as a mismatch in an arm.
#[test]
fn a_match_nothing_expects_over_a_proposition_is_refused_where_its_arms_are_data() {
    let source = r#"
        use /std/{Nat};
        induct Two: Prop | a() | b() end
        let chosen(t: Two, n: Nat) -> Nat =
            let r = match t | a() => n | b() => n end;
            r;
        /std/print(Nat/to_str(chosen(Two/a(), 3)))
        "#;

    let report = error(source);
    assert!(
        report.contains("the proposition `Two` is matched into a relevant result"),
        "{report}"
    );
}

// A goal that is itself a match stating no goal of its own lands in the sort that match's arms decide: a proposition where they are propositions, and data where they are data, which a proposition of two constructors is not matched into.
#[test]
fn a_goal_that_is_a_match_takes_the_sort_its_arms_decide() {
    let declared = r#"
        use /std/{Nat, List, Eq};
        use /std/Bool/{True};
        induct In(@T: Type): (T, List(T)) -> Prop
        | here(@x: T, @l: List(T)): (x, [x, ..l])
        | there(@x: T, @y: T, @l: List(T), rest: In()(x, l)): (x, [y, ..l])
        end
        "#;
    let inversion = fill!(
        r#"${declared}
        let unconsed(@T: Type, @P: Prop, @x: T, @m: List(T), i: In()(x, m))
            -> match m | [] => True | [k, ..r] => (head: (Eq()(x, k)) -> P, tail: (In()(x, r)) -> P) -> P end =
            match i
            | here() => (head, _) => head(Eq/refl())
            | there(rest) => (_, tail) => tail(rest)
            end;
        /std/print("held")
        "#,
        declared
    );
    let data = fill!(
        r#"${declared}
        let position(@T: Type, @x: T, @m: List(T), i: In()(x, m)) -> match m | [] => Nat | [k, ..r] => Nat end =
            match i
            | here() => 0
            | there(rest) => 1
            end;
        /std/print("held")
        "#,
        declared
    );

    assert_eq!(run(&inversion), b"held");
    let report = error(&data);
    assert!(
        report.contains("the proposition `In` is matched into a relevant result"),
        "{report}"
    );
}
