//! What a motive may name and bind over an indexed family, and the binder count it is checked against.

use crate::tests::{error, run};

// === Motives ================================================================
//
// A motive is a term checked against the eliminator's motive type, `(ī : Ī(p̄)) -> I(p̄, ī) -> Sort`. There is no motive grammar: what follows `:` is parsed by `parse_term` and checked like any other term.

// The motive need not be a lambda at all. A top-level family of the right type is eta-expanded into the motive scope, so an elimination can name the family it proves rather than restating it.
#[test]
fn a_motive_may_name_a_top_level_family() {
    let source = r#"
        use /std/{Nat, Eq};
        let discriminates(s : Nat, t : Nat, q : Eq()(s, t)) -> Type = Eq()(t, s);
        let flip(@x : Nat, @y : Nat, p : Eq()(x, y)) -> Eq()(y, x) =
            match p : discriminates
            | refl(@z) => Eq/refl()
            end;
        let _ : Eq()(4, 4) = flip(Eq/refl());
        /std/print(Nat/to_str(4))
        "#;

    assert_eq!(run(source), b"4");
}

// A motive that ignores every binder is written with `_`s, one per index and one for the scrutinee — a constant motive is a lambda like any other, not a separate rung.
#[test]
fn a_constant_motive_on_an_indexed_family_binds_placeholders() {
    let source = r#"
        use /std/{Nat};
        induct Vec(T : Type) : (n : Nat) -> pub Type
        | nil() : (0)
        | cons(@n : Nat, head : T, tail : Vec(T)(n)) : (n + 1)
        end
        let len(@T : Type, @n : Nat, v : Vec(T)(n)) -> Nat =
            match v : (_, _) => Nat
            | nil() => 0
            | cons(@m, x, xs) => m + 1
            end;
        /std/print(Nat/to_str(len(Vec/cons(1, Vec/cons(2, Vec/nil())))))
        "#;

    assert_eq!(run(source), b"2");
}

// A motive binder's annotation is an ordinary type in an ordinary position, so the scrutinee binder's annotation may name the index binders written before it — recovering the eliminated family on the motive line. This is the dependent-lambda-telescope rule (`tests::binders`) applied to a motive.
#[test]
fn a_motive_binder_annotation_may_name_earlier_index_binders() {
    let source = r#"
        use /std/{Nat, Eq};
        let flip(@A : Type, @x : A, @y : A, p : Eq()(x, y)) -> Eq()(y, x) =
            match p : (s : A, t : A, q : Eq()(s, t)) => Eq()(t, s)
            | refl(@z) => Eq/refl()
            end;
        let _ : Eq()(6, 6) = flip(Eq/refl());
        /std/print(Nat/to_str(6))
        "#;

    assert_eq!(run(source), b"6");
}

// Plicity is expressible because the annotation is a real application: `Eq` hides its type parameter, so `Eq()(s, t)` is how it is written here, and the old flat slot list that spelled it `Eq(A, s, t)` has no counterpart.
#[test]
fn a_motive_binder_annotation_obeys_the_families_plicity() {
    let source = r#"
        use /std/{Nat, Eq};
        let flip(@x : Nat, @y : Nat, p : Eq()(x, y)) -> Eq()(y, x) =
            match p : (s, t, q : Eq(@Nat)(s, t)) => Eq()(t, s)
            | refl(@z) => Eq/refl()
            end;
        let _ : Eq()(7, 7) = flip(Eq/refl());
        /std/print(Nat/to_str(7))
        "#;

    assert_eq!(run(source), b"7");
}

// A `| _ =>` catch-all on an indexed family. Every motive binds its indices whether or not the body uses them, so a default no longer collides with a "pattern motive": the enumerated arms are checked at their own case target indices and the default at the scrutinee's actual ones.
#[test]
fn a_default_arm_is_allowed_on_an_indexed_family() {
    let source = r#"
        use /std/{Nat};
        induct Vec(T : Type) : (n : Nat) -> pub Type
        | nil() : (0)
        | cons(@n : Nat, head : T, tail : Vec(T)(n)) : (n + 1)
        end
        let head_or(@T : Type, @n : Nat, v : Vec(T)(n), fallback : T) -> T =
            match v : (_, _) => T
            | cons(@m, x, xs) => x
            | _ => fallback
            end;
        let v : Vec(Nat)(2) = Vec/cons(8, Vec/cons(9, Vec/nil()));
        /std/print(Nat/to_str(head_or(v, 0)))
        "#;

    assert_eq!(run(source), b"8");
}

// The binder count is checked against the index telescope, not inferred, so an under-bound motive reports as itself instead of as a domain mismatch.
#[test]
fn an_under_bound_motive_reports_its_binder_count() {
    let source = r#"
        use /std/{Nat};
        induct Vec(T : Type) : (n : Nat) -> pub Type
        | nil() : (0)
        | cons(@n : Nat, head : T, tail : Vec(T)(n)) : (n + 1)
        end
        let len(@T : Type, @n : Nat, v : Vec(T)(n)) -> Nat =
            match v : (_) => Nat
            | nil() => 0
            | cons(@m, x, xs) => m + 1
            end;
        /std/print(Nat/to_str(len(Vec/nil())))
        "#;

    let error = error(source);
    assert!(
        error.contains("motive binds 1 name(s)") && error.contains("needs 2"),
        "unexpected error: {error}"
    );
}

// === The ambient result ======================================================
//
// An omitted motive over a variable scrutinee, in a position with an expected type, takes that type as the elimination's result and checks each arm against it with the scrutinee and its variable indices standing for the arm's case. A hypothesis whose type mentions the scrutinee — `d : Utf8(s, b)` under `match s`, over the state-indexed validity family `/std/Str` once declared — therefore needs no convoy: no family is closed over `s`, so nothing has to be typed under a binder that `d`'s type does not name. This is the shape the elaborator used to synthesize a convoy for, and the one that convoy hid from the size-change walk.
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

// === The ambient result over an expression ===================================
//
// An omitted motive over an expression scrutinee takes the ambient form as a variable's does: the goal as written, its syntactic occurrences of the expression replaced by the case, and the arm's refinement reducing every occurrence the goal reaches only by unfolding. A family solved by occurrence abstraction saw the goal reduced at its root instead, where an occurrence spelled through a `let` escaped the abstraction while the application the refinement is keyed on had been unfolded away — so `classify`'s shape, a defined guard behind a `let` after an earlier guard, was refused although the written constant motive checked it.
#[test]
fn an_elided_motive_over_an_expression_reaches_a_guard_behind_a_let() {
    let source = r#"
        use /std/{Nat, Bool, Byte};
        let ladder(c : Byte) -> Bool =
            let m = Byte/to_nat(c);
            match m < 1 | true => true | false => match Nat/in_range(m, 3, 5) | true => true | false => true end end;
        let climbs(c : Byte) -> Bool/Holds(ladder(c)) =
            match Byte/to_nat(c) < 1
            | true => Bool/True/qed()
            | false => match Nat/in_range(Byte/to_nat(c), 3, 5) | true => Bool/True/qed() | false => Bool/True/qed() end
            end;
        let aliased(n : Nat) -> Bool =
            let m = n;
            match Nat/in_range(m, 0, 1) | true => true | false => match Nat/in_range(m, 3, 5) | true => true | false => true end end;
        let climbs_aliased(n : Nat) -> Bool/Holds(aliased(n)) =
            match Nat/in_range(n, 0, 1)
            | true => Bool/True/qed()
            | false => match Nat/in_range(n, 3, 5) | true => Bool/True/qed() | false => Bool/True/qed() end
            end;
        /std/print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// An occurrence the goal reaches only by unfolding a definition is reduced by the arm's refinement, since no spelling shows it to replace.
#[test]
fn an_elided_motive_over_an_expression_reaches_an_occurrence_behind_a_definition() {
    let source = r#"
        use /std/{Nat, Bool};
        let below(n : Nat) -> Bool = match n < 5 | true => true | false => n < 10 end;
        let bounded(n : Nat, h : Nat/Lt(n, 10)) -> Bool/Holds(below(n)) =
            match n < 5
            | true => Bool/True/qed()
            | false => h
            end;
        /std/print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// A goal that writes the expression is specialized per arm: each arm is checked with the case in the comparison's place.
#[test]
fn an_elided_motive_over_an_expression_specializes_a_goal_that_writes_it() {
    let source = r#"
        use /std/{Nat, Bool};
        let pick(n : Nat) -> match n < 5 : (_) => Type | true => Nat | false => Bool end =
            match n < 5
            | true => 3
            | false => true
            end;
        /std/print(Nat/to_str(pick(2)))
        "#;

    assert_eq!(run(source), b"3");
}

// The ambient goal is still the goal: an arm whose goal does not hold at its case is refused.
#[test]
fn an_elided_motive_over_an_expression_refuses_an_arm_whose_goal_fails() {
    let source = r#"
        use /std/{Nat, Bool};
        let below(n : Nat) -> Bool = match n < 5 | true => true | false => n < 10 end;
        let unbounded(n : Nat) -> Bool/Holds(below(n)) =
            match n < 5
            | true => Bool/True/qed()
            | false => Bool/True/qed()
            end;
        /std/print("ok")
        "#;

    let error = error(source);
    assert!(error.contains("type mismatch"), "unexpected error: {error}");
}

// === Case splits over a free monoid ==========================================
//
// A fold's induction hypothesis is typed at the result at the tail, which only a family states, so a fold whose arm reads its hypothesis closes one over its scrutinee. A case split reads none, and takes the ambient result as a `Bool` match does: a goal holding a proof about the scrutinee — which a family closed over the scrutinee, and not over the proof, cannot state — needs no convoy.
#[test]
fn a_case_split_whose_goal_holds_a_proof_about_its_scrutinee_needs_no_convoy() {
    let source = r#"
        use /std/{Nat, Bytes, List, Eq};
        let at_bytes(a : Bytes, i : Nat, q : Nat/Lt(i, Bytes/len(a)))
            -> Eq()(Bytes/get(a, i, @q), Bytes/get(a, i, @q)) =
            match a | x[] => Eq/refl() | x[_, .._] => Eq/refl() end;
        let at_list(@T : Type, xs : List(T), i : Nat, q : Nat/Lt(i, List/len(xs)))
            -> Eq()(List/get(@T, xs, i, @q), List/get(@T, xs, i, @q)) =
            match xs | [] => Eq/refl() | [_, .._] => Eq/refl() end;
        let byte_of(n : Nat, q : Nat/Lt(n, 256)) -> Eq()(Nat/to_byte(n, @q), Nat/to_byte(n, @q)) =
            match n | 0 => Eq/refl() | p + 1 => Eq/refl() end;
        /std/print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// A fold whose arm reads its hypothesis still closes a family over its scrutinee, so the same goal is refused there: the proof is stated at a type the family's binder does not match.
#[test]
fn a_fold_that_reads_its_hypothesis_still_closes_a_family() {
    let source = r#"
        use /std/{Nat, Bytes, Eq};
        let at_bytes(a : Bytes, i : Nat, q : Nat/Lt(i, Bytes/len(a)))
            -> Eq()(Bytes/get(a, i, @q), Bytes/get(a, i, @q)) =
            match a | x[] => Eq/refl() | x[_, .._]; ih => ih end;
        /std/print("ok")
        "#;

    let error = error(source);
    assert!(error.contains("type mismatch"), "unexpected error: {error}");
}
