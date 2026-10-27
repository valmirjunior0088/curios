//! Solving an implicit inside an arm: under the hypothesis the arm specializes or learns, under its guard or without it, and re-expressed over the match to type it.

use crate::tests::run;

// An implicit solved from a projection of a local binding whose value discharges a bound inside a match arm. The candidate `t.0` reduces to the whole of `codes`'s fold, whose `x[h, ..t]` arm carries `Bytes/get(b, 0, @True/qed())` — a proof the arm's own refinement discharged where it was written. `Convert::solve` re-validates a candidate as an oracle, with suppression scoped to the depth it began at, so the ambient arm stays withheld and the validated term's own arms do not. Withholding *every* refinement would check the proof against the unreduced `Holds(0 < Bytes/len(b))`, reject a correct solution, and surface the implicit as a mismatch with the entire unfolded fold on its inferred side.
#[test]
fn an_implicit_solves_through_a_binding_whose_value_discharges_a_bound_in_an_arm() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Byte, Bytes, Str, Char, List, Option};
        use /std/Bool/{True};

        induct Vec(T: Type): (n: Nat) -> pub Type
        | nil(): (0)
        | cons(@n: Nat, head: T, tail: Vec(T)(n)): (n + 1)
        end

        let of_list(@T: Type, l: List(T)) -> {n: Nat, Vec(T)(n)} =
            match l | [] => (0, Vec/nil()) | [x, .._]; (m, v) => (m + 1, Vec/cons(x, v)) end;

        let to_list(@T: Type, @n: Nat, v: Vec(T)(n)) -> List(T) =
            match v: (_, _) => List(T) | nil() => [] | cons(@_, x, xs) => [x, ..to_list(xs)] end;

        let resize(w: Nat, @w0: Nat, v: Vec(Char)(w0)) -> Vec(Char)(w) =
            (match w: (k) => (n: Nat, Vec(Char)(n)) -> Vec(Char)(k)
            | 0 => (n, x) => Vec/nil()
            | p + 1; ih => (n, x) =>
                match x
                | nil() => Vec/cons('.', ih(0, Vec/nil()))
                | cons(@m, y, ys) => Vec/cons(y, ih(m, ys))
                end
            end)(w0, v);

        let codes(s: Str) -> List(Char) =
            let go(b: Bytes, acc: List(Char)) -> List(Char) =
                match b
                | x[] => acc
                | x[_, ..t] =>
                    let code = Byte/to_nat(Bytes/get(b, 0, @True/qed()));
                    go(t, [..acc, Option/unwrap_or(Char/of_nat(code), '?')])
                end;
            go(Str/to_bytes(s), []);

        let padded(s: Str, w: Nat) -> Vec(Char)(w) =
            let t = of_list(codes(s));
            resize(w, t.1);

        /std/print(Str/flatten(List/map(to_list(padded("hi", 4)), Str/of_char)))
        "#),
        b"hi.."
    );
}

// An implicit born inside a match arm that generalizes a hypothesis over the scrutinee. `Sizes(s)` computes the size record by matching on the shape, so `z : Sizes(s)` mentions the scrutinee and `retype_locals` re-assumes it under the case-specialized `{Sizes(a), Sizes(b)}` — under its *original* name, shadowing the ambient binder. A birth telescope keeps one entry per name, at its innermost binding: with `z` held twice, every metavariable born in the arm would inherit a spine with a repeated argument, which `Convert::solve`'s inversion cannot invert, so the scope check would refuse `Total(a, z.0)` for mentioning a hypothesis plainly in scope and `Vec/append`'s length would never solve.
#[test]
fn an_implicit_solves_in_an_arm_that_specializes_the_hypothesis_it_names() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Vec};

        induct Shape: Type
        | leaf() | node(a: Shape, b: Shape)
        end

        let Sizes(s: Shape) -> Type =
            match s | leaf() => Nat | node(a, b) => {Sizes(a), Sizes(b)} end;

        let Total(s: Shape, z: Sizes(s)) -> Nat =
            match s | leaf() => z | node(a, b) => Total(a, z.0) + Total(b, z.1) end;

        let build(s: Shape, z: Sizes(s)) -> Vec(Nat, Total(s, z)) =
            match s
            | leaf() => Vec/replicate(z, 0)
            | node(a, b) => Vec/append(build(a, z.0), build(b, z.1))
            end;

        let tree: Shape = Shape/node(Shape/leaf(), Shape/node(Shape/leaf(), Shape/leaf()));

        /std/print(Nat/to_str(Vec/len(build(tree, (2, (3, 4))))))
        "#),
        b"9"
    );
}

/// The size records of the test above, with `build` over them, for the arms below that re-type `z` by other routes.
const SIZES: &str = r#"
    use /std/{Nat, Vec};

    induct Shape: Type
    | leaf() | node(a: Shape, b: Shape)
    end

    let Sizes(s: Shape) -> Type =
        match s | leaf() => Nat | node(a, b) => {Sizes(a), Sizes(b)} end;

    let Total(s: Shape, z: Sizes(s)) -> Nat =
        match s | leaf() => z | node(a, b) => Total(a, z.0) + Total(b, z.1) end;

    let build(s: Shape, z: Sizes(s)) -> Vec(Nat, Total(s, z)) =
        match s
        | leaf() => Vec/replicate(z, 0)
        | node(a, b) => Vec/append(build(a, z.0), build(b, z.1))
        end;

    let tree: Shape = Shape/node(Shape/leaf(), Shape/node(Shape/leaf(), Shape/leaf()));
    "#;

// The arm above under a written motive, which makes the result a family rather than the ambient goal. Both checkers re-type locals in every arm by the one rule, `curios_analysis::retyped`, so `z` is not left `Sizes(s)` in the metavariable's birth context for `Vec/append`'s length to refuse `Total(a, z.0)`.
#[test]
fn an_implicit_solves_in_an_arm_whose_written_motive_leaves_the_hypothesis_ambient() {
    let source = format!(
        r#"{SIZES}
        let built(s: Shape, z: Sizes(s)) -> Vec(Nat, Total(s, z)) =
            match s : (_) => Vec(Nat, Total(s, z))
            | leaf() => Vec/replicate(z, 0)
            | node(a, b) => Vec/append(build(a, z.0), build(b, z.1))
            end;

        /std/print(Nat/to_str(Vec/len(built(tree, (2, (3, 4))))))
        "#
    );
    assert_eq!(run(&source), b"9");
}

// The arm above learning its shape from inside an index. `w(a, b)` targets `node(node(a, b), leaf())` against the actual `node(s, leaf())`, so the case solves the outer `s := node(a, b)` — a variable that is neither the scrutinee nor an index. Both checkers re-type by the case's whole solution through `curios_analysis::retyped`, so `z : Sizes(s)` is not left for the metavariable to read.
#[test]
fn an_implicit_solves_in_an_arm_that_learns_the_hypothesis_from_inside_an_index() {
    let source = format!(
        r#"{SIZES}
        induct W : (s : Shape) -> Type
        | w(a : Shape, b : Shape) : (Shape/node(Shape/node(a, b), Shape/leaf()))
        end

        let built(s : Shape, z : Sizes(s), x : W(Shape/node(s, Shape/leaf()))) -> Vec(Nat, Total(s, z)) =
            match x | w(a, b) => Vec/append(build(a, z.0), build(b, z.1)) end;

        let pair: Shape = Shape/node(Shape/leaf(), Shape/leaf());
        let x : W(Shape/node(pair, Shape/leaf())) = W/w(Shape/leaf(), Shape/leaf());
        /std/print(Nat/to_str(Vec/len(built(pair, (2, 3), x))))
        "#
    );
    assert_eq!(run(&source), b"5");
}

// The arm above over a `let` of the scrutinee. The kernel has substituted the `let`, so its arm meets `s` and re-types `z`; the elaborator keeps `t` as a local definition, and `curios_analysis::scrutinee_solution` reads through it to the `s` the kernel sees, so `z : Sizes(s)` is re-typed too. The second program types its hypothesis over `t` itself, which `curios_analysis::retyped` reaches by reading `t` through to `s`.
#[test]
fn an_implicit_solves_in_an_arm_over_a_let_bound_scrutinee() {
    for (label, arms) in [
        (
            "a hypothesis typed over the variable",
            "let t = s;
            match t
            | leaf() => Vec/replicate(z, 0)
            | node(a, b) => Vec/append(build(a, z.0), build(b, z.1))
            end",
        ),
        (
            "a hypothesis typed over the let",
            "let t = s;
            let w : Sizes(t) = z;
            match t
            | leaf() => Vec/replicate(w, 0)
            | node(a, b) => Vec/append(build(a, w.0), build(b, w.1))
            end",
        ),
    ] {
        let source = format!(
            r#"{SIZES}
            let built(s: Shape, z: Sizes(s)) -> Vec(Nat, Total(s, z)) =
                {arms};

            /std/print(Nat/to_str(Vec/len(built(tree, (2, (3, 4))))))
            "#
        );
        assert_eq!(run(&source), b"9", "{label}");
    }
}

// An implicit solved from a projection whose value runs a walk that carries its input's validity, which each arm's guard discharges. The guard is `h + 1 < 10`, and the occurrence it must meet sits in `step`'s unfolding behind the `let` that names `h + 1`, so it reaches the arm respelled. Elaborating the walk decides that by comparing canonical spellings, and re-validating the solution does too, suppressed or not: looking the written spelling up and nothing more would reject a correct solution and refuse `use_it`, while the same program with `step` spelling `h + 1 < 10` directly would pass.
#[test]
fn an_implicit_solves_through_an_arm_whose_guard_it_meets_respelled() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Bool, List, Vec};
        use /std/Bool/{True, False, Holds};

        induct S: Type
        | ok()
        | bad()
        end

        let step(h: Nat, s: S) -> S =
            let n = h + 1;
            match s | bad() => S/bad() | ok() => choose | Bool/not(n < 10) => S/bad() | _ => S/ok() end end;
        let run_from(s: S, l: List(Nat)) -> S = match l | [] => s | [h, ..t] => run_from(step(h, s), t) end;
        let fine(s: S) -> Bool = match s | ok() => true | bad() => false end;
        let from_bad(@l: List(Nat), v: Holds(fine(run_from(S/bad(), l)))) -> False =
            match l | [] => match v end | [_, ..t] => from_bad(@t, v) end;

        let total(l: List(Nat), v: Holds(fine(run_from(S/ok(), l)))) -> Nat =
            let go(s: S, l: List(Nat), acc: Nat, v: Holds(fine(run_from(s, l)))) -> Nat =
                match l
                | [] => acc
                | [h, ..t] =>
                    match s
                    | bad() => match from_bad(@t, v) end
                    | ok() =>
                        match h + 1 < 10
                        | false => match from_bad(@t, v) end
                        | true => go(S/ok(), t, acc + h, v)
                        end
                    end
                end;
            go(S/ok(), l, 0, v);

        let counted(l: List(Nat), v: Holds(fine(run_from(S/ok(), l)))) -> {w: Nat, v: Vec(Nat, w)} =
            let n = total(l, v);
            (w = n, v = Vec/replicate(n, 0));
        let width(@w: Nat, _v: Vec(Nat, w)) -> Nat = w;
        let use_it(l: List(Nat), v: Holds(fine(run_from(S/ok(), l)))) -> Nat = width(counted(l, v).v);

        /std/print(Nat/to_str(use_it([1, 2, 3], True/qed())))
        "#),
        b"6"
    );
}

/// An implicit born inside an arm is solved under the refinements it was born under, so its solution may rest on the arm's guard: `Eq/sym`'s `@x` is `Bytes/get(b, k)`, whose bound `k < Bytes/len(b)` holds only in the arm. Mutation-checked: re-validating with every refinement withheld refuses the program at `found`.
#[test]
fn an_implicit_born_in_an_arm_is_solved_under_the_arms_guard() {
    let output = run(r#"
        use /std/{Bytes, Byte, Nat, Eq, print};
        use /std/Bool/{True};

        let probe(b: Bytes, k: Nat, f: Byte, P: (Byte) -> Type, lead: P(f), fallback: Nat, consume: (x: Byte, P(x)) -> Nat) -> Nat =
            match k < Bytes/len(b)
            | true =>
                match Bytes/get(b, k) == f
                | true =>
                    let found = Byte/eq_of_eql(Bytes/get(b, k), f, True/qed());
                    consume(Bytes/get(b, k), Eq/subst((c: Byte) => P(c), Eq/sym(found), lead))
                | false => fallback
                end
            | false => fallback
            end;

        print(Nat/to_str(probe(x[1, 2], 1, 2, (_) => Nat, 5, 0, (_, n) => n)))
        "#);

    assert_eq!(output, b"5");
}

/// An implicit born outside an arm is solved without the arm's refinements, whatever kind of guard opens it: `pick`'s `@b` meets `W(k < n)` read as `W(true)` in one arm and `W(false)` in the other, and is solved to the guard itself. Counting a guard on an application as no refinement would commit the first arm's literal and refuse the second arm; the guard over a variable, `unstuck`, is the control.
#[test]
fn an_implicit_born_outside_an_arm_is_solved_without_its_guard() {
    let output = run(r#"
        use /std/{Bool, Nat, Str, print};
        use /std/Bool/{Holds};

        induct W: (Bool) -> pub Type
        | mk(b: Bool): (b)
        end

        let pick(@b: Bool, _: W(b)) -> Bool = b;

        let stuck(k: Nat, n: Nat) -> Bool =
            pick(match k < n | true => W/mk(k < n) | false => W/mk(k < n) end);

        let unstuck(c: Bool) -> Bool =
            pick(match c | true => W/mk(c) | false => W/mk(c) end);

        let spelled(b: Bool) -> Str = match b | true => "t" | false => "f" end;

        print(Str/flatten([spelled(stuck(1, 2)), spelled(stuck(2, 1)), spelled(unstuck(true))]))
        "#);

    assert_eq!(output, b"tft");
}

/// An implicit born under an arm's binder is re-expressed over the match's own where it rides into the match's type: `Result/success(v)`'s error type is minted under `v` and embedded in the match's type at `success(v)`, so it becomes a stand-in applied to `success(v)`, the match's type is solved from the first arm, and the second solves the stand-in to `Io/Error`. Waiting for it would hold the match's type open until `read!` pinned it to the region's monad, refusing the first arm with `Io(Result(?, Bytes))`. Mutation-checked: refusing every binder the match's type lacks refuses the program.
#[test]
fn an_implicit_born_under_an_arms_binder_is_re_expressed_over_the_match_to_type_it() {
    let output = run(r#"
        use /std/{Str, Bytes, Option, Result, Show, Try, Io};

        let program(r: Result(Io/Error, Bytes)) -> Try(Io, Io/Error, Str) =
            let read =
                match r
                | success(v) => Io/pure(Result/success(v))
                | failure(e) => Io/pure(Result/failure(e))
                end;
            let out = read!;
            let bytes = out!;
            Try/pure(Result/unwrap_or(Str/of_bytes(bytes), "?"));

        let main() -> Io({}) =
            let r = Try/run(program(Result/success(Str/to_bytes("read"))))!;
            match r
            | failure(e) => /std/print(Show/show(e))
            | success(s) => /std/print(s)
            end;
        main()
        "#);

    assert_eq!(output, b"read");
}

/// An elided element type is restricted as an omitted implicit is: each arm's `[]` leaves its element type open inside the match's type, and were both arms to wait on it, matching on `xs` would meet a type still a metavariable and refuse it as no list. The hole is minted as a silent one, the origin a parked check's placeholder shares, and it is the placeholder's own kind that keeps that one waiting. Mutation-checked: waiting on every silent hole refuses the program.
#[test]
fn an_elided_element_type_is_restricted_like_an_implicit_to_type_the_match() {
    let output = run(r#"
        use /std/{Str, List, print};

        let first(raw: List(Str)) -> Str =
            let xs = match raw | [] => [] | [_, .._] => [] end;
            match xs | [] => "none" | [s, .._] => s end;

        print(first(["a"]))
        "#);

    assert_eq!(output, b"none");
}
