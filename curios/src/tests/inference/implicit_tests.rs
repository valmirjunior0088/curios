//! Solving an implicit argument from what the call site already fixes.
//!
//! An implicit is inserted at its true domain and then has to be *solved* — against a reduction that unfolds a local definition, against an inductive's own parameter, against a motive that inserts one of its own, or right-biasedly for a higher-kinded family. Each row is a route by which the solution arrives.

use crate::tests::{error, run};

#[test]
fn an_implicit_solves_against_a_reduction_through_a_let() {
    // `Eq/refl()`'s implicit must be solved against `through(x)`, whose weak-head form is a match stuck on `0 < x` with arms mentioning the `let`-bound `y`. The reducer once bound `y` as a fresh context definition rather than substituting it, and the scope check hard-failed that spelling as an out-of-scope name, so this program refused with a type mismatch; a loop in `solve` then reified such definitions back out of the candidate. The reducer now substitutes a `let` as the kernel does, so the candidate names nothing the scope does not cover, and the loop is gone.
    let source = r#"
        use /std/{Nat, Eq, Str, Io};

        let through(x : Nat) -> Nat =
            let y = x + 1;
            match 0 < x
            | true => y
            | false => y + 1
            end;

        let probe(x : Nat) -> Eq(through(x), through(x)) = Eq/refl();
        let _ = Io/write(Io/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;

    assert_eq!(run(source), b"ok");
}

#[test]
fn implicit_inductive_type_param_executes() {
    // A `@`-marked inductive parameter is implicit at the type constructor too: `Eq2(2, 2)` infers `A` from the indices, `Eq2(@Nat, 3, 3)` pins it, and the eliminator's motive type-pattern still spells every slot. Running (not just checking) also guards metavariable spines through the Π-domain close/reopen round trip: a solved implicit type-arg's solution names a sibling binder, and without the delayed substitution the two spellings of the same domain compare as distinct.
    let source = r#"
        use /std/{Nat, Bytes, Io};
        induct Eq2(@A : Type) : (x : A, y : A) -> Type
        | refl(@z : A) : (z, z)
        end
        let sym2(@A : Type, @x : A, @y : A, p : Eq2(x, y)) -> Eq2(y, x) =
            match p : (s, t, q) => Eq2(t, s)
            | refl(@z) => Eq2/refl()
            end;
        let pinned : Eq2(@Nat, 3, 3) = Eq2/refl();
        let proof : Eq2(2, 2) = Eq2/refl();
        let inferred : Eq2(2, 2) = sym2(proof);
        match inferred : (_, _, _) => /std/Io({})
        | refl(@z) => let _ = Io/write(Io/stdout, /std/Str/to_bytes(Nat/to_str(z)))!; /std/Io/pure(())
        end
        "#;

    assert_eq!(run(source), b"2");
}

#[test]
fn implicit_inductive_type_param_rejects_explicit_spelling() {
    // With `@A` implicit, the old explicit spelling queues `Nat` into the explicit slots — one argument too many, an error rather than a silent reinterpretation. (`Eq2(@Nat, 2, 2)` is the pinned spelling.)
    let source = r#"
        use /std/{Nat, Io};
        induct Eq2(@A : Type) : (x : A, y : A) -> Type
        | refl(@z : A) : (z, z)
        end
        let bad : Eq2(Nat, 2, 2) = Eq2/refl();
        let _ = Io/write(Io/stdout, /std/Str/to_bytes("no"))!;
        /std/Io/pure(())
        "#;

    error(source);
}

// Regression: an `Eq/subst` whose motive contains `Eq(_, _)` — whose `@A` is implicit — must insert that implicit when the motive is instantiated. It used to drop it, leaving `Eq` (a 3-telescope `@A, x, y`) applied to 2 args, which panicked `reduce_apply` with "telescope arity mismatch".
#[test]
fn subst_motive_inserts_implicit_in_eq() {
    let source = r#"
        use /std/{Eq, Nat, Io};
        let g(n : Nat) -> Nat = n;
        let lemma(@a : Nat, @b : Nat, p : Eq(a, b)) -> Eq(g(a), g(b)) =
            Eq/subst((x) => Eq(g(a), g(x)), p, Eq/refl());
        let _ = lemma;
        /std/print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// The flex-apply imitation rule: an implicit higher-kinded binder `@M` is inferred from an argument's concrete type — `?M(?A) ≡ List(Nat)` commits `?M := (A) => List(A)` and `?A := Nat` — where previously only the explicit `apply_m(@List, l)` spelling checked.
#[test]
fn higher_kinded_implicit_infers_by_imitation() {
    let source = r#"
        use /std/{Nat, List, Str, Io};
        pub let apply_m(@M : (Type) -> Type, @A : Type, x : M(A)) -> M(A) = x;
        let l : List(Nat) = [1, 2];
        let k : List(Nat) = apply_m(l);
        /std/print(Nat/to_str(List/len(k)))
        "#;

    assert_eq!(run(source), b"2");
}

// Two applications of one global definition are unified by their spines before the head unfolds — the first-order approximation. `trim` is a fold that never names itself, so it is a `let` and reduction would unfold it: the left side steps to `combine(false, trim(x))`, the right to a fold stuck on `?t`, and nothing pins `?t` again. Comparing the spines first states the solution outright, as it always had for a `rec`-defined head, whose application stays folded.
#[test]
fn an_implicit_solves_by_spine_agreement_before_the_head_unfolds() {
    let source = r#"
        use /std/{Nat, Bool, Bits, Eq, Str, Io};
        let combine(head : Bool, t : Bits) -> Bits =
            match t
            | b[] => match head | true => b[head] | false => b[] end
            | b[_, .._] => b[head, ..t]
            end;
        let trim(bits : Bits) -> Bits =
            match bits | b[] => b[] | b[head, ..tail]; ih => combine(head, ih) end;
        let through(@t : Bits, q : Eq(trim(t), b[])) -> Eq(trim(t), b[]) = q;
        let probe(x : Bits, q : Eq(trim(b[0, ..x]), b[])) -> Eq(trim(b[0, ..x]), b[]) = through(q);
        let _ = Io/write(Io/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;

    assert_eq!(run(source), b"ok");
}

// Agreeing spines are sufficient, never necessary: a constant function's applications to different arguments are still equal, and the attempt's mismatch must fall through to the unfolding that decides them — with whatever the attempt committed rolled back.
#[test]
fn a_spine_mismatch_falls_through_to_unfolding() {
    let source = r#"
        use /std/{Nat, Eq, Str, Io};
        let constant(n : Nat) -> Nat = 0;
        let same : Eq(constant(2), constant(1)) = Eq/refl();
        let _ = Io/write(Io/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;

    assert_eq!(run(source), b"ok");
}

// An implicit solved from a projection of a local binding whose value discharges a bound inside a match arm. The candidate `t.0` reduces to the whole of `codes`'s fold, whose `x[h, ..t]` arm carries `Bytes/get(b, 0, @True/qed())` — a proof the arm's own refinement discharged where it was written, the shape `Str/to_list`'s scan once carried. `Convert::solve` re-validates a candidate as an oracle, and the oracle used to withhold *every* refinement, including the ones the candidate's own arms re-establish: the proof was then checked against the unreduced `Nat/Lt(0, Bytes/len(b))`, re-validation rejected a correct solution, and the implicit surfaced as a mismatch with the entire unfolded fold on its inferred side. Suppression is scoped to the depth it began at, so the ambient arm stays withheld and the validated term's own arms do not.
#[test]
fn an_implicit_solves_through_a_binding_whose_value_discharges_a_bound_in_an_arm() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Byte, Bytes, Str, Char, List, Option, Bool};

        induct Vec(T: Type): (n: Nat) -> pub Type
        | nil(): (0)
        | cons(@n: Nat, head: T, tail: Vec(T, n)): (n + 1)
        end

        let of_list(@T: Type, l: List(T)) -> {n: Nat, Vec(T, n)} =
            match l | [] => (0, Vec/nil()) | [x, .._]; (m, v) => (m + 1, Vec/cons(x, v)) end;

        let to_list(@T: Type, @n: Nat, v: Vec(T, n)) -> List(T) =
            match v: (_, _) => List(T) | nil() => [] | cons(@_, x, xs) => [x, ..to_list(xs)] end;

        let resize(w: Nat, @w0: Nat, v: Vec(Char, w0)) -> Vec(Char, w) =
            (match w: (k) => (n: Nat, Vec(Char, n)) -> Vec(Char, k)
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
                    let code = Byte/to_nat(Bytes/get(b, 0, @Bool/True/qed()));
                    go(t, [..acc, Option/unwrap_or(Char/of_nat(code), '?')])
                end;
            go(Str/to_bytes(s), []);

        let padded(s: Str, w: Nat) -> Vec(Char, w) =
            let t = of_list(codes(s));
            resize(w, t.1);

        /std/print(Str/flatten(List/map(to_list(padded("hi", 4)), Str/of_char)))
        "#),
        b"hi.."
    );
}

// An implicit born inside a match arm that generalizes a hypothesis over the scrutinee. `Sizes(s)` computes the size record by matching on the shape, so `z : Sizes(s)` mentions the scrutinee and `check_generalized_arm` re-assumes it under the case-specialized `{Sizes(a), Sizes(b)}` — under its *original* name, shadowing the ambient binder. `local` then held `z` twice, and every metavariable born in the arm inherited a spine with a repeated argument: `Convert::solve`'s inversion cannot invert a name reachable through two slots, so the scope check refused `Total(a, z.0)` for mentioning a hypothesis plainly in scope and `Vec/append`'s length never solved. A birth telescope keeps one entry per name, at its innermost binding.
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

// The arm above under a written motive. The motive makes the result a family rather than the ambient goal, and the elaborator re-typed locals only at an ambient goal, so `z` stayed `Sizes(s)` in the metavariable's birth context and `Vec/append`'s length refused `Total(a, z.0)`. The kernel re-types in every arm, and both now do by the one rule, `curios_analysis::retyped`.
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

// The arm above learning its shape from inside an index. `w(a, b)` targets `node(node(a, b), leaf())` against the actual `node(s, leaf())`, so the case solves the outer `s := node(a, b)` — a variable that is neither the scrutinee nor an index, which the elaborator's own re-typing never reached, leaving `z : Sizes(s)` for the metavariable to read. The kernel re-types by the case's whole solution, and both now do by `curios_analysis::retyped`.
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

// The arm above over a `let` of the scrutinee. The kernel has substituted the `let`, so its arm meets `s` and re-types `z`; the elaborator keeps `t` as a local definition, re-typed only what mentions `t`, and left `z : Sizes(s)` for the metavariable to read. `curios_analysis::scrutinee_solution` reads through the definition to the `s` the kernel sees. The second program types its hypothesis over `t` itself, which the elaborator's re-typing did reach, and which `curios_analysis::retyped` reaches by reading `t` through to `s`.
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

#[test]
fn an_undetermined_value_implicit_is_reported_as_undetermined_not_undischarged() {
    // `n` is a `Nat`, not a proposition: nothing about it was ever an obligation, so the report says nothing determined it and shows its type, rather than claiming nothing discharged `Nat` — which is the wording a *bound* gets, and names a fault a reader cannot find here.
    let source = r#"
        use /std/{Nat};
        let pad(@n: Nat, x: Nat) -> Nat = x;
        /std/print(Nat/to_str(pad(1)))
        "#;

    let report = error(source);
    assert!(
        report.contains("implicit argument 'n' of 'pad' was not inferred")
            && report.contains("no argument or expected type determined it (its type is Nat)")
            && !report.contains("nothing discharged"),
        "the report should say nothing determined the value, got: {report}"
    );
}

// An implicit solved from a projection whose value runs a walk that carries its input's validity, which each arm's guard discharges — the shape `Str/fold` takes once it threads a string's validity. The guard is `h + 1 < 10`, and the occurrence it must meet sits in `step`'s unfolding behind the `let` that names `h + 1`, so it reaches the arm respelled. Elaborating the walk decides that by comparing canonical spellings. Re-validating the solution looked the written spelling up and nothing more, since the canonical comparison sat on the branch suppression never takes, so a correct solution was rejected and `use_it` refused — while the same program with `step` spelling `h + 1 < 10` directly was accepted.
#[test]
fn an_implicit_solves_through_an_arm_whose_guard_it_meets_respelled() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Bool, List, Vec};

        induct S: Type
        | ok()
        | bad()
        end

        let step(h: Nat, s: S) -> S =
            let n = h + 1;
            match s | bad() => S/bad() | ok() => choose | Bool/not(n < 10) => S/bad() | _ => S/ok() end end;
        let run_from(s: S, l: List(Nat)) -> S = match l | [] => s | [h, ..t] => run_from(step(h, s), t) end;
        let fine(s: S) -> Bool = match s | ok() => true | bad() => false end;
        let from_bad(@l: List(Nat), v: Bool/Holds(fine(run_from(S/bad(), l)))) -> Bool/False =
            match l | [] => match v end | [_, ..t] => from_bad(@t, v) end;

        let total(l: List(Nat), v: Bool/Holds(fine(run_from(S/ok(), l)))) -> Nat =
            let go(s: S, l: List(Nat), acc: Nat, v: Bool/Holds(fine(run_from(s, l)))) -> Nat =
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

        let counted(l: List(Nat), v: Bool/Holds(fine(run_from(S/ok(), l)))) -> {w: Nat, v: Vec(Nat, w)} =
            let n = total(l, v);
            (w = n, v = Vec/replicate(n, 0));
        let width(@w: Nat, _v: Vec(Nat, w)) -> Nat = w;
        let use_it(l: List(Nat), v: Bool/Holds(fine(run_from(S/ok(), l)))) -> Nat = width(counted(l, v).v);

        /std/print(Nat/to_str(use_it([1, 2, 3], Bool/True/qed())))
        "#),
        b"6"
    );
}

/// An implicit born inside an arm is solved under the refinements it was born under, so its solution may rest on the arm's guard: `Eq/sym`'s `@x` is `Bytes/get(b, k)`, whose bound `k < Bytes/len(b)` holds only in the arm. Re-validating with every refinement withheld refused the program at `found`, and `/std/Str` spelled such implicits by hand. Mutation-checked: re-validating with every refinement withheld refuses it again.
#[test]
fn an_implicit_born_in_an_arm_is_solved_under_the_arms_guard() {
    let output = run(r#"
        use /std/{Bytes, Byte, Nat, Bool, Eq, print};

        let probe(b: Bytes, k: Nat, f: Byte, P: (Byte) -> Type, lead: P(f), fallback: Nat, consume: (x: Byte, P(x)) -> Nat) -> Nat =
            match k < Bytes/len(b)
            | true =>
                match Bytes/get(b, k) == f
                | true =>
                    let found = Byte/eq_of_eql(Bytes/get(b, k), f, Bool/True/qed());
                    consume(Bytes/get(b, k), Eq/subst((c: Byte) => P(c), Eq/sym(found), lead))
                | false => fallback
                end
            | false => fallback
            end;

        print(Nat/to_str(probe(x[1, 2], 1, 2, (_) => Nat, 5, 0, (_, n) => n)))
        "#);

    assert_eq!(output, b"5");
}

/// An implicit born outside an arm is solved without the arm's refinements, whatever kind of guard opens it: `pick`'s `@b` meets `W(k < n)` read as `W(true)` in one arm and `W(false)` in the other, and is solved to the guard itself. A guard on an application counted as no refinement, so the first arm's literal was committed and the second arm refused; the guard over a variable, `unstuck`, is the control that always passed.
#[test]
fn an_implicit_born_outside_an_arm_is_solved_without_its_guard() {
    let output = run(r#"
        use /std/{Bool, Nat, Str, print};

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

/// A solution is committed as written where its reduct does not re-check. `reach` reduces to `k` plus `hop` inlined, and `hop`'s absurd arm types only because its own arm refines the variable `b`: inlined, its scrutinee's type is `Nat/Lt(0, Bytes/len(b) - k)`, which the arm's equation on `Bytes/slice(…)` never reaches, so the reduct is refused at re-validation and `Eq/refl`'s `@x` is solved to `reach(b, k, @within, @here)` as written. Mutation-checked: committing the reduct alone refuses the program at `Eq/refl()`, as it refused `/std/Str`'s `occurrence` until its implicits were spelled by hand.
#[test]
fn a_solution_whose_reduct_does_not_recheck_is_committed_as_written() {
    let output = run(r#"
        use /std/{Bytes, Nat, Bool, Eq, print};

        let hop(@b: Bytes, some: Nat/Lt(0, Bytes/len(b))) -> Nat =
            match b
            | x[] => match some end
            | x[_, .._] => 1
            end;

        let reach(b: Bytes, k: Nat, @within: Nat/Le(k, Bytes/len(b)), @here: Nat/Lt(k, Bytes/len(b))) -> Nat =
            k + hop(@Bytes/drop(b, k, @within), Nat/Lt/sub_positive_of_lt(k, Bytes/len(b), here));

        pub let same(b: Bytes, k: Nat, @within: Nat/Le(k, Bytes/len(b)), @here: Nat/Lt(k, Bytes/len(b)))
            -> Eq(reach(b, k, @within, @here), reach(b, k, @within, @here)) =
            Eq/refl();

        print(Nat/to_str(reach(x[1, 2], 0, @Bool/True/qed(), @Bool/True/qed())))
        "#);

    assert_eq!(output, b"1");
}
