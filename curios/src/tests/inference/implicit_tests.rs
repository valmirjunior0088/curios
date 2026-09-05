//! Solving an implicit argument from what the call site already fixes.
//!
//! An implicit is inserted at its true domain and then has to be *solved* — against a reduction that unfolds a local definition, against an inductive's own parameter, against a motive that inserts one of its own, or right-biasedly for a higher-kinded family. Each row is a route by which the solution arrives.

use crate::tests::{error, run};

#[test]
fn an_implicit_solves_against_a_reduction_through_a_let() {
    // `Eq/refl()`'s implicit must be solved against `through(x)`, whose weak-head form is a match stuck on `0 < x` with arms mentioning the `let`-bound `y` — which the reducer splays into a context definition rather than substituting. The scope check once hard-failed that spelling as an out-of-scope name, so this program refused with a type mismatch; the reification loop in `solve` now unfolds the definition back into the candidate.
    let source = r#"
        use /std/{Nat, Eq, Handle, Str, Io};

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
        use /std/{Nat, Bytes, Handle, Io};
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
        use /std/{Nat, Handle, Io};
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
        use /std/{Eq, Nat, Handle, Io};
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
        use /std/{Nat, List, Handle, Str, Io};
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
        use /std/{Nat, Bool, Bits, Eq, Handle, Str, Io};
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
        use /std/{Nat, Eq, Handle, Str, Io};
        let constant(n : Nat) -> Nat = 0;
        let same : Eq(constant(2), constant(1)) = Eq/refl();
        let _ = Io/write(Io/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;

    assert_eq!(run(source), b"ok");
}

// An implicit solved from a projection of a local binding whose value discharges a bound inside a match arm. The candidate `t.0` reduces to the whole of `Str/to_list`'s scan fold, whose `x[h, ..t]` arm carries `peel_byte(True/qed(), valid)` — a proof the arm's own refinement discharged where it was written. `Convert::solve` re-validates a candidate as an oracle, and the oracle used to withhold *every* refinement, including the ones the candidate's own arms re-establish: the proof was then checked against the unreduced `Nat/Lt(0, Bytes/len(b))`, re-validation rejected a correct solution, and the implicit surfaced as a mismatch with the entire unfolded fold on its inferred side. Suppression is scoped to the depth it began at, so the ambient arm stays withheld and the validated term's own arms do not.
#[test]
fn an_implicit_solves_through_a_binding_whose_value_discharges_a_bound_in_an_arm() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Char, List, Vec, Handle};

        let resize(w: Nat, @w0: Nat, v: Vec(Char, w0)) -> Vec(Char, w) =
            (match w: (k) => (n: Nat, Vec(Char, n)) -> Vec(Char, k)
            | 0 => (n, x) => Vec/nil()
            | p + 1; ih => (n, x) =>
                match x
                | nil() => Vec/cons('.', ih(0, Vec/nil()))
                | cons(@m, y, ys) => Vec/cons(y, ih(m, ys))
                end
            end)(w0, v);

        let padded(s: Str, w: Nat) -> Vec(Char, w) =
            let t = Vec/of_list(Str/to_list(s));
            resize(w, t.1);

        /std/print(Str/flatten(List/map(Vec/to_list(padded("hi", 4)), Str/of_char)))
        "#),
        b"hi.."
    );
}

// An implicit born inside a match arm that generalizes a hypothesis over the scrutinee. `Sizes(s)` computes the size record by matching on the shape, so `z : Sizes(s)` mentions the scrutinee and `check_generalized_arm` re-assumes it under the case-specialized `{Sizes(a), Sizes(b)}` — under its *original* name, shadowing the ambient binder. `local` then held `z` twice, and every metavariable born in the arm inherited a spine with a repeated argument: `Convert::solve`'s inversion cannot invert a name reachable through two slots, so the scope check refused `Total(a, z.0)` for mentioning a hypothesis plainly in scope and `Vec/append`'s length never solved. A birth telescope keeps one entry per name, at its innermost binding.
#[test]
fn an_implicit_solves_in_an_arm_that_specializes_the_hypothesis_it_names() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Vec, Handle};

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

#[test]
fn an_undetermined_value_implicit_is_reported_as_undetermined_not_undischarged() {
    // `n` is a `Nat`, not a proposition: nothing about it was ever an obligation, so the report says nothing determined it and shows its type, rather than claiming nothing discharged `Nat` — which is the wording a *bound* gets, and names a fault a reader cannot find here.
    let source = r#"
        use /std/{Nat, Handle};
        let pad(@n: Nat, x: Nat) -> Nat = x;
        /std/print(Nat/to_str(pad(1)))
        "#;

    let report = error(source);
    assert!(
        report.contains("implicit argument 'n' of '/pad' was not inferred")
            && report.contains("no argument or expected type determined it (its type is Nat)")
            && !report.contains("nothing discharged"),
        "the report should say nothing determined the value, got: {report}"
    );
}
