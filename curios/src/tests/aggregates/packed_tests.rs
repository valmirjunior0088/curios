//! Indexed vectors a program declares for itself: the type-level `Vec` over `Nat` induction, and the indexed inductive whose `append` is executed rather than only compiled. The packed splices and folds are the corpus's `/aggregates/packed`.

use crate::tests::run;

#[test]
fn vec_cons_with_nat_succ() {
    let source = r#"
        let Vec(T : Type, n : std/Nat) -> Type =
            match n : (_) => Type
            | 0 => {}
            | pred + 1; ih => { T, ih }
            end;

        let cons(T : Type, n : std/Nat, x : T, xs : Vec(T, n)) -> Vec(T, std/Nat/succ(n)) =
            (x, xs);

        let head(T : Type, n : std/Nat, xs : Vec(T, std/Nat/succ(n))) -> T =
            xs.0;

        let v : Vec(std/Nat, 1) = cons(std/Nat, 0, 42, ());
        let _ = std/Io/write(std/Io/stdout, /std/Str/to_bytes(std/Nat/to_str(head(std/Nat, 0, v))))!;
        /std/Io/pure(())
    "#;

    assert_eq!(run(source), b"42");
}

#[test]
fn indexed_vec_append_executes() {
    // Rung A of the indexed-inductive ladder, *executed*: `append`'s motive binds the length index (`(v : Vec(T, k)) => Vec(T, Nat/add(k, m))`), the `cons` arm meets it through the definitional successor-peeling of `Nat/add`, and the implicit index arguments of the recursive call are solved to the arm's *first* binder. Running (not just compiling) guards the zonk realignment of multi-binder arm scopes: with the in-group order flipped, the solved indices silently referenced the wrong binder and the program trapped at runtime.
    let source = r#"
        use /std/{Nat, Bytes, Io};
        induct Vec(T : Type) : (n : Nat) -> Type
        | nil() : (0)
        | cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
        end
        let append(@T : Type, @n : Nat, @m : Nat, v : Vec(T, n), w : Vec(T, m)) -> Vec(T, Nat/add(n, m)) =
            match v : (k, v) => Vec(T, Nat/add(k, m))
            | nil() => w
            | cons(@j, x, xs) => Vec/cons(x, append(xs, w))
            end;
        let total(@n : Nat, v : Vec(Nat, n), acc : Nat) -> Nat =
            match v : (_, _) => Nat
            | nil() => acc
            | cons(@m, x, xs) => total(xs, Nat/add(acc, x))
            end;
        let a : Vec(Nat, 2) = Vec/cons(1, Vec/cons(2, Vec/nil()));
        let b : Vec(Nat, 1) = Vec/cons(4, Vec/nil());
        let c : Vec(Nat, 3) = append(a, b);
        let _ = Io/write(Io/stdout, /std/Str/to_bytes(Nat/to_str(total(c, 0))))!;
        /std/Io/pure(())
        "#;

    assert_eq!(run(source), b"7");
}
