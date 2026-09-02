//! Packed atom splices, indexed vectors, and the folds that sum a sequence.

use crate::tests::run;

#[test]
fn atom_splice_builds_the_written_sequence() {
    // `\.` splices one generator into a packed literal, between literal runs and adjacent to another atom. `i` and `bang` are symbolic, so the run is genuinely spliced rather than folded at parse time.
    let source = r#"
        use /std/{Handle, Str, Byte, Bytes, Io};
        let i : Byte = 0x69;
        let bang : Byte = 0x21;
        let _ = Io/write(Io/stdout, x[0x48, i, bang])!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"Hi!");
}

#[test]
fn atom_splices_are_the_cons_and_append_spellings() {
    // An atom leading a spread lowers to the cons spelling `curios_elab`'s packed-match refinement builds — the singleton `append(x[], h)` concatenated with the tail — so a literal written that way is the cons spine, not merely equal to one. Stated for SYMBOLIC operands through `len` and `get`, the two observations that reduce across that spine, so nothing here is reached by folding literals.
    let source = r#"
        use /std/{Handle, Str, Eq, Byte, Bytes, Bool, Bits, Nat, Option, Io};
        let cons_len(h : Byte, t : Bytes)
            -> Eq(Bytes/len(x[h, ..t]), Nat/add(1, Bytes/len(t))) = Eq/refl();
        let cons_head(h : Byte, t : Bytes)
            -> Eq(Bytes/try_get(x[h, ..t], 0), Option/some(h)) = Eq/refl();
        let bits_len(h : Bool, t : Bits)
            -> Eq(Bits/len(b[h, ..t]), Nat/add(1, Bits/len(t))) = Eq/refl();
        let _ = Io/write(Io/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

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
        use /std/{Nat, Bytes, Handle, Io};
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

#[test]
fn list_fold_sums_elements() {
    let source = r#"
        use /std/{Handle, Str, Nat, List, Io};
        let xs : List(Nat) = [10, 20, 30];
        let _ = Io/write(Io/stdout, Str/to_bytes(Nat/to_str(List/fold(xs, 0, (e, acc) => Nat/add(acc, e)))))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"60");
}

#[test]
fn bin_fold_sums_bytes() {
    let source = r#"
        use /std/{Handle, Str, Nat, Byte, Bytes, Io};
        let b = x[0x0a, 0x14, 0x1e];
        let _ = Io/write(Io/stdout, Str/to_bytes(Nat/to_str(Bytes/fold(b, 0, (byte, acc) => Nat/add(acc, Byte/to_nat(byte))))))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"60");
}

// An empty match is a vacuous elimination: it never inspects its scrutinee. A `False` is a `Prop`, so it erases (sort-driven) — a contradiction may therefore discharge into a *relevant* result, both directly (`match c : (_) => A end`) and through a discharging definition (`let absurd(@A : Type, c : False) -> A = match c end`). This is what lets an impossible runtime branch be closed off by an erased witness, the crux of the UTF-8 decode certification.
