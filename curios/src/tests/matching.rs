use curios_runtime::MockHost;

#[test]
fn opaque_inductive_is_usable_through_declaring_module_api() {
    let source = r#"
        use /std/{Nat, Handle};
        pub mod Secret
            use /std/{Nat};
            pub induct T : Type
            | wrap(Nat)
            end
            pub let make(n : Nat) -> T = T/wrap(n);
            pub let reveal(t : T) -> Nat =
                match t
                | wrap(n) => n
                end;
        end
        /std/print(Nat/to_str(Secret/reveal(Secret/make(7))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
}

#[test]
fn opaque_inductive_empty_elimination_is_private() {
    let source = r#"
        use /std/{Nat, Handle};
        pub mod Secret
            use /std/{Nat};
            pub induct T : Type
            | wrap(Nat)
            end
            pub let make(n : Nat) -> T = T/wrap(n);
        end
        let reveal(t : Secret/T) -> Nat = match t : (_) => Nat end;
        /std/print(Nat/to_str(reveal(Secret/make(7))))
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(source, system).unwrap_err();
    assert!(
        error.contains("representation of type '/Secret/T' is private"),
        "unexpected error: {error}"
    );
}

// Regression test for a bug found while building the matrix pattern compiler: minting a synthetic binder for a single, unnested constructor arm (rather than reusing the written name directly) produced a core binder whose only label was that gensym — which the erasure pass's hint-based fresh naming then chained into another gensym, compounding until a reference outran its own binding. A plain flat match must still lower with no such indirection.
#[test]
fn flat_option_match_lowers_without_synthetic_indirection() {
    let source = r#"
        use /std/{Option, Nat, Handle};
        let f(o : Option(Nat)) -> Nat =
            match o
            | some(y) => y
            | none() => 0
            end;
        /std/print(Nat/to_str(f(Option/some(5))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"5");
}

#[test]
fn bits_structural_fold_preserves_heads_and_bit_unit_tails() {
    let source = r#"
        use /std/{Bits, Nat, Handle};
        let value(bits : Bits) -> Nat =
            match bits
            | b[] => 0
            | b[head, ..tail]; ih =>
                let digit : Nat = match head | false => 0 | true => 1 end;
                digit + 2 * ih
            end;
        /std/print(Nat/to_str(value(b[\1, \0, \1, \1, \0, \0, \1, \0, \1, \1])))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"845");
}

// The spec's own motivating example: a single tupled head, fully enumerated over two independent `Option`-shaped columns.
#[test]
fn nested_ctor_pattern_dispatches_by_shape() {
    let source = r#"
        use /std/{Option, Nat, Handle};
        let f(a : Option(Nat), b : Option(Nat)) -> Nat =
            match (a, b)
            | (some(x), some(y)) => x + y
            | (some(x), none()) => x
            | (none(), some(y)) => y
            | (none(), none()) => 0
            end;
        /std/print(Nat/to_str(f(Option/some(3), Option/some(4))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
}

// Regression for a `choose` lowering bug: a condition arm followed by a refutable bind arm was parsed as `Choose`, but lowering the bind arm accidentally routed through the headed-match dependent-motive/default gate.
#[test]
fn choose_allows_condition_before_bind_arm() {
    let source = r#"
        use /std/{Bool, Nat, Option, Handle};
        let pick(prefer_fresh : Bool, cached : Option(Nat), fresh : Nat) -> Nat =
            choose
            | prefer_fresh && fresh > 0 => fresh
            | some(n) = cached => n
            | _ => 0
            end;
        /std/print(Nat/to_str(pick(true, Option/some(21), 5)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"5");
}

// A tuple value used as a match target directly — no constructor tag at all — desugars to plain projection, never a core `Match` node.
#[test]
fn tuple_match_target_projects_fields() {
    let source = r#"
        use /std/{Nat, Handle};
        let f(p : { Nat, Nat }) -> Nat =
            match p
            | (x, y) => x + y
            end;
        /std/print(Nat/to_str(f((3, 4))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
}

// A struct value used as a match target directly, including field-punning.
#[test]
fn struct_match_target_projects_fields() {
    let source = r#"
        use /std/{Nat, Handle};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let f(p : Pair(Nat, Nat)) -> Nat =
            match p
            | Pair { fst, snd } => fst + snd
            end;
        /std/print(Nat/to_str(f(Pair { fst = 3, snd = 4 })))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
}

// A struct match-arm pattern desugars to the same `proj`/`proj_label` calls an ordinary projection uses, so representation privacy is inherited automatically and unmodified — matching `struct_private_projection_rejected` in `structs.rs`, but reached through a match arm instead of `.0`.
#[test]
fn struct_arm_privacy_is_enforced() {
    let source = r#"
        use /std/{Nat, Handle};
        mod Celsius
            use /std/{Nat};
            pub struct Celsius : Type { Nat }
            pub let of_nat(n : Nat) -> Celsius = Celsius { n };
        end
        let c : Celsius/Celsius = Celsius/of_nat(42);
        match c
        | Celsius/Celsius { n } => /std/print(Nat/to_str(n))
        end
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(source, system).unwrap_err();
    assert!(
        error.contains("field") && error.contains("private"),
        "unexpected error: {error}"
    );
}

// Two rows write a tuple pattern of different arity in the same column — there is no single shape to explode into projections.
#[test]
fn matrix_match_rejects_inconsistent_tuple_arity() {
    let source = r#"
        use /std/{Nat, Handle};
        let f(p : { Nat, Nat }) -> Nat =
            match p
            | (x, y) => x
            | (x,) => x
            end;
        /std/print(Nat/to_str(f((3, 4))))
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(source, system).unwrap_err();
    assert!(
        error.contains("disagree on shape"),
        "unexpected error: {error}"
    );
}

// Two rows are identical in every column, several levels deep — an overlapping arm Path A's full-enumeration model has no priority order to resolve.
#[test]
fn matrix_match_rejects_duplicate_row() {
    let source = r#"
        use /std/{Option, Nat, Handle};
        let f(a : Option(Nat), b : Option(Nat)) -> Nat =
            match (a, b)
            | (some(x), some(y)) => x + y
            | (some(x), some(y)) => x
            | (some(x), none()) => x
            | (none(), some(y)) => y
            | (none(), none()) => 0
            end;
        /std/print(Nat/to_str(f(Option/some(3), Option/some(4))))
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(source, system).unwrap_err();
    assert!(
        error.contains("duplicate or overlapping"),
        "unexpected error: {error}"
    );
}

// A literal repeated constructor tag in a flat, single-column match — the pre-existing bug this work also fixed (it used to silently collapse to whichever arm's tag survived `BTreeMap` collection, dropping the other).
#[test]
fn matrix_match_rejects_duplicate_flat_tag() {
    let source = r#"
        use /std/{Option, Nat, Handle};
        let f(o : Option(Nat)) -> Nat =
            match o
            | some(a) => a
            | some(b) => b
            | none() => 0
            end;
        /std/print(Nat/to_str(f(Option/some(3))))
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(source, system).unwrap_err();
    assert!(
        error.contains("duplicate or overlapping"),
        "unexpected error: {error}"
    );
}

// A plain binder row (Path A's forbidden catch-all) mixed with a concrete constructor row in the same column.
#[test]
fn matrix_match_rejects_mixed_binder_and_ctor_column() {
    let source = r#"
        use /std/{Option, Nat, Handle};
        let f(o : Option(Nat)) -> Nat =
            match o
            | x => 0
            | some(y) => y
            end;
        /std/print(Nat/to_str(f(Option/some(3))))
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(source, system).unwrap_err();
    assert!(
        error.contains("disagree on shape"),
        "unexpected error: {error}"
    );
}

// A dependent motive requires the head to dispatch on a constructor tag directly — there is no core `Match` node for a tuple-headed match to attach it to.
#[test]
fn matrix_match_rejects_a_motive_on_tuple_head() {
    // A tuple-headed matrix explodes into projections and builds no core `Match` node, so there is no eliminator for a motive to be checked against. Rejected rather than silently discarded.
    let source = r#"
        use /std/{Nat, Handle};
        let f(p : { Nat, Nat }) -> Nat =
            match p : (q) => Nat
            | (x, y) => x
            end;
        /std/print(Nat/to_str(f((3, 4))))
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(source, system).unwrap_err();
    assert!(
        error.contains("written motive"),
        "unexpected error: {error}"
    );
}

// A `Nat` literal leaf (`0`/`n + 1; ih`) nested inside a constructor payload.
#[test]
fn nested_nat_pattern_dispatches_by_shape() {
    let source = r#"
        use /std/{Option, Nat, Handle};
        let f(o : Option(Nat)) -> Nat =
            match o
            | some(0) => 0
            | some(n + 1; ih) => n
            | none() => 1
            end;
        /std/print(Nat/to_str(f(Option/some(3))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"2");
}

// An `Lst` literal leaf (`[]`/`[h, ..t]`) nested inside a tuple field.
#[test]
fn nested_lst_pattern_dispatches_by_shape() {
    let source = r#"
        use /std/{Nat, Lst, Handle};
        let f(p : { Nat, Lst(Nat) }) -> Nat =
            match p
            | (x, []) => x
            | (x, [h, ..t]) => h
            end;
        /std/print(Nat/to_str(f((0, [7, 8]))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
}

// A `Bytes` literal leaf (`x[]`/`x[h, ..t]`) nested inside a tuple field.
#[test]
fn nested_bin_pattern_dispatches_by_shape() {
    let source = r#"
        use /std/{Nat, Byte, Bytes, Str, Handle};
        let f(p : { Nat, Bytes }) -> Nat =
            match p
            | (x, x[]) => x
            | (x, x[h, ..t]) => Byte/to_nat(h)
            end;
        /std/print(Nat/to_str(f((0, Str/to_bytes("A")))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"65");
}

// A `Bool` literal leaf (`true`/`false`) nested inside a constructor payload — two full rows, since a bare top-level `true`/`false` would otherwise be swallowed by the separate flat `parse_bool_match` before ever reaching the matrix grammar.
#[test]
fn nested_bool_pattern_dispatches_by_shape() {
    let source = r#"
        use /std/{Bool, Nat, Handle};
         pub induct Pair(A : Type, B : Type) : pub Type
        | pair(A, B)
        end
        let f(p : Pair(Bool, Nat)) -> Nat =
            match p
            | pair(true, y) => y
            | pair(false, y) => y + 1
            end;
        /std/print(Nat/to_str(f(Pair/pair(false, 4))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"5");
}

// A nested `Nat` pattern column missing its `0` case entirely — reachable only through the matrix grammar (the flat two-branch `parse_nat_match` can't even express incompleteness, since it requires both cases up front). These four hardcoded carriers have no core-side exhaustiveness mechanism to fall back on, unlike an ordinary constructor tag.
#[test]
fn matrix_match_rejects_incomplete_nat_pattern() {
    let source = r#"
        use /std/{Nat, Handle};
        let f(n : Nat) -> Nat =
            match n
            | n2 + 1; ih => n2
            end;
        /std/print(Nat/to_str(f(3)))
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(source, system).unwrap_err();
    assert!(
        error.contains("both of its cases"),
        "unexpected error: {error}"
    );
}

// A dependent motive is legal on a top-level `Nat` head too, not just a `Ctor` head — `BoolMatch`/`NatMatch::Induction`/`LstMatch`/`BinMatch` all already support the full motive ladder flat today. The arms are written succ-case-first: written zero-then-succ (in that literal order) is valid input to the pre-existing flat `parse_nat_match` grammar too, which would swallow the source before it ever reached the matrix compiler — Path A gives rows no priority order, so reordering doesn't change the meaning, only which parser accepts it.
#[test]
fn matrix_match_allows_dependent_motive_on_nat_head() {
    let source = r#"
        use /std/{Nat, Handle};
        let f(n : Nat) -> Nat =
            match n : (m) => Nat
            | m + 1; ih => m
            | 0 => 0
            end;
        /std/print(Nat/to_str(f(3)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"2");
}

// Regression test mirroring `flat_option_match_lowers_without_synthetic_indirection`: a single, non-nested `some(0)`/`some(n + 1; ih)`/`none()` match must lower and run correctly end-to-end, exercising `compile_ctor`'s and `compile_nat`'s single-row fast paths together — guarding against reintroducing the erasure hint-compounding bug for the new carrier leaves.
#[test]
fn nested_nat_zero_pattern_lowers_without_synthetic_indirection() {
    let source = r#"
        use /std/{Option, Nat, Handle};
        let f(o : Option(Nat)) -> Nat =
            match o
            | some(0) => 0
            | some(n + 1; ih) => n
            | none() => 1
            end;
        /std/print(Nat/to_str(f(Option/some(1))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"0");
}

// Nested literal dispatch as emitted wasm: a runtime-tainted `n == 5` inside `some(n)` selects the `some(5)` arm; the `_` fallthrough covers every other value (and `none()`). Exercises `compile_nat`'s switch mode — a `Cases::Switch` reached through a constructor payload — at runtime rather than folded.
#[test]
fn nested_nat_literal_dispatch_selects_matching_case() {
    let source = r#"
        use /std/{Option, Nat, Bytes, rand, Handle};
        let z = Bytes/len(rand/bin(0));
        let n = Nat/add(z, 5);
        let hit =
            match Option/some(n)
            | some(5) => Nat/add(z, 700)
            | _ => Nat/add(z, 999)
            end;
        /std/print(Nat/to_str(hit))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"700");
}

#[test]
fn nested_nat_literal_dispatch_falls_through_to_default() {
    let source = r#"
        use /std/{Option, Nat, Bytes, rand, Handle};
        let z = Bytes/len(rand/bin(0));
        let n = Nat/add(z, 6);
        let miss =
            match Option/some(n)
            | some(5) => Nat/add(z, 700)
            | _ => Nat/add(z, 999)
            end;
        /std/print(Nat/to_str(miss))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"999");
}

#[test]
fn effectful_match_scrutinee_runs_once() {
    let source = r#"
        use /std/{File, Handle, Async};
        match Async/block_on(File/with("log.txt", File/Mode/append(), (f) => File/write(f, /std/Str/to_bytes("x"))))
        | failure(_) => /std/print("deadlock")
        | success(outcome) =>
            match outcome
            | success(_) => /std/print("ok")
            | failure(_) => /std/print("error")
            end
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"ok");
    assert_eq!(io.file(b"log.txt"), Some(b"x".to_vec()));
}

// `choose`, exercised as emitted wasm rather than folded: `rand/bin(0)` is length 0 so `z` is a runtime-opaque 0, and `n` is a runtime 2. The first *true* condition wins — `n <= 0` and `n <= 1` are false, `n <= 2` selects `300`, and the later-true `_` default is never reached.
#[test]
fn choose_selects_first_true_arm() {
    let source = r#"
        use /std/{Nat, Bytes, rand, Handle};
        let z = Bytes/len(rand/bin(0));
        let n = Nat/add(z, 2);
        let result =
            choose
            | n <= 0 => Nat/add(z, 100)
            | n <= 1 => Nat/add(z, 200)
            | n <= 2 => Nat/add(z, 300)
            | _ => Nat/add(z, 999)
            end;
        /std/print(Nat/to_str(result))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"300");
}

// A `choose` with no condition arms is just its default. Runtime-tainted so it runs as wasm.
#[test]
fn choose_default_only() {
    let source = r#"
        use /std/{Nat, Bytes, rand, Handle};
        let z = Bytes/len(rand/bin(0));
        let result =
            choose
            | _ => Nat/add(z, 42)
            end;
        /std/print(Nat/to_str(result))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"42");
}

// A deeper condition's effect fires only when it is reached: `probe` prints its tag as a side effect, so an all-false-but-first ladder would print every tag, but a ladder whose first condition is true prints only that tag. The nested `Bool` lowering keeps deeper conditions inside the previous false branch.
#[test]
fn choose_evaluates_conditions_lazily() {
    let source = r#"
        use /std/{Nat, Bytes, rand, Handle, Bool, Str};
        let z = Bytes/len(rand/bin(0));
        let probe(tag : Str, r : Bool) -> Bool =
            let _ = /std/print(tag);
            r;
        let result =
            choose
            | probe("a", true)  => Nat/add(z, 1)
            | probe("b", false) => Nat/add(z, 2)
            | _ => Nat/add(z, 9)
            end;
        /std/print(Nat/to_str(result))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    // "a" from the winning first condition, then "1" — "b" is never evaluated.
    assert_eq!(io.output(), b"a1");
}

// A headed inductive match with a `| _ =>` catch-all: enumerated constructors take their arm, everything else the default. rand-tainted so it runs as wasm.
#[test]
fn inductive_match_catch_all_covers_unenumerated_constructors() {
    let source = r#"
        use /std/{Option, Nat, Bytes, rand, Handle};
        let f(o : Option(Nat)) -> Nat =
            match o
            | some(x) => x + 10
            | _ => 99
            end;
        let z = Bytes/len(rand/bin(0));
        /std/print(Nat/to_str((f(Option/some(5)) + f(Option/none())) + z))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    // some(5) → 15 via its arm; none() → 99 via the catch-all; 15 + 99 = 114.
    assert_eq!(io.output(), b"114");
}

// A `choose` bind arm `| pattern = value =>` (Rust `if let`): fires and binds when `value` matches, else falls through to the rest of the ladder.
#[test]
fn choose_bind_arm_destructures_or_falls_through() {
    let source = r#"
        use /std/{Option, Nat, Bytes, rand, Handle};
        let f(o : Option(Nat)) -> Nat =
            choose
            | some(x) = o => x + 10
            | _ => 99
            end;
        let z = Bytes/len(rand/bin(0));
        /std/print(Nat/to_str((f(Option/some(5)) + f(Option/none())) + z))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"114");
}

// A bind arm whose pattern is refutable at *two* points (`some` and the `cons` nested in its payload): the rest-of-ladder is shared through a nullary thunk, reached whether the outer `some` or the inner cons fails to match.
#[test]
fn choose_nested_bind_shares_the_fallthrough() {
    let source = r#"
        use /std/{Option, Lst, Nat, Bytes, rand, Handle};
        let f(o : Option(Lst(Nat))) -> Nat =
            choose
            | some([h, ..t]) = o => h + 1
            | _ => 99
            end;
        let z = Bytes/len(rand/bin(0));
        let a = f(Option/some([5, 6, 7]));
        let b = f(Option/some([]));
        let c = f(Option/none());
        /std/print(Nat/to_str(((a + b) + c) + z))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    // some([5,..]) → 6; some([]) → 99 (inner cons fails); none() → 99 (outer some fails). 6 + 99 + 99 = 204.
    assert_eq!(io.output(), b"204");
}

// === Motives ================================================================
//
// A motive is a term checked against the eliminator's motive type, `(ī : Ī(p̄)) -> I(p̄, ī) -> Sort`. There is no motive grammar: what follows `:` is parsed by `parse_term` and checked like any other term.

// The motive need not be a lambda at all. A top-level family of the right type is eta-expanded into the motive scope, so an elimination can name the family it proves rather than restating it.
#[test]
fn a_motive_may_name_a_top_level_family() {
    let source = r#"
        use /std/{Nat, Eq};
        let discriminates(s : Nat, t : Nat, q : Eq(s, t)) -> Type = Eq(t, s);
        let flip(@x : Nat, @y : Nat, p : Eq(x, y)) -> Eq(y, x) =
            match p : discriminates
            | refl(@z) => Eq/refl()
            end;
        let _ : Eq(4, 4) = flip(Eq/refl());
        /std/print(Nat/to_str(4))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"4");
}

// A motive that ignores every binder is written with `_`s, one per index and one for the scrutinee — a constant motive is a lambda like any other, not a separate rung.
#[test]
fn a_constant_motive_on_an_indexed_family_binds_placeholders() {
    let source = r#"
        use /std/{Nat, Vec};
        let len(@T : Type, @n : Nat, v : Vec(T, n)) -> Nat =
            match v : (_, _) => Nat
            | nil() => 0
            | cons(@m, x, xs) => m + 1
            end;
        /std/print(Nat/to_str(len(Vec/cons(1, Vec/cons(2, Vec/nil())))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"2");
}

// A motive binder's annotation is an ordinary type in an ordinary position, so the scrutinee binder's annotation may name the index binders written before it — recovering the eliminated family on the motive line. This is the dependent-lambda-telescope rule (`tests::binders`) applied to a motive.
#[test]
fn a_motive_binder_annotation_may_name_earlier_index_binders() {
    let source = r#"
        use /std/{Nat, Eq};
        let flip(@A : Type, @x : A, @y : A, p : Eq(x, y)) -> Eq(y, x) =
            match p : (s : A, t : A, q : Eq(s, t)) => Eq(t, s)
            | refl(@z) => Eq/refl()
            end;
        let _ : Eq(6, 6) = flip(Eq/refl());
        /std/print(Nat/to_str(6))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"6");
}

// Plicity is expressible because the annotation is a real application: `Eq` hides its type parameter, so `Eq(s, t)` is how it is written here, and the old flat slot list that spelled it `Eq(A, s, t)` has no counterpart.
#[test]
fn a_motive_binder_annotation_obeys_the_families_plicity() {
    let source = r#"
        use /std/{Nat, Eq};
        let flip(@x : Nat, @y : Nat, p : Eq(x, y)) -> Eq(y, x) =
            match p : (s, t, q : Eq(@Nat, s, t)) => Eq(t, s)
            | refl(@z) => Eq/refl()
            end;
        let _ : Eq(7, 7) = flip(Eq/refl());
        /std/print(Nat/to_str(7))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
}

// A `| _ =>` catch-all on an indexed family. Every motive binds its indices whether or not the body uses them, so a default no longer collides with a "pattern motive": the enumerated arms are checked at their own case target indices and the default at the scrutinee's actual ones.
#[test]
fn a_default_arm_is_allowed_on_an_indexed_family() {
    let source = r#"
        use /std/{Nat, Vec};
        let head_or(@T : Type, @n : Nat, v : Vec(T, n), fallback : T) -> T =
            match v : (_, _) => T
            | cons(@m, x, xs) => x
            | _ => fallback
            end;
        let v : Vec(Nat, 2) = Vec/cons(8, Vec/cons(9, Vec/nil()));
        /std/print(Nat/to_str(head_or(v, 0)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"8");
}

// The binder count is checked against the index telescope, not inferred, so an under-bound motive reports as itself instead of as a domain mismatch.
#[test]
fn an_under_bound_motive_reports_its_binder_count() {
    let source = r#"
        use /std/{Nat, Vec};
        let len(@T : Type, @n : Nat, v : Vec(T, n)) -> Nat =
            match v : (_) => Nat
            | nil() => 0
            | cons(@m, x, xs) => m + 1
            end;
        /std/print(Nat/to_str(len(Vec/nil())))
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(source, system).unwrap_err();
    assert!(
        error.contains("motive binds 1 name(s)") && error.contains("needs 2"),
        "unexpected error: {error}"
    );
}

// A refinement key is stored at the arm and probed wherever the scrutinee is mentioned again, so the two spellings have to compare equal. They did not when an argument carried an *inferred* implicit: `g(b)` elaborates to `g(@?m, b)`, and a second occurrence mints its own `?m'`, so two terms solved to the same thing keyed differently and the arm silently refined nothing — while the identical scrutinee with the implicit written out, `g(@Bool, b)`, refined. Solved metavariables are now materialized into the key, which is what makes the two spellings one.
//
// The scrutinee's head is stuck on purpose: an unfoldable head would reduce the whole term and never reach the store at all.
#[test]
fn an_inferred_implicit_does_not_break_a_refinement_key() {
    let source = r#"
        use /std/{Eq, Bool, Str};
        let refined(f : (Bool) -> Bool, g : (@A : Type, A) -> Bool, b : Bool) -> Str =
            match f(g(b))
            | true =>
                let p : Eq(f(g(b)), true) = Eq/refl();
                "refined"
            | false => "unrefined"
            end;
        /std/print(refined((x) => x, (@A, x) => true, true))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"refined");
}
