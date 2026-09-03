//! Packed literals and field stores fusing to a flat build, and the regrown store that stays lazy.

//! Structural acceptance fixtures. Each test compiles a small `.crs` fixture to the raw, pre-Binaryen wasm module and asserts a structural property of the emitted code — a clean natural loop for a hot kernel, direct recursion, the closure ABI only where a call is genuinely unknown — and that the raw module validates and executes without Binaryen repairing control flow.
//!
//! Emitted function names are `$func/<N>` ids — a module-wide monotonic index over every reachable function, prelude included — optionally suffixed with the source hint as `$func/<N>$hint`. The index carries identity; the hint is only origin annotation. Hot kernels are still located by a distinctive literal constant baked into their arithmetic (`65537` for LCG, `1000003` for trees) or by name-independent structure (self-recursion, the shared `$func/<N>`/`$clsr/<N>` index of a function used both directly and as a closure), never by a source name. A genuine irreducible-cycle dispatcher is the `loop $$dispatch/<anchor>` the emitter names in `into_wasm::expr_emitter`; an ordinary constructor-tag `switch` is not a dispatcher whatever shape it takes — a `br_table` over `$case$N`/`$tail` labels for three or more cases, a plain `if` for the two-way and one-way shapes.

use crate::tests::{census_settles, cont_optm, run};

/// A packed literal's non-constant atoms fuse into one flat chunk build (`fuse_append_chains` in `curios-cont`): the byte literal's two runtime atoms become a `BinChunk(X, 2)` and the bit literal's one a `BinChunk(B, 1)`, in place of the append-per-atom chains the lowering honestly writes, and the program still prints what the chains would. The taint keeps every atom out of constant folding, so the chunks survive to emission and the equality runs over runtime-built values.
#[test]
fn tainted_packed_literals_fuse_to_flat_chunks() {
    let source = r#"
        use /std/{Nat, List, Byte, Bytes, Bits, Bool, Str, Handle, proc};
        let taint = List/len(proc/args!);
        let a: Byte = match taint | 0 => 7 | _ => 9 end;
        let y: Byte = match taint | 0 => 8 | _ => 10 end;
        let bytes = x[a, y, 0x21];
        let t: Bool = taint == 0;
        let bits = b[t, 1];
        match bytes == x[7, 8, 0x21] && bits == b[1, 1]
        | true => /std/print("ok\n")
        | false => /std/print("bad\n")
        end
        "#;

    let dump = cont_optm(source);
    assert!(
        dump.contains("BinChunk(X, 2)"),
        "the byte atoms fuse into one chunk: {dump}"
    );
    assert!(
        dump.contains("BinChunk(B, 1)"),
        "the bit atom fuses into one chunk: {dump}"
    );

    assert_eq!(run(source), b"ok\n");
}

/// A list stored into a field the Ersd census marks indexed-only settles at the store, and the settle over the literal's unshared construction tree fuses into one exact flat build (`flatten_indexed_lists` in `curios-cont`): the spliced rebuild becomes a `ListFlat` in place of the concat-of-rope-nodes the lowering honestly writes, and the read answers the same element. The reads reach the census through `/std/List/try_get`, so the fixture also pins the census's deferral through saturated known calls — the shape every real read has at the arena.
#[test]
fn indexed_field_store_fuses_to_flat_build() {
    let source = r#"
        use /std/{Nat, List, Str, Handle, Option, proc};

        induct Box: Type
        | pack(items: List(Nat))
        end

        let taint = List/len(proc/args!);
        let pre = [taint + 1, taint + 2];
        let post = [taint + 3, taint + 4];
        let boxed = Box/pack([..pre, taint + 9, ..post]);
        match boxed
        | pack(items) =>
            match List/try_get(items, 2)
            | some(v) => /std/print(Str/concat(Nat/to_str(v), "\n"))
            | none() => /std/print("none\n")
            end
        end
        "#;

    // The named verdict first — the assertion surface — then the fused op it becomes, then the behavior.
    assert!(
        census_settles(source, "/Box", "pack", "items"),
        "the census marks the stored field indexed-only"
    );
    let dump = cont_optm(source);
    assert!(
        dump.contains("ListFlat(3)"),
        "the spliced store flattens to one exact build: {dump}"
    );

    assert_eq!(run(source), b"9\n");
}

/// An anonymous tuple's row is shared by every tuple of its width, so the census keys none of its fields: the list in one triple's first field is measured by index, the scalar in another's is never read, and the scalar is stored as it is rather than settled as a list — which trapped, `wasm unreachable`, where the program has an answer.
#[test]
fn a_shared_tuple_field_settles_nothing() {
    let source = r#"
        use /std/{Nat, List, proc};

        let n = List/len(proc/args!);
        let a: {List(Nat), Nat, Nat} = ([n, 2, 3], 0, 0);
        let b: {Nat, Nat, Nat} = (7, n, 0);
        /std/print(Nat/to_str(List/len(a.0) + b.1))
        "#;

    let dump = cont_optm(source);
    assert!(!dump.contains("ListSettle"), "{dump}");

    assert_eq!(run(source), b"3");
}

/// The builder idiom is protected: a field whose values are re-grown — the accumulator spelling `[..items, x]` — is poisoned by the census, so no settle is inserted and no flat build fires, and the loop keeps its O(1) append steps.
#[test]
fn regrown_field_store_stays_lazy() {
    let source = r#"
        use /std/{Nat, List, Str, Handle, proc};

        induct Acc: Type
        | keep(items: List(Nat))
        end

        let taint = List/len(proc/args!);
        let grow(n: Nat, acc: Acc) -> Acc =
            match n: (_) => Acc
            | 0 => acc
            | m + 1 =>
                match acc
                | keep(items) => grow(m, Acc/keep([..items, m + taint]))
                end
            end;
        let result = grow(3, Acc/keep([]));
        match result
        | keep(items) => /std/print(Str/concat(Nat/to_str(List/len(items)), "\n"))
        end
        "#;

    assert!(
        !census_settles(source, "/Acc", "keep", "items"),
        "a re-grown field is never marked"
    );
    let dump = cont_optm(source);
    assert!(
        !dump.contains("ListSettle") && !dump.contains("ListFlat"),
        "a re-grown field is never settled: {dump}"
    );

    assert_eq!(run(source), b"3\n");
}

/// The demand rule stands alone: a local concatenation whose every use is indexing-shaped — here through `try_get`'s parameter, so the fact is interprocedural — builds flat with no field store involved.
#[test]
fn indexed_local_concat_fuses_to_flat_build() {
    let source = r#"
        use /std/{Nat, List, Str, Handle, Option, proc};
        let taint = List/len(proc/args!);
        let a = [taint + 1, taint + 2];
        let b = [taint + 3, taint + 4];
        let joined = [..a, ..b];
        match List/try_get(joined, 3)
        | some(v) => /std/print(Str/concat(Nat/to_str(v), "\n"))
        | none() => /std/print("none\n")
        end
        "#;

    let dump = cont_optm(source);
    assert!(
        dump.contains("ListFlat(2)"),
        "the indexed concat flattens: {dump}"
    );

    assert_eq!(run(source), b"4\n");
}
