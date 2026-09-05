//! What a value is stored and read at: boxed payloads, immediates, destination carriers, and the type a slot carries.

//! Structural acceptance fixtures. Each test compiles a small `.crs` fixture to the raw, pre-Binaryen wasm module and asserts a structural property of the emitted code — a clean natural loop for a hot kernel, direct recursion, the closure ABI only where a call is genuinely unknown — and that the raw module validates and executes without Binaryen repairing control flow.
//!
//! Emitted function names are `$func/<N>` ids — a module-wide monotonic index over every reachable function, prelude included — optionally suffixed with the source hint as `$func/<N>$hint`. The index carries identity; the hint is only origin annotation. Hot kernels are still located by a distinctive literal constant baked into their arithmetic (`65537` for LCG, `1000003` for trees) or by name-independent structure (self-recursion, the shared `$func/<N>`/`$clsr/<N>` index of a function used both directly and as a closure), never by a source name. A genuine irreducible-cycle dispatcher is the `loop $$dispatch/<anchor>` the emitter names in `into_wasm::expr_emitter`; an ordinary constructor-tag `switch` is not a dispatcher whatever shape it takes — a `br_table` over `$case$N`/`$tail` labels for three or more cases, a plain `if` for the two-way and one-way shapes.

use crate::tests::{cont_optm, run};

use super::test_support::*;

/// T5: constructor payloads are untouched, which is what makes the locals-only scope *observable* rather than merely intended. A `Tree/node` carries its `Nat` in a `$tuple/…` field, and every such field stays `(ref null any)` — the representation analysis reaches locals and block parameters, never a heap layout, because a field is a contract between an allocation site and every reader of it rather than one function's private decision. Widening this is the successor's subject; until then a scalar field appearing here means the scope leaked.
#[test]
fn trees_constructor_payloads_stay_boxed() {
    let wat = wat(TREES);

    let scalar_fields = wat
        .lines()
        .filter(|line| line.contains("(type $tuple/") || line.trim().starts_with("(field "))
        .filter(|line| line.contains("(field $") && !line.contains("(ref null any)"))
        .collect::<Vec<_>>();

    assert!(
        scalar_fields.is_empty(),
        "tuple payloads are uniformly boxed: {scalar_fields:?}"
    );
}

/// T6: the leaf constructor rides its payload — the immediate encoding. `build`'s leaf arm returns the payload with no allocation, so its body holds exactly one construction (the node's), and `sum` discriminates leaf from node with `ref.test (ref i31)` in place of a tag read — with only one boxed constructor the tag is never read, so no `$tuple/1` cast survives in `sum`.
///
/// # What the encoding was worth, and how to retake it
///
/// Native binaries, the ladder's protocol — `cargo run --package curios -- compile programs/trees/trees.crs -o /tmp/trees`, then `echo 21 | /usr/bin/time -v /tmp/trees`, five runs, `user` seconds and max RSS. Taken **2026-08-17** on x86-64 Linux, the before row the same day at the commit before the encoding:
///
/// | Encoding | `user` | Max RSS |
/// | --- | --- | --- |
/// | tagged leaves | 0.47–0.53 s | 266 MB |
/// | leaves ride their payloads | 0.25–0.27 s | 134 MB |
///
/// Leaves are half of a perfect tree's 2^(D+1)−1 objects, and under the all-live semi-space collector halving the live bytes also halves what every collection copies — which is why the time falls with the memory rather than by the allocation count alone. `lcg` is unmoved (no variants). Both programs printed their anchors (`trees(10) = 96122`, `trees(21) = 536864`, `lcg(8) = 9345`) before either figure was read.
#[test]
fn trees_leaf_rides_its_payload() {
    let wat = wat(TREES);
    let functions = functions(&wat);

    let sum = function_with(&functions, "1000003");
    assert!(
        sum.body.contains("ref.test (ref i31)"),
        "sum dispatches on the value's kind: {}",
        sum.body
    );
    assert!(
        !sum.body.contains("$tuple/1"),
        "no tag read survives in sum: {}",
        sum.body
    );

    let build = functions
        .iter()
        .filter(|f| f.name.starts_with("$func/") && !f.body.contains("1000003"))
        .find(|f| f.self_calls() >= 2)
        .expect("build recurses directly on both subtrees");
    assert_eq!(
        build.body.matches("struct.new").count(),
        1,
        "build allocates the node and nothing else: {}",
        build.body
    );
}

/// A returned constructor is handed back as its fields rather than as a heap tuple, so nothing allocates it and nothing takes it apart.
///
/// The fixture is the intersection the return protocol exists for: too many call sites to contify, too large to inline, and a construction no caller can see. Before the protocol every one of those exclusions held and the tuple survived; the assertion is that the callee both declares several results and allocates nothing to fill them.
///
/// The premise is checked before the claim, because it is the half that decays: "too large to inline" is a statement about a constant that may move, and the two exclusions around it are structural. A fixture that has quietly lost its premise asserts nothing while still passing, so the distinctness check earns its place ahead of the test's actual subject.
#[test]
fn a_returned_constructor_is_delivered_as_its_fields() {
    let wat = wat(SPLIT_RETURN);
    let functions = functions(&wat);
    // Located by their own arithmetic rather than by name, which is not stable across the passes this exercises. The callers are expected to fold into the entry, being single-site; what must not happen is the callee folding in with them.
    let advance = function_with(&functions, "20011");
    for caller in ["30011", "40009"] {
        let home = functions
            .iter()
            .find(|function| function.body.contains(caller))
            .unwrap_or_else(|| panic!("a caller keeps its own modulus somewhere: {caller}"));
        assert_ne!(
            advance.name, home.name,
            "the callee must stay a function distinct from its callers, or there is no returned construction to deliver",
        );
    }

    assert!(
        advance
            .body
            .contains("(result (ref null any) (ref null any) (ref null any) (ref null any))"),
        "the callee must return the class's four slots: {}",
        advance.name,
    );
    assert!(
        !advance.body.contains("struct.new") && !advance.body.contains("array.new"),
        "nothing may be allocated to carry a result that is now handed back in registers: {}",
        advance.name,
    );
}

/// A variant-width filler is built at the carrier its destination slot is held at, not at the filler's own.
///
/// `fields.rs` justified the filler by unreadness — "a read at that index is reachable only where the discriminant says a wider constructor travelled" — and that was false of the emitted code, because `Context::jump_instrs` coerces *every* edge argument to the destination parameter's carrier before the tag is examined. A literal `Nat(0)` standing in a raw `Flt` slot therefore reached a `ref.cast (ref $flt)` over an `i31` and trapped, on the `none` edge, for a value nothing would have read.
///
/// The premise is asserted before the claim, and it is the half that decays: a pass that stopped raising this parameter to a register, or stopped splitting the variant at all, would leave a fixture that passes while measuring nothing. `f32.const 0` in the loop is the filler — the fixture's own float constants are `0.25`, `1.5` and `2`, none of them zero — so its presence says both that the split fired and that the slot is raw.
///
/// **Positive control, run 2026-08-18.** Restoring `CpsAtom::Literal(CpsLiteral::Nat(0))` at the `split_parameters` filler site and rebuilding makes this fixture fail as `execution failed: error while executing at wasm backtrace: 0: 0x70b - <wasm function 4>`. Reproduce by reverting that one line.
#[test]
fn a_variant_filler_is_built_at_its_destination_carrier() {
    let wat = wat(VARIANT_FILLER);
    assert!(
        wat.contains("f32.const 0\n"),
        "the premise: a filler must reach a register-held `Flt` slot, or this fixture measures nothing",
    );

    let args = ["prog", "a", "b", "c"];
    assert_eq!(
        run_raw(VARIANT_FILLER, &args),
        b"+2".to_vec(),
        "the `none` edge must carry a filler the destination can hold, rather than one it casts and traps on",
    );
}

/// G6: the raw, pre-Binaryen wasm validates and executes without Binaryen repairing control flow. `run_raw` Cranelift-compiles the raw bytes directly (validation, including control-flow well-formedness, happens there) and runs them; its output must match the ordinary Binaryen path for the same input.
#[test]
fn raw_wasm_validates_and_executes_without_binaryen() {
    for (label, source) in [
        ("lcg", LCG),
        ("trees", TREES),
        ("higher-order", HIGHER_ORDER),
        ("direct/escaping", DIRECT_ESCAPING),
        ("function-only", FUNCTION_ONLY),
        ("mutual-recursion", MUTUAL_RECURSION),
        ("split-return", SPLIT_RETURN),
        ("looped-pick", LOOPED_PICK),
        ("variant-filler", VARIANT_FILLER),
    ] {
        let args = ["prog", "a", "b", "c"];
        let raw = run_raw(source, &args);
        assert!(!raw.is_empty(), "{label} raw module produced output");
        assert_eq!(
            raw,
            run_binaryen(source, &args),
            "{label} raw output must match the Binaryen-optimized output",
        );
    }
}

/// The `Immediate` family encoding's admission is pinned against the packed immediates: a family whose unary constructor holds a `Nat` — *always* an i31 — keeps the bare encoding and its `IsImmediate` dispatch, while the same family over `Bytes` stays `Tagged`, because a packed value is only *sometimes* immediate and a bare small `Bytes` beside boxed siblings would collide with the discrimination exactly when it boxes. `FieldShape::Opaque` for packed carriers is the invariant; this is its consequence, stated where a future widening would trip it.
#[test]
fn packed_unary_payload_declines_the_immediate_encoding() {
    let nat_family = r#"
        use /std/{Nat, List, Str, proc};

        induct Gauge: Type
        | tiny(Nat)
        | pair(Nat, Nat)
        end

        let taint = List/len(proc/args!);
        let g = match taint | 0 => Gauge/tiny(7) | _ => Gauge/pair(taint, taint) end;
        match g
        | tiny(n) => /std/print(Str/concat(Nat/to_str(n), "\n"))
        | pair(a, b) => /std/print(Str/concat(Nat/to_str(a + b), "\n"))
        end
        "#;
    let bytes_family = r#"
        use /std/{Nat, List, Bytes, Str, proc};

        induct Gauge: Type
        | tiny(Bytes)
        | pair(Nat, Nat)
        end

        let taint = List/len(proc/args!);
        let g = match taint | 0 => Gauge/tiny(x[7]) | _ => Gauge/pair(taint, taint) end;
        match g
        | tiny(inner) => /std/print(Str/concat(Nat/to_str(Bytes/len(inner)), "\n"))
        | pair(a, b) => /std/print(Str/concat(Nat/to_str(a + b), "\n"))
        end
        "#;

    assert!(
        cont_optm(nat_family).contains("IsImmediate"),
        "an always-immediate unary payload keeps the bare encoding"
    );
    let dump = cont_optm(bytes_family);
    assert!(
        !dump.contains("IsImmediate"),
        "a sometimes-immediate packed payload declines it: {dump}"
    );

    // The bit grain declines identically: a `Bits` payload is sometimes-immediate too.
    let bits_family = r#"
        use /std/{Nat, List, Bits, Str, proc};

        induct Gauge: Type
        | tiny(Bits)
        | pair(Nat, Nat)
        end

        let taint = List/len(proc/args!);
        let g = match taint | 0 => Gauge/tiny(b[1]) | _ => Gauge/pair(taint, taint) end;
        match g
        | tiny(inner) => /std/print(Str/concat(Nat/to_str(Bits/len(inner)), "\n"))
        | pair(a, b) => /std/print(Str/concat(Nat/to_str(a + b), "\n"))
        end
        "#;
    assert!(
        !cont_optm(bits_family).contains("IsImmediate"),
        "a sometimes-immediate bit payload declines it too"
    );

    assert_eq!(run(nat_family), b"7\n");
    assert_eq!(run(bytes_family), b"1\n");
    assert_eq!(run(bits_family), b"1\n");
}

/// The bit grain rides the i31 exactly as the byte grain does: small literals and appends stay immediate — the equality against a compile-time literal is the canonicity check — an append past the 26-bit envelope boxes into the rope world, and two separately grown ropes still compare by content. The taint keeps every value out of constant folding, and it also rides the recursion depth: a *literal* depth sends elaboration into a runaway reduction of the open append spine — a pre-existing pathology recorded in the map-wall follow-ups, independent of the immediate representation.
#[test]
fn small_bits_ride_the_immediate_and_overflow_boxes() {
    let source = r#"
        use /std/{Nat, List, Bits, Bool, Str, proc};
        let taint = List/len(proc/args!);
        let t: Bool = taint == 0;
        let small = b[t, 1, 0, t];
        let grown = b[..small, 1];
        let widen(n: Nat, acc: Bits) -> Bits =
            match n | 0 => acc | _ => widen(n - 1, b[..acc, t]) end;
        let wide = widen(30 + taint, grown);
        let wide2 = widen(30 + taint, grown);
        match (grown == b[1, 1, 0, 1, 1]) && (wide == wide2) && (Bits/len(wide) == 35 + taint)
        | true => /std/print("ok\n")
        | false => /std/print("bad\n")
        end
        "#;

    assert_eq!(run(source), b"ok\n");
}

/// A two-way dispatch is a conditional branch, not a jump table.
///
/// Cases `{0, 1}` with nothing else reachable — every `Bool` match, and every exhaustive two-constructor tag — decide in one compare. A `br_table` is a bounds check, a `csel`, a dependent load and an indirect branch on the ISA lowerings in this pipeline's chain, and nothing below the emitter narrows it: Cranelift's aarch64 rule builds a `JTSequence` whatever the table's size, and Binaryen leaves a two-entry table alone (measured 2026-08-20 on `spines`: 60 tables emitted before this shape landed, 57 of them surviving `-O2`; 9 emitted after).
///
/// Asserted on the fixture's own kernel rather than the module, since the prelude rides along and its wider families table legitimately. Both dispatches stay runtime-tainted so neither folds — a closed condition is decided at compile time and emits no dispatch at all.
#[test]
fn a_two_way_dispatch_is_a_branch_not_a_table() {
    let source = r#"
        use /std/{Nat, Bool, List, Str, proc};

        induct Pair : Type
        | left(Nat)
        | right(Nat)
        end

        let taint = List/len(proc/args!);
        let flag : Bool = taint == 0;
        let decide(n : Nat, acc : Nat) -> Nat =
            match n : (_) => Nat
            | 0 => acc
            | m + 1; ih =>
                let chosen : Pair = match flag | true => Pair/left(m) | false => Pair/right(m) end;
                let folded = match chosen | left(x) => x + 65521 | right(x) => x + 65519 end;
                decide(m, acc + folded)
            end;
        /std/print(Nat/to_str(decide(taint + 3, 0) + decide(taint + 4, 1)))
        "#;

    let wat = wat(source);
    let functions = functions(&wat);
    let kernel = function_with(&functions, "65521");

    assert!(
        !kernel.body.contains("br_table"),
        "a two-way dispatch must branch rather than table: {}",
        kernel.body
    );
}

/// An aggregate is read at its own final type — a variant family through its own type, a structural tuple through the roster.
///
/// The `$tuple/N` family used to be a prefix subtype chain — `$tuple/4 <: $tuple/3 <: … <: $tuple/1` — so one `ref.cast (ref $tuple/1)` could read field 0 of any tuple whatever its arity. That is what made the cast a *host call*: wasmtime's `is_subtype` short-circuits only when the target is final, so every cast to a prefix of a wider object took the `is_subtype` libcall, and every real node is wider than the prefix it was read through.
///
/// Final types delete the short-circuit's precondition, and the reader finds the object's exact type by exhausting the roster instead. Correctness does not rest on an object's arity being its constructor's, which `cps/fields.rs` makes false whenever a narrow constructor materialises at its region's width.
///
/// Family keying then removed the cascade from the case it was built for. A variant family is one final struct at its own width, so a family read is a single exact cast and the roster search survives only for structural tuples — this fixture pins the family half, and the `$tuple/` finality check below still pins the other.
///
/// **Measured 2026-08-20, x86-64 Linux, release, whole-process, min of 5, anchors checked on every run.** `chain` 339.6 → 131.1 ms (**−61.4%**), `spines` 100.5 → 78.8 ms (**−21.6%**), against `lcg` +0.8%, `trees` +0.8% and `churn` +0.1% — all three inside noise, and each for a stated reason: `lcg` declares no variant at all, `trees`' leaf rides the i31 so its family never reads a tag and its one boxed constructor casts exactly, and `churn`'s hot loop is not a variant walk. The two that moved are exactly the two whose hot loop reads a multi-constructor heap family, which is what makes the figure a class rather than a coincidence.
#[test]
fn a_tuple_is_read_at_its_own_final_type() {
    let source = r#"
        use /std/{Nat, List, Str, proc};

        induct Chain : Type
        | stop()
        | link(Nat, Chain)
        end

        let taint = List/len(proc/args!);
        let build(n : Nat, acc : Chain) -> Chain =
            match n : (_) => Chain
            | 0 => acc
            | m + 1; ih => build(m, Chain/link(m, acc))
            end;
        let total(c : Chain, acc : Nat) -> Nat =
            match c
            | stop() => acc
            | link(v, rest) => total(rest, (acc + v) % 999983)
            end;
        /std/print(Nat/to_str(total(build(taint + 3, Chain/stop()), 0) + total(build(taint + 4, Chain/stop()), 1)))
        "#;

    let wat = wat(source);

    // No tuple type is a subtype of anything: the printer renders a final, supertype-less struct
    // without a `sub` wrapper, so any `sub` on one of these lines is the chain coming back.
    for line in wat.lines().filter(|line| line.contains("(type $tuple/")) {
        assert!(
            !line.contains("sub"),
            "tuple types must be final and unrelated: {line}"
        );
    }

    // Since family keying, `Chain` is one final `$row/N$/Chain` at the family's width rather than a
    // `$tuple/3` beside a `$tuple/1`, so the walk needs no cascade at all: the object's type is a
    // fact of the family, and the read is one exact cast followed by the field.
    let functions = functions(&wat);
    let kernel = function_with(&functions, "999983");
    assert!(
        kernel.body.contains("ref.cast (ref $row/") && kernel.body.contains("struct.get $row/"),
        "the walk reads the family at its own exact type: {}",
        kernel.body
    );
    assert!(
        !kernel.body.contains("ref.test"),
        "and needs no roster cascade to find it: {}",
        kernel.body
    );

    // The family types are final and unrelated too — the same property, for the types that
    // replaced the tuples on this path.
    for line in wat.lines().filter(|line| line.contains("(type $row/")) {
        assert!(
            !line.contains("sub"),
            "family types must be final and unrelated: {line}"
        );
    }
}

/// A family slot is declared at the carrier its recorded shape names, not at `anyref`.
///
/// `Chain` is `stop() | link(Nat, Chain)`, so its slots are the tag, one unsigned immediate, and one uniform reference — and the emitted struct says exactly that: `i8`, `i32`, `anyref`. Two costs die with the declaration. The tag reads back through `struct.get_u` out of a packed byte, where a uniform slot cast an `i31` and unboxed it; and the `Nat` payload arrives in a register, where the same slot boxed at every store and unboxed at every read. That pair is the largest static population in every corpus program and prices at 17% of a dispatch-heavy fold's per-element budget (`shapes.rs`'s `boxed_field_read_measurements`).
///
/// The slots are grouped by carrier rather than by field position, which is what keeps this from widening the family: a carrier's range is as wide as the constructor holding the most fields of it, so constructors agreeing on a carrier share slots. `shapes.rs`'s `slot_layout_probe` is that decision's figure — over the standard library the rule types 22 slots against positional assignment's 11, and only three families widen at all, none of them on a hot allocation path.
#[test]
fn a_monomorphic_slot_carries_its_own_type() {
    let source = r#"
        use /std/{Nat, List, Str, proc};

        induct Chain : Type
        | stop()
        | link(Nat, Chain)
        end

        let taint = List/len(proc/args!);
        let build(n : Nat, acc : Chain) -> Chain =
            match n : (_) => Chain
            | 0 => acc
            | m + 1; ih => build(m, Chain/link(m, acc))
            end;
        let total(c : Chain, acc : Nat) -> Nat =
            match c
            | stop() => acc
            | link(v, rest) => total(rest, (acc + v) % 999983)
            end;
        /std/print(Nat/to_str(total(build(taint + 3, Chain/stop()), 0) + total(build(taint + 4, Chain/stop()), 1)))
        "#;

    let wat = wat(source);

    let declaration = wat
        .lines()
        .find(|line| line.contains("(type $row/") && line.contains("Chain"))
        .expect("the Chain family declares a type");
    assert!(
        declaration.contains("i8") && declaration.contains("i32"),
        "the tag packs into a byte and the `Nat` payload is a raw scalar: {declaration}"
    );

    let functions = functions(&wat);
    let kernel = function_with(&functions, "999983");
    assert!(
        kernel.body.contains("struct.get_u $row/"),
        "the tag is read out of its packed byte, with nothing to unbox: {}",
        kernel.body
    );
    // Every read of a slot lands in a local of the slot's own carrier, so none of them is followed
    // by an unbox. The casts that remain in this walk are the function's own `anyref` parameter and
    // a hoisted literal — neither is a field, and both are somebody else's campaign.
    let lines: Vec<&str> = kernel.body.lines().collect();
    for (index, line) in lines.iter().enumerate() {
        if !line.contains("struct.get") || !line.contains("$row/") {
            continue;
        }
        assert!(
            !lines[index + 1].contains("ref.cast"),
            "a slot read is followed by an unbox: {line} then {}",
            lines[index + 1]
        );
    }
}
