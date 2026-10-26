//! What a value is stored and read at: boxed payloads, immediates, destination carriers, and the type a slot carries.

use {
    super::test_support::*,
    crate::tests::{cps_optm_module, emits, run},
};

/// The tree's payloads stay `(ref null any)` in its row, which is what makes the representation analysis's locals-only scope *observable* rather than merely intended: it reaches locals and block parameters, never a heap layout, because a field is a contract between an allocation site and every reader of it rather than one function's private decision. A row's typed slots are the door's decision, made from the recorded shape, and here it types the tag alone; a scalar payload appearing here means the analysis's scope leaked.
#[test]
fn trees_constructor_payloads_stay_boxed() {
    let wat = wat(TREES);

    let declaration = row_declaration(&wat, "Tree");
    // A node's number and its two subtrees, which a leaf's number shares a slot of.
    let payloads = declaration.matches("(field $").count() - 1;
    assert!(
        declaration.contains("(field $0 i8)")
            && payloads == 3
            && declaration.matches("(ref null any)").count() == payloads,
        "the tree's payloads are uniformly boxed beside its tag: {declaration}"
    );
}

/// The leaf constructor rides its payload — the immediate encoding. `build`'s leaf arm returns the payload with no allocation, so its body holds exactly one construction (the node's), and `sum` discriminates leaf from node with `ref.test (ref i31)` in place of a tag read — with only one boxed constructor the tag is never read, so no read of the row's packed tag survives in `sum`.
///
/// # What it last measured, and how to retake it
///
/// Native binaries, the ladder's protocol — `cargo run --package curios -- compile programs/trees/trees.crs -o /tmp/trees`, then `echo 21 | /usr/bin/time -v /tmp/trees`, five runs, `user` seconds and max RSS, on x86-64 Linux:
///
/// | Encoding | `user` | Max RSS |
/// | --- | --- | --- |
/// | leaves ride their payloads | 0.25–0.27 s | 134 MB |
///
/// Leaves are half of a perfect tree's 2^(D+1)−1 objects, and under the all-live semi-space collector the live bytes a boxed leaf would add are also what every collection copies, so the encoding saves time with the memory rather than by the allocation count alone. The program prints its anchors (`trees(10) = 96122`, `trees(21) = 536864`) before any figure is read.
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
        sum.body.contains("struct.get $row/") && !sum.body.contains("struct.get_u $row/"),
        "sum reads the node's payloads and never its tag: {}",
        sum.body
    );

    // The program's own: a function of the standard library the module links may recurse twice as well.
    let build = functions
        .iter()
        .filter(|f| f.name.starts_with("$func/") && !f.name.contains("$/std/"))
        .filter(|f| !f.body.contains("1000003"))
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
/// The fixture is the intersection the return protocol exists for: too many call sites to contify, too large to inline, and a construction no caller can see — every exclusion that would otherwise let the tuple survive; the assertion is that the callee both declares several results and allocates nothing to fill them.
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

    // The four slots are the tag and three remainders a literal modulus bounds, so each is a word, and the representation hands every one back in a register.
    assert!(
        advance.body.contains("(result i32 i32 i32 i32)"),
        "the callee must return the class's four slots, each a word: {}",
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
/// Unreadness does not justify a filler — "a read at that index is reachable only where the discriminant says a wider constructor travelled" — because `Context::jump_instrs` coerces *every* edge argument to the destination parameter's carrier before the tag is examined: a literal `Nat(0)` standing in a raw `Flt` slot would reach a `ref.cast (ref $flt)` over an `i31` and trap, on the `none` edge, for a value nothing would read.
///
/// The premise is asserted before the claim, and it is the half that decays: a pass that stopped raising this parameter to a register, or stopped splitting the variant at all, would leave a fixture that passes while measuring nothing. `f64.const 0` in the loop is the filler — the fixture's own float constants are `0.25`, `1.5` and `2`, none of them zero — so its presence says both that the split fired and that the slot is raw.
///
/// **Positive control.** The filler on the `none` edge is the one the door pads `Option`'s row with, through `Module::pad`. Guessing `Atom::Literal(Literal::Nat(Natural::zero()))` in its reference arm makes this fixture fail at its premise: the edge carries an `i31`, and no `f64.const 0` is emitted.
#[test]
fn a_variant_filler_is_built_at_its_destination_carrier() {
    let wat = wat(VARIANT_FILLER);
    assert!(
        wat.contains("f64.const 0\n"),
        "the premise: a filler must reach a register-held `Flt` slot, or this fixture measures nothing",
    );

    let args = ["prog", "a", "b", "c"];
    assert_eq!(
        run_raw(VARIANT_FILLER, &args),
        b"+4".to_vec(),
        "the `none` edge must carry a filler the destination can hold, rather than one it casts and traps on",
    );
}

/// Two constructors that share a payload slot each hand it what they hold. The float is read under one tag only, so the payload stays the reference both can hand it and the error arrives as it was sent, where a register would cast it and trap.
#[test]
fn a_payload_two_constructors_share_carries_what_each_holds() {
    let args = ["prog", "a", "b", "c"];
    assert_eq!(
        run_raw(SHARED_PAYLOAD, &args),
        b"odd 5 | +8.5 | odd 7".to_vec(),
        "the `failure` edge must carry the error, and the `success` edge the float",
    );
}

/// A float a parser binds and answers after a later step is `success`'s payload beside the refusal `failure` carries, and the program compiles and reads it.
#[test]
fn a_parser_answers_a_float_it_bound_before_a_later_step() {
    let args = ["prog", "a", "b", "c"];
    assert_eq!(run_raw(PARSED_FLOAT, &args), b"+2".to_vec());
}

/// A name whose type a match computes holds a float on one branch and a string on the other, and each arrives as it was sent: the parameter they join in is a reference, since a register would cast the string before the branch that reads a float is taken.
#[test]
fn a_value_of_a_computed_type_carries_what_each_branch_holds() {
    let args = ["prog", "a", "b", "c"];
    assert_eq!(run_raw(COMPUTED_TYPE, &args), b"+9.5 | text 7".to_vec());
}

/// The raw, pre-Binaryen wasm validates and executes without Binaryen repairing control flow. `run_raw` Cranelift-compiles the raw bytes directly (validation, including control-flow well-formedness, happens there) and runs them; its output must match the ordinary Binaryen path for the same input.
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
        ("float-literal-loop", FLOAT_LITERAL_LOOP),
        ("known-parameter-seed", KNOWN_PARAMETER_SEED),
        ("call-result-seed", CALL_RESULT_SEED),
        ("pair-step", PAIR_STEP),
        ("captured-float", CAPTURED_FLOAT),
        ("stated-seeds", STATED_SEEDS),
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
        use /std/{Nat, List, Str, Process};

        induct Gauge: Type
        | tiny(Nat)
        | pair(Nat, Nat)
        end

        let entry() -> /std/Io({}) =
            let taint = List/len(Process/args()!);
            let g = match taint | 0 => Gauge/tiny(7) | _ => Gauge/pair(taint, taint) end;
            match g
            | tiny(n) => /std/print(Str/concat(Nat/to_str(n), "\n"))
            | pair(a, b) => /std/print(Str/concat(Nat/to_str(a + b), "\n"))
            end;
        entry()
        "#;
    let bytes_family = r#"
        use /std/{Nat, List, Bytes, Str, Process};

        induct Gauge: Type
        | tiny(Bytes)
        | pair(Nat, Nat)
        end

        let entry() -> /std/Io({}) =
            let taint = List/len(Process/args()!);
            let g = match taint | 0 => Gauge/tiny(x[7]) | _ => Gauge/pair(taint, taint) end;
            match g
            | tiny(inner) => /std/print(Str/concat(Nat/to_str(Bytes/len(inner)), "\n"))
            | pair(a, b) => /std/print(Str/concat(Nat/to_str(a + b), "\n"))
            end;
        entry()
        "#;

    assert!(
        emits(&cps_optm_module(nat_family), |op| *op
            == curios_cps::Intrinsic::IsImmediate),
        "an always-immediate unary payload keeps the bare encoding"
    );
    let module = cps_optm_module(bytes_family);
    assert!(
        !emits(&module, |op| *op == curios_cps::Intrinsic::IsImmediate),
        "a sometimes-immediate packed payload declines it: {module}"
    );

    assert_eq!(run(nat_family), b"7\n");
    assert_eq!(run(bytes_family), b"1\n");
}

/// A two-way dispatch is a conditional branch, not a jump table.
///
/// Cases `{0, 1}` with nothing else reachable — every `Bool` match, and every exhaustive two-constructor tag — decide in one compare. A `br_table` is a bounds check, a `csel`, a dependent load and an indirect branch on the ISA lowerings in this pipeline's chain, and nothing below the emitter narrows it: Cranelift's aarch64 rule builds a `JTSequence` whatever the table's size, and Binaryen leaves a two-entry table alone.
///
/// Asserted on the fixture's own kernel rather than the module, since the prelude rides along and its wider families table legitimately. Both dispatches stay runtime-tainted so neither folds — a closed condition is decided at compile time and emits no dispatch at all.
#[test]
fn a_two_way_dispatch_is_a_branch_not_a_table() {
    let source = r#"
        use /std/{Nat, Bool, List, Str, Process};

        induct Pair : Type
        | left(Nat)
        | right(Nat)
        end

        let entry() -> /std/Io({}) =
            let taint = List/len(Process/args()!);
            let flag : Bool = taint == 0;
            let decide(n : Nat, acc : Nat) -> Nat =
                match n : (_) => Nat
                | 0 => acc
                | m + 1; ih =>
                    let chosen : Pair = match flag | true => Pair/left(m) | false => Pair/right(m) end;
                    let folded = match chosen | left(x) => x + 65521 | right(x) => x + 65519 end;
                    decide(m, acc + folded)
                end;
            /std/print(Nat/to_str(decide(taint + 3, 0) + decide(taint + 4, 1)));
        entry()
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

/// An aggregate is read at its own final type — a variant family through its own row, a structural tuple through the roster.
///
/// A prefix subtype chain — `$tuple/4 <: $tuple/3 <: … <: $tuple/1` — would let one `ref.cast (ref $tuple/1)` read field 0 of any tuple whatever its arity, and would make the cast a *host call*: wasmtime's `is_subtype` short-circuits only when the target is final, so every cast to a prefix of a wider object would take the `is_subtype` libcall. Final types keep the short-circuit, and the reader finds a tuple's exact type by exhausting the roster instead; correctness does not rest on an object's arity being its construction's, which `curios-cps`'s field split makes false whenever a narrow tuple materialises at its region's width. A family is one final row at its own width, so a family read is a single exact cast with no roster to search — this fixture pins that half, and the `$tuple/` finality check below the other.
///
/// The programs final types move are exactly those whose hot loop reads a multi-constructor heap family, `chain` and `spines`, which makes the gain a class rather than a coincidence; `lcg` declares no variant at all, `trees`' leaf rides the i31 so its family never reads a tag and its one boxed constructor casts exactly, and `churn`'s hot loop is not a variant walk.
#[test]
fn a_tuple_is_read_at_its_own_final_type() {
    let source = r#"
        use /std/{Nat, List, Str, Process};

        induct Chain : Type
        | stop()
        | link(Nat, Chain)
        end

        let entry() -> /std/Io({}) =
            let taint = List/len(Process/args()!);
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
            /std/print(Nat/to_str(total(build(taint + 3, Chain/stop()), 0) + total(build(taint + 4, Chain/stop()), 1)));
        entry()
        "#;

    let wat = wat(source);

    // No tuple type is a subtype of anything: the printer renders a final, supertype-less struct without a `sub` wrapper, so any `sub` on one of these lines is a prefix chain.
    for line in wat.lines().filter(|line| line.contains("(type $tuple/")) {
        assert!(
            !line.contains("sub"),
            "tuple types must be final and unrelated: {line}"
        );
    }

    // `Chain` is one final `$row/N$/Chain` at the family's width, so the walk needs no cascade at all: the object's type is a fact of the family, and the read is one exact cast followed by the field.
    let functions = functions(&wat);
    let kernel = function_with(&functions, "999983");
    assert!(
        kernel.body.contains("ref.cast (ref $row/") && kernel.body.contains("struct.get $row/"),
        "the walk reads the family at its own exact type: {}",
        kernel.body
    );
    // A number's fast path tests for an i31, `ref.test (ref i31)`; a cascade is a test against a concrete type.
    assert!(
        !kernel.body.contains("ref.test (ref $"),
        "and needs no roster cascade to find it: {}",
        kernel.body
    );

    // The family types are final and unrelated too — the same property, for the rows on this path.
    for line in wat.lines().filter(|line| line.contains("(type $row/")) {
        assert!(
            !line.contains("sub"),
            "family types must be final and unrelated: {line}"
        );
    }
}

/// A rope reference enters its base through its final shapes, never through a cast to the base.
///
/// `$rope/bytes` and `$rope/list` are the supertypes of their carrier's `leaf`, `node` and `view` and hold no object of their own, so they cannot be final, and wasmtime's `ref.cast` and `ref.test` short-circuit only on an exact match: a cast to a base is the `is_subtype` libcall for every object it ever meets, and a walk over a rope met it at every element. A reference therefore enters the base by testing the three final shapes (`Table::enter`), each one inline comparison, and the read splits test the leaf by its own type. The reason is [`a_tuple_is_read_at_its_own_final_type`]'s, for the one family of types whose base has to stay non-final; the fixture reads a text walked character by character, a list read past a concatenation, a length and an equality, all over ropes.
#[test]
fn a_rope_is_entered_through_its_final_shapes() {
    let source = r#"
        use /std/{Str, Char, Nat, List, Bytes, Process};

        let entry() -> /std/Io({}) =
            let n = List/len(Process/args()!);
            let text = Str/concat(Nat/to_str(n + 7), Str/repeat("é→𝄞", 4));
            let walked = Str/fold(text, 0, (c, a) => (a * 31 + Char/to_nat(c)) % 1000003);
            let items = [..List/range(0, n + 5), ..List/range(n, n + 3)];
            let picked = match List/try_get(items, 6) | some(v) => v | none() => 0 end;
            let again = Str/concat(Nat/to_str(n + 7), Str/repeat("é→𝄞", 4));
            let same = match Str/to_bytes(text) == Str/to_bytes(again) | true => 1 | false => 0 end;
            /std/print(Nat/to_str(walked + picked + Bytes/len(Str/to_bytes(text)) + same));
        entry()
        "#;

    let wat = wat(source);

    for base in ["$rope/bytes", "$rope/list"] {
        assert!(
            wat.lines()
                .any(|line| line.contains(&format!("(type {base} (sub "))),
            "{base} is declared non-final, which is what makes a cast to it a host call"
        );
        let cast = wat.lines().map(str::trim).find(|line| {
            ["ref.cast ", "ref.test ", "br_on_cast"]
                .iter()
                .any(|instruction| line.starts_with(instruction))
                && (line.contains(&format!("(ref {base})"))
                    || line.contains(&format!("(ref null {base})")))
        });
        assert_eq!(cast, None, "no instruction casts to or tests for {base}");
    }

    assert_eq!(run(source), b"231084");
}

/// A window walk holds its end and its length in words read unsigned. A window counts down to its end, so each field is a length or what a monus of one leaves, which the representation's bounds put below `2³²`: both ride `Repr::Length` round the loop, where an offset grown by a sum each step was bounded by nothing the analysis could see and stayed a reference.
#[test]
fn a_window_walk_holds_its_end_and_length_in_words_read_unsigned() {
    let source = r#"
        use /std/{Str, Bytes, Byte, Nat, List, Process};

        let entry() -> /std/Io({}) =
            let n = List/len(Process/args()!);
            let text = Str/to_bytes(Str/concat(Nat/to_str(n), "abcdefgh"));
            /std/print(Nat/to_str(Bytes/fold(text, 7, (b, acc) => (acc * 31 + Byte/to_nat(b)) % 65521)));
        entry()
        "#;

    let module = cps_optm_module(source);
    let decided = curios_cps::representation(&module).values;
    let fields = module
        .value_ids()
        .filter_map(|value| {
            let name = module.value(value)?.debug_name.as_deref()?;
            (name.starts_with("window/") && (name.ends_with("/end") || name.ends_with("/length")))
                .then_some((name, value))
        })
        .collect::<Vec<_>>();
    assert!(
        fields.iter().any(|(name, _)| name.ends_with("/end"))
            && fields.iter().any(|(name, _)| name.ends_with("/length")),
        "the walk is a window: {fields:?}"
    );
    for (name, value) in fields {
        assert_eq!(
            decided[&value],
            curios_cps::Storage::Raw(curios_cps::Repr::Length),
            "`{name}` rides a word read unsigned"
        );
    }

    assert_eq!(run(source), b"33712");
}

/// A family slot is declared at the carrier its recorded shape names, not at `anyref`.
///
/// `Chain` is `stop() | link(Nat, Chain)`, so its slots are the tag, the family's own row, and one uniform reference for the `Nat` — an i31 or a boxed magnitude, which no single heap type names — and the emitted struct says exactly that: `i8`, `(ref null $row/…$/Chain)`, `anyref`. The tag reads back through `struct.get_u` out of a packed byte, with nothing to unbox.
///
/// The slots are grouped by carrier rather than by field position, which is what keeps this from widening the family: a carrier's range is as wide as the constructor holding the most fields of it, so constructors agreeing on a carrier share slots and only a disagreement costs width.
#[test]
fn a_monomorphic_slot_carries_its_own_type() {
    let source = r#"
        use /std/{Nat, List, Str, Process};

        induct Chain : Type
        | stop()
        | link(Nat, Chain)
        end

        let entry() -> /std/Io({}) =
            let taint = List/len(Process/args()!);
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
            /std/print(Nat/to_str(total(build(taint + 3, Chain/stop()), 0) + total(build(taint + 4, Chain/stop()), 1)));
        entry()
        "#;

    let wat = wat(source);

    let declaration = row_declaration(&wat, "Chain");
    let row = declaration
        .split_whitespace()
        .nth(1)
        .expect("a declaration names its type");
    assert!(
        declaration.contains("(field $0 i8)")
            && declaration.contains(&format!("(field $1 (ref null {row}))"))
            && declaration.contains("(field $2 (ref null any))"),
        "the tag packs into a byte, the tail is the family's own row and the `Nat` a uniform reference: {declaration}"
    );

    let functions = functions(&wat);
    let kernel = function_with(&functions, "999983");
    assert!(
        kernel.body.contains("struct.get_u $row/"),
        "the tag is read out of its packed byte, with nothing to unbox: {}",
        kernel.body
    );
    // Every read of a slot lands in a local of the slot's own carrier, so none of them is followed by an unbox. The casts that remain in this walk are the function's own `anyref` parameter and a hoisted literal, and neither is a field.
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
