//! The typed-fields census: what the recorded field shapes say about the corpus, and what the uniform representation's box/unbox and cast classes cost statically and dynamically. `documentation/design/toolchain/a-field-is-declared-at-the-carrier-its-shape-names.md` is the decision these instrument; the figures live here, in the `stored_prelude_measurements` pattern — the command, the date, and what each probe last printed, beside the code that retakes it.

use {
    super::map_wall::{cwasm_of, run, timed},
    crate::{tests::ersd_optm, wasm_optm},
    curios_ersd::{FieldShape, Module, Sign},
    curios_pipeline::{DEFAULT_STEP_BUDGET, Stage, compile_with_prelude},
    curios_text::{Entrypoint, RootSource},
    curios_utilities::Grain,
    std::collections::BTreeMap,
};

const SPINES: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/spines/spines.crs"
));
const CHAIN: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/chain/chain.crs"
));
const TREES: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/trees/trees.crs"
));
const CHURN: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/churn/churn.crs"
));
const LCG: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/lcg/lcg.crs"
));
const MONAD_IO: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/monad_io.crs"
));
const PARSE_DIGITS: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/parse_digits.crs"
));

/// The recorder's end-to-end pin: the shapes erasure records on `/std/Map/Node` survive to the schema a compiled program's module carries. `leaf(key: Bytes, value: V)` reads packed-at-byte-grain and polymorphic-opaque; `fork(crit: Nat, zero: Node(V), one: Node(V))` reads immediate and family. The names are the schema's qualified debug names, per `curios_ersd::test_support`'s convention.
#[test]
fn a_recorded_shape_survives_to_the_program_schema() {
    let module = ersd_optm(SPINES);
    let family = module
        .families()
        .iter()
        .find(|family| family.debug_name.as_deref() == Some("/std/Map/Node"))
        .expect("the program module carries the prelude's Node family");

    let shapes: BTreeMap<&str, Vec<FieldShape>> = family
        .constructors
        .iter()
        .map(|&id| {
            let constructor = module.constructor(id).expect("live constructor");
            (
                constructor.debug_name.as_deref().expect("named"),
                constructor.fields.iter().map(|field| field.shape).collect(),
            )
        })
        .collect();

    assert_eq!(
        shapes["leaf"],
        vec![FieldShape::Packed(Grain::X), FieldShape::Opaque],
        "leaf: a Bytes key and a polymorphic value",
    );
    let [crit, zero, one] = shapes["fork"].as_slice() else {
        panic!(
            "fork carries three relevant fields, got {:?}",
            shapes["fork"]
        )
    };
    assert_eq!(
        *crit,
        FieldShape::Immediate(Sign::Unsigned),
        "fork: an unsigned Nat crit",
    );
    // The children are the family's *own* identity, which is the case the recorder exists to reach: a shape that named only "some family" could not be spent on a declared field type, and a self-reference is where that matters most.
    let (FieldShape::Family(zero), FieldShape::Family(one)) = (zero, one) else {
        panic!("fork's children record their family, got {zero:?} and {one:?}")
    };
    assert_eq!(zero, one, "both children are the same family");
    assert_eq!(
        module
            .family(*zero)
            .expect("live family")
            .debug_name
            .as_deref(),
        Some("/std/Map/Node"),
        "and it is Node's own, recorded through the self-reference",
    );
}

/// The label a shape counts under in the census table.
fn shape_class(shape: FieldShape) -> &'static str {
    match shape {
        FieldShape::Immediate(Sign::Unsigned) => "immediate",
        FieldShape::Immediate(Sign::Signed) => "immediate/signed",
        FieldShape::Flt => "flt",
        FieldShape::Packed(Grain::X) => "bytes",
        FieldShape::Packed(Grain::B) => "bits",
        FieldShape::List => "list",
        FieldShape::Closure(_) => "closure",
        FieldShape::Product(_) => "product",
        FieldShape::Family(_) => "family",
        FieldShape::Opaque => "opaque",
    }
}

/// Every field row in the module — products and constructors alike — flattened for counting.
fn field_shapes(module: &Module) -> Vec<FieldShape> {
    module
        .products()
        .iter()
        .flat_map(|schema| schema.fields.iter().map(|field| field.shape))
        .chain(
            module
                .constructors()
                .iter()
                .flat_map(|constructor| constructor.fields.iter().map(|field| field.shape)),
        )
        .collect()
}

/// The Binaryen-optimized WAT of `source` — the module a user actually runs, which is what the static cast counts must be taken over (the raw module's counts include what Binaryen deletes).
fn optimized_wat(source: &str) -> String {
    let entrypoint = source.parse::<Entrypoint>().expect("the workload parses");
    let (module, _foreigns) = compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |_| {},
    )
    .expect("the workload compiles");

    let mut printed = String::new();
    wasm_optm(&module, |stage| {
        if let Stage::WasmOptm(text) = stage {
            printed = text.to_string();
        }
    });
    printed
}

/// The census the typed-fields specification gates its third mechanism on: the recorded-shape population, the per-program static populations of the box/unbox and cast classes shaping would delete, and the type-count growth family keying would mint.
///
/// # How to run it
///
/// ```sh
/// cargo test --release --package curios --lib -- --ignored --nocapture field_shape_census
/// ```
///
/// The schema half is taken over one program's module because a unit's erased arena is cumulative — any entrypoint's module carries the whole prelude roster, so the population is /std-wide however small the program. The static half is per program over the *optimized* WAT.
///
/// # What it last printed
///
/// Taken 2026-09-12, release, x86-64 Linux, over a prelude twice the size of the one the recorder landed on (2026-08-20: 28 products, 30 families, 149 recorded fields):
///
/// ```text
/// schema roster: 55 products, 59 families, 213 constructors
/// recorded field shapes (whole prelude + program): {"bits": 4, "bytes": 50, "closure": 16, "family": 54, "flt": 2, "immediate": 91, "immediate/signed": 2, "list": 36, "opaque": 71, "product": 35}
/// spines:       i31-cast 97, box 223, unbox 258, rope-cast 121, envr-cast 32, tuple-cast 10, tuple-test 6, tuple-types 3
/// chain:        i31-cast 71, box 205, unbox 220, rope-cast 100, envr-cast 32, tuple-cast 10, tuple-test 6, tuple-types 3
/// trees:        i31-cast 74, box 207, unbox 223, rope-cast 100, envr-cast 32, tuple-cast 10, tuple-test 6, tuple-types 3
/// churn:        i31-cast 71, box 205, unbox 221, rope-cast 100, envr-cast 32, tuple-cast 10, tuple-test 6, tuple-types 3
/// lcg:          i31-cast 70, box 203, unbox 217, rope-cast 100, envr-cast 32, tuple-cast 10, tuple-test 6, tuple-types 3
/// monad_io:     i31-cast 75, box 204, unbox 219, rope-cast 100, envr-cast 33, tuple-cast 10, tuple-test 6, tuple-types 3
/// parse_digits: i31-cast 74, box 203, unbox 219, rope-cast 100, envr-cast 30, tuple-cast 10, tuple-test 6, tuple-types 3
/// ```
///
/// What the figures decided, and the larger prelude only sharpened it. **290 of 361 recorded fields — 80% — are monomorphic at erasure** (116 of 149, 78%, at the recorder's landing), so typed slots have a population; the opaque fifth is dominated by genuinely polymorphic payloads (`Option`'s, `List`'s, the dictionary fields). The i31 box/unbox class is the largest static population in every program, the rope-base casts (each a Wasmtime `is_subtype` libcall) sit at 100–121 sites, and family keying replaced the arity-keyed tuple types with the roster's nominal types — 114 of them now, against 3 tuple types per program — a growth Binaryen's closed-world passes are built to consume, not a cost. The static counts rank *populations*, not costs — the cast step's own history says a static census cannot price a dynamic class, which is what `boxed_field_read_measurements` below is for.
#[test]
#[ignore = "measurement: reports the census rather than asserting"]
fn field_shape_census() {
    let module = ersd_optm(SPINES);
    let mut classes = BTreeMap::<&str, usize>::new();
    for shape in field_shapes(&module) {
        *classes.entry(shape_class(shape)).or_default() += 1;
    }
    println!(
        "schema roster: {} products, {} families, {} constructors",
        module.products().len(),
        module.families().len(),
        module.constructors().len(),
    );
    println!("recorded field shapes (whole prelude + program): {classes:?}");

    for (label, source) in [
        ("spines", SPINES),
        ("chain", CHAIN),
        ("trees", TREES),
        ("churn", CHURN),
        ("lcg", LCG),
        ("monad_io", MONAD_IO),
        ("parse_digits", PARSE_DIGITS),
    ] {
        let wat = optimized_wat(source);
        let count = |needle: &str| wat.matches(needle).count();
        println!(
            "{label}: i31-cast {}, box {}, unbox {}, rope-cast {}, envr-cast {}, tuple-cast {}, tuple-test {}, tuple-types {}",
            count("ref.cast (ref i31)"),
            count("ref.i31"),
            count("i31.get"),
            count("ref.cast (ref $rope/"),
            count("ref.cast (ref $envr/"),
            count("ref.cast (ref $tuple/"),
            count("ref.test (ref $tuple/"),
            count("(type $tuple/"),
        );
    }
}

/// The dynamic price of one always-boxed scalar field, isolated: two folds over a resting 65 536-element list, identical but for whether the family's constructors carry a `Nat` payload — so the delta per element is the cost of reading one boxed field (the `ref.cast (ref i31)` + `i31.get_u` pair, plus the store's `ref.i31`) and nothing else. The rounds count arrives on stdin so nothing constant-folds; the slope between 100 and 300 rounds cancels the build and the fixed phases.
const FOLD_BARE: &str = r#"
use /std/{Str, Nat, List, Io};

induct F: Type
| left()
| right()
end

let build(n: Nat, acc: List(F)) -> List(F) =
    match n: (_) => List(F)
    | 0 => acc
    | m + 1; ih => build(m, [..acc, match m % 2 | 0 => F/left() | _ => F/right() end])
    end;

let rounds(r: Nat, l: List(F), s: Nat) -> Nat =
    match r: (_) => Nat
    | 0 => s
    | q + 1; ih =>
        rounds(q, l, List/fold(l, s, (e, t) =>
            match e | left() => (t + 1) % 1000003 | right() => (t + 2) % 1000003 end))
    end;

let input = /std/read()!;
match input: (_) => Io({})
| some(bytes) =>
    match Str/of_bytes(bytes): (_) => Io({})
    | some(s) =>
        match Nat/of_str(Str/trim(s)): (_) => Io({})
        | some(r) => /std/print(Str/concat(Nat/to_str(rounds(r, build(65536, []), 0)), "\n"))
        | none() => /std/print("bad\n")
        end
    | none() => /std/print("utf8\n")
    end
| none() => /std/print("none\n")
end
"#;

const FOLD_PAYLOAD: &str = r#"
use /std/{Str, Nat, List, Io};

induct F: Type
| left(Nat)
| right(Nat)
end

let build(n: Nat, acc: List(F)) -> List(F) =
    match n: (_) => List(F)
    | 0 => acc
    | m + 1; ih => build(m, [..acc, match m % 2 | 0 => F/left(m) | _ => F/right(m) end])
    end;

let rounds(r: Nat, l: List(F), s: Nat) -> Nat =
    match r: (_) => Nat
    | 0 => s
    | q + 1; ih =>
        rounds(q, l, List/fold(l, s, (e, t) =>
            match e | left(v) => (t + v) % 1000003 | right(v) => (t + v + 1) % 1000003 end))
    end;

let input = /std/read()!;
match input: (_) => Io({})
| some(bytes) =>
    match Str/of_bytes(bytes): (_) => Io({})
    | some(s) =>
        match Nat/of_str(Str/trim(s)): (_) => Io({})
        | some(r) => /std/print(Str/concat(Nat/to_str(rounds(r, build(65536, []), 0)), "\n"))
        | none() => /std/print("bad\n")
        end
    | none() => /std/print("utf8\n")
    end
| none() => /std/print("none\n")
end
"#;

/// The census's dynamic probe: nanoseconds per element-visit for the bare and payload-carrying folds, and the delta — the price of one boxed scalar field read.
///
/// # How to run it
///
/// ```sh
/// cargo test --release --package curios --lib -- --ignored --nocapture boxed_field_read_measurements
/// ```
///
/// # What it last printed
///
/// Taken 2026-09-12, release, x86-64 Linux:
///
/// ```text
/// outputs at 300 rounds: bare "491113", payload "161671"
/// bare 13.60 ns/element, payload 17.18 ns/element, boxed-field read 3.58 ns (21%)
/// ```
///
/// What the figure decided: one always-boxed scalar field costs about a fifth of even this dispatch-heavy loop's per-element budget, and it is pure representation tax — the same fold over the same list, differing by one `ref.i31` at the store and one `ref.cast (ref i31)` + `i31.get_u` at the read. The history of the same pair is the argument: a native whole-process take before the typed-table landing read 7.7 ns (18%), the take right after it 4.13 ns (17%), and today's 3.58 ns (21%) — the absolute keeps falling as the fold's per-dispatch cost is cut out from under it, while the *relative* share holds or grows — the class scales with the loop around it, which is exactly what makes it worth deleting at the representation rather than the site.
#[test]
#[ignore = "measurement: reports timings rather than asserting"]
fn boxed_field_read_measurements() {
    const ELEMENTS: f64 = 65536.0;
    const LOW: u64 = 100;
    const HIGH: u64 = 300;

    let bare = cwasm_of(FOLD_BARE);
    let payload = cwasm_of(FOLD_PAYLOAD);

    // Outputs are pinned before any figure is read, so a fixture drift fails loudly.
    let (_, bare_out) = run(&bare, HIGH);
    let (_, payload_out) = run(&payload, HIGH);
    println!(
        "outputs at {HIGH} rounds: bare {:?}, payload {:?}",
        String::from_utf8_lossy(&bare_out).trim(),
        String::from_utf8_lossy(&payload_out).trim(),
    );

    let slope = |cwasm: &[u8]| {
        let low = timed(cwasm, LOW);
        let high = timed(cwasm, HIGH);
        (high - low) * 1e6 / ((HIGH - LOW) as f64 * ELEMENTS)
    };

    let bare_ns = slope(&bare);
    let payload_ns = slope(&payload);
    println!(
        "bare {bare_ns:.2} ns/element, payload {payload_ns:.2} ns/element, boxed-field read {:.2} ns ({:.0}%)",
        payload_ns - bare_ns,
        (payload_ns - bare_ns) / payload_ns * 100.0,
    );
}

/// The family-slot probe: what typing a *tagged* family's reference fields would cost, family by family.
///
/// Every other recorded shape is typed by the door already, and each was strictly additive — the same width, fewer instructions, and for a list or a closure a deleted `is_subtype` libcall. A tagged family's reference fields are the exception on both counts. Slots are grouped by carrier, so a family-typed slot cannot share the uniform range; a family whose constructors disagree therefore *widens* to gain one. And what the widening buys is the cheap kind of cast — an exact compare against a final type — not a libcall.
///
/// So the question is not whether to type them but *where*, and the criterion is exact rather than a heuristic: a family widens or it does not. This reports the split, so a rule admitting only the free ones can be written against a number instead of an intuition.
///
/// # How to run it
///
/// ```sh
/// cargo test --release --package curios --lib -- --ignored --nocapture family_slot_probe
/// ```
///
/// # What it last printed
///
/// Taken 2026-09-12, release, x86-64 Linux (the 2026-08-20 take, over a prelude half this size, split 5 free to 3 paid, with `/std/Map/Node` already the widest row; `/std/Vec` has since become a product and left the table):
///
/// ```text
/// families holding a family-typed field: 16
///   free: 9 families, 8 slots typed at no width cost
///   paid: 7 families, 11 slots typed for 8 slots of width
///   /std/Io/Chunk/Chunk          2 slots -> 2 slots, 0 typed -> 0 typed   FREE
///   /std/Map/Node                4 slots -> 6 slots, 1 typed -> 3 typed   PAID
///   /std/Async/Step              3 slots -> 3 slots, 0 typed -> 1 typed   FREE
///   /std/Toml/Error/Error        4 slots -> 5 slots, 2 typed -> 3 typed   PAID
///   /std/Cli/Kind                3 slots -> 3 slots, 1 typed -> 2 typed   FREE
///   /std/Cli/Values              5 slots -> 5 slots, 2 typed -> 3 typed   FREE
///   /std/Cli/Cli                 6 slots -> 6 slots, 2 typed -> 3 typed   FREE
///   /std/Cli/Outcome             2 slots -> 3 slots, 0 typed -> 2 typed   PAID
///   /std/Cli/Cluster             4 slots -> 5 slots, 2 typed -> 3 typed   PAID
///   /std/Cli/Chosen              3 slots -> 4 slots, 1 typed -> 3 typed   PAID
///   /std/Fmt/Fmt                 3 slots -> 3 slots, 0 typed -> 1 typed   FREE
///   /std/Test/Test               2 slots -> 3 slots, 0 typed -> 1 typed   PAID
///   /std/Tui/Layout/Sizes        8 slots -> 9 slots, 5 typed -> 7 typed   PAID
///   /std/Tui/input/Step          3 slots -> 3 slots, 1 typed -> 2 typed   FREE
///   /std/Toml/build/Act          2 slots -> 2 slots, 0 typed -> 1 typed   FREE
///   /std/Toml/decode/Stmt        3 slots -> 3 slots, 1 typed -> 2 typed   FREE
/// ```
///
/// **What the figures decided, and it is the opposite of what the specification predicted.** The campaign was written around making a fork's children `(ref null $node)`, and `/std/Map/Node` is the *worst* row in this table: four slots to six, a half again as much live memory on the corpus's hottest allocated structure, to replace two casts that are already exact compares against a final type. Set against this campaign's own `trees` finding — that live bytes convert to time under an all-live collector — that is a trade to decline, and the door declines it.
///
/// What the door does instead is admit the free column, by an exact criterion rather than a judgement: type a family's reference slots iff the row's width is unchanged. Eight slots qualify here, on the command-line, formatting, scheduler, layout and TOML families rather than on anything the corpus allocates in bulk, so **the corpus gain is nil** and this rule is not justified by a measurement — it is justified by generalizing to code the corpus does not contain, at a runtime cost that is zero by construction. A product needs no such test: one writer can never widen a row, so its reference fields always type.
#[test]
#[ignore = "measurement: reports the split rather than asserting"]
fn family_slot_probe() {
    let module = ersd_optm(SPINES);

    // Whether a value of this family is *always* the row struct. An immediate family's bare constructor rides the i31 instead, so no heap type names its population and a slot can never be declared at it.
    let always_a_row = |family: curios_ersd::FamilyId| -> bool {
        let rows: Vec<&curios_ersd::Constructor> = module
            .family(family)
            .expect("live family")
            .constructors
            .iter()
            .map(|&id| module.constructor(id).expect("live constructor"))
            .collect();
        let bare = rows
            .iter()
            .filter(|constructor| {
                matches!(constructor.fields.as_slice(), [field] if matches!(field.shape, FieldShape::Immediate(_)))
            })
            .count();
        rows.len() < 2 || bare != 1
    };

    // The carrier a shape occupies, mirroring the door's own rule. `family_typed` is the question this probe exists to answer: a family-typed reference field is the one shape whose typing can cost width.
    let class = |shape: FieldShape, family_typed: bool| -> Option<String> {
        match shape {
            FieldShape::Immediate(Sign::Unsigned) => Some("nat".into()),
            FieldShape::Immediate(Sign::Signed) => Some("int".into()),
            FieldShape::Flt => Some("flt".into()),
            FieldShape::List => Some("list".into()),
            FieldShape::Closure(arity) => Some(format!("closure/{arity}")),
            FieldShape::Product(schema) => Some(format!("product/{schema}")),
            FieldShape::Family(family) if family_typed && always_a_row(family) => {
                Some(format!("family/{family}"))
            }
            FieldShape::Packed(_) | FieldShape::Family(_) | FieldShape::Opaque => None,
        }
    };

    // The classed layout's width and typed-slot count for one family's constructor rows.
    let layout = |rows: &[Vec<FieldShape>], family_typed: bool| -> (usize, usize) {
        let mut classed = BTreeMap::<String, usize>::new();
        let mut opaque_slots = 0;
        for row in rows {
            let mut counts = BTreeMap::<String, usize>::new();
            let mut opaque = 0;
            for &shape in row {
                match class(shape, family_typed) {
                    Some(name) => *counts.entry(name).or_default() += 1,
                    None => opaque += 1,
                }
            }
            for (name, count) in counts {
                let slot = classed.entry(name).or_default();
                *slot = (*slot).max(count);
            }
            opaque_slots = opaque_slots.max(opaque);
        }
        let typed: usize = classed.values().sum();
        (1 + typed + opaque_slots, typed)
    };

    let (mut free_families, mut paid_families) = (0, 0);
    let (mut free_slots, mut paid_slots, mut paid_width) = (0, 0, 0);
    let mut report = Vec::new();

    for family in module.families() {
        let rows: Vec<Vec<FieldShape>> = family
            .constructors
            .iter()
            .map(|&id| {
                module
                    .constructor(id)
                    .expect("live constructor")
                    .fields
                    .iter()
                    .map(|field| field.shape)
                    .collect()
            })
            .collect();

        // Collapsed and immediate encodings do not lay a tag out, so they are not what this asks about.
        if rows.len() < 2 {
            continue;
        }
        let bare = rows
            .iter()
            .filter(|row| {
                matches!(row.as_slice(), [shape] if matches!(shape, FieldShape::Immediate(_)))
            })
            .count();
        if bare == 1 {
            continue;
        }
        if !rows
            .iter()
            .flatten()
            .any(|shape| matches!(shape, FieldShape::Family(_)))
        {
            continue;
        }

        let (uniform_width, uniform_typed) = layout(&rows, false);
        let (typed_width, typed_typed) = layout(&rows, true);
        let gain = typed_typed - uniform_typed;
        match typed_width == uniform_width {
            true => {
                free_families += 1;
                free_slots += gain;
            }
            false => {
                paid_families += 1;
                paid_slots += gain;
                paid_width += typed_width - uniform_width;
            }
        }
        report.push(format!(
            "  {:<28} {} slots -> {} slots, {} typed -> {} typed{}",
            family.debug_name.as_deref().unwrap_or("?"),
            uniform_width,
            typed_width,
            uniform_typed,
            typed_typed,
            match typed_width == uniform_width {
                true => "   FREE",
                false => "   PAID",
            }
        ));
    }

    println!(
        "families holding a family-typed field: {}",
        free_families + paid_families
    );
    println!("  free: {free_families} families, {free_slots} slots typed at no width cost");
    println!(
        "  paid: {paid_families} families, {paid_slots} slots typed for {paid_width} slots of width"
    );
    for line in report {
        println!("{line}");
    }
}
