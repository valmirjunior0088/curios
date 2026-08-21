//! The typed-fields census: what the recorded field shapes say about the corpus, and what the uniform representation's box/unbox and cast classes cost statically and dynamically. `documentation/roadmap/typed-heap-fields-spec.md` is the campaign this instruments; the figures live here, in the `stored_prelude_measurements` pattern — the command, the date, and what each probe last printed, beside the code that retakes it.

use {
    super::map_wall::{cwasm_of, run, timed},
    crate::{tests::ersd_optm, to_cwasm_dumped},
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
    to_cwasm_dumped(&module, |stage| {
        if let Stage::WasmOptm(text) = stage {
            printed = text.to_string();
        }
    })
    .expect("the workload optimizes");
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
/// Taken 2026-08-20, release, x86-64 Linux, at the recorder's landing:
///
/// ```text
/// schema roster: 28 products, 30 families, 92 constructors
/// recorded field shapes (whole prelude + program): {"bits": 4, "bytes": 19, "closure": 11, "family": 15, "flt": 2, "immediate": 34, "list": 13, "opaque": 33, "product": 18}
/// spines:       i31-cast 109, box 205, unbox 186, rope-cast 72, envr-cast 17, tuple-cast 86, tuple-test 58, tuple-types 5
/// chain:        i31-cast  83, box 176, unbox 152, rope-cast 51, envr-cast 17, tuple-cast 39, tuple-test 21, tuple-types 4
/// trees:        i31-cast  82, box 168, unbox 149, rope-cast 51, envr-cast 17, tuple-cast 50, tuple-test 34, tuple-types 5
/// churn:        i31-cast  77, box 165, unbox 147, rope-cast 51, envr-cast 17, tuple-cast 27, tuple-test 15, tuple-types 4
/// lcg:          i31-cast  76, box 163, unbox 143, rope-cast 51, envr-cast 17, tuple-cast 27, tuple-test 15, tuple-types 4
/// monad_io:     i31-cast  80, box 164, unbox 145, rope-cast 51, envr-cast 18, tuple-cast 27, tuple-test 15, tuple-types 4
/// parse_digits: i31-cast  80, box 162, unbox 146, rope-cast 51, envr-cast 17, tuple-cast 32, tuple-test 18, tuple-types 4
/// ```
///
/// What the figures decided. **116 of 149 recorded fields — 78% — are monomorphic at erasure**, so typed slots have a population; the opaque third is dominated by genuinely polymorphic payloads (`Option`'s, `List`'s, the dictionary fields). The i31 box/unbox class is the largest static population in every program, the rope-base casts (each a Wasmtime `is_subtype` libcall) sit at 51–72 sites, and family keying replaces the 4–5 arity-keyed tuple types with the roster's 58 nominal types — a growth Binaryen's closed-world passes are built to consume, not a cost. The static counts rank *populations*, not costs — the cast step's own history says a static census cannot price a dynamic class, which is what `boxed_field_read_measurements` below is for.
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

rec build(n: Nat, acc: List(F)) -> List(F) =
    match n: (_) => List(F)
    | 0 => acc
    | m + 1; ih => build(m, [..acc, match m % 2 | 0 => F/left() | _ => F/right() end])
    end;

rec rounds(r: Nat, l: List(F), s: Nat) -> Nat =
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

rec build(n: Nat, acc: List(F)) -> List(F) =
    match n: (_) => List(F)
    | 0 => acc
    | m + 1; ih => build(m, [..acc, match m % 2 | 0 => F/left(m) | _ => F/right(m) end])
    end;

rec rounds(r: Nat, l: List(F), s: Nat) -> Nat =
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
/// Taken 2026-08-20, release, x86-64 Linux, right after the per-arity-typed-table landing:
///
/// ```text
/// outputs at 300 rounds: bare "491113", payload "161671"
/// bare 19.53 ns/element, payload 23.66 ns/element, boxed-field read 4.13 ns (17%)
/// ```
///
/// What the figure decided: one always-boxed scalar field costs about a sixth of even this dispatch-heavy loop's per-element budget, and it is pure representation tax — the same fold over the same list, differing by one `ref.i31` at the store and one `ref.cast (ref i31)` + `i31.get_u` at the read. A native whole-process take of the same pair before the typed-table landing read 7.7 ns (18%): the absolute halved because the typed tables cut the fold's per-dispatch cost out from under it, while the *relative* share held — the class scales with the loop around it, which is exactly what makes it worth deleting at the representation rather than the site.
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

/// The slot-layout probe: how wide a family's heap type becomes under each way of assigning its constructors' fields to slots, and how many of those slots end up carrying a type rather than `anyref`.
///
/// Three policies, over every family that takes the tagged encoding:
///
/// - **positional** — field `i` of every constructor on slot `i + 1`, which is what the family keying landed with: the family is exactly as wide as its widest constructor and a slot is typed only where every constructor writing it agrees.
/// - **classed** — one slot range per carrier, sized to the widest constructor's count of that carrier. Every slot is typed by construction, and width grows only where constructors disagree.
/// - **disjoint** — every constructor its own slot range, which types no more than classed does and pays for every field twice over.
///
/// The carriers are the ones the door actually assigns (`into_cont.rs`'s `slot_of`), so packed, closure and family shapes count as untyped here.
///
/// # How to run it
///
/// ```sh
/// cargo test --release --package curios --lib -- --ignored --nocapture slot_layout_probe
/// ```
///
/// # What it last printed
///
/// Taken 2026-08-20, release, x86-64 Linux, over 26 tagged families:
///
/// ```text
/// 26 tagged families
/// positional: 60 slots, 11 typed
/// classed:    70 slots, 22 typed
/// disjoint:   88 slots, 26 typed
/// families the classed layout widens:
///   /std/Async/Future/State      positional 0/2 typed, classed 1/3 typed, disjoint 1/3 typed
///   /std/Toml/Toml               positional 0/2 typed, classed 7/9 typed, disjoint 9/11 typed
///   /std/Async/Pause             positional 0/3 typed, classed 2/5 typed, disjoint 2/10 typed
/// ```
///
/// What the figures decided the layout. Classed doubles what positional can type for ten slots more across the whole roster, and — the reading that settled it — **only three families widen at all**, none of them on a hot allocation path: every family the corpus allocates in a loop keeps the width it had, and `/std/Map/Node` in particular stays four slots while its `crit` becomes a register. Disjoint, the specification's stated starting point, types four more slots than classed for eighteen more slots of width, so it was declined here rather than measured.
#[test]
#[ignore = "measurement: reports the layout table rather than asserting"]
fn slot_layout_probe() {
    let module = ersd_optm(SPINES);

    // The carrier a shape occupies, mirroring the door's own rule, or `None` where the slot stays the uniform reference.
    let class = |shape: FieldShape| -> Option<String> {
        match shape {
            FieldShape::Immediate(Sign::Unsigned) => Some("nat".into()),
            FieldShape::Immediate(Sign::Signed) => Some("int".into()),
            FieldShape::Flt => Some("flt".into()),
            FieldShape::List => Some("list".into()),
            FieldShape::Product(schema) => Some(format!("product/{schema}")),
            FieldShape::Packed(_)
            | FieldShape::Closure(_)
            | FieldShape::Family(_)
            | FieldShape::Opaque => None,
        }
    };

    let (mut positional_width, mut classed_width, mut disjoint_width) = (0, 0, 0);
    let (mut positional_typed, mut classed_typed, mut disjoint_typed) = (0, 0, 0);
    let mut families = 0;
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

        // Collapsed and immediate encodings never allocate a family struct, so they have no slots to lay out.
        if rows.len() < 2 {
            continue;
        }
        let bare = rows
            .iter()
            .filter(|row| matches!(row.as_slice(), [shape] if matches!(shape, FieldShape::Immediate(_))))
            .count();
        if bare == 1 {
            continue;
        }
        families += 1;

        let widest = rows.iter().map(Vec::len).max().unwrap_or(0);
        let positional: usize = (0..widest)
            .filter(|&index| {
                let written: Vec<_> = rows.iter().filter_map(|row| row.get(index)).collect();
                let first = written.first().map(|&&shape| class(shape));
                matches!(&first, Some(Some(_)))
                    && written
                        .iter()
                        .all(|&&shape| Some(class(shape)) == first.clone())
            })
            .count();

        let mut classed = BTreeMap::<String, usize>::new();
        let mut opaque_slots = 0;
        for row in &rows {
            let mut counts = BTreeMap::<String, usize>::new();
            let mut opaque = 0;
            for &shape in row {
                match class(shape) {
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
        let classed_slots: usize = classed.values().sum();
        let disjoint: usize = rows.iter().map(Vec::len).sum();
        let disjoint_typed_here = rows
            .iter()
            .flatten()
            .filter(|&&shape| class(shape).is_some())
            .count();

        positional_width += 1 + widest;
        classed_width += 1 + classed_slots + opaque_slots;
        disjoint_width += 1 + disjoint;
        positional_typed += positional;
        classed_typed += classed_slots;
        disjoint_typed += disjoint_typed_here;

        if 1 + classed_slots + opaque_slots > 1 + widest {
            report.push(format!(
                "  {:<28} positional {}/{} typed, classed {}/{} typed, disjoint {}/{} typed",
                family.debug_name.as_deref().unwrap_or("?"),
                positional,
                1 + widest,
                classed_slots,
                1 + classed_slots + opaque_slots,
                disjoint_typed_here,
                1 + disjoint,
            ));
        }
    }

    println!("{families} tagged families");
    println!(
        "positional: {positional_width} slots, {positional_typed} typed\nclassed:    {classed_width} slots, {classed_typed} typed\ndisjoint:   {disjoint_width} slots, {disjoint_typed} typed"
    );
    println!("families the classed layout widens:");
    for line in report {
        println!("{line}");
    }
}
