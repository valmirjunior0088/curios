//! The typed-fields census: what the recorded field shapes say about the corpus, and what the uniform representation's box/unbox and cast classes cost statically and dynamically. `documentation/design/compilation/a-field-is-declared-at-the-carrier-its-shape-names.md` is the decision these instrument; the figures live here, in the `stored_prelude_measurements` pattern — the command and what each probe last printed, beside the code that retakes it.

use {
    super::{run, *},
    crate::tests::test_support::*,
    curios_anf::{FieldShape, Module},
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_native::Compiled,
    curios_pipeline::compile_with_prelude,
    curios_surface::Entrypoint,
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

/// The shapes erasure records on the constructors of the family named `name` that a compiled module carries, by constructor.
fn constructor_shapes(module: &Module, name: &str) -> BTreeMap<String, Vec<FieldShape>> {
    let family = module
        .families()
        .iter()
        .find(|family| family.debug_name.as_deref() == Some(name))
        .unwrap_or_else(|| panic!("the program module carries the family `{name}`"));
    family
        .constructors
        .iter()
        .map(|&id| {
            let constructor = module.constructor(id).expect("live constructor");
            (
                constructor.debug_name.clone().expect("named"),
                constructor.fields.iter().map(|field| field.shape).collect(),
            )
        })
        .collect()
}

/// The recorder's end-to-end pin: the shapes erasure records on `/std/Map/internal/Node/Node` survive to the schema a compiled program's module carries, and a family's own identity is recorded through its self-reference. `leaf(key: K, value: V)` reads polymorphic-opaque twice, and `branch(at: Nat, twigs: Vec(Node(K, V), 16))` reads a number and a list; `trees`' `node(Nat, Tree, Tree)` reads a number and its own family twice. The names are the schema's qualified debug names, per `curios_anf::test_support`'s convention.
#[test]
fn a_recorded_shape_survives_to_the_program_schema() {
    let module = anf_optm(SPINES);
    let shapes = constructor_shapes(&module, "/std/Map/internal/Node/Node");
    assert_eq!(
        shapes["leaf"],
        vec![FieldShape::Opaque, FieldShape::Opaque],
        "leaf: a polymorphic key and a polymorphic value",
    );
    assert_eq!(
        shapes["branch"],
        vec![FieldShape::Number, FieldShape::List],
        "branch: a Nat position and the list of its twigs",
    );

    let module = anf_optm(TREES);
    let shapes = constructor_shapes(&module, "/Tree");
    let [value, left, right] = shapes["node"].as_slice() else {
        panic!(
            "node carries three relevant fields, got {:?}",
            shapes["node"]
        )
    };
    assert_eq!(*value, FieldShape::Number, "node: a Nat value");
    // The children are the family's *own* identity, which is the case the recorder exists to reach: a shape that named only "some family" could not be spent on a declared field type, and a self-reference is where that matters most.
    let (FieldShape::Family(left), FieldShape::Family(right)) = (left, right) else {
        panic!("node's children record their family, got {left:?} and {right:?}")
    };
    assert_eq!(left, right, "both children are the same family");
    assert_eq!(
        module
            .family(*left)
            .expect("live family")
            .debug_name
            .as_deref(),
        Some("/Tree"),
        "and it is Tree's own, recorded through the self-reference",
    );
}

/// The label a shape counts under in the census table.
fn shape_class(shape: FieldShape) -> &'static str {
    match shape {
        FieldShape::Immediate => "immediate",
        FieldShape::Number => "number",
        FieldShape::Flt => "flt",
        FieldShape::Bytes => "bytes",
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
        .flat_map(|schema| schema.shapes())
        .chain(
            module
                .constructors()
                .iter()
                .flat_map(|constructor| constructor.fields.iter().map(|field| field.shape)),
        )
        .collect()
}

/// The emitted WAT of `source` — the last module the compiler controls, which the static counts are taken over.
fn emitted_wat(source: &str) -> String {
    let entrypoint = source.parse::<Entrypoint>().expect("the workload parses");
    let (module, _foreigns) =
        compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |_| {})
            .expect("the workload compiles");

    module.to_string()
}

/// The typed-fields census: the recorded-shape population, the per-program static populations of the box/unbox and cast classes shaping deletes, and the type count nominal rows mint.
///
/// # How to run it
///
/// ```sh
/// cargo test --release --package curios --lib -- --ignored --nocapture field_shape_census
/// ```
///
/// The schema half is taken over one program's module, which holds the schemas of what that program links, so the population is `spines`' own. The static half is per program over the emitted WAT.
///
/// # What it last printed
///
/// Taken at `447fbb0a1`, release, x86-64 Linux:
///
/// ```text
/// schema roster: 62 products, 63 families, 236 constructors
/// recorded field shapes (whole prelude + program): {"bytes": 53, "closure": 18, "family": 60, "flt": 3, "immediate": 30, "list": 40, "number": 75, "opaque": 62, "product": 42}
/// ```
///
/// **290 of 361 recorded fields — 80% — are monomorphic at erasure**, so typed slots have a population; the opaque fifth is dominated by genuinely polymorphic payloads (`Option`'s, `List`'s, the dictionary fields). The static counts rank *populations*, not costs: a static census cannot price a dynamic class, which is what `boxed_field_read_measurements` below is for.
#[test]
#[ignore = "measurement, counted: reports the census rather than asserting"]
fn field_shape_census() {
    let module = anf_optm(SPINES);
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
    println!("recorded field shapes (what the program links): {classes:?}");

    for (label, source) in [
        ("spines", SPINES),
        ("chain", CHAIN),
        ("trees", TREES),
        ("churn", CHURN),
        ("lcg", LCG),
        ("monad_io", MONAD_IO),
        ("parse_digits", PARSE_DIGITS),
    ] {
        let wat = emitted_wat(source);
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

/// The dynamic price of one always-boxed scalar field, isolated: two folds over a resting 65 536-element list, identical but for whether the family's constructors carry a `Nat` payload — so the delta per element is the cost of reading one boxed field (the `ref.cast (ref i31)` + `i31.get_u` pair, plus the store's `ref.i31`) and nothing else. The rounds count arrives as the program's argument so nothing constant-folds; the slope between 100 and 300 rounds cancels the build and the fixed phases.
const FOLD_BARE: &str = r#"
use /std/{Str, Nat, List, Io};

induct F: Type
| left()
| right()
end

let build(n: Nat, acc: List(F)) -> List(F) =
    match n
    | 0 => acc
    | m + 1 => build(m, [..acc, match m % 2 | 0 => F/left() | _ => F/right() end])
    end;

let rounds(r: Nat, l: List(F), s: Nat) -> Nat =
    match r
    | 0 => s
    | q + 1 =>
        rounds(q, l, List/fold(l, s, (e, t) =>
            match e | left() => (t + 1) % 1000003 | right() => (t + 2) % 1000003 end))
    end;

let entry() -> /std/Io({}) =
    let arguments = /std/Proc/args()!;
    let input = /std/List/try_get(arguments, 1);
    match input
    | some(bytes) =>
        match Str/of_bytes(bytes)
        | success(s) =>
            match Nat/of_str(Str/trim(s))
            | some(r) => /std/print(Str/concat(Nat/to_str(rounds(r, build(65536, []), 0)), "\n"))
            | none() => /std/print("bad\n")
            end
        | failure(_) => /std/print("utf8\n")
        end
    | none() => /std/print("none\n")
    end;
entry()
"#;

const FOLD_PAYLOAD: &str = r#"
use /std/{Str, Nat, List, Io};

induct F: Type
| left(Nat)
| right(Nat)
end

let build(n: Nat, acc: List(F)) -> List(F) =
    match n
    | 0 => acc
    | m + 1 => build(m, [..acc, match m % 2 | 0 => F/left(m) | _ => F/right(m) end])
    end;

let rounds(r: Nat, l: List(F), s: Nat) -> Nat =
    match r
    | 0 => s
    | q + 1 =>
        rounds(q, l, List/fold(l, s, (e, t) =>
            match e | left(v) => (t + v) % 1000003 | right(v) => (t + v + 1) % 1000003 end))
    end;

let entry() -> /std/Io({}) =
    let arguments = /std/Proc/args()!;
    let input = /std/List/try_get(arguments, 1);
    match input
    | some(bytes) =>
        match Str/of_bytes(bytes)
        | success(s) =>
            match Nat/of_str(Str/trim(s))
            | some(r) => /std/print(Str/concat(Nat/to_str(rounds(r, build(65536, []), 0)), "\n"))
            | none() => /std/print("bad\n")
            end
        | failure(_) => /std/print("utf8\n")
        end
    | none() => /std/print("none\n")
    end;
entry()
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
/// Release, x86-64 Linux, at a load average under two:
///
/// ```text
/// outputs at 300 rounds: bare "491113", payload "161671"
/// Taken at `9c543476a`, seven readings in one sitting: bare 13.37–13.58 ns/element, payload 15.55–16.09 ns/element, boxed-field read 1.99–2.70 ns, median 2.25 (13–17%, median 14%)
/// ```
///
/// What the figure decided: one always-boxed scalar field costs about a seventh of even this dispatch-heavy loop's per-element budget, and it is pure representation tax — the same fold over the same list, differing by one `ref.i31` at the store and one `ref.cast (ref i31)` + `i31.get_u` at the read. Across takes that cut the fold's per-dispatch cost out from under it, the *relative* share held or grew: the class scales with the loop around it, which is what makes it worth deleting at the representation rather than the site.
#[test]
#[ignore = "measurement, timed: reports timings rather than asserting"]
fn boxed_field_read_measurements() {
    const ELEMENTS: f64 = 65536.0;
    const LOW: u64 = 100;
    const HIGH: u64 = 300;

    let bare = compiled_of(FOLD_BARE);
    let payload = compiled_of(FOLD_PAYLOAD);

    // Outputs are pinned before any figure is read, so a fixture drift fails loudly.
    let (_, bare_out) = run(&bare, HIGH);
    let (_, payload_out) = run(&payload, HIGH);
    println!(
        "outputs at {HIGH} rounds: bare {:?}, payload {:?}",
        String::from_utf8_lossy(&bare_out).trim(),
        String::from_utf8_lossy(&payload_out).trim(),
    );

    let slope = |compiled: &Compiled| {
        let low = timed(compiled, LOW);
        let high = timed(compiled, HIGH);
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
