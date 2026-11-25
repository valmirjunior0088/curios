//! The typed-fields census: what the recorded field shapes say about the corpus, and what the uniform representation's box/unbox and cast classes cost statically and dynamically. `documentation/design/compilation/a-field-is-declared-at-the-carrier-its-shape-names.md` is the decision these instrument, each taken by `cargo xbench measure <name>`.

use {
    super::test_support::*,
    crate::test_support::*,
    curios_anf::{FieldShape, Module},
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_pipeline::compile_with_prelude,
    curios_surface::Entrypoint,
    std::collections::BTreeMap,
    xbench::{Measure, read},
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
/// `cargo xbench measure field_shape_census` takes it: `roster/products`, `roster/families` and `roster/constructors` the schemas `spines` links, `shapes/<class>` how many of its recorded fields are of each shape class, and `<program>/<class>` each static class's sites in a program's emitted WAT. The schema half is taken over one program's module, which holds the schemas of what that program links, so the population is `spines`' own. The static half is per program over the emitted WAT.
///
/// # What it last printed
///
/// Counted at `06ea35a7d`, x86-64 Linux, over the emitted module:
///
/// ```text
/// schema roster: 12 products, 13 families, 47 constructors
/// recorded field shapes (what the program links): {"bytes": 1, "closure": 13, "family": 8, "immediate": 2, "list": 1, "number": 13, "opaque": 14, "product": 4}
/// spines: i31-cast 651, box 956, unbox 1021, rope-cast 114, envr-cast 67, tuple-cast 14, tuple-test 6, tuple-types 3
/// chain: i31-cast 480, box 768, unbox 776, rope-cast 107, envr-cast 55, tuple-cast 4, tuple-test 0, tuple-types 2
/// trees: i31-cast 471, box 752, unbox 766, rope-cast 107, envr-cast 67, tuple-cast 4, tuple-test 0, tuple-types 2
/// churn: i31-cast 458, box 743, unbox 753, rope-cast 107, envr-cast 55, tuple-cast 4, tuple-test 0, tuple-types 2
/// lcg: i31-cast 447, box 728, unbox 736, rope-cast 107, envr-cast 55, tuple-cast 4, tuple-test 0, tuple-types 2
/// monad_io: i31-cast 447, box 723, unbox 735, rope-cast 107, envr-cast 60, tuple-cast 4, tuple-test 0, tuple-types 2
/// parse_digits: i31-cast 444, box 707, unbox 773, rope-cast 107, envr-cast 55, tuple-cast 4, tuple-test 0, tuple-types 2
/// ```
///
/// **42 of the 56 recorded fields `spines` links — 75% — are monomorphic at erasure**, so typed slots have a population; the opaque quarter is dominated by genuinely polymorphic payloads (`Option`'s, `List`'s, the dictionary fields). The i31 box/unbox class is the largest static population in every program, and the rope-base casts, each a Wasmtime `is_subtype` libcall, stand at 107 to 114 sites. The static counts rank *populations*, not costs: a static census cannot price a dynamic class, which is what `boxed_field_read_measurements` below is for.
#[test]
#[ignore = "measurement: taken by cargo xbench measure"]
fn field_shape_census() {
    let module = anf_optm(SPINES);
    let mut classes = BTreeMap::<&str, usize>::new();
    for shape in field_shapes(&module) {
        *classes.entry(shape_class(shape)).or_default() += 1;
    }
    for (schemas, count) in [
        ("products", module.products().len()),
        ("families", module.families().len()),
        ("constructors", module.constructors().len()),
    ] {
        read(
            &format!("roster/{schemas}"),
            Measure::Count,
            None,
            count as u64,
        );
    }
    for (class, fields) in classes {
        read(
            &format!("shapes/{class}"),
            Measure::Count,
            None,
            fields as u64,
        );
    }

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
        for (class, needle) in [
            ("i31-cast", "ref.cast (ref i31)"),
            ("box", "ref.i31"),
            ("unbox", "i31.get"),
            ("rope-cast", "ref.cast (ref $rope/"),
            ("envr-cast", "ref.cast (ref $envr/"),
            ("tuple-cast", "ref.cast (ref $tuple/"),
            ("tuple-test", "ref.test (ref $tuple/"),
            ("tuple-types", "(type $tuple/"),
        ] {
            read(
                &format!("{label}/{class}"),
                Measure::Count,
                None,
                wat.matches(needle).count() as u64,
            );
        }
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
/// `cargo xbench measure boxed_field_read_measurements` takes it: `bare` and `payload`, each at 100 and 300 rounds over the 65 536-element list, the four runs turned round-robin; a fold's nanoseconds an element are the difference of its two medians over the 200 rounds' 13 107 200 visits, and the boxed-field read is the payload's less the bare's.
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
#[ignore = "measurement: taken by cargo xbench measure"]
fn boxed_field_read_measurements() {
    const LOW: u64 = 100;
    const HIGH: u64 = 300;

    let bare = compiled_of(FOLD_BARE);
    let payload = compiled_of(FOLD_PAYLOAD);

    // Outputs are pinned before any figure is read, so a fixture drift fails loudly.
    assert_eq!(run_at(&bare, HIGH).1, b"491113\n", "bare at {HIGH} rounds");
    assert_eq!(
        run_at(&payload, HIGH).1,
        b"161671\n",
        "payload at {HIGH} rounds"
    );

    turn_runs(
        7,
        &[
            ("bare", &bare, LOW),
            ("bare", &bare, HIGH),
            ("payload", &payload, LOW),
            ("payload", &payload, HIGH),
        ],
    );
}
