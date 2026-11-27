//! The census of the field shapes a compile settles on across the corpus, and what reading a boxed field costs a run against the same program's unboxed one.

use {
    super::*,
    crate::{Measure, read},
    curios::test_support::*,
    curios_anf::{FieldShape, Module},
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_pipeline::compile_with_prelude,
    curios_surface::Entrypoint,
    std::collections::BTreeMap,
};

const SPINES: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/spines/spines.crs"
));

const TREES: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/trees/trees.crs"
));

const CHAIN: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/chain/chain.crs"
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
/// # Its records
///
/// `cargo xbench report field_shape_census` reads them: each figure's readings by group, and what each sitting was taken under.
pub fn field_shape_census() {
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

let build(n: Nat, acc: List(F)) -> List(F) =>
    match n
    | 0 => acc
    | m + 1 => build(m, [..acc, match m % 2 | 0 => F/left() | _ => F/right() end])
    end;

let rounds(r: Nat, l: List(F), s: Nat) -> Nat =>
    match r
    | 0 => s
    | q + 1 =>
        rounds(q, l, List/fold(l, s, (e, t) =>
            match e | left() => (t + 1) % 1000003 | right() => (t + 2) % 1000003 end))
    end;

let entry() -> /std/Io({}) =>
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

let build(n: Nat, acc: List(F)) -> List(F) =>
    match n
    | 0 => acc
    | m + 1 => build(m, [..acc, match m % 2 | 0 => F/left(m) | _ => F/right(m) end])
    end;

let rounds(r: Nat, l: List(F), s: Nat) -> Nat =>
    match r
    | 0 => s
    | q + 1 =>
        rounds(q, l, List/fold(l, s, (e, t) =>
            match e | left(v) => (t + v) % 1000003 | right(v) => (t + v + 1) % 1000003 end))
    end;

let entry() -> /std/Io({}) =>
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
/// # Its records
///
/// `cargo xbench report boxed_field_read_measurements` reads them: each figure's readings by group, and what each sitting was taken under.
pub fn boxed_field_read_measurements() {
    const LOW: u64 = 100;
    const HIGH: u64 = 300;

    let bare = compiled_of(FOLD_BARE);
    let payload = compiled_of(FOLD_PAYLOAD);

    // Outputs are pinned before any figure is read, so a fixture drift fails loudly.
    assert_eq!(run_at(&bare, HIGH), b"491113\n", "bare at {HIGH} rounds");
    assert_eq!(
        run_at(&payload, HIGH),
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
