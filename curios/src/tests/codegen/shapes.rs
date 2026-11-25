//! The typed-fields census: what the recorded field shapes say about the corpus, and what the uniform representation's box/unbox and cast classes cost statically and dynamically. `documentation/design/compilation/a-field-is-declared-at-the-carrier-its-shape-names.md` is the decision these instrument; the census and the reads' costs are the bench's, and what stays here holds that a recorded shape reaches the program's schema.

use {
    crate::test_support::*,
    curios_anf::{FieldShape, Module},
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
