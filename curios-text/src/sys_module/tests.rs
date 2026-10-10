//! What a `/sys` description lowers to.
//!
//! The subject is the description layer, not the roster's contents. What `/sys` declares is checked where it already is — the prelude build elaborates and certifies every one of these declarations, and `into_core/sys_tests.rs` pins the root's reachability and `/sys/Io`'s roster — so a second statement of the roster here would be a copy to keep in step rather than a check. What has no other check is the shape a `Decl` lowers to, how a node's row in `Intrinsic::signature` is read into a declaration's types, and how a parameter's plicity is read.

use {
    super::*,
    crate::{LetSignature, Name, Pattern, SYNTAX, Subterm},
    curios_utilities::Plicity,
};

// A description with nothing to bind is a value, and `LetSignature::Func` with an empty telescope would lower to a binderless lambda instead.
#[test]
fn an_empty_telescope_lowers_to_a_constant_rather_than_a_nullary_function() {
    let signature = pub_let("stdin", nat(), nat_lit(0)).into_let().signature;

    assert!(
        matches!(signature, LetSignature::Name { .. }),
        "an empty telescope lowered to {signature:?}"
    );
}

#[test]
fn a_telescope_lowers_to_the_function_sugar_carrying_every_plicity_mark() {
    let declaration = pub_fn_marked(
        "get",
        vec![
            (Plicity::Implicit, "T", type_()),
            (Plicity::Explicit, "a", nat()),
        ],
        nat(),
        name("a"),
    );

    let LetSignature::Func { params, .. } = declaration.into_let().signature else {
        panic!("a telescope lowered to a constant");
    };

    let marks = params.iter().map(|param| param.plicity).collect::<Vec<_>>();
    assert_eq!(marks, [Plicity::Implicit, Plicity::Explicit]);
    assert!(matches!(&params[0].binder, Some(Pattern::Binder(binder)) if binder.as_str() == "T"));
}

// What `documented` attaches, read back off the declaration it built.
#[test]
fn a_gloss_lands_on_the_declaration_it_is_written_above() {
    let declaration = documented(
        &["Their sum.", "", "A second paragraph."],
        nat_succ(&SYNTAX),
    );

    let doc = declaration
        .doc
        .expect("a documented declaration carries one");
    assert_eq!(doc.lines, ["Their sum.", "", "A second paragraph."]);
}

// The table states an operand's type by the operand's position, and an entry names its parameters in its own order: `gt` is `lt` with its operands the other way round. A shift takes an `Int` and then a `Nat`, so the parameter standing second in the node is the `Int`.
#[test]
fn a_parameter_takes_the_type_of_the_operand_it_stands_as() {
    let declaration = operation(
        "shifted",
        &["count", "value"],
        &SYNTAX,
        Intrinsic::IntShl(name("value"), name("count")),
    );

    let types = declaration
        .params
        .iter()
        .map(|(_, label, type_)| (label.as_str(), type_.clone()))
        .collect::<Vec<_>>();
    assert_eq!(types, [("count", nat()), ("value", int())]);
    assert_eq!(declaration.output, int());
}

#[test]
fn a_type_and_a_proof_are_implicit_and_every_other_operand_is_written() {
    let declaration = operation(
        "get",
        &["T", "list", "index", "ok"],
        &SYNTAX,
        Intrinsic::ListGet {
            element: name("T"),
            list: name("list"),
            index: name("index"),
            in_range: name("ok"),
        },
    );

    let marks = declaration
        .params
        .iter()
        .map(|(plicity, _, _)| *plicity)
        .collect::<Vec<_>>();
    assert_eq!(
        marks,
        [
            Plicity::Implicit,
            Plicity::Explicit,
            Plicity::Explicit,
            Plicity::Implicit
        ]
    );
}

// A former's operand is the type a reader writes, `List(Nat)`, where an operation's is the one a call leaves to be inferred.
#[test]
fn a_formers_type_operand_is_written() {
    let declaration = operation("List", &["T"], &SYNTAX, Intrinsic::ListType(name("T")));

    assert_eq!(declaration.params[0].0, Plicity::Explicit);
    assert_eq!(declaration.output, type_());
}

// The bound is the row's own proposition over the entry's parameters: `Holds` applied to the decision, which names the divisor by the name the entry gave it.
#[test]
fn a_bound_is_the_tables_proposition_over_the_parameters_it_names() {
    let declaration = operation(
        "div",
        &["a", "b", "ok"],
        &SYNTAX,
        Intrinsic::NatDiv {
            dividend: name("a"),
            divisor: name("b"),
            non_zero: name("ok"),
        },
    );

    assert_eq!(
        declaration.params[2].2,
        applied(
            Subterm::Name(Name::new(true, SYNTAX.proof.holds.qualifier())).into(),
            vec![intrinsic(Intrinsic::NatLt(nat_lit(0), name("b")))]
        )
    );
}

// An entry is one node over its own parameters, so an operand that is none of them leaves the table nothing to type it by.
#[test]
fn an_operand_that_is_no_parameter_is_refused() {
    let node = Intrinsic::NatAdd(name("a"), name("stray"));

    assert!(projection(&["a"], &node, &SYNTAX).is_none());
}
