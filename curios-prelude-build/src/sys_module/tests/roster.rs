//! What a `/sys` description lowers to.
//!
//! The subject is the description layer, not the roster's contents. What `/sys` declares is checked where it already is — the prelude build elaborates and certifies every one of these declarations, and `into_core/sys_tests.rs` pins the root's reachability and `/sys/Io`'s roster — so a second statement of the roster here would be a copy to keep in step rather than a check. What has no other check is the shape a `Decl` lowers to, how a node's row in `Intrinsic::signature` is read into a declaration's types, and how a parameter's plicity is read.

use {
    super::*,
    curios_common::Plicity,
    curios_module::SYNTAX,
    curios_surface::{FuncSugarParam, LetSignature, Name, Pattern, Subterm},
};

/// The parameters `declaration` states, a function's.
fn parameters(declaration: &Decl) -> &[Parameter] {
    match &declaration.kind {
        Kind::Function(parameters) => parameters,
        Kind::Constant => panic!("the declaration is a function"),
    }
}

// What a declaration is, a constant or a function, is what its constructor states and never how many parameters it names: a function of no parameters is a function still, called `f()`, and a constant is written with no parameter list.
#[test]
fn a_declaration_is_a_constant_or_a_function_by_what_states_it() {
    let constant = pub_let("stdin", nat(), nat_lit(0)).into_let().signature;
    let function = pub_fn("now", vec![], nat(), nat_lit(0))
        .into_let()
        .signature;

    assert!(
        matches!(constant, LetSignature::Name { .. }),
        "a constant lowered to {constant:?}"
    );
    assert!(
        matches!(&function, LetSignature::Func { params, .. } if params.is_empty()),
        "a function of no parameters lowered to {function:?}"
    );
}

// The same of a row that is one intrinsic node: an operation is a function whatever it is handed, and a row that is a value says so, the kind of neither following from how many operands the node takes.
#[test]
fn an_operation_of_no_parameters_is_a_function_and_a_value_row_a_constant() {
    let performed = operation("handle", &[], &SYNTAX, Intrinsic::HandleType)
        .into_let()
        .signature;
    let named = constant("Handle", &SYNTAX, Intrinsic::HandleType)
        .into_let()
        .signature;

    assert!(
        matches!(&performed, LetSignature::Func { params, .. } if params.is_empty()),
        "an operation of no parameters lowered to {performed:?}"
    );
    assert!(
        matches!(named, LetSignature::Name { .. }),
        "a value row lowered to {named:?}"
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

    let marks = params
        .iter()
        .map(FuncSugarParam::plicity)
        .collect::<Vec<_>>();
    assert_eq!(marks, [Plicity::Implicit, Plicity::Explicit]);
    assert!(
        matches!(&params[0], FuncSugarParam::Bound { binder: Pattern::Binder(binder), .. } if binder.as_str() == "T")
    );
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

    let types = parameters(&declaration)
        .iter()
        .map(|parameter| (parameter.label.as_str(), parameter.type_.clone()))
        .collect::<Vec<_>>();
    let int = intrinsic(Intrinsic::IntType);

    assert_eq!(types, [("count", nat()), ("value", int.clone())]);
    assert_eq!(declaration.output, int);
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

    let marks = parameters(&declaration)
        .iter()
        .map(|parameter| parameter.plicity)
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

    assert_eq!(parameters(&declaration)[0].plicity, Plicity::Explicit);
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
        parameters(&declaration)[2].type_,
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
