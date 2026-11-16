//! A call's path, and the parts of its member it calls.

use {
    super::*,
    curios_core::{Free, Term},
};

fn var(index: u64, hint: &str) -> Term {
    Term::free_var(&Free::local(index, Some(hint)))
}

/// The parts of one member whose leading lambdas take one parameter and answer a record: at slot `0` a record holding a lambda of one more parameter at its own slot `0`, and at slot `1` a lambda of one more.
fn parts() -> Vec<PartShape> {
    vec![
        PartShape {
            member: 0,
            path: Vec::new(),
            arity: 1,
        },
        PartShape {
            member: 0,
            path: vec![0, 0],
            arity: 2,
        },
        PartShape {
            member: 0,
            path: vec![1],
            arity: 2,
        },
    ]
}

#[test]
fn a_path_is_its_root_and_its_steps_in_order() {
    let (w, a, x) = (var(0, "w"), var(1, "a"), var(2, "x"));
    let term = Term::apply(
        Term::proj(Term::apply(w.clone(), [a.clone()]), 1),
        [x.clone()],
    );

    let (root, steps) = call_path(&term);

    assert_eq!(root, w);
    assert!(
        matches!(
            &steps[..],
            [Segment::Arguments(first), Segment::Field(1), Segment::Arguments(second)]
                if *first == [a.clone()] && *second == [x.clone()]
        ),
        "{steps:?}"
    );
}

// A field is followed where the arguments before it fill the part it projects out of, through a record holding the part as well as to the part itself, and the path calls the leading lambdas it answered the record by too; a field reached too early, or one no part lies at, ends the path, and the call is the part reached so far with the arguments handed it.
#[test]
fn a_path_calls_the_part_its_fields_reach_where_its_arguments_fill_each() {
    let (a, x) = (var(1, "a"), var(2, "x"));
    let arguments =
        |terms: &[&Term]| Segment::Arguments(terms.iter().map(|term| (*term).clone()).collect());

    assert_eq!(
        part_calls(
            &parts(),
            0,
            &[arguments(&[&a]), Segment::Field(1), arguments(&[&x])]
        ),
        [(2, vec![a.clone(), x.clone()]), (0, vec![a.clone()])]
    );
    assert_eq!(
        part_calls(
            &parts(),
            0,
            &[
                arguments(&[&a]),
                Segment::Field(0),
                Segment::Field(0),
                arguments(&[&x])
            ]
        ),
        [(1, vec![a.clone(), x.clone()]), (0, vec![a.clone()])]
    );
    assert_eq!(
        part_calls(&parts(), 0, &[Segment::Field(1), arguments(&[&x])]),
        [(0, Vec::new())]
    );
    assert_eq!(
        part_calls(
            &parts(),
            0,
            &[arguments(&[&a]), Segment::Field(2), arguments(&[&x])]
        ),
        [(0, vec![a.clone()])]
    );
}

// A path that reaches no field lambda lets the record it stands at escape, so it calls every part under it with the arguments it handed: the member named bare, the member applied, and a record literal held at a field.
#[test]
fn a_path_short_of_a_field_lambda_calls_every_part_under_where_it_stands() {
    let a = var(1, "a");

    assert_eq!(
        part_calls(&parts(), 0, &[]),
        [(0, Vec::new()), (1, Vec::new()), (2, Vec::new())]
    );
    assert_eq!(
        part_calls(&parts(), 0, &[Segment::Arguments(vec![a.clone()])]),
        [
            (0, vec![a.clone()]),
            (1, vec![a.clone()]),
            (2, vec![a.clone()])
        ]
    );
    assert_eq!(
        part_calls(
            &parts(),
            0,
            &[Segment::Arguments(vec![a.clone()]), Segment::Field(0)]
        ),
        [(1, vec![a.clone()]), (0, vec![a.clone()])]
    );
}
