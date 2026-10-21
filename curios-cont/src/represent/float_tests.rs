//! Which joins a float register holds: only one every argument reaching hands a float already, a statement included, never a reference stating nothing.

use {
    super::{Storage, representation, test_support::*},
    crate::{
        Atom, Continuation, Edge, Intrinsic, Literal, Module, Node, Register, Repr, ValueDef,
        ValueId,
    },
    curios_num::{Floating, Rounding},
};

/// A join whose parameter is read as a float, entered on one edge with a float a definition produced and on the other with `other`; `params` are the entry's own. Answers the parameter.
fn float_join(module: &mut Module, params: Vec<ValueId>, flag: ValueId, other: Atom) -> ValueId {
    let joined = module.add_value(Some("joined".into()));
    let read = module.add_value(Some("read".into()));
    let float = module.add_value(Some("float".into()));

    let join = module.reserve_continuation();
    let done = finish(module, vec![Atom::Value(read)]);
    let join_body = module.add_node(Node::LetIntrinsic {
        result: read,
        op: Intrinsic::FltAdd(Rounding::TiesToEven),
        args: vec![
            Atom::Value(joined),
            Atom::Literal(Literal::Flt(Floating::from(1.0))),
        ],
        next: done,
    });
    module.define_continuation(
        join,
        Continuation {
            debug_name: Some("join".into()),
            params: vec![joined],
            body: join_body,
        },
    );

    let branch = module.add_node(Node::Switch {
        scrutinee: Atom::Value(flag),
        cases: [(
            0,
            Edge {
                target: join,
                args: vec![Atom::Value(float)],
            },
        )]
        .into(),
        default: Some(Edge {
            target: join,
            args: vec![other],
        }),
    });
    let body = module.add_node(Node::LetIntrinsic {
        result: float,
        op: Intrinsic::FltAdd(Rounding::TiesToEven),
        args: vec![
            Atom::Literal(Literal::Flt(Floating::from(2.0))),
            Atom::Literal(Literal::Flt(Floating::from(3.0))),
        ],
        next: branch,
    });
    entry(module, params, body);

    joined
}

/// The soundness half of the float rule. A reference states no shape: the one reaching this join may be the error a variant's other constructor holds in the payload slot the float shares, or the string a type computes on the other branch. The join reads a float under one branch only and the edge coerces before any branch is taken, so a register would cast the reference, and the parameter stays one.
#[test]
fn a_parameter_a_reference_reaches_is_never_held_in_a_float_register() {
    let mut module = Module::new();
    let flag = module.add_value(Some("flag".into()));
    let reference = module.add_value(Some("reference".into()));
    let joined = float_join(
        &mut module,
        vec![flag, reference],
        flag,
        Atom::Value(reference),
    );

    assert_eq!(representation(&module).values[&joined], Storage::Boxed);
}

/// The precision half: where every edge hands the parameter a float, a definition's on one and a literal on the other, the register is kept.
#[test]
fn a_parameter_every_edge_hands_a_float_is_held_in_a_register() {
    let mut module = Module::new();
    let flag = module.add_value(Some("flag".into()));
    let joined = float_join(
        &mut module,
        vec![flag],
        flag,
        Atom::Literal(Literal::Flt(Floating::from(4.0))),
    );

    assert_eq!(
        representation(&module).values[&joined],
        Storage::Raw(Repr::Flt)
    );
}

/// A reference whose type named a float states it, and feeds the register by one unbox on the edge: the stated half of the float rule, which the unstated reference above is the other side of.
#[test]
fn a_parameter_a_stated_float_reaches_is_held_in_a_register() {
    let mut module = Module::new();
    let flag = module.add_value(Some("flag".into()));
    let stated = module.add_value_def(ValueDef {
        debug_name: Some("stated".into()),
        register: Some(Register::Float),
    });
    let joined = float_join(&mut module, vec![flag, stated], flag, Atom::Value(stated));

    let representation = representation(&module);
    assert_eq!(representation.values[&joined], Storage::Raw(Repr::Flt));
    assert_eq!(
        representation.values[&stated],
        Storage::Boxed,
        "the stated value keeps the reference it arrives as"
    );
}

/// A stated word feeds a float register no more than an unstated reference does.
#[test]
fn a_parameter_a_stated_word_reaches_is_never_held_in_a_float_register() {
    let mut module = Module::new();
    let flag = module.add_value(Some("flag".into()));
    let stated = module.add_value_def(ValueDef {
        debug_name: Some("stated".into()),
        register: Some(Register::Word),
    });
    let joined = float_join(&mut module, vec![flag, stated], flag, Atom::Value(stated));

    assert_eq!(representation(&module).values[&joined], Storage::Boxed);
}
