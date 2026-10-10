//! Congruence through every intrinsic operand, and the carriers that compare element-wise.

use {
    super::test_support::*,
    curios_core::{
        Free, InstanceHead, Intrinsic, Level, MetavarId, MetavarOrigin, Nat, ReduceError, Subterm,
        Term, Universes, Var,
    },
    curios_num::{Binary, Grain},
};

#[test]
fn intrinsic_nat_add_recurses_into_operands() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));

    let this = func(
        [&x],
        Subterm::Intrinsic(Intrinsic::nat_add(
            Term::free_var(&x),
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(1usize))),
        )),
    );

    let that = func(
        [&y],
        Subterm::Intrinsic(Intrinsic::nat_add(
            Term::free_var(&y),
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(1usize))),
        )),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn intrinsic_flt_neg_recurses_into_operand() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));

    let this = func(
        [&x],
        Subterm::Intrinsic(Intrinsic::flt_neg(Term::free_var(&x))),
    );

    let that = func(
        [&y],
        Subterm::Intrinsic(Intrinsic::flt_neg(Term::free_var(&y))),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn intrinsic_nat_to_int_recurses_into_operand() {
    let mut context = context();

    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));
    let this = func(
        [&x],
        Subterm::Intrinsic(Intrinsic::nat_to_int(Term::free_var(&x))),
    );
    let that = func(
        [&y],
        Subterm::Intrinsic(Intrinsic::nat_to_int(Term::free_var(&y))),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn intrinsic_list_compares_element_wise() {
    let mut context = context();

    let this = Subterm::Intrinsic(Intrinsic::List {
        element: Term::intrinsic(Intrinsic::NatType),
        items: vec![
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(1usize))).into(),
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(2usize))).into(),
        ],
    })
    .into();

    let that = Subterm::Intrinsic(Intrinsic::List {
        element: Term::intrinsic(Intrinsic::NatType),
        items: vec![
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(1usize))).into(),
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(2usize))).into(),
        ],
    })
    .into();

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn intrinsic_list_rejects_different_lengths() {
    let mut context = context();

    let this = Subterm::Intrinsic(Intrinsic::List {
        element: Term::intrinsic(Intrinsic::NatType),
        items: vec![Subterm::Intrinsic(Intrinsic::Nat(Nat::new(1usize))).into()],
    })
    .into();

    let that = Subterm::Intrinsic(Intrinsic::List {
        element: Term::intrinsic(Intrinsic::NatType),
        items: vec![
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(1usize))).into(),
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(2usize))).into(),
        ],
    })
    .into();

    assert_eq!(conv(&mut context, &this, &that), Ok(false));
}

#[test]
fn intrinsic_bin_type_is_equal_to_itself() {
    let mut context = context();

    let this = Subterm::Intrinsic(Intrinsic::BinType(Grain::X)).into();
    let that = Subterm::Intrinsic(Intrinsic::BinType(Grain::X)).into();

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn intrinsic_bin_literal_compares_bytes() {
    let mut context = context();

    assert_eq!(
        conv(
            &mut context,
            &Subterm::Intrinsic(Intrinsic::Bin(Grain::X, Binary::from_bytes(vec![1, 2]))).into(),
            &Subterm::Intrinsic(Intrinsic::Bin(Grain::X, Binary::from_bytes(vec![1, 2]))).into(),
        ),
        Ok(true)
    );

    assert_eq!(
        conv(
            &mut context,
            &Subterm::Intrinsic(Intrinsic::Bin(Grain::X, Binary::from_bytes(vec![1, 2]))).into(),
            &Subterm::Intrinsic(Intrinsic::Bin(Grain::X, Binary::from_bytes(vec![1, 3]))).into(),
        ),
        Ok(false)
    );
}

#[test]
fn intrinsic_bin_len_recurses_into_operand() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));

    let this = func(
        [&x],
        Subterm::Intrinsic(Intrinsic::bin_len(Grain::X, Term::free_var(&x))),
    );
    let that = func(
        [&y],
        Subterm::Intrinsic(Intrinsic::bin_len(Grain::X, Term::free_var(&y))),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn intrinsic_bin_get_recurses_into_operands() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let a = context.fresh(Some("a"));
    let y = context.fresh(Some("y"));
    let b = context.fresh(Some("b"));

    let this = func(
        [&x],
        func(
            [&a],
            Subterm::Intrinsic(Intrinsic::bin_get(
                Grain::X,
                Term::free_var(&x),
                Term::free_var(&a),
                qed(),
            )),
        ),
    );

    let that = func(
        [&y],
        func(
            [&b],
            Subterm::Intrinsic(Intrinsic::bin_get(
                Grain::X,
                Term::free_var(&y),
                Term::free_var(&b),
                qed(),
            )),
        ),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn intrinsic_bin_concat_recurses_into_operands() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let a = context.fresh(Some("a"));
    let y = context.fresh(Some("y"));
    let b = context.fresh(Some("b"));

    let this = func(
        [&x],
        func(
            [&a],
            Subterm::Intrinsic(Intrinsic::bin_concat(
                Grain::X,
                [Term::free_var(&x), Term::free_var(&a)],
            )),
        ),
    );

    let that = func(
        [&y],
        func(
            [&b],
            Subterm::Intrinsic(Intrinsic::bin_concat(
                Grain::X,
                [Term::free_var(&y), Term::free_var(&b)],
            )),
        ),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

// The heads are one number spelled two ways, so the peel cannot strip them, and a peel handing the nesting back intact would leave shape congruence a two-operand concatenation against a three-operand one. Regrouping in the peel is what lets the pair reach the operand comparison that decides `n + m ≡ m + n`.
#[test]
fn a_nested_concatenation_converts_with_its_flat_spelling_past_unlike_heads() {
    let mut context = context();
    let g = context.fresh(Some("g"));
    let n = context.fresh(Some("n"));
    let m = context.fresh(Some("m"));
    let t = context.fresh(Some("t"));
    let h = context.fresh(Some("h"));

    let head = |left: &Free, right: &Free| {
        Term::apply(
            Term::free_var(&g),
            [Term::intrinsic(Intrinsic::nat_add(
                Term::free_var(left),
                Term::free_var(right),
            ))],
        )
    };
    let single = Term::intrinsic(Intrinsic::List {
        element: nat_type(),
        items: vec![Term::free_var(&h)],
    });
    let cat = |operands: Vec<Term>| Term::intrinsic(Intrinsic::list_concat(nat_type(), operands));

    let nested = cat(vec![
        cat(vec![head(&n, &m), Term::free_var(&t)]),
        single.clone(),
    ]);
    let flat = cat(vec![head(&m, &n), Term::free_var(&t), single]);

    assert_eq!(
        conv(
            &mut context,
            &func([&g, &n, &m, &t, &h], nested),
            &func([&g, &n, &m, &t, &h], flat),
        ),
        Ok(true)
    );
}

#[test]
fn intrinsic_bin_slice_recurses_into_operands() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let a = context.fresh(Some("a"));
    let p = context.fresh(Some("p"));
    let y = context.fresh(Some("y"));
    let b = context.fresh(Some("b"));
    let q = context.fresh(Some("q"));

    let this = func(
        [&x],
        func(
            [&a],
            func(
                [&p],
                Subterm::Intrinsic(Intrinsic::bin_slice(
                    Grain::X,
                    Term::free_var(&x),
                    Term::free_var(&a),
                    Term::free_var(&p),
                    qed(),
                )),
            ),
        ),
    );

    let that = func(
        [&y],
        func(
            [&b],
            func(
                [&q],
                Subterm::Intrinsic(Intrinsic::bin_slice(
                    Grain::X,
                    Term::free_var(&y),
                    Term::free_var(&b),
                    Term::free_var(&q),
                    qed(),
                )),
            ),
        ),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

// A `Bool` connective against a term that is no intrinsic at all — absorption's shape — never reaches the intrinsic congruence, so the truth table over the two sides' atoms is asked at the head dispatch. The control differs at one assignment and stays apart.
#[test]
fn a_bool_tree_converts_with_the_bare_term_it_equals_at_every_assignment() {
    let mut context = context();
    let (b, c) = (
        Term::free_var(&context.fresh(Some("b"))),
        Term::free_var(&context.fresh(Some("c"))),
    );
    let or = |left: Term, right: Term| Term::intrinsic(Intrinsic::BoolOr(left, right));
    let absorbed = or(
        b.clone(),
        Term::intrinsic(Intrinsic::BoolAnd(b.clone(), c.clone())),
    );

    assert_eq!(conv(&mut context, &absorbed, &b), Ok(true));
    assert_eq!(conv(&mut context, &b, &absorbed), Ok(true));
    assert_eq!(conv(&mut context, &or(b.clone(), c), &b), Ok(false));
}

/// Two applications of one `Nat`-valued operation that differ only in a universe instance are one number: `len(n<0>)` and `len(n<1>)` meet before their congruence, which would compare the two instances' levels and refuse. The control is two different operands at one instance, which stay apart. The kernel's twin of this proposition shares the name.
#[test]
fn one_operation_at_two_universe_instances_is_one_number() {
    let mut context = context();
    let length = |operand: &Free, level: u32| {
        Term::intrinsic(Intrinsic::bin_len(
            Grain::X,
            Term::instance(
                InstanceHead::Var(Var::free(*operand)),
                vec![Level::constant(level)],
            ),
        ))
    };
    let (n, m) = (context.fresh(Some("n")), context.fresh(Some("m")));

    assert_eq!(conv(&mut context, &length(&n, 0), &length(&n, 1)), Ok(true));
    assert_eq!(
        conv(&mut context, &length(&n, 0), &length(&m, 0)),
        Ok(false)
    );
}

/// The levels a result carries are compared before its operands: two polls of one cell whose results are the cell's family at two ground levels are two terms, though every operand agrees — which the elaborator reports as an inconsistency of its universe context, since its level comparison is a constraint. The control is the same poll at one level. The kernel's twin of this proposition shares the name.
#[test]
fn two_polls_of_one_cell_at_two_ground_levels_do_not_convert() {
    let mut context = context();
    let cell = Term::free_var(&context.fresh(Some("cell")));
    let poll = |level: u32| {
        Term::intrinsic(Intrinsic::CellPoll {
            element: Term::intrinsic(Intrinsic::NatType),
            cell: cell.clone(),
            universes: Universes::levels(vec![Level::constant(level)]),
        })
    };

    assert!(matches!(
        conv(&mut context, &poll(0), &poll(1)),
        Err(ReduceError::Universe(_))
    ));
    assert_eq!(conv(&mut context, &poll(0), &poll(0)), Ok(true));
}

/// The list view: `[?0, ..?1]` against a longer literal pairs the one written element and leaves the tail what the literal has left, where the congruence would compare a run of one element with a run of three and stop at their lengths.
#[test]
fn a_lists_unknown_tail_is_solved_to_what_a_longer_literal_has_left() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), nat_type());
    context.birth_metavar(
        MetavarId(1),
        Vec::new(),
        Term::intrinsic(Intrinsic::ListType(nat_type())),
    );

    let literal = |items: Vec<Term>| {
        Term::intrinsic(Intrinsic::List {
            element: nat_type(),
            items,
        })
    };
    let cons = Term::intrinsic(Intrinsic::list_concat(
        nat_type(),
        [literal(vec![Term::hole(0)]), Term::hole(1)],
    ));

    assert_eq!(
        conv(&mut context, &cons, &literal(vec![nat(1), nat(2), nat(3)])),
        Ok(true)
    );
    assert_eq!(context.metavar_solution(MetavarId(0)), Some(&nat(1)));
    assert_eq!(
        context.metavar_solution(MetavarId(1)),
        Some(&literal(vec![nat(2), nat(3)]))
    );
}

/// The same split against a concatenation: the tail is everything past the first element, the literal's other element and the two operands behind it.
#[test]
fn a_lists_unknown_tail_is_solved_past_a_longer_prefix_and_a_concatenation() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));
    let list_type = Term::intrinsic(Intrinsic::ListType(nat_type()));
    context.assume(&a, &list_type);
    context.assume(&b, &list_type);
    let scope = vec![(a, list_type.clone()), (b, list_type.clone())];
    context.birth_metavar(MetavarId(0), scope.clone(), nat_type());
    context.birth_metavar(MetavarId(1), scope, list_type);

    let spine = vec![Term::free_var(&a), Term::free_var(&b)];
    let literal = |items: Vec<Term>| {
        Term::intrinsic(Intrinsic::List {
            element: nat_type(),
            items,
        })
    };
    let cat = |operands: Vec<Term>| Term::intrinsic(Intrinsic::list_concat(nat_type(), operands));
    let cons = cat(vec![
        literal(vec![Term::metavar_birthed(
            0,
            MetavarOrigin::Hole,
            spine.clone(),
        )]),
        Term::metavar_birthed(1, MetavarOrigin::Hole, spine),
    ]);
    let written = cat(vec![
        literal(vec![nat(1), nat(2)]),
        Term::free_var(&a),
        Term::free_var(&b),
    ]);

    assert_eq!(conv(&mut context, &cons, &written), Ok(true));
    assert!(context.metavar_solution(MetavarId(0)).is_some());
    assert!(context.metavar_solution(MetavarId(1)).is_some());
    // What was solved makes the two spellings one, by reduction alone.
    assert_eq!(conv(&mut context, &cons, &written), Ok(true));
}
