//! The forms a print writes: a foreign call, a tuple type, a successor and a numeral operand, a family's parameters and indices, a lambda and its body, a non-finite float, an append, and the bound a print keeps.

use {
    super::*,
    crate::Telescope,
    curios_abi::{ForeignFunction, WireResults, WireReturn, WireSignature, WireType},
    std::sync::Arc,
};

/// A call of a row prints as a call of the declaration that states it: with its list where it takes no operand, a `foreign` being a function of none and never a constant, and with the type a row that does not return is handed under its mark.
#[test]
fn a_foreign_call_prints_as_a_call_of_its_declaration() {
    let call = |label: &str, params: Vec<WireType>, returns: WireReturn, operands: Vec<Term>| {
        Term::foreign(
            Arc::new(ForeignFunction {
                name: format!("/host/{label}"),
                signature: WireSignature { params, returns },
            }),
            Global::Authored(Qualifier::from(["host", label])),
            operands,
        )
        .to_string()
    };

    assert_eq!(
        call(
            "clock",
            Vec::new(),
            WireReturn::Answers(WireResults::single(WireType::Nat)),
            Vec::new(),
        ),
        "/host/clock()"
    );
    assert_eq!(
        call(
            "exit",
            vec![WireType::Byte],
            WireReturn::Never,
            vec![Term::tuple_type_unit(), Term::intrinsic(Intrinsic::Byte(3))],
        ),
        "/host/exit(@{}, 0x03)"
    );
}

/// Building a document descends once per link, so this is what [`sub`]'s guard is for — and the depth a diagnostic's term can reach is the elaborator's, not the writer's. Deep enough that a regression is a stack overflow rather than a slow test. The other two walks over a document, running and freeing it, are fixtured in `curios-utilities` at the same depth.
#[test]
fn a_deep_term_is_printed_without_overflowing() {
    const DEEP: usize = 100_000;

    let argument = Term::free_var(&Free::local(0, None));
    let mut term = Term::free_var(&Free::local(0, None));
    for _ in 0..DEEP {
        term = Term::apply(term, [argument.clone()]);
    }

    assert_eq!(term.to_string().matches('(').count(), DEEP);
}

/// A tuple type's unlabeled positions print as source writes them.
///
/// The rebuild that restores source labels reads them from [`Telescope::labels`], which renders a hintless binder as `""`. Restoring that as a *hint* would make every unlabeled position look labeled to this printer, and the rename map would then disambiguate the shared empty spelling into `2`, `3` — so `{Nat, Bool, Str}` printed as `{: Nat, 2: Bool, 3: Str}` in every report that named one.
#[test]
fn an_unlabeled_tuple_type_prints_without_labels() {
    let telescope = Telescope::build(
        [
            (Free::local(0, None), Term::intrinsic(Intrinsic::NatType)),
            (Free::local(1, None), Term::intrinsic(Intrinsic::BoolType)),
            (Free::local(2, None), Term::intrinsic(Intrinsic::ByteType)),
        ],
        (),
    );
    let labels = telescope.labels();
    let relabelled = telescope.clone().relabel(&labels);

    let tuple: Term = Subterm::TupleType(TupleType {
        telescope: relabelled,
    })
    .into();
    assert_eq!(tuple.to_string(), "{Nat, Bool, Byte}");
}

/// The other half of the same rule: a position the source *did* label keeps it through the identical rebuild.
#[test]
fn a_labeled_tuple_type_keeps_its_labels_through_a_rebuild() {
    let telescope = Telescope::build(
        [
            (
                Free::local(0, Some("fst")),
                Term::intrinsic(Intrinsic::NatType),
            ),
            (Free::local(1, None), Term::intrinsic(Intrinsic::BoolType)),
        ],
        (),
    );
    let labels = telescope.labels();
    let relabelled = telescope.clone().relabel(&labels);

    let tuple: Term = Subterm::TupleType(TupleType {
        telescope: relabelled,
    })
    .into();
    assert_eq!(tuple.to_string(), "{fst: Nat, Bool}");
}

/// A successor over a symbolic tail prints as `k + 1` without being an operator intrinsic, so as an operand it takes the parentheses an addition would. Printed bare, `n - (k + 1)` read as `n - k + 1`, which is `(n - k) + 1`.
#[test]
fn a_symbolic_successor_is_parenthesized_as_an_operand() {
    let n = Term::free_var(&Free::local(0, Some("n")));
    let successor = || {
        Term::intrinsic(Intrinsic::Nat(Nat::Succ(
            1usize.into(),
            Term::free_var(&Free::local(1, Some("k"))),
        )))
    };

    let difference = Term::intrinsic(Intrinsic::nat_sub(n.clone(), successor()));
    assert_eq!(difference.to_string(), "n - (k + 1)");

    let product = Term::intrinsic(Intrinsic::nat_mul(successor(), n));
    assert_eq!(product.to_string(), "(k + 1) * n");
}

/// A successor over zero is a numeral, which delimits itself.
#[test]
fn a_numeral_operand_prints_bare() {
    let n = Term::free_var(&Free::local(0, Some("n")));
    let difference = Term::intrinsic(Intrinsic::nat_sub(
        n,
        Term::intrinsic(Intrinsic::Nat(Nat::new(3usize))),
    ));
    assert_eq!(difference.to_string(), "n - 3");
}

/// A spelling that shortens `names` to their last segment and marks each as `marks` says, as a diagnostic's would.
fn spelling_of(names: &[(Global, Vec<Plicity>)]) -> Rc<Spelling> {
    let globals = names.iter().map(|(name, _)| *name).collect::<Vec<_>>();
    let mut plicities = Marks::new();
    for (name, marks) in names {
        plicities.insert(*name, Rc::new(marks.clone()));
    }
    Rc::new(
        Spelling::default()
            .with_short_names(Rc::new(build_shorten(&globals)))
            .with_nominal_plicities(Rc::new(plicities)),
    )
}

fn numeral(value: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(value)))
}

/// A family with parameters and indices prints the way its type-constructor function is applied — the parameters, then the indices, one call each — with the parameters marked as declared.
#[test]
fn an_indexed_family_prints_its_parameters_and_its_indices_in_two_calls() {
    let eq = Global::Authored(Qualifier::from(["std", "Eq"]));
    let spelling = spelling_of(&[(
        eq,
        vec![Plicity::Implicit, Plicity::Explicit, Plicity::Explicit],
    )]);

    let family = Term::induct_type(
        eq,
        [Term::intrinsic(Intrinsic::NatType)],
        [numeral(1), numeral(2)],
    );
    assert_eq!(family.spelled(&spelling).to_string(), "Eq(@Nat)(1, 2)");
}

/// A family with only parameters, or only indices, takes them in its one call.
#[test]
fn a_family_with_one_kind_of_argument_prints_in_one_call() {
    let option = Global::Authored(Qualifier::from(["std", "Option"]));
    let sign = Global::Authored(Qualifier::from(["std", "Sign"]));
    let spelling = spelling_of(&[
        (option, vec![Plicity::Explicit]),
        (sign, vec![Plicity::Explicit]),
    ]);

    let parameters = Term::induct_type(
        option,
        [Term::intrinsic(Intrinsic::NatType)],
        Vec::<Term>::new(),
    );
    assert_eq!(parameters.spelled(&spelling).to_string(), "Option(Nat)");

    let indices = Term::induct_type(sign, Vec::<Term>::new(), [numeral(7)]);
    assert_eq!(indices.spelled(&spelling).to_string(), "Sign(7)");
}

/// A lambda over exactly an indexed family's indices is the family at its parameters, which is writable because the family takes its indices in a call of their own.
#[test]
fn a_lambda_over_a_familys_indices_prints_as_the_family_at_its_parameters() {
    let accessible = Global::Authored(Qualifier::from(["std", "WellFounded", "Accessible"]));
    let spelling = spelling_of(&[(
        accessible,
        vec![Plicity::Implicit, Plicity::Explicit, Plicity::Explicit],
    )]);
    let a = Free::local(0, Some("A"));
    let r = Free::local(1, Some("R"));
    let x = Free::local(2, Some("x"));

    let lambda = Term::func(
        [(x, Term::free_var(&a))],
        Term::induct_type(
            accessible,
            [Term::free_var(&a), Term::free_var(&r)],
            [Term::free_var(&x)],
        ),
    );
    assert_eq!(lambda.spelled(&spelling).to_string(), "Accessible(@A, R)");
}

/// A family with no parameters contracts to its bare name.
#[test]
fn a_lambda_over_a_parameterless_familys_indices_prints_as_its_name() {
    let sign = Global::Authored(Qualifier::from(["std", "Sign"]));
    let spelling = spelling_of(&[(sign, vec![Plicity::Explicit])]);
    let i = Free::local(0, Some("i"));

    let lambda = Term::func(
        [(i, Term::intrinsic(Intrinsic::IntType))],
        Term::induct_type(sign, Vec::<Term>::new(), [Term::free_var(&i)]),
    );
    assert_eq!(lambda.spelled(&spelling).to_string(), "Sign");
}

/// A lambda over only some of the indices is no family at its parameters, and prints as the lambda it is.
#[test]
fn a_lambda_over_some_of_a_familys_indices_stays_a_lambda() {
    let eq = Global::Authored(Qualifier::from(["std", "Eq"]));
    let spelling = spelling_of(&[(
        eq,
        vec![Plicity::Implicit, Plicity::Explicit, Plicity::Explicit],
    )]);
    let y = Free::local(0, Some("y"));

    let lambda = Term::func(
        [(y, Term::intrinsic(Intrinsic::NatType))],
        Term::induct_type(
            eq,
            [Term::intrinsic(Intrinsic::NatType)],
            [numeral(1), Term::free_var(&y)],
        ),
    );
    assert_eq!(
        lambda.spelled(&spelling).to_string(),
        "(y) => Eq(@Nat)(1, y)"
    );
}

/// A lambda whose body fits stays on the arrow's line, so a diagnostic naming `(x) => x` does not split it in two. Its one parameter is parenthesized: a lambda's parameter list always is, and the parser refuses a bare `x => x`.
#[test]
fn a_short_lambda_body_stays_on_the_arrows_line() {
    let x = Free::local(0, Some("x"));
    let identity = Term::func(
        [(x, Term::intrinsic(Intrinsic::NatType))],
        Term::free_var(&x),
    );
    assert_eq!(identity.to_string(), "(x) => x");
}

/// A lambda applied where it stands is written in parentheses, the one spelling that reads back as the application: bare, `(x) => x(3)` is a lambda whose body applies `x`.
#[test]
fn an_applied_lambda_is_parenthesized() {
    let x = Free::local(0, Some("x"));
    let identity = Term::func(
        [(x, Term::intrinsic(Intrinsic::NatType))],
        Term::free_var(&x),
    );
    let applied = Term::apply(
        identity,
        [Term::intrinsic(Intrinsic::Nat(Nat::new(3usize)))],
    );
    assert_eq!(applied.to_string(), "((x) => x)(3)");
}

/// A body that carries a break of its own — a `match` — takes the line after the arrow and indents.
#[test]
fn a_lambda_body_with_a_match_breaks_after_the_arrow() {
    let b = Free::local(0, Some("b"));
    let body = Term::bool_match(
        Term::free_var(&b),
        None,
        Term::intrinsic(Intrinsic::NatType),
        Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
        Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
    );
    let lambda = Term::func([(b, Term::intrinsic(Intrinsic::BoolType))], body);
    let printed = lambda.to_string();
    assert!(printed.starts_with("(b) =>\n"), "{printed}");
}

/// A `Flt` prints as what reads back as it, bit for bit: the infinities and the default NaN of either sign as their literals, and any other NaN, which no literal spells, as the call building it from its bytes.
#[test]
fn a_non_finite_flt_prints_as_what_reads_back_as_it() {
    let printed = |value: Floating| Term::intrinsic(Intrinsic::Flt(value)).to_string();
    let negative = |value: Floating| value.copysign(Floating::infinite(true));

    assert_eq!(printed(Floating::infinite(false)), "+inf.0");
    assert_eq!(printed(Floating::infinite(true)), "-inf.0");
    assert_eq!(printed(Floating::nan()), "+nan.0");
    assert_eq!(printed(negative(Floating::nan())), "-nan.0");

    let payload = printed(Floating::from_bits(0x7ff8_0000_0000_0001));
    assert!(payload.contains("of_le_bytes(x["), "{payload}");
}

/// An append prints as the literal a program writes it with, `x[..acc, b]`, since the surface has no named form for one, and a chain of appends splices into one literal. A list append prints the same way over `[…]`.
#[test]
fn an_append_prints_as_the_literal_that_writes_it() {
    let var = |index, hint| Term::free_var(&Free::local(index, Some(hint)));
    let append =
        |bytes: Term, element: Term| Term::intrinsic(Intrinsic::BytesAppend { bytes, element });

    assert_eq!(
        append(var(0, "acc"), var(1, "b")).to_string(),
        "x[..acc, b]"
    );
    assert_eq!(
        append(append(var(0, "acc"), var(1, "b")), var(2, "c")).to_string(),
        "x[..acc, b, c]"
    );

    let list = Term::intrinsic(Intrinsic::ListAppend {
        element: Term::intrinsic(Intrinsic::NatType),
        list: var(0, "xs"),
        item: var(1, "x"),
    });
    assert_eq!(list.to_string(), "[..xs, x]");
}

/// `base` paired with itself `depth` times over: a graph of `depth + 1` nodes whose print is a tree of `2^depth` leaves.
fn paired(base: Term, depth: usize) -> Term {
    let mut term = base;
    for _ in 0..depth {
        term = Term::tuple([term.clone(), term]);
    }
    term
}

/// A print is bounded by its term's graph and not by its tree: sixty levels of a pair naming the level below twice are sixty-one nodes and a print of 2⁶⁰ leaves, which renders its beginning and elides the rest. Both the render and the dry run a rename is built from are bounded, and they elide at the same subterms. A term with no subterm met after the allowance is spent still prints, since naming it costs what eliding it would.
///
/// Mutation-checked: with the allowance never spent, neither call returns; with a term that has no subterm elided like any other, the print ends in `…`.
#[test]
fn a_print_is_bounded_by_its_terms_graph() {
    let term = Term::tuple([paired(numeral(7), 60), numeral(0)]);

    let spelling = Spelling::default();
    let rename = Rc::new(build_rename(&display_names(&term), &spelling));
    let rendered = term
        .spelled(&Rc::new(spelling.with_pretty_names(rename)))
        .to_string();

    assert!(rendered.contains(ELIDED));
    assert!(
        rendered.ends_with(", 0)"),
        "{}",
        &rendered[rendered.len() - 40..]
    );
    assert!(rendered.len() < 400_000, "{} bytes", rendered.len());
}

/// And a term whose tree is within its allowance prints whole, however large: six thousand pairs that share nothing between them are past the floor three times over, and their tree is hardly more than their graph. Nothing below the floor is elided either, shared or not.
///
/// Mutation-checked: with the allowance set to the floor alone, the six thousand are elided.
#[test]
fn a_print_within_its_allowance_is_whole() {
    let wide = Term::tuple((0..6_000).map(|index| Term::tuple([numeral(index), numeral(index)])))
        .to_string();
    assert!(!wide.contains(ELIDED));
    assert!(
        wide.ends_with("(5999, 5999))"),
        "{}",
        &wide[wide.len() - 40..]
    );

    let shared = paired(numeral(7), 3).to_string();
    assert!(!shared.contains(ELIDED));
    assert_eq!(shared.matches('7').count(), 8);
}
