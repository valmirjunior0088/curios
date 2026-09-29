use super::*;

#[test]
fn a_binder_hinted_like_a_shortened_global_is_suffixed() {
    let global = Global::Authored(Qualifier::from(["main", "helper"]));
    let shorten = build_shorten(std::slice::from_ref(&global));
    assert_eq!(shorten.get(&global).map(String::as_str), Some("helper"));

    let binder = Free::local(0, Some("helper"));
    let names = DisplayNames {
        names: BTreeSet::from([Free::Global(global), binder.clone()]),
        labels: BTreeSet::new(),
    };
    let rename = build_rename(
        &names,
        &Spelling::default().with_short_names(Rc::new(shorten)),
    );
    assert_eq!(rename.get(&binder).map(String::as_str), Some("helper2"));
}

/// A tuple label keeps the spelling it was written with, being part of its tuple type's identity, and a binder that would read like it is the one suffixed: a function's parameter `frame` beside its result's field `frame`.
#[test]
fn a_tuple_label_keeps_its_spelling_and_a_like_named_binder_is_suffixed() {
    let parameter = Free::local(0, Some("frame"));
    let label = Free::local(1, Some("frame"));
    let names = DisplayNames {
        names: BTreeSet::from([parameter.clone(), label.clone()]),
        labels: BTreeSet::from([label.clone()]),
    };
    let rename = build_rename(&names, &Spelling::default());

    assert_eq!(rename.get(&label).map(String::as_str), Some("frame"));
    assert_eq!(rename.get(&parameter).map(String::as_str), Some("frame2"));
}

/// A reader's own declaration keeps the bare name, and a like-named one from the environment takes the longer spelling that actually reaches it.
///
/// One suffix table over both tiers tied `Holds` between the two, so *neither* shortened: the name the reader had just written reported as `/Holds` while `/std/Bool/Holds` — which no bare `Holds` reaches — reported as `Bool/Holds`.
#[test]
fn a_readers_own_declaration_claims_the_bare_name_before_its_environment() {
    let own = Global::Authored(Qualifier::from(["Holds"]));
    let nested = Global::Authored(Qualifier::from(["std", "Bool", "Holds"]));

    let shorten = build_shorten_layered(std::slice::from_ref(&own), std::slice::from_ref(&nested));

    assert_eq!(shorten.get(&own).map(String::as_str), Some("Holds"));
    assert_eq!(shorten.get(&nested).map(String::as_str), Some("Bool/Holds"));
}

/// A *nested* own declaration gets no such claim. Reaching `/Vec/nil` needs `Vec/nil` or an import, exactly as reaching `/std/Vec/nil` does, so its bare label is no more writable than the environment's and it stays in the shared contest — where a genuine tie leaves both spelled in full.
///
/// Handing it the label instead spelled a goal candidate the reader could not paste: `? \u{2248} nil()` for a constructor only `Vec/nil()` reaches.
#[test]
fn a_nested_own_declaration_does_not_claim_its_bare_label() {
    let own_type = Global::Authored(Qualifier::from(["Vec"]));
    let own_ctor = Global::Authored(Qualifier::from(["Vec", "nil"]));
    let outer = Global::Authored(Qualifier::from(["std", "Vec", "nil"]));

    let own = [own_type.clone(), own_ctor.clone()];
    let shorten = build_shorten_layered(&own, std::slice::from_ref(&outer));

    // The type itself does sit at the unit root, so it takes its label and the environment's twin gives way.
    assert_eq!(shorten.get(&own_type).map(String::as_str), Some("Vec"));
    assert_eq!(shorten.get(&own_ctor), None);
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
    let globals = names
        .iter()
        .map(|(name, _)| name.clone())
        .collect::<Vec<_>>();
    Rc::new(
        Spelling::default()
            .with_short_names(Rc::new(build_shorten(&globals)))
            .with_nominal_plicities(Rc::new(names.iter().cloned().collect())),
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
        eq.clone(),
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
        (option.clone(), vec![Plicity::Explicit]),
        (sign.clone(), vec![Plicity::Explicit]),
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

/// A lambda over exactly an indexed family's indices is the family at its parameters, which is writable now that the family takes its indices in a call of their own.
#[test]
fn a_lambda_over_a_familys_indices_prints_as_the_family_at_its_parameters() {
    let accessible = Global::Authored(Qualifier::from(["std", "WellFounded", "Accessible"]));
    let spelling = spelling_of(&[(
        accessible.clone(),
        vec![Plicity::Implicit, Plicity::Explicit, Plicity::Explicit],
    )]);
    let a = Free::local(0, Some("A"));
    let r = Free::local(1, Some("R"));
    let x = Free::local(2, Some("x"));

    let lambda = Term::func(
        [(x.clone(), Term::free_var(&a))],
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
    let spelling = spelling_of(&[(sign.clone(), vec![Plicity::Explicit])]);
    let i = Free::local(0, Some("i"));

    let lambda = Term::func(
        [(i.clone(), Term::intrinsic(Intrinsic::IntType))],
        Term::induct_type(sign, Vec::<Term>::new(), [Term::free_var(&i)]),
    );
    assert_eq!(lambda.spelled(&spelling).to_string(), "Sign");
}

/// A lambda over only some of the indices is no family at its parameters, and prints as the lambda it is.
#[test]
fn a_lambda_over_some_of_a_familys_indices_stays_a_lambda() {
    let eq = Global::Authored(Qualifier::from(["std", "Eq"]));
    let spelling = spelling_of(&[(
        eq.clone(),
        vec![Plicity::Implicit, Plicity::Explicit, Plicity::Explicit],
    )]);
    let y = Free::local(0, Some("y"));

    let lambda = Term::func(
        [(y.clone(), Term::intrinsic(Intrinsic::NatType))],
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
        [(x.clone(), Term::intrinsic(Intrinsic::NatType))],
        Term::free_var(&x),
    );
    assert_eq!(identity.to_string(), "(x) => x");
}

/// A body that carries a break of its own — a `match` — still takes the line after the arrow and indents, as it did before the group.
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
    let lambda = Term::func([(b.clone(), Term::intrinsic(Intrinsic::BoolType))], body);
    let printed = lambda.to_string();
    assert!(printed.starts_with("(b) =>\n"), "{printed}");
}

/// A module `m` declaring a concept `Show` with one method `show`, whose wrapper takes the method's parameter in its group, and an alias `Wrap` taking a `Show` witness — spelled under axis (h).
struct Witnesses {
    show: Global,
    wrap: Global,
    spelling: Rc<Spelling>,
}

fn witnesses(operator: Option<InfixOp>) -> Witnesses {
    let show = Global::Authored(Qualifier::from(["m", "Show"]));
    let method = Global::Authored(Qualifier::from(["m", "Show", "show"]));
    let wrap = Global::Authored(Qualifier::from(["m", "Wrap"]));
    let mut table = WitnessSpelling::default();
    table.concepts.insert(
        show.clone(),
        vec![FieldSpelling::Method {
            wrapper: method.clone(),
            merged: true,
            operator,
        }],
    );
    let globals = [show.clone(), method, wrap.clone()];
    Witnesses {
        show,
        wrap,
        spelling: Rc::new(
            Spelling::default()
                .with_short_names(Rc::new(build_shorten(&globals)))
                .with_witness_spelling(Rc::new(table)),
        ),
    }
}

/// `(@A: Type, <witnesses…>, x: Wrap(A, use <argument>)) -> Nat`, over witness binders named by `binders`, each a `Show(A)`.
fn under_witnesses(witnesses: &Witnesses, binders: &[&str], argument: usize) -> Term {
    let a = Free::local(0, Some("A"));
    let labels = binders
        .iter()
        .enumerate()
        .map(|(index, hint)| Free::local(1 + index as u32, Some(*hint)))
        .collect::<Vec<_>>();
    let x = Free::local(9, Some("x"));
    let concept = Term::struct_type(witnesses.show.clone(), [Term::free_var(&a)]);
    let wrapped = Term::apply_marked(
        Term::free_var(&Free::Global(witnesses.wrap.clone())),
        [
            (Plicity::Explicit, Term::free_var(&a)),
            (Plicity::Witness, Term::free_var(&labels[argument])),
        ],
    );
    Term::func_type_marked(
        std::iter::once((Plicity::Implicit, a.clone(), Term::type_ground()))
            .chain(
                labels
                    .iter()
                    .map(|label| (Plicity::Witness, label.clone(), concept.clone())),
            )
            .chain(std::iter::once((Plicity::Explicit, x, wrapped))),
        Term::intrinsic(Intrinsic::NatType),
    )
}

/// A function type cannot name its witness, and a `use` argument resolution would restore is left out — so once the one reference to the binder is gone, the binder prints unnamed.
#[test]
fn a_witness_resolution_would_restore_is_left_out_and_its_binder_unnamed() {
    let witnesses = witnesses(None);
    let type_ = under_witnesses(&witnesses, &["w"], 0);
    assert_eq!(
        type_.spelled(&witnesses.spelling).to_string(),
        "(@A: Type, use Show(A), x: Wrap(A)) -> Nat"
    );
}

/// A binder shadowed by an inner one of its concept is not what resolution would find, so its argument stays, and the binder keeps the name the argument needs.
#[test]
fn a_shadowed_witness_keeps_its_argument_and_its_binder_its_name() {
    let witnesses = witnesses(None);
    let type_ = under_witnesses(&witnesses, &["outer", "inner"], 0);
    assert_eq!(
        type_.spelled(&witnesses.spelling).to_string(),
        "(@A: Type, use outer: Show(A), use Show(A), x: Wrap(A, use outer)) -> Nat"
    );
}

/// A method projected off a witness reads as the call a program writes: its wrapper, with the concept's parameter marked and the witness left to resolution.
#[test]
fn a_method_projected_off_a_witness_prints_as_its_call() {
    let witnesses = witnesses(None);
    let a = Free::local(0, Some("A"));
    let w = Free::local(1, Some("w"));
    let x = Free::local(2, Some("x"));
    let call = Term::apply(Term::proj(Term::free_var(&w), 0), [Term::free_var(&x)]);
    let type_ = Term::func_type_marked(
        [
            (Plicity::Implicit, a.clone(), Term::type_ground()),
            (
                Plicity::Witness,
                w,
                Term::struct_type(witnesses.show.clone(), [Term::free_var(&a)]),
            ),
            (Plicity::Explicit, x, Term::free_var(&a)),
        ],
        Term::apply(
            Term::free_var(&Free::Global(witnesses.wrap.clone())),
            [call],
        ),
    );
    assert_eq!(
        type_.spelled(&witnesses.spelling).to_string(),
        "(@A: Type, use Show(A), x: A) -> Wrap(show(@A, x))"
    );
}

/// A method projected off a witness resolution would not restore keeps that witness: called, as its wrapper's `use` argument, and taken as a value, as the projection itself, since the wrapper alone would resolve to the inner binder.
#[test]
fn a_method_off_a_shadowed_witness_keeps_the_witness() {
    let witnesses = witnesses(None);
    let a = Free::local(0, Some("A"));
    let outer = Free::local(1, Some("outer"));
    let inner = Free::local(2, Some("inner"));
    let x = Free::local(3, Some("x"));
    let concept = Term::struct_type(witnesses.show.clone(), [Term::free_var(&a)]);
    let method = Term::proj(Term::free_var(&outer), 0);
    let type_ = Term::func_type_marked(
        [
            (Plicity::Implicit, a.clone(), Term::type_ground()),
            (Plicity::Witness, outer, concept.clone()),
            (Plicity::Witness, inner, concept),
            (Plicity::Explicit, x.clone(), Term::free_var(&a)),
        ],
        Term::apply(
            Term::free_var(&Free::Global(witnesses.wrap.clone())),
            [Term::apply(method.clone(), [Term::free_var(&x)]), method],
        ),
    );
    assert_eq!(
        type_.spelled(&witnesses.spelling).to_string(),
        "(@A: Type, use outer: Show(A), use Show(A), x: A) -> Wrap(show(@A, use outer, x), (outer).0)"
    );
}

/// An operator's method projected off a witness reads as the operator — `!=` too, whose concept slot is its own, so the disequality is never spelled as a negated equality.
#[test]
fn an_operators_method_projected_off_a_witness_prints_as_the_operator() {
    for (op, symbol) in [(InfixOp::Eql, "=="), (InfixOp::Neq, "!=")] {
        let witnesses = witnesses(Some(op));
        let a = Free::local(0, Some("A"));
        let w = Free::local(1, Some("w"));
        let x = Free::local(2, Some("x"));
        let compared = Term::apply(
            Term::proj(Term::free_var(&w), 0),
            [Term::free_var(&x), Term::free_var(&x)],
        );
        let type_ = Term::func_type_marked(
            [
                (Plicity::Implicit, a.clone(), Term::type_ground()),
                (
                    Plicity::Witness,
                    w,
                    Term::struct_type(witnesses.show.clone(), [Term::free_var(&a)]),
                ),
                (Plicity::Explicit, x, Term::free_var(&a)),
            ],
            Term::apply(
                Term::free_var(&Free::Global(witnesses.wrap.clone())),
                [compared],
            ),
        );
        assert_eq!(
            type_.spelled(&witnesses.spelling).to_string(),
            format!("(@A: Type, use Show(A), x: A) -> Wrap(x {symbol} x)")
        );
    }
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

/// An append prints as the literal a program writes it with, `b[..acc, b]`, since the surface has no named form for one, and a chain of appends splices into one literal. A list append prints the same way over `[…]`.
#[test]
fn an_append_prints_as_the_literal_that_writes_it() {
    let var = |index, hint| Term::free_var(&Free::local(index, Some(hint)));
    let append = |bin: Term, element: Term| {
        Term::intrinsic(Intrinsic::BinAppend {
            grain: Grain::B,
            bin,
            element,
        })
    };

    assert_eq!(
        append(var(0, "acc"), var(1, "b")).to_string(),
        "b[..acc, b]"
    );
    assert_eq!(
        append(append(var(0, "acc"), var(1, "b")), var(2, "c")).to_string(),
        "b[..acc, b, c]"
    );

    let list = Term::intrinsic(Intrinsic::ListAppend {
        element: Term::intrinsic(Intrinsic::NatType),
        list: var(0, "xs"),
        item: var(1, "x"),
    });
    assert_eq!(list.to_string(), "[..xs, x]");
}
