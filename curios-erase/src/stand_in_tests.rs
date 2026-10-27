//! The stand-ins erasure puts for what it does not compute: a proof's withheld body, a decided bound's stand-in, and a function stand-in at a slot of function type.

use {
    crate::test_support::{
        arity, bound, definition, erase, fresh, global, module, nat_lit, nominal, zonked,
    },
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_cert::{Globals, certify_module},
    curios_elab::DEFAULT_BUDGET,
    curios_utilities::Qualifier,
    std::collections::BTreeMap,
};

/// Erasure's reader is handed no proof's body either: the kernel it reads through defines a definition marked a proof as unfolding to nothing, and the definition beside it to its body.
#[test]
fn a_proofs_body_is_withheld_from_erasures_reader() {
    let p = fresh(Some("P"));
    let proof = fresh(Some("p"));
    let curios_core::Item::Let(lemma) = definition(
        "lemma",
        curios_core::Term::func_type(
            [
                (p, curios_core::Term::prop()),
                (proof, curios_core::Term::free_var(&p)),
            ],
            curios_core::Term::free_var(&p),
        ),
        curios_core::Term::func(
            [
                (p, curios_core::Term::prop()),
                (proof, curios_core::Term::free_var(&p)),
            ],
            curios_core::Term::free_var(&proof),
        ),
    ) else {
        unreachable!("a fixture's definition is a `let`")
    };
    let unit = zonked(
        &module(
            vec![
                curios_core::Item::Let(Box::new(curios_core::Definition {
                    proof: true,
                    ..*lemma
                })),
                definition(
                    "two",
                    curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
                    nat_lit(2),
                ),
            ],
            nat_lit(0),
        )
        .module,
    );

    let globals = Globals::default();
    let (rechecked, mut kernel) = certify_module(&unit, DEFAULT_BUDGET, &globals, SYNTAX);
    assert!(rechecked.verdicts.is_empty(), "{:?}", rechecked.verdicts);
    kernel.read(|reader| {
        assert!(reader.value(&global("lemma")).is_none());
        assert!(reader.value(&global("two")).is_some());
    });
}

/// A decided bound's stand-in is shaped without evaluating its subject. `H(b)` is a match on `b` whose arms are the proposition `Top`, `c` folds `Nat` two hundred thousand times, and `head` hands `need` the proof `Top/qed()` of `H(c)`, which the kernel accepts by evaluating `c`. At a fresh `b`, `H(b)` is a match on it whose every arm is no function type, so no instance of it is one and the stand-in is the unit constant: erasing `head` charges under a tenth of what reducing `H(c)` alone does, where shaping the stand-in by that reduction ran the fold a second time.
#[test]
fn a_decided_bounds_stand_in_reads_nothing_of_its_subject() {
    let (b, p, pred, ih) = (
        fresh(Some("b")),
        fresh(Some("p")),
        fresh(Some("pred")),
        fresh(Some("ih")),
    );
    let bool_type = curios_core::Term::intrinsic(curios_core::Intrinsic::BoolType);
    let nat_type = curios_core::Term::intrinsic(curios_core::Intrinsic::NatType);
    let top = curios_core::Term::induct_type(
        nominal("Top"),
        Vec::<curios_core::Term>::new(),
        Vec::<curios_core::Term>::new(),
    );
    let subject = curios_core::Term::nat_match(
        nat_lit(200_000),
        None,
        bool_type.clone(),
        curios_core::Term::intrinsic(curios_core::Intrinsic::Bool(true)),
        &pred,
        &ih,
        curios_core::Term::free_var(&ih),
    );
    let bound = curios_core::Term::apply(curios_core::Term::free_var(&global("H")), [subject]);
    let qed = curios_core::Term::variant(
        nominal("Top"),
        Vec::<curios_core::Term>::new(),
        curios_core::Atom::from("qed"),
        Vec::<curios_core::Term>::new(),
    );
    let items = vec![
        definition(
            "H",
            curios_core::Term::func_type([(b, bool_type.clone())], curios_core::Term::prop()),
            curios_core::Term::func(
                [(b, bool_type.clone())],
                curios_core::Term::bool_match(
                    curios_core::Term::free_var(&b),
                    None,
                    curios_core::Term::prop(),
                    top.clone(),
                    top,
                ),
            ),
        ),
        definition(
            "need",
            curios_core::Term::func_type([(p, bound.clone())], nat_type.clone()),
            curios_core::Term::func([(p, bound.clone())], nat_lit(1)),
        ),
        definition(
            "head",
            nat_type,
            curios_core::Term::apply(curios_core::Term::free_var(&global("need")), [qed]),
        ),
    ];
    let mut induct_decls = BTreeMap::new();
    induct_decls.insert(nominal("Top"), top_induct());
    let unit = zonked(&curios_core::test_support::declaring(
        module(items, nat_lit(0)).module,
        induct_decls,
        [],
        [],
    ));
    let globals = Globals::default();
    let (rechecked, mut kernel) = certify_module(&unit, DEFAULT_BUDGET, &globals, SYNTAX);
    assert!(rechecked.verdicts.is_empty(), "{:?}", rechecked.verdicts);

    erase_unit(&mut kernel, Erasures::nothing(), &unit).expect("the unit erases");
    // `head` is the last item erased, on a budget restored for it.
    let erasing = kernel.read(|reader| reader.consumed().units());
    let reducing = kernel.read(|reader| {
        reader.restore_budget();
        crate::reduced(reader, &bound).expect("the bound reduces");
        reader.consumed().units()
    });

    assert!(
        erasing * 10 < reducing,
        "erasing `head` charged {erasing} units, reducing its bound {reducing}"
    );
}

// induct Top : Prop | qed() end — a proposition holding nothing, which every arm of the bound above lands in.
fn top_induct() -> curios_core::InductDecl {
    curios_core::InductDecl {
        universe_context: curios_core::UniverseContext::empty(),
        arity: curios_core::Telescope::done(curios_core::Telescope::done(())),
        constructors: Vec::from([(
            curios_core::Atom::from("qed"),
            curios_core::InductParam::new(curios_core::Telescope::done(Vec::new()), vec![]),
        )])
        .into(),
        result_sort: curios_core::Term::prop(),
        module: Qualifier::empty(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    }
}

/// The erased program `items` and a `need` whose one parameter is at `slot` make, its entry handing `need` the proof `witness`, with what the erased call hands it there: the stand-in of a proof `slot` types. `Top` is declared beside `items`.
fn stand_in(
    items: Vec<curios_core::Item>,
    slot: curios_core::Term,
    witness: curios_core::Term,
) -> (curios_anf::Module, curios_anf::Atom) {
    let f = fresh(Some("f"));
    let nat = curios_core::Term::intrinsic(curios_core::Intrinsic::NatType);
    let mut items = items;
    items.push(definition(
        "need",
        curios_core::Term::func_type([(f, slot.clone())], nat.clone()),
        curios_core::Term::func([(f, slot)], nat_lit(1)),
    ));
    let mut program = module(
        items,
        curios_core::Term::apply(curios_core::Term::free_var(&global("need")), [witness]),
    );
    program.module = curios_core::test_support::declaring(
        program.module,
        [(nominal("Top"), top_induct())],
        [],
        [],
    );
    let erased = erase(&program, nat);
    let handed = bound(&erased)
        .find_map(|rhs| match rhs {
            curios_anf::Rhs::Apply { arguments, .. } => arguments.first().copied(),
            _ => None,
        })
        .expect("the entry calls `need`");

    (erased, handed)
}

/// `(k: Nat) => Top/qed()`, a proof of `(k: Nat) -> Top`, with the family `(k: Nat) => Top` it is a proof of at each `k`.
fn top_everywhere() -> (curios_core::Term, curios_core::Term) {
    let (k, j) = (fresh(Some("k")), fresh(Some("j")));
    let nat = curios_core::Term::intrinsic(curios_core::Intrinsic::NatType);
    let top = curios_core::Term::induct_type(
        nominal("Top"),
        Vec::<curios_core::Term>::new(),
        Vec::<curios_core::Term>::new(),
    );
    let qed = curios_core::Term::variant(
        nominal("Top"),
        Vec::<curios_core::Term>::new(),
        curios_core::Atom::from("qed"),
        Vec::<curios_core::Term>::new(),
    );

    (
        curios_core::Term::func([(k, nat.clone())], qed),
        curios_core::Term::func([(j, nat)], top),
    )
}

/// A slot whose type a definition unfolds to a function type at every argument holds a stand-in a callee can apply: `Each(P)` at a fresh `P` is `(k: Nat) -> P(k)`, which tells nothing against reducing the slot's type whole, so the stand-in is a function of the one parameter that type has.
#[test]
fn a_slot_typed_by_a_definition_of_a_function_type_holds_a_function_stand_in() {
    let (p, k) = (fresh(Some("P")), fresh(Some("k")));
    let nat = curios_core::Term::intrinsic(curios_core::Intrinsic::NatType);
    let family = curios_core::Term::func_type([(k, nat.clone())], curios_core::Term::prop());
    let each = definition(
        "Each",
        curios_core::Term::func_type([(p, family.clone())], curios_core::Term::prop()),
        curios_core::Term::func(
            [(p, family)],
            curios_core::Term::func_type(
                [(k, nat)],
                curios_core::Term::apply(
                    curios_core::Term::free_var(&p),
                    [curios_core::Term::free_var(&k)],
                ),
            ),
        ),
    );
    let (witness, top) = top_everywhere();
    let slot = curios_core::Term::apply(curios_core::Term::free_var(&global("Each")), [top]);

    let (erased, handed) = stand_in(vec![each], slot, witness);
    assert_eq!(arity(&erased, handed), Some(1));
}

/// A slot whose type a definition's match makes a function type in one arm holds a stand-in a callee can apply: `Pick(b, P)` at a fresh `b` is a match on it with `(k: Nat) -> P(k)` in its `true` arm, so it tells nothing, and `Pick(true, P)` reduced whole is that function type.
#[test]
fn a_slot_typed_by_a_match_with_a_function_arm_holds_a_function_stand_in() {
    let (b, p, k) = (fresh(Some("b")), fresh(Some("P")), fresh(Some("k")));
    let nat = curios_core::Term::intrinsic(curios_core::Intrinsic::NatType);
    let bool_type = curios_core::Term::intrinsic(curios_core::Intrinsic::BoolType);
    let family = curios_core::Term::func_type([(k, nat.clone())], curios_core::Term::prop());
    let top = curios_core::Term::induct_type(
        nominal("Top"),
        Vec::<curios_core::Term>::new(),
        Vec::<curios_core::Term>::new(),
    );
    let pick = definition(
        "Pick",
        curios_core::Term::func_type(
            [(b, bool_type.clone()), (p, family.clone())],
            curios_core::Term::prop(),
        ),
        curios_core::Term::func(
            [(b, bool_type), (p, family)],
            curios_core::Term::bool_match(
                curios_core::Term::free_var(&b),
                None,
                curios_core::Term::prop(),
                top,
                curios_core::Term::func_type(
                    [(k, nat)],
                    curios_core::Term::apply(
                        curios_core::Term::free_var(&p),
                        [curios_core::Term::free_var(&k)],
                    ),
                ),
            ),
        ),
    );
    let (witness, top) = top_everywhere();
    let slot = curios_core::Term::apply(
        curios_core::Term::free_var(&global("Pick")),
        [
            curios_core::Term::intrinsic(curios_core::Intrinsic::Bool(true)),
            top,
        ],
    );

    let (erased, handed) = stand_in(vec![pick], slot, witness);
    assert_eq!(arity(&erased, handed), Some(1));
}
