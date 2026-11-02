//! Which of a baseline's verdicts a walk adopts, and which declarations it judges again.

use {
    super::test_support::*,
    crate::*,
    curios_analysis::test_support::*,
    curios_core::{
        Certification, Certified, Definition, Entrypoint, Free, Global, Intrinsic, Item, ItemRun,
        Level, Module, Nat, Places, Program, ReadTable, Reads, Term, Totality, UniverseParam,
        Variance,
    },
    curios_utilities::{Candidates, Digest, Facet, Member, Object, Qualifier, candidate_prefix},
    std::collections::{BTreeMap, BTreeSet},
};

const BUDGET: u64 = 1_000_000;

fn global(symbol: &str) -> Global {
    Global::Authored(Qualifier::from([symbol]))
}

fn nat() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

fn literal(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n)))
}

/// `alias : Type = <alias>`, `three : alias = 3`, `identity : (Nat) -> Nat = (x) => <returned>`, and `five : Nat = identity(5)`: `three` unfolds `alias`, and `five` types `identity` and unfolds nothing.
fn reading_module(alias: Term, returned: impl Fn(&Free) -> Term) -> Module {
    let x = Free::local(10, Some("x"));

    Module {
        mounts: Vec::new(),
        items: vec![
            authored(&global("alias"), Term::type_ground(), alias),
            authored(
                &global("three"),
                Term::free_var(&Free::from(&global("alias"))),
                literal(3),
            ),
            authored(
                &global("identity"),
                Term::func_type([(x, nat())], nat()),
                Term::func([(x, nat())], returned(&x)),
            ),
            authored(
                &global("five"),
                nat(),
                Term::apply(
                    Term::free_var(&Free::from(&global("identity"))),
                    [literal(5)],
                ),
            ),
        ]
        .into(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    }
}

/// The module every fixture here is an edit of: `alias` is `Nat` and `identity` returns its argument.
fn written() -> Module {
    reading_module(nat(), Term::free_var)
}

/// `module` walked with nothing in scope and no baseline: the walk that files a baseline's record.
fn walked(module: &Module) -> Rechecked {
    let rechecked = fixture_certified(module, BUDGET, &Globals::default(), SYNTAX);
    assert_eq!(rechecked.verdicts, Vec::new());

    rechecked
}

/// `module` walked over `baseline` as `certification` records it.
fn walked_over(module: &Module, baseline: &Module, certification: &Certification) -> Rechecked {
    walked_among(module, baseline, certification, None)
}

/// [`walked_over`], taking what it can from what `store` holds.
fn walked_among(
    module: &Module,
    baseline: &Module,
    certification: &Certification,
    store: Option<&dyn Candidates>,
) -> Rechecked {
    let (places, now) = (Places::of(baseline), Places::of(module));
    let globals = Globals::default();

    super::verdicts_into(
        &mut Kernel::new(BUDGET, SYNTAX),
        module,
        None,
        &globals,
        Some(super::Over {
            baseline: Baseline {
                module: baseline,
                places: &places,
                certification,
            },
            places: &now,
            candidates: store,
        }),
    )
}

/// `certification` with `symbol`'s entry replaced by `entry`'s answer for it.
fn doctored(
    certification: &Certification,
    symbol: &str,
    entry: impl Fn(Certified) -> Certified,
) -> Certification {
    Certification::of(
        certification.iter().map(|(name, totality)| {
            let certified = Certified {
                totality,
                reads: certification
                    .reads(name)
                    .cloned()
                    .expect("a record holds the reads of every definition it classifies"),
            };

            match *name == global(symbol) {
                true => (*name, entry(certified)),
                false => (*name, certified),
            }
        }),
        [],
        [],
        [],
    )
}

/// A record of `written` in which `five` is marked: its reads hold `alias`'s signature, which no judgment of `five` reads. A walk that hands the mark back adopted the entry, and one that judged `five` filed its own reads in its place.
fn marked() -> Certification {
    doctored(&walked(&written()).certification, "five", |certified| {
        Certified {
            reads: Reads {
                signatures: BTreeSet::from([global("identity"), global("alias")]),
                bodies: BTreeSet::new(),
            },
            ..certified
        }
    })
}

/// The fixture the trusted base is held by: a dependency's body changes, its dependent is presented as it was, and the walk judges the dependent again because its own record says it unfolded that body. `three : alias = 3` stood while `alias` was `Nat` and is refused once `alias` is `Bool`, whatever the caller took to be unchanged.
#[test]
fn a_declaration_presented_unchanged_is_judged_again_where_what_it_read_has_changed() {
    let baseline = written();
    let certification = walked(&baseline).certification;
    let edited = reading_module(Term::intrinsic(Intrinsic::BoolType), Term::free_var);

    let rechecked = walked_over(&edited, &baseline, &certification);

    assert_eq!(
        rechecked
            .verdicts
            .iter()
            .map(|verdict| verdict.name)
            .collect::<Vec<_>>(),
        vec![Some(global("three"))],
    );
}

/// `module` with its items held as a version's: a module made of it by replacing an item shares every other item with it, as a recompile's module shares the ones it kept with its baseline's.
fn versioned(module: Module) -> Module {
    Module {
        items: module.items.version(),
        ..module
    }
}

/// `module`, a version, with the item declaring `name` replaced by `item`.
fn replaced(module: &Module, name: &str, item: Item) -> Module {
    let place = Places::of(module)
        .place(&global(name))
        .expect("the module declares the name")
        .item();
    let mut items = module.items.clone();
    items.replace(place, item);

    Module {
        items,
        ..module.clone()
    }
}

/// Over a baseline the walked module is a version of, the walk compares the two by what they do not share, judges the item that changed and asks what read it, and files the record a walk over every declaration of the module files: `identity` returns `7`.
#[test]
fn a_walk_over_a_version_files_what_a_walk_over_every_declaration_files() {
    let baseline = versioned(written());
    let certification = walked(&baseline).certification;
    let x = Free::local(10, Some("x"));
    let identity = authored(
        &global("identity"),
        Term::func_type([(x, nat())], nat()),
        Term::func([(x, nat())], literal(7)),
    );

    let rechecked = walked_over(
        &replaced(&baseline, "identity", identity),
        &baseline,
        &certification,
    );

    assert_eq!(rechecked.verdicts, Vec::new());
    assert_eq!(
        rechecked.certification,
        walked(&reading_module(nat(), |_| literal(7))).certification
    );
}

/// A declaration a recompile writes under a name a unit in scope declares is refused for the name over a baseline, as over every declaration, though the unit in scope is the one the baseline's walk read: `three` respelled as the environment's `shadowed`, the baseline's other declarations shared.
///
/// Mutation-checked: with a changed item never asked whether a unit in scope declares its name, the module is judged and `shadowed` refused where the walk writes it, twice.
#[test]
fn a_name_a_unit_in_scope_declares_written_over_a_baseline_is_refused_for_it() {
    let environment = already_judged(&judged_environment());
    let globals = environment.globals();
    let baseline = versioned(written());
    let certification = fixture_certified(&baseline, BUDGET, &globals, SYNTAX).certification;
    let edited = replaced(
        &baseline,
        "three",
        authored(&shadowed_name(), nat(), literal(3)),
    );
    let (places, now) = (Places::of(&baseline), Places::of(&edited));

    let rechecked = super::verdicts_into(
        &mut Kernel::new(BUDGET, SYNTAX),
        &edited,
        None,
        &globals,
        Some(super::Over {
            baseline: Baseline {
                module: &baseline,
                places: &places,
                certification: &certification,
            },
            places: &now,
            candidates: None,
        }),
    );

    assert!(
        matches!(
            rechecked.verdicts.as_slice(),
            [Verdict { name: Some(name), error: Error::Redeclared(_) }] if *name == shadowed_name()
        ),
        "a name the environment declares was written over a baseline: {:?}",
        rechecked.verdicts
    );
}

/// A scheme's constraint strengthened under the same type and body reaches the declaration that instantiated it, as a walk over every declaration does: `A<u, v | u + 2 <= v>` in place of `u + 1 <= v`, beside `user<u, v | u + 1 <= v> : Type v = A<u, v>`, whose instance discharged the constraint `A` stated and not the one it states now.
///
/// Mutation-checked: with a signature read digested without its scheme's constraints, `user` is adopted and the walk refuses nothing.
#[test]
fn a_schemes_constraint_strengthened_reaches_the_declaration_that_instantiated_it() {
    let (_, instance) = scheme_occurrences();
    let baseline = versioned(universe_scheme_module(Some((scheme_context(), instance))));
    let certification = walked(&baseline).certification;
    let place = Places::of(&baseline)
        .place(&global("A"))
        .expect("the module declares the scheme")
        .item();
    let Item::Let(scheme) = &baseline.items[place] else {
        panic!("the scheme is a definition");
    };
    let mut stronger = scheme_context();
    stronger.constraints[0].lower = Level::param(UniverseParam(0))
        .checked_add(2)
        .expect("level admits the offset");
    let edited = replaced(
        &baseline,
        "A",
        Item::Let(Box::new(Definition {
            universe_context: stronger,
            ..(**scheme).clone()
        })),
    );

    let whole = fixture_certified(&edited, BUDGET, &Globals::default(), SYNTAX);
    let over = walked_over(&edited, &baseline, &certification);

    assert!(
        whole
            .verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("user"))
                && matches!(verdict.error, Error::UniverseInstance { .. })),
        "the control stopped refusing: `user`'s instance does not discharge `u + 2 <= v`: {:?}",
        whole.verdicts
    );
    assert_eq!(over.verdicts, whole.verdicts);
}

/// A declaration the walk does not visit is read once the walk has passed its place: `alias` respelled to name `five`, stored after it, finds it unbound, as a walk over every declaration does, though nothing `five` read moved and the walk adopts `five` without visiting it.
///
/// Mutation-checked: with a declaration the walk does not visit read wherever it stands, `alias` finds `five` and is refused for its type instead.
#[test]
fn a_declaration_the_walk_does_not_visit_is_unbound_before_its_place() {
    let baseline = versioned(written());
    let certification = walked(&baseline).certification;
    let edited = replaced(
        &baseline,
        "alias",
        authored(
            &global("alias"),
            Term::type_ground(),
            Term::free_var(&Free::from(&global("five"))),
        ),
    );

    let whole = fixture_certified(&edited, BUDGET, &Globals::default(), SYNTAX);
    let over = walked_over(&edited, &baseline, &certification);

    assert!(
        whole
            .verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::Unbound(_))),
        "a walk over every declaration finds `five` unbound at `alias`: {:?}",
        whole.verdicts
    );
    assert_eq!(over.verdicts, whole.verdicts);
}

/// A module that is the baseline's is adopted whole: every entry comes back as the baseline's record holds it, the mark with it.
#[test]
fn a_declaration_whose_terms_and_reads_stand_takes_the_baselines_verdict() {
    let baseline = written();
    let certification = marked();

    let rechecked = walked_over(&baseline, &baseline, &certification);

    assert_eq!(rechecked.verdicts, Vec::new());
    assert_eq!(rechecked.certification, certification);
}

/// A signature read stands through an edit to the body behind it: `identity` returns `7` where it returned its argument, at the type it had, so it is judged and `five`, which typed it and unfolded nothing, is adopted.
#[test]
fn a_signature_read_stands_through_an_edit_to_the_body_behind_it() {
    let baseline = written();
    let certification = marked();
    let edited = reading_module(nat(), |_| literal(7));

    let rechecked = walked_over(&edited, &baseline, &certification);

    assert_eq!(rechecked.verdicts, Vec::new());
    assert_eq!(
        rechecked.certification.reads(&global("five")),
        certification.reads(&global("five")),
    );
    assert_ne!(
        rechecked.certification.reads(&global("identity")),
        None,
        "the judged declaration is in the record beside the adopted ones",
    );
}

/// `base : (Nat) -> Nat`, total as `(n) => n` or partial as [`diverging_sink`]'s body, then `middle : Nat = base(1)`, `top : Nat = middle` and `apart : Nat = 7`, the two readers stamped as `base` is: `top` reads `middle` and nothing of `base`.
fn chained_module(partial: bool) -> Module {
    let n = Free::local(10, Some("n"));
    let stamped = |name: &str, type_: Term, body: Term| match partial {
        true => authored_partial(&global(name), type_, body),
        false => authored(&global(name), type_, body),
    };
    let base = match (partial, diverging_sink()) {
        (true, Item::Let(sink)) => stamped("base", sink.type_, sink.body),
        _ => stamped(
            "base",
            Term::func_type([(n, nat())], nat()),
            Term::func([(n, nat())], Term::free_var(&n)),
        ),
    };

    Module {
        mounts: Vec::new(),
        items: vec![
            base,
            stamped(
                "middle",
                nat(),
                Term::apply(Term::free_var(&Free::from(&global("base"))), [literal(1)]),
            ),
            stamped("top", nat(), Term::free_var(&Free::from(&global("middle")))),
            authored(&global("apart"), nat(), literal(7)),
        ]
        .into(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    }
}

/// A verdict that moves reaches what read it through a declaration whose terms stand: `base` made partial is judged, `middle`, which read its signature, is judged and comes out partial, and so does `top`, which read `middle` alone and whose terms are the baseline's. `apart` reads nothing that moved and keeps the baseline's verdict.
///
/// Mutation-checked: with the declarations that read a moved one marked and theirs not, `top` keeps the baseline's `Total`.
#[test]
fn a_verdict_that_moves_reaches_what_read_it_through_a_declaration_whose_terms_stand() {
    let baseline = chained_module(false);
    let certification = walked(&baseline).certification;
    let edited = chained_module(true);

    let rechecked = walked_over(&edited, &baseline, &certification);

    assert_eq!(rechecked.verdicts, Vec::new());
    for name in ["base", "middle", "top"] {
        assert_eq!(
            rechecked.certification.totality(&global(name)),
            Some(Totality::Partial),
            "{name}",
        );
    }
    assert_eq!(
        rechecked.certification.totality(&global("apart")),
        Some(Totality::Total),
    );
}

/// `alias` respelled: an application that reduces to `Nat`.
fn applied_nat() -> Term {
    let t = Free::local(11, Some("t"));

    Term::apply(
        Term::func([(t, Term::type_ground())], Term::free_var(&t)),
        [nat()],
    )
}

/// A body read falls with the body: `three` unfolded `alias`, so an `alias` respelled as an application that reduces to `Nat` has `three` judged again. The baseline's record calls `three` partial, which an adopted `three` stamped total would be refused over; judged, it is total and the record says so.
#[test]
fn a_body_read_falls_with_an_edit_to_the_body() {
    let baseline = written();
    let certification = doctored(&walked(&baseline).certification, "three", |certified| {
        Certified {
            totality: Totality::Partial,
            ..certified
        }
    });
    let edited = reading_module(applied_nat(), Term::free_var);

    let rechecked = walked_over(&edited, &baseline, &certification);

    assert_eq!(rechecked.verdicts, Vec::new());
    assert_eq!(
        rechecked.certification.totality(&global("three")),
        Some(Totality::Total),
    );
}

/// An adopted verdict is the baseline's, partiality included, and the stamp this text carries is held to it as a judged item's is: a record that calls `five` partial refuses a `five` stamped total, and files it partial.
#[test]
fn a_stamp_that_says_total_over_an_adopted_partial_verdict_is_refused() {
    let baseline = written();
    let certification = doctored(&walked(&baseline).certification, "five", |certified| {
        Certified {
            totality: Totality::Partial,
            ..certified
        }
    });

    let rechecked = walked_over(&baseline, &baseline, &certification);

    assert!(matches!(
        rechecked.verdicts.as_slice(),
        [verdict] if verdict.name == Some(global("five"))
            && matches!(verdict.error, Error::NotTotal { .. })
    ));
    assert_eq!(
        rechecked.certification.totality(&global("five")),
        Some(Totality::Partial),
    );
}

/// A registry entry is the baseline's by what the certifier reads of it, its terms. Elaboration's stamps take no part — the polarity vector and the variance vector, in whose place a walk holds the ones the certifier computed, the owning module and the representation's privacy — so an entry differing from the baseline's in those alone is the baseline's, and one differing in its variance vector or in a field's type is not.
///
/// Mutation-checked against each comparison: taken over the whole entry, the stamped pair is another declaration, and so is the family carrying another variance vector; and with the arity left out the record is the baseline's.
#[test]
fn a_registry_entry_is_the_baselines_by_what_the_certifier_reads_of_it() {
    let held = two_registry_entries(Varied::Nothing);
    let family = |module: &Module| (*module.induct_decls()[&shadowed_family()]).clone();
    let record = |module: &Module| (*module.struct_decls()[&kept_record()]).clone();

    let stamped = two_registry_entries(Varied::Stamps);
    assert!(
        induct_alike(&family(&stamped), &family(&held))
            && struct_alike(&record(&stamped), &record(&held)),
        "an entry differing from the baseline's in elaboration's stamps alone is another declaration",
    );
    assert!(
        induct_alike(
            &family(&two_registry_entries(Varied::Variance)),
            &family(&held)
        ),
        "a family differing from the baseline's in the variance vector it carries alone is another declaration",
    );
    assert!(
        !struct_alike(
            &record(&two_registry_entries(Varied::Field)),
            &record(&held)
        ),
        "a record differing from the baseline's in a field's type is the baseline's",
    );
}

/// `Vouched`, the proposition the lemma below states.
fn vouched() -> Term {
    Term::induct_type(
        vouched_declaration().0,
        Vec::<Term>::new(),
        Vec::<Term>::new(),
    )
}

/// `Vouched/qed(())`, its proof.
fn qed() -> Term {
    Term::variant(
        vouched_declaration().0,
        Vec::<Term>::new(),
        "qed",
        [Term::tuple(Vec::<Term>::new())],
    )
}

/// `lemma : Vouched = <proof>`, marked a proof where `marked`, `spare : Vouched = qed(())`, and `corollary : Vouched = lemma`.
fn proving_module(proof: Term, marked: bool) -> Module {
    let (vouched_name, declaration) = vouched_declaration();
    let lemma = match marked {
        true => authored_proof(&global("lemma"), vouched(), proof),
        false => authored(&global("lemma"), vouched(), proof),
    };

    collision_module(
        vec![
            lemma,
            authored(&global("spare"), vouched(), qed()),
            authored(
                &global("corollary"),
                vouched(),
                Term::free_var(&Free::from(&global("lemma"))),
            ),
        ],
        BTreeMap::from([(vouched_name, declaration)]),
    )
}

/// `qed(())` reached through a redex: another proof of the same statement.
fn proved_otherwise() -> Term {
    let u = Free::local(12, Some("u"));

    Term::apply(Term::func([(u, vouched())], Term::free_var(&u)), [qed()])
}

/// A record of `module` in which `corollary` asked for `lemma`'s body, marked as [`marked`] marks `five`: its reads hold `spare`'s signature, which no judgment of `corollary` reads, so a walk that hands the entry back adopted it.
fn asking(module: &Module) -> Certification {
    doctored(&walked(module).certification, "corollary", |certified| {
        Certified {
            reads: Reads {
                signatures: BTreeSet::from([global("lemma"), global("spare")]),
                bodies: BTreeSet::from([global("lemma")]),
            },
            ..certified
        }
    })
}

/// A proof unfolds to nothing whatever its body, so an edit to one moves no read of it: `lemma` proved another way is judged, and `corollary`, whose record says it asked for `lemma`'s body, takes the baseline's verdict.
#[test]
fn a_read_of_a_proofs_body_stands_through_an_edit_to_it() {
    let baseline = proving_module(qed(), true);
    let certification = asking(&baseline);
    let edited = proving_module(proved_otherwise(), true);

    let rechecked = walked_over(&edited, &baseline, &certification);

    assert_eq!(rechecked.verdicts, Vec::new());
    assert_eq!(
        rechecked.certification.reads(&global("corollary")),
        certification.reads(&global("corollary")),
    );
}

/// The control: left unmarked, the same statement unfolds to its body, and the same edit has `corollary` judged again.
#[test]
fn a_read_of_an_unmarked_proofs_body_falls_with_an_edit_to_it() {
    let baseline = proving_module(qed(), false);
    let certification = asking(&baseline);
    let edited = proving_module(proved_otherwise(), false);

    let rechecked = walked_over(&edited, &baseline, &certification);

    assert_eq!(rechecked.verdicts, Vec::new());
    assert_ne!(
        rechecked.certification.reads(&global("corollary")),
        certification.reads(&global("corollary")),
    );
}

/// What a name unfolds to moves with its mark though no term does: a `lemma` that starts or stops being a proof is no longer the baseline's, and `corollary`, which asked for its body, is judged again.
#[test]
fn a_read_of_a_body_falls_where_its_definition_starts_or_stops_being_a_proof() {
    for marked in [true, false] {
        let baseline = proving_module(qed(), marked);
        let certification = asking(&baseline);
        let edited = proving_module(qed(), !marked);

        let rechecked = walked_over(&edited, &baseline, &certification);

        assert_eq!(rechecked.verdicts, Vec::new());
        assert_ne!(
            rechecked.certification.reads(&global("corollary")),
            certification.reads(&global("corollary")),
            "marked in the baseline: {marked}",
        );
    }
}

/// What a store holds of the walks filed into it, in memory: each record a member of its walk's run, under the name its filing completed it with.
struct Shelf(Vec<Object>);

impl Shelf {
    /// A store holding `certification`, a walk of `module`, its records completed as a filing completes them.
    fn filed(module: &Module, certification: &Certification) -> Self {
        complete(module, &Places::of(module), certification);
        let records = certification.records().collect::<Vec<_>>();
        let run = ReadTable::run(
            records
                .iter()
                .map(|&(name, record)| (name, &***record, record.made())),
        )
        .expect("a run archives");

        Shelf(
            records
                .iter()
                .zip(0..)
                .map(|(&(name, record), index)| Object {
                    digest: record.digest(name).expect("a record archives"),
                    member: Some(Member {
                        kind: ReadTable::KIND,
                        index,
                    }),
                    ..Object::of(run.clone())
                })
                .collect(),
        )
    }
}

impl Candidates for Shelf {
    fn candidates(&self, facet: Facet, input: &Digest) -> Vec<Object> {
        let prefix = candidate_prefix(facet, input);
        self.0
            .iter()
            .filter(|object| object.digest.as_bytes().starts_with(&prefix))
            .cloned()
            .collect()
    }

    fn object(&self, digest: &Digest) -> Option<Object> {
        self.0
            .iter()
            .find(|object| object.digest == *digest)
            .cloned()
    }
}

/// `written` with `five : Nat = identity(6)`: the edit both walks below are over.
fn edited_five() -> Module {
    let five = authored(
        &global("five"),
        nat(),
        Term::apply(
            Term::free_var(&Free::from(&global("identity"))),
            [literal(6)],
        ),
    );

    replaced(&versioned(written()), "five", five)
}

/// A declaration the walk would judge takes a verdict a store holds of its item where every part that verdict read digests as it did: `five` was written, edited and written back, so the baseline's `five` is another item, and the store's record of the first is taken, the mark with it.
#[test]
fn a_declaration_to_judge_takes_a_stores_verdict_where_every_part_it_read_stands() {
    let first = marked();
    let store = Shelf::filed(&written(), &first);
    let baseline = edited_five();
    let certification = walked(&baseline).certification;

    let rechecked = walked_among(&written(), &baseline, &certification, Some(&store));

    assert_eq!(rechecked.verdicts, Vec::new());
    assert_eq!(rechecked.certification, first);
}

/// A store's verdict whose reads moved is passed over: the record of `five` says it unfolded `alias`, which is respelled as an application that reduces to `Nat`, so `five`, written back, is judged and files its own reads, as a walk over every declaration does.
#[test]
fn a_stores_verdict_that_read_a_part_since_moved_is_passed_over() {
    let unfolding = doctored(&walked(&written()).certification, "five", |certified| {
        Certified {
            reads: Reads {
                signatures: BTreeSet::from([global("identity")]),
                bodies: BTreeSet::from([global("alias")]),
            },
            ..certified
        }
    });
    let store = Shelf::filed(&written(), &unfolding);
    let baseline = edited_five();
    let certification = walked(&baseline).certification;
    let respelled = reading_module(applied_nat(), Term::free_var);

    let rechecked = walked_among(&respelled, &baseline, &certification, Some(&store));

    let place = Places::of(&respelled)
        .place(&global("five"))
        .expect("the module declares the name")
        .item();
    let identity = ItemRun::identity(&respelled.items[place]).expect("an item archives");
    assert_eq!(
        store.candidates(Facet::Judgment, &identity).len(),
        1,
        "the store holds a verdict of the item",
    );
    assert_eq!(rechecked.verdicts, Vec::new());
    assert_eq!(rechecked.certification, walked(&respelled).certification);
}

/// `Type 5`, `Type 4` and `Type 0` as hand-built universes.
fn type_at(level: u32) -> Term {
    Term::type_at(Level::constant(level))
}

/// `name : type_ = body`, stating the sort `value_sort` its value inhabits ([`curios_core::Definition::value_sort`]): the field a type alias carries and the one `read_digest` leaves out.
fn aliased(name: &Global, type_: Term, body: Term, value_sort: Term) -> Item {
    match authored(name, type_, body) {
        Item::Let(definition) => Item::Let(Box::new(Definition {
            value_sort: Some(value_sort),
            ..*definition
        })),
        item => item,
    }
}

/// Three declarations: `H : Type 5 = h_body` stating its value sort `h_value_sort`, `G : Type 5 = H` stating `g_value_sort`, and `D : (G) -> G = (x) => x`, which reads `G`'s value sort through `Sort::of` when it checks its binder.
///
/// `G`'s declared type is `Type 5` in every version, so cumulativity admits an `H` body at any level up to 5; what moves between versions is the *value* sort `H`'s body reaches, which `G` republishes.
fn value_sort_module(h_body: Term, h_value_sort: Term, g_value_sort: Term) -> Module {
    let x = Free::local(10, Some("x"));
    let g = Term::free_var(&Free::from(&global("G")));

    Module {
        mounts: Vec::new(),
        items: vec![
            aliased(&global("H"), type_at(5), h_body, h_value_sort),
            aliased(
                &global("G"),
                type_at(5),
                Term::free_var(&Free::from(&global("H"))),
                g_value_sort,
            ),
            authored(
                &global("D"),
                Term::func_type([(x, g.clone())], g.clone()),
                Term::func([(x, g.clone())], Term::free_var(&x)),
            ),
        ]
        .into(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    }
}

/// A reader of a moved `value_sort` is judged again rather than adopted, because the certifier's read-digest ([`held_digest`]) holds the sort a definition states for its value beside its type.
///
/// `H : Type 5` moves from `Nat` (value sort `Type 0`) to `Type 4` (value sort `Type 5`) under a fixed declared type, so the alias `G : Type 5 = H` republishes a value sort that moves `Type 0` → `Type 5` while `G`'s body term, the occurrence of `H`, does not. `D : (G) -> G` reads `G`'s value sort through `Sort::of`, so its verdict is a function of a field the digest now covers and the walk asks the reads of it again.
///
/// The first assertion pins the digest directly: two modules whose `G` differs only in `value_sort` digest it apart, signature read and body read alike. The three that follow are what a reader judged again must agree with — the verdict and the record of a whole compile of the same module.
///
/// Mutation-checked: dropping `self.held.value_sort` from both parts of `Readable::digest` makes the two `G` digests equal, and fails this test and [`a_reader_of_a_moved_value_sorts_level_is_refused_as_a_whole_compile_refuses_it`] and no other.
#[test]
fn a_reader_of_a_moved_value_sort_is_judged_again() {
    // The field, read off the digest: `G`'s part digests apart where its value sort is `Type 0` and where it is `Type 5`.
    let low = value_sort_module(type_at(4), type_at(5), type_at(0));
    let high = value_sort_module(type_at(4), type_at(5), type_at(5));
    let (low_places, high_places) = (Places::of(&low), Places::of(&high));
    for body in [false, true] {
        assert_ne!(
            held_digest(&low, &low_places, &global("G"), (false, None, None), body)
                .expect("G digests"),
            held_digest(&high, &high_places, &global("G"), (false, None, None), body)
                .expect("G digests"),
            "the read-digest of G holds its value_sort (body read: {body})",
        );
    }

    // The walk: `G`'s republished value sort moves `Type 0` → `Type 5`, `D` reads it, and the edited module is compiled over the baseline.
    let baseline = versioned(value_sort_module(nat(), type_at(0), type_at(0)));
    let certification = walked(&baseline).certification;
    let edited = replaced(
        &replaced(
            &baseline,
            "H",
            aliased(&global("H"), type_at(5), type_at(4), type_at(5)),
        ),
        "G",
        aliased(
            &global("G"),
            type_at(5),
            Term::free_var(&Free::from(&global("H"))),
            type_at(5),
        ),
    );

    let whole = walked(&value_sort_module(type_at(4), type_at(5), type_at(5)));
    let over = walked_over(&edited, &baseline, &certification);

    assert_eq!(
        over.verdicts,
        Vec::new(),
        "the edited module is admitted over the baseline, D judged again at G's republished value sort",
    );
    assert_eq!(
        over.verdicts, whole.verdicts,
        "a whole compile of the edited module gives D the same verdict",
    );
    assert_eq!(
        over.certification, whole.certification,
        "and files the record a whole compile files",
    );
}

/// A forged `Certification` filing a genuinely partial proof as `Total`, mounted as a unit in scope, admits a looping proof of an empty proposition: the mounted record is believed without the walk that would refuse it.
///
/// `loop : Absurd = loop` recurses without descending, so a walk refuses it as a proof that may not terminate ([`totality-of-the-erased-program`]). Filed `Total` and mounted, nothing re-judges it: `Globals::mount` stands a scope entry at its type with its body, unjudged, and the reader's totality obligation reads the forged verdict (`Globals::partial`). The control is the same record filing it `Partial`, which refuses.
///
/// The input is forged — no walk of this crate produces this record — so this names what the boundary's ground, that a scope entry was judged by the walk that built it, does not itself check: the mounted `Certification`'s totality is believed, and nothing in the record (no digest, no checksum) is consulted to tell a forged verdict from one a walk filed. A stored unit's slot is guarded by the compiler digest and the verified read-set, which bind the record to this compiler over these inputs, not to a walk having concluded it.
#[test]
fn a_forged_total_verdict_mounted_in_scope_admits_a_looping_proof() {
    let loop_name = global("loop");
    let absurd_name = global("Absurd");
    let absurd = Term::induct_type(absurd_name, Vec::<Term>::new(), Vec::<Term>::new());
    let mounted = collision_module(
        vec![authored_proof(
            &loop_name,
            absurd.clone(),
            Term::free_var(&Free::from(&loop_name)),
        )],
        BTreeMap::from([(absurd_name, proposition(Vec::new()))]),
    );
    let reader = || Program {
        module: Module {
            mounts: Vec::new(),
            items: Vec::new().into(),
            witnesses: BTreeSet::new(),
            tests: Vec::new(),
        },
        entry: Entrypoint {
            body: Term::free_var(&Free::from(&loop_name)),
            type_: Some(absurd.clone()),
        },
    };
    let certified = |totality: Totality| {
        Certification::of(
            mounted
                .items
                .iter()
                .flat_map(Item::declared_names)
                .map(|name| {
                    (
                        *name,
                        Certified {
                            totality,
                            reads: Reads::default(),
                        },
                    )
                }),
            [],
            [],
            [],
        )
    };

    let forged = certified(Totality::Total);
    let scope = AlreadyJudged::with(&mounted, &forged);
    let admitted = fixture_certified(&reader(), BUDGET, &scope.globals(), SYNTAX).verdicts;
    assert_eq!(
        admitted,
        Vec::new(),
        "a forged Total verdict admitted a looping proof of Absurd: {admitted:?}",
    );

    // The control: the same record, honest, refuses — so the admission rode on the lie.
    let honest = certified(Totality::Partial);
    let honest_scope = AlreadyJudged::with(&mounted, &honest);
    let refused = fixture_certified(&reader(), BUDGET, &honest_scope.globals(), SYNTAX).verdicts;
    assert!(
        refused
            .iter()
            .any(|verdict| matches!(verdict.error, Error::NotTotal { .. })),
        "the control stopped refusing: an honest Partial must refuse the looping proof: {refused:?}",
    );
}

/// Three aliases in a chain, each stating the sort its value inhabits: `H : Type 5 = h_body` stating `h`, `K : Type 5 = H` stating `k`, and `L : Type 5 = K` stating `l`.
///
/// `hold_value_sort` checks `L`'s stated sort against `Sort::of(K)`, which reads `K`'s *published* sort with nothing run, so `L`'s verdict is a function of `K`'s `value_sort` and of nothing else about `K`.
fn chained_sort_module(h_body: Term, h: Term, k: Term, l: Term) -> Module {
    Module {
        mounts: Vec::new(),
        items: vec![
            aliased(&global("H"), type_at(5), h_body, h),
            aliased(
                &global("K"),
                type_at(5),
                Term::free_var(&Free::from(&global("H"))),
                k,
            ),
            aliased(
                &global("L"),
                type_at(5),
                Term::free_var(&Free::from(&global("K"))),
                l,
            ),
        ]
        .into(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    }
}

/// A level alone does gate a verdict — `hold_value_sort` refuses a stated sort the value does not inhabit, reading the value's sort off a published one — so a walk over a baseline asks that reader's reads again and refuses it where a whole compile refuses it.
#[test]
fn a_reader_of_a_moved_value_sorts_level_is_refused_as_a_whole_compile_refuses_it() {
    let baseline = versioned(chained_sort_module(
        nat(),
        type_at(0),
        type_at(0),
        type_at(0),
    ));
    let certification = walked(&baseline).certification;
    // `H`'s body moves `Nat` -> `Type 4`, so its value sort moves `Type 0` -> `Type 5`, and `K` republishes it. `L` is untouched: its terms, `K`'s type and `K`'s body term all stand.
    let edited = replaced(
        &replaced(
            &baseline,
            "H",
            aliased(&global("H"), type_at(5), type_at(4), type_at(5)),
        ),
        "K",
        aliased(
            &global("K"),
            type_at(5),
            Term::free_var(&Free::from(&global("H"))),
            type_at(5),
        ),
    );

    let whole = walked_whole(&edited);
    assert!(
        whole
            .verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("L"))
                && matches!(verdict.error, Error::ValueSort { .. })),
        "a whole compile of the edited module refuses L for the sort it states: {:?}",
        whole.verdicts,
    );

    let over = walked_over(&edited, &baseline, &certification);
    assert!(
        over.verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("L"))
                && matches!(verdict.error, Error::ValueSort { .. })),
        "the walk over the baseline refuses L for the same reason: {:?}",
        over.verdicts,
    );
}

/// [`walked`] without its assertion, for a module a whole walk refuses.
fn walked_whole(module: &Module) -> Rechecked {
    fixture_certified(module, BUDGET, &Globals::default(), SYNTAX)
}

/// `M : Type 5 = L` stating `sort`: an alias of the warm unit's lying publication, judged by a whole walk with that unit in scope.
fn later_alias(sort: Term) -> Module {
    Module {
        mounts: Vec::new(),
        items: vec![aliased(
            &global("M"),
            type_at(5),
            Term::free_var(&Free::from(&global("L"))),
            sort,
        )]
        .into(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    }
}

/// A later walk reads the sort a definition states for its value with nothing run, so it refuses an alias publishing a sort that value does not inhabit: `M : Type 5 = L` stating `Type 0`, where `L` unfolds to a type at `Type 5`.
///
/// No walk files a unit whose own `L` states a sort its value denies — [`a_reader_of_a_moved_value_sorts_level_is_refused_as_a_whole_compile_refuses_it`] holds that — so the unit in scope here is the honest one, and what is under test is the reading rather than the record.
#[test]
fn a_later_walk_refuses_an_alias_publishing_a_sort_its_value_does_not_inhabit() {
    let honest = chained_sort_module(type_at(4), type_at(5), type_at(5), type_at(5));
    let record = walked(&honest).certification;
    let scope = AlreadyJudged::with(&honest, &record);
    let verdicts =
        |stated| fixture_certified(&later_alias(stated), BUDGET, &scope.globals(), SYNTAX).verdicts;

    let refused = verdicts(type_at(0));
    assert!(
        refused
            .iter()
            .any(|verdict| matches!(verdict.error, Error::ValueSort { .. })),
        "M publishing Type 0 for a value whose sort is Type 5: {refused:?}",
    );

    // The control: the same alias publishing the sort its value inhabits stands, so what refuses the first is the sort and not the shape.
    assert_eq!(verdicts(type_at(5)), Vec::new());
}

/// `Big : (T: Type 5) -> Type 6` with one constructor aiming at `L`, and `f(X: Type 0, b: Big(X)) -> X = match b at X | big() => Type 3 end`.
///
/// The arm's index inversion would refine `X`, an outer binder assumed at `Type 0`, to `L`, and keeps the refinement only where the judge holds `Sort::of(L)` below `Type 0` (`curios_analysis::invert`'s `fits_its_variable`). Kept, the arm's goal is `L`, which `Type 3` inhabits; dropped, the goal stays the abstract `X` and nothing does.
fn refining_module(body: Term) -> Module {
    refining_module_at(Term::free_var(&Free::from(&global("L"))), body)
}

/// [`refining_module_at`] with the variable assumed at `Type 0`.
fn refining_module_at(target: Term, body: Term) -> Module {
    refining_module_assumed(target, type_at(0), body)
}

/// [`refining_module`] with `target` as the index the constructor aims at and `assumed` as the sort the outer variable is assumed at.
fn refining_module_assumed(target: Term, assumed: Term, body: Term) -> Module {
    use curios_core::{
        Atom, Cases, InductArm, InductDecl, InductParam, Many, Scope, Telescope, UniverseContext,
        test_support::*,
    };

    let family = global("Big");
    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build(
            [(Free::local(902, Some("i")), type_at(5))],
            (),
        )),
        constructors: vec![(
            Atom::from("big"),
            InductParam::new(Telescope::done(vec![target]), Vec::new()),
        )]
        .into(),
        result_sort: Term::type_at(Level::constant(6)),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };
    let x = Free::local(910, Some("X"));
    let b = Free::local(911, Some("b"));
    let at = Term::induct_type(family, Vec::<Term>::new(), [Term::free_var(&x)]);
    let cases = Cases::Induct {
        cases: vec![(
            Atom::from("big"),
            InductArm::new(Scope::close(Many(0), &[], body), Vec::new()),
        )],
        default: None,
        stated: None,
    };
    let binders = [(x, assumed), (b, at.clone())];
    let item = authored(
        &global("f"),
        Term::func_type(binders.clone(), Term::free_var(&x)),
        Term::func(
            binders,
            match_ambient(Term::free_var(&b), Term::free_var(&x), cases),
        ),
    );

    declaring(
        Module {
            mounts: Vec::new(),
            items: vec![item].into(),
            witnesses: BTreeSet::new(),
            tests: Vec::new(),
        },
        BTreeMap::from([(family, declaration)]),
        [],
        [],
    )
}

/// The one gate that reads a published level and could admit a term — an index refinement the published sort makes `fits_its_variable` keep — reaches no closed term: the arm is behind a hypothesis the sort the variable is assumed at leaves uninhabited, and the arm's goal, which the publication cannot move, is the case's own target.
#[test]
fn a_kept_index_refinement_above_the_variables_sort_reaches_no_closed_term() {
    let honest = chained_sort_module(type_at(4), type_at(5), type_at(5), type_at(5));
    let record = walked(&honest).certification;
    let scope = AlreadyJudged::with(&honest, &record);
    let verdicts =
        |module: Module| fixture_certified(&module, BUDGET, &scope.globals(), SYNTAX).verdicts;

    // The arm's body is typed, so what the goal is decides: a body the refined goal does not admit is refused.
    let bogus = verdicts(refining_module(literal(0)));
    assert!(!bogus.is_empty(), "the arm's body is typed: {bogus:?}");

    // The refinement is kept: `Type 3` inhabits the refined goal.
    assert_eq!(
        verdicts(refining_module(type_at(3))),
        Vec::new(),
        "the arm is admitted",
    );

    // What the publication cannot move: the arm's goal is the case's target at every sort the variable is assumed at, since `MatchResult::at` opens the goal at the targets and reads no sort. `Type 3` inhabits `Type 4` and `0` does not, whether the rule holds the target's sort below the variable's (`Type 5`) or not (`Type 0`, `Type 1`).
    for assumed in [0, 1, 5] {
        assert_eq!(
            walked_whole(&refining_module_assumed(
                type_at(4),
                type_at(assumed),
                type_at(3)
            ))
            .verdicts,
            Vec::new(),
            "the goal is the target with the variable assumed at Type {assumed}",
        );
        assert!(
            !walked_whole(&refining_module_assumed(
                type_at(4),
                type_at(assumed),
                literal(0)
            ))
            .verdicts
            .is_empty(),
            "and a body the target does not admit is refused at Type {assumed}",
        );
    }

    // The refusal that stops the route: the arm is reachable only through a value of the family at the target, and the target does not inhabit the sort the variable is assumed at, which `subsumes` decides off the declared type and not off any publication.
    let applied = walked_whole(&applying_module()).verdicts;
    assert!(
        applied
            .iter()
            .any(|verdict| verdict.name == Some(global("g"))
                && matches!(verdict.error, Error::Mismatch { .. })),
        "nothing closed reaches the arm: {applied:?}",
    );
}

/// [`refining_module_at`]'s family and function, with `g : Type 4 = f(Type 4, Big/big())` beside them: the one call the arm is reachable through.
fn applying_module() -> Module {
    let module = refining_module_at(type_at(4), type_at(3));
    let mut items = module.items.clone();
    items.push(authored(
        &global("g"),
        type_at(4),
        Term::apply(
            Term::free_var(&Free::from(&global("f"))),
            [
                type_at(4),
                Term::variant(global("Big"), Vec::<Term>::new(), "big", Vec::<Term>::new()),
            ],
        ),
    ));

    Module { items, ..module }
}

/// One question, one answer: the digest a stage outside the certifier asks a part by ([`part_digest`]) reads a unit's classification where the certifier's own ([`read_digest`]) reads one, off a record that covers its unit and nowhere else, as [`Globals::partial`] does.
#[test]
fn a_part_digest_reads_a_classification_where_the_certifiers_own_reads_one() {
    use crate::*;

    let module = written();
    let places = Places::of(&module);
    // A record naming one of the unit's four definitions: it covers nothing, so a walk classifies the unit's items from their terms.
    let partial = Certification::of(
        [(
            global("five"),
            Certified {
                totality: Totality::Partial,
                reads: Reads::default(),
            },
        )],
        [],
        [],
        [],
    );
    assert!(
        !partial.covers(&module),
        "a record naming fewer definitions than its unit holds covers it",
    );
    let mut globals = Globals::default();
    globals.mount(&module, &places, &partial);

    let digest = |verdict| {
        held_digest(&module, &places, &global("five"), verdict, false).expect("the part digests")
    };
    assert_eq!(
        read_digest(&globals, &global("five"), false).expect("the part digests"),
        digest((false, None, None)),
        "the certifier's own digest reads the name as classified by no record",
    );
    assert_eq!(
        part_digest(&module, &places, &partial, &global("five"), false).expect("the part digests"),
        digest((false, None, None)),
        "and so does the digest a stage outside the certifier asks a part by",
    );
}

/// `induct Levelled.{u} : Type.{u + 1} | mk(x: Type.{u}) end`, whose entry carries `variances`: a payload's type mentions the level, so the walk computes it invariant.
fn varied_family(variances: Vec<Variance>) -> Module {
    use curios_core::{Atom, InductDecl, InductParam, Telescope, UniverseContext, test_support::*};
    use curios_utilities::Plicity;

    let family = global("Levelled");
    let level = Level::param(UniverseParam(0));
    let declaration = InductDecl {
        universe_context: UniverseContext {
            parameter_count: 1,
            quality_count: 0,
            constraints: Vec::new(),
        },
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("mk"),
            InductParam::new(
                Telescope::build(
                    [(Free::local(930, Some("x")), Term::type_at(level.clone()))],
                    Vec::new(),
                ),
                vec![Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort: Term::type_at(level.checked_add(1).expect("level admits the offset")),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances,
    };
    declaring(
        Module {
            mounts: Vec::new(),
            items: Vec::new().into(),
            witnesses: BTreeSet::new(),
            tests: Vec::new(),
        },
        BTreeMap::from([(family, declaration)]),
        [],
        [],
    )
}

/// The one check a judged item gets that an adopted one does not: `accept` refuses a carried variance vector claiming an irrelevance the walk denies, and `induct_alike` reads no carried vector, so the item is the baseline's and the walk adopts it.
///
/// What the adopted vector buys is nothing a judgment reads: the vector the kernel files for the entry is the baseline's, which the walk computed over the same telescopes, so every comparison of two instances reads the truthful one and the carried lie is the elaborator of a later unit's to be refused by.
#[test]
fn a_carried_variance_the_walk_denies_is_refused_where_the_item_is_judged_and_not_where_it_is_adopted()
 {
    let truthful = versioned(varied_family(vec![Variance::Invariant]));
    let certification = walked(&truthful).certification;
    let claimed = versioned(varied_family(vec![Variance::Irrelevant]));

    let whole = walked_whole(&claimed);
    assert!(
        whole
            .verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::VarianceDenied { .. })),
        "a whole walk refuses the carried irrelevance: {:?}",
        whole.verdicts,
    );

    let over = walked_over(&claimed, &truthful, &certification);
    assert_eq!(
        over.verdicts,
        Vec::new(),
        "and the walk over the baseline adopts the item without the check",
    );
    assert_eq!(
        over.certification.variances(&global("Levelled")),
        Some([Variance::Invariant].as_slice()),
        "while the vector it files, which every judgment reads, is the one the baseline's walk computed",
    );
}
