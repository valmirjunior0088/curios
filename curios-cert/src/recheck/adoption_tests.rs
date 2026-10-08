//! Which of a baseline's verdicts a walk adopts, and which declarations it judges again.

use {
    super::test_support::*,
    crate::{Baseline, Error, Globals, Kernel, Rechecked, induct_alike, struct_alike},
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Certification, Certified, Free, Global, Intrinsic, Module, Nat, Places, Reads, Term,
        Totality,
    },
    curios_utilities::Qualifier,
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
        ],
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
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
    let places = Places::of(baseline);
    let globals = Globals::default();

    super::verdicts_into(
        &mut Kernel::new(BUDGET, SYNTAX),
        module,
        None,
        &globals,
        Some(Baseline {
            module: baseline,
            places: &places,
            certification,
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
    let t = Free::local(11, Some("t"));
    let respelled = Term::apply(
        Term::func([(t, Term::type_ground())], Term::free_var(&t)),
        [nat()],
    );
    let edited = reading_module(respelled, Term::free_var);

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

/// A registry entry is the baseline's by what the certifier reads of it: its terms, and the variance vector a walk answers from. Elaboration's stamps take no part — the polarity vector, in whose place a walk holds the one the certifier computed, the owning module and the representation's privacy — so an entry differing from the baseline's in those alone is the baseline's, and one differing in its variance vector or in a field's type is not.
///
/// Mutation-checked against each comparison: taken over the whole entry, the stamped pair is another declaration; with the variance vector left out the family is the baseline's; and with the arity left out the record is.
#[test]
fn a_registry_entry_is_the_baselines_by_what_the_certifier_reads_of_it() {
    let held = two_registry_entries(Varied::Nothing);
    let family = |module: &Module| module.induct_decls[&shadowed_family()].clone();
    let record = |module: &Module| module.struct_decls[&kept_record()].clone();

    let stamped = two_registry_entries(Varied::Stamps);
    assert!(
        induct_alike(&family(&stamped), &family(&held))
            && struct_alike(&record(&stamped), &record(&held)),
        "an entry differing from the baseline's in elaboration's stamps alone is another declaration",
    );
    assert!(
        !induct_alike(
            &family(&two_registry_entries(Varied::Variance)),
            &family(&held)
        ),
        "a family differing from the baseline's in its variance vector is the baseline's",
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
