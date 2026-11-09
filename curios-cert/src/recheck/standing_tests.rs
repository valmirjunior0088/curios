//! Whether what a walk read of a declaration still stands across an edit: a signature read past an edit to the body, a body read, a proof's body, a generated definition's, a stored verdict's parts, a moved value sort, and the classification a part digest reads.

use {
    super::test_support::*,
    crate::*,
    curios_analysis::test_support::*,
    curios_common::{Candidates, Digest, Facet, Member, Object, candidate_prefix},
    curios_core::{
        Applications, Certification, Certified, Definition, DefinitionKind, Definitions, Free,
        Item, ItemRun, Places, ReadTable, Reads, Term, Totality, Universes,
    },
    std::collections::{BTreeMap, BTreeSet},
};
/// A signature read stands through an edit to the body behind it: `identity` returns `7` where it returned its argument, at the type it had, so it is judged and `five`, which typed it and unfolded nothing, is adopted.
#[test]
fn a_signature_read_stands_through_an_edit_to_the_body_behind_it() {
    let baseline = written();
    let certification = marked();
    let edited = reading_definitions(nat(), |_| literal(7));

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

/// `alias` respelled: an application that reduces to `Nat`.
fn applied_nat() -> Term {
    let t = Free::local(11, Some("t"));

    Term::apply(
        Term::func([(t, Term::type_ground())], Term::free_var(&t)),
        [nat()],
    )
}

/// A body read falls with the body: `three` unfolded `alias`, so an `alias` respelled as an application that reduces to `Nat` has `three` judged again. The baseline's record calls `three` partial, which an adopted `three` would keep; judged, it is total and the record says so.
#[test]
fn a_body_read_falls_with_an_edit_to_the_body() {
    let baseline = written();
    let certification = doctored(&walked(&baseline).certification, "three", |certified| {
        Certified {
            totality: Totality::Partial,
            ..certified
        }
    });
    let edited = reading_definitions(applied_nat(), Term::free_var);

    let rechecked = walked_over(&edited, &baseline, &certification);

    assert_eq!(rechecked.verdicts, Vec::new());
    assert_eq!(
        rechecked.certification.totality(&global("three")),
        Some(Totality::Total),
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

/// `lemma : Vouched = <proof>`, `spare : Vouched = qed(())`, and `corollary : Vouched = lemma`, with `lemma` written by its declaration where `written` and generated as a witness is otherwise: the kernel states it a proof in the first case alone, and it is marked as the module a walk hands back marks it.
fn proving_definitions(proof: Term, written: bool) -> Definitions {
    let (vouched_name, declaration) = vouched_declaration();
    let lemma = match authored_proof(&global("lemma"), vouched(), proof) {
        Item::Let(definition) if !written => Item::Let(Box::new(Definition {
            kind: DefinitionKind::Witness,
            proof: false,
            ..*definition
        })),
        lemma => lemma,
    };

    collision_definitions(
        vec![
            lemma,
            authored(&global("spare"), vouched(), qed()),
            authored(
                &global("corollary"),
                vouched(),
                Term::instance(global("lemma"), Universes::default()),
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

/// A record of `definitions` in which `corollary` asked for `lemma`'s body: its reads hold `spare`'s signature, which no judgment of `corollary` reads, so a walk that hands the entry back adopted it.
fn asking(definitions: &Definitions) -> Certification {
    doctored(
        &walked(definitions).certification,
        "corollary",
        |certified| Certified {
            reads: Reads {
                signatures: BTreeSet::from([global("lemma"), global("spare")]),
                bodies: BTreeSet::from([global("lemma")]),
            },
            ..certified
        },
    )
}

/// A proof unfolds to nothing whatever its body, so an edit to one moves no read of it: `lemma` proved another way is judged, and `corollary`, whose record says it asked for `lemma`'s body, takes the baseline's verdict.
#[test]
fn a_read_of_a_proofs_body_stands_through_an_edit_to_it() {
    let baseline = proving_definitions(qed(), true);
    let certification = asking(&baseline);
    let edited = proving_definitions(proved_otherwise(), true);

    let rechecked = walked_over(&edited, &baseline, &certification);

    assert_eq!(rechecked.verdicts, Vec::new());
    assert_eq!(
        rechecked.certification.reads(&global("corollary")),
        certification.reads(&global("corollary")),
    );
}

/// The control: generated as a witness is, the same statement is stated no proof and unfolds to its body, and the same edit has `corollary` judged again.
#[test]
fn a_read_of_a_generated_definitions_body_falls_with_an_edit_to_it() {
    let baseline = proving_definitions(qed(), false);
    let certification = asking(&baseline);
    let edited = proving_definitions(proved_otherwise(), false);

    let rechecked = walked_over(&edited, &baseline, &certification);

    assert_eq!(rechecked.verdicts, Vec::new());
    assert_ne!(
        rechecked.certification.reads(&global("corollary")),
        certification.reads(&global("corollary")),
    );
}

/// What a name unfolds to moves with what the kernel states of it though no term does: a `lemma` written by its declaration is stated a proof and one generated none, so one that moves from either to the other is no longer the baseline's, and `corollary`, which asked for its body, is judged again.
#[test]
fn a_read_of_a_body_falls_where_its_definition_starts_or_stops_being_a_proof() {
    for written in [true, false] {
        let baseline = proving_definitions(qed(), written);
        let certification = asking(&baseline);
        let edited = proving_definitions(qed(), !written);

        let rechecked = walked_over(&edited, &baseline, &certification);

        assert_eq!(rechecked.verdicts, Vec::new());
        assert_ne!(
            rechecked.certification.reads(&global("corollary")),
            certification.reads(&global("corollary")),
            "written in the baseline: {written}",
        );
    }
}

/// What a store holds of the walks filed into it, in memory: each record a member of its walk's run, under the name its filing completed it with.
struct Shelf(Vec<Object>);

impl Shelf {
    /// A store holding `certification`, a walk of `module`, its records completed as a filing completes them.
    fn filed(definitions: &Definitions, certification: &Certification) -> Self {
        complete(definitions, &Places::of(definitions), certification);
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
fn edited_five() -> Definitions {
    let five = authored(
        &global("five"),
        nat(),
        Term::apply(
            Term::instance(global("identity"), Universes::default()),
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
    let respelled = reading_definitions(applied_nat(), Term::free_var);

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

/// Three declarations: `H : Type 5 = h_body` stating its value sort `h_value_sort`, `G : Type 5 = H` stating `g_value_sort`, and `D : (G) -> G = (x) => x`, which reads `G`'s value sort through `Sort::of` when it checks its binder.
///
/// `G`'s declared type is `Type 5` in every version, so cumulativity admits an `H` body at any level up to 5; what moves between versions is the *value* sort `H`'s body reaches, which `G` republishes.
fn value_sort_definitions(h_body: Term, h_value_sort: Term, g_value_sort: Term) -> Definitions {
    let x = Free::local(10, Some("x"));
    let g = Term::instance(global("G"), Universes::default());

    Definitions {
        items: vec![
            aliased(&global("H"), type_at(5), h_body, h_value_sort),
            aliased(
                &global("G"),
                type_at(5),
                Term::instance(global("H"), Universes::default()),
                g_value_sort,
            ),
            authored(
                &global("D"),
                Term::func_type([(x, g.clone())], g.clone()),
                Term::func([(x, g.clone())], Term::free_var(&x)),
            ),
        ]
        .into(),
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
    let low = value_sort_definitions(type_at(4), type_at(5), type_at(0));
    let high = value_sort_definitions(type_at(4), type_at(5), type_at(5));
    let (low_places, high_places) = (Places::of(&low), Places::of(&high));
    for body in [false, true] {
        assert_ne!(
            held_digest(
                &low,
                &low_places,
                &global("G"),
                (false, None, None, None),
                body
            )
            .expect("G digests"),
            held_digest(
                &high,
                &high_places,
                &global("G"),
                (false, None, None, None),
                body
            )
            .expect("G digests"),
            "the read-digest of G holds its value_sort (body read: {body})",
        );
    }

    // The walk: `G`'s republished value sort moves `Type 0` → `Type 5`, `D` reads it, and the edited module is compiled over the baseline.
    let baseline = versioned(value_sort_definitions(nat(), type_at(0), type_at(0)));
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
            Term::instance(global("H"), Universes::default()),
            type_at(5),
        ),
    );

    let whole = walked(&value_sort_definitions(type_at(4), type_at(5), type_at(5)));
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

/// The sort the walk states a definition's value inhabits ([`Rechecked::definitions`]), where it states one.
fn stated_value_sort(rechecked: &Rechecked, name: &str) -> Option<Term> {
    let place = Places::of(&rechecked.definitions)
        .place(&global(name))
        .expect("the module declares the name")
        .item();

    match &rechecked.definitions.items[place] {
        Item::Let(definition) => definition.value_sort.clone(),
        Item::Rec(_) => None,
    }
}

/// A walk over a baseline states a reader of a moved value sort what a whole compile states it: `L`'s value sort is read off `K`'s published one, which reads `H`'s, so moving `H`'s body to a higher universe moves the sort the walk states of `L` though `L`'s terms stand. What each version said of its sorts is nothing the walk reads.
#[test]
fn a_reader_of_a_moved_value_sort_is_stated_the_sort_a_whole_compile_states() {
    let baseline = versioned(chained_sort_definitions(
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
            aliased(&global("H"), type_at(5), type_at(4), type_at(0)),
        ),
        "K",
        aliased(
            &global("K"),
            type_at(5),
            Term::instance(global("H"), Universes::default()),
            type_at(0),
        ),
    );

    let whole = walked(&edited);
    assert_eq!(stated_value_sort(&whole, "L"), Some(type_at(5)));

    let over = walked_over(&edited, &baseline, &certification);
    assert_eq!(over.verdicts, Vec::new());
    assert_eq!(
        stated_value_sort(&over, "L"),
        stated_value_sort(&whole, "L")
    );
}

/// `M : Type 5 = L` stating `sort`: an alias of the warm unit's lying publication, judged by a whole walk with that unit in scope.
fn later_alias(sort: Term) -> Definitions {
    Definitions {
        items: vec![aliased(
            &global("M"),
            type_at(5),
            Term::instance(global("L"), Universes::default()),
            sort,
        )]
        .into(),
    }
}

/// A later walk states an alias the sort its value inhabits, whatever the alias says of it: `M : Type 5 = L` saying `Type 0`, where `L` unfolds to a type at `Type 5`, is admitted and stated `Type 5`, read off what the walk over `L`'s unit stated with nothing run.
#[test]
fn a_later_walk_states_an_alias_the_sort_its_value_inhabits() {
    let honest = chained_sort_definitions(type_at(4), type_at(5), type_at(5), type_at(5));
    let record = walked(&honest).certification;
    let stated = walked(&honest).definitions;
    let scope = AlreadyJudged::with(&stated, &record);
    let later = fixture_certified(&later_alias(type_at(0)), BUDGET, &scope.globals(), SYNTAX);

    assert_eq!(later.verdicts, Vec::new());
    assert_eq!(stated_value_sort(&later, "M"), Some(type_at(5)));
}

/// One question, one answer: the digest a stage outside the certifier asks a part by ([`part_digest`]) reads a unit's classification where the certifier's own ([`read_digest`]) reads one, off a record that covers its unit and nowhere else, as [`Globals::partial`] does.
#[test]
fn a_part_digest_reads_a_classification_where_the_certifiers_own_reads_one() {
    use crate::*;

    let definitions = written();
    let places = Places::of(&definitions);
    // A record naming one of the unit's four definitions: it covers nothing, so a walk classifies the unit's items from their terms.
    let partial = Certification::of(
        [(
            global("five"),
            Certified {
                totality: Totality::Partial,
                applications: Applications::default(),
                reads: Reads::default(),
            },
        )],
        [],
        [],
        [],
    );
    assert!(
        !partial.covers(&definitions),
        "a record naming fewer definitions than its unit holds covers it",
    );
    let mut globals = Globals::default();
    globals.mount(&definitions, &places, &partial);

    let digest = |verdict| {
        held_digest(&definitions, &places, &global("five"), verdict, false)
            .expect("the part digests")
    };
    assert_eq!(
        read_digest(&globals, &global("five"), false).expect("the part digests"),
        digest((false, None, None, None)),
        "the certifier's own digest reads the name as classified by no record",
    );
    assert_eq!(
        part_digest(&definitions, &places, &partial, &global("five"), false)
            .expect("the part digests"),
        digest((false, None, None, None)),
        "and so does the digest a stage outside the certifier asks a part by",
    );
}
