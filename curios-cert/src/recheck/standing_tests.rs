//! Whether what a walk read of a declaration still stands across an edit: a signature read past an edit to the body, a body read, a proof's body, a generated definition's, a stored verdict's parts, a moved value sort, and the classification a part digest reads.

use {
    super::test_support::*,
    crate::*,
    curios_common::fill,
    curios_common::{Candidates, Digest, Facet, Member, Object, candidate_prefix},
    curios_core::{
        Applications, Certification, Certified, Definitions, Item, ItemRun, Places, ReadTable,
        Reads, Term, Totality,
    },
    std::collections::BTreeSet,
};
/// A signature read stands through an edit to the body behind it: `identity` returns `7` where it returned its argument, at the type it had, so it is judged and `five`, which typed it and unfolded nothing, is adopted.
#[test]
fn a_signature_read_stands_through_an_edit_to_the_body_behind_it() {
    let baseline = written();
    let certification = marked();
    let edited = reading_definitions("#NatType", "7");

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
const APPLIED_NAT: &str = "((t: Type) => t)(#NatType)";

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
    let edited = reading_definitions(APPLIED_NAT, "x");

    let rechecked = walked_over(&edited, &baseline, &certification);

    assert_eq!(rechecked.verdicts, Vec::new());
    assert_eq!(
        rechecked.certification.totality(&global("three")),
        Some(Totality::Total),
    );
}

/// `Vouched : Prop | qed(u : {})`, `lemma : Vouched = ${proof}` after `${mark}`, `spare : Vouched = qed(())`, and `corollary : Vouched = lemma`.
const PROVING: &str = r#"
induct /Vouched: pub Prop
| qed(u: {})
end

${mark}
let /lemma: #{node = InductType} /Vouched() =
    ${proof};

let /spare: #{node = InductType} /Vouched() =
    #{node = Variant} /Vouched/qed(());

let /corollary: #{node = InductType} /Vouched() =
    /lemma;
"#;

/// `Vouched/qed(())`, the proof of `Vouched`.
const QED: &str = "#{node = Variant} /Vouched/qed(())";

/// `qed(())` reached through a redex: another proof of the same statement.
const PROVED_OTHERWISE: &str =
    "((u: #{node = InductType} /Vouched()) => u)(#{node = Variant} /Vouched/qed(()))";

/// [`PROVING`] at `proof`, with `lemma` written by its declaration where `written` and generated as a witness is otherwise: the certifier states it a proof in the first case alone, and it is marked as the module a walk hands back marks it.
fn proving_definitions(proof: &str, written: bool) -> Definitions {
    let mark = match written {
        true => "#{proof = true}",
        false => "#{kind = Witness}",
    };

    core_text_module(&fill!(PROVING, mark, proof))
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
    let baseline = proving_definitions(QED, true);
    let certification = asking(&baseline);
    let edited = proving_definitions(PROVED_OTHERWISE, true);

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
    let baseline = proving_definitions(QED, false);
    let certification = asking(&baseline);
    let edited = proving_definitions(PROVED_OTHERWISE, false);

    let rechecked = walked_over(&edited, &baseline, &certification);

    assert_eq!(rechecked.verdicts, Vec::new());
    assert_ne!(
        rechecked.certification.reads(&global("corollary")),
        certification.reads(&global("corollary")),
    );
}

/// What a name unfolds to moves with what the certifier states of it though no term does: a `lemma` written by its declaration is stated a proof and one generated none, so one that moves from either to the other is no longer the baseline's, and `corollary`, which asked for its body, is judged again.
#[test]
fn a_read_of_a_body_falls_where_its_definition_starts_or_stops_being_a_proof() {
    for written in [true, false] {
        let baseline = proving_definitions(QED, written);
        let certification = asking(&baseline);
        let edited = proving_definitions(QED, !written);

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
    let five = declared_item("let /five: #NatType = /identity(6);", "five");

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
    let respelled = reading_definitions(APPLIED_NAT, "x");

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

/// Three declarations: `H : Type 5 = ${h_body}` stating its value sort `${h}`, `G : Type 5 = H` stating `${g}`, and `D : (G) -> G = (x) => x`, which reads `G`'s value sort through `Sort::of` when it checks its binder.
///
/// `G`'s declared type is `Type 5` in every version, so cumulativity admits an `H` body at any level up to 5; what moves between versions is the *value* sort `H`'s body reaches, which `G` republishes.
const VALUE_SORTS: &str = r#"
#{value_sort = ${h}}
let /H: #{level = 5} Type =
    ${h_body};

#{value_sort = ${g}}
let /G: #{level = 5} Type =
    /H;

let /D: (x: /G) -> /G =
    (x: /G) => x;
"#;

/// [`VALUE_SORTS`]' module at `h_body`, `h` and `g`.
fn value_sort_definitions(h_body: &str, h: &str, g: &str) -> Definitions {
    core_text_module(&fill!(VALUE_SORTS, h_body, h, g))
}

/// `${name} : Type 5 = ${body}`, stating the sort `${sort}` its value inhabits.
const AN_ALIAS: &str = r#"
#{value_sort = ${sort}}
let /${name}: #{level = 5} Type =
    ${body};
"#;

/// [`AN_ALIAS`] at `name`, `body` and `sort`, as an edit puts it in a version.
fn alias(name: &str, body: &str, sort: &str) -> Item {
    declared_item(&fill!(AN_ALIAS, name, body, sort), name)
}

/// `Type 0`, as Core text writes it.
const TYPE_0: &str = "Type";

/// `Type 4`.
const TYPE_4: &str = "#{level = 4} Type";

/// `Type 5`.
const TYPE_5: &str = "#{level = 5} Type";

/// A reader of a moved `value_sort` is judged again rather than adopted, because the certifier's read-digest ([`held_digest`]) holds the sort a definition states for its value beside its type.
///
/// `H : Type 5` moves from `Nat` (value sort `Type 0`) to `Type 4` (value sort `Type 5`) under a fixed declared type, so the alias `G : Type 5 = H` republishes a value sort that moves `Type 0` → `Type 5` while `G`'s body term, the occurrence of `H`, does not. `D : (G) -> G` reads `G`'s value sort through `Sort::of`, so its verdict is a function of a field the digest now covers and the walk asks the reads of it again.
///
/// The first assertion pins the digest directly: two modules whose `G` differs only in `value_sort` digest it apart, signature read and body read alike. The three that follow are what a reader judged again must agree with — the verdict and the record of a whole compile of the same module.
///
/// Mutation-checked: dropping `self.held.value_sort` from both parts of `Readable::digest` makes the two `G` digests equal, and fails this test and [`a_reader_of_a_moved_value_sort_is_stated_the_sort_a_whole_compile_states`] and no other.
#[test]
fn a_reader_of_a_moved_value_sort_is_judged_again() {
    // The field, read off the digest: `G`'s part digests apart where its value sort is `Type 0` and where it is `Type 5`.
    let low = value_sort_definitions(TYPE_4, TYPE_5, TYPE_0);
    let high = value_sort_definitions(TYPE_4, TYPE_5, TYPE_5);
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
    let baseline = versioned(value_sort_definitions("#NatType", TYPE_0, TYPE_0));
    let certification = walked(&baseline).certification;
    let edited = replaced(
        &replaced(&baseline, "H", alias("H", TYPE_4, TYPE_5)),
        "G",
        alias("G", "/H", TYPE_5),
    );

    let whole = walked(&value_sort_definitions(TYPE_4, TYPE_5, TYPE_5));
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
        Item::Rec(_) | Item::Nominal(_) => None,
    }
}

/// A walk over a baseline states a reader of a moved value sort what a whole compile states it: `L`'s value sort is read off `K`'s published one, which reads `H`'s, so moving `H`'s body to a higher universe moves the sort the walk states of `L` though `L`'s terms stand. What each version said of its sorts is nothing the walk reads.
#[test]
fn a_reader_of_a_moved_value_sort_is_stated_the_sort_a_whole_compile_states() {
    let baseline = versioned(chained_sort_definitions("#NatType", TYPE_0, TYPE_0, TYPE_0));
    let certification = walked(&baseline).certification;
    // `H`'s body moves `Nat` -> `Type 4`, so its value sort moves `Type 0` -> `Type 5`, and `K` republishes it. `L` is untouched: its terms, `K`'s type and `K`'s body term all stand.
    let edited = replaced(
        &replaced(&baseline, "H", alias("H", TYPE_4, TYPE_0)),
        "K",
        alias("K", "/H", TYPE_0),
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

/// `M : Type 5 = L` stating `Type 0`: an alias of the warm unit's lying publication, judged by a whole walk with that unit in scope.
const A_LATER_ALIAS: &str = r#"
#{value_sort = Type}
let /M: #{level = 5} Type =
    /L;
"#;

/// A later walk states an alias the sort its value inhabits, whatever the alias says of it: `M : Type 5 = L` saying `Type 0`, where `L` unfolds to a type at `Type 5`, is admitted and stated `Type 5`, read off what the walk over `L`'s unit stated with nothing run.
#[test]
fn a_later_walk_states_an_alias_the_sort_its_value_inhabits() {
    let honest = chained_sort_definitions(TYPE_4, TYPE_5, TYPE_5, TYPE_5);
    let record = walked(&honest).certification;
    let stated = walked(&honest).definitions;
    let scope = AlreadyJudged::with(&stated, &record);
    let later = text_certified_over(A_LATER_ALIAS, &scope.globals());

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
        part_digest(
            DigestedUnit::of(&definitions, &places, &partial),
            &global("five"),
            false
        )
        .expect("the part digests"),
        digest((false, None, None, None)),
        "and so does the digest a stage outside the certifier asks a part by",
    );
}
