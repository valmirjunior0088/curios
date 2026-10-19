use {
    super::*,
    crate::ReadRecord,
    curios_utilities::{Facet, Qualifier, candidate_prefix},
};

/// The name `name`.
fn named(name: &str) -> Global {
    Global::Authored(Qualifier::from([name]))
}

/// What judging a definition reads when it reads the signatures of `signatures` and the bodies of `bodies`.
fn reads(signatures: &[&str], bodies: &[&str]) -> Reads {
    Reads {
        signatures: signatures.iter().map(|name| named(name)).collect(),
        bodies: bodies.iter().map(|name| named(name)).collect(),
    }
}

/// What a filing completes a judgment of the item `input` with, reading one signature.
fn judged(input: &[u8]) -> Concluded {
    Concluded {
        input: Digest::of(input),
        signatures: vec![Some(Some([7; 32]))],
        bodies: Vec::new(),
        totality: Totality::Total,
        vectors: None,
    }
}

/// A run of reads reads back as the entries it was made of, in their order, each name it holds written once however many read it, and what completed an entry with it.
#[test]
fn a_run_of_reads_reads_back_as_its_entries() {
    let entries = [
        (named("b"), reads(&["shared", "x"], &["shared"]), None),
        (named("a"), reads(&["shared"], &[]), Some(judged(b"a"))),
    ];
    let run = ReadTable::run(
        entries
            .iter()
            .map(|(name, reads, made)| (name, reads, made.as_ref())),
    )
    .expect("a run archives");

    assert_eq!(ReadTable::read(&run).expect("a run reads back"), entries);
    assert_eq!(
        ReadTable::member(&run, 1).expect("a member reads alone"),
        entries[1]
    );
    assert_eq!(
        run.windows(b"shared".len())
            .filter(|window| *window == b"shared")
            .count(),
        1,
        "a name is written once"
    );
}

/// What a definition read is named by alone, whatever it was stored beside, and once a filing completes it, by the item it judged before what it holds.
#[test]
fn reads_are_named_by_their_definition_and_what_it_read() {
    let (name, read) = (named("a"), reads(&["x"], &[]));
    let digest =
        |record: ReadRecord, name: &Global| record.digest(name).expect("a record archives");

    assert_eq!(
        digest(ReadRecord::new(read.clone()), &name),
        digest(ReadRecord::new(read.clone()), &name)
    );
    assert_ne!(
        digest(ReadRecord::new(read.clone()), &name),
        digest(ReadRecord::new(read.clone()), &named("b"))
    );
    let completed = ReadRecord::new(read);
    completed.complete(Some(judged(b"a")));
    assert!(
        digest(completed, &name)
            .as_bytes()
            .starts_with(&candidate_prefix(Facet::Judgment, &Digest::of(b"a")))
    );
}
