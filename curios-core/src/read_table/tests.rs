use {super::*, curios_utilities::Qualifier};

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

/// A run of reads reads back as the entries it was made of, in their order, each name it holds written once however many read it.
#[test]
fn a_run_of_reads_reads_back_as_its_entries() {
    let entries = [
        (named("b"), reads(&["shared", "x"], &["shared"])),
        (named("a"), reads(&["shared"], &[])),
    ];
    let run =
        ReadTable::run(entries.iter().map(|(name, reads)| (name, reads))).expect("a run archives");

    assert_eq!(ReadTable::read(&run).expect("a run reads back"), entries);
    assert_eq!(
        run.windows(b"shared".len())
            .filter(|window| *window == b"shared")
            .count(),
        1,
        "a name is written once"
    );
}

/// What a definition read is named by alone, whatever it was stored beside.
#[test]
fn reads_are_named_by_their_definition_and_what_it_read() {
    let (name, read) = (named("a"), reads(&["x"], &[]));

    assert_eq!(
        ReadTable::identity(&name, &read),
        ReadTable::identity(&name, &read.clone())
    );
    assert_ne!(
        ReadTable::identity(&name, &read),
        ReadTable::identity(&named("b"), &read)
    );
}
