//! Coverage, strict positivity behind a record, the foreign wire contract, and the recorded agreement between the two checkers.

//! End-to-end coverage for the soundness perimeter entries that nothing else guards.
//!
//! The soundness perimeter is `documentation/soundness/`, one entry per rule, each graded *probed*, *argued*, or *auditable only* (see `documentation/design/language/the-soundness-perimeter.md`). "Probed" is a claim about executable evidence, so it needs a test that fails when the rule stops holding — otherwise the grade records what someone once tried by hand and decays the moment nobody remembers doing it.
//!
//! The entries with their own homes are not repeated here: strict positivity lives in `tests::positivity`, the two totality obligations in `tests::soundness`, and witness coherence in `tests::concepts`. What is left is the large-elimination guard, `Prop` non-informativeness, coverage, and the foreign wire contract — four rules the claim rests on that had no regression test at all.
//!
//! Each rejection asserts its *own* diagnostic, following `tests::soundness`. A perimeter test that accepts any error is worse than none: an invalid fixture passes it while the rule it names goes unchecked. That is not hypothetical — the first draft of these probes "passed" on `unbound variable`, having never reached the check at all.

use super::test_support::*;

// Coverage. A missing arm leaves an elimination undefined at that constructor, which is a proof of the motive at an index nothing established.
#[test]
fn an_elimination_must_enumerate_its_constructors() {
    rejected_by(
        AN_ELIMINATION_MUST_ENUMERATE_ITS_CONSTRUCTORS,
        "missing match case",
    );
}

// The foreign wire contract. The embedder supplies these values, so a `foreign` admitted at an arbitrary type would let the host hand back an inhabitant of a proposition that nothing ever checked.
#[test]
fn a_foreign_declaration_is_confined_to_wire_types() {
    rejected_by(
        A_FOREIGN_DECLARATION_IS_CONFINED_TO_WIRE_TYPES,
        "expected a wire type",
    );
}

// The other support the argument names, at a shape positivity's own twelve probes did not spell: they run the negative and the double negative bare, through an `induct` parameter, through a `struct` parameter, through a type alias, under `List`, behind a type-level `match`, and at a higher-kinded parameter — never behind an anonymous Σ, which is the construct this row is about.
//
// The diagnostic is what makes this more than a repeat. It reads *positively, but not strictly* rather than *negatively*, which is the same verdict the bare spelling gets: the polarity lattice is computed through the tuple component rather than the component being answered opaquely, since an opaque answer would join to `Mixed` and refuse with the other message. Refusing a merely-`Pos` diagonal is precisely what keeps `℘℘` out while `Prop` is impredicative, so this is the pairing the row rests on, checked where the row lives.
#[test]
fn a_non_strict_occurrence_behind_a_record_is_still_refused() {
    rejected_by(
        A_NON_STRICT_OCCURRENCE_BEHIND_A_RECORD_IS_STILL_REFUSED,
        "positively, but not strictly",
    );
}

/// Every perimeter fixture, put to both checkers, asserting what each says.
///
/// Each row judges the user suffix only, as `compile_entrypoint` does, so this costs what compiling sixteen small programs costs rather than sixteen walks of the standard library. It is the coverage map: where a rule's *second* opinion is recorded, or its absence admitted.
#[test]
fn the_two_checkers_agree_as_recorded() {
    for (name, source, expect_elaborator, expect_kernel) in CORPUS {
        let (elaborator, kernel) = both_checkers(source);
        agrees(name, "the elaborator", expect_elaborator, &elaborator);
        agrees(name, "the kernel", expect_kernel, &kernel);
    }
}
