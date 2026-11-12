//! What a refusal's report says and where it points, for refusals `tests::rules` holds as patterns over the checker's error: a report is read by a person, so its words and its caret are claims of their own, which no pattern over an error states.

use super::test_support::*;

// The refusal names the declaration by the keyword it is written with and stands at the field's type, spelling the field as it is written: a superclass edge under `use`, a labeled field by its label, and a method or field written as a signature over the whole of it.
#[test]
fn an_informative_field_is_reported_as_written_at_its_type() {
    for (source, words, place) in [
        (
            "use /std/{Show};\nconcept Carry(A: Type): Prop { use Show(A), }\n/std/print(\"no\")\n",
            "the concept `Carry` is declared at `Prop`\n│  its field `use Show(A)` is no proposition\n│  every field of a `Prop` concept is a proposition\n",
            "│  2 │ concept Carry(A: Type): Prop { use Show(A), }\n│    │                                    ^^^^^^^",
        ),
        (
            "use /std/{Nat};\nuse /std/Bool/{True};\npub struct Bad: Prop { first: True, value: Nat, }\n/std/print(\"no\")\n",
            "the struct `Bad` is declared at `Prop`\n│  its field `value: Nat` is no proposition\n│  every field of a `Prop` struct is a proposition\n",
            "│  3 │ pub struct Bad: Prop { first: True, value: Nat, }\n│    │                                            ^^^",
        ),
        (
            "use /std/{Nat};\nconcept Bad(A: Type): Prop { get(A) -> Nat, }\n/std/print(\"no\")\n",
            "the concept `Bad` is declared at `Prop`\n│  its field `get: (A) -> Nat` is no proposition\n│  every field of a `Prop` concept is a proposition\n",
            "│  2 │ concept Bad(A: Type): Prop { get(A) -> Nat, }\n│    │                              ^^^^^^^^^^^^^",
        ),
        (
            "use /std/{Nat};\npub struct Bad: Prop { get(Nat) -> Nat, }\n/std/print(\"no\")\n",
            "the struct `Bad` is declared at `Prop`\n│  its field `get: (Nat) -> Nat` is no proposition\n│  every field of a `Prop` struct is a proposition\n",
            "│  2 │ pub struct Bad: Prop { get(Nat) -> Nat, }\n│    │                        ^^^^^^^^^^^^^^^",
        ),
    ] {
        let report = error(source);
        assert!(report.contains(words), "expected the words:\n{report}");
        assert!(report.contains(place), "expected the place:\n{report}");
    }
}

/// A witness refusal is located at the concept application the author wrote, under a telescope as without one.
///
/// A `satisfy` block has no name, so its declared type is the only thing a caret can point at — and that type is synthesized during lowering (`witness_concept_application`), which carries the application's span — without it, every duplicate, orphan, unkeyable and irregular-premise refusal would arrive with no line at all, naming only a module that may hold a dozen witnesses. Under a telescope the declared type is a `FuncType` built around the application, so it carries the span too.
#[test]
fn a_witness_refusal_is_located_at_its_concept_application() {
    let telescoped = error(
        r#"
        use /std/{Str, Show, List};
        satisfy (@A : Type, use Show(A)) => Show(List(A)) {
            show(v) = "l"
        }
        /std/Io/pure(())
        "#,
    );
    assert!(telescoped.contains("orphan witness"), "{telescoped}");
    assert!(telescoped.contains("=> Show(List(A)) {"), "{telescoped}");

    // The nullary form, whose declared type *is* the application.
    let bare = error(
        r#"
        use /std/{Str, Show, Nat};
        satisfy Show(Nat) {
            show(n) = "n"
        }
        /std/Io/pure(())
        "#,
    );
    assert!(bare.contains("orphan witness"), "{bare}");
    assert!(bare.contains("satisfy Show(Nat) {"), "{bare}");
}
