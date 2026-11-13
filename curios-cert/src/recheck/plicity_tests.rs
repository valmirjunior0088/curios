//! Marks: the ones a registry entry declares, and the written ones the certifier holds to them.

use {
    super::test_support::*,
    crate::*,
    curios_common::{Plicity, fill},
    curios_core::{Item, Subterm},
};

/// A family of one case, `Held` with `mk(${mark}n : Nat)`, under the mark `${mark}` states, and `value` its case at `(${payload})`.
const MARKED_CASE: &str = r#"
#{absent = [/Held/mk]}
induct /Held: pub Type
| mk(${mark}n: #NatType)
end

let /value: #{node = InductType} /Held() =
    #{node = Variant} /Held/mk(${payload});
"#;

/// `f : (a : Nat) -> Type`, and `held : f(${mark}3) = 3`, its argument under the mark `${mark}` states.
const MARKED_ARGUMENT: &str = r#"
let /f: (a: #NatType) -> Type =
    (a: #NatType) => #NatType;

let /held: /f(${mark}3) =
    3;
"#;

/// `struct S(A : Type) : Type { f : A }` with `held : ${declared} = S(Nat) { 3 }`, `${declared}` an η-form of `S` applied over its whole list.
const ETA_FORMED: &str = r#"
struct /S(A: Type): pub Type {
    f: A,
}

let /held: ${declared} =
    /S(#NatType) { 3 };
"#;

/// `g : (@n : Nat) -> Nat`, a function forwarding across a mark, `(@n) => ((x) => x)(n)`, and the same function put at `(${mark}n : Nat) -> Nat` by its name, `by_name`, and as the literal itself, `by_literal`.
const FORWARDING: &str = r#"
let /g: (@n: #NatType) -> #NatType =
    (@n: #NatType) => ((x: #NatType) => x)(n);

let /by_name: (${mark}n: #NatType) -> #NatType =
    /g;

let /by_literal: (${mark}n: #NatType) -> #NatType =
    (@n: #NatType) => ((x: #NatType) => x)(n);
"#;

/// [`MARKED_CASE`] under its honest mark or a lying one, with `payload` arguments.
fn marked_case(honest: bool, payload: usize) -> String {
    let mark = if honest { "" } else { "@" };
    let payload = ["0"; 1][..payload].join(", ");

    fill!(MARKED_CASE, mark, payload)
}

/// A constructor's marks are read by one certifier rule, the arm's, which holds each binder an arm writes to the payload it binds ([`an_arm_binds_under_its_payloads_marks`]). No other rule reads them, and a constructor's *value* carries none: the same declaration under its honest mark and under a lying one, with a value of it and no match over it, must produce the same verdicts. That each binder has a mark is no one's to check: a mark is its telescope entry's own, so a signature a mark short of its binders cannot be written, and the same holds of the marks a declaration's parameters stand under in its arity.
///
/// That reason is an *inventory*: every consumer of a registry entry, read against the clauses. An inventory is exactly the kind of claim that goes stale as code moves, and the polarity vector beside it on the same entry has `curios-analysis`'s `a_carried_polarity_vector_is_recomputed_rather_than_believed` and this crate's `a_mounted_familys_vector_is_the_certifiers_and_not_its_entrys` holding its own version of this. This holds the plicity half executably instead.
///
/// The lie is merely wrong — an `Implicit` mark where the declaration says `Explicit`. What must not happen is this certifier quietly deciding a value of the family *differently* because of it, which is what a nominal rule reading the marks outside an arm would introduce without any clause noticing.
///
/// The control is [`a_wrong_payload_count_is_still_refused_under_a_lying_plicity_vector`]: the same lie beside a genuine error. Without it, "the certifier ignores the mark" and "the certifier ignores this module" read alike.
#[test]
fn a_constructors_mark_decides_nothing_of_its_value() {
    assert_eq!(
        text_verdicts(&marked_case(true, 1)),
        text_verdicts(&marked_case(false, 1)),
        "a mark no rule reads outside an arm changed a verdict",
    );
    assert_eq!(
        text_verdicts(&marked_case(false, 1)),
        Vec::new(),
        "both sides must be accepted, or the equality above is two refusals agreeing",
    );
}

/// The control for the fixture above: under the same lying plicity vector, an ordinary error is still caught.
#[test]
fn a_wrong_payload_count_is_still_refused_under_a_lying_plicity_vector() {
    let verdicts = text_verdicts(&marked_case(false, 0));

    assert!(
        refuses(&verdicts, Some("value"), miscounted(Counted::Payload, 1, 0)),
        "the certifier accepted a constructor application at the wrong payload count: {verdicts:?}",
    );
}

/// An argument stands under its parameter's mark. The application rule holds each written mark to the binder it fills, where the binder is at hand, so a spine's marks are a function of its head's type — which is what lets conversion compare two spines by their arguments alone, in this checker as in the elaborator.
///
/// The control is the same application under the parameter's own mark: refusing every application would pass the refusal alone.
#[test]
fn an_argument_stands_under_its_parameters_mark() {
    let mark = "";
    assert_eq!(
        text_verdicts(&fill!(MARKED_ARGUMENT, mark)),
        Vec::new(),
        "an argument under its parameter's own mark was refused",
    );

    let mark = "@";
    let refused = text_verdicts(&fill!(MARKED_ARGUMENT, mark));
    assert!(
        refuses(&refused, Some("held"), |error| matches!(
            error,
            Error::Mark {
                counted: Counted::Arguments,
                position: 1,
                declared: Plicity::Explicit,
                written: Plicity::Implicit,
            }
        )),
        "an argument written `@` at a plain parameter was not refused by its mark: {refused:?}",
    );
}

/// An arm binds each payload under the payload's own mark: the arm rule holds the marks an arm writes to its constructor's, as the application rule holds an argument's to its parameter's.
///
/// The control is `recheck::occurrence_tests`' `an_arm_matching_its_payload_still_reduces`, the same arm under the payload's own mark.
#[test]
fn an_arm_binds_under_its_payloads_marks() {
    let arm = "mk(@a)";
    let refused = text_verdicts(&fill!(ARM, arm));

    assert!(
        refuses(&refused, Some("held"), |error| matches!(
            error,
            Error::Mark {
                counted: Counted::ArmBinders,
                position: 1,
                declared: Plicity::Explicit,
                written: Plicity::Implicit,
            }
        )),
        "an arm binding a plain payload under `@` was not refused by its mark: {refused:?}",
    );
}

/// A former's η-form applied over its whole list is built as the node it applies, and the β-step reads neither its binder's mark nor its binder's type (`curios_core::former_eta`): what the certifier then types is the node, by the former's own parameter. So an η-form of `S` whose binder stands under a mark `S`'s parameter does not take builds `S(Nat)` and is accepted as `S(Nat)`, and one whose binder states a type `S`'s parameter does not take cannot carry an argument through it: `S(3)` is refused as `S` refuses it.
///
/// The control is the first module, accepted: refusing every type spelled as an applied η-form would pass the refusal alone.
#[test]
fn an_applied_eta_form_is_typed_by_its_formers_parameter_and_not_its_binder() {
    // Its binder under a mark `S`'s plain parameter does not take, and at a type `S`'s parameter does not take.
    let honest = "((@A: Type) => #{node = StructType} /S(A))(@#NatType)";
    let lying = "((A: #NatType) => #{node = StructType} /S(A))(3)";
    let built = |declared: &str| {
        let (definitions, _) = put()
            .read(&fill!(ETA_FORMED, declared))
            .unwrap_or_else(|unread| panic!("{unread:?}"));
        let held = definitions
            .items
            .iter()
            .find_map(|item| match item {
                Item::Let(definition) if definition.name == global("held") => Some(definition),
                _ => None,
            })
            .expect("the module defines `held`");
        assert!(
            matches!(&*held.type_, Subterm::StructType(_)),
            "an η-form of `S` applied over its whole list was not built as its node: {:?}",
            held.type_,
        );
    };

    built(honest);
    assert_eq!(
        text_verdicts(&fill!(ETA_FORMED, declared = honest)),
        Vec::new(),
        "an η-form whose binder takes another mark than its former's was refused at the node it builds",
    );

    built(lying);
    let refused = text_verdicts(&fill!(ETA_FORMED, declared = lying));
    assert!(
        refuses(&refused, Some("held"), |error| matches!(
            error,
            Error::Mismatch { expected, .. } if matches!(&***expected, Subterm::Sort(_))
        )),
        "an argument `S` refuses was carried through an η-form whose binder takes it: {refused:?}",
    );
}

/// A function that forwards across a mark, `(@n) => ((x) => x)(n)`, η-contracts to the plain `(x) => x` under reduction, which reads no mark; the typed question reads one. Put where a plain function is expected, by its name or as the literal itself, it is refused as the hidden-headed function it is, and the contraction it reduces to is never what it is taken at.
///
/// The control is the same two items at the hidden type they are, accepted.
#[test]
fn a_function_forwarding_across_a_mark_is_not_taken_at_the_type_it_contracts_to() {
    let mark = "@";
    assert_eq!(
        text_verdicts(&fill!(FORWARDING, mark)),
        Vec::new(),
        "a function forwarding across a mark was refused at its own hidden-headed type",
    );

    let mark = "";
    let refused = text_verdicts(&fill!(FORWARDING, mark));
    for item in ["by_name", "by_literal"] {
        assert!(
            refuses(&refused, Some(item), |error| matches!(
                error,
                Error::Mismatch { .. }
            )),
            "`{item}` was taken at the plain type its body contracts to: {refused:?}",
        );
    }
}
