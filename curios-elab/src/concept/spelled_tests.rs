//! What a witness signature spells before anything elaborates: the key each written shape gives, and the shapes that give none.

use {
    super::{HeadKey, Spelled, WitnessKey, Written},
    curios_core::{Free, Global, Intrinsic, Term},
    curios_utilities::{Plicity, Qualifier},
    std::collections::BTreeMap,
};

fn named(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

fn mention(path: &str) -> Term {
    Term::free_var(&Free::from(&named(path)))
}

fn nat() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

fn local(index: u32, hint: &str) -> Free {
    Free::local(index, Some(hint))
}

/// A function of one `Type` parameter to what `body` makes of it.
fn function(body: impl FnOnce(Term) -> Term) -> Term {
    let parameter = local(0, "A");
    let body = body(Term::free_var(&parameter));

    Term::func([(parameter, Term::type_ground())], body)
}

/// A unit that declares the formers `Option` and `State`, the one-parameter concepts `Show` and `Named`, whose parameter is implicit, and the two-parameter concept `Lift`, with `definitions` bound beside them.
struct Unit {
    definitions: BTreeMap<Global, Term>,
}

impl Unit {
    fn with(definitions: impl IntoIterator<Item = (&'static str, Term)>) -> Self {
        Self {
            definitions: definitions
                .into_iter()
                .map(|(path, body)| (named(path), body))
                .collect(),
        }
    }

    /// The key the witness declared at `type_` is spelled at.
    fn key(&self, type_: &Term) -> Option<(Global, WitnessKey)> {
        let written = |name: &Global| match self.definitions.get(name) {
            Some(body) => Written::Defined(body),
            None if ["Option", "State", "Show", "Named", "Lift"]
                .map(named)
                .contains(name) =>
            {
                Written::Former
            }
            None => Written::Opaque,
        };
        let marks = |name: &Global| match name {
            name if *name == named("Show") => Some(vec![Plicity::Explicit]),
            name if *name == named("Named") => Some(vec![Plicity::Implicit]),
            name if *name == named("Lift") => Some(vec![Plicity::Explicit; 2]),
            _ => None,
        };

        Spelled {
            written: &written,
            marks: &marks,
        }
        .key(type_)
    }

    /// The head a `Show` witness of `parameter` is spelled at.
    fn head(&self, parameter: Term) -> Option<HeadKey> {
        let (concept, key) = self.key(&Term::apply(mention("Show"), [parameter]))?;
        assert_eq!(concept, named("Show"));
        let [head] = key.0.try_into().expect("one parameter, one head");

        Some(head)
    }
}

fn nominal(path: &str) -> Option<HeadKey> {
    Some(HeadKey::Nominal(named(path)))
}

#[test]
fn a_former_spells_its_own_name_applied_or_not() {
    let unit = Unit::with([]);

    assert_eq!(unit.head(mention("Option")), nominal("Option"));
    assert_eq!(
        unit.head(Term::apply(mention("Option"), [nat()])),
        nominal("Option")
    );
    // A family that takes its indices in a call of their own.
    assert_eq!(
        unit.head(Term::apply(Term::apply(mention("State"), [nat()]), [nat()])),
        nominal("State")
    );
}

#[test]
fn a_type_written_out_spells_itself() {
    let unit = Unit::with([]);
    let pair = Term::tuple_type([
        (Free::local(0, None), nat()),
        (Free::local(1, Some("y")), nat()),
    ]);

    assert_eq!(unit.head(nat()), Some(HeadKey::Nat));
    assert_eq!(
        unit.head(pair),
        Some(HeadKey::TupleType(vec![String::new(), "y".to_string()]))
    );
}

/// A definition bound to a type is read through, and one that takes parameters is read through them where it is applied to every one.
#[test]
fn a_name_for_a_type_spells_the_type() {
    let unit = Unit::with([
        ("Count", nat()),
        ("Maybe", mention("Option")),
        ("Wrapped", function(|a| Term::apply(mention("Option"), [a]))),
        ("Twice", function(|a| Term::apply(mention("Wrapped"), [a]))),
    ]);

    assert_eq!(unit.head(mention("Count")), Some(HeadKey::Nat));
    assert_eq!(unit.head(mention("Maybe")), nominal("Option"));
    assert_eq!(
        unit.head(Term::apply(mention("Maybe"), [nat()])),
        nominal("Option")
    );
    assert_eq!(unit.head(mention("Wrapped")), nominal("Option"));
    assert_eq!(
        unit.head(Term::apply(mention("Wrapped"), [nat()])),
        nominal("Option")
    );
    assert_eq!(
        unit.head(Term::apply(mention("Twice"), [nat()])),
        nominal("Option")
    );
}

/// Reduction stops at a binder, so a function names the family its body applies, and a definition applied there is not read through.
#[test]
fn a_function_spells_the_former_its_body_applies() {
    let unit = Unit::with([
        ("Wrapped", function(|a| Term::apply(mention("Option"), [a]))),
        ("Twice", function(|a| Term::apply(mention("Wrapped"), [a]))),
    ]);

    assert_eq!(
        unit.head(function(|a| Term::apply(mention("State"), [nat(), a]))),
        nominal("State")
    );
    assert_eq!(unit.head(mention("Twice")), None);
    assert_eq!(
        unit.head(function(|a| Term::apply(mention("Wrapped"), [a]))),
        None
    );
}

#[test]
fn what_computes_or_names_nothing_declared_spells_no_head() {
    let unit = Unit::with([("Looping", mention("Looping"))]);

    assert_eq!(unit.head(mention("Unknown")), None);
    assert_eq!(unit.head(mention("Looping")), None);
    assert_eq!(unit.head(Term::free_var(&local(7, "T"))), None);
    assert_eq!(unit.head(Term::type_ground()), None);
    assert_eq!(unit.head(function(|a| a)), None);
    // Applied to every parameter it takes, a function is its body; its own parameter is no declared type.
    assert_eq!(
        unit.head(Term::apply(function(|_| nat()), [nat()])),
        Some(HeadKey::Nat)
    );
    assert_eq!(unit.head(Term::apply(function(|a| a), [nat()])), None);
}

#[test]
fn a_signature_spells_its_concept_and_every_parameter_behind_its_premises() {
    let unit = Unit::with([("Embeds", mention("Lift"))]);
    let binder = local(0, "A");
    let behind_premises = Term::func_type_marked(
        [(Plicity::Implicit, binder, Term::type_ground())],
        Term::apply(mention("Embeds"), [mention("Option"), nat()]),
    );

    assert_eq!(
        unit.key(&behind_premises),
        Some((
            named("Lift"),
            WitnessKey(vec![HeadKey::Nominal(named("Option")), HeadKey::Nat])
        ))
    );
}

/// A concept's parameter is written under the mark the concept declares it by, `Named(@Nat)` for a `Named(@A: Type)`.
#[test]
fn a_parameter_is_spelled_under_the_mark_its_concept_declares() {
    let unit = Unit::with([]);
    let marked = |concept: &str, mark| Term::apply_marked(mention(concept), [(mark, nat())]);

    assert_eq!(
        unit.key(&marked("Named", Plicity::Implicit)),
        Some((named("Named"), WitnessKey(vec![HeadKey::Nat])))
    );
    assert_eq!(unit.key(&marked("Named", Plicity::Explicit)), None);
    assert_eq!(unit.key(&marked("Show", Plicity::Implicit)), None);
}

#[test]
fn a_signature_that_applies_no_concept_at_its_arity_spells_no_key() {
    let unit = Unit::with([]);

    assert_eq!(unit.key(&mention("Show")), None);
    assert_eq!(unit.key(&Term::apply(mention("Option"), [nat()])), None);
    assert_eq!(unit.key(&Term::apply(mention("Lift"), [nat()])), None);
    assert_eq!(
        unit.key(&Term::apply(mention("Show"), [Term::type_ground()])),
        None
    );
}
