//! What a declaration under way needs of its unit: a read of one not yet elaborated, a witness at the key it is spelled at, and a name a proof applies.

use {
    super::test_support::{context, declare, named, unit_of_two, written},
    crate::*,
    curios_core::{
        ConceptDecl, Free, Global, HeadKey, InductDecl, StructDecl, Telescope, Term,
        UniverseContext, WitnessKey,
    },
    curios_utilities::Qualifier,
    std::collections::BTreeSet,
};

#[test]
fn removing_a_witness_hands_back_every_key_it_held() {
    let mut context = context();
    let concept = Global::Authored(Qualifier::from(["Show"]));
    let name = Global::Authored(Qualifier::from(["show"]));
    let witness = || Witness {
        name,
        module: Qualifier::empty(),
        universe_context: UniverseContext::empty(),
        signature: Term::type_ground(),
    };
    let nat = WitnessKey::from([HeadKey::Nat]);
    let bool_ = WitnessKey::from([HeadKey::Bool]);
    assert!(context.insert_witness(concept, nat, witness()).is_none());
    assert!(context.insert_witness(concept, bool_, witness()).is_none());

    let removed = context
        .remove_witness(&name)
        .into_iter()
        .collect::<BTreeSet<_>>();

    assert_eq!(removed, BTreeSet::from([(concept, nat), (concept, bool_)]));
    assert!(context.witness(&concept, &nat).is_none());
    assert!(context.witness(&concept, &bool_).is_none());
    assert!(context.remove_witness(&name).is_empty());
}

#[test]
fn a_read_of_a_declaration_that_has_not_elaborated_is_a_need() {
    let mut context = context();
    let [a, b] = unit_of_two(&mut context);
    context.attempt(&[&b]);

    assert!(context.assumption(&Free::from(&a)).is_none());

    assert_eq!(context.take_needs(), BTreeSet::from([a]));
}

/// A unit that writes a family, a record, a concept and then `reader`, none elaborated, the three entries registered as a unit's are before its first item: with no constructor, no field, and neither parameter nor field.
fn unit_of_entries(context: &mut Context) -> [Global; 4] {
    let names = [
        named("Family"),
        named("Record"),
        named("Concept"),
        named("reader"),
    ];
    let [family, record, concept, _] = names;
    context.begin_unit(written(&names), BTreeSet::from(names));
    context
        .register_induct(
            &family,
            InductDecl {
                universe_context: UniverseContext::default(),
                arity: Telescope::done(Telescope::done(())),
                constructors: Vec::new().into(),
                result_sort: Term::type_ground(),
                module: Qualifier::default(),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )
        .expect("a name is registered once");
    context
        .register_struct(
            &record,
            StructDecl {
                universe_context: UniverseContext::default(),
                arity: Telescope::done(Telescope::done(())),
                result_sort: Term::type_ground(),
                module: Qualifier::default(),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )
        .expect("a name is registered once");
    context
        .register_concept(
            &concept,
            ConceptDecl {
                universe_context: UniverseContext::default(),
                params: Telescope::done(()),
                fields: Vec::new(),
                supers: Vec::new(),
            },
        )
        .expect("a name is registered once");

    names
}

/// A registry entry read before its declaration has elaborated is the lowered one, so the read is a need, in each registry, as a missed name is.
///
/// Mutation-checked against each accessor's record: with one left out, its read needs nothing.
#[test]
fn a_registry_entry_read_before_its_declaration_has_finished_is_a_need() {
    let mut context = context();
    let [family, record, concept, reader] = unit_of_entries(&mut context);

    context.attempt(&[&reader]);
    assert!(context.induct_decl(&family).is_some());
    assert_eq!(context.take_needs(), BTreeSet::from([family]));

    context.attempt(&[&reader]);
    assert!(context.struct_decl(&record).is_some());
    assert_eq!(context.take_needs(), BTreeSet::from([record]));

    context.attempt(&[&reader]);
    assert!(context.concept(&concept).is_some());
    assert_eq!(context.take_needs(), BTreeSet::from([concept]));
}

/// A declaration reads its own entries without a need, and so does any declaration once the one that declares them has finished.
#[test]
fn a_declarations_own_entry_and_a_finished_one_are_read_without_a_need() {
    let mut context = context();
    let [family, record, concept, reader] = unit_of_entries(&mut context);
    let read = |context: &Context| {
        assert!(context.induct_decl(&family).is_some());
        assert!(context.struct_decl(&record).is_some());
        assert!(context.concept(&concept).is_some());
    };

    context.attempt(&[&family, &record, &concept]);
    read(&context);
    assert!(context.take_needs().is_empty());

    context.finish(&[&family, &record, &concept]);
    context.attempt(&[&reader]);
    read(&context);
    assert!(context.take_needs().is_empty());
}

/// A body asked for of a declaration that has not elaborated is a need, by each way a reduction asks: the name would reduce once its declaration has finished, so a reduction that goes stuck on it has read it.
///
/// Mutation-checked against each accessor's record: with one left out, its read needs nothing.
#[test]
fn a_body_read_of_a_declaration_that_has_not_elaborated_is_a_need() {
    let mut context = context();
    let [a, b] = unit_of_two(&mut context);
    let name = Free::from(&a);

    context.attempt(&[&b]);
    assert!(context.definition_body(&name).is_none());
    assert_eq!(context.take_needs(), BTreeSet::from([a]));

    context.attempt(&[&b]);
    assert!(context.var_reduct(&name).is_none());
    assert_eq!(context.take_needs(), BTreeSet::from([a]));

    context.attempt(&[&b]);
    assert!(context.var_reduct_at(&name).is_none());
    assert_eq!(context.take_needs(), BTreeSet::from([a]));

    // Its own body, and one that is missing for good once its declaration has finished, are no read of another.
    context.attempt(&[&a]);
    assert!(context.definition_body(&name).is_none());
    assert!(context.take_needs().is_empty());
    context.finish(&[&a]);
    context.attempt(&[&b]);
    assert!(context.definition_body(&name).is_none());
    assert!(context.var_reduct(&name).is_none());
    assert!(context.take_needs().is_empty());
}

/// A declaration's own name is bound by the group it becomes, so missing it is no read of another.
#[test]
fn a_declaration_reads_its_own_name_without_a_need() {
    let mut context = context();
    let [_, b] = unit_of_two(&mut context);
    context.attempt(&[&b]);

    assert!(context.assumption(&Free::from(&b)).is_none());

    assert!(context.take_needs().is_empty());
}

#[test]
fn a_finished_declaration_is_no_need_whatever_became_of_it() {
    let mut context = context();
    let [a, b] = unit_of_two(&mut context);
    context.finish(&[&a]);
    context.attempt(&[&b]);

    assert!(context.assumption(&Free::from(&a)).is_none());

    assert!(context.take_needs().is_empty());
}

/// An attempt begins with nothing needed, so what a void attempt read does not void the next.
#[test]
fn an_attempt_begins_with_nothing_needed() {
    let mut context = context();
    let [a, b] = unit_of_two(&mut context);
    context.attempt(&[&b]);
    context.need(a);

    context.attempt(&[&b]);

    assert!(context.take_needs().is_empty());
}

/// A proof the elaborator writes applies no name of its own unit until the unit's walk offers them, and then every one that elaborated, wherever each is written.
#[test]
fn a_proof_applies_a_name_of_its_unit_once_the_walk_offers_them() {
    let mut context = context();
    let [a, b] = unit_of_two(&mut context);
    for name in [&a, &b] {
        context.define_assuming(
            &Free::from(name),
            &Term::type_ground(),
            &Term::type_ground(),
            None,
        );
    }
    context.finish(&[&a, &b]);

    for declaration in [a, b] {
        context.enter_item(Some(declaration));
        assert!(!context.proof_may_apply(&a));
        assert!(!context.proof_may_apply(&b));
    }

    context.offer_lemmas();
    for declaration in [a, b] {
        context.enter_item(Some(declaration));
        assert!(context.proof_may_apply(&a));
        assert!(context.proof_may_apply(&b));
    }
}

/// A name of the unit that is not in scope is applied by no proof and needed by none, offered or not: the walk asked for the vocabulary before it offered any of it, so one that is missing was refused.
#[test]
fn a_proof_needs_no_name_of_its_unit() {
    let mut context = context();
    let [a, b] = unit_of_two(&mut context);
    context.attempt(&[&b]);
    context.enter_item(Some(b));

    assert!(!context.proof_may_apply(&a));
    context.offer_lemmas();
    assert!(!context.proof_may_apply(&a));

    assert!(context.take_needs().is_empty());
}

/// Whether a witness stands at a key is asked of every witness the unit declares, so the answer is one whichever of them has elaborated.
#[test]
fn a_declared_witness_stands_at_the_key_it_is_spelled_at_before_it_elaborates() {
    let mut context = context();
    let show = named("Show");
    declare(&mut context, "w", 0);

    assert!(context.witness_declared(&show, &WitnessKey::from([HeadKey::Nat])));
    assert!(!context.witness_declared(&show, &WitnessKey::from([HeadKey::Bool])));
    assert_eq!(
        context.witness_keys(&show),
        [(WitnessKey::from([HeadKey::Nat]), Qualifier::empty())]
    );
    assert!(context.take_needs().is_empty());
}

#[test]
fn a_goal_at_the_key_of_a_witness_that_has_not_elaborated_needs_it() {
    let mut context = context();
    let show = named("Show");
    let key = WitnessKey::from([HeadKey::Nat]);
    let witness = declare(&mut context, "w", 0);
    context.attempt(&[&named("user")]);

    assert!(context.witness_unfinished(&show, &key));
    assert_eq!(context.take_needs(), BTreeSet::from([witness]));
    assert!(!context.witness_unfinished(&show, &WitnessKey::from([HeadKey::Bool])));
    assert!(context.take_needs().is_empty());
}

/// A witness registers on its signature, so a miss on its own key is its signature asking for itself: no witness, and nothing to wait for.
#[test]
fn a_witness_does_not_need_itself() {
    let mut context = context();
    let witness = declare(&mut context, "w", 0);
    context.attempt(&[&witness]);

    assert!(!context.witness_unfinished(&named("Show"), &WitnessKey::from([HeadKey::Nat])));
    assert!(context.take_needs().is_empty());
}

/// The second of two witnesses spelled alike is the duplicate, whichever of them a goal reaches first.
#[test]
fn of_two_witnesses_spelled_alike_a_goal_needs_the_first_as_written() {
    let mut context = context();
    declare(&mut context, "a", 7);
    let first = declare(&mut context, "b", 2);
    context.attempt(&[&named("user")]);

    assert!(context.witness_unfinished(&named("Show"), &WitnessKey::from([HeadKey::Nat])));
    assert_eq!(context.take_needs(), BTreeSet::from([first]));
}

/// A refused or withheld witness leaves its key poisoned whether or not it had registered, and a finished one leaves it to the table.
#[test]
fn a_withdrawn_witness_poisons_the_key_it_is_spelled_at() {
    let mut context = context();
    let show = named("Show");
    let key = WitnessKey::from([HeadKey::Nat]);
    let refused = declare(&mut context, "w", 0);

    context.withdraw_declared_witness(&refused);

    assert!(context.is_poisoned_witness(&show, &key));
    assert!(!context.witness_declared(&show, &key));

    let mut context = self::context();
    let kept = declare(&mut context, "w", 0);
    context.finish(&[&kept]);

    assert!(!context.is_poisoned_witness(&show, &key));
    assert!(!context.witness_declared(&show, &key));
}

/// A name no declaration of the unit writes is another unit's, and is applied wherever it is in scope, before the unit's own are offered as after.
#[test]
fn a_proof_applies_another_units_name_wherever_it_is_in_scope() {
    let mut context = context();
    let [_, b] = unit_of_two(&mut context);
    let lemma = named("lemma");
    context.enter_item(Some(b));

    assert!(!context.proof_may_apply(&lemma));
    context.define_assuming(
        &Free::from(&lemma),
        &Term::type_ground(),
        &Term::type_ground(),
        None,
    );
    assert!(context.proof_may_apply(&lemma));
    assert!(context.take_needs().is_empty());
}

#[test]
fn a_forgotten_declaration_is_bound_nowhere() {
    let mut context = context();
    let name = Free::from(&Global::Authored(Qualifier::from(["gone"])));
    context.define_assuming(&name, &Term::type_ground(), &Term::type_ground(), None);
    context.assume_witness(&name, &Term::type_ground());

    context.forget(&name);

    assert!(context.assumption(&name).is_none());
    assert!(context.definition_body(&name).is_none());
    assert!(!context.locals().any(|(bound, _)| *bound == name));
    assert!(!context.witness_scope().any(|(bound, _)| *bound == name));
}
