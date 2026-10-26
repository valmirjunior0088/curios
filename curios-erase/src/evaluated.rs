//! An item evaluated where it is erased.
//!
//! **A top-level value is computed where its unit is compiled.** An item whose top-level statements apply something erases to a fragment that computes when a program starts, so every program its unit is in scope of would link it, with what it reaches, to run it or to find that nothing observes it. What such a statement computes is the item's and what it reaches to say, and no program's. So it is computed here, once: the fragment is linked last into a module of what it reaches, the closed applications among its items are folded as a program's are ([`curios_anf::evaluate_since`]), and what is left is taken back out as the item's erased form ([`curios_anf::Module::taken`]). A form that computes nothing is linked by the programs that name it and by no other.
//!
//! **A name is bound to what its item computed.** A finished statement is an alias of its result, and the name the statement bound is bound to that result: a constant, which an item that names it reads in place; a function, which one applies as it applies any; or a value. So a closure one item computed is a function another can import by the first one's name, and what a later item's evaluation computes over it names it so.
//!
//! **An item is left as it was erased where evaluating it does not finish.** A statement that performs an effect stays the call it was, which the language runs when a program starts. One the evaluator gives up on, for fuel, depth or size, is tried again by each program's own rounds. And a result that names what another item binds under no name has nothing to import it by. Each leaves an item that still computes, which a program links as it links any such.
//!
//! **What evaluating it read is every erased form it was linked after, whole.** An erased form is otherwise read for how a name is bound; an evaluation runs what the items it reaches define. So the item's trace names each of them (`Question::Ran`), whether or not the evaluation finished, since another form of one may be what lets it finish: an item is erased and evaluated again where one of them is another form. The module it ran in holds those items and no other, so the list is what was linked. Nothing is recorded of a fixed unit's, which is the compiler's own.
//!
//! **An item that declares a row is not evaluated.** What is taken back out holds a schema as an import holds one, and a declared row is its fields.

use crate::{ErasedItem, ErasedUnit, Erasures, Linking, reached};

/// What evaluating an item where it is erased came to.
pub struct Evaluation {
    /// The item's erased form with what its top-level statements compute in their place, where that leaves it computing nothing.
    pub erased: Option<ErasedItem>,
    /// A name of each item the evaluation was linked after, of a unit that is not fixed.
    pub ran: Vec<curios_core::Global>,
}

/// Evaluate `erased`, the erased form of the next item of `unit`, over what it reaches of `unit` and of `scope`. `None` for an item that computes nothing as it was erased, or that declares a row: there is nothing to evaluate, and nothing is read.
pub fn evaluate(scope: Erasures<'_>, unit: &ErasedUnit, erased: &ErasedItem) -> Option<Evaluation> {
    if !erased.module().computes() || erased.environment().declares_rows() {
        return None;
    }
    curios_profile::profile!("evaluate_item");

    let units: Vec<&ErasedUnit> = scope.units().iter().copied().chain([unit]).collect();
    let reached = reached(&units, [erased].into_iter(), false);
    let ran = units
        .iter()
        .zip(reached.items())
        .enumerate()
        .filter(|(place, _)| *place >= scope.units().len() || !scope.is_fixed(*place))
        .flat_map(|(_, (unit, items))| items.iter().map(|&place| unit.item(place)))
        .map(|item| {
            let (name, _) = item
                .environment()
                .bindings()
                .next()
                .expect("an item's erased form binds the names it declares");

            *name
        })
        .collect::<Vec<_>>();
    curios_profile::sample!(
        "evaluate::item::linked",
        reached
            .items()
            .iter()
            .map(|items| items.len())
            .sum::<usize>()
    );

    let mut linking = Linking::importing_rows();
    linking.add_reached(&units, &reached);
    let mark = linking.mark();
    let landed = linking.add(erased);
    let erased = linking
        .evaluate_since(&mark)
        .then(|| linking.taken(&units, &mark, erased, &landed))
        .flatten();
    curios_profile::sample!("evaluate::item::finished", erased.is_some() as usize);

    Some(Evaluation { erased, ran })
}
