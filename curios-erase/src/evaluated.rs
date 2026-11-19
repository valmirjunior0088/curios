//! An item evaluated where it is erased.
//!
//! **A top-level value is computed where its unit is compiled.** An item whose top-level statements apply something erases to a fragment that computes when a program starts, so every program its unit is in scope of would link it, with what it reaches, to run it or to find that nothing observes it. What such a statement computes is the item's and what it reaches to say, and no program's. So it is computed here, once: the fragment is linked last into a module of what it reaches, the closed applications among its items are folded as a program's are ([`curios_anf::evaluate_since`]), and what is left is taken back out as the item's erased form ([`curios_anf::Module::taken`]). A form that computes nothing is linked by the programs that name it and by no other.
//!
//! **A name is bound to what its item computed.** A finished statement is an alias of its result, and the name the statement bound is bound to that result: a constant, which an item that names it reads in place; a function, which one applies as it applies any; or a value. So a closure one item computed is a function another can import by the first one's name, and what a later item's evaluation computes over it names it so.
//!
//! **An item is left as it was erased where evaluating it does not finish.** One the evaluator gives up on, for fuel, depth or size, is tried again by each program's own rounds. And a result that names what another item binds under no name has nothing to import it by. Each leaves an item that still computes.
//!
//! **Whether what it left is observable is decided here too**, by the rule a program's prune keeps an item by ([`curios_anf::observable_since`]), asked in the module it was evaluated in, its functions marked total by the records of its unit and of the units in scope as a program marks them. What a top-level value can do that a program observes is not end, so one that is observable is a computation not known to end, which the language runs as every program its unit is in scope of starts, whether or not the program names it: it is what a program links for its own sake. One that is not is linked by the programs that name it, and dropped by the prune of every one that does not.
//!
//! **What evaluating it read is every erased form it was linked after, whole.** An erased form is otherwise read for how a name is bound; an evaluation runs what the items it reaches define. So the item's record names each of them, its forms read whole ([`Part::Forms`](crate::Part::Forms)), whether or not the evaluation finished, since another form of one may be what lets it finish: an item is erased and evaluated again where one of them is another form. The module it ran in holds those items and no other, so the list is what was linked. Nothing is recorded of a fixed unit's, which is the compiler's own.
//!
//! **And, where it did not finish, the verdict of each name a marking read.** Whether what is left is observable turns on the totality the records state of what it runs, which can move while every form stays the bytes it was: a termination argument is erased. So the record names the signature of each name the marking asked, which holds its totality ([`Part::Signature`](crate::Part::Signature)), and the item is decided again where one moves.
//!
//! **An item that declares a row is not evaluated.** What is taken back out holds a schema as an import holds one, and a declared row is its fields.

use {
    crate::{ErasedItem, ErasedUnit, Erasures, Linking, Sorts, reached},
    std::collections::BTreeSet,
};

/// What evaluating an item where it is erased came to.
pub struct Evaluation {
    /// The item's erased form with what its top-level statements compute in their place, where that leaves it computing nothing.
    pub erased: Option<ErasedItem>,
    /// Whether what the evaluation left computing is observable: none where it finished.
    pub observable: bool,
    /// A name of each item the evaluation was linked after, of a unit that is not fixed.
    pub ran: Vec<curios_core::Global>,
    /// Each name whose totality deciding [`Evaluation::observable`] read: none where it finished.
    pub judged: BTreeSet<curios_core::Global>,
}

/// Evaluate `erased`, the erased form at `sorts` of the next item of `unit`, over what it reaches of `unit` and of `scope`, `own` being the record of `unit`'s definitions. `None` for a form that computes nothing as it was erased, or that declares a row: there is nothing to evaluate, and nothing is read.
pub fn evaluate(
    scope: Erasures<'_>,
    (unit, own): (&ErasedUnit, &curios_core::Certification),
    (erased, sorts): (&ErasedItem, Sorts),
) -> Option<Evaluation> {
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
        .flat_map(|(_, (unit, items))| {
            items
                .iter()
                .map(|&(place, form)| unit.item(place).form(form).0)
        })
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
    let landed = linking.add(erased, &[sorts]);
    let erased = linking
        .evaluate_since(&mark)
        .then(|| linking.taken(&units, &mark, erased, &landed))
        .flatten();
    curios_profile::sample!("evaluate::item::finished", erased.is_some() as usize);
    let (observable, judged) = match erased {
        Some(_) => (false, BTreeSet::new()),
        None => {
            for certification in scope.certifications().iter().copied().chain([own]) {
                linking.mark_total(certification);
            }

            (linking.observable_since(&mark), linking.evaluated_names())
        }
    };
    curios_profile::sample!("evaluate::item::observable", observable as usize);

    Some(Evaluation {
        erased,
        observable,
        ran,
        judged,
    })
}
