//! Display-only folding of elaboration internals back into source-shaped spelling.
//!
//! A mismatch report normalizes both sides so the disagreement is visible, and normalizing `double(p + 1)` unfolds the global through its structural group, which carries no name: the report then spelled `rec #0: (n: Nat) -> Nat = n => …; #0(p) + 2` where the author wrote `double`. [`refold_recs`] recognizes a `Rec` node whose group is one the context defined — compared projected, so an instance of a universe-polymorphic group is the group it instantiates — and spells its tail with each member as the definition it is: `double(p) + 2`. This lives here rather than in the printer because the group to recognize is the *elaborated* one, which only the context holds; the lowered module the formatter has is a different structure, and on an error path there is no elaborated module at all.
//!
//! A witness is not folded here. How one reads — left out where resolution would restore it, a method projected off it spelled as its call or operator — depends on the witness binders in scope at each node, which only the printer's walk sees; that is axis (h) of `curios-core`'s printer.

#[cfg(test)]
mod tests;

use {
    super::Context,
    curios_core::{Bound, Free, Global, Rec, RecGroup, Subterm, Term, Var, Visit},
    std::rc::Rc,
};

/// Spell every unfolded top-level `rec` in `term` by its definitions' names. Display-only — see the module documentation. The table of groups is gathered only when the term holds a `Rec` node at all, which a mismatch rarely does.
pub(crate) fn refold_recs(context: &Context, term: &Term) -> Term {
    if !term.mentions_rec() {
        return term.clone();
    }
    let table: Rc<Vec<(RecGroup, Vec<Global>)>> = Rc::new(
        context
            .rec_definitions()
            .into_iter()
            .map(|(group, names)| (group.projected(), names))
            .collect(),
    );
    refold_with(&table, term)
}

fn refold_with(table: &Rc<Vec<(RecGroup, Vec<Global>)>>, term: &Term) -> Term {
    let captured = Rc::clone(table);
    // Memoized on node identity: a refold reads the node and the fixed table alone, and a report can materialize a reduct, whose tree can be exponential in its depth.
    let mut visit = Visit::rewriting_shared(
        |_, _| None,
        Box::new(move |_, term| refold_node(&captured, term)),
    );
    term.traverse(&mut visit)
}

/// The node-level refold: a `Rec` whose group the table knows opens its tail over the members' names, and the opened tail is refolded in turn, since a substituted node is not descended into.
fn refold_node(table: &Rc<Vec<(RecGroup, Vec<Global>)>>, term: &Term) -> Option<Term> {
    let Subterm::Rec(Rec { group, tail }) = &**term else {
        return None;
    };
    let projected = group.projected();
    let (_, names) = table.iter().find(|(known, _)| *known == projected)?;
    if names.len() != group.length() {
        return None;
    }
    let members = names
        .iter()
        .map(|name| Term::var(Var::free(Free::Global(*name))))
        .collect::<Vec<_>>();
    let refs = members.iter().collect::<Vec<_>>();
    Some(refold_with(table, &tail.open(&refs)))
}
