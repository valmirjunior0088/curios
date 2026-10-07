//! Folding an unfolded top-level `rec` back into its definitions' names: for display ([`refold_recs`]), and exactly, for a term the elaborator commits ([`named_members`]).
//!
//! A mismatch report normalizes both sides so the disagreement is visible, and normalizing `double(p + 1)` unfolds the global through its structural group, which carries no name: the report would then spell `rec #0: (n: Nat) -> Nat = n => …; #0(p) + 2` where the author wrote `double`. [`refold_recs`] recognizes a `Rec` node whose group is one the context defined — compared projected, so an instance of a universe-polymorphic group is the group it instantiates — and spells its tail with each member as the definition it is: `double(p) + 2`. This lives here rather than in the printer because the group to recognize is the *elaborated* one, which only the context holds; the lowered module the formatter has is a different structure, and on an error path there is no elaborated module at all.
//!
//! A witness is not folded here. How one reads — left out where resolution would restore it, a method projected off it spelled as its call or operator — depends on the witness binders in scope at each node, which only the printer's walk sees; that is axis (h) of `curios-core`'s printer.

#[cfg(test)]
mod tests;

use {
    super::Context,
    curios_core::{
        Bound, Enter, Free, Global, InstanceHead, Rec, RecGroup, Subterm, Term, Var, Visit,
    },
    std::{collections::HashSet, rc::Rc},
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

/// `term` with every member of a top-level `rec` group it holds spelled by the member's name, at the universe instance it stands at: how a term the elaborator commits — a metavariable's solution, a proof it wrote — names a recursive definition it calls.
///
/// Reduction opens a name to its group to see whether a call computes, and a call that does not compute stays headed by the group. Committed so, a solution holds a copy of the callee: re-validating it elaborates the callee's every member again, the kernel is handed the copy, and a copy that elaboration rebuilt under another spelling of one of its goals is another term to conversion, which unfolds the two side by side. The name is what the author wrote, and reduction opens it to the same group again.
///
/// Exact, where [`refold_recs`] is display's: a group is recognized by itself, and an instance of a universe-polymorphic one by reduction's own record of having opened it ([`Context::instance_of`]), so the levels a member is named at are the ones it was opened at and none is read back off the copy. A group neither recognizes — a local `rec`, an instance opened in another declaration — stands as it was.
pub(crate) fn named_members(context: &Context, term: &Term) -> Term {
    if !term.mentions_rec() {
        return term.clone();
    }
    // The groups the term holds, each beside the terms naming its members, gathered before the rewrite, which owns what it reads. A node is entered once, and a recognized group is not entered at all: its members name one another by position.
    let mut found: (HashSet<Term>, Vec<(RecGroup, Vec<Term>)>) = (HashSet::new(), Vec::new());
    term.walk(
        &mut found,
        |(seen, groups), node| {
            if !seen.insert(node.clone()) {
                return Enter::Skip(());
            }
            let Subterm::Rec(Rec { group, .. }) = &**node else {
                return Enter::Descend;
            };
            if groups.iter().any(|(known, _)| known == group) {
                return Enter::Skip(());
            }
            match members_named(context, group) {
                Some(members) => {
                    groups.push((group.clone(), members));
                    Enter::Skip(())
                }
                None => Enter::Descend,
            }
        },
        |_, _, _| (),
    );
    let (_, groups) = found;
    if groups.is_empty() {
        return term.clone();
    }
    named_with(&Rc::new(groups), term)
}

/// The terms naming `group`'s members, in member order: each member's global where the base frame defines the group itself, and that global at the instance's levels where `group` is an instance reduction opened.
fn members_named(context: &Context, group: &RecGroup) -> Option<Vec<Term>> {
    let named = |name: &Global| Var::free(Free::Global(*name));
    if group.universes().parameter_count == 0
        && let Some(names) = context.member_names(group)
    {
        return Some(names.iter().map(|name| Term::var(named(name))).collect());
    }
    let (scheme, levels) = context.instance_of(group)?;
    let names = context.member_names(scheme)?;
    Some(
        names
            .iter()
            .map(|name| Term::instance(InstanceHead::Var(named(name)), levels.clone()))
            .collect(),
    )
}

fn named_with(table: &Rc<Vec<(RecGroup, Vec<Term>)>>, term: &Term) -> Term {
    let captured = Rc::clone(table);
    // Memoized on node identity, as the display's refold is and for its reason.
    let mut visit = Visit::rewriting_shared(
        |_, _| None,
        Box::new(move |_, term| {
            let Subterm::Rec(Rec { group, tail }) = &**term else {
                return None;
            };
            let (_, members) = captured.iter().find(|(known, _)| known == group)?;
            let refs = members.iter().collect::<Vec<_>>();
            // The opened tail is named in turn, since a substituted node is not descended into.
            Some(named_with(&captured, &tail.open(&refs)))
        }),
    );
    term.traverse(&mut visit)
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
