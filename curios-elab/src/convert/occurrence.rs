//! Reading a term for the shapes solving branches on: an abstraction of a subject's occurrences, and the three metavariable tests that decide whether a problem can be attempted at all.

use {
    super::*,
    curios_core::{Free, Metavar, Subterm, Term},
};

/// Replace every occurrence of a subject term in `t` — matched by the same term equality conversion uses, at any depth (binder names are entropy-fresh, so a free-named subject cannot be captured by an inner scope) — with its birth binder's name. Top-down: an outer match wins and is not descended into. Subjects are pairwise distinct by construction, so the match is unambiguous.
pub(super) fn abstract_occurrences(t: &Term, subjects: &[(Term, Free)]) -> Term {
    if let Some((_, name)) = subjects.iter().find(|(s, _)| s == t) {
        return Term::free_var(name);
    }

    let owned = subjects.to_vec();
    t.traverse(&mut Visit::rewriting(
        |_, _| None,
        Box::new(move |_, term: &Term| {
            owned
                .iter()
                .find(|(s, _)| s == term)
                .map(|(_, name)| Term::free_var(name))
        }),
    ))
}

/// `Some(metavar)` iff `term` is an unsolved bare metavariable head. (`reduce` already resolves solved metavariables, so a metavariable surviving to weak-head normal form is necessarily unsolved.)
pub(super) fn as_metavar(term: &Term) -> Option<&Metavar> {
    match &**term {
        Subterm::Metavar(metavar) => Some(metavar),
        _ => None,
    }
}

/// `true` iff a match scrutinee is stuck on a metavariable itself — bare, or an application headed by one. This is the side that could not reduce, so its stuck layer may trail the partner's; decomposing across that misalignment is what the match-match gate above forbids.
pub(super) fn flex_scrutinee(term: &Term) -> bool {
    match &**term {
        Subterm::Metavar(_) => true,
        Subterm::Apply(apply) => matches!(&*apply.head, Subterm::Metavar(_)),
        _ => false,
    }
}

/// `true` iff `term` — already in weak-head normal form, so anything foldable has folded — is stuck on an unsolved metavariable: an intrinsic operation still carrying one, a match whose node mentions one (a metavariable in the scrutinee may fold the match once solved; one in an arm may make the arm compare equal), or an elimination — application, projection — headed by such a shape or by an unsolved metavariable itself. A structural mismatch against such a term is undecided rather than definite, so the drain parks it watching the metavariables instead of failing; a stuck comparison with *no* unsolved metavariable anywhere in these positions keeps its hard mismatch, since nothing could ever wake it.
///
/// The metavariable-headed application is the case [`Convert::imitate_flex_apply`] states the rule for and only covers against a nominal type: `?f(a) ≡ t` against a *value* `t` has no imitation to try, but refuting nothing proves nothing either — a constant solution could still exist, and pinning `a` later may expose an abstraction. It used to fall through to a hard mismatch here, which is what made `Eq/cong(?, h)` report "type mismatch" for a hole standing exactly where the function belongs.
pub(super) fn blocked_on_unsolved_metavar(context: &Context, term: &Term) -> bool {
    let has_unsolved = |term: &Term| {
        term.metavars()
            .iter()
            .any(|id| context.metavar_solution(*id).is_none())
    };

    match &**term {
        Subterm::Metavar(metavar) => context.metavar_solution(metavar.id).is_none(),
        Subterm::Intrinsic(_) | Subterm::Match(_) => has_unsolved(term),
        Subterm::Apply(apply) => blocked_on_unsolved_metavar(context, &apply.head),
        Subterm::Proj(proj) => blocked_on_unsolved_metavar(context, &proj.head),
        _ => false,
    }
}
