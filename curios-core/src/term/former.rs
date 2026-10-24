//! A type former short of its application: the η-form it is spelled by, what that is read back as, and the node it is applied to.

#[cfg(test)]
mod tests;

use crate::{Bound, Func, Global, InductType, Intrinsic, StructType, Subterm, Telescope, Term};

/// A recognized type-former eta shape: the former's identity, the arguments left once its binders are stripped, and the former's whole arity, against which those arguments' declared marks are read.
pub enum FormerEta {
    Nominal(Global, Vec<Term>, usize),
    Intrinsic(&'static str),
}

/// The type former a lambda is the η-form of, read on the *unopened* telescope: a lambda whose binders are the last arguments of a former's node in order — the nominal node, or a unary intrinsic carrier — with the arguments before them closed under the binders (`reach() == 0`), which is what guarantees the binders occur nowhere else.
///
/// **A lambda to a call.** A family takes its parameters in one call and its indices in another, so the lambdas a former short of its application is spelled by follow its calls: nested, each taking a whole call, `(T) => (n) => Vec(T)(n)` for the bare family and `(n) => Vec(A)(n)` for the family at its parameters. Of a former that takes one call, the lambda may take a suffix of it, `(x) => Either(E, x)`, the right-biased candidate an imitation builds. A lambda over binders of two calls, or over some of a family's indices, is no former: no call writes it.
///
/// **One recognizer, two readers.** The printers write such a lambda as the former it η-expands, and a term built as one applied over the whole of its first parameter list is built as what that application reduces to (`Term::from`), so the two agree on what a former short of its application is. The binders' plicities are not read: the η-forms this recognizes are a former's own — its entry's binders, or an imitation's, which copy the former's birth type — and a β-step reads none.
pub fn former_eta(telescope: &Telescope<Term>) -> Option<FormerEta> {
    // How many binders each lambda takes, outermost first, and the node the innermost ends in.
    let mut layers = vec![telescope.len()];
    let mut terminal = telescope.terminal();
    while let Subterm::Func(Func { telescope }) = &**terminal {
        layers.push(telescope.len());
        terminal = telescope.terminal();
    }
    if layers.contains(&0) {
        return None;
    }
    let binders = layers.iter().sum::<usize>();

    // The node's arguments in the order its calls take them, where each call's begin, and what it is a node of. A family takes its indices in a call of their own.
    let (former, arguments, starts) = match &**terminal {
        Subterm::InductType(InductType {
            name,
            params,
            indices,
            ..
        }) => (
            FormerEta::Nominal(*name, Vec::new(), params.len() + indices.len()),
            params.iter().chain(indices).collect::<Vec<_>>(),
            match (params.is_empty(), indices.is_empty()) {
                (false, false) => vec![0, params.len()],
                _ => vec![0],
            },
        ),
        Subterm::StructType(StructType { name, params, .. }) => (
            FormerEta::Nominal(*name, Vec::new(), params.len()),
            params.iter().collect(),
            vec![0],
        ),
        Subterm::Intrinsic(Intrinsic::IoType(payload)) => {
            (FormerEta::Intrinsic("Io"), vec![payload], vec![0])
        }
        Subterm::Intrinsic(Intrinsic::ListType(payload)) => {
            (FormerEta::Intrinsic("List"), vec![payload], vec![0])
        }
        Subterm::Intrinsic(Intrinsic::ChannelType(payload)) => {
            (FormerEta::Intrinsic("Channel"), vec![payload], vec![0])
        }
        Subterm::Intrinsic(Intrinsic::CellType(payload)) => {
            (FormerEta::Intrinsic("Cell"), vec![payload], vec![0])
        }
        _ => return None,
    };
    // Whether the former takes one call and it takes parameters, a suffix of which the outermost lambda may take.
    let one_call_of_parameters = starts.len() == 1
        && !matches!(&**terminal, Subterm::InductType(InductType { indices, .. }) if !indices.is_empty());
    let (prefix, trailing) = arguments.split_at(arguments.len().checked_sub(binders)?);

    // Outermost binder first: under `binders` binders the first is bound at `binders - 1` and the last at `0`.
    let in_order = trailing.iter().enumerate().all(|(position, term)| {
        matches!(&***term, Subterm::Var(var) if var.as_bound() == Some(binders - 1 - position))
    });
    let closed = prefix.iter().all(|term| term.reach() == 0);
    // Each lambda, innermost first, takes a whole call: the outermost may instead take a suffix of a former's one call of parameters.
    let mut end = arguments.len();
    let follows_calls = layers.iter().enumerate().rev().all(|(layer, &taken)| {
        let begin = end - taken;
        let next = starts
            .iter()
            .copied()
            .find(|&start| start > begin)
            .unwrap_or(arguments.len());
        let whole = starts.contains(&begin) && end == next;
        let suffix = layer == 0 && one_call_of_parameters;
        end = begin;
        whole || suffix
    });
    if !(in_order && closed && follows_calls) {
        return None;
    }

    Some(match former {
        FormerEta::Nominal(name, _, arity) => FormerEta::Nominal(
            name,
            prefix.iter().map(|&term| term.clone()).collect(),
            arity,
        ),
        intrinsic @ FormerEta::Intrinsic(_) => intrinsic,
    })
}

/// `subterm` as a term is built from it: itself, or, where it applies a type former's η-form ([`former_eta`]) over the whole of the lambda's parameter list, the node or the narrower η-form that application reduces to.
///
/// **The one β-step taken where a term is made.** A former short of its application is spelled by its η-form, and an application of one is reached by elaboration, by a substitution putting an η-form at a head and by an imitation's solution alike. Taken here, where every term a traversal, a substitution or a reduction rebuilds is made (`Term::rebuilt`), a former applied in full is one node however its application was built, as GHC's `mkAppTy` keeps a saturated `TyConApp` visibly so through a substitution. A β-step changes no term's meaning, and it is taken of an η-form alone, whose application is the node it holds.
pub(super) fn built(subterm: Subterm) -> Subterm {
    let Subterm::Apply(apply) = &subterm else {
        return subterm;
    };
    let Subterm::Func(Func { telescope }) = &*apply.head else {
        return subterm;
    };
    if telescope.len() != apply.arguments.len() || former_eta(telescope).is_none() {
        return subterm;
    }
    let arguments = apply.params().collect::<Vec<_>>();
    Term::unwrap_or_clone(telescope.open(&arguments))
}
