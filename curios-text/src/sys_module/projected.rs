//! A `/sys` declaration's types, read off core's table.
//!
//! [`projection`] builds an entry's node over core variables, one per parameter, asks `Intrinsic::signature` what the node demands and produces, and reads the answer back as surface terms. Nothing here knows an operation: the walk is the table's, and the read-back is one rule per term former a signature states a type in.
//!
//! **Plicity is read off the row as well.** A type operand is one a call leaves to be inferred, a proof is one the elaborator discharges, and every other operand is written — except in a former, whose type operand is the one thing a reader writes, `List(Nat)`.
//!
//! A bound is read back over the nodes the row states it in, `Holds(Nat/lt(i, Bytes/len(b)))`, and not through the `/sys` wrappers a caller's guard is written with. A case equation answers its scrutinee's reduct, which is those nodes (`documentation/design/soundness/case-equations-and-their-key.md`), so a guard discharges the bound either way.

use {
    super::{applied, fn_over, intrinsic, name, type_, unit},
    crate::{Intrinsic, Name, Subterm, Term},
    curios_core::{Operand, Produced},
    curios_utilities::{Plicity, Qualifier, SyntaxRegistry},
};

/// What the table says of one node: each parameter's plicity and type, in the parameters' order, and the result.
pub(super) struct Projection {
    pub(super) params: Vec<(Plicity, Term)>,
    pub(super) output: Term,
}

/// The parameter a bare name operand is, by position.
fn parameter(params: &[&str], operand: &Term) -> Option<usize> {
    match operand.as_subterm() {
        Subterm::Name(name) if name.is_single() && !name.is_abs() => {
            params.iter().position(|param| *param == name.head())
        }
        _ => None,
    }
}

fn absolute(qualifier: &Qualifier) -> Term {
    Subterm::Name(Name::new(true, *qualifier)).into()
}

/// A type the table states, as the surface term a declaration would write: an intrinsic node over its operands read the same way, a parameter by its name, a registered declaration by its absolute path.
fn read(params: &[&str], term: &curios_core::Term) -> Option<Term> {
    let read_all = |terms: &mut dyn Iterator<Item = &curios_core::Term>| {
        terms
            .map(|term| read(params, term))
            .collect::<Option<Vec<_>>>()
    };

    Some(match &**term {
        curios_core::Subterm::Intrinsic(node) => intrinsic(
            node.try_map(&mut |operand| read(params, operand).ok_or(()))
                .ok()?,
        ),
        curios_core::Subterm::Var(var) => {
            let free = var.as_free()?;

            match free.local_index() {
                Some(index) => name(params.get(index as usize)?),
                None => match free.as_global()? {
                    curios_core::Global::Authored(qualifier) => absolute(qualifier),
                    curios_core::Global::Witness(_) => return None,
                },
            }
        }
        curios_core::Subterm::Apply(apply) => {
            let head = read(params, &apply.head)?;

            applied(head, read_all(&mut apply.params())?)
        }
        curios_core::Subterm::InductType(nominal) if nominal.indices.is_empty() => {
            let curios_core::Global::Authored(qualifier) = &nominal.name else {
                return None;
            };
            let family = absolute(qualifier);

            match nominal.params.is_empty() {
                true => family,
                false => applied(family, read_all(&mut nominal.params.iter())?),
            }
        }
        curios_core::Subterm::TupleType(tuple) if tuple.telescope.is_empty() => unit(),
        curios_core::Subterm::Type(_) => type_(),
        _ => return None,
    })
}

/// What core's table says of `node`, whose operands are the parameters `params` names. `None` where an operand is not a bare parameter, or where the table states a type this reads no rule for.
pub(super) fn projection(
    params: &[&str],
    node: &Intrinsic,
    syntax: &SyntaxRegistry,
) -> Option<Projection> {
    // The node over core variables: one local per parameter, at the parameter's position.
    let placed = node
        .try_map(&mut |operand| {
            parameter(params, operand)
                .map(|index| {
                    curios_core::Term::var(curios_core::Var::free(curios_core::Free::local(
                        index as u32,
                        None,
                    )))
                })
                .ok_or(())
        })
        .ok()?;
    let signature = placed.signature(syntax);
    let former = matches!(signature.produced, Produced::Sort);
    let holds = curios_core::Global::Authored(syntax.proof.holds.qualifier());
    // A proof is implicit: what a bound is stated in is `Holds` over a decision.
    let proof = |demanded: &curios_core::Term| match &**demanded {
        curios_core::Subterm::Apply(apply) => match &*apply.head {
            curios_core::Subterm::Var(var) => {
                var.as_free().and_then(curios_core::Free::as_global) == Some(&holds)
            }
            _ => false,
        },
        _ => false,
    };

    let mut telescope = params.iter().map(|_| None).collect::<Vec<_>>();
    for (operand, demand) in placed.operands().into_iter().zip(&signature.operands) {
        let curios_core::Subterm::Var(var) = &**operand else {
            return None;
        };
        let index = var.as_free()?.local_index()? as usize;
        let entry = match demand {
            Operand::IsType => (
                match former {
                    true => Plicity::Explicit,
                    false => Plicity::Implicit,
                },
                type_(),
            ),
            Operand::At(demanded) => (
                match proof(demanded) {
                    true => Plicity::Implicit,
                    false => Plicity::Explicit,
                },
                read(params, demanded)?,
            ),
            Operand::Function { domains, codomain } => (
                Plicity::Explicit,
                fn_over(
                    domains
                        .iter()
                        .map(|domain| read(params, domain))
                        .collect::<Option<_>>()?,
                    read(params, codomain)?,
                ),
            ),
        };
        *telescope.get_mut(index)? = Some(entry);
    }
    let output = match &signature.produced {
        Produced::Fixed(produced) => read(params, produced)?,
        Produced::Sort => type_(),
    };

    Some(Projection {
        params: telescope.into_iter().collect::<Option<_>>()?,
        output,
    })
}
