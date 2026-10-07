//! Spike: a `/sys` declaration's types read off core's table rather than restated.
//!
//! [`projection`] builds an entry's node over core variables, one per parameter, asks `Intrinsic::signature` what it demands and produces, and reads the answer back as surface terms. Nothing here knows an operation: the walk is the table's, and the read-back is one rule per term former a signature uses.

use {
    super::{applied, fn_over, intrinsic, name, type_, unit},
    crate::{LetSignature, Module, Name, Pattern, Subterm, Term, TopItem},
    curios_core::{Intrinsic, Operand, Produced},
    curios_utilities::{Plicity, Qualifier, SyntaxRegistry},
};

/// What the table says of one node: each parameter's plicity and type, in the parameters' order, and the result.
pub(super) struct Projection {
    pub(super) params: Vec<(Plicity, Term)>,
    pub(super) output: Term,
}

/// The parameter a bare name operand is, by position.
fn parameter(params: &[String], operand: &Term) -> Option<usize> {
    match operand.as_subterm() {
        Subterm::Name(name) if name.is_single() && !name.is_abs() => {
            params.iter().position(|param| param == name.head())
        }
        _ => None,
    }
}

fn absolute(qualifier: &Qualifier) -> Term {
    Subterm::Name(Name::new(true, *qualifier)).into()
}

/// A type the table states, as the surface term a declaration would write: an intrinsic node over its operands read the same way, a parameter by its name, a registered declaration by its absolute path.
fn read(params: &[String], term: &curios_core::Term) -> Option<Term> {
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
    params: &[String],
    node: &Intrinsic<Term>,
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

/// Every declaration of `module` that is one node over its own parameters, its types replaced by what the table says of that node.
pub(super) fn project_types(module: &mut Module, syntax: &SyntaxRegistry) {
    project_items(&mut module.items, syntax);
}

fn project_items(items: &mut [TopItem], syntax: &SyntaxRegistry) {
    for item in items {
        match item {
            TopItem::Mod(module) => {
                if let Some(inner) = &mut module.module {
                    project_items(&mut inner.items, syntax);
                }
            }
            TopItem::Let(bindings) => {
                for binding in bindings {
                    match &mut binding.signature {
                        LetSignature::Func {
                            params,
                            output,
                            body,
                        } => {
                            let Subterm::Operation(node) = body.as_subterm() else {
                                continue;
                            };
                            let names = params
                                .iter()
                                .map(|param| match &param.binder {
                                    Some(Pattern::Binder(label)) => label.as_str().to_string(),
                                    _ => String::new(),
                                })
                                .collect::<Vec<_>>();
                            let Some(projected) = projection(&names, node, syntax) else {
                                continue;
                            };

                            for (param, (plicity, type_)) in params.iter_mut().zip(projected.params)
                            {
                                param.plicity = plicity;
                                param.type_ = type_;
                            }
                            *output = projected.output;
                        }
                        LetSignature::Name {
                            type_: Some(output),
                            body,
                        } => {
                            let Subterm::Operation(node) = body.as_subterm() else {
                                continue;
                            };
                            if let Some(projected) = projection(&[], node, syntax) {
                                *output = projected.output;
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }
}

/// One written declaration beside what the table says of it.
#[cfg(test)]
pub(super) struct Compared {
    pub(super) path: String,
    pub(super) parameters: usize,
    /// `None` where the entry is not one node over its own parameters.
    pub(super) differences: Option<Vec<Difference>>,
}

/// One place a written declaration and the table disagree.
#[cfg(test)]
pub(super) struct Difference {
    /// The parameter, or `result`.
    pub(super) subject: String,
    /// Whether this is the type of a proof both hold implicit: the one place the roster spells what the table states another way.
    pub(super) bound: bool,
    pub(super) written: String,
    pub(super) table: String,
}

/// Every `let` of `module` whose body is one intrinsic node, compared with its projection.
#[cfg(test)]
pub(super) fn compare(module: &Module, syntax: &SyntaxRegistry) -> Vec<Compared> {
    let mut compared = Vec::new();
    walk(&module.items, "/sys", syntax, &mut compared);

    compared
}

#[cfg(test)]
fn walk(items: &[TopItem], prefix: &str, syntax: &SyntaxRegistry, out: &mut Vec<Compared>) {
    for item in items {
        match item {
            TopItem::Mod(module) => {
                if let Some(inner) = &module.module {
                    let prefix = format!("{prefix}/{}", module.label.as_str());
                    walk(&inner.items, &prefix, syntax, out);
                }
            }
            TopItem::Let(bindings) => {
                for binding in bindings {
                    let (written, output, body) = match &binding.signature {
                        LetSignature::Func {
                            params,
                            output,
                            body,
                        } => (
                            params
                                .iter()
                                .map(|param| {
                                    let label = match &param.binder {
                                        Some(Pattern::Binder(label)) => label.as_str().to_string(),
                                        _ => String::new(),
                                    };
                                    (label, param.plicity, param.type_.clone())
                                })
                                .collect::<Vec<_>>(),
                            output.clone(),
                            body,
                        ),
                        LetSignature::Name {
                            type_: Some(output),
                            body,
                        } => (Vec::new(), output.clone(), body),
                        _ => continue,
                    };
                    let Subterm::Operation(node) = body.as_subterm() else {
                        continue;
                    };
                    let names = written
                        .iter()
                        .map(|(label, _, _)| label.clone())
                        .collect::<Vec<_>>();
                    let differences = projection(&names, node, syntax).map(|projected| {
                        let mut differences = Vec::new();
                        for ((label, plicity, type_), (projected_plicity, projected_type)) in
                            written.iter().zip(&projected.params)
                        {
                            if plicity != projected_plicity {
                                differences.push(Difference {
                                    subject: label.clone(),
                                    bound: false,
                                    written: format!("{plicity:?}"),
                                    table: format!("{projected_plicity:?}"),
                                });
                            }
                            if type_ != projected_type {
                                differences.push(Difference {
                                    subject: label.clone(),
                                    bound: *plicity == Plicity::Implicit
                                        && *projected_plicity == Plicity::Implicit,
                                    written: type_.to_string(),
                                    table: projected_type.to_string(),
                                });
                            }
                        }
                        if output != projected.output {
                            differences.push(Difference {
                                subject: "result".to_string(),
                                bound: false,
                                written: output.to_string(),
                                table: projected.output.to_string(),
                            });
                        }
                        differences
                    });

                    out.push(Compared {
                        path: format!("{prefix}/{}", binding.label.as_str()),
                        parameters: written.len(),
                        differences,
                    });
                }
            }
            _ => {}
        }
    }
}
