//! The elaborator's driver for the shared variance analysis: when it runs, and where its vectors live.
//!
//! The analysis itself lives in `curios-analysis/src/variance.rs` (see that module for the rule and its rationale) and is run by both checkers. It runs here twice for each declaration, for one reason each. When a family's group finalizes, because a later item of the same declaration may compare two of its instances: that vector is kept on the context's registry entry, and it is the one the unit carries. And as the declaration is published, where its telescopes are final and meta-free: that run writes nothing and holds the carried vector to what the declaration says.
//!
//! The same walk reads a definition, once, where its levels have closed ([`definition_variances`]): which of them its value holds. That vector is the elaborator's alone, since the kernel decides two instances of a definition by unfolding them, so nothing holds it to a second reading.

use {
    super::{Context, Error, Published, zonk_solved_term_metas},
    curios_analysis::{Declarations, Passed, VarianceVectors, held_levels, variance_vectors},
    curios_core::{
        Families, Func, FuncType, Global, InductDecl, InductParam, LevelHead, StructDecl, Subterm,
        Term, UniverseContext, UniverseParam, Variance, universe_params,
    },
    std::collections::BTreeMap,
};

/// The registries as conversion's level alignment reads them: a family's vector is the one its entry holds while the unit elaborates, empty for a family still inside its own group, so every level of it is compared there.
impl Families for Context {
    fn induct(&self, name: &Global) -> Option<&InductDecl> {
        self.induct_decl(name)
    }

    fn struct_(&self, name: &Global) -> Option<&StructDecl> {
        self.struct_decl(name)
    }
}

/// Record each of `names`' variance vectors on its registry entry: the families of one group, analyzed together as that group finalizes, so a later item that compares two of their instances reads them.
///
/// Every earlier family's vector answers from the registry. A solved metavariable is spliced in first; one still unsolved is read as mentioning every level, its solution being free to. A reduction the budget refuses reads the same way, and is no error here: the vector it leaves claims less.
pub(crate) fn record_variances(context: &mut Context, names: &[Global]) {
    curios_profile::profile!("record_variances");
    let mut inducts = BTreeMap::new();
    let mut structs = BTreeMap::new();
    for name in names {
        if let Some(declaration) = context.induct_decl(name).cloned() {
            let spliced = InductDecl {
                arity: zonk_solved_term_metas(context, &declaration.arity),
                constructors: declaration
                    .constructors
                    .iter()
                    .map(|(tag, constructor)| {
                        (
                            tag.clone(),
                            InductParam {
                                telescope: zonk_solved_term_metas(context, &constructor.telescope),
                            },
                        )
                    })
                    .collect(),
                ..declaration
            };
            inducts.insert(*name, spliced);
        } else if let Some(declaration) = context.struct_decl(name).cloned() {
            let spliced = StructDecl {
                arity: zonk_solved_term_metas(context, &declaration.arity),
                ..declaration
            };
            structs.insert(*name, spliced);
        }
    }

    let VarianceVectors { vectors, .. } =
        variance_vectors(context, Declarations::of(&inducts, &structs));
    for (name, vector) in vectors {
        if let Some(declaration) = context.induct_decl(&name).cloned() {
            context.update_induct(
                &name,
                InductDecl {
                    variances: vector,
                    ..declaration
                },
            );
        } else if let Some(declaration) = context.struct_decl(&name).cloned() {
            context.update_struct(
                &name,
                StructDecl {
                    variances: vector,
                    ..declaration
                },
            );
        }
    }
}

/// Which of a definition's universe parameters its value holds, read where its levels have closed: one entry per parameter of `universe_context`, none for a definition that takes none.
///
/// **A definition that answers a type is walked.** Its body is applied at as many fresh binders as the outer telescope of its type holds and read as a family's telescopes are ([`held_levels`]). Forcing the application opens the lambdas the body is written as, so a parameter's annotation is never read, and the result sort is not handed over: both bound a level from below and say nothing of what the value holds. A lambda left inside the reduct is read with its binders' types, the kernel comparing them where two lambdas meet at no function type, and what the walk cannot see through marks every level it carries.
///
/// **Any other definition is read as written.** Every level its body mentions beneath its own binders is held. The walk forces each closed part of what it reads, which for a type is how an alias is seen through and for a function's body is its closed computations run, the cost a vector exists to save; and a level a value's term mentions is nearly always one its instances differ in.
///
/// Either way a vector claims an irrelevance only where unfolding two instances at the same arguments gives one term, which is what lets conversion require nothing of that level. A reduction the budget refuses reads the term as written, and is no error here: the vector it leaves claims less.
pub(crate) fn definition_variances(
    context: &mut Context,
    universe_context: &UniverseContext,
    type_: &Term,
    body: &Term,
) -> Vec<Variance> {
    let parameters = universe_context.parameter_count;
    if parameters == 0 {
        return Vec::new();
    }
    curios_profile::profile!("definition_variances");

    let outer = match &**type_ {
        Subterm::FuncType(FuncType { telescope, .. }) => Some(telescope),
        _ => None,
    };
    let held: Box<dyn Fn(UniverseParam) -> bool> = match answers_a_type(type_) {
        true => {
            let applied = match outer {
                Some(telescope) => {
                    let mut arguments = Vec::with_capacity(telescope.len());
                    let mut cursor = telescope.cursor();
                    while let Some(mark) = cursor.mark() {
                        let binder = Term::free_var(&context.fresh(None));
                        arguments.push((mark, binder.clone()));
                        cursor.advance(binder);
                    }

                    Term::apply_marked(body.clone(), arguments)
                }
                None => body.clone(),
            };
            let found = held_levels(context, &[applied], Passed::AsWritten);

            Box::new(move |parameter| found.holds(LevelHead::Param(parameter)))
        }
        false => {
            let written = match (&**body, outer) {
                (Subterm::Func(Func { telescope, .. }), Some(outer))
                    if telescope.len() == outer.len() =>
                {
                    universe_params(telescope.terminal())
                }
                _ => universe_params(body),
            };

            Box::new(move |parameter| written.contains(&parameter))
        }
    };

    (0..parameters)
        .map(|index| match held(UniverseParam(index)) {
            true => Variance::Invariant,
            false => Variance::Irrelevant,
        })
        .collect()
}

/// Whether a definition of this type answers a type: its terminal, past every function type, is a sort.
fn answers_a_type(type_: &Term) -> bool {
    let mut terminal = type_;
    while let Subterm::FuncType(FuncType { telescope, .. }) = &**terminal {
        terminal = telescope.terminal();
    }

    matches!(&**terminal, Subterm::Type(_) | Subterm::Prop)
}

/// Recompute the vector of each entry `published` declares over its zonked telescopes and hold the one the entry carries to it.
///
/// The entries of one declaration are exactly the set to analyze, and anything outside it answers from this context's registry, for the reason [`decide_polarities`](super::decide_polarities) gives. The carried vector is the one taken as the family's group finalized, and it stays: the declaration's later items were compared under it, so a stored unit hands a later compile the reading its own items had and a reused family is read as a re-elaborated one is. What this run decides is that it claims no irrelevance the final telescopes deny. It may claim less, a telescope having held a metavariable since solved, and that family is counted rather than rewritten.
///
/// A reduction the budget refused reads its term as mentioning every level, so a denial that follows one may be the budget's alone, and the budget is what is reported.
pub(crate) fn hold_variances(context: &mut Context, published: &Published) -> Result<(), Error> {
    if published.induct_decls.is_empty() && published.struct_decls.is_empty() {
        return Ok(());
    }

    curios_profile::profile!("check_variance");
    let inducts = published
        .induct_decls
        .iter()
        .cloned()
        .collect::<BTreeMap<_, _>>();
    let structs = published
        .struct_decls
        .iter()
        .cloned()
        .collect::<BTreeMap<_, _>>();
    let VarianceVectors { vectors, exhausted } =
        variance_vectors(context, Declarations::of(&inducts, &structs));

    let carried = |name: &Global| {
        inducts
            .get(name)
            .map(|declaration| &declaration.variances)
            .or_else(|| structs.get(name).map(|declaration| &declaration.variances))
    };

    for (name, settled) in &vectors {
        let Some(carried) = carried(name) else {
            continue;
        };
        let denied = carried.iter().zip(settled).position(|(carried, settled)| {
            *carried == Variance::Irrelevant && *settled == Variance::Invariant
        });
        if let Some(level) = denied {
            return Err(match exhausted {
                Some((walked, error)) => error.in_declaration(&walked.symbol(), Some(&walked)),
                None => Error::UniverseInvariant(format!(
                    "{name}: universe level {level} is carried as irrelevant, and its declaration mentions it"
                )),
            });
        }
    }
    curios_profile::sample!(
        "variance::read_short_at_finalization",
        vectors
            .iter()
            .filter(|(name, settled)| carried(name).is_some_and(|carried| carried != *settled))
            .count()
    );

    Ok(())
}
