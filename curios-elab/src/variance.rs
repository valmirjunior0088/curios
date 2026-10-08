//! The elaborator's driver for the shared variance analysis: when it runs, and where its vectors live.
//!
//! The analysis itself lives in `curios-analysis/src/variance.rs` (see that module for the rule and its rationale) and is run by both checkers. It runs here twice for each declaration, for one reason each. When a family's group finalizes, because a later item of the same declaration may compare two of its instances: that vector is kept on the context's registry entry, and it is the one the unit carries. And as the declaration is published, where its telescopes are final and meta-free: that run writes nothing and holds the carried vector to what the declaration says.
//!
//! The same walk reads a definition, once, where its levels have closed ([`definition_variances`]): which of them its value holds. That vector is the elaborator's alone, since the kernel decides two instances of a definition by unfolding them, so nothing holds it to a second reading.

use {
    super::{Context, Error, Published, zonk_solved_term_metas},
    curios_analysis::{Declarations, VarianceVectors, held_by, held_levels, variance_vectors},
    curios_core::{
        Bound, Families, Func, FuncType, Global, InductDecl, InductParam, Level, LevelHead,
        StructDecl, Subterm, Term, UniverseContext, UniverseMetaId, UniverseParam, Variance,
        rewrite_universe_levels_scoped_shared, universe_params,
    },
    std::{
        collections::{BTreeMap, BTreeSet},
        slice,
    },
};

/// The registries as conversion's level alignment reads them: a family's vector is the one its entry holds while the unit elaborates, empty for a family still inside its own group, so every level of it is compared there, and a definition's is the one it finished with.
impl Families for Context<'_> {
    fn induct(&self, name: &Global) -> Option<&InductDecl> {
        self.induct_decl(name)
    }

    fn struct_(&self, name: &Global) -> Option<&StructDecl> {
        self.struct_decl(name)
    }

    fn definition(&self, name: &Global) -> Option<(&[Variance], usize)> {
        self.definition_variances(name)
    }
}

/// The registries as the structural arms read them, where no type is at hand for eta to apply two curried heads at: the families' vectors, no definition's, and a head short of its full application compared level by level, which is the kernel's rule for two instances that reduction leaves standing.
pub(crate) struct Structural<'a>(pub(crate) &'a Context<'a>);

impl Families for Structural<'_> {
    fn induct(&self, name: &Global) -> Option<&InductDecl> {
        self.0.induct_decl(name)
    }

    fn struct_(&self, name: &Global) -> Option<&StructDecl> {
        self.0.struct_decl(name)
    }

    fn sets_curried_apart(&self) -> bool {
        false
    }
}

/// Record each of `names`' variance vectors on its registry entry: the families of one group, analyzed together as that group finalizes, so a later item that compares two of their instances reads them.
///
/// Every earlier family's vector answers from the registry. A solved metavariable is spliced in first; one still unsolved is read as mentioning every level, its solution being free to. A reduction the budget refuses reads the same way, and is no error here: the vector it leaves claims less.
pub(crate) fn record_variances(context: &mut Context, names: &[Global]) {
    curios_profile::profile!("record_variances");
    let (inducts, structs) = spliced(context, names);

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

/// The levels still open that the families among `names` hold, the group they close with: what an instance of one holds in its index types, payloads, fields and targets, read by the walk their vectors are computed by once their levels have closed ([`record_variances`]). `None` where a telescope may hold any level, a metavariable in it being still unsolved.
///
/// A level it reports is one two instances would be told apart by, so it is the family's parameter. A level of a telescope it does not report is one no instance holds. A reduction the budget refuses reads its term as holding every level it carries, and is no error here: what is reported claims less.
pub(crate) fn family_levels(
    context: &mut Context,
    names: &[Global],
) -> Option<BTreeSet<UniverseMetaId>> {
    curios_profile::profile!("family_levels");
    let (inducts, structs) = spliced(context, names);
    let found = held_by(context, Declarations::of(&inducts, &structs));
    if found.every {
        return None;
    }

    Some(
        found
            .heads
            .into_iter()
            .filter_map(|head| match head {
                LevelHead::Meta(meta) => Some(meta),
                LevelHead::Param(_) => None,
            })
            .collect(),
    )
}

/// The registry entries of `names` with every solved metavariable spliced into their telescopes: what the analysis reads of a group as it closes.
fn spliced(
    context: &Context,
    names: &[Global],
) -> (BTreeMap<Global, InductDecl>, BTreeMap<Global, StructDecl>) {
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

    (inducts, structs)
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
            let found = held_levels(context, &[applied]);

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

/// The levels `type_`, a declaration's signature while its levels are still open, holds where conversion compares them: in each of its domains and in a terminal that is no sort, at a position no family or definition is irrelevant in. They are what [`UniverseSolver::finalize`](crate::UniverseSolver::finalize) holds as the declaration's parameters.
///
/// A terminal sort is unread, cumulativity lifting it, and every other part is read as it is written ([`compared_in`]). Where a part may hold any level, every level the signature mentions is held.
pub(crate) fn compared_levels(context: &mut Context, type_: &Term) -> BTreeSet<UniverseMetaId> {
    curios_profile::profile!("compared_levels");
    let type_ = &zonk_solved_term_metas(context, type_);
    let mut parts = Vec::new();
    let mut terminal = type_.clone();
    loop {
        let rest = match &*terminal {
            Subterm::FuncType(FuncType { telescope, .. }) => {
                let mut cursor = telescope.cursor();
                while let Some((_, domain)) = cursor.entry() {
                    parts.push(domain);
                    cursor.advance(Term::free_var(&context.fresh(None)));
                }
                cursor.body()
            }
            _ => None,
        };
        match rest {
            Some(rest) => terminal = rest,
            None => break,
        }
    }
    if !matches!(&*terminal, Subterm::Type(_) | Subterm::Prop(_)) {
        parts.push(terminal);
    }

    compared_in(context, &parts).unwrap_or_else(|| context.universe_metas_in(type_))
}

/// The levels still open that `parts` hold where two instances of them would be compared, or `None` where a part may hold any.
///
/// **Read by the alignment conversion reads two terms by.** Each part is aligned with itself one level up ([`Term::level_differences`]), so every level it carries is one pair, set apart by the position it stands at as the alignment sets it apart between two terms: compared, at a position a vector calls irrelevant, or at a function short of its application, a bare former or a lambda's binder annotation, which eta applies before either side is read. A level in a compared pair is held. So what a declaration keeps is what conversion would be asked to equate between two instances of it, by the one reading of which positions those are, and nothing is reduced to say so: an applied definition answers by the vector it finished with.
///
/// That holds a level reduction would have dropped, in an argument a definition's value never reads, which costs a parameter and commits nothing. A metavariable still unsolved may be solved with any level, so a part that holds one answers `None`.
fn compared_in(context: &Context, parts: &[Term]) -> Option<BTreeSet<UniverseMetaId>> {
    let mut held = BTreeSet::new();
    for part in parts {
        if part.has_metavar() {
            return None;
        }
        let lifted =
            rewrite_universe_levels_scoped_shared(part, |_, level: &Level| level.succ()).ok()?;
        let differences = part.level_differences(&lifted, |_| false, context)?;
        held.extend(
            differences
                .compared
                .iter()
                .flat_map(|(_, level, _)| level.metas()),
        );
    }

    Some(held)
}

/// The levels still open that the body of a declaration answering a type holds where conversion compares them once an occurrence of it is unfolded; none for a declaration that answers anything else. A level among them that the body alone mentions is the declaration's parameter as a signature's is, where any other body level is a classifier nothing can observe.
///
/// The body is read as a signature's parts are ([`compared_in`]), as it is written: a binder's annotation is unread, being the signature's to hold, and a member of a group is read as a `let` is, its siblings standing as the names they are assumed under. Where the body may hold any level, every level it mentions is held.
///
/// `lifted` are the levels of the declaration's result sort alone, and none of them is held, nor any level conversion has joined to one: a body restates the sort it answers at wherever it says what it answers, in a match's motive, and cumulativity lifts the result there as at the signature's terminal.
pub(crate) fn body_levels(
    context: &Context,
    type_: &Term,
    body: &Term,
    lifted: &BTreeSet<UniverseMetaId>,
) -> BTreeSet<UniverseMetaId> {
    if !answers_a_type(type_) {
        return BTreeSet::new();
    }
    curios_profile::profile!("body_levels");
    let body = zonk_solved_term_metas(context, body);
    let held = compared_in(context, slice::from_ref(&body))
        .unwrap_or_else(|| context.universe_metas_in(&body));

    let solver = context.universes();
    let lifted = solver.representatives(lifted.iter().copied());
    held.into_iter()
        .filter(|meta| solver.representatives([*meta]).is_disjoint(&lifted))
        .collect()
}

/// Whether a definition of this type answers a type: its terminal, past every function type, is a sort.
fn answers_a_type(type_: &Term) -> bool {
    let mut terminal = type_;
    while let Subterm::FuncType(FuncType { telescope, .. }) = &**terminal {
        terminal = telescope.terminal();
    }

    matches!(&**terminal, Subterm::Type(_) | Subterm::Prop(_))
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
