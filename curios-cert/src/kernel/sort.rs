//! Which universe a type inhabits, and the type of a neutral spine.
//!
//! Conversion is type-directed in Curios: a goal at a `Prop`-sorted type is
//! discharged without comparing the sides at all, because any two inhabitants
//! of a proposition are definitionally equal. So before conversion can compare
//! anything it has to know a type's *sort*, and that is a typing question.
//!
//! Answering it in full would need [`infer`](super::infer), which needs
//! conversion, which is a cycle. The way out is the same one the elaborator
//! takes: a *synthesis-only* type computation for neutral spines
//! ([`synth_neutral`]) that reads types off binders and declarations without
//! ever checking anything, and therefore never reaches conversion. It answers
//! "what type does this spine have, if it has one" and nothing else.
//!
//! Unlike the elaborator's, neither function here guesses. Where a shape cannot
//! be classified the kernel refuses; see the module documentation on
//! [`kernel`](super) for why a guessed level is the unsound direction.

#[cfg(test)]
mod tests;

use {
    super::{Kernel, KernelError, whnf::whnf},
    curios_core::{
        Apply, Field, FuncType, InductType, Level, Prim, Proj, RecMember, Reducer, StructType,
        Subterm, Telescope, Term, TupleType, UniverseInst, instantiate_universe_levels_scoped,
    },
};

/// The universe a type inhabits.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Sort {
    Type(Level),
    Prop,
}

impl Sort {
    /// The universe term this sort denotes, which is what a type former reports
    /// as its own type.
    pub fn term(self) -> Term {
        match self {
            Sort::Type(level) => Term::type_at(level),
            Sort::Prop => Term::prop(),
        }
    }

    /// Whether this is a strict proposition — the question proof irrelevance
    /// turns on.
    pub fn is_prop(&self) -> bool {
        matches!(self, Sort::Prop)
    }
}

/// Decode a term that is *already* a universe — a kind's codomain, a match
/// motive, a synthesized neutral's type — into the sort it names.
///
/// Distinct from [`Sort::of`], which classifies an arbitrary type:
/// `as_sort(Prop)` is `Prop`, whereas `Sort::of(Prop)` is `Type 0`, since the
/// universe `Prop` is itself `Type`-sorted.
pub(crate) fn as_sort(kernel: &mut Kernel, universe: &Term) -> Result<Sort, KernelError> {
    let reduced = whnf(kernel, universe.clone())?;

    match &*reduced {
        Subterm::Prop => Ok(Sort::Prop),
        Subterm::Type(level) => Ok(Sort::Type(level.clone())),
        _ => Err(KernelError::NotASort(reduced.clone())),
    }
}

impl Sort {
    /// The sort of `type_`.
    pub(crate) fn of(kernel: &mut Kernel, type_: &Term) -> Result<Sort, KernelError> {
        let reduced = kernel.reduce_forced(type_.clone())?;

        match &*reduced {
            // A nominal type's sort is declared, not derived: the declaration says
            // whether the family lands in `Type` or in `Prop`, at the universes
            // this occurrence instantiated it at.
            Subterm::InductType(InductType {
                name, universes, ..
            }) => {
                let declaration = kernel
                    .induct_decl(name)
                    .ok_or_else(|| KernelError::Undeclared(name.clone()))?;
                kernel.check_instance(&declaration.universe_context, universes)?;
                let result_sort =
                    instantiate_universe_levels_scoped(&declaration.result_sort, universes)?;

                as_sort(kernel, &result_sort)
            }
            Subterm::StructType(StructType {
                name, universes, ..
            }) => {
                let declaration = kernel
                    .struct_decl(name)
                    .ok_or_else(|| KernelError::Undeclared(name.clone()))?;
                kernel.check_instance(&declaration.universe_context, universes)?;
                let result_sort =
                    instantiate_universe_levels_scoped(&declaration.result_sort, universes)?;

                as_sort(kernel, &result_sort)
            }

            // A *non-empty* record of propositions is a proposition. The empty
            // tuple is unit rather than a proposition: it is what an effect returns
            // (`/std/print : .. -> {}`), so it must be kept at runtime, and calling
            // it a proposition would erase it.
            Subterm::TupleType(TupleType { telescope }) if !telescope.is_empty() => {
                let telescope = telescope.clone();
                scoped(kernel, |kernel| sort_of_sigma(kernel, telescope))
            }
            Subterm::TupleType(_) => Ok(Sort::Type(Level::zero())),

            // Π into a proposition is a proposition, regardless of what it
            // quantifies over — that is what makes `(n : Nat) -> P(n)` erasable.
            // Otherwise the level is the join of the domains and the codomain.
            Subterm::FuncType(FuncType { telescope, .. }) => {
                let telescope = telescope.clone();
                scoped(kernel, |kernel| sort_of_pi(kernel, telescope))
            }

            Subterm::Prim(prim) => sort_of_prim(kernel, prim),

            // A type-valued `match` (`rec Lt = match n : Prop | ..`): its motive is
            // the sort, which every arm shares.
            Subterm::Match(m) => {
                let binders = (0..m.motive.arity())
                    .map(|_| Term::free_var(&kernel.fresh(None)))
                    .collect::<Vec<_>>();
                let refs = binders.iter().collect::<Vec<_>>();
                let motive = m.motive.open(&refs);

                as_sort(kernel, &motive)
            }

            // A neutral type — a `Prop` hypothesis, or a family application stuck
            // on a variable. Its synthesized type *is* its sort.
            Subterm::Var(_) | Subterm::Apply(_) | Subterm::Proj(_) | Subterm::RecMember(_) => {
                let synthesized = synth_neutral(kernel, &reduced)?
                    .ok_or_else(|| KernelError::Unclassified(reduced.clone()))?;

                as_sort(kernel, &synthesized)
            }

            // `Type u : Type (u + 1)`, and `Prop : Type 0`.
            Subterm::Type(level) => Ok(Sort::Type(level.succ()?)),
            Subterm::Prop => Ok(Sort::Type(Level::zero())),

            Subterm::UniverseInst(instance) => Sort::of(kernel, &instance.head),

            _ => Err(KernelError::Unclassified(reduced.clone())),
        }
    }
}

/// Run `walk` with every binder it opens closed again afterwards, on the
/// failing path as well as the succeeding one.
fn scoped(
    kernel: &mut Kernel,
    walk: impl FnOnce(&mut Kernel) -> Result<Sort, KernelError>,
) -> Result<Sort, KernelError> {
    let mark = kernel.mark();
    let outcome = walk(kernel);
    kernel.retract(mark);

    outcome
}

/// Σ: a record of nothing but propositions is a proposition; otherwise its
/// level is the join of its fields'.
///
/// Each binder joins the local scope carrying its own type before the walk
/// descends, because a later field may mention an earlier one. Opening with a
/// variable nothing can type would leave [`synth_neutral`] unable to classify
/// every occurrence of it further in — which here is not imprecision but a
/// refusal.
fn sort_of_sigma(kernel: &mut Kernel, telescope: Telescope<()>) -> Result<Sort, KernelError> {
    let mut telescope = telescope;
    let mut levels = Vec::new();

    loop {
        match telescope {
            Telescope::Cons(field, rest) => {
                // A field that is itself a proposition contributes no level:
                // `Prop` sits below the hierarchy rather than in it.
                if let Sort::Type(level) = Sort::of(kernel, &field)? {
                    levels.push(level);
                }

                let binder = kernel.fresh(rest.first_hint());
                kernel.assume(&binder, &field);
                telescope = rest.open(&[&Term::free_var(&binder)]);
            }
            Telescope::Done(_) => {
                return Ok(match levels.is_empty() {
                    true => Sort::Prop,
                    false => Sort::Type(Level::max(levels)),
                });
            }
        }
    }
}

/// Π: a function into a proposition is a proposition, whatever it quantifies
/// over; otherwise its level is the join of its domains' and its codomain's.
fn sort_of_pi(kernel: &mut Kernel, telescope: Telescope<Term>) -> Result<Sort, KernelError> {
    let mut telescope = telescope;
    let mut levels = Vec::new();

    loop {
        match telescope {
            Telescope::Cons(domain, rest) => {
                if let Sort::Type(level) = Sort::of(kernel, &domain)? {
                    levels.push(level);
                }

                let binder = kernel.fresh(rest.first_hint());
                kernel.assume(&binder, &domain);
                telescope = rest.open(&[&Term::free_var(&binder)]);
            }
            Telescope::Done(codomain) => {
                return Ok(match Sort::of(kernel, &codomain)? {
                    Sort::Prop => Sort::Prop,
                    Sort::Type(output) => {
                        levels.push(output);
                        Sort::Type(Level::max(levels))
                    }
                });
            }
        }
    }
}

/// The sort of a primitive type former.
///
/// A closed primitive quantifies over nothing and sits at level 0. A
/// parameterized one carries its parameter's level: `Lst : Type u -> Type u`,
/// and pinning that at 0 would claim the type is smaller than it is — the
/// unsound direction, and what would let a large type be stored in a small
/// universe.
fn sort_of_prim(kernel: &mut Kernel, prim: &Prim) -> Result<Sort, KernelError> {
    match prim {
        Prim::BoolType
        | Prim::NatType
        | Prim::ByteType
        | Prim::IntType
        | Prim::FltType
        | Prim::BinType(_)
        | Prim::HandleType => Ok(Sort::Type(Level::zero())),

        // A list or cell *of* proofs is not itself a proposition: it has a
        // length, or an identity, so its inhabitants are distinguishable and
        // irrelevance does not apply. It lands in `Type`, and `Prop : Type 0`.
        Prim::LstType(element) | Prim::CellType(element) => {
            let element = element.clone();

            Ok(match Sort::of(kernel, &element)? {
                Sort::Type(level) => Sort::Type(level),
                Sort::Prop => Sort::Type(Level::zero()),
            })
        }

        // A primitive *value* is not a type, so nothing here classifies it.
        other => Err(KernelError::Unclassified(Term::prim(other.clone()))),
    }
}

/// The type of a neutral spine, read off binders and declarations without
/// checking anything.
///
/// `None` where the spine is not one this can type — a shape whose type would
/// need a judgment rather than a lookup. Callers turn that into a refusal;
/// nothing here guesses.
///
/// This must never reach [`convert`](super::convert): it is what breaks the
/// cycle between conversion and inference, and it stays broken only because
/// every arm below is a lookup, a substitution, or a reduction.
pub(crate) fn synth_neutral(kernel: &mut Kernel, term: &Term) -> Result<Option<Term>, KernelError> {
    match &**term {
        Subterm::Var(var) => Ok(kernel.type_of(var.unwrap()).cloned()),

        // A recursive member's type is carried by its own group, so this is a
        // read rather than a lookup and cannot re-enter the group it names.
        Subterm::RecMember(RecMember { group, index }) => Ok(Some(group.member_type(*index))),

        // A polymorphic head at the levels this occurrence chose. The scheme is
        // read *uninstantiated* and substituted at those levels; going through
        // the `Var` arm would read an already-instantiated type and have
        // nothing left to substitute.
        Subterm::UniverseInst(UniverseInst { head, levels }) => match &**head {
            Subterm::Var(var) => {
                let name = var.unwrap();

                // A local binder is monomorphic: it was opened at one type, so
                // there is no scheme to instantiate and the levels say nothing.
                if let Some(type_) = kernel.local_type(name) {
                    return Ok(Some(type_.clone()));
                }

                let Some((scheme, context)) = kernel.scheme_of(name) else {
                    return Ok(None);
                };
                let (scheme, context) = (scheme.clone(), context.clone());
                kernel.check_instance(&context, levels)?;

                Ok(Some(instantiate_universe_levels_scoped(&scheme, levels)?))
            }
            Subterm::RecMember(RecMember { group, index }) => {
                kernel.check_instance(group.universes(), levels)?;
                let group = group.instantiate_universes(levels)?;

                Ok(Some(group.member_type(*index)))
            }
            _ => Ok(None),
        },

        Subterm::Apply(Apply { head, params, .. }) => {
            let Some(head_type) = synth_neutral(kernel, head)? else {
                return Ok(None);
            };

            match Term::unwrap_or_clone(whnf(kernel, head_type)?) {
                Subterm::FuncType(FuncType { telescope, .. })
                    if telescope.len() == params.len() =>
                {
                    let refs = params.iter().collect::<Vec<_>>();
                    Ok(Some(telescope.open(&refs)))
                }
                // A partially applied spine still has a type: the residual
                // function type, with the supplied arguments substituted into
                // the entries that remain.
                Subterm::FuncType(FuncType {
                    telescope,
                    plicities,
                }) if telescope.len() > params.len() => Ok(Some(
                    Subterm::FuncType(FuncType {
                        telescope: telescope.open_params(params),
                        plicities: plicities[params.len()..].to_vec(),
                    })
                    .into(),
                )),
                _ => Ok(None),
            }
        }

        Subterm::Proj(Proj {
            head,
            field: Field::Index(index),
        }) => {
            let Some(head_type) = synth_neutral(kernel, head)? else {
                return Ok(None);
            };

            match Term::unwrap_or_clone(whnf(kernel, head_type)?) {
                // A Σ field's type may mention the earlier fields, which are
                // named here by projections of the same head.
                Subterm::TupleType(TupleType { telescope }) => {
                    Ok(telescope.nth(*index, |j| Term::proj(head.clone(), j)))
                }
                // A nominal record's field types live on its declaration,
                // instantiated first at the head's universes and then at its
                // parameters. Concept dispatch is a projection out of a witness
                // record, so this arm is what types a method call.
                Subterm::StructType(StructType {
                    name,
                    universes,
                    params,
                }) => {
                    let Some(declaration) = kernel.struct_decl(&name) else {
                        return Ok(None);
                    };
                    if declaration.fields.len() < params.len() {
                        return Ok(None);
                    }

                    let fields = instantiate_universe_levels_scoped(
                        &declaration.fields.clone(),
                        &universes,
                    )?;

                    Ok(fields
                        .open_params(&params)
                        .nth(*index, |j| Term::proj(head.clone(), j)))
                }
                _ => Ok(None),
            }
        }

        _ => Ok(None),
    }
}
