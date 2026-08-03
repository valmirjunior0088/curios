//! Which universe a type inhabits, and the type of a neutral spine.
//!
//! Conversion is type-directed in Curios: a goal at a `Prop`-sorted type is discharged without comparing the sides at all, because any two inhabitants of a proposition are definitionally equal. So before conversion can compare anything it has to know a type's *sort*, and that is a typing question.
//!
//! Answering it in full would need [`infer`](super::infer), which needs conversion, which is a cycle. The way out is the same one the elaborator takes: a *synthesis-only* type computation for neutral spines ([`synth_neutral`]) that reads types off binders and declarations without ever checking anything, and therefore never reaches conversion. It answers "what type does this spine have, if it has one" and nothing else.
//!
//! Unlike the elaborator's, neither function here guesses. Where a shape cannot be classified the kernel refuses; see the module documentation on [`kernel`](super) for why a guessed level is the unsound direction.

#[cfg(test)]
mod tests;

use {
    super::{Kernel, KernelError, whnf::whnf},
    curios_core::{
        Apply, Bound, Field, FuncType, InductType, Level, Prim, Proj, Reducer, StructType, Subterm,
        Telescope, Term, TupleType, UniverseInst, instantiate_universe_levels_scoped,
    },
};

/// The universe a type inhabits.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Sort {
    Type(Level),
    Prop,
}

impl Sort {
    /// The universe term this sort denotes, which is what a type former reports as its own type.
    pub fn term(self) -> Term {
        match self {
            Sort::Type(level) => Term::type_at(level),
            Sort::Prop => Term::prop(),
        }
    }

    /// Whether this is a strict proposition — the question proof irrelevance turns on.
    pub fn is_prop(&self) -> bool {
        matches!(self, Sort::Prop)
    }
}

/// Decode a term that is *already* a universe — a kind's codomain, a match motive, a synthesized neutral's type — into the sort it names.
///
/// Distinct from [`Sort::of`], which classifies an arbitrary type: `as_sort(Prop)` is `Prop`, whereas `Sort::of(Prop)` is `Type 0`, since the universe `Prop` is itself `Type`-sorted.
/// An occurrence supplies exactly as many parameters, or indices, as its declaration declares.
///
/// The counterpart of [`Kernel::check_instance`](crate::Kernel) for the arities a nominal term carries beside its universe instance — an occurrence's parameters and indices, and a value's parameters. Both are read from the declaration the moment anything asks what an occurrence *is*, and both were taken on the occurrence's own word: an `InductType` at no parameters for a one-parameter family was classified as a well-formed type, and every consumer of the arity after that was wrong about it — `instantiate` peeling a prefix that is not there, and `indices_at` reaching `Telescope::open`'s assertion and aborting the process. Checked here, beside the universe width, because this is where a declaration is consulted for an occurrence at all.
pub(super) fn arity_matches(expected: usize, actual: usize) -> Result<(), KernelError> {
    match expected == actual {
        true => Ok(()),
        false => Err(KernelError::Arity { expected, actual }),
    }
}

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
            // A nominal type's sort is declared, not derived: the declaration says whether the family lands in `Type` or in `Prop`, at the universes this occurrence instantiated it at.
            Subterm::InductType(InductType {
                name,
                universes,
                params,
                indices,
            }) => {
                let declaration = kernel
                    .induct_decl(name)
                    .ok_or_else(|| KernelError::Undeclared(name.clone()))?;
                kernel.check_instance(&declaration.universe_context, universes)?;
                arity_matches(declaration.param_count(), params.len())?;
                arity_matches(declaration.index_count(), indices.len())?;
                let result_sort =
                    instantiate_universe_levels_scoped(&declaration.result_sort, universes)?;

                as_sort(kernel, &result_sort)
            }
            Subterm::StructType(StructType {
                name,
                universes,
                params,
            }) => {
                let declaration = kernel
                    .struct_decl(name)
                    .ok_or_else(|| KernelError::Undeclared(name.clone()))?;
                kernel.check_instance(&declaration.universe_context, universes)?;
                arity_matches(declaration.param_count(), params.len())?;
                let result_sort =
                    instantiate_universe_levels_scoped(&declaration.result_sort, universes)?;

                as_sort(kernel, &result_sort)
            }

            // A *non-empty* record of propositions is a proposition. The empty tuple is unit rather than a proposition: it is what an effect returns (`/std/print : .. -> {}`), so it must be kept at runtime, and calling it a proposition would erase it.
            Subterm::TupleType(TupleType { telescope }) if !telescope.is_empty() => {
                let telescope = telescope.clone();

                // Σ: a record of nothing but propositions is a proposition; otherwise its level is the join of its fields'.
                kernel.scoped(|kernel| {
                    sort_of_binders(kernel, telescope, |_, levels, ()| {
                        Ok(match levels.is_empty() {
                            true => Sort::Prop,
                            false => Sort::Type(Level::max(levels)),
                        })
                    })
                })
            }
            Subterm::TupleType(_) => Ok(Sort::Type(Level::zero())),

            // Π into a proposition is a proposition, regardless of what it quantifies over — that is what makes `(n : Nat) -> P(n)` erasable. Otherwise the level is the join of the domains and the codomain.
            Subterm::FuncType(FuncType { telescope, .. }) => {
                let telescope = telescope.clone();

                kernel.scoped(|kernel| {
                    sort_of_binders(kernel, telescope, |kernel, mut levels, codomain| {
                        Ok(match Sort::of(kernel, &codomain)? {
                            Sort::Prop => Sort::Prop,
                            Sort::Type(output) => {
                                levels.push(output);
                                Sort::Type(Level::max(levels))
                            }
                        })
                    })
                })
            }

            Subterm::Prim(prim) => sort_of_prim(kernel, prim),

            // A type-valued `match` (`rec Lt = match n : Prop | ..`): its motive is the sort, which every arm shares.
            Subterm::Match(m) => {
                let binders = (0..m.motive.arity())
                    .map(|_| Term::free_var(&kernel.fresh(None)))
                    .collect::<Vec<_>>();
                let refs = binders.iter().collect::<Vec<_>>();
                let motive = m.motive.open(&refs);

                as_sort(kernel, &motive)
            }

            // A neutral type — a `Prop` hypothesis, or a family application stuck on a variable.
            Subterm::Var(_) | Subterm::Apply(_) | Subterm::Proj(_) => {
                sort_of_neutral(kernel, &reduced)
            }

            // A projection of a recursive group is neutral in the same way: `rec Lt = ..; Lt` is stuck at its own fixed point, and its type is the one its group carries.
            Subterm::Rec(_) if reduced.as_rec_proj().is_some() => sort_of_neutral(kernel, &reduced),

            // `Type u : Type (u + 1)`, and `Prop : Type 0`.
            Subterm::Type(level) => Ok(Sort::Type(level.succ()?)),
            Subterm::Prop => Ok(Sort::Type(Level::zero())),

            Subterm::UniverseInst(instance) => Sort::of(kernel, &instance.head),

            _ => Err(KernelError::Unclassified(reduced.clone())),
        }
    }
}

/// The sort of a neutral type: what [`synth_neutral`] reads off its head *is* its sort.
fn sort_of_neutral(kernel: &mut Kernel, reduced: &Term) -> Result<Sort, KernelError> {
    let synthesized = synth_neutral(kernel, reduced)?
        .ok_or_else(|| KernelError::Unclassified(reduced.clone()))?;

    as_sort(kernel, &synthesized)
}

/// Walk a binder telescope, collecting the level each domain contributes, and hand the collected levels and the terminal to the clause that decides the former's own sort.
///
/// A domain that is itself a proposition contributes no level: `Prop` sits below the hierarchy rather than in it. Each binder joins the local scope carrying its own type before the walk descends, because a later domain may mention an earlier one — and opening with a variable nothing can type would leave [`synth_neutral`] unable to classify every occurrence of it further in, which here is not imprecision but a refusal.
///
/// Σ and Π share every line of that and differ only in the terminal clause, which is exactly where the two rules genuinely differ: a record is a proposition when *all* its fields are, a function when its *codomain* is. Splitting there keeps that difference the only thing either caller states.
fn sort_of_binders<B: Bound>(
    kernel: &mut Kernel,
    telescope: Telescope<B>,
    terminal: impl FnOnce(&mut Kernel, Vec<Level>, B) -> Result<Sort, KernelError>,
) -> Result<Sort, KernelError> {
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
            Telescope::Done(body) => return terminal(kernel, levels, *body),
        }
    }
}

/// The sort of a primitive type former.
///
/// A closed primitive quantifies over nothing and sits at level 0. A parameterized one carries its parameter's level: `Lst : Type u -> Type u`, and pinning that at 0 would claim the type is smaller than it is — the unsound direction, and what would let a large type be stored in a small universe.
///
/// Reachable from [`infer_prim`](super::infer::infer_prim) as well, which types these formers rather than restating the rule: a second copy read the element's sort as the former's and typed a list of proofs at `Prop`.
pub(crate) fn sort_of_prim(kernel: &mut Kernel, prim: &Prim) -> Result<Sort, KernelError> {
    match prim {
        Prim::BoolType
        | Prim::NatType
        | Prim::ByteType
        | Prim::IntType
        | Prim::FltType
        | Prim::BinType(_)
        | Prim::HandleType => Ok(Sort::Type(Level::zero())),

        // A list or cell *of* proofs is not itself a proposition: it has a length, or an identity, so its inhabitants are distinguishable and irrelevance does not apply. It lands in `Type`, and `Prop : Type 0`.
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

/// The type of a neutral spine, read off binders and declarations without checking anything.
///
/// `None` where the spine is not one this can type — a shape whose type would need a judgment rather than a lookup. Callers turn that into a refusal; nothing here guesses.
///
/// This must never reach [`convert`](super::convert): it is what breaks the cycle between conversion and inference, and it stays broken only because every arm below is a lookup, a substitution, or a reduction.
pub(crate) fn synth_neutral(kernel: &mut Kernel, term: &Term) -> Result<Option<Term>, KernelError> {
    // A projection's type is carried by its own group, so this is a read rather than a lookup and cannot re-enter the group it names.
    if let Some((group, index)) = term.as_rec_proj() {
        return Ok(Some(group.member_type(index)));
    }

    match &**term {
        Subterm::Var(var) => Ok(kernel.type_of(var.unwrap()).cloned()),

        // A polymorphic head at the levels this occurrence chose. The scheme is read *uninstantiated* and substituted at those levels; going through the `Var` arm would read an already-instantiated type and have nothing left to substitute.
        Subterm::UniverseInst(UniverseInst { head, levels }) => {
            if let Some((group, index)) = head.as_rec_proj() {
                kernel.check_instance(group.universes(), levels)?;
                let group = group.instantiate_universes(levels)?;

                return Ok(Some(group.member_type(index)));
            }

            match &**head {
                Subterm::Var(var) => {
                    let name = var.unwrap();

                    // A local binder is monomorphic: it was opened at one type, so there is no scheme to instantiate and the levels say nothing.
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
                _ => Ok(None),
            }
        }

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
                // A partially applied spine still has a type: the residual function type, with the supplied arguments substituted into the entries that remain.
                // `plicities` is documented as parallel to the telescope and is *sliced* here, so the guard covers it as well: a vector shorter than the arguments supplied would panic rather than refuse, and a spine whose type is malformed has no residual type to report.
                Subterm::FuncType(FuncType {
                    telescope,
                    plicities,
                }) if telescope.len() > params.len() && plicities.len() >= params.len() => {
                    Ok(Some(
                        Subterm::FuncType(FuncType {
                            telescope: telescope.open_params(params),
                            plicities: plicities[params.len()..].to_vec(),
                        })
                        .into(),
                    ))
                }
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
                // A Σ field's type may mention the earlier fields, which are named here by projections of the same head.
                Subterm::TupleType(TupleType { telescope }) => {
                    Ok(telescope.nth(*index, |j| Term::proj(head.clone(), j)))
                }
                // A nominal record's field types live on its declaration, instantiated first at the head's universes and then at its parameters. Concept dispatch is a projection out of a witness record, so this arm is what types a method call.
                Subterm::StructType(StructType {
                    name,
                    universes,
                    params,
                }) => {
                    let Some(declaration) = kernel.struct_decl(&name) else {
                        return Ok(None);
                    };
                    if declaration.param_count() != params.len() {
                        return Ok(None);
                    }

                    let arity =
                        instantiate_universe_levels_scoped(&declaration.arity.clone(), &universes)?;

                    Ok(arity
                        .open(&params.iter().collect::<Vec<_>>())
                        .nth(*index, |j| Term::proj(head.clone(), j)))
                }
                _ => Ok(None),
            }
        }

        _ => Ok(None),
    }
}

/// Whether a value of this type can be distinguished from another of the same type — the question the large-elimination guard and the `Prop`-field rule both turn on.
///
/// Exactly one thing cannot be: a proof, because irrelevance makes any two inhabitants of a proposition interchangeable. Everything else can, **a type included**.
///
/// That a type is erased is not the criterion, and reading it as one is what certified a closed inhabitant of `False`. Erasure governs what the *runtime* can observe; irrelevance is a claim about *definitional equality*, and conversion reads a type-valued position back in full. A proposition carrying `A : Type` is identified with one carrying `B`, so eliminating it — or projecting it, which meets no guard at all — makes `A` and `B` convertible, and transport does the rest. `crate::recheck::tests::a_derivation_through_a_type_carrying_proposition_is_refused` holds that derivation shut; `erased_half` asks the runtime question and is where the structural `Type(_) | Prop` test legitimately belongs.
///
/// Public for one reason: `curios-elab` writes this rule a second time as `is_prop`, and the two disagreed. A compile cannot observe that — the elaborator refuses such a declaration before the kernel is asked — so the two are compared directly by `curios-elab`'s `typing::tests::both_checkers_decide_non_informativeness_alike`, which needs to name this one. It answers a question about a type and admits nothing on its own, so exporting it widens what the trusted base can be *asked* without widening what it can be told.
pub fn carries_information(kernel: &mut Kernel, type_: &Term) -> Result<bool, KernelError> {
    Ok(!Sort::of(kernel, type_)?.is_prop())
}
