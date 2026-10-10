//! The universe a type inhabits, as conversion classifies it: its quality, `Prop` for a strict proposition, and its level.

#[cfg(test)]
mod tests;

use {
    super::probe_level_fallback,
    crate::{Binders, Context, Opened, reduce, reduce_forced, synth_neutral},
    curios_core::{
        Bound, Free, FuncType, InductType, Instance, InstanceHead, Intrinsic, Level, MatchResult,
        Quality, ReduceError, StructType, Subterm, Term, TupleType,
    },
};

/// The universe a type inhabits: its quality, as far as it is solved, and its level.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Sort {
    pub(crate) quality: Quality,
    pub(crate) level: Level,
}

impl Sort {
    /// The quality of a record of fields at `this` and at `that`: a type's where either is one, a proposition's where both are, and an open one where it is the only one that is not a proposition's. Two open qualities apart have no join a sort can state, and are read as a type's, which is what a quality nothing decides closes as.
    pub(crate) fn join(this: Quality, that: Quality) -> Quality {
        match (this, that) {
            (Quality::Type, _) | (_, Quality::Type) => Quality::Type,
            (Quality::Prop, other) | (other, Quality::Prop) => other,
            (this, that) if this == that => this,
            _ => Quality::Type,
        }
    }

    /// `Type` at `level`.
    pub(crate) fn type_at(level: Level) -> Sort {
        Sort {
            quality: Quality::Type,
            level,
        }
    }

    /// `Prop` at `level`.
    pub(crate) fn prop_at(level: Level) -> Sort {
        Sort {
            quality: Quality::Prop,
            level,
        }
    }

    /// Whether this is a sort of propositions. One whose quality is open is not: irrelevance is taken where a type is known a proposition, and a sort not decided yet, or left to a caller, is not known one.
    pub(crate) fn is_prop(&self) -> bool {
        self.quality.is_prop()
    }

    /// The sort of `type_`. Any two inhabitants of a `Prop` are definitionally equal (proof irrelevance), so a conversion problem at a prop type is discharged without comparing the sides. Conservative: a shape this cannot classify is reported as `Type` — under-approximating prop-ness is sound; the reverse (a non-prop reported as a prop) is the unsound direction and never happens. An unsolved metavariable is classified by its own type rather than defaulted, so one whose type is `Prop` is a proposition before it is solved — which is what makes `p` in `@P: Prop, @p: P` a bound at its insertion, parked and filled once `P` is known, where defaulting would read it as an ordinary implicit nothing retries. A quality still open is handed back as it stands, a metavariable or a declaration's parameter, and is no proposition's to [`Sort::is_prop`].
    pub(crate) fn of(context: &mut Context, type_: &Term) -> Result<Sort, ReduceError> {
        Sort::of_in(context, &mut Vec::new(), type_)
    }

    /// [`Sort::of`] under the binders a surrounding telescope walk has opened. The `opened` scope is threaded rather than installed on the [`Context`], because assuming a binder bumps the mutation stamp that validates the memoization caches, and a walk that assumed at every binder would invalidate them.
    pub(crate) fn of_in(
        context: &mut Context,
        opened: &mut Vec<(Free, Term)>,
        type_: &Term,
    ) -> Result<Sort, ReduceError> {
        Sort::of_under(context, &Binders::default(), opened, type_)
    }

    /// [`Sort::of_in`] for a type a conversion problem holds, under the binders the problem is posed under ([`Binders`]) as well.
    ///
    /// Remembered per type while the context stands as it did (`Caches::sorts`), for a type with no loose index that names none of `opened` and none of `binders`: such a type's sort is a function of the context alone, and probing here, at every field and domain the rules below ask about, is what makes a record of two fields at one type cost one classification. A type that names a problem's binder is classified each time: read where the binders are not in hand it falls to the conservative answer, and that answer remembered would be served to a reading that has them, a verdict following the order of questions.
    pub(crate) fn of_under(
        context: &mut Context,
        binders: &Binders,
        opened: &mut Vec<(Free, Term)>,
        type_: &Term,
    ) -> Result<Sort, ReduceError> {
        let remembered = type_.reach() == 0
            && (!type_.has_local_free()
                || (opened
                    .iter()
                    .all(|(binder, _)| !type_.mentions_free(binder))
                    && !binders.names_one_in(type_)));
        if remembered && let Some(sort) = context.cached_sort(type_) {
            return Ok(sort);
        }

        let sort = Sort::classify_under(context, binders, opened, type_)?;
        // A sort read while its quality is unsolved is the answer of this reading alone: the next one may meet it solved.
        if remembered && !matches!(sort.quality, Quality::Meta(_)) {
            context.record_sort(type_.clone(), sort.clone());
        }

        Ok(sort)
    }

    /// [`Sort::of_under`]'s rules, one per type former.
    fn classify_under(
        context: &mut Context,
        binders: &Binders,
        opened: &mut Vec<(Free, Term)>,
        type_: &Term,
    ) -> Result<Sort, ReduceError> {
        // Asked of the type as written, before anything is run to classify it.
        if let Some(level) = Sort::declared_into_prop(context, binders, opened, type_)? {
            return Ok(Sort::prop_at(level));
        }

        let reduced = reduce_forced(context, type_.clone())?;

        Ok(match &*reduced {
            Subterm::InductType(InductType {
                name, universes, ..
            }) => match context.induct_decl(name).cloned() {
                Some(induct_decl) => {
                    let induct_decl = context
                        .instantiate_induct_decl_at(&induct_decl, universes)
                        .map_err(ReduceError::Universe)?;
                    Sort::from_universe(context, &induct_decl.result_sort)?
                }
                None => {
                    probe_level_fallback("induct decl missing", &reduced);
                    Sort::type_at(Level::zero())
                }
            },
            Subterm::StructType(StructType {
                name, universes, ..
            }) => match context.struct_decl(name).cloned() {
                Some(struct_decl) => {
                    let struct_decl = context
                        .instantiate_struct_decl_at(&struct_decl, universes)
                        .map_err(ReduceError::Universe)?;
                    Sort::from_universe(context, &struct_decl.result_sort)?
                }
                None => {
                    probe_level_fallback("struct decl missing", &reduced);
                    Sort::type_at(Level::zero())
                }
            },
            // A *non-empty* record of propositions is a proposition. The empty tuple `{}` is unit, not a prop: it is the result type of effects (`/std/print : .. -> {}`), so it stays `Type` (the `_` arm) and is kept at runtime rather than erased.
            Subterm::TupleType(TupleType { telescope, .. }) if !telescope.is_empty() => {
                // A later field may mention an earlier one, so each opened binder joins `opened` before the walk descends. See the `FuncType` arm below for why leaving it out is not merely imprecise but silently wrong. Each field is opened once, at every binder before it: opening binder by binder would rewrite every later field, whose metavariable spines name every earlier one.
                let mut levels = Vec::new();
                let mut quality = Quality::Prop;
                let mark = opened.len();
                telescope.walk_producing(|_, hint, ty| {
                    let field = Sort::of_under(context, binders, opened, &ty)?;
                    quality = Sort::join(quality, field.quality);
                    levels.push(field.level);
                    let binder = context.fresh(hint);
                    let variable = Term::free_var(&binder);
                    opened.push((binder, ty));
                    Ok(variable)
                })?;
                opened.truncate(mark);

                Sort {
                    quality,
                    level: Level::max(levels),
                }
            }
            // The empty tuple `{}` is unit — it quantifies over nothing, so level 0 is the answer rather than a default, and like `Prop` below it is not worth reporting as a fallback.
            Subterm::TupleType(_) => Sort::type_at(Level::zero()),
            // An intrinsic type former states its own level.
            //
            // A closed intrinsic is small: it quantifies over nothing, so it sits at level 0. A parameterized one carries its parameter's level — `List : Type u -> Type u` — and pinning those at 0 would claim the type is smaller than it is, which is the unsound direction: it is what would let a large type be stored in a small universe.
            Subterm::Intrinsic(intrinsic) => match intrinsic {
                Intrinsic::BoolType
                | Intrinsic::NatType
                | Intrinsic::ByteType
                | Intrinsic::IntType
                | Intrinsic::FltType
                | Intrinsic::BinType(_)
                | Intrinsic::HandleType => Sort::type_at(Level::zero()),
                Intrinsic::ListType(element)
                | Intrinsic::CellType(element)
                | Intrinsic::ChannelType(element)
                | Intrinsic::IoType(element) => {
                    let element = element.clone();
                    // A list, cell, or description of proofs is not itself a proposition — it has length, identity, or an effect, so its inhabitants are distinguishable and proof irrelevance does not apply. It lands at `Type` whatever its element's quality, at the element's level. For `Io` that is what keeps sort-driven erasure from dropping a description of a proof and its host effect with it.
                    Sort::type_at(Sort::of_under(context, binders, opened, &element)?.level)
                }
                // An intrinsic *value* in type position is not a shape this can classify, and unlike the formers above it is not expected.
                //
                // Listed exhaustively rather than caught by a wildcard, because the wildcard's answer is *wrong* for any former added after it: it pins the level at 0 while `curios-cert`'s `sort_of_intrinsic` reads the parameter's, and nothing catches the disagreement until the fixed prelude's kernel recheck reports a ground `Type` against a `Type.{u}`, naming no item. Every other intrinsic match in the workspace is exhaustive; this one has to be too, so a new former forces a level decision at compile time instead of inheriting a default.
                Intrinsic::Bin(..)
                | Intrinsic::BinAnd { .. }
                | Intrinsic::BinAppend { .. }
                | Intrinsic::BinConcat { .. }
                | Intrinsic::BinEql(..)
                | Intrinsic::BinGet { .. }
                | Intrinsic::BinLen(..)
                | Intrinsic::BinOr { .. }
                | Intrinsic::BinReinterp { .. }
                | Intrinsic::BinReplicate { .. }
                | Intrinsic::BinSlice { .. }
                | Intrinsic::BinXor { .. }
                | Intrinsic::Bool(..)
                | Intrinsic::BoolAnd(..)
                | Intrinsic::BoolEql(..)
                | Intrinsic::BoolNeq(..)
                | Intrinsic::BoolOr(..)
                | Intrinsic::BoolXor(..)
                | Intrinsic::Byte(..)
                | Intrinsic::ByteToNat(..)
                | Intrinsic::Cell { .. }
                | Intrinsic::CellPoll { .. }
                | Intrinsic::CellFill { .. }
                | Intrinsic::Channel { .. }
                | Intrinsic::ChannelPush { .. }
                | Intrinsic::ChannelTake { .. }
                | Intrinsic::ChannelClose { .. }
                | Intrinsic::ChannelClosed { .. }
                | Intrinsic::ChannelCount { .. }
                | Intrinsic::ChannelCapacity { .. }
                | Intrinsic::Flt(..)
                | Intrinsic::FltAbs(..)
                | Intrinsic::FltAdd(..)
                | Intrinsic::FltCopysign(..)
                | Intrinsic::FltDiv(..)
                | Intrinsic::FltEql(..)
                | Intrinsic::FltFma(..)
                | Intrinsic::FltLt(..)
                | Intrinsic::FltLe(..)
                | Intrinsic::FltMax(..)
                | Intrinsic::FltMin(..)
                | Intrinsic::FltMul(..)
                | Intrinsic::FltNeg(..)
                | Intrinsic::FltNeq(..)
                | Intrinsic::FltOfLeBytes { .. }
                | Intrinsic::FltRem(..)
                | Intrinsic::FltRoundIntegral(..)
                | Intrinsic::FltSqrt(..)
                | Intrinsic::FltSub(..)
                | Intrinsic::FltToInt { .. }
                | Intrinsic::FltToLeBytes(..)
                | Intrinsic::FltToNat { .. }
                | Intrinsic::FltMantissa { .. }
                | Intrinsic::FltExponent { .. }
                | Intrinsic::Handle(..)
                | Intrinsic::Int(..)
                | Intrinsic::IntAdd(..)
                | Intrinsic::IntAnd(..)
                | Intrinsic::IntDiv { .. }
                | Intrinsic::IntEql(..)
                | Intrinsic::IntLt(..)
                | Intrinsic::IntLe(..)
                | Intrinsic::IntMul(..)
                | Intrinsic::IntNeq(..)
                | Intrinsic::IntOr(..)
                | Intrinsic::IntRem { .. }
                | Intrinsic::IntShl(..)
                | Intrinsic::IntShr(..)
                | Intrinsic::IntSub(..)
                | Intrinsic::IntToFlt(..)
                | Intrinsic::IntToNat { .. }
                | Intrinsic::IntXor(..)
                | Intrinsic::IntMin(..)
                | Intrinsic::IntMax(..)
                | Intrinsic::IntAbs(..)
                | Intrinsic::IntSign(..)
                | Intrinsic::IoBind { .. }
                | Intrinsic::IoPure { .. }
                | Intrinsic::List { .. }
                | Intrinsic::ListAppend { .. }
                | Intrinsic::ListConcat { .. }
                | Intrinsic::ListGet { .. }
                | Intrinsic::ListLen { .. }
                | Intrinsic::ListMap { .. }
                | Intrinsic::ListFold { .. }
                | Intrinsic::ListSlice { .. }
                | Intrinsic::Nat(..)
                | Intrinsic::NatAdd(..)
                | Intrinsic::NatAnd(..)
                | Intrinsic::NatDiv { .. }
                | Intrinsic::NatEql(..)
                | Intrinsic::NatLt(..)
                | Intrinsic::NatLe(..)
                | Intrinsic::NatMul(..)
                | Intrinsic::NatNeq(..)
                | Intrinsic::NatOr(..)
                | Intrinsic::NatPow(..)
                | Intrinsic::NatRem { .. }
                | Intrinsic::NatShl(..)
                | Intrinsic::NatShr(..)
                | Intrinsic::NatSub(..)
                | Intrinsic::NatToByte { .. }
                | Intrinsic::NatToFlt(..)
                | Intrinsic::NatToInt(..)
                | Intrinsic::NatXor(..)
                | Intrinsic::NatMin(..)
                | Intrinsic::NatMax(..) => {
                    probe_level_fallback("non-type intrinsic", &reduced);
                    Sort::type_at(Level::zero())
                }
            },
            // A host call is a value, never a type, so it takes the same ground level the non-type intrinsics above do.
            Subterm::Foreign(..) => {
                probe_level_fallback("host call", &reduced);
                Sort::type_at(Level::zero())
            }
            // Π into a proposition is a proposition.
            Subterm::FuncType(FuncType { telescope, .. }) => {
                // Each opened binder must carry its domain type, not merely be substituted in. Opening with a free variable nothing can type leaves `synth_neutral` returning `None` for every occurrence of it in the codomain, and that `None` is read as level 0 — so the sort of every dependent codomain would collapse to `Type 0` regardless of the binder's real level, silently under-generalizing exactly the declarations whose codomain mentions a binder: every concept wrapper, and every higher-order polymorphic function.
                let mut domains = Vec::new();
                let mark = opened.len();
                let (_, output) = telescope.walk_producing(|_, hint, domain| {
                    domains.push(Sort::of_under(context, binders, opened, &domain)?.level);
                    let binder = context.fresh(hint);
                    let variable = Term::free_var(&binder);
                    opened.push((binder, domain));
                    Ok(variable)
                })?;
                // A function type takes its codomain's quality, a proposition where it answers one, at the join of its domains' levels and the codomain's.
                let output = Sort::of_under(context, binders, opened, &output)?;
                domains.push(output.level);
                opened.truncate(mark);
                Sort {
                    quality: output.quality,
                    level: Level::max(domains),
                }
            }
            // A type-valued match (`Lt = match _ : Prop | ..`): its sort is its result — a constant `Prop` when the result is a proposition.
            Subterm::Match(m) => match &m.result {
                MatchResult::Family(motive) => {
                    let vars: Vec<Term> = (0..motive.arity())
                        .map(|_| Term::free_var(&context.fresh(None)))
                        .collect();
                    let refs: Vec<&Term> = vars.iter().collect();
                    Sort::from_universe(context, &motive.open(&refs))?
                }
                MatchResult::Ambient(goal) => Sort::from_universe(context, goal)?,
            },
            // A neutral type (a `Prop` hypothesis, or a stuck family application): its synthesized type is its sort.
            Subterm::Var(_) | Subterm::Apply(_) | Subterm::Proj(_) => {
                match synth_neutral(context, binders, opened, &reduced)? {
                    Some(sort) => Sort::from_universe(context, &sort)?,
                    None => {
                        probe_level_fallback("neutral type unsynthesizable", &reduced);
                        Sort::type_at(Level::zero())
                    }
                }
            }
            Subterm::Sort(_, level) => Sort::type_at(level.succ().map_err(ReduceError::Universe)?),
            Subterm::Instance(instance) => {
                Sort::of_under(context, binders, opened, &instance.head.to_term())?
            }
            // An unsolved metavariable is classified by the sort its own type states, which no solution changes: one whose type is `Prop` is a proposition before it is solved, so `@P: Prop, @p: P` makes `p` a bound before `P` is known, and one whose type's quality is open has that quality, for a reader that decides it or takes its own safe answer. A solved one never reaches here, since reduction substitutes its solution, and a sort is closed, so the spine an occurrence carries changes nothing. At a type its level is what the solution will say, so it keeps the fallback below.
            Subterm::Metavar(metavar) => {
                let result = context
                    .metavar_entry(metavar.id)
                    .map(|entry| entry.result.clone());
                let stated = match result {
                    Some(result) => match &*reduce(context, result)? {
                        Subterm::Sort(quality, level) => {
                            Some((context.universes().quality(*quality), level.clone()))
                        }
                        _ => None,
                    },
                    None => None,
                };
                match stated {
                    Some((Quality::Prop, level)) => Sort::prop_at(level),
                    Some((quality @ (Quality::Meta(_) | Quality::Param(_)), _)) => {
                        probe_level_fallback("unsolved metavariable", &reduced);
                        Sort {
                            quality,
                            level: Level::zero(),
                        }
                    }
                    Some((Quality::Type, _)) | None => {
                        probe_level_fallback("unsolved metavariable", &reduced);
                        Sort::type_at(Level::zero())
                    }
                }
            }
            _ => {
                probe_level_fallback("unclassified shape", &reduced);
                Sort::type_at(Level::zero())
            }
        })
    }

    /// Whether `type_`, as written, is an application of a declaration that lands in `Prop` — the kernel's rule, `curios-cert`'s `declared_into_prop`, which states why a proposition is known by its head and why only a declaration's word is taken. Here a declaration is a global name with an assumption, a member of the group being elaborated included: its signature was checked before any body was, and reduction reads it where the member is still a slot.
    pub(crate) fn declared_into_prop(
        context: &mut Context,
        binders: &Binders,
        opened: &Opened,
        type_: &Term,
    ) -> Result<Option<Level>, ReduceError> {
        let mut head = type_;
        while let Subterm::Apply(apply) = &**head {
            head = &apply.head;
        }
        let name = match &**head {
            Subterm::Var(var) => var.unwrap(),
            Subterm::Instance(Instance {
                head: InstanceHead::Var(var),
                ..
            }) => var.unwrap(),
            _ => return Ok(None),
        };
        let declared = matches!(&**type_, Subterm::Apply(_))
            && name.as_global().is_some()
            && context.assumption(name).is_some();
        if !declared {
            return Ok(None);
        }

        Ok(match synth_neutral(context, binders, opened, type_)? {
            Some(declared) => match &*reduce(context, declared)? {
                Subterm::Sort(quality, level)
                    if context.universes().quality(*quality).is_prop() =>
                {
                    Some(level.clone())
                }
                _ => None,
            },
            None => None,
        })
    }

    /// The universe term this sort denotes. The inverse of [`Sort::from_universe`]; used as the type-of-a-type a type-former reports.
    pub(crate) fn term(self) -> Term {
        Term::sort_at(self.quality, self.level)
    }

    /// Decode a universe term — a kind's codomain, a match motive, or a synthesized neutral type — into its sort. Distinct from [`Sort::of`], which classifies an arbitrary *type*: `from_universe(Prop) = Prop`, whereas `of(Prop) = Type` (the universe `Prop` is itself `Type`-sorted).
    fn from_universe(context: &mut Context, universe: &Term) -> Result<Sort, ReduceError> {
        let reduced = reduce(context, universe.clone())?;
        Ok(match &*reduced {
            Subterm::Sort(quality, level) => Sort {
                quality: context.universes().quality(*quality),
                level: level.clone(),
            },
            _ => {
                probe_level_fallback("universe term is not a sort", &reduced);
                Sort::type_at(Level::zero())
            }
        })
    }
}
