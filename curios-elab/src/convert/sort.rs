//! The universe a type inhabits, as conversion classifies it: `Prop` for a strict proposition, `Type` otherwise.

use {
    super::*,
    curios_core::{Subterm, Term},
};

/// The universe a type inhabits — `Prop` for a strict proposition, `Type` otherwise.
#[derive(Clone, PartialEq, Eq)]
pub(crate) enum Sort {
    Type(Level),
    Prop,
}

impl Sort {
    /// The sort of `type_`. Any two inhabitants of a `Prop` are definitionally equal (proof irrelevance), so a conversion problem at a prop type is discharged without comparing the sides. Conservative: a shape this cannot classify is reported as `Type` — under-approximating prop-ness is sound; the reverse (a non-prop reported as a prop) is the unsound direction and never happens.
    pub(crate) fn of(context: &mut Context, type_: &Term) -> Result<Sort, ReduceError> {
        Sort::of_in(context, &mut Vec::new(), type_)
    }

    /// [`Sort::of`] under the binders a surrounding telescope walk has opened. The `opened` scope is threaded rather than installed on the [`Context`] — see [`Opened`] for why that distinction is load-bearing.
    pub(crate) fn of_in(
        context: &mut Context,
        opened: &mut Vec<(Free, Term)>,
        type_: &Term,
    ) -> Result<Sort, ReduceError> {
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
                    Sort::Type(Level::zero())
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
                    Sort::Type(Level::zero())
                }
            },
            // A *non-empty* record of propositions is a proposition. The empty tuple `{}` is unit, not a prop: it is the result type of effects (`/std/print : .. -> {}`), so it stays `Type` (the `_` arm) and is kept at runtime rather than erased.
            Subterm::TupleType(TupleType { telescope, .. }) if !telescope.is_empty() => {
                // A later field may mention an earlier one, so each opened binder joins `opened` before the walk descends. See the `FuncType` arm below for why leaving it out is not merely imprecise but silently wrong.
                let mut tele = telescope.clone();
                let mut levels = Vec::new();
                let mark = opened.len();
                let sort = loop {
                    match tele {
                        Telescope::Cons(ty, rest) => {
                            if let Sort::Type(level) = Sort::of_in(context, opened, &ty)? {
                                levels.push(level);
                            }
                            let binder = context.fresh(rest.first_hint());
                            let v = Term::free_var(&binder);
                            opened.push((binder, ty));
                            tele = rest.open(&[&v]);
                        }
                        Telescope::Done(_) => {
                            break if levels.is_empty() {
                                Sort::Prop
                            } else {
                                Sort::Type(Level::max(levels))
                            };
                        }
                    }
                };
                opened.truncate(mark);
                sort
            }
            // The empty tuple `{}` is unit — it quantifies over nothing, so level 0 is the answer rather than a default, and like `Prop` below it is not worth reporting as a fallback.
            Subterm::TupleType(_) => Sort::Type(Level::zero()),
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
                | Intrinsic::HandleType => Sort::Type(Level::zero()),
                Intrinsic::ListType(element)
                | Intrinsic::CellType(element)
                | Intrinsic::IoType(element) => {
                    let element = element.clone();
                    match Sort::of_in(context, opened, &element)? {
                        Sort::Type(level) => Sort::Type(level),
                        // A list, cell, or description of proofs is not itself a proposition — it has length, identity, or an effect, so its inhabitants are distinguishable and proof irrelevance does not apply. It lands at `Type` instead, and `Prop : Type 0`. For `Io` that is what keeps sort-driven erasure from dropping a description of a proof and its host effect with it.
                        Sort::Prop => Sort::Type(Level::zero()),
                    }
                }
                // An intrinsic *value* in type position is not a shape this can classify, and unlike the formers above it is not expected.
                //
                // Listed exhaustively rather than caught by a wildcard, because the wildcard's answer is *wrong* for any former added after it: it pins the level at 0 while `curios-cert`'s `sort_of_intrinsic` reads the parameter's, and nothing catches the disagreement until the fixed prelude's kernel recheck reports a ground `Type` against a `Type.{u}`, naming no item. Every other intrinsic match in the workspace is exhaustive; this one has to be too, so a new former forces a level decision at compile time instead of inheriting a default.
                Intrinsic::Bin(..)
                | Intrinsic::BinAppend { .. }
                | Intrinsic::BinConcat { .. }
                | Intrinsic::BinEql(..)
                | Intrinsic::BinGet { .. }
                | Intrinsic::BinLen(..)
                | Intrinsic::BinSlice { .. }
                | Intrinsic::Bool(..)
                | Intrinsic::BoolAnd(..)
                | Intrinsic::BoolEql(..)
                | Intrinsic::BoolNeq(..)
                | Intrinsic::BoolOr(..)
                | Intrinsic::BoolXor(..)
                | Intrinsic::Byte(..)
                | Intrinsic::ByteEql(..)
                | Intrinsic::ByteLt(..)
                | Intrinsic::ByteLe(..)
                | Intrinsic::ByteToNat(..)
                | Intrinsic::Cell { .. }
                | Intrinsic::CellGet { .. }
                | Intrinsic::CellSet { .. }
                | Intrinsic::ProcExit { .. }
                | Intrinsic::Flt(..)
                | Intrinsic::FltAbs(..)
                | Intrinsic::FltAdd(..)
                | Intrinsic::FltCeil(..)
                | Intrinsic::FltCopysign(..)
                | Intrinsic::FltDiv(..)
                | Intrinsic::FltEql(..)
                | Intrinsic::FltFloor(..)
                | Intrinsic::FltLt(..)
                | Intrinsic::FltLe(..)
                | Intrinsic::FltMax(..)
                | Intrinsic::FltMin(..)
                | Intrinsic::FltMul(..)
                | Intrinsic::FltNearest(..)
                | Intrinsic::FltNeg(..)
                | Intrinsic::FltNeq(..)
                | Intrinsic::FltOfLeBytes { .. }
                | Intrinsic::FltRem(..)
                | Intrinsic::FltSqrt(..)
                | Intrinsic::FltSub(..)
                | Intrinsic::FltToInt { .. }
                | Intrinsic::FltToLeBytes(..)
                | Intrinsic::FltToNat { .. }
                | Intrinsic::FltTrunc(..)
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
                | Intrinsic::IoBind { .. }
                | Intrinsic::IoPure { .. }
                | Intrinsic::List { .. }
                | Intrinsic::ListAppend { .. }
                | Intrinsic::ListConcat { .. }
                | Intrinsic::ListGet { .. }
                | Intrinsic::ListLen { .. }
                | Intrinsic::ListMap { .. }
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
                | Intrinsic::NatRem { .. }
                | Intrinsic::NatShl(..)
                | Intrinsic::NatShr(..)
                | Intrinsic::NatSub(..)
                | Intrinsic::NatToByte(..)
                | Intrinsic::NatToFlt(..)
                | Intrinsic::NatToInt(..)
                | Intrinsic::NatXor(..) => {
                    probe_level_fallback("non-type intrinsic", &reduced);
                    Sort::Type(Level::zero())
                }
            },
            // A host call is a value, never a type, so it takes the same ground level the non-type intrinsics above do.
            Subterm::Foreign(..) => {
                probe_level_fallback("host call", &reduced);
                Sort::Type(Level::zero())
            }
            // Π into a proposition is a proposition.
            Subterm::FuncType(FuncType { telescope, .. }) => {
                // Each opened binder must carry its domain type, not merely be substituted in. Opening with a free variable nothing can type leaves `synth_neutral` returning `None` for every occurrence of it in the codomain, and that `None` is read as level 0 — so the sort of every dependent codomain collapsed to `Type 0` regardless of the binder's real level. That silently under-generalized exactly the declarations whose codomain mentions a binder: every concept wrapper, and every higher-order polymorphic function.
                let mut telescope = telescope.clone();
                let mut domains = Vec::new();
                let mark = opened.len();
                let sort = loop {
                    match telescope {
                        Telescope::Cons(domain, rest) => {
                            if let Sort::Type(level) = Sort::of_in(context, opened, &domain)? {
                                domains.push(level);
                            }
                            let binder = context.fresh(rest.first_hint());
                            let var = Term::free_var(&binder);
                            opened.push((binder, domain));
                            telescope = rest.open(&[&var]);
                        }
                        Telescope::Done(output) => {
                            break match Sort::of_in(context, opened, &output)? {
                                Sort::Prop => Sort::Prop,
                                Sort::Type(output) => {
                                    domains.push(output);
                                    Sort::Type(Level::max(domains))
                                }
                            };
                        }
                    }
                };
                opened.truncate(mark);
                sort
            }
            // A type-valued match (`Lt = match _ : Prop | ..`): its sort is the motive — a constant `Prop` when the result is a proposition.
            Subterm::Match(m) => {
                let motive = m.motive.clone();
                let vars: Vec<Term> = (0..motive.arity())
                    .map(|_| Term::free_var(&context.fresh(None)))
                    .collect();
                let refs: Vec<&Term> = vars.iter().collect();
                Sort::from_universe(context, &motive.open(&refs))?
            }
            // A neutral type (a `Prop` hypothesis, or a stuck family application): its synthesized type is its sort.
            Subterm::Var(_) | Subterm::Apply(_) | Subterm::Proj(_) => {
                match synth_neutral(context, opened, &reduced)? {
                    Some(sort) => Sort::from_universe(context, &sort)?,
                    None => {
                        probe_level_fallback("neutral type unsynthesizable", &reduced);
                        Sort::Type(Level::zero())
                    }
                }
            }
            Subterm::Type(level) => Sort::Type(level.succ().map_err(ReduceError::Universe)?),
            Subterm::Instance(instance) => Sort::of_in(context, opened, &instance.head.to_term())?,
            // `Prop` reaches here too, and `Prop : Type 0` is exactly right, so it is not a fallback and is not worth reporting. A `Metavar` is the opposite: an unsolved type pinned to level 0 is precisely the collapse under investigation.
            _ => {
                if !matches!(&*reduced, Subterm::Prop) {
                    probe_level_fallback("unclassified shape", &reduced);
                }
                Sort::Type(Level::zero())
            }
        })
    }

    /// The universe term this sort denotes — `Type` or `Prop`. The inverse of [`Sort::from_universe`]; used as the type-of-a-type a type-former reports.
    pub(crate) fn term(self) -> Term {
        match self {
            Sort::Type(level) => Term::type_at(level),
            Sort::Prop => Term::prop(),
        }
    }

    /// Decode a universe term — a kind's codomain, a match motive, or a synthesized neutral type — into its sort. Distinct from [`Sort::of`], which classifies an arbitrary *type*: `from_universe(Prop) = Prop`, whereas `of(Prop) = Type` (the universe `Prop` is itself `Type`-sorted).
    fn from_universe(context: &mut Context, universe: &Term) -> Result<Sort, ReduceError> {
        let reduced = reduce(context, universe.clone())?;
        Ok(match &*reduced {
            Subterm::Prop => Sort::Prop,
            Subterm::Type(level) => Sort::Type(level.clone()),
            _ => {
                probe_level_fallback("universe term is not a sort", &reduced);
                Sort::Type(Level::zero())
            }
        })
    }
}
