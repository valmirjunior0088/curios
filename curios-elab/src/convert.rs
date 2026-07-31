mod prim;
use crate::TermBuilders;
use prim::*;

#[cfg(test)]
mod tests;

use {
    super::{Context, check, reduce, reduce_forced, unfold_rec, unfold_rec_apply},
    curios_base::Plicity,
    curios_core::{
        Apply, Bound, Carrier, Cases, Field, Free, Func, FuncType, InductType, Level, Many, Match,
        Metavar, Prim, Proj, Rec, RecMember, ReduceError, Scope, Struct, StructType, Subterm,
        Telescope, Term, Three, Tuple, TupleType, UniverseConstraintKind, UniverseConstraintOrigin,
        UniverseContext, UniverseInst, Var, Variant, Visit, instantiate_universe_levels_scoped,
        project_erased_universes,
    },
    std::collections::{HashMap, HashSet, VecDeque},
};

/// The outcome of attempting to solve a metavariable against a candidate.
enum Solved {
    /// Committed `m := t`.
    Done,
    /// Not yet solvable (embedded unsolved metavariable): the goal is parked on the `blocked` queue and retried after later progress.
    Postponed,
    /// Unsolvable: occurs-check, scope-check, or re-validation failure.
    Failed,
}

fn instantiate_bound_at<B: Bound>(
    context: &mut Context,
    universe_context: &UniverseContext,
    value: &B,
    universes: &[Level],
) -> Result<B, ReduceError> {
    context
        .instantiate_universe_bound_at(universe_context, value, universes)
        .map_err(ReduceError::Universe)
}

pub(crate) fn convert(
    context: &mut Context,
    type_: &Term,
    this: &Term,
    that: &Term,
) -> Result<bool, ReduceError> {
    // The strict boolean-oracle view: "not yet decidable" is *not* "equal". Everything that needs a definite yes/no (re-validation, the inverter, type-level dispatch) uses this; only the elaboration turnaround (`expect`) may treat `Blocked` as provisional success by parking it.
    Ok(matches!(
        convert_outcome(context, type_, this, that)?,
        Outcome::Converts
    ))
}

pub(crate) fn convert_outcome(
    context: &mut Context,
    type_: &Term,
    this: &Term,
    that: &Term,
) -> Result<Outcome, ReduceError> {
    Convert::new(type_.clone(), this.clone(), that.clone()).outcome(context)
}

/// The verdict of a conversion run, distinguishing "provably unequal" from "not yet decidable".
#[derive(Debug)]
pub(crate) enum Outcome {
    /// Definitionally equal.
    Converts,
    /// A hard structural mismatch — provably unequal, no solution can help.
    Mismatch,
    /// Quiesced with constraints still blocked on unsolved metavariables: undecided either way. The blocked goals are surrendered to the caller — the elaboration turnaround parks them on the `Context` to be retried when a watched metavariable is solved.
    Blocked(Vec<Goal>),
}

/// Binders opened locally by [`Sort::of`] while walking a telescope, innermost last.
///
/// These deliberately do *not* go into the [`Context`]. `Sort::of` runs on every conversion goal (through `is_prop`), and `Context::assume` bumps `mutation_stamp`, which is what validates the memoization caches — assuming here would invalidate them continuously and starve a coinductive comparison of its budget. Keeping the binders local also keeps `Sort::of` observationally read-only, which the conversion history relies on: labels minted here are never recorded in `Convert::minted`, so they must never reach a goal. They cannot, because `Sort::of` returns a `Sort`.
pub(crate) type Opened = [(Free, Term)];

/// Synthesize the type of a neutral (a `Var`/`Apply`/`Proj` spine) *without* validating its subterms. Returns `None` when the head is out of scope or the spine is not a typeable neutral — callers fall back conservatively. Built only from the same primitives `infer` uses (`Context::assumption`, `reduce`, `Telescope::open`/`nth`), so there is no duplicated typing judgment to drift from `infer`.
pub(crate) fn synth_neutral(
    context: &mut Context,
    opened: &Opened,
    term: &Term,
) -> Result<Option<Term>, ReduceError> {
    match &**term {
        Subterm::Var(var) => {
            let name = var.unwrap();
            // Locally opened binders shadow the context, innermost first.
            if let Some((_, type_)) = opened.iter().rev().find(|(bound, _)| bound == name) {
                return Ok(Some(type_.clone()));
            }
            context
                .instantiate_assumption_universes(name)
                .map(|instance| instance.map(|(type_, _)| type_))
                .map_err(ReduceError::Universe)
        }
        // A recursive member's type is carried by its own group — no lookup, and no unfolding, so this cannot re-enter the group it names.
        Subterm::RecMember(RecMember { group, index }) => Ok(Some(group.member_type(*index))),

        // A universe-polymorphic head, at the levels this occurrence chose. The scheme is read *uninstantiated* and substituted at `levels`; going through the `Var` arm below would instead instantiate it at fresh levels and then have nothing left to substitute.
        Subterm::UniverseInst(UniverseInst { head, levels }) => match &**head {
            Subterm::Var(var) => {
                let name = var.unwrap();
                if let Some((_, type_)) = opened.iter().rev().find(|(bound, _)| bound == name) {
                    return Ok(Some(type_.clone()));
                }
                let Some(scheme) = context.assumption(name).cloned() else {
                    return Ok(None);
                };
                instantiate_universe_levels_scoped(&scheme, levels)
                    .map(Some)
                    .map_err(ReduceError::Universe)
            }
            Subterm::RecMember(RecMember { group, index }) => {
                let group = group
                    .instantiate_universes(levels)
                    .map_err(ReduceError::Universe)?;
                Ok(Some(group.member_type(*index)))
            }
            _ => Ok(None),
        },

        Subterm::Apply(Apply { head, params, .. }) => {
            let Some(head_type) = synth_neutral(context, opened, head)? else {
                return Ok(None);
            };

            match Term::unwrap_or_clone(reduce(context, head_type)?) {
                Subterm::FuncType(FuncType { telescope, .. })
                    if telescope.len() == params.len() =>
                {
                    let refs = params.iter().collect::<Vec<_>>();
                    Ok(Some(telescope.open(&refs)))
                }
                // A partially applied spine still has a type: the residual function type, with the supplied arguments substituted into the entries that remain.
                Subterm::FuncType(FuncType {
                    telescope,
                    plicities,
                }) if telescope.len() > params.len() => {
                    let residual = telescope.open_params(params);
                    Ok(Some(
                        Subterm::FuncType(FuncType {
                            telescope: residual,
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
            let Some(head_type) = synth_neutral(context, opened, head)? else {
                return Ok(None);
            };

            match Term::unwrap_or_clone(reduce(context, head_type)?) {
                Subterm::TupleType(TupleType { telescope, .. }) => {
                    Ok(telescope.nth(*index, |j| Term::proj(head.clone(), j)))
                }
                // A nominal structure's field types live on its declaration, instantiated at the head's universes and then at its parameters — the same two steps `elaborate_proj` takes. Concept dispatch is a projection out of a witness dictionary, so this arm is what types a method call.
                Subterm::StructType(StructType {
                    name,
                    universes,
                    params,
                }) => {
                    let Some(declaration) = context.struct_decl(&name).cloned() else {
                        return Ok(None);
                    };
                    if declaration.fields.len() < params.len() {
                        return Ok(None);
                    }
                    let fields = instantiate_bound_at(
                        context,
                        &declaration.universe_context,
                        &declaration.fields,
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

/// Recover the parameter types of an application from the head's function type, opening each successive entry with the actual arguments (dependency). `None` when the head's type is unavailable or not a `FuncType` of matching arity — callers fall back to comparing arguments at `Term::type_ground()`.
fn apply_param_types(
    context: &mut Context,
    head: &Term,
    params: &[Term],
) -> Result<Option<Vec<Term>>, ReduceError> {
    let Some(head_type) = synth_neutral(context, &[], head)? else {
        return Ok(None);
    };

    let telescope = match Term::unwrap_or_clone(reduce(context, head_type)?) {
        Subterm::FuncType(FuncType { telescope, .. }) if telescope.len() == params.len() => {
            telescope
        }
        _ => return Ok(None),
    };

    let mut types = Vec::with_capacity(params.len());
    telescope.walk(params, |_, _, ty| {
        types.push(ty.clone());
        Ok(())
    })?;

    Ok(Some(types))
}

/// Report a site that could not determine a universe level and fell back to `Type 0`. Diagnostic only: it changes nothing, and exists to answer whether the concept-wrapper universe failures originate in these fallbacks rather than in how a wrapper is generalized.
#[cfg(feature = "profile")]
fn probe_level_fallback(site: &'static str, type_: &Term) {
    curios_profile::tracing::debug!(
        target: "curios_elab::sort",
        site,
        type_ = %type_,
        "level defaulted to 0",
    );
}

#[cfg(not(feature = "profile"))]
fn probe_level_fallback(_site: &'static str, _type_: &Term) {}

/// The universe a type inhabits — `Prop` for a strict proposition, `Type` otherwise.
#[derive(Clone, PartialEq, Eq)]
pub(crate) enum Sort {
    Type(Level),
    Prop,
}

impl Sort {
    /// The sort of `type_`. Any two inhabitants of a `Prop` are definitionally equal (proof irrelevance), so a conversion goal at a prop type is discharged without comparing the sides. Conservative: a shape this cannot classify is reported as `Type` — under-approximating prop-ness is sound; the reverse (a non-prop reported as a prop) is the unsound direction and never happens.
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
            // A primitive type former states its own level.
            //
            // A closed primitive is small: it quantifies over nothing, so it sits at level 0. A parameterized one carries its parameter's level — `Lst : Type u -> Type u` — and pinning those at 0 would claim the type is smaller than it is, which is the unsound direction: it is what would let a large type be stored in a small universe.
            Subterm::Prim(prim) => match prim {
                Prim::BoolType
                | Prim::NatType
                | Prim::ByteType
                | Prim::IntType
                | Prim::FltType
                | Prim::BinType(_)
                | Prim::HandleType => Sort::Type(Level::zero()),
                Prim::LstType(element) | Prim::CellType(element) => {
                    let element = element.clone();
                    match Sort::of_in(context, opened, &element)? {
                        Sort::Type(level) => Sort::Type(level),
                        // A list or cell of proofs is not itself a proposition — it has length, or identity, so its inhabitants are distinguishable and proof irrelevance does not apply. It lands at `Type` instead, and `Prop : Type 0`.
                        Sort::Prop => Sort::Type(Level::zero()),
                    }
                }
                // A primitive *value* in type position is not a shape this can classify, and unlike the formers above it is not expected.
                _ => {
                    probe_level_fallback("non-type primitive", &reduced);
                    Sort::Type(Level::zero())
                }
            },
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
            Subterm::UniverseInst(instance) => Sort::of_in(context, opened, &instance.head)?,
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

/// One conversion constraint, `this ≡ that : type_` (comparison is type-directed: the type drives η-expansion and proof irrelevance). Public because a [`Outcome::Blocked`] verdict surrenders its still-blocked goals to the caller, which parks them on the [`Context`] (as `ParkedWork::Conversion`) for retry once a watched metavariable is solved.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Goal {
    pub type_: Term,
    pub this: Term,
    pub that: Term,
}

#[derive(Debug)]
pub(crate) struct Convert {
    // Structural goals already seen, stored as `history_key` fingerprints. A recurring goal is assumed to hold — the coinductive reading: a genuine cycle leaves nothing but itself to check, and any finite disagreement surfaces on a sibling goal first.
    history: HashSet<Goal>,
    pending: VecDeque<Goal>,
    // Constraints postponed because a side is flexible but not yet solvable (flex–flex with distinct heads, or a candidate carrying an unsolved metavariable). Retried whenever a fresh solution lands.
    blocked: Vec<Goal>,
    // Whether a metavariable was solved since the last `blocked` sweep — the signal that retrying `blocked` could make further progress.
    progress: bool,
    // The opening labels this conversion minted to compare under binders (label → mint sequence), and the placeholder pool `history_key` renames them into. Fingerprints only — the goals actually processed keep their globally-unique labels.
    minted: HashMap<Free, usize>,
    placeholders: Vec<Term>,
}

impl Convert {
    fn compare_levels(
        context: &mut Context,
        left: &[Level],
        right: &[Level],
    ) -> Result<bool, ReduceError> {
        if left.len() != right.len() {
            return Ok(false);
        }
        for (left, right) in left.iter().zip(right) {
            context
                .universes_mut()
                .add_eq(
                    left.clone(),
                    right.clone(),
                    UniverseConstraintOrigin::new(UniverseConstraintKind::Conversion),
                )
                .map_err(ReduceError::Universe)?;
        }
        Ok(true)
    }

    fn new(type_: Term, this: Term, that: Term) -> Self {
        Self {
            history: HashSet::new(),
            pending: VecDeque::from([Goal { type_, this, that }]),
            blocked: Vec::new(),
            progress: false,
            minted: HashMap::new(),
            placeholders: Vec::new(),
        }
    }

    fn in_history(&mut self, goal: &Goal) -> bool {
        !self.history.insert(goal.clone())
    }

    /// Mint a fresh label for opening scopes under structural comparison, recording it for `history_key`. The label itself is ordinary entropy freshness — recording changes nothing about the terms conversion builds.
    fn opening(&mut self, context: &mut Context, hint: Option<&str>) -> Free {
        let binder = context.fresh(hint);
        self.minted.insert(binder.clone(), self.minted.len());
        binder
    }

    /// The history fingerprint of a goal: every opening label this conversion minted (see [`Convert::opening`]) is renamed to a per-run placeholder, in mint order. A comparison recurring under later openings — the same match arm re-opened at a fresh binder on each round of an unfolding cycle — differs from its previous visit only in that entropy, so the rename collapses the rounds onto one `history` entry and the recurrence rule in `drain` fires: a cycle with no finite disagreement is definitional equality, as for equirecursive types. Genuinely growing comparisons never recur and still spend the budget.
    ///
    /// A pure key transform: the goal conversion processes keeps its globally-unique labels, so no freshness contract (`capture` inversion, `abstract_occurrences`, `define_fresh`) is involved — the placeholders are themselves entropy-fresh and never enter a real term.
    fn history_key(&mut self, context: &mut Context, goal: &Goal) -> Goal {
        if self.minted.is_empty() {
            return goal.clone();
        }

        let mut free = goal.type_.free_vars();
        free.extend(goal.this.free_vars());
        free.extend(goal.that.free_vars());

        let mut present = free
            .iter()
            .filter_map(|name| self.minted.get(name).map(|&seq| (seq, name)))
            .collect::<Vec<_>>();
        if present.is_empty() {
            return goal.clone();
        }

        // Mint order, so a recurrence — whose structural steps minted their openings in the same relative order — maps positionally onto the same placeholders.
        present.sort_unstable_by_key(|&(seq, _)| seq);
        let labels = present.iter().map(|&(_, l)| l).collect::<Vec<_>>();

        while self.placeholders.len() < labels.len() {
            self.placeholders.push(Term::free_var(&context.fresh(None)));
        }
        let refs = self.placeholders[..labels.len()].iter().collect::<Vec<_>>();

        Goal {
            type_: goal.type_.capture(&labels).release(&refs),
            this: goal.this.capture(&labels).release(&refs),
            that: goal.that.capture(&labels).release(&refs),
        }
    }

    pub(crate) fn enqueue(&mut self, type_: Term, this: Term, that: Term) {
        self.pending.push_back(Goal { type_, this, that });
    }

    /// Enqueue the bodies of two arity-3 cons arms (`Bin`/`Lst`) for comparison, opened under shared fresh binders for `(head, tail, ih)`.
    fn compare_cons_three(
        &mut self,
        context: &mut Context,
        this: Scope<Three>,
        that: Scope<Three>,
    ) {
        let a = Term::free_var(&self.opening(context, None));
        let b = Term::free_var(&self.opening(context, None));
        let c = Term::free_var(&self.opening(context, None));
        self.enqueue(
            Term::type_ground(),
            this.open(&[&a, &b, &c]),
            that.open(&[&a, &b, &c]),
        );
    }

    fn dequeue(&mut self, context: &Context) -> Result<Option<Goal>, ReduceError> {
        context.spend()?;

        Ok(self.pending.pop_front())
    }

    fn compare_func_type(
        &mut self,
        context: &mut Context,
        this: FuncType,
        that: FuncType,
    ) -> Result<bool, ReduceError> {
        // Plicity is part of a function type's identity and calling convention: an explicit slot and an implicit/witness slot are *not* convertible even when their domains and results are, so that a convertible annotation or alias can never reinterpret which binders elaboration inserts. Compare the whole plicity vector (arity included) up front — the telescope walk below then compares the dependent domains.
        if this.plicities != that.plicities {
            return Ok(false);
        }
        fn walk(
            cmp: &mut Convert,
            context: &mut Context,
            this: &Telescope<Term>,
            that: &Telescope<Term>,
        ) -> Result<bool, ReduceError> {
            match (this, that) {
                (Telescope::Cons(ty_a, rest_a), Telescope::Cons(ty_b, rest_b)) => {
                    cmp.enqueue(Term::type_ground(), ty_a.clone(), ty_b.clone());
                    let v = Term::free_var(&cmp.opening(context, rest_a.first_hint()));
                    let inner_a = rest_a.open(&[&v]);
                    let inner_b = rest_b.open(&[&v]);
                    walk(cmp, context, &inner_a, &inner_b)
                }
                (Telescope::Done(out_a), Telescope::Done(out_b)) => {
                    cmp.enqueue(Term::type_ground(), (**out_a).clone(), (**out_b).clone());
                    Ok(true)
                }
                _ => Ok(false),
            }
        }
        walk(self, context, &this.telescope, &that.telescope)
    }

    /// η-frame at a function type of arity `n`: mint `n` fresh argument variables (recorded openings — see [`Convert::history_key`]) and recover the codomain after instantiating them, falling back to `Type` when `type_` does not reduce to a function type.
    fn func_eta_args(
        &mut self,
        context: &mut Context,
        n: usize,
        type_: Term,
    ) -> Result<(Vec<Term>, Term), ReduceError> {
        let ys: Vec<Term> = (0..n)
            .map(|_| Term::free_var(&self.opening(context, None)))
            .collect();
        let output_type = match Term::unwrap_or_clone(reduce(context, type_)?) {
            Subterm::FuncType(FuncType { telescope, .. }) => {
                telescope.open(&ys.iter().collect::<Vec<_>>())
            }
            _ => Term::type_ground(),
        };
        Ok((ys, output_type))
    }

    fn compare_func(
        &mut self,
        context: &mut Context,
        this: Func,
        that: Func,
        type_: Term,
    ) -> Result<bool, ReduceError> {
        // Plicity is part of a function's canonical identity. Well-typed functions compared at the same function type necessarily agree, but the explicit check preserves the Core invariant and rejects malformed or pre-elaboration terms that reach conversion unexpectedly.
        if this.plicities != that.plicities {
            return Ok(false);
        }
        let (ys, output_type) = self.func_eta_args(context, this.telescope.len(), type_)?;
        let y_refs = ys.iter().collect::<Vec<_>>();
        self.enqueue(
            output_type,
            this.telescope.open(&y_refs),
            that.telescope.open(&y_refs),
        );

        Ok(true)
    }

    fn compare_apply(
        &mut self,
        context: &mut Context,
        this: Apply,
        that: Apply,
    ) -> Result<bool, ReduceError> {
        if this.params.len() != that.params.len() {
            return Ok(false);
        }

        // Recover the real argument types from the head's function type so η fires at the correct type (e.g. a unit-typed argument is compared at `()`, where proof irrelevance makes distinct neutrals equal). Falls back to `Term::type_ground()` when the head's type is unavailable.
        let param_types = apply_param_types(context, &this.head, &this.params)?;

        self.enqueue(Term::type_ground(), this.head, that.head);

        for (i, (a, b)) in this.params.into_iter().zip(that.params).enumerate() {
            let type_ = param_types
                .as_ref()
                .and_then(|types| types.get(i).cloned())
                .unwrap_or_else(Term::type_ground);
            self.enqueue(type_, a, b);
        }

        Ok(true)
    }

    /// Compare applications of the same structural recursive member without assuming that member is injective. Convertible spines establish the result by congruence. A genuinely different spine instead triggers one symmetric delta step: distinct arguments may still produce equal results for a recursive function.
    fn compare_same_rec_apply(
        &mut self,
        context: &mut Context,
        this: Apply,
        that: Apply,
        type_: Term,
    ) -> Result<bool, ReduceError> {
        if this.params.len() != that.params.len() {
            return Ok(false);
        }

        let param_types = apply_param_types(context, &this.head, &this.params)?;
        let mut blocked = false;
        for (index, (a, b)) in this.params.iter().zip(&that.params).enumerate() {
            let param_type = param_types
                .as_ref()
                .and_then(|types| types.get(index).cloned())
                .unwrap_or_else(Term::type_ground);
            match convert_outcome(context, &param_type, a, b)? {
                Outcome::Converts => {}
                Outcome::Blocked(_) => blocked = true,
                Outcome::Mismatch => {
                    let this = unfold_rec_apply(context, this)?;
                    let that = unfold_rec_apply(context, that)?;
                    return match (this, that) {
                        (Some(this), Some(that)) => {
                            self.enqueue(type_, this, that);
                            Ok(true)
                        }
                        _ => Ok(false),
                    };
                }
            }
        }

        match blocked {
            true => self.compare_apply(context, this, that),
            false => Ok(true),
        }
    }

    fn compare_tuple_type(
        &mut self,
        context: &mut Context,
        this: TupleType,
        that: TupleType,
    ) -> Result<bool, ReduceError> {
        fn walk(
            cmp: &mut Convert,
            context: &mut Context,
            this: &Telescope<()>,
            that: &Telescope<()>,
        ) -> Result<bool, ReduceError> {
            match (this, that) {
                (Telescope::Cons(ty_a, rest_a), Telescope::Cons(ty_b, rest_b)) => {
                    // Field labels are part of a tuple type's identity: `{ a : Nat } ≢ { Nat } ≢ { b : Nat }` (the unlabeled "" is just another label). This is deliberately tuple-only — function-type parameter names stay alpha-convertible (see `compare_func_type`, where `first_hint` feeds freshness, never equality).
                    if rest_a.first_hint().unwrap_or_default()
                        != rest_b.first_hint().unwrap_or_default()
                    {
                        return Ok(false);
                    }
                    cmp.enqueue(Term::type_ground(), ty_a.clone(), ty_b.clone());
                    let v = Term::free_var(&cmp.opening(context, rest_a.first_hint()));
                    let inner_a = rest_a.open(&[&v]);
                    let inner_b = rest_b.open(&[&v]);
                    walk(cmp, context, &inner_a, &inner_b)
                }
                (Telescope::Done(_), Telescope::Done(_)) => Ok(true),
                _ => Ok(false),
            }
        }
        walk(self, context, &this.telescope, &that.telescope)
    }

    fn compare_tuple(
        &mut self,
        context: &mut Context,
        this: Tuple,
        that: Tuple,
        type_: Term,
    ) -> Result<bool, ReduceError> {
        let n = this.fields.len();
        if n != that.fields.len() {
            return Ok(false);
        }

        let cur = match Term::unwrap_or_clone(reduce(context, type_)?) {
            Subterm::TupleType(TupleType { telescope, .. }) if telescope.len() == n => {
                Some(telescope)
            }
            _ => None,
        };

        self.enqueue_fields(this.fields, that.fields, cur);

        Ok(true)
    }

    fn compare_proj(&mut self, this: Proj, that: Proj) -> Result<bool, ReduceError> {
        if this.field != that.field {
            return Ok(false);
        }
        self.enqueue(Term::type_ground(), this.head, that.head);
        Ok(true)
    }

    fn compare_induct_type(
        &mut self,
        context: &mut Context,
        this: InductType,
        that: InductType,
    ) -> Result<bool, ReduceError> {
        if this.name != that.name
            || this.params.len() != that.params.len()
            || this.indices.len() != that.indices.len()
        {
            return Ok(false);
        }
        if !Self::compare_levels(context, &this.universes, &that.universes)? {
            return Ok(false);
        }

        // Recover the full param+index telescope from the registry so each argument compares at its declared type rather than a flat `Type`. When an index is a proof, that type is a proposition and irrelevance applies — a stuck `Eq(P, p, q)` converts with `Eq(P, p, p)`. Falls back to `Type` if the inductive is somehow absent or arity-mismatched.
        let this_args: Vec<Term> = this
            .params
            .iter()
            .chain(this.indices.iter())
            .cloned()
            .collect();
        let arg_types = match context.induct_decl(&this.name).cloned() {
            Some(induct_decl) => Some(instantiate_bound_at(
                context,
                &induct_decl.universe_context,
                &induct_decl.indices,
                &this.universes,
            )?),
            None => None,
        };
        let arg_types = match arg_types {
            Some(telescope) if telescope.len() == this_args.len() => {
                let mut types = Vec::with_capacity(this_args.len());
                telescope.walk(&this_args, |_, _, ty| {
                    types.push(ty.clone());
                    Ok(())
                })?;
                Some(types)
            }
            _ => None,
        };

        let args = this
            .params
            .into_iter()
            .chain(this.indices)
            .zip(that.params.into_iter().chain(that.indices));
        for (i, (a, b)) in args.enumerate() {
            let type_ = arg_types
                .as_ref()
                .and_then(|types| types.get(i).cloned())
                .unwrap_or_else(Term::type_ground);
            self.enqueue(type_, a, b);
        }

        Ok(true)
    }

    /// Enqueue each `this`/`that` field pair at the type the telescope assigns it (each `rest` opened at the field value), defaulting to `Type` once the telescope is exhausted or absent. Shared by `compare_variant`, `compare_struct`, and `compare_tuple`: η and proof irrelevance must fire per field rather than at a flat `Type`.
    fn enqueue_fields<B: Bound>(
        &mut self,
        this: Vec<Term>,
        that: Vec<Term>,
        mut telescope: Option<Telescope<B>>,
    ) {
        for (a, b) in this.into_iter().zip(that) {
            let type_ = match telescope.take() {
                Some(Telescope::Cons(ty, rest)) => {
                    telescope = Some(rest.open(&[&a]));
                    ty
                }
                _ => Term::type_ground(),
            };
            self.enqueue(type_, a, b);
        }
    }

    fn compare_variant(
        &mut self,
        context: &mut Context,
        this: Variant,
        that: Variant,
    ) -> Result<bool, ReduceError> {
        if this.name != that.name
            || this.tag != that.tag
            || this.params.len() != that.params.len()
            || this.payload.len() != that.payload.len()
        {
            return Ok(false);
        }
        if !Self::compare_levels(context, &this.universes, &that.universes)? {
            return Ok(false);
        }

        // Recover the payload types from the constructor's registry telescope (instantiated at this side's parameters) so each payload field compares at its own type — η and proof irrelevance fire per field, instead of a structural compare at `Type`. Same recovery `erase` uses; falls back to `Type` if the inductive is somehow absent.
        let telescope = match context.induct_decl(&this.name).cloned() {
            Some(induct_decl) => {
                let constructor = induct_decl.constructor(&this.tag);
                match constructor {
                    Some(constructor) => Some(
                        instantiate_bound_at(
                            context,
                            &induct_decl.universe_context,
                            &constructor.telescope,
                            &this.universes,
                        )?
                        .open_params(&this.params),
                    ),
                    None => None,
                }
            }
            None => None,
        };

        for (a, b) in this.params.into_iter().zip(that.params) {
            self.enqueue(Term::type_ground(), a, b);
        }

        self.enqueue_fields(this.payload, that.payload, telescope);

        Ok(true)
    }

    fn compare_struct_type(
        &mut self,
        context: &mut Context,
        this: StructType,
        that: StructType,
    ) -> Result<bool, ReduceError> {
        if this.name != that.name || this.params.len() != that.params.len() {
            return Ok(false);
        }
        if !Self::compare_levels(context, &this.universes, &that.universes)? {
            return Ok(false);
        }

        for (a, b) in this.params.into_iter().zip(that.params) {
            self.enqueue(Term::type_ground(), a, b);
        }

        Ok(true)
    }

    fn compare_struct(
        &mut self,
        context: &mut Context,
        this: Struct,
        that: Struct,
    ) -> Result<bool, ReduceError> {
        if this.name != that.name
            || this.params.len() != that.params.len()
            || this.fields.len() != that.fields.len()
        {
            return Ok(false);
        }
        if !Self::compare_levels(context, &this.universes, &that.universes)? {
            return Ok(false);
        }

        // Recover the field types from the registry (instantiated at this side's parameters) so each field compares at its own type — η and proof irrelevance fire per field, as `compare_tuple` does with the tuple type's telescope. Falls back to `Type` if the struct is somehow absent.
        let telescope = match context.struct_decl(&this.name).cloned() {
            Some(struct_decl) => Some(
                instantiate_bound_at(
                    context,
                    &struct_decl.universe_context,
                    &struct_decl.fields,
                    &this.universes,
                )?
                .open_params(&this.params),
            ),
            None => None,
        };

        for (a, b) in this.params.into_iter().zip(that.params) {
            self.enqueue(Term::type_ground(), a, b);
        }

        self.enqueue_fields(this.fields, that.fields, telescope);

        Ok(true)
    }

    fn compare_match(
        &mut self,
        context: &mut Context,
        this: Match,
        that: Match,
    ) -> Result<bool, ReduceError> {
        self.enqueue(Term::type_ground(), this.head, that.head);

        // The motive's arity is 1 except for an annotated inductive-match motive (pattern binders then the scrutinee); different arities are structurally distinct.
        if this.motive.arity() != that.motive.arity() {
            return Ok(false);
        }

        let labels = (0..this.motive.arity())
            .map(|_| Term::free_var(&self.opening(context, None)))
            .collect::<Vec<_>>();
        let label_refs = labels.iter().collect::<Vec<_>>();
        self.enqueue(
            Term::type_ground(),
            this.motive.open(&label_refs),
            that.motive.open(&label_refs),
        );

        match (this.cases, that.cases) {
            (
                Cases::Bool {
                    false_case: this_false,
                    true_case: this_true,
                },
                Cases::Bool {
                    false_case: that_false,
                    true_case: that_true,
                },
            ) => {
                self.enqueue(Term::type_ground(), this_false, that_false);
                self.enqueue(Term::type_ground(), this_true, that_true);
                Ok(true)
            }

            (
                Cases::Switch {
                    cases: this_cases,
                    default: this_default,
                },
                Cases::Switch {
                    cases: that_cases,
                    default: that_default,
                },
            ) => {
                if this_cases.len() != that_cases.len() {
                    return Ok(false);
                }

                for ((kl, vl), (kr, vr)) in this_cases.into_iter().zip(that_cases) {
                    if kl != kr {
                        return Ok(false);
                    }
                    self.enqueue(Term::type_ground(), vl, vr);
                }

                self.enqueue(Term::type_ground(), this_default, that_default);
                Ok(true)
            }

            // The pattern is elaboration-time data, fully reflected in the motive scope once checked — convertibility ignores it. The default, though, is a real arm: two matches convert only if their catch-alls agree in presence and body.
            (
                Cases::Induct {
                    cases: this_cases,
                    default: this_default,
                    ..
                },
                Cases::Induct {
                    cases: that_cases,
                    default: that_default,
                    ..
                },
            ) => {
                if this_cases.len() != that_cases.len() {
                    return Ok(false);
                }

                for ((this_atom, this_scope), (that_atom, that_scope)) in
                    this_cases.into_iter().zip(that_cases)
                {
                    if this_atom != that_atom || this_scope.arity() != that_scope.arity() {
                        return Ok(false);
                    }

                    let binders = (0..this_scope.arity())
                        .map(|_| Term::free_var(&self.opening(context, None)))
                        .collect::<Vec<_>>();
                    let binder_refs = binders.iter().collect::<Vec<_>>();

                    self.enqueue(
                        Term::type_ground(),
                        this_scope.open(&binder_refs),
                        that_scope.open(&binder_refs),
                    );
                }

                match (this_default, that_default) {
                    (None, None) => {}
                    (Some(this), Some(that)) => self.enqueue(Term::type_ground(), this, that),
                    _ => return Ok(false),
                }

                Ok(true)
            }

            (
                Cases::FreeMonoid {
                    carrier: this_carrier,
                },
                Cases::FreeMonoid {
                    carrier: that_carrier,
                },
            ) => match (this_carrier, that_carrier) {
                (
                    Carrier::Nat {
                        empty_case: this_empty,
                        cons_case: this_cons,
                    },
                    Carrier::Nat {
                        empty_case: that_empty,
                        cons_case: that_cons,
                    },
                ) => {
                    self.enqueue(Term::type_ground(), this_empty, that_empty);

                    // The unary cons arm binds (predecessor, ih); open both under shared fresh binders and compare the bodies.
                    let a = Term::free_var(&self.opening(context, None));
                    let b = Term::free_var(&self.opening(context, None));
                    self.enqueue(
                        Term::type_ground(),
                        this_cons.open(&[&a, &b]),
                        that_cons.open(&[&a, &b]),
                    );

                    Ok(true)
                }
                (
                    Carrier::Bin {
                        empty_case: this_empty,
                        cons_case: this_cons,
                        ..
                    },
                    Carrier::Bin {
                        empty_case: that_empty,
                        cons_case: that_cons,
                        ..
                    },
                ) => {
                    self.enqueue(Term::type_ground(), this_empty, that_empty);
                    self.compare_cons_three(context, this_cons, that_cons);

                    Ok(true)
                }
                (
                    Carrier::Lst {
                        elem: this_elem,
                        empty_case: this_empty,
                        cons_case: this_cons,
                    },
                    Carrier::Lst {
                        elem: that_elem,
                        empty_case: that_empty,
                        cons_case: that_cons,
                    },
                ) => {
                    self.enqueue(Term::type_ground(), this_elem, that_elem);
                    self.enqueue(Term::type_ground(), this_empty, that_empty);
                    self.compare_cons_three(context, this_cons, that_cons);

                    Ok(true)
                }
                // Distinct carriers are never structurally convertible.
                _ => Ok(false),
            },

            // Unreachable under the dispatch guard (same `Cases` discriminant); distinct kinds are never structurally convertible.
            _ => Ok(false),
        }
    }

    fn compare_rec(
        &mut self,
        context: &mut Context,
        this: Rec,
        that: Rec,
    ) -> Result<bool, ReduceError> {
        if this.group.length() != that.group.length() {
            return Ok(false);
        }

        let labels = (0..this.group.length())
            .map(|_| Term::free_var(&self.opening(context, None)))
            .collect::<Vec<_>>();

        let labels = labels.iter().collect::<Vec<_>>();

        for (this_member, that_member) in this.group.iter().zip(that.group.iter()) {
            let (this_type, this_body) = (&this_member.type_, &this_member.body);
            let (that_type, that_body) = (&that_member.type_, &that_member.body);
            self.enqueue(
                Term::type_ground(),
                this_type.open(&labels),
                that_type.open(&labels),
            );
            self.enqueue(
                Term::type_ground(),
                this_body.open(&labels),
                that_body.open(&labels),
            );
        }

        self.enqueue(
            Term::type_ground(),
            this.tail.open(&labels),
            that.tail.open(&labels),
        );

        Ok(true)
    }

    fn eta_expand_func(
        &mut self,
        context: &mut Context,
        func: Func,
        other: Term,
        type_: Term,
    ) -> Result<bool, ReduceError> {
        let (ys, output_type) = self.func_eta_args(context, func.telescope.len(), type_)?;
        let body = func.telescope.open(&ys.iter().collect::<Vec<_>>());
        self.enqueue(output_type, body, Term::apply(other, ys));
        Ok(true)
    }

    fn eta_expand_tuple(
        &mut self,
        context: &mut Context,
        tuple: Tuple,
        other: Term,
        type_: Term,
    ) -> Result<bool, ReduceError> {
        let n = tuple.fields.len();

        let cur = match Term::unwrap_or_clone(reduce(context, type_)?) {
            Subterm::TupleType(TupleType { telescope, .. }) if telescope.len() == n => {
                Some(telescope)
            }
            _ => None,
        };

        let projections = (0..n)
            .map(|i| Term::proj(other.clone(), i))
            .collect::<Vec<_>>();
        self.enqueue_fields(tuple.fields, projections, cur);

        Ok(true)
    }

    fn eta_expand_struct(
        &mut self,
        context: &mut Context,
        struct_: Struct,
        other: Term,
        type_: Term,
    ) -> Result<bool, ReduceError> {
        let n = struct_.fields.len();

        // Recover the field types from the registry, exactly as `compare_struct` does — a `Struct` value carries no telescope of its own, unlike a `Tuple`'s inline `TupleType`.
        let cur = match Term::unwrap_or_clone(reduce(context, type_)?) {
            Subterm::StructType(StructType {
                name,
                universes,
                params,
            }) if name == struct_.name => match context.struct_decl(&name).cloned() {
                Some(struct_decl) => Some(
                    instantiate_bound_at(
                        context,
                        &struct_decl.universe_context,
                        &struct_decl.fields,
                        &universes,
                    )?
                    .open_params(&params),
                ),
                None => None,
            },
            _ => None,
        };

        let projections = (0..n)
            .map(|i| Term::proj(other.clone(), i))
            .collect::<Vec<_>>();
        self.enqueue_fields(struct_.fields, projections, cur);

        Ok(true)
    }

    fn eta_expand_neutral(
        &mut self,
        context: &mut Context,
        this: Term,
        that: Term,
        type_: Term,
    ) -> Result<bool, ReduceError> {
        match Term::unwrap_or_clone(reduce(context, type_)?) {
            Subterm::FuncType(FuncType { telescope, .. }) => {
                let n = telescope.len();
                let ys: Vec<Term> = (0..n)
                    .map(|_| Term::free_var(&self.opening(context, None)))
                    .collect();
                let y_refs: Vec<&Term> = ys.iter().collect();
                let output_type = telescope.open(&y_refs);
                self.enqueue(
                    output_type,
                    Term::apply(this, ys.clone()),
                    Term::apply(that, ys),
                );
                Ok(true)
            }
            Subterm::TupleType(TupleType { telescope, .. }) => {
                for i in 0..telescope.len() {
                    self.enqueue(
                        Term::type_ground(),
                        Term::proj(this.clone(), i),
                        Term::proj(that.clone(), i),
                    );
                }
                Ok(true)
            }
            // A nominal struct is a named tuple with no constructor tag (`structure.rs`'s own doc: "an Inductive minus the indices and the per-constructor map"), so it gets the same η treatment — recover the field count from the registry (`compare_struct` recovers the field *types* the same way) since, unlike a `TupleType`, a `StructType` doesn't carry its telescope inline.
            Subterm::StructType(StructType { name, .. }) => {
                let n = context
                    .struct_decl(&name)
                    .map(|struct_decl| struct_decl.fields.len() - struct_decl.params.len());
                let Some(n) = n else {
                    return Ok(false);
                };
                for i in 0..n {
                    self.enqueue(
                        Term::type_ground(),
                        Term::proj(this.clone(), i),
                        Term::proj(that.clone(), i),
                    );
                }
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    /// Solve `?id[spine] ≈ t` (the rigid side, already in weak-head normal form). Implements the pattern fragment: embedded-metavariable guard, occurs check, spine-as-renaming inversion (which subsumes the scope check, and reifies reducer-minted `let` definitions out of the candidate rather than refusing them), and re-validation against the frozen birth context, before committing the solution in birth-named form.
    fn solve(
        &mut self,
        context: &mut Context,
        metavar: &Metavar,
        t: &Term,
    ) -> Result<Solved, ReduceError> {
        let id = metavar.id;
        if context.is_rec_slot(id) {
            return Ok(Solved::Postponed);
        }
        let metavars = t.metavars();

        // Occurs check: a candidate mentioning `id` itself is an infinite solution.
        if metavars.contains(&id) {
            #[cfg(feature = "profile")]
            curios_profile::tracing::debug!(target: "curios_elab::solve", meta = id.0, "failed: occurs check");
            return Ok(Solved::Failed);
        }

        // Embedded-metavariable guard: any *other* unsolved metavariable in the candidate may carry a wider context than `id`'s, so solving now could let the solution escape its scope. Postpone (the stand-in for pruning).
        if metavars
            .iter()
            .any(|other| context.metavar_solution(*other).is_none())
        {
            #[cfg(feature = "profile")]
            curios_profile::tracing::debug!(
                target: "curios_elab::solve",
                meta = id.0,
                blockers = %metavars
                    .iter()
                    .filter(|other| context.metavar_solution(**other).is_none())
                    .map(|other| {
                        let result = context
                            .metavar_entry(*other)
                            .map(|entry| format!("{}", entry.result))
                            .unwrap_or_else(|| "<no birth record>".into());
                        format!("?{} : {result}", other.0)
                    })
                    .collect::<Vec<_>>()
                    .join(", "),
                "postponed: embedded metavariable",
            );
            return Ok(Solved::Postponed);
        }

        let Some(entry) = context.metavar_entry(id) else {
            // No birth record (e.g. a synthesis-position hole that never reached a checking site): nothing to validate against, cannot solve.
            #[cfg(feature = "profile")]
            curios_profile::tracing::debug!(target: "curios_elab::solve", meta = id.0, "failed: no birth record");
            return Ok(Solved::Failed);
        };
        let telescope = entry.telescope.clone();
        let result = entry.result.clone();

        // Every birthed occurrence carries its full spine: `elaborate_apply` opens telescopes with rebuilt arguments, so no lowered bare copy of a birthed hole survives into compared types. (An empty telescope's identity spine is legitimately empty.)
        assert_eq!(
            metavar.spine.len(),
            telescope.len(),
            "metavariable spine arity diverged from its birth telescope"
        );

        // Resolve *solved-metavariable* entries to their values first — a solved entry stands for its (possibly variable) value, and chains terminate because the occurs check forbids solution cycles. Entries are otherwise deliberately unreduced: a name backed by a definition must stay that name, or an obviously-invertible renaming looks flexible.
        let entries = metavar
            .spine
            .iter()
            .map(|term| {
                let mut entry = term.clone();
                while let Subterm::Metavar(m) = &*entry {
                    match context.resolve_metavar(m) {
                        Some(resolved) => entry = resolved,
                        None => break,
                    }
                }
                entry
            })
            .collect::<Vec<_>>();

        // Invert the spine through its *pattern* entries — a syntactic free variable whose name no other entry shares. A non-variable or duplicated entry is simply not invertible; the solution then may not depend on that slot, which the scope check below enforces — pruning in its simplest form.
        let image: Vec<_> = {
            let names = entries
                .iter()
                .map(|term| match &**term {
                    Subterm::Var(var) => var.as_free(),
                    _ => None,
                })
                .collect::<Vec<_>>();

            names
                .iter()
                .zip(telescope.iter())
                .filter_map(|(name, (birth, _))| {
                    let name = (*name)?;
                    // A duplicated image name is ambiguous to invert.
                    let unique = names.iter().filter(|n| **n == Some(name)).count() == 1;
                    unique.then(|| (name.clone(), birth))
                })
                .collect()
        };

        // Non-pattern entries that are meta-free and pairwise distinct become *abstraction subjects*: every occurrence of the entry inside the candidate rewrites to the entry's birth binder, extending inversion beyond the pattern fragment (the practical "abstracting over non-variable terms" move; the choice of all occurrences is checked by the round-trip verification below and by re-validation). An entry embedding a metavariable, or equal to another entry, stays ambiguous, and the candidate may not depend on it. Each subject contributes both its spellings — as written, and as the reducer exposes it at a whnf position (the candidate's root arrives reduced while deep positions do not) — except a reduced form that is a bare variable, which would collide with the renaming machinery.
        let mut subjects = Vec::new();
        for (entry, (birth, _)) in entries.iter().zip(telescope.iter()) {
            if matches!(&**entry, Subterm::Var(_))
                || !entry.metavars().is_empty()
                || entries.iter().filter(|e| *e == entry).count() != 1
            {
                continue;
            }

            subjects.push((entry.clone(), birth.clone()));

            // An entry the type level may not reduce (an effectful scrutinee, say) simply contributes no reduced spelling — only an exhausted budget propagates.
            let reduced = match reduce(context, entry.clone()) {
                Ok(reduced) => reduced,
                Err(ReduceError::Exhausted) => return Err(ReduceError::Exhausted),
                Err(_) => continue,
            };
            let ambiguous = matches!(&*reduced, Subterm::Var(_))
                || entries.contains(&reduced)
                || subjects.iter().any(|(s, _)| *s == reduced);
            if !ambiguous {
                subjects.push((reduced, birth.clone()));
            }
        }

        let mut abstracted = match subjects.is_empty() {
            true => t.clone(),
            false => abstract_occurrences(t, &subjects),
        };

        // Scope check, through the inversion: every free variable of the (abstracted) candidate must correspond to exactly one birth binder. A name that is no entry at all can never become one — out of scope; a name only reachable through a non-pattern or duplicated slot is not provably determined — postpone.
        //
        // One class of free name is context entanglement rather than scope escape: `reduce_let` binds a `let` as an entropy-fresh context *definition* and leaves the name in the reduced spelling, so a candidate that arrived through the reducer can mention a definition no birth binder covers — refusing it would fail equations whose raw spelling is a perfectly scoped solution. Reify such a name by its definition and rescan: the value can only mention earlier definitions (each is released against the prefix that precedes it), so the loop terminates, and a value that embeds the solved metavariable or an unsolved one re-enters the occurs and embedded-metavariable verdicts it would have received in the candidate itself.
        let allowed = image
            .iter()
            .map(|(name, _)| name.clone())
            .chain(subjects.iter().map(|(_, birth)| birth.clone()))
            .collect::<HashSet<_>>();
        loop {
            let mut reify = None;
            for name in abstracted.free_vars() {
                if allowed.contains(&name) {
                    continue;
                }
                // A top-level definition is a global constant, in scope everywhere and absent from Γ's spine by construction (Γ holds only local binders); a solution may mention it freely. This is what lets an item elaborate independently of the ambient prelude.
                if context.is_top_level(&name) {
                    continue;
                }
                if context.definition_body(&name).is_some() {
                    reify = Some(name);
                    break;
                }
                let mentioned = entries.is_empty()
                    || entries
                        .iter()
                        .any(|entry| entry.free_vars().contains(&name));
                #[cfg(feature = "profile")]
                curios_profile::tracing::debug!(
                    target: "curios_elab::solve",
                    meta = id.0,
                    %name,
                    mentioned,
                    "scope check rejected a free name",
                );
                return Ok(match mentioned {
                    true => Solved::Postponed,
                    false => Solved::Failed,
                });
            }
            let Some(name) = reify else {
                break;
            };
            let value = context
                .definition_body(&name)
                .expect("the rescan probe found this definition")
                .clone();
            let value_metavars = value.metavars();
            if value_metavars.contains(&id) {
                return Ok(Solved::Failed);
            }
            if value_metavars
                .iter()
                .any(|other| context.metavar_solution(*other).is_none())
            {
                return Ok(Solved::Postponed);
            }
            abstracted = abstracted.capture(&[&name]).release(&[&value]);
        }

        // Invert, storing the solution in birth-named form. The identity renaming (every invertible entry still its own birth binder, and nothing abstracted) skips the rewrite — from the possibly reified candidate, never the raw one, or the scope loop's substitutions would be discarded here.
        let inverted = if subjects.is_empty() && image.iter().all(|(img, birth)| img == *birth) {
            abstracted.clone()
        } else {
            let binders = image.iter().map(|(name, _)| name).collect::<Vec<_>>();
            let birth_vars = image
                .iter()
                .map(|(_, birth)| Term::free_var(birth))
                .collect::<Vec<_>>();
            let refs = birth_vars.iter().collect::<Vec<_>>();
            abstracted.capture(&binders).release(&refs)
        };

        // The equation must hold by construction: resolving the candidate solution back through this occurrence's spine must reproduce the candidate. This guards the whole abstraction/inversion pair — a missed occurrence or an unfaithful rename postpones instead of committing a wrong solution. Syntactic equality is the fast path; an abstraction that matched a *reduced* spelling resolves back to the raw one, so the fallback criterion is definitional — a strict conversion, which cannot solve anything here since both sides are meta-free past the embedded guard.
        if !metavar.spine.is_empty() {
            let binders = telescope.iter().map(|(name, _)| name).collect::<Vec<_>>();
            let refs = entries.iter().collect::<Vec<_>>();
            let resolved = inverted.capture(&binders).release(&refs);
            if resolved != *t && !convert(context, &Term::type_ground(), &resolved, t)? {
                #[cfg(feature = "profile")]
                curios_profile::tracing::debug!(
                    target: "curios_elab::solve",
                    meta = id.0,
                    "postponed: inversion round-trip disagreed",
                );
                return Ok(Solved::Postponed);
            }
        }

        // Re-validation: the (inverted) candidate must *check* against the metavariable's frozen result type, under its birth context Γ, as an *oracle* — counterfactual refinements and constraint parking both suppressed (see `Context::with_oracle`). Stable definitions are kept. Checking (rather than synthesizing then converting) admits candidates that are checkable but not inferable — a bare lambda whose domain only `result` knows, an unannotated tuple — which are still the correct solution at the frozen type. The validation run itself can solve *other* metavariables (inference may mint and pin fresh implicits); the mark/rollback bracket unwinds those if the candidate is rejected, so a failed oracle leaves no fingerprints.
        let mark = context.solution_mark();
        let revalidated = context.with_frame(|context| {
            for (name, ty) in telescope.iter() {
                context.assume(name, ty);
            }

            context.with_oracle(|context| match check(context, &inverted, result.clone()) {
                Ok(_) => Ok(true),
                // A meta-free, well-scoped candidate that fails to check against the frozen type is not validly typed here — reject the solution. (Under the oracle's suppressed parking an undecided check surfaces as an error too, and likewise rejects.)
                Err(_error) => {
                    // The oracle's verdict is a boolean, so this error is otherwise discarded — and it is exactly what explains a rejected candidate that looks correct at the use site.
                    #[cfg(feature = "profile")]
                    curios_profile::tracing::debug!(
                        target: "curios_elab::solve",
                        meta = id.0,
                        error = %_error,
                        "re-validation rejected the candidate",
                    );
                    Ok(false)
                }
            })
        })?;

        if !revalidated {
            #[cfg(feature = "profile")]
            curios_profile::tracing::debug!(
                target: "curios_elab::solve",
                meta = id.0,
                against = %result,
                "failed: re-validation",
            );
            context.rollback_solutions(mark);
            return Ok(Solved::Failed);
        }

        context.solve_metavar(id, inverted);
        self.progress = true;
        Ok(Solved::Done)
    }

    /// Park a goal from inside a structural arm: remove it from `history` (so a retry after fresh progress is not skipped as already-handled) and push it to `blocked`. Returns `Ok(true)` — a blocked goal is undecided, never a mismatch.
    fn block(&mut self, context: &mut Context, goal: Goal) -> Result<bool, ReduceError> {
        let key = self.history_key(context, &goal);
        self.history.remove(&key);
        self.blocked.push(goal);
        Ok(true)
    }

    /// Flex-apply imitation — the restricted higher-order rule: for `?m(a₁ … aₖ) ≡ T(p̄ ++ ī)` where `T` is a nominal (inductive or struct) type constructor or a unary primitive type former (`Lst`, `Cell`), commit the *imitation* solution `?m := λx₁…xₖ. T(x₁, …, xₖ)` and equate arguments pairwise. This is what unifies `?M(?A) ≡ Option(Nat)` for higher-kinded concepts — the same commitment Lean's first-order approximation and Agda's constructor-headed unification make.
    ///
    /// Deliberate v1 restrictions (cf. the guards in `solve`):
    /// - exact arity only — `ā.len()` must equal `params.len() + indices.len()`; partial application is future work;
    /// - imitation only — the constant (`?m := λ_. T(b̄)`) and projection (`?m := λx. x`) solutions are never produced;
    /// - rigid heads only — nominal constructors and the unary primitive formers (`Lst`, `Cell`), whose argument rides inside the `Prim` node;
    /// - flex–flex stays blocked (dispatched before the structural match);
    /// - a rejected or postponed guess *blocks* the goal, never hard-fails it: refuting the imitation does not prove the equation unsatisfiable (a constant solution could still exist), and blocking preserves the drain's retry semantics — a permanently blocked goal surfaces as a type mismatch at its origin.
    ///
    /// The callers' arms fire for *any* `Apply` against a nominal type; the unsolved-bare-metavariable-head test lives here, and its else branch reproduces the `eta_expand_neutral` fallthrough these pairs took before this rule existed.
    fn imitate_flex_apply(
        &mut self,
        context: &mut Context,
        flex: Apply,
        rigid: Term,
        rigid_raw: &Term,
        goal: Goal,
    ) -> Result<bool, ReduceError> {
        let Some(metavar) = as_metavar(&flex.head).cloned() else {
            return self.eta_expand_neutral(context, goal.this, goal.that, goal.type_);
        };

        type MkBody = Box<dyn Fn(&[Term]) -> Term>;
        let (rigid_args, mk_body): (Vec<Term>, MkBody) = match &*rigid {
            Subterm::InductType(induct_decl) => {
                let name = induct_decl.name.clone();
                let universes = induct_decl.universes.clone();
                let n_params = induct_decl.params.len();
                let args = induct_decl
                    .params
                    .iter()
                    .chain(&induct_decl.indices)
                    .cloned()
                    .collect();
                (
                    args,
                    Box::new(move |vars| {
                        let (params, indices) = vars.split_at(n_params);
                        Term::induct_type_at(
                            name.clone(),
                            universes.clone(),
                            params.iter().cloned(),
                            indices.iter().cloned(),
                        )
                    }),
                )
            }
            // Struct types carry no indices.
            Subterm::StructType(struct_decl) => {
                let name = struct_decl.name.clone();
                let universes = struct_decl.universes.clone();
                (
                    struct_decl.params.clone(),
                    Box::new(move |vars| {
                        Term::struct_type_at(name.clone(), universes.clone(), vars.iter().cloned())
                    }),
                )
            }
            // The unary primitive type formers: their argument rides inside the `Prim` node, so the imitation body rebuilds the node over the binder (`?m := λT. Lst(T)` for `?m(?A) ≡ Lst(Nat)`).
            Subterm::Prim(Prim::LstType(elem)) => (
                vec![elem.clone()],
                Box::new(|vars| Term::prim(Prim::LstType(vars[0].clone()))),
            ),
            Subterm::Prim(Prim::CellType(elem)) => (
                vec![elem.clone()],
                Box::new(|vars| Term::prim(Prim::CellType(vars[0].clone()))),
            ),
            _ => unreachable!("the callers pass a nominal or prim-former rigid side"),
        };
        let arity = rigid_args.len();

        if flex.params.len() != arity {
            return self.block(context, goal);
        }

        // The candidate's binders come from `?m`'s frozen birth type, which must itself reduce to a function type of the same arity. Peeling opens each binder as a free variable of its own label (elaborated labels are entropy-fresh), so dependent domains stay well-scoped.
        let Some(entry) = context.metavar_entry(metavar.id) else {
            return self.block(context, goal);
        };
        let result = reduce(context, entry.result.clone())?;
        let Subterm::FuncType(func_type) = &*result else {
            return self.block(context, goal);
        };
        if func_type.plicities.len() != arity {
            return self.block(context, goal);
        }

        // The candidate copies `?m`'s birth function type's plicities so the imitation is convertible with that type (plicity is part of function identity — see `compare_func`).
        let mut domains: Vec<(Plicity, Free, Term)> = Vec::with_capacity(arity);
        let mut telescope = func_type.telescope.clone();
        for plicity in func_type.plicities.iter().copied() {
            let Telescope::Cons(ty, rest) = telescope else {
                unreachable!("plicities parallel the telescope");
            };
            let binder = context.fresh(rest.first_hint());
            telescope = rest.open(&[&Term::free_var(&binder)]);
            domains.push((plicity, binder, ty.clone()));
        }

        // The candidate body mirrors the rigid node's shape (for an inductive, its params/indices split — `elaborate_induct_type` re-checks the node against the full telescope during re-validation and rejects a wrong split).
        let binder_vars = domains
            .iter()
            .map(|(_, binder, _)| Term::free_var(binder))
            .collect::<Vec<_>>();
        let body = mk_body(&binder_vars);
        let candidate = Term::func_marked(domains.clone(), body);

        // The invariant `solve_refinement_free` protects, upheld here by hand (the committed solution is the built candidate, not the rigid side itself): a solution must not be derived from a spelling that holds only under counterfactual match-arm refinements. When refinements are live and the unrefined spelling differs, the nominal shape may be the refinement's doing — postpone rather than commit.
        if context.has_refinements() {
            let suppressed = context
                .with_suppressed_refinements(|context| reduce(context, rigid_raw.clone()))?;
            if suppressed != rigid {
                return self.block(context, goal);
            }
        }

        // `solve` supplies the occurs check, the embedded-metavariable guard, the spine inversion and scope check (against `?m`'s own birth spine), and re-validation: the candidate is `check`ed against the frozen birth type under the birth context, which is what rejects an ill-kinded imitation before it commits.
        match self.solve(context, &metavar, &candidate)? {
            Solved::Done => {}
            Solved::Postponed | Solved::Failed => return self.block(context, goal),
        }

        // Equate arguments pairwise, at the candidate telescope's domains (mirroring `compare_apply`'s recovered argument types).
        for ((flex_arg, rigid_arg), (_, _, domain)) in
            flex.params.into_iter().zip(rigid_args).zip(domains)
        {
            self.enqueue(domain, flex_arg, rigid_arg);
        }

        Ok(true)
    }

    /// Solve flex–rigid with a *refinement-free* candidate. The drain reduces goals under the live frame, where counterfactual match-arm refinements apply — sound for discharging a goal, but not for committing a solution: a metavariable must not be pinned to a value that holds only counterfactually inside an arm (`?k := 0` because the nil arm refined `n := 0`). When refinements are in scope, re-reduce the original rigid term with them suppressed and solve against that spelling; refinements only ever *add* reductions, so a solution found this way still discharges the refined goal. A goal whose verdict changes under suppression is the refinement's doing — nothing is globally forced, so it postpones rather than failing or committing.
    fn solve_refinement_free(
        &mut self,
        context: &mut Context,
        metavar: &Metavar,
        rigid: &Term,
        rigid_raw: &Term,
    ) -> Result<Solved, ReduceError> {
        if !context.has_refinements() {
            return self.solve(context, metavar, rigid);
        }

        let suppressed =
            context.with_suppressed_refinements(|context| reduce(context, rigid_raw.clone()))?;

        // Refinements made no difference for this term: the unguarded path, hard verdicts included.
        if suppressed == *rigid {
            return self.solve(context, metavar, rigid);
        }

        // The unrefined spelling is itself flexible — only the refinement made the side look rigid. Undecided.
        if as_metavar(&suppressed).is_some() {
            return Ok(Solved::Postponed);
        }

        Ok(match self.solve(context, metavar, &suppressed)? {
            Solved::Done => Solved::Done,
            // A verdict the refinement-free spelling cannot reach (out of scope, ill-typed at the birth context) is not a hard failure of the goal — the refined spelling may still discharge it once the metavariable is pinned elsewhere.
            Solved::Postponed | Solved::Failed => Solved::Postponed,
        })
    }

    fn outcome(&mut self, context: &mut Context) -> Result<Outcome, ReduceError> {
        loop {
            if !self.drain(context)? {
                return Ok(Outcome::Mismatch);
            }

            // Fixpoint: retry postponed constraints only when a fresh solution since the last sweep could have unblocked them.
            if self.progress && !self.blocked.is_empty() {
                self.pending = std::mem::take(&mut self.blocked).into();
                self.progress = false;
            } else {
                break;
            }
        }

        // A constraint still blocked at quiescence is undecided, not unequal: surrender it to the caller rather than conflating the two.
        Ok(match self.blocked.is_empty() {
            true => Outcome::Converts,
            false => Outcome::Blocked(std::mem::take(&mut self.blocked)),
        })
    }

    /// Drain `pending` once. Returns `Ok(false)` on a hard mismatch; `Ok(true)` when the queue empties (possibly leaving `blocked` constraints).
    fn drain(&mut self, context: &mut Context) -> Result<bool, ReduceError> {
        while let Some(Goal { type_, this, that }) = self.dequeue(context)? {
            // Reflexivity needs no evaluation. In particular, do not force an identical folded recursive computation merely because it sits under a strict primitive operation.
            if this == that {
                continue;
            }

            // Universe instantiations are computationally irrelevant: Curios has no universe reflection, and erasure removes every instance, nominal level vector, and `Type` payload. For values (goals not checked at `Type` itself), identical erased spellings are definitionally equal without unfolding their shared head. This also prevents independently fresh instances of a partial definition from being evaluated merely to rediscover the same value. Type expressions still take the structural path below, where their level vectors generate equality constraints.
            if (this.has_universe_data() || that.has_universe_data())
                && project_erased_universes(&this) == project_erased_universes(&that)
                && !matches!(&*reduce(context, type_.clone())?, Subterm::Type(_))
            {
                continue;
            }

            // The unreduced spellings, kept for the flex–rigid case: the reductions below apply counterfactual match-arm refinements, and a candidate *solution* must be derived without them (see `solve_refinement_free`).
            let this_raw = this.clone();
            let that_raw = that.clone();

            let this = reduce(context, this)?;
            let that = reduce(context, that)?;
            let type_ = reduce(context, type_)?;

            if this == that {
                continue;
            }

            // Flexible heads are dispatched before history and before the structural/η fallthrough — a flexible head must never be η-expanded into a spine.
            match (as_metavar(&this).cloned(), as_metavar(&that).cloned()) {
                (Some(this_m), Some(that_m)) => {
                    // Same head, different spines (node duplication under different openings): entrywise spine agreement is a *sufficient* congruence condition. Probed only when both spines are meta-free — a probe that could solve metavariables would overcommit, since agreement is not necessary for the goal to hold.
                    if this_m.id == that_m.id
                        && this_m
                            .spine
                            .iter()
                            .chain(that_m.spine.iter())
                            .all(|entry| entry.metavars().is_empty())
                    {
                        let mut entrywise = true;
                        for (a, b) in this_m.spine.iter().zip(that_m.spine.iter()) {
                            if !convert(context, &Term::type_ground(), a, b)? {
                                entrywise = false;
                                break;
                            }
                        }
                        if entrywise {
                            continue;
                        }
                    }

                    if context.is_rec_slot(this_m.id) || context.is_rec_slot(that_m.id) {
                        self.blocked.push(Goal { type_, this, that });
                        continue;
                    }

                    // Distinct heads (or an undecided probe): v1 flex–flex does no intersection — postpone.
                    self.blocked.push(Goal { type_, this, that });
                    continue;
                }
                (Some(metavar), None) => {
                    if context.is_rec_slot(metavar.id) {
                        self.blocked.push(Goal { type_, this, that });
                        continue;
                    }
                    match self.solve_refinement_free(context, &metavar, &that, &that_raw)? {
                        Solved::Done => continue,
                        Solved::Postponed => {
                            self.blocked.push(Goal { type_, this, that });
                            continue;
                        }
                        Solved::Failed => return Ok(false),
                    }
                }
                (None, Some(metavar)) => {
                    if context.is_rec_slot(metavar.id) {
                        self.blocked.push(Goal { type_, this, that });
                        continue;
                    }
                    match self.solve_refinement_free(context, &metavar, &this, &this_raw)? {
                        Solved::Done => continue,
                        Solved::Postponed => {
                            self.blocked.push(Goal { type_, this, that });
                            continue;
                        }
                        Solved::Failed => return Ok(false),
                    }
                }
                (None, None) => {}
            }

            // Definitional proof irrelevance: any two inhabitants of a strict proposition are convertible. Placed after the metavar dispatch so a flexible side is still solved against the other (a metavar is not left dangling merely because its type is a proposition).
            if let Sort::Prop = Sort::of(context, &type_)? {
                continue;
            }

            let goal = Goal {
                type_: type_.clone(),
                this: this.clone(),
                that: that.clone(),
            };

            let key = self.history_key(context, &goal);
            if self.in_history(&key) {
                continue;
            }

            let ok = match (Term::unwrap_or_clone(this), Term::unwrap_or_clone(that)) {
                (Subterm::Prim(this), Subterm::Prim(that)) => convert_prim(self, this, that)?,
                (Subterm::Type(this), Subterm::Type(that)) => {
                    context
                        .universes_mut()
                        .add_eq(
                            this,
                            that,
                            UniverseConstraintOrigin::new(UniverseConstraintKind::Conversion),
                        )
                        .map_err(ReduceError::Universe)?;
                    true
                }
                (Subterm::UniverseInst(this), Subterm::UniverseInst(that)) => {
                    if !Self::compare_levels(context, &this.levels, &that.levels)? {
                        false
                    } else {
                        self.enqueue(type_, this.head, that.head);
                        true
                    }
                }
                // Same-kind matches compare structurally; cross-kind pairs fall through to `eta_expand_neutral` (e.g. proof irrelevance at unit).
                (Subterm::Match(this), Subterm::Match(that))
                    if std::mem::discriminant(&this.cases)
                        == std::mem::discriminant(&that.cases) =>
                {
                    self.compare_match(context, this, that)?
                }
                (Subterm::FuncType(this), Subterm::FuncType(that)) => {
                    self.compare_func_type(context, this, that)?
                }
                (Subterm::Func(this), Subterm::Func(that)) => {
                    self.compare_func(context, this, that, type_.clone())?
                }
                (Subterm::Func(func), other) => {
                    self.eta_expand_func(context, func, other.into(), type_.clone())?
                }
                (other, Subterm::Func(func)) => {
                    self.eta_expand_func(context, func, other.into(), type_.clone())?
                }
                // Both sides already reached `reduce`'s weak-head normal form before this dispatch (see `drain`), so a surviving `Apply` is genuinely stuck — same head or not, there is nothing further unfolding could reveal; `compare_apply` handles both uniformly (enqueuing the heads themselves when they differ, which recurs to a hard mismatch or resolves through whatever solves the head). The canonicalized `history` (see `history_key`) still recognizes a coinductive recurrence across repeated rounds of an unfolding cycle; a genuinely growing comparison spends the budget instead.
                (Subterm::Apply(this_a), Subterm::Apply(that_a)) => {
                    match (&*this_a.head, &*that_a.head) {
                        (Subterm::RecMember(this), Subterm::RecMember(that)) if this == that => {
                            self.compare_same_rec_apply(context, this_a, that_a, type_.clone())?
                        }
                        // Applications of *different* folded members (or a folded member against another stuck head) may still compute the same value, so congruence proves nothing: take one symmetric delta step instead.
                        (Subterm::RecMember(_), _) | (_, Subterm::RecMember(_)) => {
                            let this_unfolded = unfold_rec_apply(context, this_a.clone())?;
                            let that_unfolded = unfold_rec_apply(context, that_a.clone())?;
                            match (this_unfolded, that_unfolded) {
                                (None, None) => self.compare_apply(context, this_a, that_a)?,
                                (this_unfolded, that_unfolded) => {
                                    let this = this_unfolded
                                        .unwrap_or_else(|| Subterm::Apply(this_a).into());
                                    let that = that_unfolded
                                        .unwrap_or_else(|| Subterm::Apply(that_a).into());
                                    self.enqueue(type_.clone(), this, that);
                                    true
                                }
                            }
                        }
                        _ => self.compare_apply(context, this_a, that_a)?,
                    }
                }
                (Subterm::Apply(apply), other) if matches!(&*apply.head, Subterm::RecMember(_)) => {
                    match unfold_rec_apply(context, apply)? {
                        Some(unfolded) => {
                            self.enqueue(type_, unfolded, other.into());
                            true
                        }
                        None => false,
                    }
                }
                (other, Subterm::Apply(apply)) if matches!(&*apply.head, Subterm::RecMember(_)) => {
                    match unfold_rec_apply(context, apply)? {
                        Some(unfolded) => {
                            self.enqueue(type_, other.into(), unfolded);
                            true
                        }
                        None => false,
                    }
                }
                (Subterm::TupleType(this), Subterm::TupleType(that)) => {
                    self.compare_tuple_type(context, this, that)?
                }
                (Subterm::Tuple(this), Subterm::Tuple(that)) => {
                    self.compare_tuple(context, this, that, type_.clone())?
                }
                (Subterm::Tuple(tuple), other) => {
                    self.eta_expand_tuple(context, tuple, other.into(), type_.clone())?
                }
                (other, Subterm::Tuple(tuple)) => {
                    self.eta_expand_tuple(context, tuple, other.into(), type_.clone())?
                }
                (Subterm::Proj(this), Subterm::Proj(that)) => self.compare_proj(this, that)?,
                (Subterm::InductType(this), Subterm::InductType(that)) => {
                    self.compare_induct_type(context, this, that)?
                }
                (Subterm::Variant(this), Subterm::Variant(that)) => {
                    self.compare_variant(context, this, that)?
                }
                (Subterm::StructType(this), Subterm::StructType(that)) => {
                    self.compare_struct_type(context, this, that)?
                }
                (Subterm::Struct(this), Subterm::Struct(that)) => {
                    self.compare_struct(context, this, that)?
                }
                (Subterm::Struct(struct_), other) => {
                    self.eta_expand_struct(context, struct_, other.into(), type_.clone())?
                }
                (other, Subterm::Struct(struct_)) => {
                    self.eta_expand_struct(context, struct_, other.into(), type_.clone())?
                }
                (Subterm::Rec(this), Subterm::Rec(that)) => {
                    self.compare_rec(context, this, that)?
                }
                (Subterm::RecMember(this), Subterm::RecMember(that)) => {
                    if this.index != that.index {
                        self.enqueue(
                            type_,
                            this.group.member_body(this.index),
                            that.group.member_body(that.index),
                        );
                        true
                    } else {
                        self.compare_rec(
                            context,
                            Rec {
                                tail: Scope::constant(
                                    Many(this.group.length()),
                                    Term::var(Var::bound(this.index)),
                                ),
                                group: this.group,
                            },
                            Rec {
                                tail: Scope::constant(
                                    Many(that.group.length()),
                                    Term::var(Var::bound(that.index)),
                                ),
                                group: that.group,
                            },
                        )?
                    }
                }
                (Subterm::RecMember(member), other) => {
                    self.enqueue(type_, member.group.member_body(member.index), other.into());
                    true
                }
                (other, Subterm::RecMember(member)) => {
                    self.enqueue(type_, other.into(), member.group.member_body(member.index));
                    true
                }
                (Subterm::Rec(rec), other) => {
                    let tail = unfold_rec(context, rec);
                    self.enqueue(type_, tail, other.into());
                    true
                }
                (other, Subterm::Rec(rec)) => {
                    let tail = unfold_rec(context, rec);
                    self.enqueue(type_, other.into(), tail);
                    true
                }
                // Flex-apply imitation: a stuck application against a nominal type constructor. The unsolved-metavariable-head guard (and the `eta_expand_neutral` fallthrough for every other `Apply`) lives inside `imitate_flex_apply`.
                (Subterm::Apply(apply), Subterm::InductType(rigid)) => {
                    let rigid: Term = Subterm::InductType(rigid).into();
                    self.imitate_flex_apply(context, apply, rigid, &that_raw, goal.clone())?
                }
                (Subterm::InductType(rigid), Subterm::Apply(apply)) => {
                    let rigid: Term = Subterm::InductType(rigid).into();
                    self.imitate_flex_apply(context, apply, rigid, &this_raw, goal.clone())?
                }
                (Subterm::Apply(apply), Subterm::StructType(rigid)) => {
                    let rigid: Term = Subterm::StructType(rigid).into();
                    self.imitate_flex_apply(context, apply, rigid, &that_raw, goal.clone())?
                }
                (Subterm::StructType(rigid), Subterm::Apply(apply)) => {
                    let rigid: Term = Subterm::StructType(rigid).into();
                    self.imitate_flex_apply(context, apply, rigid, &this_raw, goal.clone())?
                }
                (
                    Subterm::Apply(apply),
                    Subterm::Prim(rigid @ (Prim::LstType(_) | Prim::CellType(_))),
                ) => {
                    let rigid: Term = Subterm::Prim(rigid).into();
                    self.imitate_flex_apply(context, apply, rigid, &that_raw, goal.clone())?
                }
                (
                    Subterm::Prim(rigid @ (Prim::LstType(_) | Prim::CellType(_))),
                    Subterm::Apply(apply),
                ) => {
                    let rigid: Term = Subterm::Prim(rigid).into();
                    self.imitate_flex_apply(context, apply, rigid, &this_raw, goal.clone())?
                }
                (this_n, that_n) => {
                    self.eta_expand_neutral(context, this_n.into(), that_n.into(), type_)?
                }
            };

            if !ok {
                // A structural mismatch where a side is a primitive stuck on an unsolved metavariable is undecided, not provably unequal: solving the metavariable may fold the operation (`?m - 1` against `0` folds once `?m := 1` lands), which no structural rule anticipates. Park the goal instead of failing — rigid-head disagreements, which no solution can repair, still mismatch here. The goal leaves `history` so a retry after fresh progress is not skipped as already-handled.
                if prim_blocked_on_metavar(context, &goal.this)
                    || prim_blocked_on_metavar(context, &goal.that)
                {
                    let key = self.history_key(context, &goal);
                    self.history.remove(&key);
                    self.blocked.push(goal);
                    continue;
                }
                return Ok(false);
            }
        }

        Ok(true)
    }
}

/// Replace every occurrence of a subject term in `t` — matched by the same term equality conversion uses, at any depth (binder names are entropy-fresh, so a free-named subject cannot be captured by an inner scope) — with its birth binder's name. Top-down: an outer match wins and is not descended into. Subjects are pairwise distinct by construction, so the match is unambiguous.
fn abstract_occurrences(t: &Term, subjects: &[(Term, Free)]) -> Term {
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
fn as_metavar(term: &Term) -> Option<&Metavar> {
    match &**term {
        Subterm::Metavar(metavar) => Some(metavar),
        _ => None,
    }
}

/// `true` iff `term` — already in weak-head normal form, so a foldable literal would have folded — is a primitive operation still carrying an unsolved metavariable: the stuck-on-a-metavariable shape whose structural mismatches are undecided rather than definite.
fn prim_blocked_on_metavar(context: &Context, term: &Term) -> bool {
    matches!(&**term, Subterm::Prim(_))
        && term
            .metavars()
            .iter()
            .any(|id| context.metavar_solution(*id).is_none())
}
