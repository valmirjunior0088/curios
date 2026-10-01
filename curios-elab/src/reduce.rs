#[cfg(test)]
mod intrinsic_tests;
#[cfg(test)]
mod kernel_agreement_tests;
#[cfg(test)]
mod partial_arithmetic_tests;
#[cfg(test)]
mod reduction_tests;
#[cfg(test)]
pub(crate) mod test_support;

use {
    super::{Context, Settled, levels_clash_on_a_decided_instance, zonk_solved_term_metas},
    curios_analysis::could_reduce_to,
    curios_core::{
        Advance, Apply, Argument, Bound, Carrier, Cases, ClosedHost, Cost, Demand, Field, Free,
        FreeMonoid, Func, FuncType, Global, HeadTag, InductDecl, InductType, Instance,
        InstanceHead, Intrinsic, Layer, Let, Match, MatchResult, Metavar, Nat, Probe, Proj, Rec,
        RecGroup, ReduceError, Reducer, Struct, StructDecl, StructType, Subterm, Telescope, Term,
        Tuple, TupleType, Var, Variant, Visit, accelerable, instantiate_universe_levels_scoped,
        probe_spellings, project_erased_universes, reduce_closed, reduce_intrinsic,
    },
    curios_utilities::recurse,
};

/// The elaborator's side of the closed-machine seam: the same delta `reduce_var` and `reduce_instance` perform, handed to the shared machine so a closed term evaluates at machine depth under this strategy's own charges.
impl ClosedHost for Context {
    fn closed_body(&self, name: &Free) -> Option<&Term> {
        self.var_reduct(name)
    }

    fn closed_body_at(&self, name: &Free) -> Option<&Term> {
        self.var_reduct_at(name)
    }
}

/// Whether the closed machine may take `term` in this context: the representation-side gate ([`accelerable`]) plus the judgment-side one — no refinement of any kind registered, suppressed or live, because a refined closed scrutinee *is* the arm's assumed value, a refined projection is its recorded reduct, and a *suppressed* key must stay withheld rather than be evaluated.
fn machine_admissible(context: &Context, term: &Term) -> bool {
    accelerable(term) && !context.any_refinements_registered()
}

/// The elaborator's reduction strategy, supplied to `curios-core`'s intrinsic folds. It is the full-strength one: definitions unfold, metavariables resolve, scrutinee refinements fire, and every step is charged against the declaration's budget.
impl Reducer for Context {
    fn reduce(&mut self, term: Term) -> Result<Term, ReduceError> {
        reduce(self, term)
    }

    fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError> {
        reduce_forced(self, term)
    }

    fn spend(&mut self, cost: Cost) -> Result<(), ReduceError> {
        Context::spend(self, cost)
    }

    fn fresh_binder(&mut self, hint: Option<&str>) -> Free {
        self.fresh(hint)
    }
}

/// The elaborator's side of the shared-analysis seam.
///
/// Both methods are `typing`'s wrappers: they exist so a failure reaches the user as a spanned diagnostic naming the offending term rather than as a bare `ReduceError`, which is precisely the split [`Env::Error`](curios_analysis::Env::Error) formalizes.
impl curios_analysis::Env for Context {
    type Error = crate::Error;

    fn force(&mut self, term: &Term) -> Result<Term, Self::Error> {
        crate::reduce_with(self, term)
    }

    fn assumption(&self, name: &Free) -> Option<&Term> {
        Context::assumption(self, name)
    }

    /// A local binder in scope, whether assumed or kept as a `let` definition. `Context::assumption` answers for top-level names too, which the kernel's locals never include.
    fn is_local(&self, name: &Free) -> bool {
        name.is_local()
            && (Context::assumption(self, name).is_some() || self.definition_body(name).is_some())
    }

    fn fresh(&mut self, hint: Option<&str>) -> Free {
        Context::fresh(self, hint)
    }

    fn unfold(&self, name: &Free) -> Option<&Term> {
        self.definition_body(name)
    }

    fn induct_decl(&self, name: &Global) -> Option<&InductDecl> {
        Context::induct_decl(self, name)
    }

    fn struct_decl(&self, name: &Global) -> Option<&StructDecl> {
        Context::struct_decl(self, name)
    }
}

impl curios_analysis::Judge for Context {
    fn convert_at(&mut self, type_: &Term, this: &Term, that: &Term) -> Result<bool, Self::Error> {
        crate::convert_at(self, type_, this, that)
    }
}

enum Reduce {
    Continue(Term),
    Break(Term),
}

/// Open a `rec` group's tail over structural folded member terms. This is a pure binder operation: it neither mints names nor mutates the context.
pub(crate) fn unfold_rec(context: &mut Context, rec: Rec) -> Result<Term, ReduceError> {
    let members = rec.group.members();
    // The member vector, the ref vector over it, and the opening — one substitution pass per member.
    context.spend(
        Cost::collection(members.len() as u64)
            .saturating_mul(2)
            .saturating_add(Cost::term(1).saturating_mul(members.len() as u64)),
    )?;

    let refs = members.iter().collect::<Vec<_>>();

    Ok(rec.tail.open(&refs))
}

/// Expose only `Rec` binding syntax, without unfolding a selected member's fixed point. This strips down to the projection `rec f = ...; f` denotes, preserving the folded recursive call as the canonical neutral.
fn expose_rec_tail(context: &mut Context, mut term: Term) -> Result<Term, ReduceError> {
    loop {
        // A projection is the fixed point of this unfolding: opening its tail over the group yields the same term, so stripping stops here.
        if term.as_rec_proj().is_some() {
            return Ok(term);
        }

        match Term::unwrap_or_clone(term) {
            Subterm::Rec(rec) => {
                let tail = unfold_rec(context, rec)?;
                term = reduce(context, tail)?;
            }
            other => return Ok(other.into()),
        }
    }
}

/// Delta-unfold one folded recursive application. Ordinary reduction does not call this: eliminators and conversion use it only when the folded call's result shape is demanded or must be compared with a differently-shaped term.
pub(crate) fn unfold_rec_apply(
    context: &mut Context,
    apply: Apply,
) -> Result<Option<Term>, ReduceError> {
    let Apply { head, arguments } = apply;
    let head = reduce(context, head)?;
    let head = expose_rec_tail(context, head)?;

    // A projection is the shape a *recursive* member keeps: opening the group's tail over its own members reproduces it, which is where `expose_rec_tail` stops. A member that does not occur in its own body has no fixed point to keep, so the same opening reduces past the projection to the member's value, and the applicable term is then the exposed head itself. Both are the one beta step this function exists to take, and taking only the first would leave the other spelling folded with its answer in hand — an `induct`'s type constructor lowers into a `rec` whatever its arity, so a caller reaching the unfolded spelling would see a nominal type and one reaching the folded spelling a stuck application, and a checker reading the two would disagree about the same declaration.
    let body = match head.as_rec_proj() {
        Some((group, index)) => {
            let body = reduce(context, group.member_body(index))?;

            force_rec(context, body)?
        }
        None => head,
    };
    let telescope = match Term::unwrap_or_clone(body) {
        Subterm::Func(Func { telescope, .. }) => telescope,
        // The head is itself a folded call: a member whose result is a function, applied past its own parameters. Unfolding that call one step and applying what it becomes to what is left is this application's one step, which is what lets a demand reach through `f(a)(b)` to `f(a)` — read one level deep, the outer application would be a neutral nothing unfolds.
        Subterm::Apply(inner) => {
            return Ok(unfold_rec_apply(context, inner)?.map(|unfolded| {
                Subterm::Apply(Apply {
                    head: unfolded,
                    arguments,
                })
                .into()
            }));
        }
        _ => return Ok(None),
    };
    context.spend(
        Cost::collection(arguments.len() as u64)
            .saturating_add(Cost::term(1).saturating_mul(arguments.len() as u64)),
    )?;
    let param_refs = arguments
        .iter()
        .map(|argument| &argument.term)
        .collect::<Vec<_>>();

    Ok(Some(telescope.open(&param_refs)))
}

/// A folded recursive spelling: a `rec` projection, a `rec` block, or an application spine headed by a projection — the one weak-head value a forced demand must not be served.
fn is_folded(term: &Term) -> bool {
    term.spine_rec_proj().is_some() || matches!(&**term, Subterm::Rec(_))
}

/// Force a `rec` group in WHNF position. The main loop treats a `Rec` node as a normal form, so an eliminator that demands its value unfolds it here and re-reduces, repeating if the opened tail is itself a `rec`. A non-productive group spins until the step budget runs out — exactly as a top-level `rec` does.
///
/// The force either reaches a value some eliminator can absorb or returns the input unchanged. What it keeps is decided by whether the unfolding got anywhere, and there are two ways for it to have done so: a **head constructor** is progress by productivity, and a reduct **free of the group** is progress by termination. Only a reduct that is still neutral *and* still mentions the group is an unfolding that achieved nothing — the restuck case — and there the folded spelling stays the canonical normal form.
///
/// Testing the head alone would conflate *neutral because stuck* with *neutral because that is the answer*: `go(0, acc)` reduces correctly to `acc`, and throwing that bare `Var` away would make the base case of any lemma about an accumulator unprovable in decided form. Testing occurrence alone would conflate *restuck* with *productive* and discard `cons(x, go(k, …))`. Each half is load-bearing over the fixed prelude, where thousands of decisions reach each arm.
///
/// The group is `folded`'s own, deliberately: what this protects is the idempotence of forcing *this* term, so the cycle to rule out is the reduct re-mentioning the group whose call was demanded. All three outcomes are idempotent — `force(force(t)) = force(t)` — so what this clause decides is completeness, not whether the reducer stops; the budget spent per iteration already does that.
fn force_rec(context: &mut Context, term: Term) -> Result<Term, ReduceError> {
    // A closed term takes the machine at the eliminator's demand; the recursive loop below is the strategy for everything the gate declines.
    //
    // The forced value is stored in the declaration's reduction cache unless it is a folded recursive spelling — a `reduce` probe must never be served a fold it expects to keep folded, but any other forced value is a weak-head form like any cached reduct. Without this store the elaborator would re-run the machine for every position that demands the same closed value — checking, conversion and re-validation each paying a `Str` literal's full scan while the kernel replays its memo.
    if machine_admissible(context, &term) {
        // The store below is what a later demand for the same value hits, and this is where it hits: a probe that finds an unfolded value answers without a run, while one that finds the folded spelling — a plain reduct stored under itself — has nothing to serve and runs.
        if let Some(cached) = context.cached_reduced(&term)
            && !is_folded(&cached)
        {
            return Ok(cached);
        }
        let entry = term.clone();
        let result = reduce_closed(context, term, Demand::Forced)?;
        let folded = is_folded(&result);
        if !folded {
            context.reduce(entry, &result);
        }

        return Ok(result);
    }

    let folded = term.clone();
    let mut term = term;
    loop {
        context.spend(Cost::STEP)?;

        // Unfolding a projection means stepping to the member's body; unfolding any other `rec` means opening its tail.
        if let Some((group, index)) = term.as_rec_proj() {
            let body = group.member_body(index);
            term = reduce(context, body)?;
            continue;
        }

        match Term::unwrap_or_clone(term) {
            Subterm::Rec(rec) => {
                let tail = unfold_rec(context, rec)?;
                term = reduce(context, tail)?;
            }
            Subterm::Apply(apply) => match unfold_rec_apply(context, apply)? {
                Some(unfolded) => term = reduce(context, unfolded)?,
                None => return Ok(folded),
            },
            other => {
                let neutral = matches!(
                    other,
                    Subterm::Match(_) | Subterm::Var(_) | Subterm::Metavar(_) | Subterm::Proj(_)
                );
                let reduct: Term = other.into();

                // A head was exposed, so the unfolding produced something an eliminator can absorb.
                if !neutral {
                    return Ok(reduct);
                }

                // Neutral: keep it only if the group is gone from it, and fall back to the folded spelling otherwise.
                return Ok(match demanded_group(context, &folded)? {
                    Some(group) if reduct.mentions_rec_member(&group) => folded,
                    _ => reduct,
                });
            }
        }
    }
}

/// The group whose call `folded` is, for [`force_rec`]'s occurrence test.
///
/// A projection and a bare `rec` value carry it directly; an application carries it at the head of its spine, which is where `unfold_rec_apply` already looked — and looking again is a reduction-cache hit rather than a second traversal. `None` for a term that is not a recursive call at all, where nothing can restick and the reduct is kept.
fn demanded_group(context: &mut Context, folded: &Term) -> Result<Option<RecGroup>, ReduceError> {
    if let Some((group, _)) = folded.as_rec_proj() {
        return Ok(Some(group.clone()));
    }

    match &**folded {
        Subterm::Rec(rec) => Ok(Some(rec.group.clone())),
        Subterm::Apply(Apply { head, .. }) => {
            let head = reduce(context, head.clone())?;
            let head = expose_rec_tail(context, head)?;

            Ok(head.spine_rec_proj().map(|(group, _)| group.clone()))
        }
        _ => Ok(None),
    }
}

/// Reduce to WHNF and then force a `rec` head: used wherever an eliminator (`match`/application/projection) demands a value, so an inner `rec` reduces just like a top-level one instead of staying stuck.
pub(crate) fn reduce_forced(context: &mut Context, term: Term) -> Result<Term, ReduceError> {
    let reduced = reduce(context, term)?;
    force_rec(context, reduced)
}

/// The *cheap* refinement key: metavariable solutions materialized and universe instances erased, with every argument left exactly as written.
///
/// [`canonical_scrutinee`] additionally reduces each argument, which is what collapses occurrences differing only in argument spelling — and what makes *recording* a refinement cost whatever its operands cost to evaluate. A guard over an expensive operand then pays for the very computation it was written to avoid, when the arm is entered and before any probe happens; `10 <= Bytes/len(built)` forces `built` to register a fact about it. Both sites therefore key on this form first and escalate to the canonical one only on a miss, which is a strict superset: every occurrence the canonical key matches still matches, and one spelled as the guard was matches without reducing anything.
///
/// Zonking and universe erasure stay eager because they are cheap by construction — the walk returns at a cached `has_metavar` bit — and because the key is wrong without them for the reason each states.
///
/// **The universe erasure is deliberate, and the exactness it gives up is recovered at the read rather than here.** `Type u` embeds a level in a term, so two instances of one applied definition can reduce to different values and erasing identifies them — which is why `curios-cert`'s key keeps its universes (`Scope::refine`, with `recheck::universes_tests::a_case_equation_does_not_refine_an_occurrence_at_another_universe_instance` as the fixture). Keeping them *here* would not be the repair: every polymorphic occurrence mints fresh universe metavariables (`UniverseSolver::instantiate`) and this walk materializes *term* metas only, so a verbatim key would split two occurrences of one scrutinee and the prelude would stop elaborating at `/std/List.crs`'s `match i < len(a)`, whose `true` arm supplies `/sys/List/get`'s implicit `ok` by exactly this refinement.
///
/// The asymmetry with the kernel is *when*, not what. The rule is not in dispute — identify only terms already definitionally equal — and the kernel states it exactly because it judges a module whose levels are settled. This key is computed while they are still being solved: an arm records it on entry and it is then probed for as long as the arm stands, with metavariables solved in between. So "concrete levels kept apart, undecided ones collapsed" cannot be a property of a key at all; it is a property of a *comparison*, and the only step that happens after solving is the read. `Context::scrutinee_reduct` and `Context::proj_reduct` make it there, by declining a hit whose two unerased spellings disagree on an instance both sides have decided — the refusing direction, so a coarse key costs reductions and never admits one.
pub(crate) fn shallow_scrutinee(context: &Context, term: &Term) -> Term {
    project_erased_universes(&zonk_solved_term_metas(context, term))
}

/// The canonical form of a (potential) scrutinee refinement key: the head kept verbatim — so the refined function (`classify`, `Nat/in_range`) is *not* unfolded and stays the key — with each argument reduced to WHNF. Probing through one canonicalizer makes occurrences that differ only in argument spelling (`c` vs `Bin/at(cons(c,t),0,_)`, `lo` vs a projection that reduces to it) collapse to the same key. A non-application is its own canonical form.
///
/// Reached from the escalation path alone, never from a store: [`shallow_scrutinee`] is what a key is recorded under, and this is what decides a probe the recorded spelling missed.
///
/// Argument reduction is a [`Probe`]: an argument that cannot reduce at the type level (a runtime-only IO intrinsic's result, such as `/sys/Handle/poll`'s, or an out-of-range access) is kept verbatim rather than forced. Such an argument was never going to differ in spelling — the only occurrence is the scrutinee itself, which matches the key raw — so keeping it raw both avoids forcing effects at elaboration and still matches.
pub(crate) fn canonical_scrutinee(context: &mut Context, term: &Term) -> Result<Term, ReduceError> {
    let canonical = match &**term {
        Subterm::Apply(Apply { head, arguments }) => {
            let arguments = arguments
                .iter()
                .map(|argument| {
                    let term = reduce(context, argument.term.clone())
                        .probed()?
                        .unwrap_or_else(|| argument.term.clone());
                    Ok(Argument {
                        term,
                        plicity: argument.plicity,
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;

            Ok(Subterm::Apply(Apply {
                head: head.clone(),
                arguments,
            })
            .into())
        }
        // An intrinsic's operands are arguments in the same sense, and a *key* is where it tells: a guard is recorded as written, so `10 <= Bytes/len(b)` keeps the `/sys` application and the concept dispatch it was spelled with, while every occurrence the reducer meets carries the intrinsics they unfold to, the arithmetic normal form they fold to, and — where the base is a local definition — the value it unfolds to. Only reduction reaches all three.
        //
        // The operands, never the node: the discipline the `Apply` arm above states as keeping the head verbatim. Reducing the node would meet this very key's refinement and canonicalize it to the arm's own value.
        //
        Subterm::Intrinsic(intrinsic) => canonical_operands(context, intrinsic),
        _ => Ok(term.clone()),
    }?;
    // A *solved* metavariable is materialized rather than left standing as its identity, for the same reason the levels below are erased: two occurrences of one written term elaborate to two independently minted metavariables, and an inferred implicit one level down — `g(@?m, b)` against `g(@?m', b)` with both solved to `Bool` — would then store a key no probe can match, and the refinement would silently not fire where the identical term with the implicit supplied explicitly does. Cheap where it does not apply: the walk returns at a cached `has_metavar` bit.
    let canonical = zonk_solved_term_metas(context, &canonical);
    // Erased for the same reason, and unsound for the same reason, as in [`shallow_scrutinee`] — which carries the account and why keeping the levels is not the repair.
    Ok(project_erased_universes(&canonical))
}

/// `intrinsic` with each operand in weak-head normal form, each a [`Probe`] as [`canonical_scrutinee`]'s arguments are: an operand that cannot reduce is kept as written.
///
/// The operands and never the node, and the two passes agree on what an operand is because both are `Intrinsic::traverse`, the one definition of an intrinsic's operands — the correspondence `convert`'s `decompose` already rests on.
fn canonical_operands(context: &mut Context, intrinsic: &Intrinsic) -> Result<Term, ReduceError> {
    let mut masking = Visit::masking(|_, _: &Var| None, Term::type_ground());
    intrinsic.traverse(&mut masking);

    let mut operands = Vec::new();
    for operand in masking.take_masked_children() {
        operands.push(
            reduce(context, operand.clone())
                .probed()?
                .unwrap_or(operand),
        );
    }

    let mut index = 0;
    let rebuilt = intrinsic.traverse(&mut Visit::rewriting(
        |_, _: &Var| None,
        Box::new(move |_, operand: &Term| {
            let value = operands.get(index).cloned();
            index += 1;

            Some(value.unwrap_or_else(|| operand.clone()))
        }),
    ));

    Ok(Subterm::Intrinsic(rebuilt).into())
}

fn reduce_apply(context: &mut Context, apply: Apply) -> Result<Reduce, ReduceError> {
    let Apply { head, arguments } = apply;

    let param_refs = arguments
        .iter()
        .map(|argument| &argument.term)
        .collect::<Vec<_>>();

    let head = reduce(context, head)?;
    let head = expose_rec_tail(context, head)?;
    match Term::unwrap_or_clone(head) {
        Subterm::Func(Func { telescope, .. }) => Ok(Reduce::Continue(telescope.open(&param_refs))),
        head => Ok(Reduce::Break(Term::from(Subterm::Apply(Apply {
            head: head.into(),
            arguments,
        })))),
    }
}

fn reduce_proj(context: &mut Context, proj: Proj) -> Result<Reduce, ReduceError> {
    let Proj { head, field } = proj;
    // Label projections are normally resolved (and rebuilt positionally) by elaborate. One path reaches here with a label still attached: `elaborate_apply` substitutes a *postponed* argument's raw surface term into the remaining telescope and the result type (that raw spelling is load-bearing — beta-reducing it through the result is what lets `expect` pin the metavariables the postponed slot is waiting on). If such an argument is a lambda whose body projects by label, beta-reduction here manufactures a label projection on a not-yet-solved head, which no earlier pass could have resolved. Leave it stuck rather than panicking: the conversion it sits under then fails as an ordinary mismatch at its origin span, or succeeds once the slot is settled and re-opened.
    let Field::Index(index) = field else {
        return Ok(Reduce::Break(Term::from(Subterm::Proj(Proj {
            head,
            field,
        }))));
    };

    if let Some(v) = context.proj_reduct(&head, index) {
        return Ok(Reduce::Continue(v.clone()));
    }

    match Term::unwrap_or_clone(reduce_forced(context, head)?) {
        Subterm::Tuple(Tuple { fields, .. }) => Ok(Reduce::Continue(
            fields
                .into_iter()
                .nth(index)
                .expect("Proj: index out of bounds"),
        )),
        // The untyped reducer's flat view of a constructor value, mirroring the runtime layout `(tag, payload...)`: field i + 1 is the i-th payload component. Field 0 (the tag) is never projected at the term level — dispatch inspects the `Variant` directly. Nothing in this module builds such a projection (see [`reduce_match`]'s rejected alternative); the view stays because a `Proj` over a constructor value can still arrive from elsewhere, and answering it is strictly better than leaving it stuck.
        Subterm::Variant(ctor) if (1..=ctor.payload.len()).contains(&index) => {
            Ok(Reduce::Continue(
                ctor.payload
                    .into_iter()
                    .nth(index - 1)
                    .expect("index bounded above"),
            ))
        }
        // A struct value is projected positionally with *no* tag offset (unlike `Variant`, whose field 0 is the tag): `Proj(Struct, i)` is field `i`.
        Subterm::Struct(Struct { fields, .. }) if index < fields.len() => Ok(Reduce::Continue(
            fields.into_iter().nth(index).expect("index bounded above"),
        )),
        head => {
            let head: Term = head.into();
            match context.proj_reduct(&head, index) {
                Some(v) => Ok(Reduce::Continue(v.clone())),
                None => Ok(Reduce::Break(Term::proj(head, index))),
            }
        }
    }
}

fn reduce_func_eta(context: &mut Context, func: Func) -> Result<Reduce, ReduceError> {
    let n = func.telescope.len();

    // Three arity-sized vectors and one opening, charged whether or not the probe succeeds — the probe is what allocates.
    context.spend(
        Cost::collection(n as u64)
            .saturating_mul(3)
            .saturating_add(Cost::term(1).saturating_mul(n as u64)),
    )?;

    let freshs = (0..n).map(|_| context.fresh(None)).collect::<Vec<_>>();

    let ys = freshs.iter().map(Term::free_var).collect::<Vec<_>>();

    let y_refs = ys.iter().collect::<Vec<_>>();

    match Term::unwrap_or_clone(func.telescope.open(&y_refs)) {
        Subterm::Apply(Apply { head, arguments })
            if arguments.len() == n
                && arguments.iter().enumerate().all(
                    |(i, a)| matches!(a.term.as_ref(), Subterm::Var(v) if v.unwrap() == &freshs[i]),
                )
                && freshs.iter().all(|f| !head.free_vars().contains(f)) =>
        {
            Ok(Reduce::Continue(head))
        }
        _ => Ok(Reduce::Break(Term::from(Subterm::Func(func)))),
    }
}

/// Dispatch a `match` over its scrutinee's already-reduced-and-forced value, where `forced` is what `reduce_forced` produced for it.
///
/// **Rejected — binding an arm to projections of the original scrutinee.** Taking the unreduced scrutinee alongside `forced` and opening the arm at `head.(i + 1)`, the flat view in [`reduce_proj`], would keep a reduced payload from carrying evaluated definition internals — local-`let` annotation holes elaboration never births — into types flowing on to `zonk`. It would buy that at the cost of emitting a term Core cannot type: `Proj` has no rule for an inductive value, so the residual is well-formed only to the untyped reducer. One escaping into a metavariable solution candidate is refused by the re-validation in `convert`'s `solve` as `NotATuple`, which is a *hard* verdict — the goal fails outright instead of parking, and an ordinary program comparing a matched payload against its value would be rejected. Binding the payload directly is also what the kernel does, so the two checkers agree here.
fn reduce_match(forced: Term, result: MatchResult, cases: Cases) -> Reduce {
    match cases {
        Cases::Bool {
            false_case,
            true_case,
        } => match Term::unwrap_or_clone(forced) {
            Subterm::Intrinsic(Intrinsic::Bool(false)) => Reduce::Continue(false_case),
            Subterm::Intrinsic(Intrinsic::Bool(true)) => Reduce::Continue(true_case),
            forced => Reduce::Break(Term::from(Subterm::Match(Match {
                head: forced.into(),
                result,
                cases: Cases::Bool {
                    false_case,
                    true_case,
                },
            }))),
        },

        Cases::Switch { cases, default } => {
            let scrutinee = forced;
            // A literal `Nat` is the kernel's spine floor over a `Zero` inner, so `is_zero` on the peeled inner is exactly "is this a concrete `k`?" — the same spine view the arithmetic family reads. A literal dispatches to its case, or the default when none matches; a symbolic scrutinee rebuilds the neutral switch.
            let (value, inner) = Nat::decompose(&scrutinee);

            match Nat::is_zero(&inner) {
                true => {
                    let body = cases
                        .iter()
                        .find(|(key, _)| key == &value)
                        .map(|(_, body)| body)
                        .unwrap_or(&default);

                    Reduce::Continue(body.clone())
                }
                false => Reduce::Break(Term::from(Subterm::Match(Match {
                    head: scrutinee,
                    result,
                    cases: Cases::Switch { cases, default },
                }))),
            }
        }

        // Dispatch on the reduced scrutinee — a `Variant` directly, or one reached through a match-arm refinement (`refine_head` registers `head := ctor_val`, which `reduce` follows). The selected arm is opened at the constructor's own payload components; `Scope::open` is what holds an arm and the constructor it dispatches on to the same arity.
        Cases::Induct { cases, default } => {
            if let Subterm::Variant(ctor) = &*forced {
                if let Some((_, scope)) = cases.iter().find(|(tag, _)| tag == &ctor.tag) {
                    let payload = ctor.payload.iter().collect::<Vec<_>>();

                    return Reduce::Continue(scope.open(&payload));
                }

                // A concrete constructor with no enumerated arm takes the catch-all default, which binds nothing (no scope to open).
                if let Some(default) = &default {
                    return Reduce::Continue(default.clone());
                }
            }

            Reduce::Break(Term::from(Subterm::Match(Match {
                head: forced,
                result,
                cases: Cases::Induct { cases, default },
            })))
        }

        // Structural induction on a native free-monoid intrinsic (`Nat`/`Bin`/`List`). The carrier-specific one-step decode lives in `FreeMonoid::uncons` (the eliminator-side analogue of `spine::peel_intrinsic`); this driver is the shared catamorphism over it. An identity `Layer` takes the empty arm; a cons `Layer` peels a generator (its head absent for the unary `Nat`) and recurses symbolically for the induction hypothesis; a stuck scrutinee rebuilds.
        Cases::FreeMonoid { carrier } => {
            let scrutinee = Term::unwrap_or_clone(forced);

            let layer = match &carrier {
                Carrier::Nat { .. } => FreeMonoid::Unary,
                Carrier::Bin { grain, .. } => FreeMonoid::Bin(*grain),
                Carrier::List { .. } => FreeMonoid::List,
            }
            .uncons(scrutinee);

            match layer {
                Layer::Empty => Reduce::Continue(match carrier {
                    Carrier::Nat { empty_case, .. }
                    | Carrier::Bin { empty_case, .. }
                    | Carrier::List { empty_case, .. } => empty_case,
                }),
                Layer::Cons { head, tail } => {
                    let ih: Term = Subterm::Match(Match {
                        head: tail.clone(),
                        result: result.clone(),
                        cases: Cases::FreeMonoid {
                            carrier: carrier.clone(),
                        },
                    })
                    .into();

                    // The cons arm binds the generator's payload (a head, absent for the unary `Nat`), then the tail and the induction hypothesis.
                    Reduce::Continue(match &carrier {
                        Carrier::Nat { cons_case, .. } => cons_case.open(&[&tail, &ih]),
                        Carrier::Bin { cons_case, .. } | Carrier::List { cons_case, .. } => {
                            cons_case.open(&[
                                head.as_ref().expect("Bin/List cons layer carries a head"),
                                &tail,
                                &ih,
                            ])
                        }
                    })
                }
                Layer::Stuck(scrutinee) => Reduce::Break(Term::from(Subterm::Match(Match {
                    head: scrutinee.into(),
                    result,
                    cases: Cases::FreeMonoid { carrier },
                }))),
            }
        }
    }
}

/// Zeta: substitute a `let`'s bindings into its tail, as the kernel's `step_let` does.
///
/// Binding each value as a fresh context definition and opening the tail over those names would copy no value into its uses, but the minted names would stand in reducts where the kernel's copy of the same reduction has the values: two unfoldings of one definition would name its `let`s differently, so every comparison by spelling — a sum's cancellation, a position's peel through a concatenation, a guard's refinement key — would miss terms conversion identifies, and the metavariable solver would have to reify minted names back out of its candidates. Substituting is the kernel's rule, so a reduct is spelled as the kernel spells it. Bindings are non-recursive and bind left to right, so binding `i` sees exactly the values before it.
fn reduce_let(context: &mut Context, let_: Let) -> Result<Reduce, ReduceError> {
    // One values vector, and a fresh ref vector at every binding — triangular in the run's length, and charged as the kernel charges it.
    let bindings = let_.bindings.len() as u64;
    context.spend(
        Cost::collection(bindings)
            .saturating_add(Cost::buffer(
                bindings.saturating_mul(bindings.saturating_add(1)) / 2,
            ))
            .saturating_add(Cost::term(1).saturating_mul(bindings)),
    )?;

    let mut values: Vec<Term> = Vec::with_capacity(let_.bindings.len());
    for binding in &let_.bindings {
        let refs = values.iter().collect::<Vec<_>>();
        values.push(binding.value().release(&refs));
    }

    let refs = values.iter().collect::<Vec<_>>();
    Ok(Reduce::Continue(let_.tail.open(&refs)))
}

fn reduce_var(context: &Context, var: Var) -> Reduce {
    match context.var_reduct(var.unwrap()) {
        Some(next) => Reduce::Continue(next.clone()),
        None => Reduce::Break(Term::var(var)),
    }
}

fn reduce_metavar(context: &Context, metavar: Metavar) -> Reduce {
    // Resolution rewrites the (birth-named) solution through the occurrence's spine, so a solution mentioning a sibling binder lands on whatever that binder corresponds to here.
    match context.resolve_metavar(&metavar) {
        Some(solution) => Reduce::Continue(solution),
        None => Reduce::Break(Term::from(Subterm::Metavar(metavar))),
    }
}

fn reduce_instance(context: &Context, instance: Instance) -> Result<Reduce, ReduceError> {
    let Instance { head, levels } = instance;

    let reduct = match &head {
        InstanceHead::Var(var) => match context.var_reduct_at(var.unwrap()).cloned() {
            Some(reduct) => reduct,
            None => return Ok(Reduce::Break(Term::instance(head, levels))),
        },
        InstanceHead::RecProj(group, index) => {
            return Ok(Reduce::Continue(Term::rec_proj(
                group
                    .instantiate_universes(&levels)
                    .map_err(ReduceError::Universe)?,
                *index,
            )));
        }
    };

    // The variable's stored value may itself be a projection, whose group takes the instance whole rather than a per-level rewrite.
    let reduct = match reduct.as_rec_proj() {
        Some((group, index)) => Term::rec_proj(
            group
                .instantiate_universes(&levels)
                .map_err(ReduceError::Universe)?,
            index,
        ),
        None => {
            instantiate_universe_levels_scoped(&reduct, &levels).map_err(ReduceError::Universe)?
        }
    };
    Ok(Reduce::Continue(reduct))
}

/// What canonicalizing one refinement key may spend before the attempt is abandoned.
///
/// A ceiling on *discarded* work, not a limit on what a program may compute: settling a key collapses two spellings of one comparison, and failing to settle it leaves them uncollapsed, so the program means the same either way. Sized to settle the shapes a guard actually takes — a comparison over parameters, over a local definition, over a measured literal — while stopping well short of a subject an accumulation built, which is the case that would otherwise spend a whole declaration on one attempt.
const CANONICAL_KEY_ALLOWANCE: u64 = 100_000;

/// [`canonical_scrutinee`] of a *registered key*, capped and memoized.
///
/// Both halves are load-bearing and neither works alone. The escalation runs per probe while this answer is per key, so without the memo one guard's subject is re-derived at every node of that operation in the declaration. And the first attempt can be the expensive one, so without the cap there is no first success to memoize — a guard over a subject a hundred thousand iterations built consumes the declaration's whole budget on that attempt.
///
/// A key the allowance stopped short is memoized as *itself*, so the bail is paid once and the key keeps its written spelling.
fn canonical_key(context: &mut Context, key: &Term, original: &Term) -> Result<Term, ReduceError> {
    if let Some(cached) = context.cached_canonical_key(key) {
        return Ok(cached);
    }

    // Canonicalized from the *original* spelling, never the key: the key is universes-erased, and erasure strips the `Instance` a polymorphic global unfolds through, so reducing the key stalls exactly where the probe side reduced — reduce-then-erase and erase-then-reduce disagree, and the probe side is reduce-then-erase.
    let canonical = context
        .within_allowance(CANONICAL_KEY_ALLOWANCE, |context| {
            canonical_scrutinee(context, original)
        })?
        .unwrap_or_else(|| key.clone());

    context.record_canonical_key(key.clone(), &canonical);

    Ok(canonical)
}

/// A stuck comparison under a guard recorded on another spelling of it, asked at the shallow key under each of [`probe_spellings`] in turn. The false arm of `n < m` refines `n < m` and nothing else, and `m <= n` is that fact read the other way, so the dual answers with the literal negated. A bound reaches the probe as `i + 1 <= len(l)` while the guard that decided it was written `i < len(l)`, one fact on `Nat` and on `Int`, so the successor spelling answers with the literal carried across — these are the same proposition, where a dual's are opposite ones. And the two compose: the false arm of `x <= 4` is the fact `5 <= x`, which the dual of the successor spelling answers. Lookup only — the store keeps every key as written, which is what the comparison record protects and what keeps this the kernel's rule too.
fn refined_spelling(context: &Context, term: &Term) -> Option<Term> {
    let Subterm::Intrinsic(intrinsic) = &**term else {
        return None;
    };
    probe_spellings(intrinsic).find_map(|(spelling, negated)| {
        shallow_answer(context, &Term::intrinsic(spelling), negated)
    })
}

/// What the shallow key of `spelling` holds, as the answer for the probe it spells: the guard's literal, negated where `spelling` is a dual of the probe.
fn shallow_answer(context: &Context, spelling: &Term, negated: bool) -> Option<Term> {
    if !context.scrutinee_head_refined(spelling.head_key()?) {
        return None;
    }
    let literal = context
        .scrutinee_reduct(&shallow_scrutinee(context, spelling), spelling)?
        .as_bool()?;
    Some(Term::intrinsic(Intrinsic::Bool(literal != negated)))
}

/// The refinement probe for an intrinsic the loop has just folded — the second look every *other* arm of the dispatch gets for free.
///
/// **Why one arm needs its own.** Each arm returns a [`Reduce`]: on progress it `Continue`s, the loop comes back around, and the probe at the top runs again on the new term. That is how a refinement keeps up with reduction. This arm answers a normal form in one step and breaks, so without this the store is asked exactly once, about a term whose operands have not been reduced yet.
///
/// **And the key sits between the two shapes, which is what makes both probes necessary.** A guard is recorded as written (`shallow_scrutinee`), so `10 <= Bytes/len(b)` registers `10 <= /sys/Bytes/len(b)`. A window bound instantiated at a call arrives as `(0 + 10) <= /sys/Bytes/len(b)` — matching on the right, not the left — and the fold reduces *both* operands at once, to `10 <= Bytes/len(b)` with the `/sys` global unfolded to its `BinLen` intrinsic — spelled the same, a different node — matching on the left, not the right. Neither probe point alone ever sees a matching pair, which is why the escalation is what decides it and why it belongs here: after the fold the probe side is already canonical, so only the key has to be reduced, where escalating before the fold would reduce both.
fn refined_after_fold(context: &mut Context, folded: &Term) -> Result<Option<Term>, ReduceError> {
    if !context.has_scrutinee_refinements() {
        return Ok(None);
    }

    let Some(head) = folded.head_key() else {
        return Ok(None);
    };

    if let Some(value) = refined_by_spelling(context, folded, head)? {
        return Ok(Some(value));
    }

    let Subterm::Intrinsic(intrinsic) = &**folded else {
        return Ok(None);
    };
    // The other spellings, in the order the kernel asks them. A dual is asked at its shallow key alone. The successor spelling goes through the same two steps the written one does, and needs them for the same reason: a guard `i < List/len(l)` records its operand as the call the author wrote, while the bound `i + 1 <= List/len(l)` arrives with that call folded to its `ListLen` intrinsic, so the shallow probe misses on the seam exactly where it misses on a spelling, and only the escalation brings the two together. Its literal is the key's own, this being one proposition spelled twice rather than a negation.
    for (spelling, negated) in probe_spellings(intrinsic) {
        let spelling = Term::intrinsic(spelling);
        let answer = match (negated, spelling.head_key()) {
            (true, _) => shallow_answer(context, &spelling, true),
            (false, Some(head)) => refined_by_spelling(context, &spelling, head)?,
            (false, None) => None,
        };
        if answer.is_some() {
            return Ok(answer);
        }
    }

    Ok(None)
}

/// One spelling's lookup: the shallow key first, then the canonical escalation, or `None` where nothing is registered under `head` at all.
///
/// Suppression needs no arm: `scrutinee_reduct` withholds under it, and breaking on the folded term leaves standing the neutral a suppressed key wants.
///
/// The escalation brings both sides to the canonical form — the key's, capped and memoized, so once per key rather than once per node; the probe's through the same `canonical_scrutinee` the key's is, so the two meet however either was spelled. The probe side cannot be taken as canonical already, because `reduce_intrinsic` does not leave every operand in weak-head normal form: a `&&` behind a stuck left leaves its right as written. In practice the probe before decomposition reaches a connective first, since the loop re-runs it on every continued term; this one decides the folds that change a spelling, and canonicalizing an already-reduced operand is a cache hit.
fn refined_by_spelling(
    context: &mut Context,
    probe: &Term,
    head: HeadTag<'_>,
) -> Result<Option<Term>, ReduceError> {
    if !context.scrutinee_head_refined(head) {
        return Ok(None);
    }

    let shallow = shallow_scrutinee(context, probe);
    if let Some(value) = context.scrutinee_reduct(&shallow, probe) {
        return Ok(Some(value.clone()));
    }

    let entries = context.scrutinee_entries(head);
    if entries.is_empty() {
        return Ok(None);
    }
    let canonical = canonical_scrutinee(context, probe)?;
    for (key, entry) in entries {
        if canonical_key(context, &key, &entry.original)? == canonical
            && !levels_clash_on_a_decided_instance(context, probe, &entry.original)?
        {
            return Ok(Some(entry.value));
        }
    }

    Ok(None)
}

/// The refinement probe at a stuck reduct: an entry's *reduced* spelling, where the written one and its canonical form both missed.
///
/// **The kernel's `refined_reduct`, so that the two checkers look in the same places.** Every other lookup here keeps a key's head as written — the shallow key verbatim, the escalation with only its arguments reduced — so without this a stuck form reduction reached *through* the guard's definition would never meet it: under `match small(k) | true => …`, with `small(n) = n < 10`, the arm's hypothesis `Holds(k < 10)` is the guard itself one definition down, the kernel answers it `true`, and the elaborator would refuse it. A field checked before its struct's parameter is inferred meets the same miss later, parked already unfolded to `?k < 10` and retried as `k < 10`.
///
/// So each entry is compared at the form reduction itself gives it — weak-head, operands canonical where it is a tagged comparison, solved metavariables materialized, universes erased — and the dual and successor spellings with it, exactly as the kernel settles and compares. The settled spellings are asked first, innermost first, and only when none answers is the innermost entry not yet asked settled, one at a time, until one answers or none is left: a settlement is the one cost here, and a probe an already-settled spelling answers pays none.
///
/// **What decides whether to settle at all is the kernel's filter**: an entry is a candidate only if the probe names no local its key does not, since reduction can drop a local and never introduce one. Without it, every stuck form under a binder some other judgment opened would settle some key in any arm. And the probe must bear a local itself, as the kernel's must: a local-free term has nothing an arm's equation could be about that reduction would not already have decided.
fn refined_reduct(context: &mut Context, value: &Term) -> Result<Option<Term>, ReduceError> {
    if !context.has_scrutinee_refinements()
        || !value.has_local_free()
        || context.visible_scrutinee_entries().next().is_none()
    {
        return Ok(None);
    }
    curios_profile::profile!("reduce::refined_reduct");

    let probe = reduct_spelling(context, value)?;
    // The spellings a settled entry can answer, each with whether its literal is negated on the way: the probe itself and its successor spelling are the entry's proposition, the duals its negation.
    let others = match &*probe {
        Subterm::Intrinsic(intrinsic) => probe_spellings(intrinsic)
            .map(|(spelling, negated)| (Term::intrinsic(spelling), negated))
            .collect(),
        _ => Vec::new(),
    };
    let spellings = std::iter::once((probe.clone(), false))
        .chain(others)
        .map(|(spelling, negated)| (project_erased_universes(&spelling), negated))
        .collect::<Vec<_>>();

    loop {
        match scan_settled(context, value, &probe, &spellings)? {
            Scan::Answer(answer) => return Ok(Some(answer)),
            Scan::Settle {
                frame,
                key,
                original,
            } => settle(context, frame, key, &original)?,
            Scan::Miss => return Ok(None),
        }
    }
}

/// What one pass over the visible entries found for a stuck reduct.
enum Scan {
    /// A settled spelling answered: the entry's value, negated where the probe met it as its dual.
    Answer(Term),
    /// Nothing settled answered, and this is the innermost entry the probe could be a reduct of that has never been settled.
    Settle {
        frame: usize,
        key: Term,
        original: Term,
    },
    /// Nothing settled answered, and nothing the probe could be a reduct of is left to settle.
    Miss,
}

/// One pass, innermost first: a settled spelling that answers wins outright, and only where none does is the first eligible unsettled entry handed back to be settled — the kernel's order, which asks every settled spelling before it pays for a settlement. Every comparison is a cached hash until two terms are equal.
fn scan_settled(
    context: &Context,
    value: &Term,
    probe: &Term,
    spellings: &[(Term, bool)],
) -> Result<Scan, ReduceError> {
    let mut unsettled = None;

    for (frame, key, entry) in context.visible_scrutinee_entries() {
        match context.settled_key(frame, key) {
            Some(Some(settled)) => {
                for (spelling, negated) in spellings {
                    if settled.compared != *spelling
                        || levels_clash_on_a_decided_instance(context, value, &settled.unerased)?
                    {
                        continue;
                    }

                    let answer = match negated {
                        false => Some(entry.value.clone()),
                        true => entry
                            .value
                            .as_bool()
                            .map(|literal| Term::intrinsic(Intrinsic::Bool(!literal))),
                    };
                    if let Some(answer) = answer {
                        return Ok(Scan::Answer(answer));
                    }
                }
            }
            Some(None) => {}
            None => {
                if unsettled.is_none() && could_reduce_to(&entry.original, probe) {
                    unsettled = Some((frame, key.clone(), entry.original.clone()));
                }
            }
        }
    }

    Ok(match unsettled {
        Some((frame, key, original)) => Scan::Settle {
            frame,
            key,
            original,
        },
        None => Scan::Miss,
    })
}

/// Settle the reduced spelling of the entry `key` registered in `frame`: its unerased spelling reduced with that frame and every frame inside it withheld, then brought to [`reduct_spelling`]'s form.
///
/// **Withheld for the kernel's two reasons.** The entry's own frame holds the equation being settled, which reducing its key would meet at the first probe and answer with the case value it is assuming; and an inner frame retracts before the entry does, so a spelling resting on one would outlive its justification. The frames outside are exactly the equations the entry may rest on, and `Frames::withhold_refinements_from` leaves them live.
///
/// **Capped where the kernel is not**, at the allowance a canonical key takes, because it is the same kind of work: optional, since an unsettled entry answers nothing and the program means what it meant, and unbounded in the worst case, since a guard over a subject an accumulation built reduces that accumulation. The kernel settles each entry once; the elaborator settles one again after every invalidation that clears the settled spellings, which `Caches::settled_keys` accounts for. A refusal or a bail settles the entry as having no reduced spelling, so it is paid once; exhaustion of the declaration itself propagates.
fn settle(
    context: &mut Context,
    frame: usize,
    key: Term,
    original: &Term,
) -> Result<(), ReduceError> {
    // The key and its frame are what a hunt for repeated settlements needs: the same pair recurring is an entry settled again after an invalidation, and the costliest calls name the keys that pay.
    curios_profile::profile!("reduce::settle", key = %original, frame);
    let settled = context.with_refinements_withheld_from(frame, |context| {
        context.within_allowance(CANONICAL_KEY_ALLOWANCE, |context| {
            let reduct = reduce(context, original.clone())?;
            reduct_spelling(context, &reduct)
        })
    });

    match settled {
        Ok(reduct) => {
            let settled = reduct.map(|unerased| Settled {
                compared: project_erased_universes(&unerased),
                unerased,
            });
            context.record_settled_key(frame, key, settled);
            Ok(())
        }
        Err(error) => {
            context.record_settled_key(frame, key, None);
            Err(error)
        }
    }
}

/// The spelling a reduct is compared in: its operands in weak-head normal form where it is a tagged comparison — a connective's right operand behind a stuck left is otherwise left as written, and the two sides would differ by exactly the fold the escalation exists to see through — and its solved metavariables materialized. Unerased: a hit reads its universe instance from it, and erasure is the comparison's.
///
/// The kernel's `canonical_operands`, gated the same way, on a `head_key` rather than on every intrinsic.
fn reduct_spelling(context: &mut Context, term: &Term) -> Result<Term, ReduceError> {
    let spelled = match (&**term, term.head_key()) {
        (Subterm::Intrinsic(intrinsic), Some(_)) => canonical_operands(context, intrinsic)?,
        _ => term.clone(),
    };

    Ok(zonk_solved_term_metas(context, &spelled))
}

/// Reduce `term` until its head constructor is stable.
///
/// Reduction re-enters itself once per operand of a nested intrinsic, once per link of a spine peel, and once per level of a match tower, so a *data*-shaped term puts its depth on the native stack even though its unrolling does not. Running inside [`recurse`] rather than aborting is what keeps [`DEFAULT_STEP_BUDGET`](crate::DEFAULT_STEP_BUDGET) the only bound that decides whether a term reduces: a stack limit would make acceptance depend on the host's stack size and on frame sizes the optimizer chose, which is exactly the machine-dependence the step budget exists to keep out of the answer.
///
/// A budget that bounded *steps* alone would leave the memory such a walk allocates unbounded: a runaway type-level computation would run until the budget stopped it, allocating as it went. A transition costs one unit, a construction costs what it builds, and the [`Cost::FRAME`] charged at this bracket prices the native frame a level takes — so the stack this walk grows into is bounded by the same number that bounds how far it reduces, and both are facts about the program rather than about the host. `documentation/design/soundness/a-reduction-step-costs-what-it-builds.md` carries the decision.
pub(crate) fn reduce(context: &mut Context, term: Term) -> Result<Term, ReduceError> {
    // The level itself, charged when it is a new peak — the kernel's `whnf` charges the same row the same way. See [`Context::enter_level`] and [`Cost::FRAME`].
    context.enter_level()?;
    let reduct = recurse(|| reduce_within(context, term));
    context.leave_level();

    reduct
}

fn reduce_within(context: &mut Context, mut term: Term) -> Result<Term, ReduceError> {
    if let Some(cached) = context.cached_reduced(&term) {
        return Ok(cached);
    }

    // A closed term takes the machine: same rules, same counter, machine depth instead of one native frame per element. Stored under the same cache entry a strategy-derived reduct would be.
    if machine_admissible(context, &term) {
        let entry = term.clone();
        let result = reduce_closed(context, term, Demand::Whnf)?;
        context.reduce(entry, &result);

        return Ok(result);
    }

    let entry = term.clone();
    // Whether the loop is continuing from an answer the reduced spellings gave. See the `Reduce::Break` arm below.
    let mut answered = false;

    loop {
        context.spend(Cost::STEP)?;

        let step = 'step: {
            // Index refinement for stuck applications (convertibility-keyed). Gated cheaply — store non-empty, then a refined applied-head symbol — before keying the candidate and looking it up.
            if context.has_scrutinee_refinements()
                && let Some(head) = term.head_key()
                && context.scrutinee_head_refined(head)
            {
                let shallow = shallow_scrutinee(context, &term);

                if let Some(value) = context.scrutinee_reduct(&shallow, &term) {
                    // A key a suppressed frame withholds answers `None` here, so this serves only what is live — the caller's own arm outside a re-validation, and the validated term's own arms within one.
                    break 'step Reduce::Continue(value.clone());
                } else if context.refinements_suppressed() && context.is_scrutinee_key(&shallow) {
                    // Withhold the value, but keep an application key neutral — as a `Var` key already is — so `solve_at_birth`'s committed spelling stays a term the live refinement can fire on (the registered form, never the unfolded body). What stays neutral is the probe as spelled, never the key: the key erases universe instances, and a solution committed from it would hold a bare occurrence of a universe scheme, which the kernel refuses.
                    break 'step Reduce::Break(term.clone());
                } else {
                    // Escalate: the candidate and the registered key are spelled differently, so decide it by *convertible* arguments rather than written ones. Only here is anything reduced, and only against entries sharing this head — canonicalizing one under another head would spend the declaration's budget to learn nothing.
                    //
                    // Under suppression too, since `scrutinee_entries` reads only the frames suppression does not withhold: a re-validated term's own arms answer a respelled occurrence as they answer one spelled like their guard, and the arm the caller sits in answers neither. On the unsuppressed branch alone, a candidate whose arm meets its guard through a `let` in another definition's unfolding — `Str/step`'s `n` — would be rejected where elaborating the same term accepts it.
                    let candidates = context.scrutinee_entries(head);

                    if !candidates.is_empty() {
                        let canonical = canonical_scrutinee(context, &term)?;

                        for (key, entry) in candidates {
                            if canonical_key(context, &key, &entry.original)? == canonical
                                && !levels_clash_on_a_decided_instance(
                                    context,
                                    &term,
                                    &entry.original,
                                )?
                            {
                                break 'step Reduce::Continue(entry.value);
                            }
                        }
                    }
                }
            }

            // Another spelling, when the written one has no key: the false arm of `n < m` recorded `n < m` alone, and `m <= n` is the same fact read the other way; a bound arriving as `i + 1 <= len(l)` under a guard written `i < len(l)` is one fact spelled twice; and the false arm of `x <= 4` is `5 <= x`, the two readings composed. Lookup only — every key stays as written.
            if context.has_scrutinee_refinements()
                && let Some(value) = refined_spelling(context, &term)
            {
                break 'step Reduce::Continue(value);
            }

            match Term::unwrap_or_clone(term) {
                // **The one arm that answers a normal form and breaks, so the one that has to ask the store a second time.** Every other arm `Continue`s when it makes progress and the loop comes back around, which re-runs the probe above on the new term — that is how a refinement keeps up with reduction, and why no other arm needs anything here. This one folds and leaves, so a fold that turns the term *into* the registered shape would never be asked about again: `Le(s + l, len b)` instantiated at a call is `NatLe(0 + n, len b)`, whose sum folds to `n` only inside `reduce_intrinsic`, and every window bound in the language has that shape.
                //
                // The shallow key alone. After the fold the term is a normal form, so the escalation the probe above needs — for a candidate spelled differently — has nothing left to collapse, and reducing operands to find out is what the split between `shallow_scrutinee` and `canonical_scrutinee` exists to avoid: an intrinsic's head tag is its *operation*, so one registered guard would put that cost on every comparison of that operation in the declaration. Suppression needs no arm either: `scrutinee_reduct` withholds under it, and breaking on the folded term is already the neutral a suppressed key wants left standing.
                Subterm::Intrinsic(intrinsic) => {
                    let folded: Term = reduce_intrinsic(context, &intrinsic)?.into();

                    match refined_after_fold(context, &folded)? {
                        Some(value) => Reduce::Continue(value),
                        None => Reduce::Break(folded),
                    }
                }
                // The scrutinee is reduced by a nested call, so a tower of matches over a deep closed spine costs one native frame per link. That is data-shaped depth, which is what [`recurse`] at the entry point is for. The nested call probes and stores the reduction cache under the scrutinee itself, so a warm scrutinee needs no special case here.
                Subterm::Match(m) => {
                    let value = reduce(context, m.head)?;

                    reduce_match(force_rec(context, value)?, m.result, m.cases)
                }
                Subterm::Apply(apply) => reduce_apply(context, apply)?,
                Subterm::Proj(proj) => reduce_proj(context, proj)?,
                Subterm::Func(func) => reduce_func_eta(context, func)?,
                Subterm::Let(let_) => reduce_let(context, let_)?,
                Subterm::Var(var) => reduce_var(context, var),
                Subterm::Metavar(metavar) => reduce_metavar(context, metavar),
                Subterm::Instance(instance) => reduce_instance(context, instance)?,
                // `InductType`/`Variant` and `StructType`/`Struct` are intrinsic normal forms, like `Tuple`: their sub-terms are not reduced in WHNF.
                term => Reduce::Break(term.into()),
            }
        };

        match step {
            Reduce::Continue(next) => term = next,
            // A stuck form standing under an arm's equation *is* that case's value, and the written spellings have all been asked by now: the one probe left is the entries' reduced spellings, the kernel's second point.
            //
            // **An answer is final.** What it hands back is a case value — a constructor or a literal, a normal form — so there is nothing left for an equation to say about it, and it is not asked again. That is a rule rather than an observation: a reduced spelling can itself be a case value, where equations outside an entry decide its key, and a frozen frame restored for a retry re-registers an arm's equation in a frame inside its own, whose spelling then settles to the very value it assumes. Asked again, such a value would answer itself, which the loop would take for progress until the budget ran out, and two entries spelled as each other's values would trade it back and forth the same way.
            Reduce::Break(result) => match answered {
                true => {
                    context.reduce(entry, &result);
                    return Ok(result);
                }
                false => match refined_reduct(context, &result)? {
                    Some(value) => {
                        answered = true;
                        term = value;
                    }
                    None => {
                        context.reduce(entry, &result);
                        return Ok(result);
                    }
                },
            },
        }
    }
}

/// Reduce `term` to a deep normal form for **diagnostic display**: every position is taken to weak-head normal form and its sub-terms recursively normalized, opening the type-former binders (`FuncType`/`Func`/`TupleType`) under fresh variables.
///
/// `reduce` alone stops at the head: an inductive type's indices are not sub-reduced, so a concept-method projection standing in an index position — `Vec(Nat, Add/add(0, 1))`, spelled `Vec(Nat, (sys/witness@0).0(0, 1))` once resolution has picked the intrinsic witness — survives verbatim into a type-mismatch message. Normalizing the index collapses it to the value it denotes (`Vec(Nat, 1)`), or, when an operand is symbolic, to the underlying operator intrinsic (`Vec(Nat, n + m)`) the printer spells infix.
///
/// Display-only and best-effort: the result is never fed back into the kernel, and an exhausted step budget propagates so callers can fall back to the un-normalized spelling. The binder-heavy stuck forms (`Rec`, `Match`) keep their WHNF shape rather than being reduced under their own binders — they seldom carry the arithmetic this targets, and opening every case arm buys a diagnostic nothing.
///
/// A name whose unfolding stalls at one of those forms keeps its name. `double(n)` over a `rec` unfolds to the folded call's canonical neutral — a `RecProj`-headed application — and a `match`-defined function applied to a variable unfolds to a stuck `Match`; the printer has no name for either, so it spells the whole body, a recursive group twice over, once per reference, and the reader's `n` is renamed against the binders the body brought in. The body says nothing the name does not, so the head stays as written and only the arguments normalize. A name that unfolds to something that *computed* — a literal, a constructor, a type former — still unfolds, which is what the witness-collapse and `2 + 3` fixtures in `curios/src/tests/runtime/diagnostic_tests.rs` and `curios-pipeline/src/tests/diagnostic_tests.rs` hold.
pub(crate) fn normalize(context: &mut Context, term: Term) -> Result<Term, ReduceError> {
    // Charged and guarded as `zonk_term` is: a level is a peak of depth the budget prices ([`Cost::FRAME`]), and the walk runs inside [`recurse`] so a deep term buys depth with heap rather than overflowing the native stack. Display-only or not, this walk is a route into unbounded computation — a term ten thousand applications deep, or a solution that reaches itself, would otherwise send it down the main thread's stack until the process aborted with no diagnostic at all, which is the one outcome a *diagnostic* walk must not have. Charged, the declaration's budget refuses the walk and the caller falls back to the un-normalized spelling, as its contract already allows.
    context.enter_level()?;
    let normalized = recurse(|| normalize_level(context, term));
    context.leave_level();

    normalized
}

/// The one choke point of the normalization walk, under the level [`normalize`] charged.
fn normalize_level(context: &mut Context, term: Term) -> Result<Term, ReduceError> {
    let reduced = reduce_forced(context, term.clone())?;
    if stalled_unfolding(&term, &reduced) {
        return normalize_arguments(context, term);
    }
    let span = reduced.span();

    let inner = match Term::unwrap_or_clone(reduced) {
        Subterm::Apply(Apply { head, arguments }) => Subterm::Apply(Apply {
            head: normalize(context, head)?,
            arguments: normalize_argument_vec(context, arguments)?,
        }),
        Subterm::Proj(Proj { head, field }) => Subterm::Proj(Proj {
            head: normalize(context, head)?,
            field,
        }),
        Subterm::InductType(InductType {
            name,
            universes,
            params,
            indices,
        }) => Subterm::InductType(InductType {
            name,
            universes,
            params: normalize_each(context, params)?,
            indices: normalize_each(context, indices)?,
        }),
        Subterm::StructType(StructType {
            name,
            universes,
            params,
        }) => Subterm::StructType(StructType {
            name,
            universes,
            params: normalize_each(context, params)?,
        }),
        Subterm::Variant(Variant {
            name,
            universes,
            params,
            tag,
            payload,
        }) => Subterm::Variant(Variant {
            name,
            universes,
            params: normalize_each(context, params)?,
            tag,
            payload: normalize_each(context, payload)?,
        }),
        Subterm::Struct(Struct {
            name,
            universes,
            params,
            fields,
            entries,
        }) => Subterm::Struct(Struct {
            name,
            universes,
            params: normalize_each(context, params)?,
            fields: normalize_each(context, fields)?,
            entries,
        }),
        Subterm::Tuple(Tuple { fields, names }) => Subterm::Tuple(Tuple {
            fields: normalize_each(context, fields)?,
            names,
        }),
        Subterm::FuncType(func_type) => {
            let plicities = func_type.plicities().to_vec();
            Subterm::FuncType(FuncType::new(
                normalize_telescope(context, func_type.telescope)?,
                plicities,
            ))
        }
        Subterm::Func(func) => {
            let plicities = func.plicities().to_vec();
            Subterm::Func(Func::new(
                normalize_telescope(context, func.telescope)?,
                plicities,
            ))
        }
        Subterm::TupleType(TupleType { telescope }) => Subterm::TupleType(TupleType {
            telescope: normalize_tuple_telescope(context, telescope)?,
        }),
        Subterm::Metavar(Metavar { id, spine, origin }) => Subterm::Metavar(Metavar {
            id,
            spine: normalize_each(context, spine.to_vec())?.into(),
            origin,
        }),
        // Leaves (`Type`/`Prop`/`Var`/`Intrinsic`, the last already carrying reduced operands) and the binder-heavy stuck forms (`Let`/`Rec`/`Match`) keep their weak-head normal shape. A stuck `Instance` is both at once: its head is a variable or an already-stuck projection, so there is nothing under it to normalize.
        other => other,
    };

    Ok(match span {
        Some(span) => Term::spanned(span, inner),
        None => Term::from(inner),
    })
}

fn normalize_each(context: &mut Context, terms: Vec<Term>) -> Result<Vec<Term>, ReduceError> {
    terms.into_iter().map(|t| normalize(context, t)).collect()
}

fn normalize_argument_vec(
    context: &mut Context,
    arguments: Vec<Argument>,
) -> Result<Vec<Argument>, ReduceError> {
    arguments
        .into_iter()
        .map(|argument| {
            Ok(Argument {
                term: normalize(context, argument.term)?,
                plicity: argument.plicity,
            })
        })
        .collect()
}

/// The head of an application, or the term itself: the position a name sits in, and the position a stuck form shows at. An instance wrapper is not looked through — its typed head is not a term — so a caller asking about the head's shape matches `Instance` beside the bare shape it wraps.
pub(crate) fn applied_head(term: &Term) -> &Term {
    match &**term {
        Subterm::Apply(Apply { head, .. }) => head,
        _ => term,
    }
}

/// Whether reducing `written` to `reduced` only unfolded a name into one of the binder-heavy stuck forms — a folded recursive call or recursive group, a stuck `match`, a lambda or a `let`, bare or at the head of an application. `double(n)` over a `rec` is the paradigm: the name unfolds to the folded call's canonical neutral, which spells as the whole group and says nothing the name did not. [`normalize`] keeps the name for display, and `convert`'s solver commits it as a solution's spelling.
pub(crate) fn stalled_unfolding(written: &Term, reduced: &Term) -> bool {
    matches!(
        &**applied_head(written),
        Subterm::Var(_)
            | Subterm::Instance(Instance {
                head: InstanceHead::Var(_),
                ..
            })
    ) && matches!(
        &**applied_head(reduced),
        Subterm::Rec(_)
            | Subterm::Match(_)
            | Subterm::Func(_)
            | Subterm::Let(_)
            | Subterm::Instance(Instance {
                head: InstanceHead::RecProj(..),
                ..
            })
    )
}

/// [`normalize`] with the head held as written: the arguments are normalized, the name is not unfolded.
fn normalize_arguments(context: &mut Context, term: Term) -> Result<Term, ReduceError> {
    let span = term.span();

    let inner = match Term::unwrap_or_clone(term) {
        Subterm::Apply(Apply { head, arguments }) => Subterm::Apply(Apply {
            head,
            arguments: normalize_argument_vec(context, arguments)?,
        }),
        other => other,
    };

    Ok(match span {
        Some(span) => Term::spanned(span, inner),
        None => Term::from(inner),
    })
}

/// Normalize a function/Π telescope (`Func`/`FuncType`): each parameter type, then the body, every one opened under fresh variables standing for the binders before it and re-closed under their labels — the display-side counterpart of [`convert`](mod@crate::convert)'s `compare_func_type` walk.
fn normalize_telescope(
    context: &mut Context,
    telescope: Telescope<Term>,
) -> Result<Telescope<Term>, ReduceError> {
    let (entries, body) = normalize_entries(context, &telescope)?;
    Ok(Telescope::build(entries, normalize(context, body)?))
}

/// Normalize a Σ telescope (`TupleType`): its field types, exactly like [`normalize_telescope`]. The `Done` body is `()`, carrying nothing to reduce.
fn normalize_tuple_telescope(
    context: &mut Context,
    telescope: Telescope<()>,
) -> Result<Telescope<()>, ReduceError> {
    let (entries, ()) = normalize_entries(context, &telescope)?;
    Ok(Telescope::build(entries, ()))
}

/// Each entry normalized under a fresh variable per binder before it, in one walk, with the payload opened at all of them: what the two normalizers rebuild with one pass of `Telescope::build`, rather than recursing into the reopened rest and re-closing every level.
fn normalize_entries<B: Bound>(
    context: &mut Context,
    telescope: &Telescope<B>,
) -> Result<(Vec<(Free, Term)>, B), ReduceError> {
    let mut entries = Vec::new();
    let mut cursor = telescope.cursor();

    while let Some((_, ty)) = cursor.entry() {
        let ty = normalize(context, ty)?;
        entries.push((cursor.advance_fresh(|hint| context.fresh(hint)), ty));
    }

    Ok((entries, cursor.body().expect("a cursor past every entry")))
}
