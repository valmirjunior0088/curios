//! Closed-term evaluation on an explicit stack: the machine that makes a fold cost what it computes rather than what the recursive strategy spends walking it.
//!
//! Both checkers' reducers are recursive functions that re-enter themselves once per operand of a nested intrinsic and once per link of a match tower, and both substitute arguments unreduced. On *closed* terms those are the two habits that manufacture cost the term never asked for: the re-entry puts one native frame — priced at [`Cost::FRAME`] — on the stack per element of a data-shaped walk, and the unreduced substitution threads accumulator chains that make every memo entry linear in how far the walk has come. This module evaluates the same terms with the recursion reified into a [`Frame`] stack and every substituted term taken to a value first, so depth costs the small frame it takes and the chains an unreduced substitution would thread are never built — an accumulator that is *genuinely* a growing closed neutral is the value itself, and the run-scoped memo below keeps re-encountering one linear.
//!
//! # Why this is representation, not judgment
//!
//! The two checkers duplicate their reduction strategies on purpose: strategy decides which *open* terms convert, and a strategy bug shared by both checkers is a bug neither can catch. A closed term is outside that argument — it has one result, whichever order finds it — so evaluating one is the same kind of thing [`reduce_intrinsic`] is: arithmetic on the representation, shared so the two checkers cannot price the same program differently. The host still owns everything judgment-shaped, through the [`ClosedHost`] seam: what a global unfolds to, and every charge, which lands on the host's own counter through [`Reducer::spend`].
//!
//! # What the machine refuses to see
//!
//! The entry gate is [`accelerable`] — no local frees and no metavariables, two bits cached per node — plus a host-side condition the hosts enforce at their call sites: no case-equation refinements in scope, because inside an arm a closed scrutinee *is* the arm's assumed value, definitionally, and evaluating it would answer a different question. A term that fails the gate takes the ordinary recursive strategy, unchanged.
//!
//! # How the machine differs from the strategy it replaces, and why each difference is safe
//!
//! **Substituted terms are evaluated first.** Arguments, `let` values, and `Induct` payload binds are taken to weak-head values before substitution, where the hosts substitute them unreduced. On a closed term the result is the same by confluence; what changes is cost, which is the point. Two consequences are handled rather than hoped away: a substituend whose evaluation *errors* falls back to its unreduced spelling — the hosts defer such an error until the position is demanded, and the fallback preserves exactly that — and a substituend whose evaluation exhausts the budget propagates, because spend is never refunded. An induction hypothesis is the deliberate exception: it is bound unreduced exactly as the hosts bind it, because evaluating one eagerly would run the whole tail fold whether or not the arm uses it.
//!
//! **Eta is contracted, with the hosts' own probe.** `(x) => f(x)` steps to `f` exactly as under the strategies, through the fresh binder identities [`ClosedHost::fresh_binder`] mints. This was learned rather than assumed: a first version skipped the probe on the theory that conversion decides eta anyway, and the elaborator's witness keying — which reads rigid heads off weak-head forms — stopped finding `Async` under `(A) => Async(A)` and failed the prelude at `/std/File`.
//!
//! **`Induct` arms bind payload values.** The kernel binds an arm to the scrutinee's payload directly; the elaborator binds projections of the original head, guarding annotation holes a reduced payload could carry. A closed, metavariable-free payload carries none, so the machine uses the kernel's rule on both sides.
//!
//! # What a step costs
//!
//! A dispatch spends [`Cost::STEP`], an opening spends the kernel's beta formula, a frame is charged per new *peak* of machine depth at [`machine_frame`] — the few slots a [`Frame`] takes, priced honestly the way [`Cost::FRAME`] prices the native frame the recursive strategy takes — and every write to the run-scoped memo is charged as the two handles it stores. That difference is the whole yield: a fold's per-element price falls from the native frame row to the transitions and openings the fold actually performs, and what the strategy amortized through its compilation-scoped memos the machine amortizes through the run-scoped one, since its internal steps bypass the hosts' tables.

use {
    super::{
        Apply, Bound, Carrier, Cases, Cost, Field, Free, FreeMonoid, Func, Intrinsic, Layer, Let,
        LetBinding, Many, Match, Nat, Proj, Rec, RecGroup, ReduceError, Reducer, Scope, Subterm,
        Telescope, Term, instantiate_universe_levels_scoped, reduce_intrinsic,
    },
    curios_abi::ForeignFunction,
    curios_utilities::Plicity,
    std::{collections::HashMap, sync::Arc},
};

/// What one machine frame costs when it deepens the stack past its previous peak: a [`Frame`] is a few slots of reducer-owned storage, priced on the buffer row. The native-frame analogue is [`Cost::FRAME`], three orders of magnitude larger, and the gap between the two is what this module exists to stop spending.
fn machine_frame() -> Cost {
    Cost::buffer(2)
}

/// What the machine asks of its host: the reduction seam every intrinsic fold already drives, plus the judgment the strategy owns — what a global unfolds to, and a fresh binder identity for the eta probe. Charges land on the host's counter through [`Reducer::spend`], so both checkers price a machine run identically because both run the same machine.
pub trait ClosedHost: Reducer {
    /// What `name` unfolds to through a bare occurrence. A universe-polymorphic definition is withheld, exactly as the host's own delta withholds it.
    fn closed_body(&self, name: &Free) -> Option<&Term>;

    /// What `name` unfolds to at a stated universe instance — the one position a polymorphic definition may be unfolded from.
    fn closed_body_at(&self, name: &Free) -> Option<&Term>;

    /// A fresh binder identity, for the eta probe's openings.
    fn fresh_binder(&mut self, hint: Option<&str>) -> Free;
}

/// Whether `term` is in the machine's domain: no local frees and no metavariables, read off the per-node cached bits in O(1). The hosts add their own judgment-side condition — no refinements in scope — at the call site.
pub fn accelerable(term: &Term) -> bool {
    !term.has_local_free() && !term.has_metavar()
}

/// How far the caller wants the focus taken: [`Demand::Whnf`] is the plain reducer contract, where a folded recursive application is its own normal form; [`Demand::Forced`] is the eliminator's, where a `rec` head unfolds because a value is required.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Demand {
    Whnf,
    Forced,
}

/// Evaluate a closed `term` to the value `demand` asks for, spending from `host`'s counter.
///
/// The caller is responsible for the gate: `term` must satisfy [`accelerable`], and no case-equation refinement may be in scope. The result is a weak-head value under the host's own reduction rules — computed in constant native depth per element of any data-shaped walk, which is the entire difference.
pub fn reduce_closed<H: ClosedHost>(
    host: &mut H,
    term: Term,
    demand: Demand,
) -> Result<Term, ReduceError> {
    Machine {
        stack: Vec::new(),
        peak: 0,
        values: HashMap::new(),
    }
    .run(host, term, demand)
}

/// What to do next: evaluate a term at a demand, or hand a finished value to the innermost frame.
enum Step {
    Eval(Term, Demand),
    Value(Term),
}

/// What an [`Args`] run does with its collected values once the last one is in.
enum Finish {
    /// Open a function telescope over the values and continue with the body.
    Beta {
        telescope: Telescope<Term>,
        demand: Demand,
    },
    /// Open an `Induct` arm over the payload values.
    Arm { body: Scope<Many>, demand: Demand },
    /// Fold the intrinsic through [`reduce_intrinsic`], serving it the values through a [`Tap`], and remember the result under `key` — the whole intrinsic term as focused.
    ///
    /// The memo is what keeps a *closed neutral* accumulator linear. A fold whose combining operation is genuinely stuck — byte arithmetic no rule folds, a runtime-only operation — accumulates a neutral chain one link deeper per element, exactly as it does under the hosts; without the memo the machine would re-walk that chain at every element, and a pathological program would price *worse* here than under the strategy this replaces. With it, a chain link evaluates once and every later encounter is one probe.
    Intrinsic { key: Term, intrinsic: Intrinsic },
    /// Rebuild the inert host call over its evaluated operands.
    Foreign { function: Arc<ForeignFunction> },
}

/// A left-to-right evaluation of a fixed list of terms, then a [`Finish`] over the values.
///
/// `catch` marks the positions the hosts substitute unreduced — beta arguments and arm payload binds — where an evaluation error must fall back to the original spelling rather than surface, because the host would have deferred it until the position was demanded. Intrinsic and foreign operands are strict in the hosts too, so their runs do not catch.
struct Args {
    originals: Vec<Term>,
    done: Vec<Term>,
    next: usize,
    item_demand: Demand,
    catch: bool,
    finish: Finish,
}

/// One reified level of the recursive strategy: what the enclosing computation will do with the value under focus.
enum Frame {
    /// An application waiting for its head. `memo_key` is the application itself, recorded with its value when the head exposes a plain function — see the machine's eval arm for why the decision lives here.
    Head {
        params: Vec<Term>,
        plicities: Vec<Plicity>,
        demand: Demand,
        memo_key: Option<Term>,
    },
    /// A forced recursive application waiting for its member's body to expose a function.
    RecBody {
        params: Vec<Term>,
        folded: Term,
        group: RecGroup,
    },
    /// The restuck test of `force`: a reduct that is still neutral and still mentions the group achieved nothing, and the folded spelling stays the canonical normal form.
    RecGuard {
        folded: Term,
        group: RecGroup,
    },
    /// A match waiting for its scrutinee.
    MatchK {
        motive: Scope<Many>,
        cases: Cases,
        demand: Demand,
    },
    /// A projection waiting for its head.
    ProjK {
        index: usize,
        demand: Demand,
    },
    /// Record the value about to be produced under `key` in the run-scoped memo — pushed under a forced application or member selection, so the second occurrence of the same closed call is one probe. This is the machine's spelling of the amortization the hosts get from their own memos, which the machine's internal steps bypass.
    Memo {
        key: Term,
    },
    /// A `let` run evaluating its bindings left to right; `released` holds the values already in.
    LetK {
        released: Vec<Term>,
        current: Term,
        bindings: Vec<LetBinding>,
        next: usize,
        tail: Scope<Many>,
        demand: Demand,
    },
    Args(Args),
}

/// The [`Reducer`] a [`Finish::Intrinsic`] hands to [`reduce_intrinsic`]: operand lookups are served from the machine's already-computed values, so the fold sees evaluated operands at constant native depth, and anything it conjures beyond them takes the host's ordinary path.
struct Tap<'a, H: Reducer> {
    host: &'a mut H,
    values: &'a HashMap<Term, Term>,
}

impl<H: Reducer> Reducer for Tap<'_, H> {
    fn reduce(&mut self, term: Term) -> Result<Term, ReduceError> {
        match self.values.get(&term) {
            Some(value) => Ok(value.clone()),
            None => self.host.reduce(term),
        }
    }

    fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError> {
        match self.values.get(&term) {
            Some(value) => Ok(value.clone()),
            None => self.host.reduce_forced(term),
        }
    }

    fn spend(&mut self, cost: Cost) -> Result<(), ReduceError> {
        self.host.spend(cost)
    }
}

struct Machine {
    stack: Vec<Frame>,
    peak: usize,
    /// Run-scoped intrinsic reducts, keyed on the term as focused — see [`Finish::Intrinsic`] for the chain it exists to keep linear. Dropped with the run; each insertion is charged before it is made.
    values: HashMap<Term, Term>,
}

impl Machine {
    fn run<H: ClosedHost>(
        mut self,
        host: &mut H,
        term: Term,
        demand: Demand,
    ) -> Result<Term, ReduceError> {
        let mut step = Step::Eval(term, demand);

        loop {
            let outcome = match step {
                Step::Eval(term, demand) => self.eval(host, term, demand),
                Step::Value(value) => match self.stack.pop() {
                    None => return Ok(value),
                    Some(frame) => self.apply(host, frame, value),
                },
            };

            step = match outcome {
                Ok(next) => next,
                Err(error) => self.catch(error)?,
            };
        }
    }

    /// Resume from an evaluation error at the nearest catching frame, or propagate it.
    ///
    /// The frames between the error site and the catcher are discarded wholesale: the host would have substituted the whole original spelling unreduced, so everything inside it is deferred together. Exhaustion always propagates — the budget never refunds, so there is nothing to resume with.
    fn catch(&mut self, error: ReduceError) -> Result<Step, ReduceError> {
        if error.is_exhausted() {
            return Err(error);
        }

        while let Some(frame) = self.stack.pop() {
            match frame {
                Frame::Args(args) if args.catch => {
                    let original = args.originals[args.next - 1].clone();
                    self.stack.push(Frame::Args(args));
                    return Ok(Step::Value(original));
                }
                // A `let` value the hosts would have substituted unreduced: resume with the released spelling, deferring the error to whatever demands the binding.
                Frame::LetK {
                    released,
                    current,
                    bindings,
                    next,
                    tail,
                    demand,
                } => {
                    let original = current.clone();
                    self.stack.push(Frame::LetK {
                        released,
                        current,
                        bindings,
                        next,
                        tail,
                        demand,
                    });
                    return Ok(Step::Value(original));
                }
                _ => {}
            }
        }

        Err(error)
    }

    /// Push a frame, charging [`machine_frame`] when it deepens the stack past its previous peak — the machine-frame reading of the rule [`Cost::FRAME`] states for native levels.
    fn push<H: Reducer>(&mut self, host: &mut H, frame: Frame) -> Result<(), ReduceError> {
        if self.stack.len() + 1 > self.peak {
            self.peak = self.stack.len() + 1;
            host.spend(machine_frame())?;
        }

        self.stack.push(frame);
        Ok(())
    }

    fn eval<H: ClosedHost>(
        &mut self,
        host: &mut H,
        term: Term,
        demand: Demand,
    ) -> Result<Step, ReduceError> {
        host.spend(Cost::STEP)?;

        #[cfg(test)]
        if std::env::var_os("CURIOS_MACHINE_TRACE").is_some() {
            eprintln!("eval depth={} {}", self.stack.len(), term);
        }

        // A projection is the fixed point of `rec` unfolding: opening its tail yields the same term, so at a plain demand it *is* its own weak-head form.
        //
        // Decided from the shape, ahead of the memo, because a projection is the one key whose value differs by demand and the memo does not tag demands. The forced arm below records the member's body under the projection, so a run that forced a bare selection and then asked a plain demand for it — a `let` value at an intrinsic operand, then an ordinary call on the same member — was served that body, and the machine unfolded a recursive call the strategy leaves folded. The guard on [`Frame::Memo`] does not cover this: it tests the recorded *value* for a folded spelling, and here it is the *key* that is one.
        if matches!(demand, Demand::Whnf) && term.as_rec_proj().is_some() {
            return Ok(Step::Value(term));
        }

        if let Some(value) = self.values.get(&term) {
            return Ok(Step::Value(value.clone()));
        }

        // The forced demand steps into the member's body, and the value is remembered under the projection so a recursive member's body is opened and eta-probed once per run rather than once per call.
        if let Some((group, index)) = term.as_rec_proj() {
            let body = group.member_body(index);
            let key = term.clone();
            self.push(host, Frame::Memo { key })?;

            return Ok(Step::Eval(body, Demand::Forced));
        }

        match Term::unwrap_or_clone(term) {
            Subterm::Var(var) => match host.closed_body(var.unwrap()) {
                Some(body) => Ok(Step::Eval(body.clone(), demand)),
                None => Ok(Step::Value(Term::var(var))),
            },

            aps @ Subterm::Apply(_) => {
                // The application is remembered whole so that, if its head turns out to be a plain function, its value can be recorded under it — the same closed call, `step` at the same byte and state, then evaluates once per run and probes thereafter. Whether to record is the head frame's decision, because only there is the head known: a member selection's calls never repeat within a run (their fold argument strictly shrinks), and a folded spelling must never be served to a demand that would unfold it.
                let key = Term::from(aps);
                let Subterm::Apply(Apply {
                    head,
                    params,
                    plicities,
                }) = Term::unwrap_or_clone(key.clone())
                else {
                    unreachable!("rebuilt from the same value")
                };
                self.push(
                    host,
                    Frame::Head {
                        params,
                        plicities,
                        demand,
                        memo_key: Some(key),
                    },
                )?;
                Ok(Step::Eval(head, Demand::Whnf))
            }

            Subterm::Match(Match {
                head,
                motive,
                cases,
            }) => {
                self.push(
                    host,
                    Frame::MatchK {
                        motive,
                        cases,
                        demand,
                    },
                )?;
                Ok(Step::Eval(head, Demand::Forced))
            }

            Subterm::Proj(Proj { head, field }) => match field {
                Field::Index(index) => {
                    self.push(host, Frame::ProjK { index, demand })?;
                    Ok(Step::Eval(head, Demand::Forced))
                }
                // A label that survived to reduction has no positional meaning yet and stays stuck, exactly as it does in both hosts.
                field => Ok(Step::Value(Term::from(Subterm::Proj(Proj { head, field })))),
            },

            Subterm::Let(Let { bindings, tail }) => {
                // The kernel's zeta charge: one values vector, a ref vector per binding (triangular), and one substitution pass per binding.
                let count = bindings.len() as u64;
                host.spend(
                    Cost::collection(count)
                        .saturating_add(Cost::buffer(
                            count.saturating_mul(count.saturating_add(1)) / 2,
                        ))
                        .saturating_add(Cost::term(1).saturating_mul(count)),
                )?;

                self.let_advance(
                    host,
                    Vec::with_capacity(bindings.len()),
                    bindings,
                    0,
                    tail,
                    demand,
                )
            }

            Subterm::Rec(rec) => match demand {
                // A `rec` block is weak-head normal; only a demand for its value opens the tail.
                Demand::Whnf => Ok(Step::Value(Term::from(Subterm::Rec(rec)))),
                Demand::Forced => Ok(Step::Eval(unfold_rec(rec), Demand::Forced)),
            },

            Subterm::Intrinsic(intrinsic) => {
                let subterm = Subterm::Intrinsic(intrinsic);
                let mut operands = Vec::new();
                subterm.any_child_term(&mut |child| {
                    operands.push(child.clone());
                    false
                });
                let Subterm::Intrinsic(intrinsic) = subterm else {
                    unreachable!("rebuilt from the same value")
                };

                let count = operands.len();
                self.args(
                    host,
                    Args {
                        originals: operands,
                        done: Vec::with_capacity(count),
                        next: 0,
                        // The hosts' intrinsic folds force their operands, so operand errors surface here exactly as they do there.
                        item_demand: Demand::Forced,
                        catch: false,
                        finish: Finish::Intrinsic {
                            key: Term::from(Subterm::Intrinsic(intrinsic.clone())),
                            intrinsic,
                        },
                    },
                )
            }

            Subterm::Foreign(function, operands) => {
                host.spend(Cost::collection(operands.len() as u64))?;

                let count = operands.len();
                self.args(
                    host,
                    Args {
                        originals: operands,
                        done: Vec::with_capacity(count),
                        next: 0,
                        item_demand: Demand::Whnf,
                        catch: false,
                        finish: Finish::Foreign { function },
                    },
                )
            }

            Subterm::UniverseInst(instance) => {
                let reduct = match &*instance.head {
                    Subterm::Var(var) => host.closed_body_at(var.unwrap()).cloned(),
                    _ => Some(instance.head.clone()),
                };
                let Some(reduct) = reduct else {
                    return Ok(Step::Value(Term::universe_inst(
                        instance.head,
                        instance.levels,
                    )));
                };

                let levels = instance.levels;
                let reduct = match reduct.as_rec_proj() {
                    Some((group, index)) => Term::rec_proj(
                        group
                            .instantiate_universes(&levels)
                            .map_err(ReduceError::Universe)?,
                        index,
                    ),
                    None => instantiate_universe_levels_scoped(&reduct, &levels)
                        .map_err(ReduceError::Universe)?,
                };

                Ok(Step::Eval(reduct, demand))
            }

            // The entry gate excludes metavariables, but a definition *body* the machine unfolds mid-run may still carry solved ones — the elaborator zonks its store at module boundaries, not per definition. Resolution is the host strategy's judgment, so the node bails to it; a metavariable the host leaves standing is genuinely unsolved and is its own normal form, as it is in both hosts.
            metavar @ Subterm::Metavar(_) => {
                let resolved = host.reduce(Term::from(metavar))?;

                match &*resolved {
                    Subterm::Metavar(_) => Ok(Step::Value(resolved)),
                    _ => Ok(Step::Eval(resolved, demand)),
                }
            }

            // Eta for functions, exactly the hosts' probe: `(x) => f(x)` is `f` provided `f` mentions none of the binders. This is not a normalization nicety — the elaborator's witness keying reads rigid heads off weak-head forms, so `(A) => Async(A)` must contract to `Async` here as it does under the strategy.
            Subterm::Func(func) => {
                let arity = func.telescope.len();
                host.spend(
                    Cost::collection(arity as u64)
                        .saturating_mul(3)
                        .saturating_add(Cost::term(1).saturating_mul(arity as u64)),
                )?;

                let binders = (0..arity)
                    .map(|_| host.fresh_binder(None))
                    .collect::<Vec<_>>();
                let occurrences = binders.iter().map(Term::free_var).collect::<Vec<_>>();
                let refs = occurrences.iter().collect::<Vec<_>>();

                match Term::unwrap_or_clone(func.telescope.open(&refs)) {
                    Subterm::Apply(Apply { head, params, .. })
                        if params.len() == arity
                            && params.iter().enumerate().all(|(index, param)| {
                                matches!(param.as_ref(), Subterm::Var(var) if var.unwrap() == &binders[index])
                            })
                            && binders
                                .iter()
                                .all(|binder| !head.free_vars().contains(binder)) =>
                    {
                        Ok(Step::Eval(head, demand))
                    }
                    _ => {
                        // Remember the probe's negative outcome under the function itself, so a body reached again — a plain definition's, unfolded at every call — pays the eta probe once per run.
                        let value = Term::from(Subterm::Func(func));
                        host.spend(Cost::buffer(2))?;
                        self.values.insert(value.clone(), value.clone());
                        Ok(Step::Value(value))
                    }
                }
            }

            // Every remaining former — sorts, function and tuple types, tuples, structs, variants, inductive types, transients — is weak-head normal with its sub-terms unentered, exactly as in both hosts.
            value => Ok(Step::Value(Term::from(value))),
        }
    }

    fn apply<H: ClosedHost>(
        &mut self,
        host: &mut H,
        frame: Frame,
        value: Term,
    ) -> Result<Step, ReduceError> {
        match frame {
            Frame::Head {
                params,
                plicities,
                demand,
                memo_key,
            } => {
                if value.as_rec_proj().is_some() {
                    return match demand {
                        // A folded recursive application is the canonical weak-head normal form.
                        Demand::Whnf => Ok(Step::Value(Term::from(Subterm::Apply(Apply {
                            head: value,
                            params,
                            plicities,
                        })))),
                        Demand::Forced => {
                            let (group, index) = value.as_rec_proj().expect("checked above");
                            let (group, index) = (group.clone(), index);
                            let folded = Term::from(Subterm::Apply(Apply {
                                head: value,
                                params: params.clone(),
                                plicities: plicities.clone(),
                            }));
                            let body = group.member_body(index);
                            self.push(
                                host,
                                Frame::RecBody {
                                    params,
                                    folded,
                                    group,
                                },
                            )?;
                            Ok(Step::Eval(body, Demand::Forced))
                        }
                    };
                }

                match Term::unwrap_or_clone(value) {
                    // `expose_rec_tail`: a `rec` block at an application head strips to its tail even under plain reduction.
                    Subterm::Rec(rec) => {
                        self.push(
                            host,
                            Frame::Head {
                                params,
                                plicities,
                                demand,
                                memo_key,
                            },
                        )?;
                        Ok(Step::Eval(unfold_rec(rec), Demand::Whnf))
                    }
                    Subterm::Func(Func { telescope, .. }) if telescope.len() == params.len() => {
                        if let Some(key) = memo_key {
                            self.push(host, Frame::Memo { key })?;
                        }
                        self.beta(host, telescope, params, demand)
                    }
                    head => Ok(Step::Value(Term::from(Subterm::Apply(Apply {
                        head: head.into(),
                        params,
                        plicities,
                    })))),
                }
            }

            Frame::RecBody {
                params,
                folded,
                group,
            } => match Term::unwrap_or_clone(value) {
                Subterm::Func(Func { telescope, .. }) if telescope.len() == params.len() => {
                    self.push(host, Frame::RecGuard { folded, group })?;
                    self.beta(host, telescope, params, Demand::Forced)
                }
                // The member's body did not expose a saturated function: the folded spelling stays the normal form, exactly as `force` keeps it.
                _ => Ok(Step::Value(folded)),
            },

            Frame::RecGuard { folded, group } => {
                let neutral = matches!(
                    &*value,
                    Subterm::Match(_) | Subterm::Var(_) | Subterm::Metavar(_) | Subterm::Proj(_)
                );

                match neutral && value.mentions_rec_member(&group) {
                    true => Ok(Step::Value(folded)),
                    false => Ok(Step::Value(value)),
                }
            }

            Frame::MatchK {
                motive,
                cases,
                demand,
            } => self.dispatch(host, motive, cases, value, demand),

            Frame::ProjK { index, demand } => match Term::unwrap_or_clone(value) {
                Subterm::Tuple(tuple) if index < tuple.fields.len() => Ok(Step::Eval(
                    tuple
                        .fields
                        .into_iter()
                        .nth(index)
                        .expect("index bounded above"),
                    demand,
                )),
                // The flat runtime view of a constructor value: field `i + 1` is payload component `i`.
                Subterm::Variant(ctor) if (1..=ctor.payload.len()).contains(&index) => {
                    Ok(Step::Eval(
                        ctor.payload
                            .into_iter()
                            .nth(index - 1)
                            .expect("index bounded above"),
                        demand,
                    ))
                }
                Subterm::Struct(struct_) if index < struct_.fields.len() => Ok(Step::Eval(
                    struct_
                        .fields
                        .into_iter()
                        .nth(index)
                        .expect("index bounded above"),
                    demand,
                )),
                head => Ok(Step::Value(Term::proj(Term::from(head), index))),
            },

            Frame::LetK {
                mut released,
                current: _,
                bindings,
                next,
                tail,
                demand,
            } => {
                released.push(value);
                self.let_advance(host, released, bindings, next, tail, demand)
            }

            Frame::Memo { key } => {
                // A folded recursive spelling is a weak-head value that a *forced* probe must not be served, and the memo does not tag demands — so a rec-shaped value is simply not recorded. Every other weak-head value is its own forced form, since forcing only unfolds recursive heads.
                let folded = value.as_rec_proj().is_some()
                    || matches!(&*value, Subterm::Rec(_))
                    || matches!(&*value, Subterm::Apply(apply) if apply.head.as_rec_proj().is_some());

                if !folded {
                    // One entry of two `Term` handles, on the buffer row, charged before the write.
                    host.spend(Cost::buffer(2))?;
                    self.values.insert(key, value.clone());
                }

                Ok(Step::Value(value))
            }

            Frame::Args(mut args) => {
                args.done.push(value);
                self.args(host, args)
            }
        }
    }

    /// Charge and perform a beta step: open `telescope` over the *values* of `params`, evaluated left to right with the error fallback substitution positions get.
    fn beta<H: ClosedHost>(
        &mut self,
        host: &mut H,
        telescope: Telescope<Term>,
        params: Vec<Term>,
        demand: Demand,
    ) -> Result<Step, ReduceError> {
        host.spend(
            Cost::collection(params.len() as u64)
                .saturating_add(Cost::term(1).saturating_mul(params.len() as u64)),
        )?;

        let count = params.len();
        self.args(
            host,
            Args {
                originals: params,
                done: Vec::with_capacity(count),
                next: 0,
                item_demand: Demand::Whnf,
                catch: true,
                finish: Finish::Beta { telescope, demand },
            },
        )
    }

    /// Advance an [`Args`] run: evaluate the next pending term, or finish over the collected values.
    fn args<H: ClosedHost>(&mut self, host: &mut H, mut args: Args) -> Result<Step, ReduceError> {
        if args.next < args.originals.len() {
            let next = args.originals[args.next].clone();
            let demand = args.item_demand;
            args.next += 1;
            self.push(host, Frame::Args(args))?;
            return Ok(Step::Eval(next, demand));
        }

        match args.finish {
            Finish::Beta { telescope, demand } => {
                let refs = args.done.iter().collect::<Vec<_>>();
                Ok(Step::Eval(telescope.open(&refs), demand))
            }
            Finish::Arm { body, demand } => {
                let refs = args.done.iter().collect::<Vec<_>>();
                Ok(Step::Eval(body.open(&refs), demand))
            }
            Finish::Intrinsic { key, intrinsic } => {
                // Safety: keyed on `Term`, whose `OnceCell` scalar caches trip Clippy's interior-mutability warning. The logical value is immutable, and hashing and equality stay stable across those caches filling.
                #[allow(clippy::mutable_key_type)]
                let values = args
                    .originals
                    .iter()
                    .cloned()
                    .zip(args.done.iter().cloned())
                    .collect::<HashMap<_, _>>();
                let mut tap = Tap {
                    host,
                    values: &values,
                };

                let value = Term::from(reduce_intrinsic(&mut tap, &intrinsic)?);
                // Two entries of two `Term` handles each, on the buffer row, charged before they are made. The self-entry is what makes a *rebuilt* neutral — a chain link — recognizable as already evaluated when it comes back around as somebody's operand.
                host.spend(Cost::buffer(4))?;
                self.values.insert(value.clone(), value.clone());
                self.values.insert(key, value.clone());

                Ok(Step::Value(value))
            }
            Finish::Foreign { function } => Ok(Step::Value(Term::from(Subterm::Foreign(
                function, args.done,
            )))),
        }
    }

    /// Advance a `let` run: release the next binding against the values before it and evaluate it, or open the tail over all of them.
    fn let_advance<H: ClosedHost>(
        &mut self,
        host: &mut H,
        released: Vec<Term>,
        bindings: Vec<LetBinding>,
        next: usize,
        tail: Scope<Many>,
        demand: Demand,
    ) -> Result<Step, ReduceError> {
        if next < bindings.len() {
            let refs = released.iter().collect::<Vec<_>>();
            let current = bindings[next].value().release(&refs);
            self.push(
                host,
                Frame::LetK {
                    released,
                    current: current.clone(),
                    bindings,
                    next: next + 1,
                    tail,
                    demand,
                },
            )?;
            return Ok(Step::Eval(current, Demand::Whnf));
        }

        let refs = released.iter().collect::<Vec<_>>();
        Ok(Step::Eval(tail.open(&refs), demand))
    }

    /// Iota: dispatch a match on its scrutinee's value, one arm per [`Cases`] family, mirroring the hosts' rules with payload values bound directly.
    fn dispatch<H: ClosedHost>(
        &mut self,
        host: &mut H,
        motive: Scope<Many>,
        cases: Cases,
        scrutinee: Term,
        demand: Demand,
    ) -> Result<Step, ReduceError> {
        match cases {
            Cases::Bool {
                false_case,
                true_case,
            } => match scrutinee.as_bool() {
                Some(false) => Ok(Step::Eval(false_case, demand)),
                Some(true) => Ok(Step::Eval(true_case, demand)),
                None => Ok(Step::Value(Term::from(Subterm::Match(Match {
                    head: scrutinee,
                    motive,
                    cases: Cases::Bool {
                        false_case,
                        true_case,
                    },
                })))),
            },

            Cases::Switch { cases, default } => {
                let (value, inner) = Nat::decompose(&scrutinee);

                match Nat::is_zero(&inner) {
                    true => Ok(Step::Eval(
                        value
                            .to_u32()
                            .and_then(|key| cases.get(&key))
                            .unwrap_or(&default)
                            .clone(),
                        demand,
                    )),
                    false => Ok(Step::Value(Term::from(Subterm::Match(Match {
                        head: scrutinee,
                        motive,
                        cases: Cases::Switch { cases, default },
                    })))),
                }
            }

            Cases::Induct { cases, default } => {
                if let Subterm::Variant(ctor) = &*scrutinee {
                    if let Some((_, arm)) = cases
                        .iter()
                        .find(|(candidate, _)| candidate == &ctor.tag)
                        .filter(|(_, arm)| arm.arity() == ctor.payload.len())
                    {
                        return self.args(
                            host,
                            Args {
                                originals: ctor.payload.clone(),
                                done: Vec::with_capacity(ctor.payload.len()),
                                next: 0,
                                item_demand: Demand::Whnf,
                                catch: true,
                                finish: Finish::Arm {
                                    body: arm.body.clone(),
                                    demand,
                                },
                            },
                        );
                    }

                    if let Some(default) = default {
                        return Ok(Step::Eval(default, demand));
                    }
                }

                Ok(Step::Value(Term::from(Subterm::Match(Match {
                    head: scrutinee,
                    motive,
                    cases: Cases::Induct { cases, default },
                }))))
            }

            Cases::FreeMonoid { carrier } => {
                let layer = match &carrier {
                    Carrier::Nat { .. } => FreeMonoid::Unary,
                    Carrier::Bin { grain, .. } => FreeMonoid::Bin(*grain),
                    Carrier::List { .. } => FreeMonoid::List,
                }
                .uncons(Term::unwrap_or_clone(scrutinee));

                match layer {
                    Layer::Empty => Ok(Step::Eval(
                        match carrier {
                            Carrier::Nat { empty_case, .. }
                            | Carrier::Bin { empty_case, .. }
                            | Carrier::List { empty_case, .. } => empty_case,
                        },
                        demand,
                    )),
                    Layer::Cons { head, tail } => {
                        // The induction hypothesis is bound unreduced, exactly as the hosts bind it: evaluating it eagerly would run the whole tail fold whether or not the arm uses it. An arm that does use it demands it through a frame, at machine depth.
                        let hypothesis: Term = Subterm::Match(Match {
                            head: tail.clone(),
                            motive: motive.clone(),
                            cases: Cases::FreeMonoid {
                                carrier: carrier.clone(),
                            },
                        })
                        .into();

                        Ok(Step::Eval(
                            match &carrier {
                                Carrier::Nat { cons_case, .. } => {
                                    cons_case.open(&[&tail, &hypothesis])
                                }
                                Carrier::Bin { cons_case, .. }
                                | Carrier::List { cons_case, .. } => cons_case.open(&[
                                    head.as_ref().expect("a Bin/List cons layer carries a head"),
                                    &tail,
                                    &hypothesis,
                                ]),
                            },
                            demand,
                        ))
                    }
                    Layer::Stuck(stuck) => Ok(Step::Value(Term::from(Subterm::Match(Match {
                        head: stuck.into(),
                        motive,
                        cases: Cases::FreeMonoid { carrier },
                    })))),
                }
            }
        }
    }
}

/// Open a `rec` group's tail over its members — a pure binder operation, shared verbatim with both hosts' spelling of the same step.
fn unfold_rec(rec: Rec) -> Term {
    let members = rec.group.members();
    let refs = members.iter().collect::<Vec<_>>();

    rec.tail.open(&refs)
}

#[cfg(test)]
mod tests;
