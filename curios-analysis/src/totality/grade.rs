//! Grading a call: what its arguments are, measured against the caller's parameters under what the arms enclosing it established.
//!
//! Shared by every walk that finds calls — the elaborator's discovery [`Walk`](super::Walk), and the kernel's own typing walk — so a call is graded by one function whichever walk met it. What a walk contributes is its [`SizeContext`], built as it enters and leaves arms, and the arguments it saw at the call.

use {
    super::{Call, Carriers, Guard, Matrix, Shape, Tag, spine},
    crate::{Env, forceable},
    curios_core::{
        Free, FreeMonoid, Intrinsic, Layer, Nat, Probe, Struct, Subterm, Term, Tuple, Variant,
    },
    curios_num::Natural,
    std::collections::{BTreeMap, BTreeSet},
};

/// Whether shape reading may force `term`.
///
/// [`forceable`] answers the two halves every analysis on this seam shares — whether the head could move, and whether the term is closed enough to reduce. This adds totality's own, and it is the whole of what replaced a step count: **a `rec` head is refused.** The group under analysis is the one whose termination is being decided, and a group already in scope may be legitimately partial — `check_rec_group` demands descent only where a member is erased, so `/std/Async`'s scheduler and `/std/Json/decode` classify `Partial` and are still defined. Unfolding either can spin, and refusing makes the read stop on its own terms instead of by exhausting the reduction budget.
///
/// Measured inert: across the corpus not one of 288 load-bearing unfoldings had a `rec` head, so this refuses nothing that was being read and is a determinism guarantee rather than a restriction.
fn readable(term: &Term) -> bool {
    forceable(term) && !matches!(&**term, Subterm::Rec(_))
}

/// How deep a refinement chain may be expanded.
///
/// Each nested `match` on a binder introduced by an outer arm adds one level — `raw_trimmed` reaches three — so this is generous. It exists only so a pathological or cyclic refinement map cannot loop.
pub(super) const EXPAND_FUEL: usize = 16;

/// What the arms enclosing a position have established about the binders in scope: what each has been refined to, which are known nonzero, and which are constructor payloads.
///
/// Built by whichever walk reaches the position — entered on the way into an arm and left on the way out, so knowledge never escapes the branch that established it — and read by [`grade`] at each call.
#[derive(Default)]
pub struct SizeContext {
    /// What each binder has been refined to by the arms enclosing the position being walked.
    pub(super) refined: BTreeMap<Free, Shape>,
    /// The binders an enclosing arm has established are not zero.
    ///
    /// This is what makes an arithmetic decrease sound rather than merely plausible: `n / k` is below `n` only when `n` is nonzero, and without the guard `rec loop(n : Nat) -> Nat = loop(n / 10)` would be accepted while looping forever at zero.
    pub(super) nonzero: BTreeSet<Free>,
    /// The binders the enclosing inductive arms bound as constructor payloads.
    ///
    /// An application whose head is one of these reads as the payload itself, which is what grades `below(y, r)` below the `intro(x, below)` an arm refined the scrutinee to. A head bound anywhere else — a parameter, a lambda binder — is not in the set and reads as it always did.
    pub(super) payloads: BTreeSet<Free>,
    /// One entry per scope [`SizeContext::enter`] has opened and [`SizeContext::exit`] has yet to close.
    pub(super) scopes: Vec<Undo>,
}

/// What one [`SizeContext::enter`] changed, so its [`SizeContext::exit`] puts back exactly that and nothing else.
///
/// Recorded on the way in rather than recomputed on the way out, because only the entering side can tell the difference that matters: a binder an outer arm had already refined, or already knew nonzero, must be left standing as that outer arm left it.
pub(super) struct Undo {
    /// The refined binder and what it stood for before, `None` when the scope refined nothing.
    refined: Option<(Free, Option<Shape>)>,
    /// The binder this scope added to `nonzero`, `None` when it added none — an already-known binder included.
    nonzero: Option<Free>,
    /// The binders this scope added to `payloads`, empty when it added none.
    payloads: Vec<Free>,
}

impl SizeContext {
    /// Enter an arm that establishes `binder` is `value`, that `nonzero` cannot be zero, and that `payloads` are the constructor's payloads — each where given.
    ///
    /// `value` is read as a shape against the context as it stands before the arm, which is where the value was built. A read the budget refuses opens nothing, so the refusal leaves the context as balanced as it found it.
    pub fn enter<E: Env>(
        &mut self,
        env: &mut E,
        refine: Option<(Free, &Term)>,
        nonzero: Option<Free>,
        payloads: Vec<Free>,
    ) -> Result<(), E::Error> {
        let refine = match refine {
            Some((binder, value)) => {
                let shape = Grader { env, context: self }.shape_of(value)?;
                Some((binder, shape))
            }
            None => None,
        };
        self.open(refine, nonzero, payloads);
        Ok(())
    }

    /// [`SizeContext::enter`] with the refinement already a shape — the discovery walk builds an arm's shape from the binders it opened.
    ///
    /// The three kinds of scoped knowledge open together because they close together: a boolean arm refines its scrutinee *and* rules out zero, an inductive arm refines its scrutinee *and* binds payloads, and one bracket is one thing to keep balanced instead of three.
    pub(super) fn open(
        &mut self,
        refine: Option<(Free, Shape)>,
        nonzero: Option<Free>,
        payloads: Vec<Free>,
    ) {
        let refined = refine.map(|(binder, shape)| {
            let previous = self.refined.insert(binder, shape);
            (binder, previous)
        });
        let nonzero = match nonzero {
            Some(atom) if self.nonzero.insert(atom) => Some(atom),
            _ => None,
        };
        let payloads = payloads
            .into_iter()
            .filter(|binder| self.payloads.insert(*binder))
            .collect();

        self.scopes.push(Undo {
            refined,
            nonzero,
            payloads,
        });
    }

    /// Close the innermost open scope, putting back exactly what its [`SizeContext::enter`] recorded.
    pub fn exit(&mut self) {
        let undo = self
            .scopes
            .pop()
            .expect("every exit is bracketed with its own enter");

        if let Some((binder, previous)) = undo.refined {
            match previous {
                Some(previous) => self.refined.insert(binder, previous),
                None => self.refined.remove(&binder),
            };
        }
        if let Some(atom) = undo.nonzero {
            self.nonzero.remove(&atom);
        }
        for binder in undo.payloads {
            self.payloads.remove(&binder);
        }
    }

    /// Whether every scope entered has been left.
    pub fn is_balanced(&self) -> bool {
        self.scopes.is_empty()
    }

    /// How many scopes are open — a mark [`SizeContext::exit_to`] returns to.
    pub fn depth(&self) -> usize {
        self.scopes.len()
    }

    /// Close every scope opened since `depth` was the [`SizeContext::depth`], innermost first — for a walk that brackets scopes by marks rather than pairing each entry with its exit.
    pub fn exit_to(&mut self, depth: usize) {
        while self.scopes.len() > depth {
            self.exit();
        }
    }
}

/// Grade a call from the member whose parameters are `params` to a member taking `callee_arity` parameters, with `arguments`, under `context`: each argument against each parameter, as strictly smaller, equal, or unrelated.
pub fn grade<E: Env>(
    env: &mut E,
    context: &SizeContext,
    caller: usize,
    params: &[Free],
    callee: usize,
    callee_arity: usize,
    arguments: &[Term],
) -> Result<Call, E::Error> {
    curios_profile::profile!("grade");
    let mut grader = Grader { env, context };
    let mut matrix = Matrix::unknown(params.len(), callee_arity);

    let expanded = params
        .iter()
        .map(|param| grader.expand(param, EXPAND_FUEL))
        .collect::<Vec<_>>();

    for (column, argument) in arguments.iter().enumerate().take(callee_arity) {
        let shape = grader.shape_of(argument)?;
        for (row, parameter) in expanded.iter().enumerate() {
            matrix.set(row, column, shape.against(parameter));
        }
    }

    Ok(Call {
        caller,
        callee,
        matrix,
    })
}

/// The binder a boolean arm taken `taken` rules zero out for, when `head` compares a binder against a literal in a way that does.
pub fn nonzero_by<E: Env>(env: &mut E, head: &Term, taken: bool) -> Result<Option<Free>, E::Error> {
    let context = SizeContext::default();
    let guard = Grader {
        env,
        context: &context,
    }
    .guard(head)?;
    Ok(guard
        .filter(|guard| guard.establishes_nonzero(taken))
        .map(|guard| guard.atom))
}

/// The reader of terms as sizes: a checker to force through, and the context the arms established.
pub(super) struct Grader<'a, E: Env> {
    pub(super) env: &'a mut E,
    pub(super) context: &'a SizeContext,
}

impl<E: Env> Grader<'_, E> {
    /// The value a binder currently stands for, following the refinements the enclosing arms established.
    pub(super) fn expand(&self, var: &Free, fuel: usize) -> Shape {
        if fuel == 0 {
            return Shape::Atom(*var);
        }
        match self.context.refined.get(var) {
            None => Shape::Atom(*var),
            Some(shape) => self.expand_shape(shape, fuel - 1),
        }
    }

    fn expand_shape(&self, shape: &Shape, fuel: usize) -> Shape {
        match shape {
            Shape::Atom(var) => self.expand(var, fuel),
            // Already relative to a binder; expanding that binder would only lose the identity the claim is stated against.
            Shape::Smaller(below) => Shape::Smaller(*below),
            Shape::Opaque => Shape::Opaque,
            Shape::Node(tag, kids) => Shape::Node(
                tag.clone(),
                kids.iter()
                    .map(|kid| self.expand_shape(kid, fuel))
                    .collect(),
            ),
            // Rebuilt through the constructors so a tail that expands into a run of the same carrier merges back into one canonical run.
            Shape::UnaryRun { count, tail } => {
                Shape::unary_run(count.clone(), self.expand_shape(tail, fuel))
            }
            Shape::ElemRun {
                carrier,
                heads,
                tail,
            } => Shape::elem_run(
                *carrier,
                heads
                    .iter()
                    .map(|head| self.expand_shape(head, fuel))
                    .collect(),
                self.expand_shape(tail, fuel),
            ),
        }
    }

    /// Read a term as a constructor tree.
    ///
    /// A constructor, a free-monoid layer, and a recognised arithmetic decrease read directly; everything else goes to [`Grader::unfolded_shape`], which sees through the definitions standing between a term and its shape. That fallback carries most of what the size order knows — an operator resolves a witness, so even `n - 1` reaches here as a projection — and it is not bounded by a step count; see [`readable`] for what bounds it instead.
    pub(super) fn shape_of(&mut self, term: &Term) -> Result<Shape, E::Error> {
        Ok(match &**term {
            Subterm::Var(var) => {
                if let Some(free) = var.as_free() {
                    return Ok(self.expand(free, EXPAND_FUEL));
                }
                Shape::Opaque
            }

            Subterm::Variant(Variant { tag, payload, .. }) => {
                let kids = self.shapes_of(payload)?;
                Shape::Node(Tag::Variant(tag.clone()), kids)
            }

            Subterm::Struct(Struct { name, fields, .. }) => {
                let kids = self.shapes_of(fields)?;
                Shape::Node(Tag::Struct(*name), kids)
            }

            Subterm::Tuple(Tuple { fields, .. }) => {
                let kids = self.shapes_of(fields)?;
                Shape::Node(Tag::Tuple, kids)
            }

            Subterm::Intrinsic(Intrinsic::Bool(value)) => {
                Shape::Node(Tag::Bool(*value), Vec::new())
            }

            Subterm::Intrinsic(Intrinsic::Nat(_)) => self.monoid_shape(FreeMonoid::Unary, term)?,

            Subterm::Intrinsic(
                Intrinsic::Bin(grain, _)
                | Intrinsic::BinAppend { grain, .. }
                | Intrinsic::BinConcat { grain, .. }
                | Intrinsic::BinSlice { grain, .. },
            ) => self.monoid_shape(FreeMonoid::Bin(*grain), term)?,

            Subterm::Intrinsic(
                Intrinsic::List { .. }
                | Intrinsic::ListAppend { .. }
                | Intrinsic::ListConcat { .. }
                | Intrinsic::ListSlice { .. },
            ) => self.monoid_shape(FreeMonoid::List, term)?,

            // Arithmetic descent. Both operations are monotone and floor-like on Core's unbounded `Nat` — `NatDiv` folds through `Natural` division and `NatSub` truncates at zero — so each is below its left operand whenever that operand is nonzero.
            Subterm::Intrinsic(Intrinsic::NatDiv {
                dividend: left,
                divisor: right,
                ..
            }) => self.arithmetic_shape(left, right, &Natural::from(2usize))?,

            Subterm::Intrinsic(Intrinsic::NatSub(left, right)) => {
                self.arithmetic_shape(left, right, &Natural::from(1usize))?
            }

            // An application of a constructor payload reads as the payload it came from: a function-typed payload is a branching node whose children are its applications, so `below(y, r)` grades below `intro(x, below)` for the reason `below` does. The head is read through the same refinement expansion a parameter gets, so a payload bound by a nested pattern reads the same. Any other head — a parameter, a lambda binder, a global — falls through to unfolding, as every application did before.
            Subterm::Apply(_) => {
                let (head, _) = spine(term);
                if let Subterm::Var(var) = &*head
                    && let Some(free) = var.as_free()
                    && self.context.payloads.contains(free)
                {
                    return Ok(self.expand(free, EXPAND_FUEL));
                }
                self.unfolded_shape(term)?
            }

            _ => self.unfolded_shape(term)?,
        })
    }

    /// Each of `terms` read as a shape, in order.
    fn shapes_of(&mut self, terms: &[Term]) -> Result<Vec<Shape>, E::Error> {
        terms.iter().map(|term| self.shape_of(term)).collect()
    }

    /// Read `left op right` as a decrease on the binder `left` stands for.
    ///
    /// `least` is the smallest literal right-hand operand that makes the operation strictly decreasing: `2` for division, because `n / 1` is `n`, and `1` for subtraction, because `n - 0` is `n`. A non-literal operand, an operand below `least`, or a left side that is neither the binder nor already a decrease on one, all read as unread — which is what this term read as before the rule existed.
    fn arithmetic_shape(
        &mut self,
        left: &Term,
        right: &Term,
        least: &Natural,
    ) -> Result<Shape, E::Error> {
        let Some(divisor) = right.as_nat().and_then(|nat| nat.to_natural()) else {
            return Ok(Shape::Opaque);
        };
        if divisor < *least {
            return Ok(Shape::Opaque);
        }
        Ok(match self.shape_of(left)? {
            // `n` itself, and an arm has ruled out zero.
            Shape::Atom(atom) if self.context.nonzero.contains(&atom) => Shape::Smaller(atom),
            // Already below `below`, and these operations never grow: dividing or subtracting again keeps it below.
            Shape::Smaller(below) => Shape::Smaller(below),
            _ => Shape::Opaque,
        })
    }

    /// Decode a whole free-monoid prefix into one packed run over the shape of whatever stops the peel.
    ///
    /// The run mirrors the carrier's own representation instead of re-expanding it: a `Nat`'s successor count is read wholesale off the packed spine, and a `Bin`/`List` prefix is peeled breadth-wise into one head vector. One node per layer would recurse — in construction and in every later comparison — as deep as the literal is large, and a `Nat` literal's value is unbounded by the source that spelled it.
    fn monoid_shape(&mut self, carrier: FreeMonoid, term: &Term) -> Result<Shape, E::Error> {
        let carriers = match carrier {
            FreeMonoid::Unary => {
                return match &**term {
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) => {
                        Ok(Shape::Node(Tag::Empty(Carriers::Unary), Vec::new()))
                    }
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(spine, inner))) => {
                        Ok(Shape::unary_run(spine.clone(), self.shape_of(inner)?))
                    }
                    _ => self.unfolded_shape(term),
                };
            }
            FreeMonoid::Bin(_) => Carriers::Bin,
            FreeMonoid::List => Carriers::List,
        };

        let mut heads = Vec::new();
        let mut rest = Term::unwrap_or_clone(term.clone());
        loop {
            match carrier.uncons(rest) {
                Layer::Empty => {
                    break Ok(Shape::elem_run(
                        carriers,
                        heads,
                        Shape::Node(Tag::Empty(carriers), Vec::new()),
                    ));
                }
                Layer::Cons { head, tail } => {
                    if let Some(head) = head {
                        heads.push(self.shape_of(&head)?);
                    }
                    rest = Term::unwrap_or_clone(tail);
                }
                Layer::Stuck(stuck) => {
                    let stuck = Term::from(stuck);
                    let tail = match heads.is_empty() {
                        // Nothing peeled: the whole term is what stuck, and dispatching it back through `shape_of` would land right here again — force it instead.
                        true => self.unfolded_shape(&stuck)?,
                        // The remainder after a peeled prefix is an arbitrary term — a binder, another literal spelling, an application — and gets the full dispatch, exactly as the tail of every peeled layer did when the layers were nested nodes.
                        false => self.shape_of(&stuck)?,
                    };
                    break Ok(Shape::elem_run(carriers, heads, tail));
                }
            }
        }
    }

    /// Unfold weak-head steps until the term reads as a shape, or stops moving.
    ///
    /// Definitions stand between a term and its constructor shape, and no enumeration of *which* closes the set: measured over the corpus, 206 of 288 load-bearing unfoldings are witness projections (an operator resolves a witness, so `n - 1` arrives as `(w).0(n, 1)`), 11 are `/sys` intrinsic wrappers, and 65 are ordinary definitions like `/big_nat/mul/small` and `/std/Str/step`. Unfolding is uniform over all of them because δ and β preserve meaning: a decrease visible after unfolding is a decrease in the term's value.
    ///
    /// There is no step count. Termination rests on what this pass is handed rather than on a budget: every walk that grades a call has typed the terms it reads before asking — the discovery walk runs after the bodies are checked, and the kernel grades a call once its arguments are — positivity refuses a negative occurrence, and the universe hierarchy refuses `Type : Type` — so a well-typed rec-free term normalizes. [`readable`] keeps `rec` out, and the checker's own reduction budget remains the backstop for anything that still fails to settle: the force is a [`Probe`], so a term with no reading is opaque and the budget's refusal propagates as the analysis's own.
    ///
    /// Removing the count changed no verdict in the corpus, and the reason is worth keeping: [`Env::force`] is a full weak-head normalization, so it walks an entire forwarder chain in one call and the count bounded *re-entries* here rather than unfoldings. Measured, 286 of 288 load-bearing unfoldings re-entered once and none more than twice, against a bound of three. What the removal buys is a stated condition in place of a number, not reach.
    fn unfolded_shape(&mut self, term: &Term) -> Result<Shape, E::Error> {
        if !readable(term) {
            return Ok(Shape::Opaque);
        }
        let Some(reduced) = self.env.force(term).probed()? else {
            return Ok(Shape::Opaque);
        };
        if reduced == *term {
            return Ok(Shape::Opaque);
        }
        self.shape_of(&reduced)
    }

    /// Read a boolean scrutinee as a comparison against a literal.
    ///
    /// Neither spelling arrives as an intrinsic: an operator (`n < 10`) resolves a witness and comes through as a projection out of it, and a named comparison (`Nat/lt(n, 10)`) stays an application of a one-line `/sys` wrapper. The same unfolding [`Grader::shape_of`] uses is what exposes the intrinsic under both, and it is the same [`Probe`].
    pub(super) fn guard(&mut self, head: &Term) -> Result<Option<Guard>, E::Error> {
        if let Some(guard) = Guard::read(head) {
            return Ok(Some(guard));
        }
        if !readable(head) {
            return Ok(None);
        }
        let Some(reduced) = self.env.force(head).probed()? else {
            return Ok(None);
        };
        if reduced == *head {
            return Ok(None);
        }
        self.guard(&reduced)
    }
}
