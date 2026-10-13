//! The eliminators: what a match is typed by, each as the three steps a caller takes it in.
//!
//! An eliminator *resolves* its scrutinee and its result, *enters* a case, which is the frame an arm is typed in and the type it is typed at, and *rebuilds* the match from its arms ([`NominalSplit`], [`BoolSplit`], [`SwitchSplit`], [`NatSplit`], [`SequenceSplit`]). What an arm's body is comes from whoever asked: a match written one level deep hands its lowered arm to be checked ([`elaborate_match`]), and the matrix compiler hands the rows of the case over the columns left (`matrix`). So what an arm assumes is stated once and read by both.

use {
    super::{Context, Error, Misaligned, Mode, align, check, elaborate, expect, stands_at},
    crate::{
        BinderSite, MotiveShape, check_motive, dead_guard, decided_sort_of, hold_prop, is_prop,
        probe_open_sort, quality_of, reduce_with, refine_head, resolved_for_display,
        same_uncommitted, zonk_solved_term_metas,
    },
    curios_analysis::{Invert, retyped, scrutinee_solution, solve_indices, wholly_pinned},
    curios_core::{
        Advance, Arity, Atom, Carrier, Cases, Free, Global, InductArm, InductDecl, InductType,
        Intrinsic, IntrinsicHead, Many, Match, MatchResult, MetavarOrigin, Nat, Quality, Scope,
        Subterm, Telescope, Term, Three, Two, Universes,
    },
    curios_num::{Binary, Grain, Natural},
    curios_utilities::Plicity,
};

/// A match's scrutinee, elaborated once before any arm is: rebuilt, beside its type reduced — what every eliminator reads its carrier off.
pub(super) struct Scrutinee {
    term: Term,
    type_: Term,
}

impl Scrutinee {
    /// `head` elaborated as a scrutinee.
    pub(super) fn read(context: &mut Context, head: &Term) -> Result<Self, Error> {
        let (head, head_type) = elaborate(context, head, Mode::Infer)?;

        // The scrutinee is read with its solved metavariables substituted, and so is the goal an arm is checked at: an arm's goal takes the case where the scrutinee occurs in it (`MatchResult::at`), found by identity, and an operator reaches its concept through a witness metavariable of its own, solved and not yet spliced, so two spellings of one comparison are two terms until both are substituted.
        Ok(Self {
            term: zonk_solved_term_metas(context, &head),
            type_: reduce_with(context, &head_type)?,
        })
    }

    /// The rebuilt scrutinee and its type, required to be the given intrinsic type; any other is refused as `eliminator` refuses it.
    fn of_intrinsic(
        self,
        context: &mut Context,
        eliminator: Eliminator,
        expected: IntrinsicHead,
    ) -> Result<(Term, Term), Error> {
        let Self { term: head, type_ } = self;
        let carrier = match expected {
            IntrinsicHead::Nat => matches!(&*type_, Subterm::Intrinsic(Intrinsic::NatType)),
            IntrinsicHead::Bool => matches!(&*type_, Subterm::Intrinsic(Intrinsic::BoolType)),
            IntrinsicHead::Bin(grain) => {
                matches!(&*type_, Subterm::Intrinsic(Intrinsic::BinType(actual)) if *actual == grain)
            }
        };

        match carrier {
            true => Ok((head, type_)),
            false => Err(refused_scrutinee(
                eliminator,
                resolved_for_display(context, &type_),
            )),
        }
    }
}

/// Which eliminator a split is, by what it refuses a scrutinee as.
#[derive(Debug, Clone, Copy)]
pub(super) enum Eliminator {
    Bool,
    /// The fold over a natural and the dispatch on one alike.
    Nat,
    List,
    Bin(Grain),
    /// `vacuous` where the match writes no arm and no default: what it eliminates is then said to be empty.
    Nominal {
        vacuous: bool,
    },
}

/// The refusal `eliminator` gives a scrutinee of `type_`, which is the type in its display form: an eliminator holds the reduct, which spells a recursive name as its group.
fn refused_scrutinee(eliminator: Eliminator, type_: Term) -> Error {
    match eliminator {
        Eliminator::Bool => Error::not_bool_type(type_),
        Eliminator::Nat => Error::not_nat_type(type_),
        Eliminator::List => Error::not_list_type(type_),
        Eliminator::Bin(grain) => Error::not_bin_type(grain, type_),
        Eliminator::Nominal { vacuous } => Error::not_a_induct_type(type_, vacuous),
    }
}

/// When a match is elaborated in checking mode, solve its motive against the expected type *before* the arms are checked. An omitted motive is a constant scope wrapping a fresh metavar (`Term::match_motive_written`), so `motive.open` is that bare metavar and this pins it to `expected` up front — checking-only arms (tuples, constructors) then see a concrete target instead of an unsolved hole, and a result mentioning an enclosing type variable is taken straight from `expected` rather than inverted out of an arm. For an explicit motive it is the same consistency check that the `Check` turnaround would otherwise run post-hoc on the match's type (`elaborate_subterm`), only earlier.
fn seed_motive(
    context: &mut Context,
    term: &Term,
    motive: &Scope<Many>,
    head: &Term,
    mode: &Mode,
) -> Result<(), Error> {
    if let Mode::Check(expected) = mode {
        expect(context, term, &motive.open(&[head]), expected)?;
    }

    Ok(())
}

/// Resolve the (arity-one) motive of an intrinsic eliminator that keeps a family: a fold whose arm reads its hypothesis, or any intrinsic elimination whose motive is written or inferred — an elided one checked against an expected type is otherwise ambient (`resolve_intrinsic_result`). An elided motive checked against an expected type and matched on a *bare variable* scrutinee is synthesised dependent — abstracting that variable out of the expected type — so each arm checks against the goal specialised at its constructor (`0` / `pred + 1`, `x[]` / `head :: tail`, `false` / `true`, ...) rather than the unspecialised expected a constant motive would leave.
///
/// This complements `solve`'s occurrence abstraction (`convert.rs`), which already derives the dependent motive for a *compound* scrutinee: there the scrutinee is a clean abstraction subject in the motive metavar's spine, whereas a bare variable coincides with its own context binder — a duplicated, non-invertible spine entry that `solve` must leave alone. So anything but an elided-checking-mode-bare-variable match keeps the metavar path verbatim, letting `solve` (or the constant motive) do its job.
fn resolve_intrinsic_motive(
    context: &mut Context,
    head_type: &Term,
    head: &Term,
    motive: &Scope<Many>,
    mode: &Mode,
) -> Result<Scope<Many>, Error> {
    let shape = MotiveShape::Intrinsic(head_type);

    if let (Mode::Check(expected), Subterm::Var(var)) = (mode, &**head)
        && is_elided_motive(motive)
        && let Some(binder) = var.as_free()
    {
        let synthesized = Scope::close(Many(1), &[binder], expected.clone());
        return check_motive(context, &shape, &synthesized);
    }

    check_motive(context, &shape, motive)
}

/// The result of a fold, by the rule every reader of one states — erasure's split-or-fold reading and the kernel's arm rule too: the induction hypothesis is assumed exactly when the arm reads it, and only a family types it, at the motive opened at the tail. An arm that may read it (`may_read_hypothesis`) keeps the family, seeded against the expected type. An arm that cannot is a case split, resolved as a `Bool` elimination is: ambient where the motive is elided and checked, since nothing needs the type a hypothesis would have had.
///
/// The rebuilt motive is what everything below opens: insertion saturates applications during elaboration, and a lowered (under-applied) motive body reaching the reducer would open a telescope at the wrong arity.
fn resolve_fold_result(
    context: &mut Context,
    head_type: &Term,
    head: &Term,
    motive: &Scope<Many>,
    (term, mode): (&Term, &Mode),
    reads_hypothesis: bool,
) -> Result<MatchResult, Error> {
    if !reads_hypothesis {
        let result = resolve_intrinsic_result(context, head_type, head, motive, mode)?;
        if let Some(motive) = result.family() {
            seed_motive(context, term, motive, head, mode)?;
        }
        return Ok(result);
    }

    let motive = resolve_intrinsic_motive(context, head_type, head, motive, mode)?;
    seed_motive(context, term, &motive, head, mode)?;

    Ok(MatchResult::Family(motive))
}

/// Whether `term` holds a written goal.
pub(super) fn holds_goal(term: &Term) -> bool {
    matches!(&**term, Subterm::Metavar(metavar) if metavar.origin == MetavarOrigin::Goal)
        || term.any_child_term(&mut |child| holds_goal(child))
}

/// Whether a fold's written cons arm may read its induction hypothesis, the binder at `index`: it names the binder, or it holds a written goal, which stands for a term not yet written and may name anything in scope. Nothing else reaches the hypothesis without naming it — a metavariable is solved from types, and no type in scope mentions the hypothesis unless the arm does — so an arm that does neither elaborates to one that reads none, which is the occurrence the kernel and erasure read off the elaborated arm.
fn may_read_hypothesis<N: Arity>(arm: &Scope<N>, index: usize) -> bool {
    arm.uses(index) || holds_goal(arm.body())
}

/// One arm of a free-monoid elimination, at its case value: `body` is handed the type the arm is typed at and answers the arm. A case split's has the scrutinee refined to the case, the locals its solution re-types re-assumed, and the result taken there.
///
/// An arm of a fold — an elimination whose cons arm reads its hypothesis — learns nothing of the scrutinee: the fold at the tail is the same arms over the tail, so reduction takes each at every tail of the value, where the scrutinee is no case of it. It is typed at the motive opened at its case value and under nothing else, the kernel's rule (`check_free_monoid`), and a hypothesis about the scrutinee rides in the motive.
fn fold_arm(
    context: &mut Context,
    head: &Term,
    result: &MatchResult,
    value: &Term,
    reads_hypothesis: bool,
    body: impl FnOnce(&mut Context, Term) -> Result<Term, Error>,
) -> Result<Term, Error> {
    if reads_hypothesis {
        return body(context, result.at(head, &[], &[], value));
    }

    open_arm(context, head, value)
        .and_then(|()| {
            retype_locals(context, head, value, Vec::new());
            body(context, result.at(head, &[], &[], value))
        })
        .map_err(|error| from_arm(context, head, value, error))
}

/// Open the arm at `value` of a match on `head` in the frame being elaborated: the frame is told what arm it is ([`Context::open_arm`]), and the scrutinee is refined to the case ([`refine_head`]).
fn open_arm(context: &mut Context, head: &Term, value: &Term) -> Result<(), Error> {
    context.open_arm(head, value);

    refine_head(context, head, value)
}

/// `error`, raised in the arm at `value`, reported as one in a dead arm where a guard says the arm is never taken ([`dead_guard`]).
fn from_arm(context: &mut Context, head: &Term, value: &Term, error: Error) -> Error {
    match dead_guard(context, head, value) {
        Some(dead) => error.in_dead_arm(&dead),
        None => error,
    }
}

/// A `Nat` fold with its scrutinee and its result resolved: `0` and `predecessor + 1`, the second with a hypothesis about the predecessor where its arm reads one.
pub(super) struct NatSplit {
    head: Term,
    result: MatchResult,
    reads_hypothesis: bool,
}

impl NatSplit {
    /// `reads_hypothesis` says the successor arm may read its hypothesis ([`may_read_hypothesis`]), which is what keeps the result a family ([`resolve_fold_result`]).
    pub(super) fn resolve(
        context: &mut Context,
        scrutinee: Scrutinee,
        motive: &Scope<Many>,
        site: (&Term, &Mode),
        reads_hypothesis: bool,
    ) -> Result<Self, Error> {
        let (head, _) = scrutinee.of_intrinsic(context, Eliminator::Nat, IntrinsicHead::Nat)?;
        let result = resolve_fold_result(
            context,
            &Subterm::Intrinsic(Intrinsic::NatType).into(),
            &head,
            motive,
            site,
            reads_hypothesis,
        )?;

        Ok(Self {
            head,
            result,
            reads_hypothesis,
        })
    }

    /// The case `0`: `body` is handed the type the arm is typed at, in the frame it is typed in, and answers the arm.
    ///
    /// A case split's arms have the scrutinee refined to their case (as `Bool`/`Switch` do), so a hypothesis whose type mentions it reduces at the arm's value with no hand-written convoy. A fold's arms learn nothing of it ([`fold_arm`]).
    pub(super) fn zero(
        &self,
        context: &mut Context,
        body: impl FnOnce(&mut Context, Term) -> Result<Term, Error>,
    ) -> Result<Term, Error> {
        let value: Term = Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))).into();

        context.with_frame(|context| {
            fold_arm(
                context,
                &self.head,
                &self.result,
                &value,
                self.reads_hypothesis,
                body,
            )
        })
    }

    /// The case `predecessor + 1`, under the two binders it opens: the hypothesis is assumed exactly where the arm reads it.
    pub(super) fn successor(
        &self,
        context: &mut Context,
        predecessor: &Free,
        hypothesis: &Free,
        body: impl FnOnce(&mut Context, Term) -> Result<Term, Error>,
    ) -> Result<Term, Error> {
        context.with_frame(|context| {
            context.assume(predecessor, &Subterm::Intrinsic(Intrinsic::NatType).into());
            if let Some(motive) = self.result.family().filter(|_| self.reads_hypothesis) {
                context.assume(hypothesis, &motive.open(&[&Term::free_var(predecessor)]));
            }

            let value: Term = Subterm::Intrinsic(Intrinsic::nat_add(
                Term::free_var(predecessor),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(1usize))),
            ))
            .into();

            fold_arm(
                context,
                &self.head,
                &self.result,
                &value,
                self.reads_hypothesis,
                body,
            )
        })
    }

    /// The match's own type: the result at the scrutinee as it stands.
    pub(super) fn result_type(&self) -> Term {
        self.result.of(&self.head, &[])
    }

    /// The match over the two arms, the second closed over the binders it was entered under.
    pub(super) fn rebuild(
        self,
        zero: Term,
        predecessor: &Free,
        hypothesis: &Free,
        successor: Term,
    ) -> (Term, Term) {
        let result_type = self.result.of(&self.head, &[]);
        let rebuilt = Subterm::Match(Match {
            head: self.head,
            result: self.result,
            // `Nat` is the free monoid on one payload-less generator: its cons arm binds just (predecessor, ih), so the carrier is `Nat` and the head is absent.
            cases: Cases::FreeMonoid {
                carrier: Carrier::Nat {
                    empty_case: zero,
                    cons_case: Scope::close(Two, &[predecessor, hypothesis], successor),
                },
            },
        })
        .into();

        (rebuilt, result_type)
    }
}

fn elaborate_nat_match(
    context: &mut Context,
    scrutinee: Scrutinee,
    motive: &Scope<Many>,
    zero_case: &Term,
    succ_case: &Scope<Two>,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let reads_hypothesis = may_read_hypothesis(succ_case, 1);
    let split = NatSplit::resolve(context, scrutinee, motive, (term, &mode), reads_hypothesis)?;
    let zero = split.zero(context, |context, expected| {
        check(context, zero_case, expected)
    })?;

    let predecessor = context.fresh_for(succ_case.hint(0), succ_case.written(0));
    let hypothesis = context.fresh_for(succ_case.hint(1), succ_case.written(1));
    let successor = split.successor(context, &predecessor, &hypothesis, |context, expected| {
        let body = succ_case.open(&[&Term::free_var(&predecessor), &Term::free_var(&hypothesis)]);

        check(context, &body, expected)
    })?;

    Ok(split.rebuild(zero, &predecessor, &hypothesis, successor))
}

/// The sequences a fold splits into the empty one and an atom ahead of a tail.
#[derive(Debug, Clone, Copy)]
pub(super) enum Sequence {
    List,
    Bin(Grain),
}

/// What a sequence's cases are built from: a list's element type, read off the scrutinee's type, or a packed sequence's grain.
enum Atoms {
    List { element: Term },
    Bin { grain: Grain },
}

/// A fold over a list or a packed sequence with its scrutinee and its result resolved: the empty case, and an atom ahead of a tail with a hypothesis about the tail where its arm reads one.
pub(super) struct SequenceSplit {
    head: Term,
    head_type: Term,
    atoms: Atoms,
    result: MatchResult,
    reads_hypothesis: bool,
}

impl SequenceSplit {
    /// `reads_hypothesis` says the nonempty arm may read its hypothesis ([`may_read_hypothesis`]), which is what keeps the result a family ([`resolve_fold_result`]).
    pub(super) fn resolve(
        context: &mut Context,
        sequence: Sequence,
        scrutinee: Scrutinee,
        motive: &Scope<Many>,
        site: (&Term, &Mode),
        reads_hypothesis: bool,
    ) -> Result<Self, Error> {
        let (head, head_type, atoms) = match sequence {
            // `List` carries an element type the eliminator must read off the scrutinee (unlike `Nat`, whose carrier is parameterless) — its type must be `List(elem)`.
            Sequence::List => {
                let Scrutinee {
                    term: head,
                    type_: head_type,
                } = scrutinee;
                let element = match &*head_type {
                    Subterm::Intrinsic(Intrinsic::ListType(element)) => element.clone(),
                    _ => {
                        return Err(refused_scrutinee(
                            Eliminator::List,
                            resolved_for_display(context, &head_type),
                        ));
                    }
                };

                (head, head_type, Atoms::List { element })
            }
            // `Bin` is a parameterless carrier (like `Nat`/`Bool`), so the scrutinee's type is just `Bin` — no element type to read off the head as `List` needs.
            Sequence::Bin(grain) => {
                let (head, head_type) = scrutinee.of_intrinsic(
                    context,
                    Eliminator::Bin(grain),
                    IntrinsicHead::Bin(grain),
                )?;

                (head, head_type, Atoms::Bin { grain })
            }
        };
        let result =
            resolve_fold_result(context, &head_type, &head, motive, site, reads_hypothesis)?;

        Ok(Self {
            head,
            head_type,
            atoms,
            result,
            reads_hypothesis,
        })
    }

    /// The empty case: `body` is handed the type the arm is typed at, in the frame it is typed in, and answers the arm.
    ///
    /// A case split's arms have the scrutinee refined to their case (as `Bool`/`Switch` do), so a hypothesis whose type mentions it reduces at the arm's value with no hand-written convoy. A fold's arms learn nothing of it ([`fold_arm`]).
    pub(super) fn empty(
        &self,
        context: &mut Context,
        body: impl FnOnce(&mut Context, Term) -> Result<Term, Error>,
    ) -> Result<Term, Error> {
        let value: Term = Subterm::Intrinsic(match &self.atoms {
            Atoms::List { element } => Intrinsic::List {
                element: element.clone(),
                items: vec![],
            },
            Atoms::Bin { grain } => Intrinsic::Bin(*grain, Binary::empty()),
        })
        .into();

        context.with_frame(|context| {
            fold_arm(
                context,
                &self.head,
                &self.result,
                &value,
                self.reads_hypothesis,
                body,
            )
        })
    }

    /// The case of an atom ahead of a tail, under the three binders it opens — the atom, the tail and the hypothesis about the tail, which is assumed exactly where the arm reads it.
    pub(super) fn cons(
        &self,
        context: &mut Context,
        [atom, tail, hypothesis]: &[Free; 3],
        body: impl FnOnce(&mut Context, Term) -> Result<Term, Error>,
    ) -> Result<Term, Error> {
        context.with_frame(|context| {
            let (atom_type, value): (Term, Term) = match &self.atoms {
                Atoms::List { element } => {
                    // The cons value `head :: tail`, encoded as the monoid operation on a singleton and the tail (no separate prepend intrinsic).
                    let value = Subterm::Intrinsic(Intrinsic::ListConcat {
                        element: element.clone(),
                        operands: vec![
                            Subterm::Intrinsic(Intrinsic::List {
                                element: element.clone(),
                                items: vec![Term::free_var(atom)],
                            })
                            .into(),
                            Term::free_var(tail),
                        ],
                    });

                    (element.clone(), value.into())
                }
                Atoms::Bin { grain } => {
                    let grain = *grain;
                    let atom_type = Subterm::Intrinsic(match grain {
                        Grain::B => Intrinsic::BoolType,
                        Grain::X => Intrinsic::ByteType,
                    });
                    // The cons value `head :: tail`, encoded as the monoid operation on the singleton `[head]` and the tail. A `Bits`/`Bytes` literal holds only concrete bytes, so the singleton of the symbolic byte `head` is `append(x[], head)` (an atom appended to the empty packed sequence), not a literal run.
                    let singleton: Term = Subterm::Intrinsic(Intrinsic::BinAppend {
                        grain,
                        bin: Subterm::Intrinsic(Intrinsic::Bin(grain, Binary::empty())).into(),
                        element: Term::free_var(atom),
                    })
                    .into();
                    let value = Subterm::Intrinsic(Intrinsic::BinConcat {
                        grain,
                        operands: vec![singleton, Term::free_var(tail)],
                    });

                    (atom_type.into(), value.into())
                }
            };

            context.assume(atom, &atom_type);
            context.assume(tail, &self.head_type);
            if let Some(motive) = self.result.family().filter(|_| self.reads_hypothesis) {
                context.assume(hypothesis, &motive.open(&[&Term::free_var(tail)]));
            }

            fold_arm(
                context,
                &self.head,
                &self.result,
                &value,
                self.reads_hypothesis,
                body,
            )
        })
    }

    /// The match's own type: the result at the scrutinee as it stands.
    pub(super) fn result_type(&self) -> Term {
        self.result.of(&self.head, &[])
    }

    /// The match over the two arms, the second closed over the binders it was entered under.
    pub(super) fn rebuild(
        self,
        empty: Term,
        [atom, tail, hypothesis]: &[Free; 3],
        cons: Term,
    ) -> (Term, Term) {
        let result_type = self.result.of(&self.head, &[]);
        let cons_case = Scope::close(Three, &[atom, tail, hypothesis], cons);
        let carrier = match self.atoms {
            Atoms::List { element } => Carrier::List {
                elem: element,
                empty_case: empty,
                cons_case,
            },
            Atoms::Bin { grain } => Carrier::Bin {
                grain,
                empty_case: empty,
                cons_case,
            },
        };
        let rebuilt = Subterm::Match(Match {
            head: self.head,
            result: self.result,
            cases: Cases::FreeMonoid { carrier },
        })
        .into();

        (rebuilt, result_type)
    }
}

fn elaborate_list_match(
    context: &mut Context,
    scrutinee: Scrutinee,
    motive: &Scope<Many>,
    empty_case: &Term,
    cons_case: &Scope<Three>,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    elaborate_sequence_match(
        context,
        Sequence::List,
        scrutinee,
        motive,
        (empty_case, cons_case),
        (term, mode),
    )
}

fn elaborate_bin_match(
    context: &mut Context,
    grain: Grain,
    scrutinee: Scrutinee,
    motive: &Scope<Many>,
    cases: (&Term, &Scope<Three>),
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    elaborate_sequence_match(
        context,
        Sequence::Bin(grain),
        scrutinee,
        motive,
        cases,
        (term, mode),
    )
}

/// A fold over a list or a packed sequence written one level deep: each arm is its lowered body, checked.
fn elaborate_sequence_match(
    context: &mut Context,
    sequence: Sequence,
    scrutinee: Scrutinee,
    motive: &Scope<Many>,
    (empty_case, cons_case): (&Term, &Scope<Three>),
    (term, mode): (&Term, Mode),
) -> Result<(Term, Term), Error> {
    let reads_hypothesis = may_read_hypothesis(cons_case, 2);
    let split = SequenceSplit::resolve(
        context,
        sequence,
        scrutinee,
        motive,
        (term, &mode),
        reads_hypothesis,
    )?;
    let empty = split.empty(context, |context, expected| {
        check(context, empty_case, expected)
    })?;

    let binders = [
        context.fresh_for(cons_case.hint(0), cons_case.written(0)),
        context.fresh_for(cons_case.hint(1), cons_case.written(1)),
        context.fresh_for(cons_case.hint(2), cons_case.written(2)),
    ];
    let cons = split.cons(context, &binders, |context, expected| {
        let [atom, tail, hypothesis] = &binders;
        let body = cons_case.open(&[
            &Term::free_var(atom),
            &Term::free_var(tail),
            &Term::free_var(hypothesis),
        ]);

        check(context, &body, expected)
    })?;

    Ok(split.rebuild(empty, &binders, cons))
}

/// The result of an elimination whose arms read no induction hypothesis — `Bool`, `Switch`, and a fold's case split (`resolve_fold_result`): an elided motive checked against an expected type takes that type as its ambient result, whatever the scrutinee, and anything else resolves as an intrinsic motive. A fold whose arm reads its hypothesis keeps the family — the hypothesis is typed at the motive opened at the tail.
///
/// An expression scrutinee takes the ambient form as a variable does: each arm checks against the goal with the scrutinee's syntactic occurrences standing for the case (`MatchResult::at`), and the arm's refinement reduces any occurrence the goal reaches only by unfolding. A family solved by `solve`'s occurrence abstraction instead sees the goal as it arrives reduced at its root, and an occurrence spelled through a `let` there escapes the abstraction while the application the refinement is keyed on has been unfolded away, which would leave an arm less than the unabstracted goal gives it.
fn resolve_intrinsic_result(
    context: &mut Context,
    head_type: &Term,
    head: &Term,
    motive: &Scope<Many>,
    mode: &Mode,
) -> Result<MatchResult, Error> {
    if let Mode::Check(expected) = mode
        && is_elided_motive(motive)
    {
        return Ok(MatchResult::Ambient(zonk_solved_term_metas(
            context, expected,
        )));
    }
    resolve_intrinsic_motive(context, head_type, head, motive, mode).map(MatchResult::Family)
}

/// A `Nat` dispatch with its scrutinee and its result resolved: a case for each literal written, and the default any other value takes.
pub(super) struct SwitchSplit {
    head: Term,
    result: MatchResult,
}

impl SwitchSplit {
    pub(super) fn resolve(
        context: &mut Context,
        scrutinee: Scrutinee,
        motive: &Scope<Many>,
        term: &Term,
        mode: &Mode,
    ) -> Result<Self, Error> {
        let (head, _) = scrutinee.of_intrinsic(context, Eliminator::Nat, IntrinsicHead::Nat)?;

        // The *rebuilt* motive throughout, as in [`NatSplit::resolve`], or the ambient goal.
        let result = resolve_intrinsic_result(
            context,
            &Subterm::Intrinsic(Intrinsic::NatType).into(),
            &head,
            motive,
            mode,
        )?;
        if let Some(motive) = result.family() {
            seed_motive(context, term, motive, &head, mode)?;
        }

        Ok(Self { head, result })
    }

    /// The case at the literal `value`: `body` is handed the type the arm is typed at, in the frame it is typed in, and answers the arm.
    pub(super) fn arm(
        &self,
        context: &mut Context,
        value: &Natural,
        body: impl FnOnce(&mut Context, Term) -> Result<Term, Error>,
    ) -> Result<Term, Error> {
        let literal: Term = Subterm::Intrinsic(Intrinsic::Nat(Nat::new(value.clone()))).into();

        context
            .with_frame(|context| {
                open_arm(context, &self.head, &literal)?;
                retype_locals(context, &self.head, &literal, Vec::new());
                body(context, self.result.at(&self.head, &[], &[], &literal))
            })
            .map_err(|error| from_arm(context, &self.head, &literal, error))
    }

    /// What any other value takes. It refines nothing, so it is typed at the result at the scrutinee itself.
    pub(super) fn default(
        &self,
        context: &mut Context,
        body: impl FnOnce(&mut Context, Term) -> Result<Term, Error>,
    ) -> Result<Term, Error> {
        body(context, self.result.of(&self.head, &[]))
    }

    /// The match's own type: the result at the scrutinee as it stands.
    pub(super) fn result_type(&self) -> Term {
        self.result.of(&self.head, &[])
    }

    /// The match over `cases`, which arrive strictly ascending as `Cases::Switch` states them, and the default.
    pub(super) fn rebuild(self, cases: Vec<(Natural, Term)>, default: Term) -> (Term, Term) {
        let result_type = self.result.of(&self.head, &[]);
        let rebuilt = Subterm::Match(Match {
            head: self.head,
            result: self.result,
            cases: Cases::Switch { cases, default },
        })
        .into();

        (rebuilt, result_type)
    }
}

fn elaborate_switch(
    context: &mut Context,
    scrutinee: Scrutinee,
    motive: &Scope<Many>,
    cases: &[(Natural, Term)],
    default: &Term,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let split = SwitchSplit::resolve(context, scrutinee, motive, term, &mode)?;

    // The arms keep the order they arrived in, which `Cases::Switch` states is strictly ascending — elaborating an arm rewrites its body, never its key.
    let mut cases_elaborated = Vec::with_capacity(cases.len());
    for (n, body) in cases {
        let body = split.arm(context, n, |context, expected| {
            check(context, body, expected)
        })?;
        cases_elaborated.push((n.clone(), body));
    }
    let default = split.default(context, |context, expected| {
        check(context, default, expected)
    })?;

    Ok(split.rebuild(cases_elaborated, default))
}

/// Whether a rebuilt match's `goal` is the type it is now checked at, as far as conversion says with nothing committed: a pair that would need a metavariable solved is left to the checked type.
fn kept_goal(context: &mut Context, goal: &Term, expected: &Term) -> Result<bool, Error> {
    same_uncommitted(context, goal, expected).map_err(|error| {
        Error::from_reduce(error, |refusal| {
            Error::convert_exhausted(goal.clone(), expected.clone(), refusal)
        })
    })
}

pub(crate) fn elaborate_match(
    context: &mut Context,
    m: &Match,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // A rebuilt match coming back through re-elaboration at its ambient goal: the goal *is* the expected type it was elaborated against, so it re-elaborates as the elided motive it came from, checked against that goal. Checked at another spelling of that type — the one an arm around it specializes now, where its first elaboration met the scrutinee behind a metavariable not yet substituted — it keeps the goal it holds, so elaborating what elaboration built gives it back: conversion compares two goals with no arm's equation, and a rebuilt copy of a recursive member that differed from the member in a goal's spelling would be another term to it.
    let mode = match (&m.result, mode) {
        (MatchResult::Ambient(goal), Mode::Infer) => Mode::Check(goal.clone()),
        (MatchResult::Ambient(goal), Mode::Check(expected))
            if *goal != expected && kept_goal(context, goal, &expected)? =>
        {
            Mode::Check(goal.clone())
        }
        (_, mode) => mode,
    };

    let scrutinee = Scrutinee::read(context, &m.head)?;

    // A scrutinee whose type is still open is refused by its eliminator as one of any other type is: the term the scrutinee is has completed what it awaits, so nothing read later gives it a carrier.
    context.awaiting(|context| eliminate(context, m, term, mode, scrutinee))
}

/// Elaborate a match over its elaborated scrutinee, by the eliminator its cases name.
fn eliminate(
    context: &mut Context,
    m: &Match,
    term: &Term,
    mode: Mode,
    scrutinee: Scrutinee,
) -> Result<(Term, Term), Error> {
    let Match { result, cases, .. } = m;
    let elided;
    let motive = match result {
        MatchResult::Family(motive) => motive,
        MatchResult::Ambient(_) => {
            elided = Term::match_motive_written(Term::hole(context.mint_metavar()));
            &elided
        }
    };

    match cases {
        Cases::Bool {
            false_case,
            true_case,
        } => elaborate_bool_match(
            context, scrutinee, motive, false_case, true_case, term, mode,
        ),
        Cases::Switch { cases, default } => {
            elaborate_switch(context, scrutinee, motive, cases, default, term, mode)
        }
        // The instance a match states is this elaboration's to write, off the type its scrutinee is elaborated at: one read back is written again.
        Cases::Induct { cases, default, .. } => elaborate_induct_match(
            context,
            InductMatchInput {
                scrutinee,
                motive,
                cases,
                default: default.as_ref(),
                term,
            },
            mode,
        ),
        Cases::FreeMonoid {
            carrier:
                Carrier::Nat {
                    empty_case,
                    cons_case,
                },
        } => elaborate_nat_match(
            context, scrutinee, motive, empty_case, cons_case, term, mode,
        ),
        Cases::FreeMonoid {
            carrier:
                Carrier::List {
                    empty_case,
                    cons_case,
                    ..
                },
        } => elaborate_list_match(
            context, scrutinee, motive, empty_case, cons_case, term, mode,
        ),
        Cases::FreeMonoid {
            carrier:
                Carrier::Bin {
                    grain,
                    empty_case,
                    cons_case,
                },
        } => elaborate_bin_match(
            context,
            *grain,
            scrutinee,
            motive,
            (empty_case, cons_case),
            term,
            mode,
        ),
    }
}

struct InductMatchInput<'a> {
    scrutinee: Scrutinee,
    motive: &'a Scope<Many>,
    cases: &'a [(Atom, InductArm)],
    default: Option<&'a Term>,
    term: &'a Term,
}

/// A `Bool` elimination with its scrutinee and its result resolved: the two cases.
pub(super) struct BoolSplit {
    head: Term,
    result: MatchResult,
}

impl BoolSplit {
    pub(super) fn resolve(
        context: &mut Context,
        scrutinee: Scrutinee,
        motive: &Scope<Many>,
        term: &Term,
        mode: &Mode,
    ) -> Result<Self, Error> {
        let (head, _) = scrutinee.of_intrinsic(context, Eliminator::Bool, IntrinsicHead::Bool)?;

        // The *rebuilt* motive throughout, as in [`NatSplit::resolve`], or the ambient goal.
        let result = resolve_intrinsic_result(
            context,
            &Subterm::Intrinsic(Intrinsic::BoolType).into(),
            &head,
            motive,
            mode,
        )?;
        if let Some(motive) = result.family() {
            seed_motive(context, term, motive, &head, mode)?;
        }

        Ok(Self { head, result })
    }

    /// The case at `value`: `body` is handed the type the arm is typed at, in the frame it is typed in, and answers the arm.
    pub(super) fn arm(
        &self,
        context: &mut Context,
        value: bool,
        body: impl FnOnce(&mut Context, Term) -> Result<Term, Error>,
    ) -> Result<Term, Error> {
        let literal: Term = Subterm::Intrinsic(Intrinsic::Bool(value)).into();

        context
            .with_frame(|context| {
                open_arm(context, &self.head, &literal)?;
                retype_locals(context, &self.head, &literal, Vec::new());
                body(context, self.result.at(&self.head, &[], &[], &literal))
            })
            .map_err(|error| from_arm(context, &self.head, &literal, error))
    }

    /// The match's own type: the result at the scrutinee as it stands.
    pub(super) fn result_type(&self) -> Term {
        self.result.of(&self.head, &[])
    }

    /// The match over the two arms.
    pub(super) fn rebuild(self, false_case: Term, true_case: Term) -> (Term, Term) {
        let result_type = self.result.of(&self.head, &[]);
        let rebuilt = Subterm::Match(Match {
            head: self.head,
            result: self.result,
            cases: Cases::Bool {
                false_case,
                true_case,
            },
        })
        .into();

        (rebuilt, result_type)
    }
}

fn elaborate_bool_match(
    context: &mut Context,
    scrutinee: Scrutinee,
    motive: &Scope<Many>,
    false_case: &Term,
    true_case: &Term,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let split = BoolSplit::resolve(context, scrutinee, motive, term, &mode)?;
    let false_case = split.arm(context, false, |context, expected| {
        check(context, false_case, expected)
    })?;
    let true_case = split.arm(context, true, |context, expected| {
        check(context, true_case, expected)
    })?;

    Ok(split.rebuild(false_case, true_case))
}

/// Whether a proposition may be matched into data: it declares one constructor, and that constructor's index targets pin every payload binder (matching a value against a target recovers the binder, as `Eq`'s `refl(z) : (z, z)` recovers `z`), decided by the walk both checkers share. Occurrence is not determination: `blur(a)` mentions `a` and pins nothing, since `blur` need not be injective. A payload that is a proof is not excused, since the arm would be handed the proof the match was given; and a second constructor the scrutinee's indices rule out is one the kernel counts all the same.
fn singleton_eliminable(context: &mut Context, induct_decl: &InductDecl, params: &[Term]) -> bool {
    let [(tag, _)] = induct_decl.constructors.as_slice() else {
        return false;
    };
    let Some(payload) = induct_decl.instantiate(tag, params) else {
        return false;
    };

    // Open the payload telescope under fresh binders; the terminal's indices are the constructor's targets.
    let mut binders: Vec<Free> = Vec::new();
    let mut cursor = payload.cursor();
    while cursor.entry().is_some() {
        binders.push(cursor.advance_fresh(|hint| context.fresh(hint)));
    }
    let terminal = cursor.body().expect("a cursor past every entry");

    wholly_pinned(&binders, &terminal)
}

/// A nominal elimination with its scrutinee and its result resolved: the family the scrutinee's type is an instance of, at its parameters and its indices, and the result each arm is typed at a case of.
///
/// The intrinsic eliminator's typing rule. Arm binders are typed directly from the constructor's registry telescope instantiated at the scrutinee type's parameters — no projections from a stuck payload — and each arm's written binders are aligned to that telescope's marks, a hidden payload left out bound all the same.
///
/// The motive binds the scrutinee's indices and then the scrutinee, whatever its body does with them: each arm checks against the motive at *that case's* target indices, the whole match types at the scrutinee's *actual* indices, and a catch-all default — which binds nothing and refines no index — checks at the actual indices too.
pub(super) struct NominalSplit {
    head: Term,
    head_type: Term,
    name: Global,
    universes: Universes,
    params: Vec<Term>,
    /// The scrutinee's indices, each reduced where the match stands.
    actual_indices: Vec<Term>,
    /// The family's declaration at the scrutinee's universe levels.
    induct_decl: InductDecl,
    result: MatchResult,
    /// The match's own type: the result at the scrutinee's actual indices and the scrutinee itself.
    result_type: Term,
    /// Whether the scrutinee's family is a proposition.
    proposition: bool,
    /// Whether it is one whose one constructor has every payload pinned: the only kind a match may take into data once it has an arm, and the kind whose arms are written over what their equations state.
    pinned: bool,
}

impl NominalSplit {
    /// `vacuous` says the match writes no arm and no default, which is how a scrutinee of no nominal type is refused.
    pub(super) fn resolve(
        context: &mut Context,
        scrutinee: Scrutinee,
        motive: &Scope<Many>,
        (term, mode): (&Term, &Mode),
        vacuous: bool,
    ) -> Result<Self, Error> {
        let Scrutinee {
            term: head,
            type_: head_type,
        } = scrutinee;

        let (name, universes, params, indices) = match &*head_type {
            Subterm::InductType(InductType {
                name,
                universes,
                params,
                indices,
            }) => (*name, universes.clone(), params.clone(), indices.clone()),
            _ => {
                return Err(refused_scrutinee(
                    Eliminator::Nominal { vacuous },
                    resolved_for_display(context, &head_type),
                ));
            }
        };

        // Reducing the scrutinee type to weak-head normal form leaves its index *arguments* untouched, so an index that is an outer-arm key (`s` refined to `Scan/bad()` by an enclosing match) still reads as the bare variable. Reduce each index in the current (refined) context so inversion sees the forced value and pins arm binders against it, rather than refusing it as a key-shaped index, which refinement handles.
        let actual_indices = indices
            .iter()
            .map(|index| reduce_with(context, index))
            .collect::<Result<Vec<_>, _>>()?;

        let Some(induct_decl) = context.induct_decl(&name).cloned() else {
            return Err(Error::unknown_declaration(name));
        };
        let induct_decl = context.instantiate_induct_decl_at(&induct_decl, &universes)?;

        // Opacity covers every eliminator, including defaults and vacuous or inversion-discharged matches. Check before motives, coverage, or index refinement can reveal representation facts.
        if !induct_decl.rep_public
            && context
                .island()
                .is_some_and(|island| !island.is_within(&induct_decl.module))
        {
            return Err(Error::private_representation(name));
        }

        // The motive abstracts the index telescope instantiated at the scrutinee's actual parameters, then the scrutinee.
        let shape = MotiveShape::Induct {
            name: &name,
            universes: &universes,
            params: &params,
            indices: induct_decl.indices_at(&params),
        };

        // An elided motive checked against an expected type takes that type as its *ambient* result: each arm is checked against the expected type with the scrutinee and its variable indices standing for the arm's case, and the match's result is the expected type itself. That is what a hand-written convoy arranges, with no convoy: the ambient form needs no family to close, so a hypothesis whose type mentions the scrutinee rides along unchanged — see `MatchResult::Ambient`. An expression scrutinee takes it too, its syntactic occurrences standing for the case and the arm's refinement reducing the rest, for the reason `resolve_intrinsic_result` gives. A written motive, and inference mode, are a family, checked or solved.
        let result = match mode {
            Mode::Check(expected) if is_elided_motive(motive) => {
                MatchResult::Ambient(zonk_solved_term_metas(context, expected))
            }
            _ => MatchResult::Family(check_motive(context, &shape, motive)?),
        };

        // The match's own type: the result at the scrutinee's actual indices and the scrutinee itself — opened from the *rebuilt* motive, as in [`NatSplit::resolve`], or the ambient goal as written.
        let result_type = result.of(&head, &actual_indices);

        // The seed (`seed_motive`'s job, generalized over the pattern binders): in checking mode, pin the motive — a bare metavar when elided — to the expected type before the arms are checked. An ambient result is that type already, and the expectation is met by construction.
        if let Mode::Check(expected) = mode {
            expect(context, term, &result_type, expected)?;
        }

        let proposition = is_prop(context, &head_type)?;
        let pinned = proposition && singleton_eliminable(context, &induct_decl, &params);

        Ok(Self {
            head,
            head_type,
            name,
            universes,
            params,
            actual_indices,
            induct_decl,
            result,
            result_type,
            proposition,
            pinned,
        })
    }

    /// The match's own type: the result at the scrutinee's actual indices and the scrutinee itself.
    pub(super) fn result_type(&self) -> Term {
        self.result_type.clone()
    }

    /// The large-elimination guard, asked before an arm is read: a strict proposition may not be eliminated into a relevant (data) result — observing which inhabitant it was would break proof irrelevance. Permitted only by *empty* elimination (no arm, every constructor ruled out) or by the elimination of a family with one constructor whose every payload its indices pin. `armless` says the match writes no arm and `defaulted` that it writes a catch-all.
    ///
    /// Answers whether the result's sort is still undecided, which [`NominalSplit::rebuild`] holds to `Prop` once the arms have decided it.
    pub(super) fn admit(
        &self,
        context: &mut Context,
        armless: bool,
        defaulted: bool,
    ) -> Result<bool, Error> {
        // A match with no arm is vacuous: the coverage check below proves every constructor ruled out at the scrutinee's indices, so nothing is eliminated. Any other is judged by the family's declared constructors, as the kernel judges it, so a second constructor the indices rule out here counts all the same. A catch-all default stands in for more than one constructor — a relevant result branching on an erased scrutinee's tag is exactly what this guard forbids — so it is never permitted here. (A prop-into-prop defaulted match never reaches this branch.)
        let permitted = !defaulted && (armless || self.pinned);
        // Every other match on a proposition answers a proposition and nothing else. A result known for data is refused here, before an arm is read. One whose sort nothing has decided waits for the arms, which decide it where they are typed, and is held to `Prop` after them ([`hold_prop`]): so a match its arms make data is refused as this elimination, and not as a mismatch in an arm.
        Ok(match self.proposition && !permitted {
            false => false,
            true => {
                let quality = quality_of(context, &self.result_type)?;
                probe_open_sort(context, "elaborate_induct_match", quality);
                match quality {
                    Quality::Prop => false,
                    Quality::Meta(_) => true,
                    Quality::Type | Quality::Param(_) => {
                        return Err(eliminated_into_data(context, &self.name, &self.result_type));
                    }
                }
            }
        })
    }

    /// Every written arm must name a constructor: the refusal of the first of `tags` that names none. Coverage is decided per constructor — a missing arm is legal iff inversion proves it impossible ([`NominalSplit::ruled_out`]).
    pub(super) fn declared<'t>(
        &self,
        tags: impl IntoIterator<Item = &'t Atom>,
    ) -> Result<(), Error> {
        let Some(tag) = tags.into_iter().find(|tag| !self.induct_decl.declares(tag)) else {
            return Ok(());
        };

        // Each constructor as a pattern writes it, one placeholder per plain payload — a hidden one is left out — so the report shows what the arm could have named.
        let constructors = self
            .induct_decl
            .constructor_order()
            .map(|constructor| {
                let payload = self
                    .induct_decl
                    .payload_plicities(constructor)
                    .unwrap_or_default()
                    .iter()
                    .filter(|plicity| **plicity == Plicity::Explicit)
                    .map(|_| "_")
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{constructor}({payload})")
            })
            .collect();

        Err(Error::unknown_match_constructor(
            self.name,
            tag.to_string(),
            constructors,
        ))
    }

    /// The family's constructors in the order it declares them, which is the order a match's arms are elaborated and stored in however they are written: two matches differing only in how their arms were written produce the same term.
    pub(super) fn constructors(&self) -> impl Iterator<Item = &Atom> {
        self.induct_decl.constructor_order()
    }

    /// Whether the arm for `tag` may be left out.
    ///
    /// Inversion — checker-verified omission: a missing arm is accepted iff first-order unification of the scrutinee's actual indices against this case's targets finds a *definite* clash, in either direction — the answer a written arm is checked under, so an arm may be left out exactly where the kernel would type none of it. The arm is then pruned (erase fills its slot with an unreachable body); anything short of definite keeps the arm mandatory.
    pub(super) fn ruled_out(&self, context: &mut Context, tag: &Atom) -> Result<bool, Error> {
        // An unindexed inductive has nothing to invert: every arm is reachable and a missing one is plainly missing.
        if self.actual_indices.is_empty() {
            return Ok(false);
        }

        let payload = self.payload(tag);
        let labels = (0..payload.len())
            .map(|_| context.fresh(None))
            .collect::<Vec<_>>();

        // The case is opened as a written arm's is, its binders assumed, because inversion reconciles a binder forced twice *at its type*: `refl(@z) : (z, z)` against `Eq()(false, true)` is decided by what `false` and `true` are at `Bool`, and a binder that is only a name has no type to be asked at.
        let inversion = context.with_frame(|context| {
            let ix_c = assume_payload(context, payload, &labels);
            solve_indices(context, &self.actual_indices, &ix_c, &labels)
        })?;

        Ok(matches!(inversion, Invert::Impossible))
    }

    /// The refusal of a match that leaves out the arm for `tag` where nothing rules the case out.
    pub(super) fn missing(&self, tag: &Atom) -> Error {
        match self.actual_indices.is_empty() {
            true => Error::match_case_missing(self.name, tag.to_string()),
            false => Error::missing_arm_not_impossible(tag.clone()),
        }
    }

    /// The payload of `tag`'s case, at the scrutinee's parameters.
    pub(super) fn payload(&self, tag: &Atom) -> Telescope<Vec<Term>> {
        self.induct_decl
            .instantiate(tag, &self.params)
            .expect("constructor instantiates at its inductive's parameters")
    }

    /// The case of `tag`, whose `payload` is opened under `labels`, one for each of its binders, each assumed at its declared (dependent) type: `body` is handed the type the arm is typed at, in the frame it is typed in, and answers the arm, which is closed over every payload.
    pub(super) fn arm(
        &self,
        context: &mut Context,
        tag: &Atom,
        payload: Telescope<Vec<Term>>,
        labels: &[Free],
        body: impl FnOnce(&mut Context, Term) -> Result<Term, Error>,
    ) -> Result<(Atom, InductArm), Error> {
        let arity = payload.len();
        let payload_plicities = payload.marks();
        let payload_proofs = payload.proofs();
        let vars = labels.iter().map(Term::free_var).collect::<Vec<_>>();

        // Refinement propagates `head := ctor_val` to other occurrences of the scrutinee in the arm body; the binder types themselves came from the telescope below. Built at the scrutinee's own universe levels, because this value outlives the refinement: the motive is opened on it, so it is what a metavariable in an arm's expected type is solved to — the `@z` of an `Eq/refl()` against `Eq()(len(xs), len(xs))` — and a level-less occurrence of a polymorphic family zonks into the definition, where the arity check (or, for a prelude family it cannot see, the kernel) refuses it.
        let ctor_val = Term::variant_at(
            self.name,
            self.universes.clone(),
            self.params.clone(),
            tag.clone(),
            vars,
        );

        // Why the arm is never taken, where its case's indices clash with the scrutinee's.
        let mut ruled_out = None;
        let (body, held) = context
            .with_frame(|context| {
                let ix_c = assume_payload(context, payload, labels);
                open_arm(context, &self.head, &ctor_val)?;

                // The index equations: the most-general solution of `actual indices ~ case targets`, both directions — arm binders pinned to the actuals they must equal, outer variables refined to the targets they must equal — by the kernel's own function, and recorded as the same frame-scoped refinements. A definite clash means the arm is unreachable; it was written, so it is simply checked as is, with nothing solved, and an error it raises is reported as one in an arm that may be left out. Refinements never justify the typing (the motive application does); they are convertibility aids, so the body's occurrences of a solved variable reduce at the arm's indices.
                let solutions = match solve_indices(context, &self.actual_indices, &ix_c, labels)? {
                    Invert::Solved(solutions) => solutions,
                    Invert::Impossible => {
                        let case = Term::induct_type_at(
                            self.name,
                            self.universes.clone(),
                            self.params.clone(),
                            ix_c.clone(),
                        );
                        let scrutinee = Term::induct_type_at(
                            self.name,
                            self.universes.clone(),
                            self.params.clone(),
                            self.actual_indices.clone(),
                        );
                        ruled_out = Some(context.rule_out_arm(case, scrutinee));

                        Vec::new()
                    }
                };
                for (name, solution) in &solutions {
                    context.refine(name, solution);
                }

                // The result at this case: a family's index binders take the case's target indices and its scrutinee binder the constructed value; an ambient goal has the case's targets and value substituted for its variable indices and scrutinee.
                let expected = self
                    .result
                    .at(&self.head, &self.actual_indices, &ix_c, &ctor_val);
                // What the equations hold each payload binder to, kept for the arm that is rebuilt below.
                let held = solutions
                    .iter()
                    .filter(|(name, _)| labels.contains(name))
                    .cloned()
                    .collect::<Vec<_>>();
                retype_locals(context, &self.head, &ctor_val, solutions);

                body(context, expected).map(|body| (body, held))
            })
            .map_err(|error| match &ruled_out {
                Some(dead) => error.in_dead_arm(dead),
                None => from_arm(context, &self.head, &ctor_val, error),
            })?;

        // A match on a proposition whose one constructor is wholly pinned writes a payload binder as what the arm's equations hold it to, which is the index that pins it wherever the scrutinee's indices unify with the constructor's. An arm written so names no payload, and on a closed term the match is its body with the proof unreduced. A binder the equations leave open stays named: the arm is typed at it, and reduction binds it to the index when the match fires.
        let body = match self.pinned {
            true => body.substitute(&held),
            false => body,
        };

        let label_refs = labels.iter().collect::<Vec<_>>();
        // Rebuild the arm over every payload under the constructor's canonical marks, so a re-elaborated arm writes each binder and re-checks identically (idempotence). Each binder states what its payload's does of whether it binds a proof, which the constructor's declaration said when it was published.
        Ok((
            tag.clone(),
            InductArm::new(
                Scope::close(Many(arity), &label_refs, body).with_proofs(&payload_proofs),
                payload_plicities,
            ),
        ))
    }

    /// The catch-all: `body` is handed the type it is typed at and answers it. It binds nothing and refines no index, so it is typed at the unrefined head — the result at the actual scrutinee, exactly as [`SwitchSplit::default`] types its default.
    pub(super) fn default(
        &self,
        context: &mut Context,
        body: impl FnOnce(&mut Context, Term) -> Result<Term, Error>,
    ) -> Result<Term, Error> {
        context.with_frame(|context| body(context, self.result_type.clone()))
    }

    /// The match over `cases`, in the order [`NominalSplit::constructors`] gives, and the catch-all where one is written. `undecided` is what [`NominalSplit::admit`] answered, and `term` the match a sort decided here is reported with.
    pub(super) fn rebuild(
        self,
        context: &mut Context,
        cases: Vec<(Atom, InductArm)>,
        default: Option<Term>,
        undecided: bool,
        term: &Term,
    ) -> Result<(Term, Term), Error> {
        if undecided && !hold_prop(context, &self.result_type, term.extent())? {
            return Err(eliminated_into_data(context, &self.name, &self.result_type));
        }

        // A match with one arm over a proposition whose indices pin its payload states the instance it eliminates: reduction fires it on that instance and reads no binder's type, so what it is moves with the term and with nothing else.
        let stated =
            (self.pinned && default.is_none() && cases.len() == 1).then(|| self.head_type.clone());
        let rebuilt = Subterm::Match(Match {
            head: self.head,
            result: self.result,
            cases: Cases::Induct {
                cases,
                default,
                stated,
            },
        })
        .into();

        Ok((rebuilt, self.result_type))
    }
}

/// For each payload of `tag`'s case, the binder among `written` that names it, or `None` where none does.
///
/// An arm's written binders meet the payload's slots as a lambda's meet a function type's ([`align`]): the plain binders are the plain payloads, in order, and between two of them the hidden binders written are the first of their run. A hidden payload no binder was written for is bound all the same, hintless, so nothing in the arm names it and it is in scope as its mark makes it.
pub(super) fn payload_fills(
    tag: &Atom,
    payload: &Telescope<Vec<Term>>,
    written: &[Plicity],
) -> Result<Vec<Option<usize>>, Error> {
    let payload_plicities = payload.marks();
    let plain = |marks: &[Plicity]| {
        marks
            .iter()
            .filter(|mark| **mark == Plicity::Explicit)
            .count()
    };
    align(&payload_plicities, written).map_err(|misaligned| match misaligned {
        // Plain payloads against plain binders: a hidden payload is bound rather than written, so a total would name a count the arm may not write.
        Misaligned::Missing | Misaligned::Extra { .. } => {
            Error::ctor_arity_mismatch(tag.clone(), plain(&payload_plicities), plain(written))
        }
        Misaligned::Mark { member, slot } => Error::hidden_member_out_of_order(
            written[member],
            payload_plicities[slot],
            payload.labels()[slot],
        ),
        // A marked binder with no hidden payload left in its run: at the plain payload the plain binders before it have reached, which no mark binds, or past the last.
        Misaligned::Surplus { member } => match stands_at(&payload_plicities, written, member) {
            Some(slot) => Error::BinderPlicityMismatch {
                site: BinderSite::Payload {
                    constructor: tag.to_string(),
                },
                position: slot + 1,
                binder: String::new(),
                expected: payload_plicities[slot],
                written: written[member],
            },
            None => Error::hidden_member_without_slot(written[member]),
        },
    })
}

/// A nominal match written one level deep: each arm is its lowered body under the binders it writes, checked.
fn elaborate_induct_match(
    context: &mut Context,
    input: InductMatchInput<'_>,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let InductMatchInput {
        scrutinee,
        motive,
        cases,
        default,
        term,
    } = input;

    // A match with no arms is a vacuous elimination — either of an empty inductive (`False`) or of one whose every constructor inversion-clashes at the scrutinee's indices. Such a match compiles to unreachable code that never inspects the scrutinee. A proof/type scrutinee carries no runtime content (sort-driven erasure), so an empty match discharging an erased witness of falsity into a relevant result is sound without a usage discipline.
    let vacuous = cases.is_empty() && default.is_none();
    let split = NominalSplit::resolve(context, scrutinee, motive, (term, &mode), vacuous)?;
    let undecided = split.admit(context, cases.is_empty(), default.is_some())?;
    split.declared(cases.iter().map(|(tag, _)| tag))?;

    // Built by walking the *declaration* order, not the written order, so the elaborated arm sequence is canonical: two matches differing only in how their arms were written produce the same term.
    let mut cases_elaborated = Vec::new();
    for tag in split.constructors() {
        let Some((_, scope)) = cases.iter().find(|(candidate, _)| candidate == tag) else {
            // A catch-all default covers every un-enumerated constructor, so a missing arm needs neither the unindexed-completeness check nor index inversion — the default is checked once, below.
            if default.is_some() || split.ruled_out(context, tag)? {
                continue;
            }

            return Err(split.missing(tag));
        };

        let payload = split.payload(tag);
        let fills = payload_fills(tag, &payload, scope.plicities())?;

        // Open the telescope with fresh names, one per payload: a written binder's under its own label, each binder assumed at its declared (dependent) type.
        let labels = fills
            .iter()
            .map(|fill| match fill {
                Some(member) => {
                    context.fresh_for(scope.body.hint(*member), scope.body.written(*member))
                }
                None => context.fresh(None),
            })
            .collect::<Vec<_>>();

        // The body is closed over the binders the arm wrote, which the alignment matched to their payloads in order.
        let written = labels
            .iter()
            .zip(&fills)
            .filter(|(_, fill)| fill.is_some())
            .map(|(label, _)| Term::free_var(label))
            .collect::<Vec<_>>();
        let written = written.iter().collect::<Vec<_>>();
        cases_elaborated.push(
            split.arm(context, tag, payload, &labels, |context, expected| {
                check(context, &scope.open(&written), expected)
            })?,
        );
    }

    let default = default
        .map(|default| {
            split.default(context, |context, expected| {
                check(context, default, expected)
            })
        })
        .transpose()?;

    split.rebuild(context, cases_elaborated, default, undecided, term)
}

/// The refusal of a match on the proposition `name` into `result_type`, which is no proposition: this elimination's. Where the result stands at a `Sort` the declaration wrote, it is a type's because an earlier term decided so, the reader having written neither sort, and the refusal is that the two terms decide it apart.
fn eliminated_into_data(context: &mut Context, name: &Global, result_type: &Term) -> Error {
    match decided_sort_of(context, result_type) {
        Some(other) => Error::sort_decided_twice(true, other),
        None => Error::large_elim_of_prop(*name),
    }
}

/// Open a case's instantiated telescope under `labels`, each entered at its declared (dependent) type under its payload's mark, and answer the index targets the signature terminates in, stated over those binders. The caller holds the frame the assumptions live in.
///
/// A written arm and an omitted one open their case the same way, so inversion is put the same question about both — which is also the question the kernel puts, its callers reaching the unifier through a payload open that assumes every binder.
fn assume_payload(
    context: &mut Context,
    telescope: Telescope<Vec<Term>>,
    labels: &[Free],
) -> Vec<Term> {
    let mut cursor = telescope.cursor();
    for label in labels {
        let (_, type_) = cursor
            .entry()
            .expect("a case's binders parallel its telescope");
        let mark = cursor
            .mark()
            .expect("a case's binders parallel its telescope");
        context.enter(label, &type_, mark);
        cursor.advance(Term::free_var(label));
    }

    match cursor.body() {
        Some(targets) => targets,
        None => unreachable!("a case's binders parallel its telescope"),
    }
}

/// Re-assume every local a case's solution re-types, at its specialized type: `curios_analysis::retyped` over `solutions` joined by the scrutinee's own (`curios_analysis::scrutinee_solution`), which is the list and the rule the kernel's arm applies. The arm's refinements make such a type *reduce* at the case, which is enough for the body's own conversions, but not for a metavariable born in the arm: it keeps the types of its birth context and checks its solution against them, retried outside the frame — `z : Sizes(s)` used as `(z).0` under `s := node(a, b)` has to be a tuple there, which the re-typed entry states outright.
fn retype_locals(
    context: &mut Context,
    head: &Term,
    value: &Term,
    mut solutions: Vec<(Free, Term)>,
) {
    let value = value.substitute(&solutions);
    if let Some(solution) = scrutinee_solution(&*context, head, &value) {
        solutions.push(solution);
    }

    let locals = context.locals().cloned().collect::<Vec<_>>();
    for (name, type_) in retyped(&*context, &locals, &solutions) {
        context.assume(&name, &type_);
    }
}

/// Whether a motive scope is the lowering's elided form — a bare metavariable body (`match s | ..` or the explicit hole `match s : _ | ..`), as opposed to a user-written constant or scrutinee-binding motive. Only the elided form takes the ambient result; everything else is taken verbatim.
fn is_elided_motive(motive: &Scope<Many>) -> bool {
    // A silent hole only: a written `?` motive is a user-written motive the author is asking about, checked against the eliminator's motive type like any other and reported by zonk (`MetavarOrigin` states the rule).
    matches!(&**motive.body(), Subterm::Metavar(metavar) if metavar.is_hole())
}
