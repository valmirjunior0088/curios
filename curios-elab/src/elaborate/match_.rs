//! The eliminators: what a match is typed by, each as the three steps a caller takes it in.
//!
//! An eliminator *resolves* its scrutinee and its result, *enters* a case, which is the frame an arm is typed in and the type it is typed at, and *rebuilds* the match from its arms ([`NominalSplit`], [`BoolSplit`], [`SwitchSplit`], [`NatSplit`], [`SequenceSplit`]). What an arm's body is comes from whoever asked: a match written one level deep hands its lowered arm to be checked ([`elaborate_match`]), and the matrix compiler hands the rows of the case over the columns left (`matrix`). So what an arm assumes is stated once and read by both.

use {
    super::{Misaligned, Mode, align, elaborate, stands_at},
    crate::{
        BinderSite, Context, Error, check, check_is_sort, dead_guard, decided_sort_of, expect,
        hold_prop, is_prop, pending, probe_open_sort, quality_of, reduce_with, refine_head,
        resolved_for_display, same_uncommitted, stuck_on_metavar, zonk_solved_term_metas,
    },
    curios_analysis::{Invert, retyped, scrutinee_solution, solve_indices, wholly_pinned},
    curios_common::Plicity,
    curios_core::{
        Advance, Atom, Carrier, Cases, Free, Global, InductArm, InductDecl, InductType, Intrinsic,
        IntrinsicHead, Many, Match, Nat, One, Quality, Scope, Subterm, Telescope, Term, Two,
        Universes, goal_at,
    },
    curios_num::{Binary, Natural},
    std::collections::{HashMap, HashSet},
};

/// A match's scrutinee, elaborated once before any arm is: rebuilt, beside its type reduced — what every eliminator reads its carrier off.
pub struct Scrutinee {
    term: Term,
    type_: Term,
}

impl Scrutinee {
    /// `head` elaborated as a scrutinee.
    pub fn read(context: &mut Context, head: &Term) -> Result<Self, Error> {
        let (head, head_type) = elaborate(context, head, Mode::Infer)?;

        // The scrutinee is read with its solved metavariables substituted, and so is the goal an arm is checked at: an arm's goal takes the case where the scrutinee occurs in it (`goal_at`), found by identity, and an operator reaches its concept through a witness metavariable of its own, solved and not yet spliced, so two spellings of one comparison are two terms until both are substituted.
        Ok(Self {
            term: zonk_solved_term_metas(context, &head),
            type_: reduce_with(context, &head_type)?,
        })
    }

    /// The rebuilt scrutinee and its type, required to be the given intrinsic type; any other is refused as `eliminator` refuses it. A type nothing has decided yet, stuck on a metavariable — a written goal's stand-in is one — is held to that type, which the arms name, as a checked term's is.
    fn of_intrinsic(
        self,
        context: &mut Context,
        eliminator: Eliminator,
        expected: IntrinsicHead,
    ) -> Result<(Term, Term), Error> {
        let Self { term: head, type_ } = self;
        if stuck_on_metavar(context, &type_) {
            let carrier: Term = Subterm::Intrinsic(match expected {
                IntrinsicHead::Nat => Intrinsic::NatType,
                IntrinsicHead::Bool => Intrinsic::BoolType,
                IntrinsicHead::Bytes => Intrinsic::BytesType,
            })
            .into();
            expect(context, &head, &type_, &carrier)?;

            return Ok((head, carrier));
        }
        let carrier = match expected {
            IntrinsicHead::Nat => matches!(&*type_, Subterm::Intrinsic(Intrinsic::NatType)),
            IntrinsicHead::Bool => matches!(&*type_, Subterm::Intrinsic(Intrinsic::BoolType)),
            IntrinsicHead::Bytes => {
                matches!(&*type_, Subterm::Intrinsic(Intrinsic::BytesType))
            }
        };

        match carrier {
            true => Ok((head, type_)),
            false => Err(refused_scrutinee(context, eliminator, &type_)),
        }
    }
}

/// Which eliminator a split is, by what it refuses a scrutinee as.
#[derive(Debug, Clone, Copy)]
pub enum Eliminator {
    Bool,
    /// The case split of a natural and the dispatch on one alike.
    Nat,
    List,
    Bytes,
    /// `vacuous` where the match writes no arm and no default: what it eliminates is then said to be empty.
    Nominal {
        vacuous: bool,
    },
}

/// The refusal `eliminator` gives a scrutinee of `type_`, which shows the type in its display form: an eliminator holds the reduct, which spells a recursive name as its group.
fn refused_scrutinee(context: &mut Context, eliminator: Eliminator, type_: &Term) -> Error {
    pending(context, [type_], move |reporting, [type_]| {
        let type_ = resolved_for_display(reporting, &type_);
        match eliminator {
            Eliminator::Bool => Error::not_bool_type(type_),
            Eliminator::Nat => Error::not_nat_type(type_),
            Eliminator::List => Error::not_list_type(type_),
            Eliminator::Bytes => Error::not_bytes_type(type_),
            Eliminator::Nominal { vacuous } => Error::not_a_induct_type(type_, vacuous),
        }
    })
}

/// The goal a match's arms inhabit, `written` being the goal lowering minted or a rebuilt match holds: the expected type where the match is checked, which each arm inhabits at its case with the scrutinee standing for it, and where it is inferred the written goal checked as a type, which the arms then solve. A match whose arms answer types that depend on their case is therefore checked, as a `let` or a signature checks it.
fn resolve_goal(context: &mut Context, written: &Term, mode: &Mode) -> Result<Term, Error> {
    match mode {
        Mode::Check(expected) => Ok(zonk_solved_term_metas(context, expected)),
        Mode::Infer => Ok(check_is_sort(context, written)?.0),
    }
}

/// One arm of a free-monoid case split, at its case value: `body` is handed the type the arm is typed at and answers the arm, with the scrutinee refined to the case, the locals its solution re-types re-assumed, and the result taken there.
fn split_arm(
    context: &mut Context,
    head: &Term,
    goal: &Term,
    value: &Term,
    body: impl FnOnce(&mut Context, Term) -> Result<Term, Error>,
) -> Result<Term, Error> {
    open_arm(context, head, value)
        .and_then(|()| {
            retype_locals(context, head, value, Vec::new());
            body(context, goal_at(goal, head, &[], &[], value))
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

/// A `Nat` case split with its scrutinee and its result resolved: `0` and `predecessor + 1`.
pub struct NatSplit {
    head: Term,
    goal: Term,
}

impl NatSplit {
    pub fn resolve(
        context: &mut Context,
        scrutinee: Scrutinee,
        goal: &Term,
        mode: &Mode,
    ) -> Result<Self, Error> {
        let (head, _) = scrutinee.of_intrinsic(context, Eliminator::Nat, IntrinsicHead::Nat)?;

        let goal = resolve_goal(context, goal, mode)?;

        Ok(Self { head, goal })
    }

    /// The case `0`: `body` is handed the type the arm is typed at, in the frame it is typed in, and answers the arm. The scrutinee is refined to the case (as `Bool`/`Switch` do), so a hypothesis whose type mentions it reduces at the arm's value with no hand-written convoy.
    pub fn zero(
        &self,
        context: &mut Context,
        body: impl FnOnce(&mut Context, Term) -> Result<Term, Error>,
    ) -> Result<Term, Error> {
        let value: Term = Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))).into();

        context.with_frame(|context| split_arm(context, &self.head, &self.goal, &value, body))
    }

    /// The case `predecessor + 1`, under the binder it opens.
    pub fn successor(
        &self,
        context: &mut Context,
        predecessor: &Free,
        body: impl FnOnce(&mut Context, Term) -> Result<Term, Error>,
    ) -> Result<Term, Error> {
        context.with_frame(|context| {
            context.assume(predecessor, &Subterm::Intrinsic(Intrinsic::NatType).into());

            let value: Term = Subterm::Intrinsic(Intrinsic::nat_add(
                Term::free_var(predecessor),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(1usize))),
            ))
            .into();

            split_arm(context, &self.head, &self.goal, &value, body)
        })
    }

    /// The match's own type: the result at the scrutinee as it stands.
    pub fn result_type(&self) -> Term {
        self.goal.clone()
    }

    /// The match over the two arms, the second closed over the binder it was entered under.
    pub fn rebuild(&self, zero: Term, predecessor: &Free, successor: Term) -> (Term, Term) {
        let result_type = self.goal.clone();
        let rebuilt = Subterm::Match(Match {
            head: self.head.clone(),
            goal: self.goal.clone(),
            // `Nat` is the free monoid on one payload-less generator: its cons arm binds just the predecessor, so the carrier is `Nat` and the head is absent.
            cases: Cases::FreeMonoid {
                carrier: Carrier::Nat {
                    empty_case: zero,
                    cons_case: Scope::close(One, &[predecessor], successor),
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
    goal: &Term,
    zero_case: &Term,
    succ_case: &Scope<One>,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let split = NatSplit::resolve(context, scrutinee, goal, &mode)?;
    let zero = split.zero(context, |context, expected| {
        check(context, zero_case, expected)
    })?;

    let predecessor = context.fresh_for(succ_case.hint(0), succ_case.written(0));
    let successor = split.successor(context, &predecessor, |context, expected| {
        let body = succ_case.open(&[&Term::free_var(&predecessor)]);

        check(context, &body, expected)
    })?;

    Ok(split.rebuild(zero, &predecessor, successor))
}

/// The sequences a case split takes apart into the empty one and an atom ahead of a tail.
#[derive(Debug, Clone, Copy)]
pub enum Sequence {
    List,
    Bytes,
}

/// What a sequence's cases are built from: a list's element type, read off the scrutinee's type, or nothing for `Bytes`.
enum Atoms {
    List { element: Term },
    Bytes,
}

/// A case split of a list or a packed sequence with its scrutinee and its result resolved: the empty case, and an atom ahead of a tail.
pub struct SequenceSplit {
    head: Term,
    head_type: Term,
    atoms: Atoms,
    goal: Term,
}

impl SequenceSplit {
    pub fn resolve(
        context: &mut Context,
        sequence: Sequence,
        scrutinee: Scrutinee,
        goal: &Term,
        mode: &Mode,
    ) -> Result<Self, Error> {
        let (head, head_type, atoms) = match sequence {
            // `List` carries an element type the eliminator must read off the scrutinee (unlike `Nat`, whose carrier is parameterless) — its type must be `List(elem)`.
            Sequence::List => {
                let Scrutinee {
                    term: head,
                    type_: head_type,
                } = scrutinee;
                // A type nothing has decided yet is held to a list, as [`Scrutinee::of_intrinsic`] holds one to its carrier, of an element type left to what pins it.
                let head_type = match stuck_on_metavar(context, &head_type) {
                    true => {
                        let slot = Term::hole(context.mint_metavar());
                        let element = check_is_sort(context, &slot)?.0;
                        let list: Term = Subterm::Intrinsic(Intrinsic::ListType(element)).into();
                        expect(context, &head, &head_type, &list)?;

                        list
                    }
                    false => head_type,
                };
                let element = match &*head_type {
                    Subterm::Intrinsic(Intrinsic::ListType(element)) => element.clone(),
                    _ => {
                        return Err(refused_scrutinee(context, Eliminator::List, &head_type));
                    }
                };

                (head, head_type, Atoms::List { element })
            }
            // `Bytes` is a parameterless carrier (like `Nat`/`Bool`), so the scrutinee's type is just `Bytes` — no element type to read off the head as `List` needs.
            Sequence::Bytes => {
                let (head, head_type) =
                    scrutinee.of_intrinsic(context, Eliminator::Bytes, IntrinsicHead::Bytes)?;

                (head, head_type, Atoms::Bytes)
            }
        };
        let goal = resolve_goal(context, goal, mode)?;

        Ok(Self {
            head,
            head_type,
            atoms,
            goal,
        })
    }

    /// The empty case: `body` is handed the type the arm is typed at, in the frame it is typed in, and answers the arm. The scrutinee is refined to the case (as `Bool`/`Switch` do), so a hypothesis whose type mentions it reduces at the arm's value with no hand-written convoy.
    pub fn empty(
        &self,
        context: &mut Context,
        body: impl FnOnce(&mut Context, Term) -> Result<Term, Error>,
    ) -> Result<Term, Error> {
        let value: Term = Subterm::Intrinsic(match &self.atoms {
            Atoms::List { element } => Intrinsic::List {
                element: element.clone(),
                items: vec![],
            },
            Atoms::Bytes => Intrinsic::Bytes(Binary::empty()),
        })
        .into();

        context.with_frame(|context| split_arm(context, &self.head, &self.goal, &value, body))
    }

    /// The case of an atom ahead of a tail, under the two binders it opens.
    pub fn cons(
        &self,
        context: &mut Context,
        [atom, tail]: &[Free; 2],
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
                Atoms::Bytes => {
                    let atom_type = Subterm::Intrinsic(Intrinsic::ByteType);
                    // The cons value `head :: tail`, encoded as the monoid operation on the singleton `[head]` and the tail. A `Bytes` literal holds only concrete bytes, so the singleton of the symbolic byte `head` is `append(x[], head)` (an atom appended to the empty packed sequence), not a literal run.
                    let singleton: Term = Subterm::Intrinsic(Intrinsic::BytesAppend {
                        bytes: Subterm::Intrinsic(Intrinsic::Bytes(Binary::empty())).into(),
                        element: Term::free_var(atom),
                    })
                    .into();
                    let value = Subterm::Intrinsic(Intrinsic::BytesConcat {
                        operands: vec![singleton, Term::free_var(tail)],
                    });

                    (atom_type.into(), value.into())
                }
            };

            context.assume(atom, &atom_type);
            context.assume(tail, &self.head_type);

            split_arm(context, &self.head, &self.goal, &value, body)
        })
    }

    /// The match's own type: the result at the scrutinee as it stands.
    pub fn result_type(&self) -> Term {
        self.goal.clone()
    }

    /// The match over the two arms, the second closed over the binders it was entered under.
    pub fn rebuild(&self, empty: Term, [atom, tail]: &[Free; 2], cons: Term) -> (Term, Term) {
        let result_type = self.goal.clone();
        let cons_case = Scope::close(Two, &[atom, tail], cons);
        let carrier = match &self.atoms {
            Atoms::List { element } => Carrier::List {
                elem: element.clone(),
                empty_case: empty,
                cons_case,
            },
            Atoms::Bytes => Carrier::Bytes {
                empty_case: empty,
                cons_case,
            },
        };
        let rebuilt = Subterm::Match(Match {
            head: self.head.clone(),
            goal: self.goal.clone(),
            cases: Cases::FreeMonoid { carrier },
        })
        .into();

        (rebuilt, result_type)
    }
}

fn elaborate_list_match(
    context: &mut Context,
    scrutinee: Scrutinee,
    goal: &Term,
    empty_case: &Term,
    cons_case: &Scope<Two>,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    elaborate_sequence_match(
        context,
        Sequence::List,
        scrutinee,
        goal,
        (empty_case, cons_case),
        mode,
    )
}

fn elaborate_bytes_match(
    context: &mut Context,
    scrutinee: Scrutinee,
    goal: &Term,
    cases: (&Term, &Scope<Two>),
    mode: Mode,
) -> Result<(Term, Term), Error> {
    elaborate_sequence_match(context, Sequence::Bytes, scrutinee, goal, cases, mode)
}

/// A case split of a list or a packed sequence written one level deep: each arm is its lowered body, checked.
fn elaborate_sequence_match(
    context: &mut Context,
    sequence: Sequence,
    scrutinee: Scrutinee,
    goal: &Term,
    (empty_case, cons_case): (&Term, &Scope<Two>),
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let split = SequenceSplit::resolve(context, sequence, scrutinee, goal, &mode)?;
    let empty = split.empty(context, |context, expected| {
        check(context, empty_case, expected)
    })?;

    let binders = [
        context.fresh_for(cons_case.hint(0), cons_case.written(0)),
        context.fresh_for(cons_case.hint(1), cons_case.written(1)),
    ];
    let cons = split.cons(context, &binders, |context, expected| {
        let [atom, tail] = &binders;
        let body = cons_case.open(&[&Term::free_var(atom), &Term::free_var(tail)]);

        check(context, &body, expected)
    })?;

    Ok(split.rebuild(empty, &binders, cons))
}

/// A `Nat` dispatch with its scrutinee and its result resolved: a case for each literal written, and the default any other value takes.
pub struct SwitchSplit {
    head: Term,
    goal: Term,
}

impl SwitchSplit {
    pub fn resolve(
        context: &mut Context,
        scrutinee: Scrutinee,
        goal: &Term,
        mode: &Mode,
    ) -> Result<Self, Error> {
        let (head, _) = scrutinee.of_intrinsic(context, Eliminator::Nat, IntrinsicHead::Nat)?;

        let goal = resolve_goal(context, goal, mode)?;

        Ok(Self { head, goal })
    }

    /// The case at the literal `value`: `body` is handed the type the arm is typed at, in the frame it is typed in, and answers the arm.
    pub fn arm(
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
                body(context, goal_at(&self.goal, &self.head, &[], &[], &literal))
            })
            .map_err(|error| from_arm(context, &self.head, &literal, error))
    }

    /// What any other value takes. It refines nothing, so it is typed at the result at the scrutinee itself.
    pub fn default(
        &self,
        context: &mut Context,
        body: impl FnOnce(&mut Context, Term) -> Result<Term, Error>,
    ) -> Result<Term, Error> {
        body(context, self.goal.clone())
    }

    /// The match's own type: the result at the scrutinee as it stands.
    pub fn result_type(&self) -> Term {
        self.goal.clone()
    }

    /// The match over `cases`, which arrive strictly ascending as `Cases::Switch` states them, and the default.
    pub fn rebuild(&self, cases: Vec<(Natural, Term)>, default: Term) -> (Term, Term) {
        let result_type = self.goal.clone();
        let rebuilt = Subterm::Match(Match {
            head: self.head.clone(),
            goal: self.goal.clone(),
            cases: Cases::Switch { cases, default },
        })
        .into();

        (rebuilt, result_type)
    }
}

fn elaborate_switch(
    context: &mut Context,
    scrutinee: Scrutinee,
    goal: &Term,
    cases: &[(Natural, Term)],
    default: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let split = SwitchSplit::resolve(context, scrutinee, goal, &mode)?;

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

pub fn elaborate_match(
    context: &mut Context,
    m: &Match,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // A rebuilt match coming back through re-elaboration at its goal: the goal *is* the expected type it was elaborated against, so it re-elaborates checked against that goal, where lowering's goal is a hole the mode decides. Checked at another spelling of that type — the one an arm around it specializes now, where its first elaboration met the scrutinee behind a metavariable not yet substituted — it keeps the goal it holds, so elaborating what elaboration built gives it back: conversion compares two goals with no arm's equation, and a rebuilt copy of a recursive member that differed from the member in a goal's spelling would be another term to it.
    let written = !matches!(&*m.goal, Subterm::Metavar(metavar) if metavar.is_hole());
    let mode = match (written, mode) {
        (true, Mode::Infer) => Mode::Check(m.goal.clone()),
        (true, Mode::Check(expected))
            if m.goal != expected && kept_goal(context, &m.goal, &expected)? =>
        {
            Mode::Check(m.goal.clone())
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
    let Match { goal, cases, .. } = m;

    match cases {
        Cases::Bool {
            false_case,
            true_case,
        } => elaborate_bool_match(context, scrutinee, goal, false_case, true_case, mode),
        Cases::Switch { cases, default } => {
            elaborate_switch(context, scrutinee, goal, cases, default, mode)
        }
        // The instance a match states is this elaboration's to write, off the type its scrutinee is elaborated at: one read back is written again.
        Cases::Induct { cases, default, .. } => elaborate_induct_match(
            context,
            InductMatchInput {
                scrutinee,
                goal,
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
        } => elaborate_nat_match(context, scrutinee, goal, empty_case, cons_case, mode),
        Cases::FreeMonoid {
            carrier:
                Carrier::List {
                    empty_case,
                    cons_case,
                    ..
                },
        } => elaborate_list_match(context, scrutinee, goal, empty_case, cons_case, mode),
        Cases::FreeMonoid {
            carrier:
                Carrier::Bytes {
                    empty_case,
                    cons_case,
                },
        } => elaborate_bytes_match(context, scrutinee, goal, (empty_case, cons_case), mode),
    }
}

struct InductMatchInput<'a> {
    scrutinee: Scrutinee,
    goal: &'a Term,
    cases: &'a [(Atom, InductArm)],
    default: Option<&'a Term>,
    term: &'a Term,
}

/// A `Bool` elimination with its scrutinee and its result resolved: the two cases.
pub struct BoolSplit {
    head: Term,
    goal: Term,
}

impl BoolSplit {
    pub fn resolve(
        context: &mut Context,
        scrutinee: Scrutinee,
        goal: &Term,
        mode: &Mode,
    ) -> Result<Self, Error> {
        let (head, _) = scrutinee.of_intrinsic(context, Eliminator::Bool, IntrinsicHead::Bool)?;

        let goal = resolve_goal(context, goal, mode)?;

        Ok(Self { head, goal })
    }

    /// The case at `value`: `body` is handed the type the arm is typed at, in the frame it is typed in, and answers the arm.
    pub fn arm(
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
                body(context, goal_at(&self.goal, &self.head, &[], &[], &literal))
            })
            .map_err(|error| from_arm(context, &self.head, &literal, error))
    }

    /// The match's own type: the result at the scrutinee as it stands.
    pub fn result_type(&self) -> Term {
        self.goal.clone()
    }

    /// The match over the two arms.
    pub fn rebuild(&self, false_case: Term, true_case: Term) -> (Term, Term) {
        let result_type = self.goal.clone();
        let rebuilt = Subterm::Match(Match {
            head: self.head.clone(),
            goal: self.goal.clone(),
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
    goal: &Term,
    false_case: &Term,
    true_case: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let split = BoolSplit::resolve(context, scrutinee, goal, &mode)?;
    let false_case = split.arm(context, false, |context, expected| {
        check(context, false_case, expected)
    })?;
    let true_case = split.arm(context, true, |context, expected| {
        check(context, true_case, expected)
    })?;

    Ok(split.rebuild(false_case, true_case))
}

/// Whether a proposition may be matched into data: it declares one constructor, and that constructor's index targets pin every payload binder (matching a value against a target recovers the binder, as `Eq`'s `refl(z) : (z, z)` recovers `z`), decided by the walk both checkers share. Occurrence is not determination: `blur(a)` mentions `a` and pins nothing, since `blur` need not be injective. A payload that is a proof is not excused, since the arm would be handed the proof the match was given; and a second constructor the scrutinee's indices rule out is one the certifier counts all the same.
fn singleton_eliminable(
    context: &mut Context,
    name: &Global,
    universes: &Universes,
    params: &[Term],
) -> Result<bool, Error> {
    let payload = match context.induct_decl(name) {
        Some(family) => match family.constructors().as_slice() {
            [(tag, _)] => family.at(universes).payload(tag, params),
            _ => None,
        },
        None => None,
    };
    let Some(payload) = payload.transpose()? else {
        return Ok(false);
    };

    // Open the payload telescope under fresh binders; the terminal's indices are the constructor's targets.
    let mut binders: Vec<Free> = Vec::new();
    let mut cursor = payload.cursor();
    while cursor.entry().is_some() {
        binders.push(cursor.advance_fresh(|hint| context.fresh(hint)));
    }
    let terminal = cursor.body().expect("a cursor past every entry");

    Ok(wholly_pinned(&binders, &terminal))
}

/// A nominal elimination with its scrutinee and its result resolved: the family the scrutinee's type is an instance of, at its parameters and its indices, and the result each arm is typed at a case of.
///
/// The intrinsic eliminator's typing rule. Arm binders are typed directly from the constructor's registry telescope instantiated at the scrutinee type's parameters — no projections from a stuck payload — and each arm's written binders are aligned to that telescope's marks, a hidden payload left out bound all the same.
///
/// Each arm checks against the goal at *that case's* target indices, its variable indices and scrutinee standing for the case, the whole match types at the goal itself, and a catch-all default — which binds nothing and refines no index — checks at the goal too.
pub struct NominalSplit {
    head: Term,
    head_type: Term,
    name: Global,
    universes: Universes,
    params: Vec<Term>,
    /// The scrutinee's indices, each reduced where the match stands.
    actual_indices: Vec<Term>,
    /// The goal, and the match's own type.
    goal: Term,
    /// Whether the scrutinee's family is a proposition.
    proposition: bool,
    /// Whether it is one whose one constructor has every payload pinned: the only kind a match may take into data once it has an arm, and the kind whose arms are written over what their equations state.
    pinned: bool,
}

impl NominalSplit {
    /// `vacuous` says the match writes no arm and no default, which is how a scrutinee of no nominal type is refused.
    pub fn resolve(
        context: &mut Context,
        scrutinee: Scrutinee,
        goal: &Term,
        mode: &Mode,
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
                    context,
                    Eliminator::Nominal { vacuous },
                    &head_type,
                ));
            }
        };

        // Reducing the scrutinee type to weak-head normal form leaves its index *arguments* untouched, so an index that is an outer-arm key (`s` refined to `Scan/bad()` by an enclosing match) still reads as the bare variable. Reduce each index in the current (refined) context so inversion sees the forced value and pins arm binders against it, rather than refusing it as a key-shaped index, which refinement handles.
        let actual_indices = indices
            .iter()
            .map(|index| reduce_with(context, index))
            .collect::<Result<Vec<_>, _>>()?;

        let Some(family) = context.induct_decl(&name) else {
            return Err(Error::unknown_declaration(name));
        };
        let (rep_public, module) = (family.rep_public(), family.module());
        // The family's constraints instantiated where the scrutinee stands, once: every later read of the family is of the constructors the arms name, at these universes.
        context
            .induct_facet(&name, &universes, |_| Ok(()))
            .expect("a family the registry holds is read at an occurrence's universes")?;

        // Opacity covers every eliminator, including defaults and vacuous or inversion-discharged matches. Check before coverage or index refinement can reveal representation facts.
        if !rep_public
            && context
                .island()
                .is_some_and(|island| !island.is_within(&module))
        {
            return Err(Error::private_representation(name));
        }

        let goal = resolve_goal(context, goal, mode)?;

        let proposition = is_prop(context, &head_type)?;
        let pinned = proposition && singleton_eliminable(context, &name, &universes, &params)?;

        Ok(Self {
            head,
            head_type,
            name,
            universes,
            params,
            actual_indices,
            goal,
            proposition,
            pinned,
        })
    }

    /// The match's own type: the result at the scrutinee's actual indices and the scrutinee itself.
    pub fn result_type(&self) -> Term {
        self.goal.clone()
    }

    /// The large-elimination guard, asked before an arm is read: a strict proposition may not be eliminated into a relevant (data) result — observing which inhabitant it was would break proof irrelevance. Permitted only by *empty* elimination (no arm, every constructor ruled out) or by the elimination of a family with one constructor whose every payload its indices pin. `armless` says the match writes no arm and `defaulted` that it writes a catch-all.
    ///
    /// Answers whether the result's sort is still undecided, which [`NominalSplit::rebuild`] holds to `Prop` once the arms have decided it.
    pub fn admit(
        &self,
        context: &mut Context,
        armless: bool,
        defaulted: bool,
    ) -> Result<bool, Error> {
        // A match with no arm is vacuous: the coverage check below proves every constructor ruled out at the scrutinee's indices, so nothing is eliminated. Any other is judged by the family's declared constructors, as the certifier judges it, so a second constructor the indices rule out here counts all the same. A catch-all default stands in for more than one constructor — a relevant result branching on an erased scrutinee's tag is exactly what this guard forbids — so it is never permitted here. (A prop-into-prop defaulted match never reaches this branch.)
        let permitted = !defaulted && (armless || self.pinned);
        // Every other match on a proposition answers a proposition and nothing else. A result known for data is refused here, before an arm is read. One whose sort nothing has decided waits for the arms, which decide it where they are typed, and is held to `Prop` after them ([`hold_prop`]): so a match its arms make data is refused as this elimination, and not as a mismatch in an arm.
        Ok(match self.proposition && !permitted {
            false => false,
            true => {
                let quality = quality_of(context, &self.goal)?;
                probe_open_sort(context, "elaborate_induct_match", quality);
                match quality {
                    Quality::Prop => false,
                    Quality::Meta(_) => true,
                    Quality::Type | Quality::Param(_) => {
                        return Err(eliminated_into_data(context, &self.name, &self.goal));
                    }
                }
            }
        })
    }

    /// Every written arm must name a constructor: the refusal of the first of `tags` that names none. Coverage is decided per constructor — a missing arm is legal iff inversion proves it impossible ([`NominalSplit::ruled_out`]).
    pub fn declared<'t>(
        &self,
        context: &Context,
        tags: impl IntoIterator<Item = &'t Atom>,
    ) -> Result<(), Error> {
        let family = self.family(context);
        let Some(tag) = tags.into_iter().find(|tag| !family.declares(tag)) else {
            return Ok(());
        };

        // Each constructor as a pattern writes it, one placeholder per plain payload — a hidden one is left out — so the report shows what the arm could have named.
        let constructors = family
            .constructor_order()
            .map(|constructor| {
                let payload = family
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

    /// The scrutinee's family as the registry holds it, read by reference: what is instantiated of it is read where it is needed, at the scrutinee's universes, whose constraints [`NominalSplit::resolve`] instantiated.
    fn family<'c>(&self, context: &'c Context) -> &'c InductDecl {
        context
            .induct_decl(&self.name)
            .expect("a resolved scrutinee's family is registered")
    }

    /// The constructors a match's arms are elaborated at, in the order the family declares them, which is the order its arms are stored in however they are written: two matches differing only in how their arms were written produce the same term. Where a catch-all stands, the ones `written` names, each once, the default taking the rest, so nothing else is read of the family; otherwise every one, each a written arm or a missing one to rule out. A tag the family does not declare is none of them ([`NominalSplit::declared`] refuses it).
    pub fn cases<'t>(
        &self,
        context: &Context,
        written: impl IntoIterator<Item = &'t Atom>,
        defaulted: bool,
    ) -> Vec<Atom> {
        let family = self.family(context);
        match defaulted {
            true => {
                let mut named = written
                    .into_iter()
                    .filter_map(|tag| Some((family.constructor_index(tag)?, tag)))
                    .collect::<Vec<_>>();
                named.sort_unstable_by_key(|&(position, _)| position);
                named.dedup_by_key(|&mut (position, _)| position);
                named.into_iter().map(|(_, tag)| tag.clone()).collect()
            }
            false => family.constructor_order().cloned().collect(),
        }
    }

    /// Whether a constructor no arm names is left possible, past the ones the arms `named`: what a row that writes a binder is the arm of. An unindexed family leaves one where it declares more than the arms name, which is read off the names alone; an indexed one where inversion rules out not every one of the rest.
    pub fn leaves_unnamed<'t>(
        &self,
        context: &mut Context,
        named: impl IntoIterator<Item = &'t Atom>,
    ) -> Result<bool, Error> {
        let family = self.family(context);
        let named = named
            .into_iter()
            .filter(|tag| family.declares(tag))
            .collect::<HashSet<_>>();
        if !self.indexed() {
            return Ok(named.len() < family.constructors().len());
        }
        let unnamed = family
            .constructor_order()
            .filter(|tag| !named.contains(tag))
            .cloned()
            .collect::<Vec<_>>();
        for tag in &unnamed {
            if !self.ruled_out(context, tag)? {
                return Ok(true);
            }
        }

        Ok(false)
    }

    /// Whether the arm for `tag` may be left out.
    ///
    /// Inversion — checker-verified omission: a missing arm is accepted iff first-order unification of the scrutinee's actual indices against this case's targets finds a *definite* clash, in either direction — the answer a written arm is checked under, so an arm may be left out exactly where the certifier would type none of it. The arm is then pruned (erase fills its slot with an unreachable body); anything short of definite keeps the arm mandatory.
    pub fn ruled_out(&self, context: &mut Context, tag: &Atom) -> Result<bool, Error> {
        // An unindexed inductive has nothing to invert: every arm is reachable and a missing one is plainly missing.
        if self.actual_indices.is_empty() {
            return Ok(false);
        }

        let payload = self.payload(context, tag)?;
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

    /// Whether the family is indexed, which is where inversion may excuse an arm left out.
    pub fn indexed(&self) -> bool {
        !self.actual_indices.is_empty()
    }

    /// The refusal of a match that leaves out the arm for `tag` where nothing rules the case out.
    pub fn missing(&self, tag: &Atom) -> Error {
        match self.actual_indices.is_empty() {
            true => Error::match_case_missing(self.name, tag.to_string()),
            false => Error::missing_arm_not_impossible(tag.clone()),
        }
    }

    /// The payload of `tag`'s case, at the scrutinee's parameters and universes: that one constructor of the family instantiated.
    pub fn payload(&self, context: &Context, tag: &Atom) -> Result<Telescope<Vec<Term>>, Error> {
        self.family(context)
            .at(&self.universes)
            .payload(tag, &self.params)
            .expect("constructor instantiates at its inductive's parameters")
            .map_err(Error::from)
    }

    /// The case of `tag`, whose `payload` is opened under `labels`, one for each of its binders, each assumed at its declared (dependent) type: `body` is handed the type the arm is typed at, in the frame it is typed in, and answers the arm, which is closed over every payload.
    pub fn arm(
        &self,
        context: &mut Context,
        tag: &Atom,
        payload: Telescope<Vec<Term>>,
        labels: &[Free],
        body: impl FnOnce(&mut Context, Term) -> Result<Term, Error>,
    ) -> Result<(Atom, InductArm), Error> {
        let arity = payload.len();
        let payload_plicities = payload.marks();
        let vars = labels.iter().map(Term::free_var).collect::<Vec<_>>();

        // Refinement propagates `head := ctor_val` to other occurrences of the scrutinee in the arm body; the binder types themselves came from the telescope below. Built at the scrutinee's own universe levels, because this value outlives the refinement: the goal is read at it, so it is what a metavariable in an arm's expected type is solved to — the `@z` of an `Eq/refl()` against `Eq()(len(xs), len(xs))` — and a level-less occurrence of a polymorphic family zonks into the definition, where the arity check (or, for a prelude family it cannot see, the certifier) refuses it.
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

                // The index equations: the most-general solution of `actual indices ~ case targets`, both directions — arm binders pinned to the actuals they must equal, outer variables refined to the targets they must equal — by the certifier's own function, and recorded as the same frame-scoped refinements. A definite clash means the arm is unreachable; it was written, so it is simply checked as is, with nothing solved, and an error it raises is reported as one in an arm that may be left out. Refinements never justify the typing (the goal at the case does); they are convertibility aids, so the body's occurrences of a solved variable reduce at the arm's indices.
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

                // The goal at this case: the case's targets and value substituted for its variable indices and scrutinee.
                let expected = goal_at(
                    &self.goal,
                    &self.head,
                    &self.actual_indices,
                    &ix_c,
                    &ctor_val,
                );
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
        // Rebuild the arm over every payload under the constructor's canonical marks, so a re-elaborated arm writes each binder and re-checks identically (idempotence). Whether a payload is a proof is its constructor's to say, which the closed machine asks of its host where the arm fires.
        Ok((
            tag.clone(),
            InductArm::new(
                Scope::close(Many(arity), &label_refs, body),
                payload_plicities,
            ),
        ))
    }

    /// The catch-all: `body` is handed the type it is typed at and answers it. It binds nothing and refines no index, so it is typed at the unrefined head — the result at the actual scrutinee, exactly as [`SwitchSplit::default`] types its default.
    pub fn default(
        &self,
        context: &mut Context,
        body: impl FnOnce(&mut Context, Term) -> Result<Term, Error>,
    ) -> Result<Term, Error> {
        context.with_frame(|context| body(context, self.goal.clone()))
    }

    /// The match over `cases`, in the order [`NominalSplit::cases`] gives, and the catch-all where one is written. `undecided` is what [`NominalSplit::admit`] answered, and `term` the match a sort decided here is reported with.
    pub fn rebuild(
        &self,
        context: &mut Context,
        cases: Vec<(Atom, InductArm)>,
        default: Option<Term>,
        undecided: bool,
        term: &Term,
    ) -> Result<(Term, Term), Error> {
        if undecided && !hold_prop(context, &self.goal, term.extent())? {
            return Err(eliminated_into_data(context, &self.name, &self.goal));
        }

        // A match with one arm over a proposition whose indices pin its payload states the instance it eliminates: reduction fires it on that instance and reads no binder's type, so what it is moves with the term and with nothing else.
        let stated =
            (self.pinned && default.is_none() && cases.len() == 1).then(|| self.head_type.clone());
        let rebuilt = Subterm::Match(Match {
            head: self.head.clone(),
            goal: self.goal.clone(),
            cases: Cases::Induct {
                cases,
                default,
                stated,
            },
        })
        .into();

        Ok((rebuilt, self.goal.clone()))
    }
}

/// For each payload of `tag`'s case, the binder among `written` that names it, or `None` where none does.
///
/// An arm's written binders meet the payload's slots as a lambda's meet a function type's ([`align()`]): the plain binders are the plain payloads, in order, and between two of them the hidden binders written are the first of their run. A hidden payload no binder was written for is bound all the same, hintless, so nothing in the arm names it and it is in scope as its mark makes it.
pub fn payload_fills(
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
            payload.hints()[slot],
        ),
        // A marked binder with no hidden payload left in its run: at the plain payload the plain binders before it have reached, which no mark binds, or past the last.
        Misaligned::Surplus { member } => match stands_at(&payload_plicities, written, member) {
            Some(slot) => Error::BinderPlicityMismatch {
                site: BinderSite::Payload {
                    constructor: tag.to_string(),
                },
                position: slot + 1,
                binder: None,
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
        goal,
        cases,
        default,
        term,
    } = input;

    // A match with no arms is a vacuous elimination — either of an empty inductive (`False`) or of one whose every constructor inversion-clashes at the scrutinee's indices. Such a match compiles to unreachable code that never inspects the scrutinee. A proof/type scrutinee carries no runtime content (sort-driven erasure), so an empty match discharging an erased witness of falsity into a relevant result is sound without a usage discipline.
    let vacuous = cases.is_empty() && default.is_none();
    let split = NominalSplit::resolve(context, scrutinee, goal, &mode, vacuous)?;
    let undecided = split.admit(context, cases.is_empty(), default.is_some())?;
    split.declared(context, cases.iter().map(|(tag, _)| tag))?;

    // Each written arm by its tag, the first written of a tag standing for it.
    let written = cases
        .iter()
        .rev()
        .map(|(tag, scope)| (tag, scope))
        .collect::<HashMap<_, _>>();
    // Built in the *declaration* order, not the written order, so the elaborated arm sequence is canonical. A catch-all default covers every un-enumerated constructor, so with one only the written arms are walked, and a missing arm needs neither the unindexed-completeness check nor index inversion — the default is checked once, below.
    let mut cases_elaborated = Vec::new();
    for tag in split.cases(context, cases.iter().map(|(tag, _)| tag), default.is_some()) {
        let tag = &tag;
        let Some(&scope) = written.get(tag) else {
            if split.ruled_out(context, tag)? {
                continue;
            }

            return Err(split.missing(tag));
        };

        let payload = split.payload(context, tag)?;
        let fills = payload_fills(tag, &payload, scope.plicities())?;

        // Open the telescope with fresh names, one per payload: a written binder's under its own label, each binder assumed at its declared (dependent) type.
        let labels = fills
            .iter()
            .map(|fill| match fill {
                Some(member) => {
                    context.fresh_for(scope.body().hint(*member), scope.body().written(*member))
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
/// A written arm and an omitted one open their case the same way, so inversion is put the same question about both — which is also the question the certifier puts, its callers reaching the unifier through a payload open that assumes every binder.
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

/// Re-assume every local a case's solution re-types, at its specialized type: `curios_analysis::retyped` over `solutions` joined by the scrutinee's own (`curios_analysis::scrutinee_solution`), which is the list and the rule the certifier's arm applies. The arm's refinements make such a type *reduce* at the case, which is enough for the body's own conversions, but not for a metavariable born in the arm: it keeps the types of its birth context and checks its solution against them, retried outside the frame — `z : Sizes(s)` used as `(z).0` under `s := node(a, b)` has to be a tuple there, which the re-typed entry states outright.
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

    // The binders past the base frame alone: a declaration's type is closed, so no solution of an arm's locals retypes one, and reading them all would cost every arm the program's size. Each type is read with its committed solutions substituted: a local `let`'s type stays as written, a solved metavariable whose spine names every local before it, which is no mention of the variable the case solves.
    let locals = context.local_binders().cloned().collect::<Vec<_>>();
    let locals = locals
        .into_iter()
        .map(|(name, type_)| (name, zonk_solved_term_metas(&*context, &type_)))
        .collect::<Vec<_>>();
    let retyped = retyped(&*context, &locals, &solutions);
    context.assume_all(retyped);
}
