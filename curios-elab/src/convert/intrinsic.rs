//! Congruence for intrinsic operations.
//!
//! Two intrinsics are equal when they are the same operation applied to convertible operands. That is a congruence rule rather than a computation rule — the computation already happened, since both sides arrived reduced and a foldable operation would have folded.
//!
//! The rule is stated *generically* rather than as one arm per operation, and once for both checkers: `curios-analysis`'s `convert_intrinsics` runs the carriers' algebra and reads the congruence off the traversal that defines an intrinsic's operands. A hand-written pair match over a roster of upwards of a hundred entries is a list whose omissions are silent — `convert` short-circuits on syntactic identity before reaching here, so a missing arm only surfaces on two spellings that are convertible without being identical, as a *hard mismatch* rather than a postponement. What this module keeps is the elaborator's own part: its preparation, its packed-literal view, and its discharge.

use {
    super::{Convert, convert, solved_linearly},
    crate::{Context, zonk_solved_term_metas},
    curios_algebra::{Cut, split},
    curios_analysis::{
        Agreement, Congruence, Driver, Obligation, Outcome, Pass, connectives_agree,
        connectives_agree_classed, convert_classed, convert_intrinsics,
    },
    curios_core::{Classes, Cost, Free, Intrinsic, ReduceError, Reducer, Subterm, Term},
    curios_num::{Binary, Grain},
    curios_utilities::SyntaxRegistry,
};

/// Whether `this` and `that` are the same intrinsic operation on convertible operands, as far as the elaborator decides now — `curios-analysis`'s `convert_intrinsics`, the kernel's chain too, discharged the elaborator's way: a residual and every operand join the queue, and the levels compare by the elaborator's rule, which may constrain them.
pub(crate) fn convert_intrinsic(
    cmp: &mut Convert,
    context: &mut Context,
    this: Intrinsic,
    that: Intrinsic,
) -> Result<bool, ReduceError> {
    let pass = convert_intrinsics(
        &mut Elaborating { cmp, context },
        this.clone(),
        that.clone(),
    )?;
    let outcome = match pass {
        Pass::Settled(outcome) => outcome,
        Pass::Atoms(atoms) => {
            let classes = classes(context, &atoms)?;
            convert_classed(
                &mut Elaborating { cmp, context },
                this.clone(),
                that.clone(),
                &classes,
            )?
        }
    };
    match outcome {
        Outcome::Equal => Ok(true),
        Outcome::Unequal => Ok(false),
        // Operands nothing paired: nothing is enqueued, so no solution is picked by the order the operands stand in.
        Outcome::Unpaired => unsettled(cmp, context, this, that),
        Outcome::Residual(this, that) => {
            cmp.enqueue(Term::type_ground(), this, that);
            Ok(true)
        }
        Outcome::Congruence(Congruence {
            this_levels,
            that_levels,
            operands,
        }) => {
            if !Convert::compare_levels(context, &this_levels, &that_levels)? {
                return Ok(false);
            }
            let Some(operands) = operands else {
                return unsettled(cmp, context, this, that);
            };
            for Obligation { type_, this, that } in operands {
                cmp.enqueue(type_.unwrap_or_else(Term::type_ground), this, that);
            }
            Ok(true)
        }
    }
}

/// A pair the chain settled nothing of and handed nothing back for: operands unpaired, or two sides that are not one operation. Where it is an equation linear in its one unsolved metavariable, that metavariable has one solution and is solved to it, and the pair, now without it, is compared again. Otherwise it is a mismatch, which the drain parks rather than reports where either side still holds an unsolved metavariable, since solving one may yet settle it.
fn unsettled(
    cmp: &mut Convert,
    context: &mut Context,
    this: Intrinsic,
    that: Intrinsic,
) -> Result<bool, ReduceError> {
    match solved_linearly(context, &this, &that)? {
        true => {
            cmp.progress = true;
            convert_intrinsic(cmp, context, this, that)
        }
        false => Ok(false),
    }
}

/// Whether a `Bool` connective on either side of a pair that is not two intrinsics agrees with the other side: `curios-analysis`'s `connectives_agree`, its atoms classed here where it hands them back.
pub(super) fn connectives_convert(
    context: &mut Context,
    this: &Term,
    that: &Term,
) -> Result<bool, ReduceError> {
    match connectives_agree(context, this, that)? {
        Agreement::Agree => Ok(true),
        Agreement::Silent => Ok(false),
        Agreement::Atoms(atoms) => {
            let classes = classes(context, &atoms)?;
            connectives_agree_classed(context, this, that, &classes)
        }
    }
}

/// Which of `atoms` are one, as far as the elaborator can say without deciding anything else: two atoms are one where they convert with no solution and no universe constraint committed. Each comparison runs in the bracket a witness probe uses and is rolled back whatever it found, so a pair that would need a metavariable solved stays two atoms, and classing commits nothing.
fn classes(context: &mut Context, atoms: &[Term]) -> Result<Classes, ReduceError> {
    curios_profile::profile!("convert::classes");
    Classes::of(atoms, |this, that| {
        // Hand-paired rather than bracketed by a closure, as the witness probe's is: this sits on conversion's recursion.
        let mark = context.solution_mark();
        let solutions = context.solutions_committed();
        let universes = context.universes().state_token();
        let converts = convert(context, &Term::type_ground(), this, that);
        let committed = context.solutions_committed() != solutions
            || context.universes().state_token() != universes;
        context.rollback_solutions(mark);
        context.end_solutions(mark);
        Ok(converts? && !committed)
    })
}

/// The elaborator as the shared chain's driver: its context reduces, and its queue takes the packed-literal view's goals.
struct Elaborating<'a> {
    cmp: &'a mut Convert,
    context: &'a mut Context,
}

impl Reducer for Elaborating<'_> {
    fn reduce(&mut self, term: Term) -> Result<Term, ReduceError> {
        Reducer::reduce(self.context, term)
    }

    fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError> {
        Reducer::reduce_forced(self.context, term)
    }

    fn spend(&mut self, cost: Cost) -> Result<(), ReduceError> {
        Reducer::spend(self.context, cost)
    }

    fn fresh_binder(&mut self, hint: Option<&str>) -> Free {
        Reducer::fresh_binder(self.context, hint)
    }
}

impl Driver for Elaborating<'_> {
    /// **A summand meets its own spelling only once its solved metavariables are substituted.** Every peel pairs by identity — a summand cancels against a summand, a leaf joins a leaf set, an atom indexes a truth table — and while the signature holding them is being checked, two occurrences of `a + 1` are two terms: an operator reaches its concept through a witness metavariable of its own, solved to the one witness and not yet spliced. Unprepared, `f(a + 1) + f(c + 1)` against its commutation would share no summand, fall to the positional congruence, and be refused there as `a` against `c` — where the kernel, handed zonked terms, accepts the equation.
    fn prepare(&mut self, intrinsic: Intrinsic) -> Intrinsic {
        let solved = zonk_solved_term_metas(self.context, &Term::intrinsic(intrinsic.clone()));
        match &*solved {
            Subterm::Intrinsic(solved) => solved.clone(),
            _ => intrinsic,
        }
    }

    /// Solving-side only: the view undoes exactly the constant folding that removed the spine spelling from the literal side — `append(b[], true)` folds to `b[1]`, and no shape congruence relates the folded form to `append(b[], ?h)`. The reducer's laws are untouched, and once the goals commit their solutions the folded spellings agree by plain reduction, so the kernel needs no matching rule.
    fn packed_view(&mut self, this: &Intrinsic, that: &Intrinsic) -> Option<bool> {
        packed_literal_view(self.cmp, this, that)
    }

    fn syntax(&self) -> SyntaxRegistry {
        self.context.syntax()
    }
}

/// Decompose a nonempty (or empty) packed literal against an `append`/`concat` spine of the same grain, when the spine's segment lengths determine the split. `None` when the pair is not literal-versus-spine or a middle segment's length is unknown — the caller's congruence (and the drain's metavariable parking) keep their own handling. `Some(false)` is a definite structural length clash: segment lengths are fixed by the spine's shape, so no metavariable solution can repair them.
fn packed_literal_view(cmp: &mut Convert, this: &Intrinsic, that: &Intrinsic) -> Option<bool> {
    match (literal_of(this), literal_of(that)) {
        (Some((grain, lit)), None) => split_against(cmp, grain, lit, that),
        (None, Some((grain, lit))) => split_against(cmp, grain, lit, this),
        _ => None,
    }
}

fn literal_of(intrinsic: &Intrinsic) -> Option<(Grain, &Binary)> {
    match intrinsic {
        Intrinsic::Bin(grain, value) => Some((*grain, value)),
        _ => None,
    }
}

/// The number of atoms `intrinsic` denotes when its shape determines it: a literal's stored length, an `append`'s base plus one, a `concat`'s segment sum. `None` for anything symbolic-length (a variable, a slice, a metavariable).
fn known_len(grain: Grain, term: &Term) -> Option<usize> {
    let Subterm::Intrinsic(intrinsic) = &**term else {
        return None;
    };
    match intrinsic {
        Intrinsic::Bin(found, value) if *found == grain => Some(literal_len(grain, value)),
        Intrinsic::BinAppend {
            grain: found,
            bin: base,
            element: _,
        } if *found == grain => known_len(grain, base).map(|len| len + 1),
        Intrinsic::BinConcat {
            grain: found,
            operands,
        } if *found == grain => operands
            .iter()
            .map(|operand| known_len(grain, operand))
            .sum(),
        _ => None,
    }
}

fn literal_len(grain: Grain, value: &Binary) -> usize {
    match grain {
        Grain::B => value.bit_length(),
        Grain::X => value
            .to_bytes()
            .expect("an X-grain literal packs whole bytes")
            .len(),
    }
}

/// The literal's atoms `lo..hi` as a `Bin` literal of the same grain.
fn literal_slice(grain: Grain, value: &Binary, lo: usize, hi: usize) -> Term {
    Term::intrinsic(Intrinsic::Bin(
        grain,
        match grain {
            Grain::B => Binary::from_bits((lo..hi).map(|index| value.bit(index).unwrap())),
            Grain::X => Binary::from_bytes(value.to_bytes().unwrap()[lo..hi].to_vec()),
        },
    ))
}

/// The literal's atom at `index` as the element intrinsic an `append` operand carries: a `Bool` for `Bits`, a `Byte` for `Bytes`.
fn literal_atom(grain: Grain, value: &Binary, index: usize) -> Term {
    Term::intrinsic(match grain {
        Grain::B => Intrinsic::Bool(value.bit(index).unwrap()),
        Grain::X => Intrinsic::Byte(value.to_bytes().unwrap()[index]),
    })
}

/// Split `lit` against one spine node, enqueuing the aligned sub-goals.
fn split_against(cmp: &mut Convert, grain: Grain, lit: &Binary, spine: &Intrinsic) -> Option<bool> {
    let len = literal_len(grain, lit);
    match spine {
        // `append(base, atom) = base ++ [atom]`: the last literal atom pairs with `atom`, the rest with `base`. An empty literal against an always-nonempty `append` is a definite clash.
        Intrinsic::BinAppend {
            grain: found,
            bin: base,
            element: atom,
        } if *found == grain => {
            if len == 0 {
                return Some(false);
            }
            cmp.enqueue(
                Term::type_ground(),
                base.clone(),
                literal_slice(grain, lit, 0, len - 1),
            );
            cmp.enqueue(
                Term::type_ground(),
                atom.clone(),
                literal_atom(grain, lit, len - 1),
            );
            Some(true)
        }
        // `concat` splits at its segments' known lengths, consumed left to right, one trailing unknown-length segment taking the remainder — `curios-algebra`'s `split`. Each segment is related to its range of the literal as the split reaches it, so a split that clashes or abstains has already related the segments before where it stopped.
        Intrinsic::BinConcat {
            grain: found,
            operands,
        } if *found == grain => {
            let lengths = operands
                .iter()
                .map(|operand| known_len(grain, operand))
                .collect::<Vec<_>>();
            let split = split(&lengths, len);
            for (operand, range) in operands.iter().zip(split.ranges) {
                cmp.enqueue(
                    Term::type_ground(),
                    operand.clone(),
                    literal_slice(grain, lit, range.start, range.end),
                );
            }
            match split.cut {
                Cut::Whole => Some(true),
                Cut::Clash => Some(false),
                Cut::Undetermined => None,
            }
        }
        _ => None,
    }
}
