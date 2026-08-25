//! Eliminations: the semantic identity selection the specification names.
//!
//! Each Core elimination erases to its most precise erased form — `SwitchBool`, `SwitchNat`, `FoldNat`, first-class `FoldSequence` over the sequence itself, and schema-carrying `MatchVariant` whose arms bind the payload directly. Both sequence forms keep the peel-versus-fold distinction: a cons arm that ignores its induction hypothesis erases to `UnconsSequence`, one peel rather than an n-step fold, so a non-tail recursive caller does not re-run the whole fold at every level. Neither names a read: how a sequence is taken apart belongs to the lowering that performs it, and this crate names no index, window or length at all — see [A lowering names the elimination it performs](../../../documentation/design/toolchain/a-lowering-names-the-elimination-it-performs.md).
//!
//! The scrutinee is erased exactly once. The fold forms alias a non-variable head on the Core side first — a fresh variable defined as the head, its label mapped to the once-erased operand — because their peels re-derive Core terms from the head (`head - 1`, the sequence's length and slices), and walking those must re-resolve to the same operand instead of re-erasing an effectful expression. The dispatch forms (`Bool`, `Switch`, the inductive match) never re-derive from the head, so they refine the *original* head term — arm bodies were elaborated against reductions keyed on that term, and refining an alias in its place would break their re-derived typing. Arm-side refinement is typing-only: it reproduces the context the arm was elaborated in and emits nothing.

use {
    super::{
        Atom, Carrier, Cases, Context, Error, InductArm, InductDecl, InductType, Intrinsic,
        IntrinsicHead, Lowering, Many, Match, Nat, Outcome, Scope, Subterm, Telescope, Term, Three,
        Two, emitted, expect_intrinsic_head, infer, is_erasable, reduce_with, refine_head,
    },
    curios_core::{Free, Level},
    curios_utilities::{Grain, PackedBin},
    std::collections::BTreeMap,
};

/// The `List`/`Bin` carrier a sequence fold eliminates: the carrier-specific reads, so the fold erasure stays carrier-agnostic.
#[derive(Clone, Copy)]
enum SeqCarrier<'a> {
    List { element: &'a Term },
    Bin { grain: Grain },
}

impl SeqCarrier<'_> {
    fn grain(self) -> curios_ersd::SequenceGrain {
        match self {
            SeqCarrier::List { .. } => curios_ersd::SequenceGrain::List,
            SeqCarrier::Bin { grain } => curios_ersd::SequenceGrain::Bin(grain),
        }
    }

    /// The type of the element the cons arm binds — the list element type, or the grain's own scalar shape for a packed binary.
    fn element_type(self) -> Term {
        match self {
            SeqCarrier::List { element } => element.clone(),
            SeqCarrier::Bin { grain } => Term::intrinsic(match grain {
                Grain::B => Intrinsic::BoolType,
                Grain::X => Intrinsic::ByteType,
            }),
        }
    }

    /// The empty sequence — the value the empty arm refines the scrutinee to.
    fn empty_value(self) -> Term {
        match self {
            SeqCarrier::List { element } => Term::intrinsic(Intrinsic::List {
                element: element.clone(),
                items: vec![],
            }),
            SeqCarrier::Bin { grain } => Term::intrinsic(Intrinsic::Bin(grain, PackedBin::empty())),
        }
    }

    /// The cons value `head :: tail` — the monoid operation on a singleton and the tail, the same shape elaboration checked the arm under, so the arm refines the scrutinee to the term it was elaborated against.
    fn cons_value(self, head: &Term, tail: &Term) -> Term {
        match self {
            SeqCarrier::List { element } => Term::intrinsic(Intrinsic::ListConcat {
                element: element.clone(),
                operands: vec![
                    Term::intrinsic(Intrinsic::List {
                        element: element.clone(),
                        items: vec![head.clone()],
                    }),
                    tail.clone(),
                ],
            }),
            SeqCarrier::Bin { grain } => Term::intrinsic(Intrinsic::BinConcat {
                grain,
                operands: vec![
                    Term::intrinsic(Intrinsic::BinAppend {
                        grain,
                        bin: Term::intrinsic(Intrinsic::Bin(grain, PackedBin::empty())),
                        element: head.clone(),
                    }),
                    tail.clone(),
                ],
            }),
        }
    }
}

/// The match-wide data an inductive elimination threads to each arm. The motive opens at the scrutinee's indices followed by the head — the case's target indices in an enumerated arm, the actual ones in the default.
struct InductMatch<'a> {
    head: &'a Term,
    motive: &'a Scope<Many>,
    name: &'a curios_core::Global,
    universes: &'a [Level],
    params: &'a [Term],
    actual_indices: &'a [Term],
}

impl Lowering {
    /// Erase a match by its elimination form.
    pub(super) fn erase_match(
        &mut self,
        context: &mut Context,
        m: &Match,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let Match {
            head,
            motive,
            cases,
        } = m;
        match cases {
            Cases::Bool {
                false_case,
                true_case,
            } => self.erase_bool(context, head, motive, false_case, true_case, hint),
            Cases::Switch { cases, default } => {
                self.erase_switch(context, head, motive, cases, default, hint)
            }
            Cases::Induct { cases, default } => {
                self.erase_induct(context, head, motive, cases, default.as_ref(), hint)
            }
            Cases::FreeMonoid {
                carrier:
                    Carrier::Nat {
                        empty_case,
                        cons_case,
                    },
            } => self.erase_nat_fold(context, head, motive, empty_case, cons_case, hint),
            Cases::FreeMonoid {
                carrier:
                    Carrier::List {
                        elem,
                        empty_case,
                        cons_case,
                    },
            } => self.erase_seq_fold(
                context,
                head,
                motive,
                SeqCarrier::List { element: elem },
                empty_case,
                cons_case,
                hint,
            ),
            Cases::FreeMonoid {
                carrier:
                    Carrier::Bin {
                        grain,
                        empty_case,
                        cons_case,
                    },
            } => self.erase_seq_fold(
                context,
                head,
                motive,
                SeqCarrier::Bin { grain: *grain },
                empty_case,
                cons_case,
                hint,
            ),
        }
    }

    /// Erase the scrutinee exactly once. A variable head passes through; a compound head is walked once, then aliased on the Core side — a fresh variable defined as the head, mapped to the erased operand — so every Core term later re-derived from the head resolves to the same operand.
    fn scrutinee_operand(
        &mut self,
        context: &mut Context,
        head: &Term,
        head_type: &Term,
    ) -> Result<Result<(Term, curios_ersd::Atom), Outcome>, Error> {
        let atom = match self.walk(context, head, head_type, Some("scrutinee"))? {
            Outcome::Emitted(atom) => atom,
            diverged => return Ok(Err(diverged)),
        };
        if matches!(&**head, Subterm::Var(_)) {
            return Ok(Ok((head.clone(), atom)));
        }
        let name = context.fresh(Some("scrutinee"));
        context.define_assuming(&name, head_type, head, None);
        self.environment.bind(&name, atom);
        Ok(Ok((Term::free_var(&name), atom)))
    }

    /// Open a fresh block, erase `body` as a tail refined by `head = value` and typed at `motive(value)`, and seal it — the leaf-arm shape with no payload binders.
    fn refined_arm(
        &mut self,
        context: &mut Context,
        head: &Term,
        value: &Term,
        motive: &Scope<Many>,
        body: &Term,
    ) -> Result<curios_ersd::BlockId, Error> {
        let expected = motive.open(&[value]);
        self.builder.open_block();
        let outcome = context.with_frame(|context| {
            refine_head(context, head, value)?;
            self.walk(context, body, &expected, None)
        })?;
        Ok(self.seal(outcome))
    }

    /// Open a fresh block, erase `body` as a tail typed at `expected` without refining the scrutinee, and seal it — the catch-all default and the peeled dispatch arm.
    fn open_arm(
        &mut self,
        context: &mut Context,
        expected: &Term,
        body: &Term,
    ) -> Result<curios_ersd::BlockId, Error> {
        self.builder.open_block();
        let outcome = self.walk(context, body, expected, None)?;
        Ok(self.seal(outcome))
    }

    /// A dependent `Bool` elimination — a first-class `SwitchBool`, distinct from a `Nat` dispatch.
    fn erase_bool(
        &mut self,
        context: &mut Context,
        head: &Term,
        motive: &Scope<Many>,
        false_case: &Term,
        true_case: &Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let head_type = expect_intrinsic_head(context, head, IntrinsicHead::Bool)?;
        let scrutinee = emitted!(self.walk(context, head, &head_type, Some("scrutinee"))?);

        let if_false = self.refined_arm(
            context,
            head,
            &Term::intrinsic(Intrinsic::Bool(false)),
            motive,
            false_case,
        )?;
        let if_true = self.refined_arm(
            context,
            head,
            &Term::intrinsic(Intrinsic::Bool(true)),
            motive,
            true_case,
        )?;

        Ok(self.bind(
            hint,
            curios_ersd::Rhs::SwitchBool {
                scrutinee,
                if_false,
                if_true,
            },
        ))
    }

    /// A sparse `Nat` dispatch — `SwitchNat`: each case refines the scrutinee to its key; the default sees the unrefined head.
    fn erase_switch(
        &mut self,
        context: &mut Context,
        head: &Term,
        motive: &Scope<Many>,
        cases: &BTreeMap<u32, Term>,
        default: &Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let head_type = expect_intrinsic_head(context, head, IntrinsicHead::Nat)?;
        let scrutinee = emitted!(self.walk(context, head, &head_type, Some("scrutinee"))?);

        let mut nat_cases = Vec::with_capacity(cases.len());
        for (&key, body) in cases {
            let value = Term::intrinsic(Intrinsic::Nat(Nat::new(key)));
            let block = self.refined_arm(context, head, &value, motive, body)?;
            nat_cases.push(curios_ersd::NatCase { key, block });
        }

        let default_expected = motive.open(&[head]);
        let default = self.open_arm(context, &default_expected, default)?;

        Ok(self.bind(
            hint,
            curios_ersd::Rhs::SwitchNat {
                scrutinee,
                cases: nat_cases,
                default,
            },
        ))
    }

    /// A `Nat` free-monoid elimination: a successor arm that uses its induction hypothesis is a `FoldNat`; one that ignores it is a case split, peeled to a single-key dispatch at `pred := head - 1`.
    fn erase_nat_fold(
        &mut self,
        context: &mut Context,
        head: &Term,
        motive: &Scope<Many>,
        empty_case: &Term,
        cons_case: &Scope<Two>,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let head_type = expect_intrinsic_head(context, head, IntrinsicHead::Nat)?;
        let (head, scrutinee) = match self.scrutinee_operand(context, head, &head_type)? {
            Ok(pair) => pair,
            Err(diverged) => return Ok(diverged),
        };

        let zero = self.refined_arm(
            context,
            &head,
            &Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
            motive,
            empty_case,
        )?;

        // The hypothesis is dead: a case split, not a fold.
        if !cons_case.uses(1) {
            let pred = Term::intrinsic(Intrinsic::nat_sub(
                head.clone(),
                Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
            ));
            // The hypothesis never appears, so any term serves its slot.
            let dead_hypothesis = Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)));
            let peeled = cons_case.open(&[&pred, &dead_hypothesis]);
            let expected = motive.open(&[&head]);
            let default = self.open_arm(context, &expected, &peeled)?;

            return Ok(self.bind(
                hint,
                curios_ersd::Rhs::SwitchNat {
                    scrutinee,
                    cases: vec![curios_ersd::NatCase {
                        key: 0,
                        block: zero,
                    }],
                    default,
                },
            ));
        }

        // Induction: bind the predecessor and the hypothesis, then erase the successor arm at `motive(pred + 1)`.
        let pred_hint = cons_case.first_hint().map(str::to_string);
        let hypothesis_hint = cons_case.second_hint().map(str::to_string);
        let pred_label = context.fresh(pred_hint.as_deref());
        let hypothesis_label = context.fresh(hypothesis_hint.as_deref());
        let predecessor = self.builder.value(pred_hint);
        let hypothesis = self.builder.value(hypothesis_hint);
        self.environment
            .bind(&pred_label, curios_ersd::Atom::Value(predecessor));
        self.environment
            .bind(&hypothesis_label, curios_ersd::Atom::Value(hypothesis));

        self.builder.open_block();
        let outcome = context.with_frame(|context| {
            let pred_var = Term::free_var(&pred_label);
            context.assume(&pred_label, &Term::intrinsic(Intrinsic::NatType));
            context.assume(&hypothesis_label, &motive.open(&[&pred_var]));

            let successor = Term::intrinsic(Intrinsic::nat_add(
                pred_var.clone(),
                Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
            ));
            refine_head(context, &head, &successor)?;

            let body = cons_case.open(&[&pred_var, &Term::free_var(&hypothesis_label)]);
            let expected = motive.open(&[&successor]);
            self.walk(context, &body, &expected, None)
        })?;
        let step = self.seal(outcome);

        Ok(self.bind(
            hint,
            curios_ersd::Rhs::FoldNat {
                scrutinee,
                zero,
                step: curios_ersd::FoldNatStep {
                    predecessor,
                    hypothesis,
                    block: step,
                },
            },
        ))
    }

    /// A `List`/`Bin` free-monoid elimination: a cons arm that uses its hypothesis is a first-class `FoldSequence` whose step binds the element, the suffix, and the accumulator; one that ignores it is a first-class `UnconsSequence` binding the element and the suffix alone. Both hand the reads to `curios-ersd`'s lowering rather than emitting them, which is what keeps a window's operand convention out of this crate.
    #[allow(clippy::too_many_arguments)]
    fn erase_seq_fold(
        &mut self,
        context: &mut Context,
        head: &Term,
        motive: &Scope<Many>,
        carrier: SeqCarrier<'_>,
        empty_case: &Term,
        cons_case: &Scope<Three>,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let head_type = infer(context, head)?;
        let head_type = reduce_with(context, &head_type)?;
        let (head, sequence) = match self.scrutinee_operand(context, head, &head_type)? {
            Ok(pair) => pair,
            Err(diverged) => return Ok(diverged),
        };

        let empty = self.refined_arm(context, &head, &carrier.empty_value(), motive, empty_case)?;

        // The hypothesis is dead: a case split over the length, peeling the cons arm at the head element and tail slice.
        //
        // The peel binds *names* for the element and the suffix rather than writing `get`/`slice` terms into the arm and letting the walk typecheck them. That is a typing requirement, not a style choice: both reads are bounded operations, and the bound each would carry — `0 < len` for the element, `1 <= len` for the window — is exactly what the dispatch establishes, so at the point the terms would be constructed there is no proof to hand them.
        //
        // What the arm loses by it is the definitional connection back to the scrutinee: the binders are opaque assumptions of the element and sequence types rather than the reads themselves. Nothing needs that connection, because the arm is checked at `motive(head)`, which does not mention either.
        //
        // **The reads are not emitted here at all.** `UnconsSequence` is one peel, and how a peel is performed belongs to the lowering that performs it — `into_cont`'s `emit_peel`, which the fold reaches too. Open-coding it here meant this crate had to hold a window's operand convention, and a window is the one shape whose operands have changed under it.
        if !cons_case.uses(2) {
            let sequence_atom = sequence;
            let element_hint = cons_case.first_hint().map(str::to_string);
            let suffix_hint = cons_case.second_hint().map(str::to_string);
            let element_label = context.fresh(element_hint.as_deref());
            let suffix_label = context.fresh(suffix_hint.as_deref());
            let element = self.builder.value(element_hint);
            let suffix = self.builder.value(suffix_hint);

            self.environment
                .bind(&element_label, curios_ersd::Atom::Value(element));
            self.environment
                .bind(&suffix_label, curios_ersd::Atom::Value(suffix));

            self.builder.open_block();
            let outcome = context.with_frame(|context| {
                context.assume(&element_label, &carrier.element_type());
                context.assume(&suffix_label, &head_type);

                // The hypothesis never appears, so any term serves its slot.
                let dead_hypothesis = Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)));
                let peeled = cons_case.open(&[
                    &Term::free_var(&element_label),
                    &Term::free_var(&suffix_label),
                    &dead_hypothesis,
                ]);
                let expected = motive.open(&[&head]);
                self.walk(context, &peeled, &expected, None)
            })?;
            let cons_block = self.seal(outcome);

            return Ok(self.bind(
                hint,
                curios_ersd::Rhs::UnconsSequence {
                    grain: carrier.grain(),
                    scrutinee: sequence_atom,
                    empty,
                    cons: curios_ersd::UnconsSequenceStep {
                        element,
                        suffix,
                        block: cons_block,
                    },
                },
            ));
        }

        let element_hint = cons_case.first_hint().map(str::to_string);
        let suffix_hint = cons_case.second_hint().map(str::to_string);
        let accumulator_hint = cons_case.third_hint().map(str::to_string);
        let element_label = context.fresh(element_hint.as_deref());
        let suffix_label = context.fresh(suffix_hint.as_deref());
        let accumulator_label = context.fresh(accumulator_hint.as_deref());
        let element = self.builder.value(element_hint);
        let suffix = self.builder.value(suffix_hint);
        let accumulator = self.builder.value(accumulator_hint);
        self.environment
            .bind(&element_label, curios_ersd::Atom::Value(element));
        self.environment
            .bind(&suffix_label, curios_ersd::Atom::Value(suffix));
        self.environment
            .bind(&accumulator_label, curios_ersd::Atom::Value(accumulator));

        self.builder.open_block();
        let outcome = context.with_frame(|context| {
            let element_var = Term::free_var(&element_label);
            let suffix_var = Term::free_var(&suffix_label);
            context.assume(&element_label, &carrier.element_type());
            context.assume(&suffix_label, &head_type);
            context.assume(&accumulator_label, &motive.open(&[&suffix_var]));

            let cons_value = carrier.cons_value(&element_var, &suffix_var);
            refine_head(context, &head, &cons_value)?;

            let body = cons_case.open(&[
                &element_var,
                &suffix_var,
                &Term::free_var(&accumulator_label),
            ]);
            let expected = motive.open(&[&cons_value]);
            self.walk(context, &body, &expected, None)
        })?;
        let step = self.seal(outcome);

        Ok(self.bind(
            hint,
            curios_ersd::Rhs::FoldSequence {
                grain: carrier.grain(),
                scrutinee: sequence,
                empty,
                step: curios_ersd::FoldSequenceStep {
                    element,
                    suffix,
                    accumulator,
                    block: step,
                },
            },
        ))
    }

    /// A nominal inductive elimination — a schema-carrying `MatchVariant` whose arms bind the payload positionally. A constructor with no arm is covered by the default or, absent one, trapped by an `Unreachable` arm; an erasable (proof/type) scrutinee has no runtime tag and reduces to its single live arm. A vacuous elimination (no arms, no default) is unreachable code and diverges without erasing the head.
    fn erase_induct(
        &mut self,
        context: &mut Context,
        head: &Term,
        motive: &Scope<Many>,
        cases: &[(Atom, InductArm)],
        default: Option<&Term>,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        if cases.is_empty() && default.is_none() {
            return Ok(Outcome::Diverged(curios_ersd::Terminator::Unreachable));
        }

        let head_type = infer(context, head)?;
        let head_type = reduce_with(context, &head_type)?;
        let (name, universes, params, actual_indices) = match &*head_type {
            Subterm::InductType(InductType {
                name,
                universes,
                params,
                indices,
            }) => (
                name.clone(),
                universes.clone(),
                params.clone(),
                indices.clone(),
            ),
            _ => unreachable!("erase: inductive match scrutinee checked by elaborate"),
        };
        let induct_decl = context
            .induct_decl(&name)
            .cloned()
            .expect("erase: scrutinee type names a registered inductive");
        let induct_decl = context.instantiate_induct_decl_at(&induct_decl, &universes)?;

        let m = InductMatch {
            head,
            motive,
            name: &name,
            universes: &universes,
            params: &params,
            actual_indices: &actual_indices,
        };

        // A proof/type scrutinee carries no runtime tag; its subsingleton elimination reduces to its single live arm, erased inline.
        if is_erasable(context, &head_type)? {
            return self.erase_erasable_scrutinee(context, &m, &induct_decl, cases, default);
        }

        let row = self.induct_row(context, &name)?;
        let scrutinee = emitted!(self.walk(context, head, &head_type, Some("scrutinee"))?);

        let mut arms = Vec::new();
        for (index, tag) in induct_decl.constructor_order().enumerate() {
            let constructor = row.constructors[index].id;
            let mask = row.constructors[index].mask.clone();
            match cases
                .iter()
                .find(|(candidate, _)| candidate == tag)
                .map(|(_, arm)| arm)
            {
                Some(arm) => {
                    let telescope = induct_decl
                        .instantiate(tag, m.params)
                        .expect("erase: constructor instantiates at its inductive's parameters");
                    // Erasure opens the arm positionally; plicity is irrelevant.
                    let arm = self.variant_arm(
                        context,
                        &m,
                        (tag, &arm.body),
                        telescope,
                        &mask,
                        constructor,
                    )?;
                    arms.push(arm);
                }
                // A missing constructor without a default is provably impossible: an `Unreachable` arm, still declaring its payload binders to satisfy the constructor's arity.
                None if default.is_none() => {
                    let bindings = mask
                        .iter()
                        .filter(|&&erased| !erased)
                        .map(|_| self.builder.value(None))
                        .collect();
                    self.builder.open_block();
                    let block = self
                        .builder
                        .seal_block(curios_ersd::Terminator::Unreachable);
                    arms.push(curios_ersd::VariantArm {
                        constructor,
                        bindings,
                        block,
                    });
                }
                None => {}
            }
        }

        let default = default
            .map(|default| self.default_arm(context, &m, default))
            .transpose()?;

        Ok(self.bind(
            hint,
            curios_ersd::Rhs::MatchVariant {
                family: row.family,
                scrutinee,
                arms,
                default,
            },
        ))
    }

    /// One constructor arm: bind its payload (a kept field to a fresh value in payload order, an erased field to the unit constant), refine the scrutinee and indices, and erase the body as a tail at the opened motive.
    fn variant_arm(
        &mut self,
        context: &mut Context,
        m: &InductMatch<'_>,
        (tag, scope): (&Atom, &Scope<Many>),
        telescope: Telescope<Vec<Term>>,
        mask: &[bool],
        constructor: curios_ersd::ConstructorId,
    ) -> Result<curios_ersd::VariantArm, Error> {
        let hints = scope
            .hint_iter()
            .map(|label| label.map(str::to_string))
            .collect::<Vec<_>>();
        let labels = hints
            .iter()
            .map(|label| context.fresh(label.as_deref()))
            .collect::<Vec<_>>();
        let vars = labels.iter().map(Term::free_var).collect::<Vec<_>>();

        let mut bindings = Vec::new();
        for ((label, hint), &erased) in labels.iter().zip(hints).zip(mask) {
            if erased {
                let unit = self.unit();
                self.environment.bind(label, unit);
            } else {
                let value = self.builder.value(hint);
                self.environment
                    .bind(label, curios_ersd::Atom::Value(value));
                bindings.push(value);
            }
        }

        self.builder.open_block();
        let outcome = context.with_frame(|context| {
            let expected = refine_arm(context, m, tag, &labels, &vars, telescope)?;
            let body = scope.open(&vars.iter().collect::<Vec<_>>());
            self.walk(context, &body, &expected, None)
        })?;
        let block = self.seal(outcome);

        Ok(curios_ersd::VariantArm {
            constructor,
            bindings,
            block,
        })
    }

    /// The catch-all default: binds nothing and sees the unrefined head, so the motive opens at the match's actual parameters/indices and head.
    fn default_arm(
        &mut self,
        context: &mut Context,
        m: &InductMatch<'_>,
        default: &Term,
    ) -> Result<curios_ersd::BlockId, Error> {
        let default_refs = m.actual_indices.iter().chain([m.head]).collect::<Vec<_>>();
        let expected = m.motive.open(&default_refs);
        self.open_arm(context, &expected, default)
    }

    /// A match on an erasable (proof/type) scrutinee: no runtime tag, so the subsingleton elimination reduces to its single live arm, erased inline in the current block. Every payload binder is erased and resolves to the unit constant; the head is never erased (it is a dropped value).
    fn erase_erasable_scrutinee(
        &mut self,
        context: &mut Context,
        m: &InductMatch<'_>,
        induct_decl: &InductDecl,
        cases: &[(Atom, InductArm)],
        default: Option<&Term>,
    ) -> Result<Outcome, Error> {
        // No enumerated arm — a bare `_` ladder over a subsingleton: the default is the single live result and binds nothing.
        let Some((tag, scope)) = cases.iter().next() else {
            let default = default.expect("erase: erasable match with no arms has a default");
            let default_refs = m.actual_indices.iter().chain([m.head]).collect::<Vec<_>>();
            let expected = m.motive.open(&default_refs);
            return self.walk(context, default, &expected, None);
        };

        let telescope = induct_decl
            .instantiate(tag, m.params)
            .expect("erase: constructor instantiates at its inductive's parameters");
        let labels = scope
            .hint_iter()
            .map(|label| context.fresh(label))
            .collect::<Vec<_>>();
        let vars = labels.iter().map(Term::free_var).collect::<Vec<_>>();

        for label in &labels {
            let unit = self.unit();
            self.environment.bind(label, unit);
        }

        context.with_frame(|context| {
            let expected = refine_arm(context, m, tag, &labels, &vars, telescope)?;
            let body = scope.open(&vars.iter().collect::<Vec<_>>());
            self.walk(context, &body, &expected, None)
        })
    }
}

/// Assume the payload binders at their instantiated types, refine the scrutinee to `tag(vars)` and the actual indices to this case's targets, and return the arm body's expected type. Typing-only: it touches the context and emits nothing.
fn refine_arm(
    context: &mut Context,
    m: &InductMatch<'_>,
    tag: &Atom,
    labels: &[Free],
    vars: &[Term],
    telescope: Telescope<Vec<Term>>,
) -> Result<Term, Error> {
    let mut telescope = telescope;
    for (label, var) in labels.iter().zip(vars) {
        match telescope {
            Telescope::Cons(type_, rest) => {
                context.assume(label, &type_);
                telescope = rest.open(&[var]);
            }
            Telescope::Done(_) => unreachable!("erase: constructor arity checked by elaborate"),
        }
    }

    let target_indices = match &telescope {
        Telescope::Done(targets) => (**targets).clone(),
        Telescope::Cons(..) => unreachable!("erase: constructor arity checked by elaborate"),
    };

    let constructor_value = Term::variant_at(
        m.name.clone(),
        m.universes.to_vec(),
        m.params.to_vec(),
        tag.clone(),
        vars.to_vec(),
    );
    refine_head(context, m.head, &constructor_value)?;
    for (actual, target) in m.actual_indices.iter().zip(&target_indices) {
        refine_head(context, actual, target)?;
    }

    let arm_refs = target_indices
        .iter()
        .chain([&constructor_value])
        .collect::<Vec<_>>();
    Ok(m.motive.open(&arm_refs))
}
