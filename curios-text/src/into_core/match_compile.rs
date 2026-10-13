use {
    super::{Binder, Hoisted, Lowerer},
    crate::{
        BinPattern, Choose, ChooseArm, ChooseTest, Error, ListPattern, Match, MatchPattern,
        MatrixArm, NatLiteral, NatPattern, Pattern, PatternField, Term,
    },
    curios_utilities::Span,
    std::mem,
};

/// One unit of match lowering: borrows the [`Lowerer`] doing the surrounding term lowering — for name resolution, scoping, and recursing back into [`Lowerer::term`] or [`Lowerer::region`] at each arm's body — and writes a match as the matrix it is written as ([`curios_core::Matrix`]). The patterns are compiled where the scrutinee's type is known, by elaboration; what is decided here is what the syntax alone says.
pub(super) struct MatchCompiler<'l, 'a, 'b> {
    lowerer: &'l Lowerer<'a, 'b>,
}

impl<'l, 'a, 'b> MatchCompiler<'l, 'a, 'b> {
    pub(super) fn new(lowerer: &'l Lowerer<'a, 'b>) -> Self {
        Self { lowerer }
    }

    /// Leaf shim for the plain (non-region) lowering path — matches [`Self::headed`]'s `leaf: fn(&Self, &Term)` shape. Also passed as a bare function pointer from [`Lowerer::subterm`], so it needs to be visible there too.
    pub(super) fn term(&self, term: &Term) -> Result<curios_core::Term, Error> {
        self.lowerer.term(term)
    }

    /// Leaf shim for the region (bang-hoisting) lowering path.
    fn region(&self, term: &Term) -> Result<curios_core::Term, Error> {
        self.lowerer.region(term)
    }

    /// Desugars a `match` inside a region: the scrutinee's bangs are collected into `binds` (hoisted out — the scrutinee runs unconditionally), while each arm is desugared as its own region (branch-local effects). This mirrors the `Match` arm of `subterm`, swapping `self.term` for `collect` on the head and `region` on arm bodies.
    pub(super) fn match_region(
        &self,
        match_: &Match,
        binds: &mut Vec<Hoisted>,
    ) -> Result<curios_core::Term, Error> {
        let head = self.collect(&match_.head, binds)?;
        self.headed(head, &match_.motive, &match_.arms, Self::region)
    }

    /// Desugars a `choose` inside a region: the head arm's test mirrors the `subterm` path's right-fold, but only the *first* condition runs unconditionally, so only its bangs hoist into `binds`. Every deeper condition evaluates just when its predecessor is false, so each lowers as its own sub-region inside that false branch (see [`Self::ladder_region`]).
    pub(super) fn choose_region(
        &self,
        choose: &Choose,
        binds: &mut Vec<Hoisted>,
    ) -> Result<curios_core::Term, Error> {
        self.ladder_region(&choose.arms, &choose.default, binds)
    }

    /// Lowers a `choose`'s arms inside a region. The head arm's test (a condition or a bind scrutinee) collects its bangs into `binds` (the caller's region — it runs unconditionally at this level); its body is its own region; the rest of the ladder — reached only when this test fails — is lowered as a *fresh* region so deeper tests' bangs stay branch-local. With no arms left, the ladder is just its default, run unconditionally in the current region.
    fn ladder_region(
        &self,
        arms: &[ChooseArm],
        default: &Term,
        binds: &mut Vec<Hoisted>,
    ) -> Result<curios_core::Term, Error> {
        let Some((arm, rest)) = arms.split_first() else {
            return self.collect(default, binds);
        };
        let mut rest_binds = Vec::new();
        let rest_term = self.ladder_region(rest, default, &mut rest_binds)?;
        let rest_wrapped = self.wrap(rest_binds, rest_term)?;
        match &arm.test {
            ChooseTest::Cond(condition) => {
                let head = self.collect(condition, binds)?;
                let true_case = self.region(&arm.body)?;
                Ok(curios_core::Term::bool_match_scoped(
                    head,
                    curios_core::Term::match_motive_written(curios_core::Term::hole(
                        self.context.fresh_metavar(),
                    )),
                    rest_wrapped,
                    true_case,
                ))
            }
            ChooseTest::Bind { pattern, value } => {
                let value = self.collect(value, binds)?;
                self.bind_arm(
                    pattern,
                    value,
                    &arm.body,
                    rest_wrapped,
                    MatchCompiler::region,
                )
            }
        }
    }

    /// Lowers a `choose` bind arm `| pattern = value => body` — with `value` and `rest`, what it falls through to, already lowered — as the matrix of its one row over `value` that falls through to `rest`. A refutable pattern only: a bare-binder pattern cannot fail (a plain `let`) and is rejected.
    pub(super) fn bind_arm(
        &self,
        pattern: &MatchPattern,
        value: curios_core::Term,
        body: &Term,
        rest: curios_core::Term,
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Term, Error> {
        if matches!(pattern, MatchPattern::Binder(_)) {
            return Err(Error::BindArmIrrefutable);
        }

        Ok(curios_core::Term::matrix(curios_core::Matrix {
            head: value,
            motive: None,
            rows: vec![self.row(pattern, body, None, leaf)?],
            fallthrough: Some(rest),
        }))
    }

    /// Lowers a headed match — its scrutinee already lowered — as the matrix of its arms, each as it is written. `leaf` is the per-body lowering — [`Self::term`] on the plain path, [`Self::region`] on the region path — so both share this, mirroring every other `Match` arm's `term`/`region` split.
    ///
    /// Two refusals are read off the arms as they are written. A final arm that binds a name after arms that dispatch is a mistake where a default was meant ([`Error::MatchNamedCatchAll`]). And a written motive is only meaningful when the head itself dispatches on one carrier directly — every arm's *top-level* pattern being the same dispatchable shape, a final `_` apart — since that is the only case where a core `Match` node exists for the *original* scrutinee to attach the motive to: a tuple/struct-headed or plain-binder match never builds one, and the motive would be silently discarded ([`Error::MatrixMotiveRequiresCtorHead`]).
    pub(super) fn headed(
        &self,
        head: curios_core::Term,
        motive: &Option<Term>,
        arms: &[MatrixArm],
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Term, Error> {
        let dispatching = dispatching(arms)?;
        let homogeneous = dispatching.first().is_none_or(|first| {
            first.pattern.is_dispatchable()
                && dispatching
                    .iter()
                    .all(|arm| mem::discriminant(&arm.pattern) == mem::discriminant(&first.pattern))
        });
        if !homogeneous && motive.is_some() {
            return Err(Error::MatrixMotiveRequiresCtorHead);
        }

        let rows = arms
            .iter()
            .map(|arm| self.row(&arm.pattern, &arm.body, arm.span.as_ref(), leaf))
            .collect::<Result<Vec<_>, Error>>()?;
        let motive = motive
            .as_ref()
            .map(|motive| self.term(motive))
            .transpose()?;

        Ok(curios_core::Term::matrix(curios_core::Matrix {
            head,
            motive,
            rows,
            fallthrough: None,
        }))
    }

    /// One arm as a row: its pattern, and its body lowered under the names the pattern binds, as the function over those binders — placed where the pattern is written, which is where a row no value reaches is reported.
    fn row(
        &self,
        pattern: &MatchPattern,
        body: &Term,
        span: Option<&Span>,
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Row, Error> {
        let mut binders = Vec::new();
        let pattern = self.pattern(pattern, &mut binders);
        let body = self.bound(&binders, || leaf(self, body))?;
        // A pattern's binder has no type written: the scrutinee's gives it one where the pattern is compiled.
        let function = curios_core::Term::func(
            binders
                .iter()
                .map(|(_, binder)| {
                    (
                        *binder,
                        curios_core::Term::hole(self.context.fresh_metavar()),
                    )
                })
                .collect::<Vec<_>>(),
            body,
        );

        Ok(curios_core::Row {
            pattern,
            body: match span {
                Some(span) => self.context.placed(span, function),
                None => function,
            },
        })
    }

    /// A pattern as Core holds it, with every name it binds minted onto `binders` in the order it writes them.
    fn pattern(&self, pattern: &MatchPattern, binders: &mut Vec<Binder>) -> curios_core::Pattern {
        match pattern {
            MatchPattern::Binder(name) => {
                binders.push(self.pattern_binder(name));

                curios_core::Pattern::Binder
            }
            MatchPattern::Variant { tag, args } => curios_core::Pattern::Variant {
                tag: curios_core::Atom::from(tag.as_str()),
                args: args
                    .iter()
                    .map(|(plicity, arg)| (*plicity, self.pattern(arg, binders)))
                    .collect(),
            },
            MatchPattern::Tuple(fields) => curios_core::Pattern::Tuple(
                fields
                    .iter()
                    .map(|field| (field.label.clone(), self.pattern(&field.value, binders)))
                    .collect(),
            ),
            MatchPattern::Struct { head, fields } => curios_core::Pattern::Struct {
                head: head.clone(),
                fields: fields
                    .iter()
                    .map(|field| (field.label.clone(), self.pattern(&field.value, binders)))
                    .collect(),
            },
            MatchPattern::Bool(value) => curios_core::Pattern::Bool(*value),
            MatchPattern::Char(character) => curios_core::Pattern::Character(*character),
            MatchPattern::Nat(NatPattern::Zero) => curios_core::Pattern::Zero,
            MatchPattern::Nat(NatPattern::Succ { pred_label, ih }) => {
                binders.push(self.pattern_binder(pred_label));

                curios_core::Pattern::Succ(self.hypothesis(ih, binders))
            }
            MatchPattern::Nat(NatPattern::Lit(NatLiteral(value, _))) => {
                curios_core::Pattern::Literal(value.clone())
            }
            MatchPattern::List(ListPattern::Nil) => curios_core::Pattern::Nil,
            MatchPattern::List(ListPattern::Cons {
                head_label,
                tail_label,
                ih,
            }) => {
                binders.push(self.pattern_binder(head_label));
                binders.push(self.pattern_binder(tail_label));

                curios_core::Pattern::Cons(self.hypothesis(ih, binders))
            }
            MatchPattern::Bin(BinPattern::End(grain)) => curios_core::Pattern::End(*grain),
            MatchPattern::Bin(BinPattern::Atom {
                grain,
                head_label,
                tail_label,
                ih,
            }) => {
                binders.push(self.pattern_binder(head_label));
                binders.push(self.pattern_binder(tail_label));

                curios_core::Pattern::Atom(*grain, self.hypothesis(ih, binders))
            }
        }
    }

    /// What a fold's hypothesis is bound by, where one is written, its names minted onto `binders` after the binders it stands beside.
    fn hypothesis(
        &self,
        hypothesis: &Option<Pattern>,
        binders: &mut Vec<Binder>,
    ) -> Option<curios_core::Bind> {
        hypothesis
            .as_ref()
            .map(|pattern| self.bind(pattern, binders))
    }

    fn bind(&self, pattern: &Pattern, binders: &mut Vec<Binder>) -> curios_core::Bind {
        match pattern {
            Pattern::Binder(name) => {
                binders.push(self.pattern_binder(name));

                curios_core::Bind::Binder
            }
            Pattern::Tuple(fields) | Pattern::Struct { fields, .. } => {
                curios_core::Bind::Fields(self.binds(fields, binders))
            }
        }
    }

    fn binds(
        &self,
        fields: &[PatternField],
        binders: &mut Vec<Binder>,
    ) -> Vec<(Option<String>, curios_core::Bind)> {
        fields
            .iter()
            .map(|field| (field.label.clone(), self.bind(&field.value, binders)))
            .collect()
    }
}

/// The arms of a headed match that are not its default: every arm, less a final top-level bare `_` after arms that each dispatch — a constructor tag or any of the hardcoded-carrier leaves ([`MatchPattern::is_dispatchable`]). A *named* final binder in that position is a mistake ([`Error::MatchNamedCatchAll`]). A lone `_` with no concrete arms is not a default — it stays a binding — and neither is a `_` after tuple/struct arms, which project exhaustively and need none.
fn dispatching(arms: &[MatrixArm]) -> Result<&[MatrixArm], Error> {
    match arms.split_last() {
        Some((last, init))
            if !init.is_empty() && init.iter().all(|arm| arm.pattern.is_dispatchable()) =>
        {
            match &last.pattern {
                MatchPattern::Binder(name) if name == "_" => Ok(init),
                MatchPattern::Binder(_) => Err(Error::MatchNamedCatchAll),
                _ => Ok(arms),
            }
        }
        _ => Ok(arms),
    }
}

impl<'l, 'a, 'b> std::ops::Deref for MatchCompiler<'l, 'a, 'b> {
    type Target = Lowerer<'a, 'b>;

    fn deref(&self) -> &Lowerer<'a, 'b> {
        self.lowerer
    }
}
