use {
    super::{Binder, Lowerer, RegionFrame, bindable},
    crate::Error,
    curios_common::Span,
    curios_surface::{
        Alternative, BytesPattern, Choose, ChooseArm, ChooseTest, Guard, HeadlessArm,
        HeadlessMatch, ListPattern, Match, MatchPattern, MatchPatternField, MatrixArm, NatLiteral,
        NatPattern, Pattern, PatternField, Term,
    },
    std::{collections::BTreeSet, mem},
};

/// One unit of match lowering: borrows the [`Lowerer`] doing the surrounding term lowering — for name resolution, scoping, and recursing back into [`Lowerer::term`] or [`Lowerer::region`] at each arm's body — and writes a match as the matrix it is written as ([`curios_core::Matrix`]). The patterns are compiled where the scrutinee's type is known, by elaboration; what is decided here is what the syntax alone says.
pub struct MatchCompiler<'l, 'a, 'b> {
    lowerer: &'l Lowerer<'a, 'b>,
}

impl<'l, 'a, 'b> MatchCompiler<'l, 'a, 'b> {
    pub fn new(lowerer: &'l Lowerer<'a, 'b>) -> Self {
        Self { lowerer }
    }

    /// Leaf shim for the plain (non-region) lowering path — matches [`Self::headed`]'s `leaf: fn(&Self, &Term)` shape. Also passed as a bare function pointer from [`Lowerer::subterm`], so it needs to be visible there too.
    pub fn term(&self, term: &Term) -> Result<curios_core::Term, Error> {
        self.lowerer.term(term)
    }

    /// Leaf shim for the region (bang-hoisting) lowering path.
    fn region(&self, term: &Term) -> Result<curios_core::Term, Error> {
        self.lowerer.region(term)
    }

    /// Desugars a `match` inside a region: the scrutinee's bangs are collected into `binds` (hoisted out — the scrutinee runs unconditionally), while each arm is desugared as its own region (branch-local effects). This mirrors the `Match` arm of `subterm`, swapping `self.term` for `collect` on the head and `region` on arm bodies.
    pub fn match_region(
        &self,
        match_: &Match,
        binds: &mut Vec<RegionFrame>,
    ) -> Result<curios_core::Term, Error> {
        let head = self.collect(&match_.head, binds)?;
        self.headed(head, &match_.motive, &match_.arms, Self::region)
    }

    /// A match with no head inside a region: there is no scrutinee to run ahead of the arms, so nothing hoists, and each guard and each body is a region of its own.
    pub fn headless_region(&self, headless: &HeadlessMatch) -> Result<curios_core::Term, Error> {
        self.headless(headless, Self::region)
    }

    /// A match with no head as the match of no columns: a match over `()` whose rows write `_`, each with its arm's guards and body, the default last. `leaf` lowers each guard and each body, as [`Self::headed`]'s does.
    pub fn headless(
        &self,
        HeadlessMatch { arms, default }: &HeadlessMatch,
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Term, Error> {
        let mut rows = arms
            .iter()
            .map(|HeadlessArm { guards, body }| self.unheaded(guards, body, leaf))
            .collect::<Result<Vec<_>, Error>>()?;
        rows.push(self.unheaded(&[], default, leaf)?);

        Ok(curios_core::Term::matrix(curios_core::Matrix {
            head: curios_core::Term::tuple(Vec::<curios_core::Term>::new()),
            motive: None,
            arms: rows
                .into_iter()
                .map(|row| curios_core::Arm {
                    alternatives: vec![row],
                })
                .collect(),
        }))
    }

    /// One arm of a match with no head as its row: `_` over the `()` the match is over, which binds nothing its guards or body can name.
    fn unheaded(
        &self,
        guards: &[Guard],
        body: &Term,
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Row, Error> {
        let unnamed = self.mint([String::new()]);

        self.row(
            curios_core::Pattern::Binder,
            unnamed,
            guards,
            body,
            None,
            leaf,
        )
    }

    /// Desugars a `choose` inside a region: the head arm's test mirrors the `subterm` path's right-fold, but only the *first* condition runs unconditionally, so only its bangs hoist into `binds`. Every deeper condition evaluates just when its predecessor is false, so each lowers as its own sub-region inside that false branch (see [`Self::ladder_region`]).
    pub fn choose_region(
        &self,
        choose: &Choose,
        binds: &mut Vec<RegionFrame>,
    ) -> Result<curios_core::Term, Error> {
        self.ladder_region(&choose.arms, &choose.default, binds)
    }

    /// Lowers a `choose`'s arms inside a region. The head arm's test (a condition or a bind scrutinee) collects its bangs into `binds` (the caller's region — it runs unconditionally at this level); its body is its own region; the rest of the ladder — reached only when this test fails — is lowered as a *fresh* region so deeper tests' bangs stay branch-local. With no arms left, the ladder is just its default, run unconditionally in the current region.
    fn ladder_region(
        &self,
        arms: &[ChooseArm],
        default: &Term,
        binds: &mut Vec<RegionFrame>,
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
            ChooseTest::Bind {
                alternatives,
                value,
            } => {
                let value = self.collect(value, binds)?;
                self.bind_arm(
                    alternatives,
                    value,
                    &arm.body,
                    rest_wrapped,
                    MatchCompiler::region,
                )
            }
        }
    }

    /// Lowers a `choose` bind arm `| pattern | … = value => body` — with `value` and `rest`, the rest of its ladder, already lowered — as the matrix of two arms over `value`: its own, and a `_` whose body is `rest`, which a value no alternative matches takes. Refutable alternatives only: a bare binder cannot fail (a plain `let`) and is rejected.
    pub fn bind_arm(
        &self,
        alternatives: &[Alternative],
        value: curios_core::Term,
        body: &Term,
        rest: curios_core::Term,
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Term, Error> {
        for alternative in alternatives {
            if let MatchPattern::Binder(name) = &alternative.pattern {
                return Err(placed(
                    Error::BindCannotFail,
                    name.span().or(alternative.span.as_ref()),
                ));
            }
        }

        // The `_` binds nothing its body can name.
        let (_, unnamed) = self.mint([String::new()]).remove(0);
        let rest = curios_core::Row {
            pattern: curios_core::Pattern::Binder,
            guards: Vec::new(),
            body: curios_core::Term::func(
                vec![(
                    unnamed,
                    curios_core::Term::hole(self.context.fresh_metavar()),
                )],
                rest,
            ),
        };

        Ok(curios_core::Term::matrix(curios_core::Matrix {
            head: value,
            motive: None,
            arms: vec![
                self.arm(alternatives, &[], body, leaf)?,
                curios_core::Arm {
                    alternatives: vec![rest],
                },
            ],
        }))
    }

    /// Lowers a headed match — its scrutinee already lowered — as the matrix of its arms, each as it is written. `leaf` is the per-body lowering — [`Self::term`] on the plain path, [`Self::region`] on the region path — so both share this, mirroring every other `Match` arm's `term`/`region` split.
    ///
    /// Two refusals are read off the arms as they are written. A final arm that binds a name after arms that dispatch is a mistake where a default was meant ([`Error::MatchNamedCatchAll`]). And a written motive is only meaningful when the head itself dispatches on one carrier directly — every alternative's *top-level* pattern being the same dispatchable shape, a final `_` apart — since that is the only case where a core `Match` node exists for the *original* scrutinee to attach the motive to: a tuple/struct-headed or plain-binder match never builds one, and the motive would be silently discarded ([`Error::MatrixMotiveRequiresCtorHead`]).
    pub fn headed(
        &self,
        head: curios_core::Term,
        motive: &Option<Term>,
        arms: &[MatrixArm],
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Term, Error> {
        let mut patterns = dispatching(arms)?
            .iter()
            .flat_map(|arm| &arm.alternatives)
            .map(|alternative| &alternative.pattern);
        let homogeneous = patterns.next().is_none_or(|first| {
            first.is_dispatchable()
                && patterns.all(|pattern| mem::discriminant(pattern) == mem::discriminant(first))
        });
        if !homogeneous && let Some(motive) = motive {
            return Err(placed(Error::MatrixMotiveRequiresCtorHead, motive.span()));
        }

        let arms = arms
            .iter()
            .map(|arm| self.arm(&arm.alternatives, &arm.guards, &arm.body, leaf))
            .collect::<Result<Vec<_>, Error>>()?;
        let motive = motive
            .as_ref()
            .map(|motive| self.term(motive))
            .transpose()?;

        Ok(curios_core::Term::matrix(curios_core::Matrix {
            head,
            motive,
            arms,
        }))
    }

    /// One arm: a row for each alternative, each its pattern and the guards and body lowered again under that pattern's names, so that each is typed apart with metavariables of its own. Every alternative binds the names the first does, since the guards and the body read each name of the arm in each of them, and a name an alternative lacks would read whatever else it names there ([`Error::MatchAlternativeNames`]).
    fn arm(
        &self,
        alternatives: &[Alternative],
        guards: &[Guard],
        body: &Term,
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Arm, Error> {
        let mut names = None;
        let alternatives = alternatives
            .iter()
            .map(|alternative| {
                let mut binders = Vec::new();
                let pattern = self.pattern(&alternative.pattern, &mut binders)?;
                let written = binders
                    .iter()
                    .map(|(name, _)| name.clone())
                    .filter(|name| bindable(name))
                    .collect::<BTreeSet<_>>();
                let first = names.get_or_insert_with(|| written.clone());
                if let Some(name) = first.symmetric_difference(&written).next() {
                    return Err(placed(
                        Error::MatchAlternativeNames { name: name.clone() },
                        alternative.span.as_ref(),
                    ));
                }

                self.row(
                    pattern,
                    binders,
                    guards,
                    body,
                    alternative.span.as_ref(),
                    leaf,
                )
            })
            .collect::<Result<Vec<_>, Error>>()?;

        Ok(curios_core::Arm { alternatives })
    }

    /// One alternative as a row: its pattern, each guard lowered under the names written before it, and the body under all of them, each as the function over those binders — the body placed where the pattern is written, which is where a row no value reaches is reported. A bind guard's pattern binds its names after the ones before it, and is refused where it cannot fail ([`Error::BindCannotFail`]).
    fn row(
        &self,
        pattern: curios_core::Pattern,
        mut binders: Vec<Binder>,
        guards: &[Guard],
        body: &Term,
        span: Option<&Span>,
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Row, Error> {
        let mut lowered = Vec::with_capacity(guards.len());
        for guard in guards {
            lowered.push(match guard {
                Guard::Holds(condition) => {
                    curios_core::Guard::Holds(self.function(&binders, condition, leaf)?)
                }
                Guard::Binds { pattern, value } => {
                    if !takes_apart(&pattern.pattern) {
                        return Err(placed(Error::BindCannotFail, pattern.span.as_ref()));
                    }
                    let value = self.function(&binders, value, leaf)?;
                    let pattern = self.pattern(&pattern.pattern, &mut binders)?;

                    curios_core::Guard::Binds { pattern, value }
                }
            });
        }
        let function = self.function(&binders, body, leaf)?;

        Ok(curios_core::Row {
            pattern,
            guards: lowered,
            body: match span {
                Some(span) => self.context.placed(span, function),
                None => function,
            },
        })
    }

    /// `term` lowered under `binders` as the function over them. A pattern's binder has no type written: the scrutinee's gives it one where the pattern is compiled, and a bind guard's the value's.
    fn function(
        &self,
        binders: &[Binder],
        term: &Term,
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Term, Error> {
        let body = self.bound(binders, || leaf(self, term))?;

        Ok(curios_core::Term::func(
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
        ))
    }

    /// A pattern as Core holds it, with every name it binds minted onto `binders` in the order it writes them. A struct pattern's head is resolved as a struct literal's is ([`Lowerer::resolve_nominal`]): the scrutinee's type is held to the structure it names where the pattern is compiled.
    fn pattern(
        &self,
        pattern: &MatchPattern,
        binders: &mut Vec<Binder>,
    ) -> Result<curios_core::Pattern, Error> {
        Ok(match pattern {
            MatchPattern::Binder(name) => {
                binders.push(self.pattern_binder(name));

                curios_core::Pattern::Binder
            }
            MatchPattern::Variant { tag, args } => curios_core::Pattern::Variant {
                tag: curios_core::Atom::from(tag.as_str()),
                args: args
                    .iter()
                    .map(|(plicity, arg)| Ok((*plicity, self.pattern(arg, binders)?)))
                    .collect::<Result<_, Error>>()?,
            },
            MatchPattern::Tuple(fields) => {
                curios_core::Pattern::Tuple(self.fields(fields, binders)?)
            }
            MatchPattern::Struct { head, fields } => curios_core::Pattern::Struct {
                head: head.join(),
                structure: match self.resolve_nominal(head)? {
                    Ok(structure) => curios_core::Mention::Declaration(structure),
                    Err(unbound) => curios_core::Mention::Local(unbound),
                },
                fields: self.fields(fields, binders)?,
            },
            MatchPattern::Bool(value) => curios_core::Pattern::Bool(*value),
            MatchPattern::Char(character) => curios_core::Pattern::Character(*character),
            MatchPattern::Nat(NatPattern::Zero) => curios_core::Pattern::Zero,
            MatchPattern::Nat(NatPattern::Succ { pred_label, ih }) => {
                binders.push(self.pattern_binder(pred_label));

                curios_core::Pattern::Succ(self.hypothesis(ih, binders)?)
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

                curios_core::Pattern::Cons(self.hypothesis(ih, binders)?)
            }
            MatchPattern::Bytes(BytesPattern::End) => curios_core::Pattern::End,
            MatchPattern::Bytes(BytesPattern::Atom {
                head_label,
                tail_label,
                ih,
            }) => {
                binders.push(self.pattern_binder(head_label));
                binders.push(self.pattern_binder(tail_label));

                curios_core::Pattern::Atom(self.hypothesis(ih, binders)?)
            }
        })
    }

    /// The fields of a tuple or struct pattern, each under the label it is written with.
    fn fields(
        &self,
        fields: &[MatchPatternField],
        binders: &mut Vec<Binder>,
    ) -> Result<Vec<(Option<String>, curios_core::Pattern)>, Error> {
        fields
            .iter()
            .map(|field| Ok((field.label.clone(), self.pattern(&field.value, binders)?)))
            .collect()
    }

    /// What a fold's hypothesis is bound by, where one is written, its names minted onto `binders` after the binders it stands beside.
    fn hypothesis(
        &self,
        hypothesis: &Option<Pattern>,
        binders: &mut Vec<Binder>,
    ) -> Result<Option<curios_core::Bind>, Error> {
        hypothesis
            .as_ref()
            .map(|pattern| self.bind(pattern, binders))
            .transpose()
    }

    fn bind(
        &self,
        pattern: &Pattern,
        binders: &mut Vec<Binder>,
    ) -> Result<curios_core::Bind, Error> {
        Ok(match pattern {
            Pattern::Binder(name) => {
                binders.push(self.pattern_binder(name));

                curios_core::Bind::Binder
            }
            Pattern::Tuple(fields) => curios_core::Bind::Fields(self.binds(fields, binders)?),
            Pattern::Struct { head, fields } => curios_core::Bind::Struct {
                head: head.join(),
                structure: match self.resolve_nominal(head)? {
                    Ok(structure) => curios_core::Mention::Declaration(structure),
                    Err(unbound) => curios_core::Mention::Local(unbound),
                },
                fields: self.binds(fields, binders)?,
            },
        })
    }

    fn binds(
        &self,
        fields: &[PatternField],
        binders: &mut Vec<Binder>,
    ) -> Result<Vec<(Option<String>, curios_core::Bind)>, Error> {
        fields
            .iter()
            .map(|field| Ok((field.label.clone(), self.bind(&field.value, binders)?)))
            .collect()
    }
}

/// The arms of a headed match that are not its default: every arm, less a final top-level bare `_` with no guard after arms that each dispatch — a constructor tag or any of the hardcoded-carrier leaves ([`MatchPattern::is_dispatchable`]), in each of their alternatives. A *named* final binder with no guard in that position is a mistake ([`Error::MatchNamedCatchAll`]). A lone `_` with no concrete arms is not a default — it stays a binding — and neither is a `_` after tuple/struct arms, which project exhaustively and need none, nor a `_` beside other alternatives, which meets them, nor a guarded one, which takes nothing for itself.
fn dispatching(arms: &[MatrixArm]) -> Result<&[MatrixArm], Error> {
    let dispatches = |arm: &MatrixArm| {
        arm.alternatives
            .iter()
            .all(|alternative| alternative.pattern.is_dispatchable())
    };

    match arms.split_last() {
        Some((last, init))
            if !init.is_empty() && init.iter().all(dispatches) && last.guards.is_empty() =>
        {
            match last.alternatives.as_slice() {
                [
                    Alternative {
                        pattern: MatchPattern::Binder(name),
                        ..
                    },
                ] if name.as_str() == "_" => Ok(init),
                [
                    Alternative {
                        pattern: MatchPattern::Binder(name),
                        span,
                    },
                ] => Err(placed(
                    Error::MatchNamedCatchAll {
                        binder: name.to_string(),
                    },
                    name.span().or(span.as_ref()),
                )),
                _ => Ok(arms),
            }
        }
        _ => Ok(arms),
    }
}

/// `error` at `span`, where the tree holds one: a refusal about a binder or a motive stands on it and not on the match around it, which is where an enclosing lowering places whatever reaches it bare.
fn placed(error: Error, span: Option<&Span>) -> Error {
    match span {
        Some(span) => error.at(span.clone()),
        None => error,
    }
}

/// Whether `pattern` takes some part of a value apart, and so can fail: a binder takes nothing apart, and a tuple or a structure takes apart what one of its fields does.
fn takes_apart(pattern: &MatchPattern) -> bool {
    match pattern {
        MatchPattern::Binder(_) => false,
        MatchPattern::Tuple(fields) | MatchPattern::Struct { fields, .. } => {
            fields.iter().any(|field| takes_apart(&field.value))
        }
        MatchPattern::Variant { .. }
        | MatchPattern::Bool(_)
        | MatchPattern::Char(_)
        | MatchPattern::Nat(_)
        | MatchPattern::List(_)
        | MatchPattern::Bytes(_) => true,
    }
}

impl<'l, 'a, 'b> std::ops::Deref for MatchCompiler<'l, 'a, 'b> {
    type Target = Lowerer<'a, 'b>;

    fn deref(&self) -> &Lowerer<'a, 'b> {
        self.lowerer
    }
}
