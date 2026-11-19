use {
    super::{Binder, Lowerer, RegionFrame, bindable},
    crate::Error,
    curios_common::{Said, Span},
    curios_surface::{
        Alternative, Elements, Guard, HeadlessArm, HeadlessMatch, Label, Match, MatchPattern,
        MatchPatternField, MatrixArm, NatLiteral, NatPattern, Subterm, Term,
    },
    std::collections::BTreeSet,
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
        self.headed(&match_.head, head, &match_.arms, Self::region)
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
            arms: rows
                .into_iter()
                .map(|row| curios_core::Arm {
                    alternatives: vec![row],
                })
                .collect(),
        }))
    }

    /// One arm of a match with no head as its row: `_` over the `()` the match is over, which binds nothing.
    fn unheaded(
        &self,
        guards: &[Guard],
        body: &Term,
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Row, Error> {
        self.row(
            curios_core::Pattern::WILDCARD,
            Vec::new(),
            guards,
            body,
            None,
            leaf,
        )
    }

    /// Lowers a headed match — its scrutinee already lowered, `written` the head as it is written — as the matrix of its arms, each as it is written. `leaf` is the per-body lowering — [`Self::term`] on the plain path, [`Self::region`] on the region path — so both share this, mirroring every other `Match` arm's `term`/`region` split.
    ///
    /// A final arm that binds a name after arms that dispatch takes every value they leave under that name, and where the head is a name it renames what the head already names, which `renaming-binder` reports ([`Self::renaming`]). Whether the name is a constructor of what the arms dispatch over is elaboration's to say, which reads the scrutinee's family.
    pub fn headed(
        &self,
        written: &Term,
        head: curios_core::Term,
        arms: &[MatrixArm],
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Term, Error> {
        if let Some(lint) = self.renaming(written, arms) {
            self.lowerer.context.lint(lint);
        }

        let arms = arms
            .iter()
            .map(|arm| self.arm(&arm.alternatives, &arm.guards, &arm.body, leaf))
            .collect::<Result<Vec<_>, Error>>()?;

        Ok(curios_core::Term::matrix(curios_core::Matrix {
            head,
            arms,
        }))
    }

    /// The `renaming-binder` lint of a match whose head is written as a name and whose final arm binds the default its arms leave ([`default_binder`]): the arm's name is the head's again, so it reads under a second name what the first already names. Decided here, where the text as written is the last fact it needs.
    fn renaming(&self, written: &Term, arms: &[MatrixArm]) -> Option<curios_core::Lint> {
        let Subterm::Name(head) = written.as_subterm() else {
            return None;
        };
        let (binder, span) = default_binder(arms)?;
        let head = head.join();
        let mut said = Said::line("renaming binder");
        said.stating(format_args!(
            "`{binder}` renames `{head}`, which this match takes apart"
        ));
        said.stating(format_args!("read `{head}`, or write `_`"));

        Some(curios_core::Lint {
            kind: curios_core::LintKind::RenamingBinder,
            said,
            at: span.and_then(|span| self.lowerer.context.extent(span)),
        })
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
                let pattern = self.whole(&alternative.pattern, &mut binders)?;
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
                    let pattern = self.whole(&pattern.pattern, &mut binders)?;

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

    /// [`Self::pattern`] for an arm's or a bind guard's whole pattern, where a `_` takes whatever the patterns before it left rather than a part of a value: no place is given it.
    fn whole(
        &self,
        pattern: &MatchPattern,
        binders: &mut Vec<Binder>,
    ) -> Result<curios_core::Pattern, Error> {
        match pattern {
            MatchPattern::Binder(name) if !bindable(name.as_str()) => {
                Ok(curios_core::Pattern::WILDCARD)
            }
            _ => self.pattern(pattern, binders),
        }
    }

    /// A pattern as Core holds it, with every name it binds minted onto `binders` in the order it writes them. A struct pattern's head is resolved as a struct literal's is ([`Lowerer::resolve_nominal`]): the scrutinee's type is held to the structure it names where the pattern is compiled.
    fn pattern(
        &self,
        pattern: &MatchPattern,
        binders: &mut Vec<Binder>,
    ) -> Result<curios_core::Pattern, Error> {
        Ok(match pattern {
            // A `_` inside a pattern takes its place among the declaration's written binders: a payload or a component it leaves is one a lint may ask the class of.
            MatchPattern::Binder(name) if !bindable(name.as_str()) => {
                curios_core::Pattern::Wildcard(curios_core::WrittenPlace(self.wildcard_place(name)))
            }
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
            MatchPattern::Char(character) => curios_core::Pattern::Character(character.value),
            MatchPattern::Str(literal) => curios_core::Pattern::Str(literal.value.clone()),
            MatchPattern::Nat(NatPattern::Zero) => curios_core::Pattern::Zero,
            MatchPattern::Nat(NatPattern::Succ { pred_label, step }) => {
                curios_core::Pattern::Succ {
                    predecessor: self.slot(pred_label, binders),
                    step: step.0.clone(),
                }
            }
            MatchPattern::Nat(NatPattern::Lit(NatLiteral(value, _))) => {
                curios_core::Pattern::Literal(value.clone())
            }
            MatchPattern::List(elements) => {
                curios_core::Pattern::List(self.elements(elements, binders)?)
            }
            MatchPattern::Bytes(elements) => {
                curios_core::Pattern::Bytes(self.elements(elements, binders)?)
            }
        })
    }

    /// A run's elements as Core holds them, each a pattern, then its rest where it is open, every name minted in the order the run writes it.
    fn elements(
        &self,
        elements: &Elements,
        binders: &mut Vec<Binder>,
    ) -> Result<curios_core::Elements, Error> {
        Ok(match elements {
            Elements::Closed(elements) => curios_core::Elements::Closed(
                elements
                    .iter()
                    .map(|element| self.pattern(element, binders))
                    .collect::<Result<_, Error>>()?,
            ),
            Elements::Open { first, then, rest } => {
                let first = Box::new(self.pattern(first, binders)?);
                let then = then
                    .iter()
                    .map(|element| self.pattern(element, binders))
                    .collect::<Result<_, Error>>()?;
                let rest = self.slot(rest, binders);

                curios_core::Elements::Open { first, then, rest }
            }
        })
    }

    /// A successor's predecessor or an open run's rest, its name minted onto `binders` where it writes one: a `_` binds nothing.
    fn slot(&self, name: &Label, binders: &mut Vec<Binder>) -> curios_core::Slot {
        match bindable(name.as_str()) {
            true => {
                binders.push(self.pattern_binder(name));

                curios_core::Slot::Binder
            }
            false => curios_core::Slot::Wildcard,
        }
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
}

/// The name a headed match's final arm binds where it is the default its arms leave: one binder, no guard, after arms that each dispatch — a constructor tag or any of the hardcoded-carrier leaves ([`MatchPattern::is_dispatchable`]), in each of their alternatives — with where it is written. None for a `_`, which binds nothing, for a lone binder with no arm before it, which is the whole match's binding, and for one after tuple or struct arms, which project exhaustively and leave nothing.
fn default_binder(arms: &[MatrixArm]) -> Option<(&str, Option<&Span>)> {
    let dispatches = |arm: &MatrixArm| {
        arm.alternatives
            .iter()
            .all(|alternative| alternative.pattern.is_dispatchable())
    };
    let (last, init) = arms.split_last()?;
    if init.is_empty() || !init.iter().all(dispatches) || !last.guards.is_empty() {
        return None;
    }
    match last.alternatives.as_slice() {
        [
            Alternative {
                pattern: MatchPattern::Binder(name),
                span,
            },
        ] if name.as_str() != "_" => Some((name.as_str(), name.span().or(span.as_ref()))),
        _ => None,
    }
}

/// `error` at `span`, where the tree holds one: a refusal about a binder stands on it and not on the match around it, which is where an enclosing lowering places whatever reaches it bare.
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
        | MatchPattern::Str(_)
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
