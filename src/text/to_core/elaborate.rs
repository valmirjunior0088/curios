use {
    super::Context,
    crate::{
        core,
        text::{
            BinLiteral, Error, Let, Match, Motive, Nat, NatLiteral, NatMatch, Prim, Subterm, Term,
        },
    },
    num_bigint::BigUint,
};

pub struct Elaborate<'a, 'b> {
    context: &'a Context<'b>,
}

/// The active bind of a `with` region: an atomic term denoting a binary bind
/// `(M A, A -> M B) -> M B`. [`Elaborate::instantiate`] re-elaborates `term` (so its
/// `?` holes are fresh per `!` site) and applies it to `(action, continuation)`.
struct Bind<'t> {
    term: &'t Term,
}

impl<'a, 'b> Elaborate<'a, 'b> {
    pub fn new(context: &'a Context<'b>) -> Self {
        Self { context }
    }

    pub fn term(&self, term: &Term) -> Result<core::Term, Error> {
        let span = term.span().cloned();
        let elaborated = match span.as_ref() {
            Some(s) => self
                .subterm(term.as_subterm())
                .map_err(|error| error.at(s.clone()))?,
            None => self.subterm(term.as_subterm())?,
        };
        Ok(match span {
            Some(s) => core::Term::spanned(s, elaborated),
            None => elaborated,
        })
    }

    fn subterm(&self, term: &Subterm) -> Result<core::Term, Error> {
        Ok(match term {
            Subterm::Type => core::Term::type_(),
            Subterm::Hole => core::Term::metavar(self.context.fresh_metavar()),
            Subterm::Prim(prim) => core::Term::prim(self.prim(prim)?),
            Subterm::Name(name) => {
                let resolved = if name.is_abs() || !name.is_single() {
                    self.context.resolve_term_name(name)?.join()
                } else {
                    let label = name.head();

                    match self.context.bindings().get(label) {
                        Some(full) => full.join(),
                        None => label.to_string(),
                    }
                };

                core::Term::var(core::Var::free(resolved))
            }
            Subterm::FuncType(ft) => core::Term::func_type_marked(
                ft.params
                    .iter()
                    .map(|(plicity, label, ty)| {
                        Ok((*plicity, label.clone().unwrap_or_default(), self.term(ty)?))
                    })
                    .collect::<Result<Vec<_>, Error>>()?,
                self.term(&ft.output)?,
            ),
            Subterm::Func(func) => {
                // Each parameter's domain is its annotation lowered, or — for the
                // `(x) => …` sugar — a fresh hole (exactly like `Subterm::Hole`).
                // The hole is solved against the expected function type when the
                // lambda is checked, or synthesized from the annotation when the
                // lambda is inferred. Core's `func` constructor closes the param
                // names over both the body and the later (dependent) domains.
                let params = func
                    .params
                    .iter()
                    .map(|(name, annotation)| {
                        let domain = match annotation {
                            Some(ty) => self.term(ty)?,
                            None => core::Term::metavar(self.context.fresh_metavar()),
                        };
                        Ok((name.clone(), domain))
                    })
                    .collect::<Result<Vec<_>, Error>>()?;
                core::Term::func(params, self.term(&func.body)?)
            }
            Subterm::Apply(apply) => core::Term::apply_marked(
                self.term(&apply.head)?,
                apply
                    .params
                    .iter()
                    .map(|(plicity, p)| Ok((*plicity, self.term(p)?)))
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            Subterm::TupleType(tt) => core::Term::tuple_type(
                tt.fields
                    .iter()
                    .map(|(label, type_)| {
                        Ok((label.clone().unwrap_or_default(), self.term(type_)?))
                    })
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            Subterm::Tuple(tuple) => core::Term::tuple(
                tuple
                    .fields
                    .iter()
                    .map(|field| self.term(field))
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            Subterm::Proj(proj) => core::Term::proj(self.term(&proj.head)?, proj.index),
            Subterm::Match(match_) => match match_ {
                Match::Bln(bm) => {
                    let (label, body) = self.motive_parts(&bm.motive)?;
                    core::Term::bln_match(
                        self.term(&bm.head)?,
                        label,
                        body,
                        self.term(&bm.false_case)?,
                        self.term(&bm.true_case)?,
                    )
                }
                Match::Nat(NatMatch::Induction {
                    head,
                    motive,
                    zero_case,
                    pred_label,
                    ih_label,
                    succ_case,
                }) => {
                    let (label, body) = self.motive_parts(motive)?;
                    core::Term::nat_induction(
                        self.term(head)?,
                        label,
                        body,
                        self.term(zero_case)?,
                        pred_label.clone(),
                        ih_label.clone(),
                        self.term(succ_case)?,
                    )
                }
                Match::Nat(NatMatch::Dispatch {
                    head,
                    motive,
                    cases,
                    default,
                }) => {
                    let (label, motive_body) = self.motive_parts(motive)?;
                    core::Term::nat_dispatch(
                        self.term(head)?,
                        label,
                        motive_body,
                        cases
                            .iter()
                            .map(|(&nat, body)| Ok((nat, self.term(body)?)))
                            .collect::<Result<Vec<_>, Error>>()?,
                        self.term(default)?,
                    )
                }
                Match::Union(um) => {
                    // A union match lowers to a core `Match` with
                    // `Cases::Union`, carrying the arm binders as scopes; core
                    // elaboration types the binders from the scrutinee type's
                    // registry telescopes.
                    self.union_match(
                        self.term(&um.head)?,
                        &um.motive,
                        um.cases
                            .iter()
                            .map(|(label, case)| {
                                Ok((
                                    core::Atom::from(label.as_str()),
                                    case.binders.clone(),
                                    self.term(&case.body)?,
                                ))
                            })
                            .collect::<Result<Vec<_>, Error>>()?,
                    )?
                }
            },
            Subterm::Let(let_) => core::Term::let_(
                let_.label.clone(),
                self.term(&let_.signature.type_())?,
                self.term(&let_.signature.body())?,
                self.term(&let_.tail)?,
            ),
            Subterm::Rec(rec) => core::Term::rec(
                rec.items
                    .iter()
                    .map(|it| {
                        Ok((
                            it.label.clone(),
                            self.term(&it.signature.type_())?,
                            self.term(&it.signature.body())?,
                        ))
                    })
                    .collect::<Result<Vec<_>, Error>>()?,
                self.term(&rec.tail)?,
            ),
            // `with <bind> <body>` opens a monadic block. The body is desugared
            // (eliminating every `Bang`) into ordinary core terms by re-elaborating
            // the bind and applying it to `(action, continuation)` per `!` site. See
            // `region`/`instantiate`.
            Subterm::With(with) => {
                let bind = Bind { term: &with.bind };
                self.region(&with.body, &bind)?
            }
            // A bang outside any `with` body has no continuation to hoist to.
            Subterm::Bang(_) => return Err(Error::BangOutsideWith),
        })
    }

    /// Desugars `term` as a single **region** under the active `bind`. A region
    /// is a stretch of a `with` body that shares one continuation; each `!` in it
    /// hoists to the top of the region, never past a boundary (lambda body, match
    /// arm, nested `with`). Boundaries re-root a region. See `WITH.md`.
    fn region(&self, term: &Term, bind: &Bind) -> Result<core::Term, Error> {
        match term.as_subterm() {
            // A `let`'s bound expression evaluates in place (its bangs hoist to
            // this region); the tail continues the same region (a bang there
            // hoists after `x` is bound, not above the `let`).
            Subterm::Let(let_) => {
                let mut binds = Vec::new();
                let let_term = self.build_let(let_, bind, &mut binds)?;
                self.wrap(binds, let_term, bind)
            }
            // The scrutinee evaluates before branching (its bangs hoist here);
            // each arm is its own region (branch-local effects).
            Subterm::Match(match_) => {
                let mut binds = Vec::new();
                let match_term = self.match_region(match_, bind, &mut binds)?;
                self.wrap(binds, match_term, bind)
            }
            // A lambda re-roots the region (same bind, lexically in scope).
            Subterm::Func(func) => {
                let params = func
                    .params
                    .iter()
                    .map(|(name, annotation)| {
                        let domain = match annotation {
                            Some(ty) => self.term(ty)?,
                            None => core::Term::metavar(self.context.fresh_metavar()),
                        };
                        Ok((name.clone(), domain))
                    })
                    .collect::<Result<Vec<_>, Error>>()?;
                Ok(core::Term::func(params, self.region(&func.body, bind)?))
            }
            // A nested `with` switches the bind and desugars independently.
            Subterm::With(with) => {
                let inner = Bind { term: &with.bind };
                self.region(&with.body, &inner)
            }
            // Spine forms (atomic / apply / tuple / proj): collect bangs in
            // left-to-right evaluation order, then wrap.
            _ => {
                let mut binds = Vec::new();
                let body = self.collect(term, bind, &mut binds)?;
                self.wrap(binds, body, bind)
            }
        }
    }

    /// Walks a non-boundary expression, elaborating to core and accumulating each
    /// `Bang` into `binds` (in evaluation order) replaced by a fresh variable.
    /// Boundary/binding forms desugar as their own nested region; `let`/`match`
    /// hoist their bound-expression/scrutinee bangs into the *enclosing* `binds`.
    fn collect(
        &self,
        term: &Term,
        bind: &Bind,
        binds: &mut Vec<(String, core::Term)>,
    ) -> Result<core::Term, Error> {
        Ok(match term.as_subterm() {
            Subterm::Bang(action) => {
                // The action is itself desugared first, so its inner bangs
                // evaluate before this one (left-to-right).
                let action = self.collect(action, bind, binds)?;
                let name = self.context.fresh_binder();
                let var = core::Term::var(core::Var::free(name.clone()));
                binds.push((name, action));
                var
            }
            Subterm::Apply(apply) => core::Term::apply_marked(
                self.collect(&apply.head, bind, binds)?,
                apply
                    .params
                    .iter()
                    .map(|(plicity, p)| Ok((*plicity, self.collect(p, bind, binds)?)))
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            Subterm::Tuple(tuple) => core::Term::tuple(
                tuple
                    .fields
                    .iter()
                    .map(|f| self.collect(f, bind, binds))
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            Subterm::Proj(proj) => {
                core::Term::proj(self.collect(&proj.head, bind, binds)?, proj.index)
            }
            // A `let`/`match` sub-expression hoists its bound-expression /
            // scrutinee bangs into the enclosing region (this `binds`).
            Subterm::Let(let_) => self.build_let(let_, bind, binds)?,
            Subterm::Match(match_) => self.match_region(match_, bind, binds)?,
            // A lambda is a value and a nested `with` is independent: neither
            // hoists anything outward, so desugar each as its own region.
            Subterm::Func(_) | Subterm::With(_) => self.region(term, bind)?,
            // Leaves elaborate normally. A `Bang` reachable here (e.g. nested in a
            // type position) hits `self.term`'s `Bang` arm and is rejected.
            _ => self.term(term)?,
        })
    }

    /// Builds `let x = value; tail` for a `let` inside a `with` region, collecting
    /// the bound expression's bangs into `binds` and desugaring the tail as the
    /// continuation of the same region.
    fn build_let(
        &self,
        let_: &Let,
        bind: &Bind,
        binds: &mut Vec<(String, core::Term)>,
    ) -> Result<core::Term, Error> {
        let value = self.collect(&let_.signature.body(), bind, binds)?;
        let tail = self.region(&let_.tail, bind)?;
        Ok(core::Term::let_(
            let_.label.clone(),
            self.term(&let_.signature.type_())?,
            value,
            tail,
        ))
    }

    /// Desugars a `match` inside a `with` region: the scrutinee's bangs are
    /// collected into `binds` (hoisted out — the scrutinee runs unconditionally),
    /// while each arm is desugared as its own region (branch-local effects). This
    /// mirrors the `Match` arm of `subterm`, swapping `self.term` for `collect`
    /// on heads and `region` on arm bodies.
    fn match_region(
        &self,
        match_: &Match,
        bind: &Bind,
        binds: &mut Vec<(String, core::Term)>,
    ) -> Result<core::Term, Error> {
        Ok(match match_ {
            Match::Bln(bm) => {
                let (label, body) = self.motive_parts(&bm.motive)?;
                core::Term::bln_match(
                    self.collect(&bm.head, bind, binds)?,
                    label,
                    body,
                    self.region(&bm.false_case, bind)?,
                    self.region(&bm.true_case, bind)?,
                )
            }
            Match::Nat(NatMatch::Induction {
                head,
                motive,
                zero_case,
                pred_label,
                ih_label,
                succ_case,
            }) => {
                let (label, body) = self.motive_parts(motive)?;
                core::Term::nat_induction(
                    self.collect(head, bind, binds)?,
                    label,
                    body,
                    self.region(zero_case, bind)?,
                    pred_label.clone(),
                    ih_label.clone(),
                    self.region(succ_case, bind)?,
                )
            }
            Match::Nat(NatMatch::Dispatch {
                head,
                motive,
                cases,
                default,
            }) => {
                let (label, motive_body) = self.motive_parts(motive)?;
                core::Term::nat_dispatch(
                    self.collect(head, bind, binds)?,
                    label,
                    motive_body,
                    cases
                        .iter()
                        .map(|(&nat, body)| Ok((nat, self.region(body, bind)?)))
                        .collect::<Result<Vec<_>, Error>>()?,
                    self.region(default, bind)?,
                )
            }
            Match::Union(um) => {
                // Mirrors the `Match::Union` arm of `subterm` (see there for the
                // type-directed dispatch downstream), swapping `collect` on the
                // head and `region` on the arm bodies.
                let head = self.collect(&um.head, bind, binds)?;

                self.union_match(
                    head,
                    &um.motive,
                    um.cases
                        .iter()
                        .map(|(label, case)| {
                            Ok((
                                core::Atom::from(label.as_str()),
                                case.binders.clone(),
                                self.region(&case.body, bind)?,
                            ))
                        })
                        .collect::<Result<Vec<_>, Error>>()?,
                )?
            }
        })
    }

    /// Wraps `body` in one instantiation of the bind template per collected bang.
    /// The first-collected bang (`binds[0]`) becomes the outermost bind, preserving
    /// left-to-right evaluation order. Continuation lambdas are built with
    /// `core::Term::func` over the gensym'd free name, whose `capture` closes it
    /// robustly under nesting; the domain is a fresh hole, inference-solved.
    fn wrap(
        &self,
        binds: Vec<(String, core::Term)>,
        body: core::Term,
        bind: &Bind,
    ) -> Result<core::Term, Error> {
        binds
            .into_iter()
            .rev()
            .try_fold(body, |acc, (name, action)| {
                let domain = core::Term::metavar(self.context.fresh_metavar());
                let cont = core::Term::func([(name, domain)], acc);
                self.instantiate(bind, action, cont)
            })
    }

    /// Instantiates the bind for one `!` site: re-elaborate the bind term, then apply
    /// it to `(action, cont)`. The term is re-elaborated each call, so its `?` holes
    /// get *fresh* metavariables — a region can therefore sequence actions of
    /// differing result types. Because the result keeps the bind's own head (e.g.
    /// `Parse/bind`) in head position — never a bare lambda — it synthesizes without
    /// annotations.
    fn instantiate(
        &self,
        bind: &Bind,
        action: core::Term,
        cont: core::Term,
    ) -> Result<core::Term, Error> {
        Ok(core::Term::apply(self.term(bind.term)?, [action, cont]))
    }

    /// Splits an optional match motive into its `(label, body)` for the core
    /// match constructors. An omitted motive (`None`) lowers to an unlabelled
    /// fresh metavariable body — the same as writing `: _` — so a non-dependent
    /// match infers its motive by unifying the arms against that metavariable.
    /// The annotated form is union-only and goes through `union_match` instead.
    fn motive_parts<'m>(
        &self,
        motive: &'m Option<Motive>,
    ) -> Result<(Option<&'m str>, core::Term), Error> {
        match motive {
            Some(Motive::Constant(body)) => Ok((None, self.term(body)?)),
            Some(Motive::Scrutinee { label, body }) => Ok((Some(label), self.term(body)?)),
            Some(Motive::Annotated { .. }) => Err(Error::AnnotatedMotiveNotUnion),
            None => Ok((None, core::Term::metavar(self.context.fresh_metavar()))),
        }
    }

    /// Builds the core union match for both lowering paths (`subterm` and
    /// `match_region`). A plain motive goes through `motive_parts`; the
    /// annotated type-pattern form resolves its union name, classifies its
    /// slots — a bare identifier that resolves to no module binding is a
    /// binder candidate (locals are invisible here; core elaboration
    /// validates positionally against the registry), anything else verbatim —
    /// and closes the motive body over the binder labels then the scrutinee.
    fn union_match(
        &self,
        head: core::Term,
        motive: &Option<Motive>,
        cases: Vec<(core::Atom, Vec<String>, core::Term)>,
    ) -> Result<core::Term, Error> {
        let Some(Motive::Annotated {
            label,
            name,
            slots,
            body,
        }) = motive
        else {
            let (label, body) = self.motive_parts(motive)?;
            return Ok(core::Term::union_match(head, label, body, cases));
        };

        // Resolve the annotation's union name exactly like a term reference.
        let resolved = if name.is_abs() || !name.is_single() {
            self.context.resolve_term_name(name)?.join()
        } else {
            match self.context.bindings().get(name.head()) {
                Some(full) => full.join(),
                None => name.head().to_string(),
            }
        };

        let mut binders = Vec::new();
        let mut pattern_slots = Vec::new();
        for slot in slots {
            match slot.as_subterm() {
                // A single unqualified identifier naming no module binding:
                // a binder candidate. (One that *does* name a module binding
                // — `Vec(Nat, k)`'s `Nat` — must stay a reference: binding it
                // would capture the global's occurrences in `P`.)
                Subterm::Name(n)
                    if n.is_single()
                        && !n.is_abs()
                        && self.context.bindings().get(n.head()).is_none() =>
                {
                    binders.push(n.head().to_string());
                    pattern_slots.push(core::MotiveSlot::Binder);
                }
                _ => pattern_slots.push(core::MotiveSlot::Term(self.term(slot)?)),
            }
        }

        Ok(core::Term::union_match_motive(
            head,
            binders,
            label,
            self.term(body)?,
            core::MotivePattern {
                name: resolved,
                slots: pattern_slots,
            },
            cases,
        ))
    }

    pub fn prim(&self, prim: &Prim) -> Result<core::Prim, Error> {
        Ok(match prim {
            Prim::BlnType => core::Prim::BlnType,
            Prim::Bln(b) => core::Prim::Bln(*b),
            Prim::NatType => core::Prim::NatType,
            Prim::Nat(Nat::Zero) => core::Prim::Nat(core::Nat::Zero),
            Prim::Nat(Nat::Succ(NatLiteral::Number(spine), inner)) => {
                core::Prim::Nat(core::Nat::Succ(spine.clone(), self.term(inner)?))
            }
            Prim::Nat(Nat::Succ(NatLiteral::Char(c), inner)) => core::Prim::Nat(core::Nat::Succ(
                BigUint::from(*c as usize),
                self.term(inner)?,
            )),
            Prim::NatEql(left, right) => core::Prim::nat_eql(self.term(left)?, self.term(right)?),
            Prim::NatNeq(left, right) => core::Prim::nat_neq(self.term(left)?, self.term(right)?),
            Prim::NatAdd(left, right) => core::Prim::nat_add(self.term(left)?, self.term(right)?),
            Prim::NatSub(left, right) => core::Prim::nat_sub(self.term(left)?, self.term(right)?),
            Prim::NatMul(left, right) => core::Prim::nat_mul(self.term(left)?, self.term(right)?),
            Prim::NatLt(left, right) => core::Prim::nat_lt(self.term(left)?, self.term(right)?),
            Prim::NatDiv(left, right) => core::Prim::nat_div(self.term(left)?, self.term(right)?),
            Prim::NatRem(left, right) => core::Prim::nat_rem(self.term(left)?, self.term(right)?),
            Prim::NatGt(left, right) => core::Prim::nat_gt(self.term(left)?, self.term(right)?),
            Prim::NatLte(left, right) => core::Prim::nat_lte(self.term(left)?, self.term(right)?),
            Prim::NatGte(left, right) => core::Prim::nat_gte(self.term(left)?, self.term(right)?),
            Prim::IntType => core::Prim::IntType,
            Prim::Int(value) => core::Prim::Int(core::Int::new(*value as i64)),
            Prim::IntEql(left, right) => core::Prim::int_eql(self.term(left)?, self.term(right)?),
            Prim::IntNeq(left, right) => core::Prim::int_neq(self.term(left)?, self.term(right)?),
            Prim::IntAdd(left, right) => core::Prim::int_add(self.term(left)?, self.term(right)?),
            Prim::IntSub(left, right) => core::Prim::int_sub(self.term(left)?, self.term(right)?),
            Prim::IntMul(left, right) => core::Prim::int_mul(self.term(left)?, self.term(right)?),
            Prim::IntDiv(left, right) => core::Prim::int_div(self.term(left)?, self.term(right)?),
            Prim::IntRem(left, right) => core::Prim::int_rem(self.term(left)?, self.term(right)?),
            Prim::IntLt(left, right) => core::Prim::int_lt(self.term(left)?, self.term(right)?),
            Prim::IntGt(left, right) => core::Prim::int_gt(self.term(left)?, self.term(right)?),
            Prim::IntLte(left, right) => core::Prim::int_lte(self.term(left)?, self.term(right)?),
            Prim::IntGte(left, right) => core::Prim::int_gte(self.term(left)?, self.term(right)?),
            Prim::FltType => core::Prim::FltType,
            Prim::Flt(flt) => core::Prim::Flt(core::Flt::from_f32(*flt)),
            Prim::FltAdd(left, right) => core::Prim::flt_add(self.term(left)?, self.term(right)?),
            Prim::FltSub(left, right) => core::Prim::flt_sub(self.term(left)?, self.term(right)?),
            Prim::FltMul(left, right) => core::Prim::flt_mul(self.term(left)?, self.term(right)?),
            Prim::FltDiv(left, right) => core::Prim::flt_div(self.term(left)?, self.term(right)?),
            Prim::FltEql(left, right) => core::Prim::flt_eql(self.term(left)?, self.term(right)?),
            Prim::FltNeq(left, right) => core::Prim::flt_neq(self.term(left)?, self.term(right)?),
            Prim::FltLt(left, right) => core::Prim::flt_lt(self.term(left)?, self.term(right)?),
            Prim::FltGt(left, right) => core::Prim::flt_gt(self.term(left)?, self.term(right)?),
            Prim::FltLte(left, right) => core::Prim::flt_lte(self.term(left)?, self.term(right)?),
            Prim::FltGte(left, right) => core::Prim::flt_gte(self.term(left)?, self.term(right)?),
            Prim::FltMin(left, right) => core::Prim::flt_min(self.term(left)?, self.term(right)?),
            Prim::FltMax(left, right) => core::Prim::flt_max(self.term(left)?, self.term(right)?),
            Prim::FltNeg(inner) => core::Prim::flt_neg(self.term(inner)?),
            Prim::FltAbs(inner) => core::Prim::flt_abs(self.term(inner)?),
            Prim::FltSqrt(inner) => core::Prim::flt_sqrt(self.term(inner)?),
            Prim::FltFloor(inner) => core::Prim::flt_floor(self.term(inner)?),
            Prim::FltCeil(inner) => core::Prim::flt_ceil(self.term(inner)?),
            Prim::FltTrunc(inner) => core::Prim::flt_trunc(self.term(inner)?),
            Prim::FltNearest(inner) => core::Prim::flt_nearest(self.term(inner)?),
            Prim::NatToStr(inner) => core::Prim::nat_to_str(self.term(inner)?),
            Prim::IoPrint(inner) => core::Prim::io_print(self.term(inner)?),
            Prim::IoRead => core::Prim::IoRead,
            Prim::IntToStr(inner) => core::Prim::int_to_str(self.term(inner)?),
            Prim::FltToStr(inner) => core::Prim::flt_to_str(self.term(inner)?),
            Prim::NatToInt(inner) => core::Prim::nat_to_int(self.term(inner)?),
            Prim::NatToFlt(inner) => core::Prim::nat_to_flt(self.term(inner)?),
            Prim::IntToNat(inner) => core::Prim::int_to_nat(self.term(inner)?),
            Prim::IntToFlt(inner) => core::Prim::int_to_flt(self.term(inner)?),
            Prim::FltToNat(inner) => core::Prim::flt_to_nat(self.term(inner)?),
            Prim::FltToInt(inner) => core::Prim::flt_to_int(self.term(inner)?),
            Prim::BinType => core::Prim::BinType,
            Prim::Bin(BinLiteral::Bytes(bytes)) => core::Prim::Bin(bytes.clone()),
            Prim::Bin(BinLiteral::String(string)) => core::Prim::Bin(string.as_bytes().to_vec()),
            Prim::BinLen(inner) => core::Prim::bin_len(self.term(inner)?),
            Prim::BinEql(left, right) => core::Prim::bin_eql(self.term(left)?, self.term(right)?),
            Prim::BinGet(bin, index) => core::Prim::bin_get(self.term(bin)?, self.term(index)?),
            Prim::BinSlice(bin, start, end) => {
                core::Prim::bin_slice(self.term(bin)?, self.term(start)?, self.term(end)?)
            }
            Prim::BinAppend(bin, byte) => core::Prim::bin_append(self.term(bin)?, self.term(byte)?),
            Prim::BinConcat(left, right) => {
                core::Prim::bin_concat([self.term(left)?, self.term(right)?])
            }
            Prim::ArrType(inner) => core::Prim::arr_type(self.term(inner)?),
            Prim::Arr(elems) => core::Prim::Arr(
                elems
                    .iter()
                    .map(|elem| self.term(elem))
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            Prim::ArrLen(ty, inner) => core::Prim::arr_len(self.term(ty)?, self.term(inner)?),
            Prim::ArrGet(ty, list, index) => {
                core::Prim::arr_get(self.term(ty)?, self.term(list)?, self.term(index)?)
            }
            Prim::ArrSlice(ty, list, start, end) => core::Prim::arr_slice(
                self.term(ty)?,
                self.term(list)?,
                self.term(start)?,
                self.term(end)?,
            ),
            Prim::ArrAppend(ty, list, elem) => {
                core::Prim::arr_append(self.term(ty)?, self.term(list)?, self.term(elem)?)
            }
            Prim::ArrConcat(ty, left, right) => {
                core::Prim::arr_concat(self.term(ty)?, [self.term(left)?, self.term(right)?])
            }
        })
    }
}
