use {
    super::Context,
    crate::{
        ArrMatch, BinMatch, Error, Field, Let, Match, Motive, Name, Nat, NatLiteral, NatMatch,
        Prim, Subterm, Syn, Term,
    },
    num_bigint::BigUint,
    std::{cell::RefCell, collections::BTreeSet, sync::Arc},
};

pub struct Lower<'a, 'b> {
    context: &'a Context<'b>,
    /// The user-written names bound by the enclosing local binders (function and
    /// `let`/`rec` binders, match-arm patterns, motive labels). A bare reference
    /// whose name is in this set resolves to the binder rather than a like-named
    /// module binding — see [`Self::resolve_name`]. Internal gensym binders (the
    /// `#`-sigil names from [`Context::fresh_binder`]) can never collide with a
    /// source identifier, so they are deliberately never inserted here.
    scope: RefCell<BTreeSet<String>>,
}

/// One elaborated arm of a single-level core inductive match: the constructor tag,
/// its payload binder names, and the (already-lowered) body.
type InductiveCase = (curios_core::Atom, Vec<String>, curios_core::Term);

/// The active bind of a `let !` region: an atomic term denoting a binary bind
/// `(M A, A -> M B) -> M B`. [`Lower::instantiate`] re-elaborates `term` (so its
/// `?` holes are fresh per `!` site) and applies it to `(action, continuation)`.
struct Bind<'t> {
    term: &'t Term,
}

impl<'a, 'b> Lower<'a, 'b> {
    pub fn new(context: &'a Context<'b>) -> Self {
        Self {
            context,
            scope: RefCell::new(BTreeSet::new()),
        }
    }

    /// Lowers under an extended local scope: each of `names` is treated as an
    /// in-scope binder for the duration of `body`, then the previous scope is
    /// restored. Only names this call *newly* introduces are removed on exit, so
    /// an inner binder shadowing an outer one of the same name leaves the outer
    /// binding intact. Empty (unlabelled) and `_` names bind nothing and are
    /// skipped. The scope borrow is released before `body` runs, so nested
    /// `scoped` calls and [`Self::resolve_name`] reads inside it are free.
    fn scoped<T>(
        &self,
        names: impl IntoIterator<Item = String>,
        body: impl FnOnce() -> Result<T, Error>,
    ) -> Result<T, Error> {
        let mut added = Vec::new();
        {
            let mut scope = self.scope.borrow_mut();
            for name in names {
                if name.is_empty() || name == "_" {
                    continue;
                }
                if scope.insert(name.clone()) {
                    added.push(name);
                }
            }
        }
        let result = body();
        let mut scope = self.scope.borrow_mut();
        for name in &added {
            scope.remove(name);
        }
        result
    }

    /// The binder names a parameter list introduces — each parameter's name, all
    /// in scope across the body. These shadow like-named module bindings; the
    /// wildcard `_` rides along but is ignored by [`Self::scoped`].
    fn param_names(params: &[(String, Option<Term>)]) -> Vec<String> {
        params.iter().map(|(name, _)| name.clone()).collect()
    }

    pub fn term(&self, term: &Term) -> Result<curios_core::Term, Error> {
        let span = term.span().cloned();
        let elaborated = match span.as_ref() {
            Some(s) => self
                .subterm(term.as_subterm())
                .map_err(|error| error.at(s.clone()))?,
            None => self.subterm(term.as_subterm())?,
        };
        Ok(match span {
            Some(s) => curios_core::Term::spanned(s, elaborated),
            None => elaborated,
        })
    }

    /// Resolve a surface name to its qualified (joined) core name — the same
    /// rule the `Subterm::Name` term-reference arm uses.
    fn resolve_name(&self, name: &Name) -> Result<String, Error> {
        Ok(if name.is_abs() || !name.is_single() {
            self.context.resolve_term_name(name)?.join()
        } else if self.scope.borrow().contains(name.head()) {
            // A local binder shadows any like-named module binding: emit the
            // spelled (unqualified) name, which core then resolves to the
            // innermost enclosing binder. Without this an in-scope module binding
            // of the same name would unlawfully capture the reference — and inside
            // a qualified module the module's name (`std/Task/go`) and the local
            // binder (`go`) are *different* strings, so core cannot recover from a
            // wrong choice made here.
            name.head().to_string()
        } else {
            match self.context.bindings().get(name.head()) {
                Some(full) => full.join(),
                None => name.head().to_string(),
            }
        })
    }

    // The meta-emitter: a string literal becomes a proof-carrying `/syn/Str/Str`
    // value `Str { bytes = <Bin>, valid = <Utf8 derivation> }`. The derivation is the
    // canonical `more`-spine (one `more` per byte, ending in `stop`), starting from
    // the `lead` state — `valid`'s type is `Valid(b) = Utf8(lead, b)`. `valid` is
    // erased, so at runtime `Str` collapses to its `Bin` bytes — a literal costs
    // exactly what a `Bin` literal did.
    fn str_literal(&self, bytes: &[u8]) -> curios_core::Term {
        curios_core::Term::struct_named(
            "/syn/Str/Str",
            Vec::<curios_core::Term>::new(),
            [
                (
                    None,
                    curios_core::Term::prim(curios_core::Prim::Bin(bytes.to_vec())),
                ),
                (None, self.utf8_derivation(bytes, Self::scan_lead())),
            ],
        )
    }

    // A constructor/function `Var` applied to `args` — the absolute core name as the
    // parser would resolve it (privacy is a surface-resolution concern; these are
    // already-resolved core `Var`s, so referencing a private `/syn` helper is fine).
    fn syn_call(
        name: &str,
        args: impl IntoIterator<Item = curios_core::Term>,
    ) -> curios_core::Term {
        curios_core::Term::apply(
            curios_core::Term::var(curios_core::Var::free(name)),
            args.into_iter().collect::<Vec<_>>(),
        )
    }

    fn scan_lead() -> curios_core::Term {
        Self::syn_call("/syn/Str/Scan/lead", [])
    }

    // The `Utf8(state, bytes)` derivation. `state` is carried as a *symbolic* term —
    // `lead()` at the top, then `step(c, state)` per byte — so each recursive `rest`'s
    // expected index (`Utf8(step(c, state), tail)`) is definitionally the state we
    // thread in, with no metavar/`step`-inversion. The final `stop : Utf8(lead, \\)`
    // matches because `step` of the last byte reduces back to `lead` for valid UTF-8
    // (a string literal is valid UTF-8 by construction).
    fn utf8_derivation(&self, bytes: &[u8], state: curios_core::Term) -> curios_core::Term {
        match bytes.split_first() {
            None => Self::syn_call("/syn/Str/Utf8/stop", []),
            Some((&head, tail)) => {
                let byte: curios_core::Term = curios_core::Term::prim(curios_core::Prim::Nat(
                    curios_core::Nat::new(head as usize),
                ));
                let next = Self::syn_call("/syn/Str/step", [byte.clone(), state.clone()]);
                Self::syn_call(
                    "/syn/Str/Utf8/more",
                    [
                        byte,
                        state,
                        curios_core::Term::prim(curios_core::Prim::Bin(tail.to_vec())),
                        self.utf8_derivation(tail, next),
                    ],
                )
            }
        }
    }

    // A list literal `[e0, e1, …]` desugars to a `/syn/Lst/Lst` cons-spine
    // `cons(e0, cons(e1, … nil()))`. The element type is an implicit the literal
    // can't name; elaboration inserts it (a metavar) and solves it from the
    // elements or the expected type — exactly as a hand-written `Lst/cons` would.
    fn lst_literal(&self, elems: &[Term]) -> Result<curios_core::Term, Error> {
        let mut spine = Self::syn_call("/syn/Lst/Lst/nil", []);
        for elem in elems.iter().rev() {
            spine = Self::syn_call("/syn/Lst/Lst/cons", [self.term(elem)?, spine]);
        }
        Ok(spine)
    }

    // A `/syn` literal — its value is synthesized from `/syn` by the meta-emitter
    // rather than lowered to a core primitive.
    fn syn_literal(&self, syn: &Syn) -> Result<curios_core::Term, Error> {
        match syn {
            Syn::Str(string) => Ok(self.str_literal(string.as_bytes())),
            Syn::Lst(elems) => self.lst_literal(elems),
        }
    }

    fn subterm(&self, term: &Subterm) -> Result<curios_core::Term, Error> {
        Ok(match term {
            Subterm::Type => curios_core::Term::type_(),
            Subterm::Prop => curios_core::Term::prop(),
            Subterm::Hole => curios_core::Term::metavar(self.context.fresh_metavar()),
            // A `/syn` literal (string or list) desugars via the meta-emitter to a
            // `/syn` construction (see `syn_literal`), never a core primitive.
            Subterm::Syn(syn) => self.syn_literal(syn)?,
            Subterm::Prim(prim) => curios_core::Term::prim(self.prim(prim)?),
            Subterm::NumLit(num_lit) => curios_core::Term::num_lit(
                num_lit.magnitude.clone(),
                num_lit.signed,
                num_lit.negative,
            ),
            Subterm::Infix(infix) => curios_core::Term::infix(
                infix.op,
                self.term(&infix.left)?,
                self.term(&infix.right)?,
            ),
            Subterm::Name(name) => {
                curios_core::Term::var(curios_core::Var::free(self.resolve_name(name)?))
            }
            // Each parameter type sees the *preceding* parameters' binders, and
            // the output sees them all (a dependent Π-type), so they lower under a
            // progressively-extended scope.
            Subterm::FuncType(ft) => {
                let mut seen = Vec::new();
                let mut params = Vec::with_capacity(ft.params.len());
                for param in &ft.params {
                    let domain = self.scoped(seen.clone(), || self.term(&param.type_))?;
                    let name = param.label.clone().unwrap_or_default();
                    seen.push(name.clone());
                    params.push((param.plicity, name, domain));
                }
                let output = self.scoped(seen, || self.term(&ft.output))?;
                curios_core::Term::func_type_marked(params, output)
            }
            Subterm::Func(func) => {
                let body =
                    self.scoped(Self::param_names(&func.params), || self.term(&func.body))?;
                let (params, body) = self.lower_func_params(&func.params, body)?;
                curios_core::Term::func(params, body)
            }
            Subterm::Apply(apply) => curios_core::Term::apply_marked(
                self.term(&apply.head)?,
                apply
                    .params
                    .iter()
                    .map(|(plicity, p)| Ok((*plicity, self.term(p)?)))
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            // A dependent Σ-type: each field type sees the preceding fields'
            // labels, so they lower under a progressively-extended scope. The
            // signature sugar `f(params) -> T` is undone here.
            Subterm::TupleType(tt) => {
                let mut seen = Vec::new();
                let mut fields = Vec::with_capacity(tt.fields.len());
                for param in &tt.fields {
                    let type_ = param.desugared_type();
                    let lowered = self.scoped(seen.clone(), || self.term(&type_))?;
                    let name = param.label.clone().unwrap_or_default();
                    seen.push(name.clone());
                    fields.push((name, lowered));
                }
                curios_core::Term::tuple_type(fields)
            }
            // The definition sugar `f(params) = value` is undone here.
            Subterm::Tuple(tuple) => curios_core::Term::tuple_named(
                tuple
                    .fields
                    .iter()
                    .map(|field| Ok((field.label.clone(), self.term(&field.desugared_value())?)))
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            Subterm::Proj(proj) => {
                let head = self.term(&proj.head)?;
                match &proj.field {
                    Field::Index(index) => curios_core::Term::proj(head, *index),
                    Field::Label(label) => curios_core::Term::proj_label(head, label.clone()),
                }
            }
            // A struct literal lowers to a `curios_core::Struct` carrying the resolved
            // (qualified) struct name, the head parameters (empty → core
            // elaboration mints metavariables), and the field values with their
            // written names (validated positionally and dropped by elaborate).
            // Construction privacy is enforced in core (`elaborate_struct`),
            // alongside projection privacy.
            Subterm::StructLit(lit) => curios_core::Term::struct_named(
                self.resolve_name(&lit.head)?,
                lit.params
                    .iter()
                    .map(|p| self.term(p))
                    .collect::<Result<Vec<_>, Error>>()?,
                lit.fields
                    .iter()
                    .map(|field| Ok((field.label.clone(), self.term(&field.desugared_value())?)))
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            Subterm::Match(match_) => match match_ {
                Match::Bln(bm) => {
                    let (label, body) = self.motive_parts(&bm.motive)?;
                    curios_core::Term::bln_match(
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
                    curios_core::Term::nat_match(
                        self.term(head)?,
                        label,
                        body,
                        self.term(zero_case)?,
                        pred_label.clone(),
                        ih_label.clone(),
                        self.scoped([pred_label.clone(), ih_label.clone()], || {
                            self.term(succ_case)
                        })?,
                    )
                }
                Match::Nat(NatMatch::Dispatch {
                    head,
                    motive,
                    cases,
                    default,
                }) => {
                    let (label, motive_body) = self.motive_parts(motive)?;
                    curios_core::Term::switch(
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
                Match::Inductive(um) => {
                    // A distinct-tag inductive match lowers to a single-level core
                    // `Match`. Bodies lower here (under the arm's payload binders);
                    // `inductive_rows` checks distinctness and assembles the cases.
                    let head = self.term(&um.head)?;
                    let arms = um
                        .arms
                        .iter()
                        .map(|arm| {
                            let body = self.scoped(arm.args.clone(), || self.term(&arm.body))?;
                            Ok((arm.tag.clone(), arm.args.clone(), body))
                        })
                        .collect::<Result<Vec<_>, Error>>()?;
                    self.inductive_rows(head, &um.motive, arms)?
                }
                Match::Arr(ArrMatch {
                    head,
                    motive,
                    empty_case,
                    head_label,
                    tail_label,
                    ih_label,
                    cons_case,
                }) => {
                    let (label, body) = self.motive_parts(motive)?;
                    // The element type is type-directed (read off the scrutinee
                    // during elaboration), so lowering leaves it a hole.
                    curios_core::Term::arr_match(
                        self.term(head)?,
                        curios_core::Term::metavar(self.context.fresh_metavar()),
                        label,
                        body,
                        self.term(empty_case)?,
                        head_label.clone(),
                        tail_label.clone(),
                        ih_label.clone(),
                        self.scoped(
                            [head_label.clone(), tail_label.clone(), ih_label.clone()],
                            || self.term(cons_case),
                        )?,
                    )
                }
                Match::Bin(BinMatch {
                    head,
                    motive,
                    empty_case,
                    head_label,
                    tail_label,
                    ih_label,
                    cons_case,
                }) => {
                    let (label, body) = self.motive_parts(motive)?;
                    curios_core::Term::bin_match(
                        self.term(head)?,
                        label,
                        body,
                        self.term(empty_case)?,
                        head_label.clone(),
                        tail_label.clone(),
                        ih_label.clone(),
                        self.scoped(
                            [head_label.clone(), tail_label.clone(), ih_label.clone()],
                            || self.term(cons_case),
                        )?,
                    )
                }
            },
            // A `let` is non-recursive: its binder is in scope only in the tail,
            // never in its own type or value.
            Subterm::Let(let_) => {
                let type_ = self.term(&let_.signature.type_())?;
                let value = self.term(&let_.signature.body())?;
                let tail = self.scoped([let_.binder.clone()], || self.term(&let_.tail))?;
                curios_core::Term::let_(self.pattern_binder_name(&let_.binder), type_, value, tail)
            }
            // A `rec` is mutually recursive: every item label is in scope across
            // all item types, all item bodies, and the tail.
            Subterm::Rec(rec) => {
                let labels = rec
                    .items
                    .iter()
                    .map(|it| it.label.clone())
                    .collect::<Vec<_>>();
                self.scoped(labels, || {
                    Ok(curios_core::Term::rec(
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
                    ))
                })?
            }
            // `let !` opens a monadic block. The body is desugared (eliminating every
            // `Bang`) into ordinary core terms by re-elaborating the bind and applying
            // it to `(action, continuation)` per `!` site. See `region`/`instantiate`.
            Subterm::LetBang(let_bang) => {
                let bind = Bind {
                    term: &let_bang.bind,
                };
                self.region(&let_bang.body, &bind)?
            }
            // A bang outside any `let !` body has no continuation to hoist to.
            Subterm::Bang(_) => return Err(Error::BangWithoutBind),
        })
    }

    /// Desugars `term` as a single **region** under the active `bind`. A region
    /// is a stretch of a `let !` body that shares one continuation; each `!` in it
    /// hoists to the top of the region, never past a boundary (lambda body, match
    /// arm, nested `let !`). Boundaries re-root a region.
    fn region(&self, term: &Term, bind: &Bind) -> Result<curios_core::Term, Error> {
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
                let body = self.scoped(Self::param_names(&func.params), || {
                    self.region(&func.body, bind)
                })?;
                let (params, body) = self.lower_func_params(&func.params, body)?;
                Ok(curios_core::Term::func(params, body))
            }
            // A nested `let !` switches the bind and desugars independently.
            Subterm::LetBang(let_bang) => {
                let inner = Bind {
                    term: &let_bang.bind,
                };
                self.region(&let_bang.body, &inner)
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
        binds: &mut Vec<(String, curios_core::Term)>,
    ) -> Result<curios_core::Term, Error> {
        Ok(match term.as_subterm() {
            Subterm::Bang(action) => {
                // The action is itself desugared first, so its inner bangs
                // evaluate before this one (left-to-right).
                let action = self.collect(action, bind, binds)?;
                let name = self.context.fresh_binder();
                let var = curios_core::Term::var(curios_core::Var::free(name.clone()));
                binds.push((name, action));
                var
            }
            Subterm::Apply(apply) => curios_core::Term::apply_marked(
                self.collect(&apply.head, bind, binds)?,
                apply
                    .params
                    .iter()
                    .map(|(plicity, p)| Ok((*plicity, self.collect(p, bind, binds)?)))
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            Subterm::Tuple(tuple) => curios_core::Term::tuple_named(
                tuple
                    .fields
                    .iter()
                    .map(|field| {
                        let value = field.desugared_value();
                        Ok((field.label.clone(), self.collect(&value, bind, binds)?))
                    })
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            Subterm::Proj(proj) => {
                let head = self.collect(&proj.head, bind, binds)?;
                match &proj.field {
                    Field::Index(index) => curios_core::Term::proj(head, *index),
                    Field::Label(label) => curios_core::Term::proj_label(head, label.clone()),
                }
            }
            // A struct literal's field values hoist their bangs into this
            // region, exactly like a tuple's.
            Subterm::StructLit(lit) => curios_core::Term::struct_named(
                self.resolve_name(&lit.head)?,
                lit.params
                    .iter()
                    .map(|p| self.collect(p, bind, binds))
                    .collect::<Result<Vec<_>, Error>>()?,
                lit.fields
                    .iter()
                    .map(|field| {
                        let value = field.desugared_value();
                        Ok((field.label.clone(), self.collect(&value, bind, binds)?))
                    })
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            // An infix operator's operands hoist their bangs into this region,
            // exactly like an application's arguments.
            Subterm::Infix(infix) => curios_core::Term::infix(
                infix.op,
                self.collect(&infix.left, bind, binds)?,
                self.collect(&infix.right, bind, binds)?,
            ),
            // A `let`/`match` sub-expression hoists its bound-expression /
            // scrutinee bangs into the enclosing region (this `binds`).
            Subterm::Let(let_) => self.build_let(let_, bind, binds)?,
            Subterm::Match(match_) => self.match_region(match_, bind, binds)?,
            // A lambda is a value and a nested `let !` is independent: neither
            // hoists anything outward, so desugar each as its own region.
            Subterm::Func(_) | Subterm::LetBang(_) => self.region(term, bind)?,
            // Leaves elaborate normally. A `Bang` reachable here (e.g. nested in a
            // type position) hits `self.term`'s `Bang` arm and is rejected.
            _ => self.term(term)?,
        })
    }

    /// Builds `let x = value; tail` for a `let` inside a `let !` region,
    /// collecting the bound expression's bangs into `binds` and desugaring the
    /// tail as the continuation of the same region.
    fn build_let(
        &self,
        let_: &Let,
        bind: &Bind,
        binds: &mut Vec<(String, curios_core::Term)>,
    ) -> Result<curios_core::Term, Error> {
        let value = self.collect(&let_.signature.body(), bind, binds)?;
        let tail = self.scoped([let_.binder.clone()], || self.region(&let_.tail, bind))?;
        let type_ = self.term(&let_.signature.type_())?;
        Ok(curios_core::Term::let_(
            self.pattern_binder_name(&let_.binder),
            type_,
            value,
            tail,
        ))
    }

    /// Lowers a function's parameters into core binder `(name, domain)` pairs.
    /// Each parameter binds a single name; an un-annotated parameter takes a fresh
    /// metavar domain. The body needs no wrapping — there is no destructuring.
    fn lower_func_params(
        &self,
        params: &[(String, Option<Term>)],
        body: curios_core::Term,
    ) -> Result<(Vec<(String, curios_core::Term)>, curios_core::Term), Error> {
        let lowered = params
            .iter()
            .map(|(name, annotation)| {
                let domain = match annotation {
                    Some(annotation) => self.term(annotation)?,
                    None => curios_core::Term::metavar(self.context.fresh_metavar()),
                };
                Ok((self.pattern_binder_name(name), domain))
            })
            .collect::<Result<Vec<_>, Error>>()?;
        Ok((lowered, body))
    }

    /// A pattern binder's core name: `_` mints a fresh internal name (so repeated
    /// wildcards never collide), any other identifier is used verbatim.
    fn pattern_binder_name(&self, name: &str) -> String {
        match name {
            "_" => self.context.fresh_binder(),
            name => name.to_string(),
        }
    }

    /// Desugars a `match` inside a `let !` region: the scrutinee's bangs are
    /// collected into `binds` (hoisted out — the scrutinee runs unconditionally),
    /// while each arm is desugared as its own region (branch-local effects). This
    /// mirrors the `Match` arm of `subterm`, swapping `self.term` for `collect`
    /// on heads and `region` on arm bodies.
    fn match_region(
        &self,
        match_: &Match,
        bind: &Bind,
        binds: &mut Vec<(String, curios_core::Term)>,
    ) -> Result<curios_core::Term, Error> {
        Ok(match match_ {
            Match::Bln(bm) => {
                let (label, body) = self.motive_parts(&bm.motive)?;
                curios_core::Term::bln_match(
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
                curios_core::Term::nat_match(
                    self.collect(head, bind, binds)?,
                    label,
                    body,
                    self.region(zero_case, bind)?,
                    pred_label.clone(),
                    ih_label.clone(),
                    self.scoped([pred_label.clone(), ih_label.clone()], || {
                        self.region(succ_case, bind)
                    })?,
                )
            }
            Match::Nat(NatMatch::Dispatch {
                head,
                motive,
                cases,
                default,
            }) => {
                let (label, motive_body) = self.motive_parts(motive)?;
                curios_core::Term::switch(
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
            Match::Inductive(um) => {
                // Mirrors the `Match::Inductive` arm of `subterm` (see there), swapping
                // `collect` on the head and `region` on the arm bodies (branch-local
                // effects).
                let head = self.collect(&um.head, bind, binds)?;
                let arms = um
                    .arms
                    .iter()
                    .map(|arm| {
                        let names = arm.args.clone();
                        let body = self.scoped(names, || self.region(&arm.body, bind))?;
                        Ok((arm.tag.clone(), arm.args.clone(), body))
                    })
                    .collect::<Result<Vec<_>, Error>>()?;
                self.inductive_rows(head, &um.motive, arms)?
            }
            Match::Arr(ArrMatch {
                head,
                motive,
                empty_case,
                head_label,
                tail_label,
                ih_label,
                cons_case,
            }) => {
                let (label, body) = self.motive_parts(motive)?;
                curios_core::Term::arr_match(
                    self.collect(head, bind, binds)?,
                    curios_core::Term::metavar(self.context.fresh_metavar()),
                    label,
                    body,
                    self.region(empty_case, bind)?,
                    head_label.clone(),
                    tail_label.clone(),
                    ih_label.clone(),
                    self.scoped(
                        [head_label.clone(), tail_label.clone(), ih_label.clone()],
                        || self.region(cons_case, bind),
                    )?,
                )
            }
            Match::Bin(BinMatch {
                head,
                motive,
                empty_case,
                head_label,
                tail_label,
                ih_label,
                cons_case,
            }) => {
                let (label, body) = self.motive_parts(motive)?;
                curios_core::Term::bin_match(
                    self.collect(head, bind, binds)?,
                    label,
                    body,
                    self.region(empty_case, bind)?,
                    head_label.clone(),
                    tail_label.clone(),
                    ih_label.clone(),
                    self.scoped(
                        [head_label.clone(), tail_label.clone(), ih_label.clone()],
                        || self.region(cons_case, bind),
                    )?,
                )
            }
        })
    }

    /// Wraps `body` in one instantiation of the bind template per collected bang.
    /// The first-collected bang (`binds[0]`) becomes the outermost bind, preserving
    /// left-to-right evaluation order. Continuation lambdas are built with
    /// `curios_core::Term::func` over the gensym'd free name, whose `capture` closes it
    /// robustly under nesting; the domain is a fresh hole, inference-solved.
    fn wrap(
        &self,
        binds: Vec<(String, curios_core::Term)>,
        body: curios_core::Term,
        bind: &Bind,
    ) -> Result<curios_core::Term, Error> {
        binds
            .into_iter()
            .rev()
            .try_fold(body, |acc, (name, action)| {
                let domain = curios_core::Term::metavar(self.context.fresh_metavar());
                let cont = curios_core::Term::func([(name, domain)], acc);
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
        action: curios_core::Term,
        cont: curios_core::Term,
    ) -> Result<curios_core::Term, Error> {
        Ok(curios_core::Term::apply(
            self.term(bind.term)?,
            [action, cont],
        ))
    }

    /// Splits an optional match motive into its `(label, body)` for the core
    /// match constructors. An omitted motive (`None`) lowers to an unlabelled
    /// fresh metavariable body — the same as writing `: _` — so a non-dependent
    /// match infers its motive by unifying the arms against that metavariable.
    /// The annotated form is inductive-only and goes through `inductive_match` instead.
    fn motive_parts<'m>(
        &self,
        motive: &'m Option<Motive>,
    ) -> Result<(Option<&'m str>, curios_core::Term), Error> {
        match motive {
            Some(Motive::Constant(body)) => Ok((None, self.term(body)?)),
            // The scrutinee label binds the matched value inside the motive body.
            Some(Motive::Scrutinee { label, body }) => Ok((
                Some(label),
                self.scoped([label.clone()], || self.term(body))?,
            )),
            Some(Motive::Annotated { .. }) => Err(Error::AnnotatedMotiveNotInductive),
            None => Ok((
                None,
                curios_core::Term::metavar(self.context.fresh_metavar()),
            )),
        }
    }

    /// Builds the core inductive match for both lowering paths (`subterm` and
    /// `match_region`). A plain motive goes through `motive_parts`; the
    /// annotated type-pattern form resolves its inductive name, classifies its
    /// slots — a bare identifier that resolves to no module binding is a
    /// binder candidate (locals are invisible here; core elaboration
    /// validates positionally against the registry), anything else verbatim —
    /// and closes the motive body over the binder labels then the scrutinee.
    fn inductive_match(
        &self,
        head: curios_core::Term,
        motive: &Option<Motive>,
        cases: Vec<InductiveCase>,
    ) -> Result<curios_core::Term, Error> {
        let Some(Motive::Annotated {
            label,
            name,
            slots,
            body,
        }) = motive
        else {
            let (label, body) = self.motive_parts(motive)?;
            return Ok(curios_core::Term::inductive_match(head, label, body, cases));
        };

        // Resolve the annotation's inductive name exactly like a term reference.
        let resolved = self.resolve_name(name)?;

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
                    pattern_slots.push(curios_core::MotiveSlot::Binder);
                }
                _ => pattern_slots.push(curios_core::MotiveSlot::Term(self.term(slot)?)),
            }
        }

        // The index binders are in scope inside the motive body.
        let motive_body = self.scoped(binders.clone(), || self.term(body))?;

        Ok(curios_core::Term::inductive_match_motive(
            head,
            binders,
            label,
            motive_body,
            curios_core::MotivePattern {
                name: resolved,
                slots: pattern_slots,
            },
            cases,
        ))
    }

    /// Assembles an inductive match's already-lowered constructor arms into a
    /// single-level core `Match` via [`Self::inductive_match`]. The surface
    /// grammar guarantees each arm is one constructor binding its payload by name;
    /// unknown tags, arity, exhaustiveness, and repeated tags are all verified by
    /// core elaboration against the inductive's registry.
    fn inductive_rows(
        &self,
        head: curios_core::Term,
        motive: &Option<Motive>,
        arms: Vec<(String, Vec<String>, curios_core::Term)>,
    ) -> Result<curios_core::Term, Error> {
        let mut cases = Vec::with_capacity(arms.len());

        for (tag, args, body) in arms {
            let binders = args
                .iter()
                .map(|name| self.pattern_binder_name(name))
                .collect();
            cases.push((curios_core::Atom::from(tag.as_str()), binders, body));
        }

        self.inductive_match(head, motive, cases)
    }

    pub fn prim(&self, prim: &Prim) -> Result<curios_core::Prim, Error> {
        Ok(match prim {
            Prim::BlnType => curios_core::Prim::BlnType,
            Prim::Bln(b) => curios_core::Prim::Bln(*b),
            Prim::BlnAnd(left, right) => {
                curios_core::Prim::BlnAnd(self.term(left)?, self.term(right)?)
            }
            Prim::BlnOr(left, right) => {
                curios_core::Prim::BlnOr(self.term(left)?, self.term(right)?)
            }
            Prim::BlnXor(left, right) => {
                curios_core::Prim::BlnXor(self.term(left)?, self.term(right)?)
            }
            Prim::BlnEql(left, right) => {
                curios_core::Prim::BlnEql(self.term(left)?, self.term(right)?)
            }
            Prim::BlnNeq(left, right) => {
                curios_core::Prim::BlnNeq(self.term(left)?, self.term(right)?)
            }
            Prim::NatType => curios_core::Prim::NatType,
            Prim::Nat(Nat::Zero) => curios_core::Prim::Nat(curios_core::Nat::Zero),
            Prim::Nat(Nat::Succ(NatLiteral::Number(spine, _), inner)) => {
                curios_core::Prim::Nat(curios_core::Nat::Succ(spine.clone(), self.term(inner)?))
            }
            Prim::Nat(Nat::Succ(NatLiteral::Char(c), inner)) => curios_core::Prim::Nat(
                curios_core::Nat::Succ(BigUint::from(*c as usize), self.term(inner)?),
            ),
            Prim::NatEql(left, right) => {
                curios_core::Prim::nat_eql(self.term(left)?, self.term(right)?)
            }
            Prim::NatNeq(left, right) => {
                curios_core::Prim::nat_neq(self.term(left)?, self.term(right)?)
            }
            Prim::NatAdd(left, right) => {
                curios_core::Prim::nat_add(self.term(left)?, self.term(right)?)
            }
            Prim::NatSub(left, right) => {
                curios_core::Prim::nat_sub(self.term(left)?, self.term(right)?)
            }
            Prim::NatMul(left, right) => {
                curios_core::Prim::nat_mul(self.term(left)?, self.term(right)?)
            }
            Prim::NatLt(left, right) => {
                curios_core::Prim::nat_lt(self.term(left)?, self.term(right)?)
            }
            Prim::NatDiv(left, right) => {
                curios_core::Prim::nat_div(self.term(left)?, self.term(right)?)
            }
            Prim::NatRem(left, right) => {
                curios_core::Prim::nat_rem(self.term(left)?, self.term(right)?)
            }
            Prim::NatGt(left, right) => {
                curios_core::Prim::nat_gt(self.term(left)?, self.term(right)?)
            }
            Prim::NatLte(left, right) => {
                curios_core::Prim::nat_lte(self.term(left)?, self.term(right)?)
            }
            Prim::NatGte(left, right) => {
                curios_core::Prim::nat_gte(self.term(left)?, self.term(right)?)
            }
            Prim::NatAnd(left, right) => {
                curios_core::Prim::NatAnd(self.term(left)?, self.term(right)?)
            }
            Prim::NatOr(left, right) => {
                curios_core::Prim::NatOr(self.term(left)?, self.term(right)?)
            }
            Prim::NatXor(left, right) => {
                curios_core::Prim::NatXor(self.term(left)?, self.term(right)?)
            }
            Prim::NatShl(left, right) => {
                curios_core::Prim::NatShl(self.term(left)?, self.term(right)?)
            }
            Prim::NatShr(left, right) => {
                curios_core::Prim::NatShr(self.term(left)?, self.term(right)?)
            }
            Prim::IntType => curios_core::Prim::IntType,
            Prim::Int(value) => curios_core::Prim::Int(curios_core::Int::new(*value as i64)),
            Prim::IntEql(left, right) => {
                curios_core::Prim::int_eql(self.term(left)?, self.term(right)?)
            }
            Prim::IntNeq(left, right) => {
                curios_core::Prim::int_neq(self.term(left)?, self.term(right)?)
            }
            Prim::IntAdd(left, right) => {
                curios_core::Prim::int_add(self.term(left)?, self.term(right)?)
            }
            Prim::IntSub(left, right) => {
                curios_core::Prim::int_sub(self.term(left)?, self.term(right)?)
            }
            Prim::IntMul(left, right) => {
                curios_core::Prim::int_mul(self.term(left)?, self.term(right)?)
            }
            Prim::IntDiv(left, right) => {
                curios_core::Prim::int_div(self.term(left)?, self.term(right)?)
            }
            Prim::IntRem(left, right) => {
                curios_core::Prim::int_rem(self.term(left)?, self.term(right)?)
            }
            Prim::IntLt(left, right) => {
                curios_core::Prim::int_lt(self.term(left)?, self.term(right)?)
            }
            Prim::IntGt(left, right) => {
                curios_core::Prim::int_gt(self.term(left)?, self.term(right)?)
            }
            Prim::IntLte(left, right) => {
                curios_core::Prim::int_lte(self.term(left)?, self.term(right)?)
            }
            Prim::IntGte(left, right) => {
                curios_core::Prim::int_gte(self.term(left)?, self.term(right)?)
            }
            Prim::IntAnd(left, right) => {
                curios_core::Prim::IntAnd(self.term(left)?, self.term(right)?)
            }
            Prim::IntOr(left, right) => {
                curios_core::Prim::IntOr(self.term(left)?, self.term(right)?)
            }
            Prim::IntXor(left, right) => {
                curios_core::Prim::IntXor(self.term(left)?, self.term(right)?)
            }
            Prim::IntShl(left, right) => {
                curios_core::Prim::IntShl(self.term(left)?, self.term(right)?)
            }
            Prim::IntShr(left, right) => {
                curios_core::Prim::IntShr(self.term(left)?, self.term(right)?)
            }
            Prim::FltType => curios_core::Prim::FltType,
            Prim::Flt(flt) => curios_core::Prim::Flt(curios_core::Flt::from_f32(*flt)),
            Prim::FltAdd(left, right) => {
                curios_core::Prim::flt_add(self.term(left)?, self.term(right)?)
            }
            Prim::FltSub(left, right) => {
                curios_core::Prim::flt_sub(self.term(left)?, self.term(right)?)
            }
            Prim::FltMul(left, right) => {
                curios_core::Prim::flt_mul(self.term(left)?, self.term(right)?)
            }
            Prim::FltDiv(left, right) => {
                curios_core::Prim::flt_div(self.term(left)?, self.term(right)?)
            }
            Prim::FltRem(left, right) => {
                curios_core::Prim::FltRem(self.term(left)?, self.term(right)?)
            }
            Prim::FltEql(left, right) => {
                curios_core::Prim::flt_eql(self.term(left)?, self.term(right)?)
            }
            Prim::FltNeq(left, right) => {
                curios_core::Prim::flt_neq(self.term(left)?, self.term(right)?)
            }
            Prim::FltLt(left, right) => {
                curios_core::Prim::flt_lt(self.term(left)?, self.term(right)?)
            }
            Prim::FltGt(left, right) => {
                curios_core::Prim::flt_gt(self.term(left)?, self.term(right)?)
            }
            Prim::FltLte(left, right) => {
                curios_core::Prim::flt_lte(self.term(left)?, self.term(right)?)
            }
            Prim::FltGte(left, right) => {
                curios_core::Prim::flt_gte(self.term(left)?, self.term(right)?)
            }
            Prim::FltMin(left, right) => {
                curios_core::Prim::flt_min(self.term(left)?, self.term(right)?)
            }
            Prim::FltMax(left, right) => {
                curios_core::Prim::flt_max(self.term(left)?, self.term(right)?)
            }
            Prim::FltNeg(inner) => curios_core::Prim::flt_neg(self.term(inner)?),
            Prim::FltAbs(inner) => curios_core::Prim::flt_abs(self.term(inner)?),
            Prim::FltSqrt(inner) => curios_core::Prim::flt_sqrt(self.term(inner)?),
            Prim::FltFloor(inner) => curios_core::Prim::flt_floor(self.term(inner)?),
            Prim::FltCeil(inner) => curios_core::Prim::flt_ceil(self.term(inner)?),
            Prim::FltTrunc(inner) => curios_core::Prim::flt_trunc(self.term(inner)?),
            Prim::FltNearest(inner) => curios_core::Prim::flt_nearest(self.term(inner)?),
            Prim::FltToLeBin(inner) => curios_core::Prim::flt_to_le_bin(self.term(inner)?),
            Prim::NatToInt(inner) => curios_core::Prim::nat_to_int(self.term(inner)?),
            Prim::IoType => curios_core::Prim::IoType,
            Prim::Io(token) => curios_core::Prim::Io(*token),
            Prim::IoEql(left, right) => {
                curios_core::Prim::io_eql(self.term(left)?, self.term(right)?)
            }
            Prim::Foreign(function, args) => curios_core::Prim::Foreign(
                Arc::clone(function),
                args.iter()
                    .map(|arg| self.term(arg))
                    .collect::<Result<_, _>>()?,
            ),
            Prim::IoExit(type_, code) => {
                curios_core::Prim::IoExit(self.term(type_)?, self.term(code)?)
            }
            Prim::NatToFlt(inner) => curios_core::Prim::nat_to_flt(self.term(inner)?),
            Prim::IntToNat(inner) => curios_core::Prim::int_to_nat(self.term(inner)?),
            Prim::IntToFlt(inner) => curios_core::Prim::int_to_flt(self.term(inner)?),
            Prim::FltToNat(inner) => curios_core::Prim::flt_to_nat(self.term(inner)?),
            Prim::FltToInt(inner) => curios_core::Prim::flt_to_int(self.term(inner)?),
            Prim::BinType => curios_core::Prim::BinType,
            // `\hex` is a raw byte sequence.
            Prim::Bin(bytes) => curios_core::Prim::Bin(bytes.clone()),
            Prim::BinLen(inner) => curios_core::Prim::bin_len(self.term(inner)?),
            Prim::BinEql(left, right) => {
                curios_core::Prim::bin_eql(self.term(left)?, self.term(right)?)
            }
            Prim::BinGet(bin, index) => {
                curios_core::Prim::bin_get(self.term(bin)?, self.term(index)?)
            }
            Prim::BinSlice(bin, start, end) => {
                curios_core::Prim::bin_slice(self.term(bin)?, self.term(start)?, self.term(end)?)
            }
            Prim::BinAppend(bin, byte) => {
                curios_core::Prim::bin_append(self.term(bin)?, self.term(byte)?)
            }
            Prim::BinConcat(left, right) => {
                curios_core::Prim::bin_concat([self.term(left)?, self.term(right)?])
            }
            Prim::BinFlatten(operand) => curios_core::Prim::bin_flatten(self.term(operand)?),
            Prim::ArrType(inner) => curios_core::Prim::arr_type(self.term(inner)?),
            Prim::Arr(elems) => {
                curios_core::Prim::Arr(elems.iter().map(|elem| self.term(elem)).collect::<Result<
                    Vec<_>,
                    Error,
                >>(
                )?)
            }
            Prim::ArrLen(ty, inner) => {
                curios_core::Prim::arr_len(self.term(ty)?, self.term(inner)?)
            }
            Prim::ArrGet(ty, list, index) => {
                curios_core::Prim::arr_get(self.term(ty)?, self.term(list)?, self.term(index)?)
            }
            Prim::ArrSlice(ty, list, start, end) => curios_core::Prim::arr_slice(
                self.term(ty)?,
                self.term(list)?,
                self.term(start)?,
                self.term(end)?,
            ),
            Prim::ArrAppend(ty, list, elem) => {
                curios_core::Prim::arr_append(self.term(ty)?, self.term(list)?, self.term(elem)?)
            }
            Prim::ArrConcat(ty, left, right) => {
                curios_core::Prim::arr_concat(self.term(ty)?, [self.term(left)?, self.term(right)?])
            }
            Prim::ArrFlatten(ty, operand) => {
                curios_core::Prim::arr_flatten(self.term(ty)?, self.term(operand)?)
            }
            Prim::ArrMap(a, b, f, arr) => curios_core::Prim::arr_map(
                self.term(a)?,
                self.term(b)?,
                self.term(f)?,
                self.term(arr)?,
            ),
            Prim::CellType(inner) => curios_core::Prim::cell_type(self.term(inner)?),
            Prim::Cell(type_, init) => {
                curios_core::Prim::cell_new(self.term(type_)?, self.term(init)?)
            }
            Prim::CellSet(type_, cell, value) => {
                curios_core::Prim::cell_set(self.term(type_)?, self.term(cell)?, self.term(value)?)
            }
            Prim::CellGet(type_, cell) => {
                curios_core::Prim::cell_get(self.term(type_)?, self.term(cell)?)
            }
        })
    }
}
