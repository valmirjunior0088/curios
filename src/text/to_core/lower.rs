use {
    super::Context,
    crate::{
        core,
        text::{
            ArrMatch, BinMatch, Error, Field, Let, Match, Motive, Name, Nat, NatLiteral, NatMatch,
            Pattern, PatternLit, Prim, Subterm, Syn, Term,
        },
    },
    num_bigint::BigUint,
    std::{cell::RefCell, collections::BTreeSet},
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
type InductiveCase = (core::Atom, Vec<String>, core::Term);

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

    /// Collects the user-written identifiers a binder pattern introduces — the
    /// names that must shadow module bindings inside the binder's scope. Nested
    /// tuple/struct/variant patterns contribute their leaves; the wildcard `_`
    /// rides along but is ignored by [`Self::scoped`].
    fn pattern_names(pattern: &Pattern, out: &mut Vec<String>) {
        match pattern {
            Pattern::Bind(name) => out.push(name.clone()),
            Pattern::Tuple(fields) => fields.iter().for_each(|p| Self::pattern_names(p, out)),
            Pattern::Struct { fields, .. } => {
                fields.iter().for_each(|(_, p)| Self::pattern_names(p, out))
            }
            Pattern::Variant { args, .. } => args.iter().for_each(|p| Self::pattern_names(p, out)),
            Pattern::Lit(_) => {}
        }
    }

    /// The binder names of a single pattern (see [`Self::pattern_names`]).
    fn names_of(pattern: &Pattern) -> Vec<String> {
        let mut out = Vec::new();
        Self::pattern_names(pattern, &mut out);
        out
    }

    /// The binder names of a parameter list — every parameter pattern's leaves,
    /// all in scope across the body (see [`Self::pattern_names`]).
    fn param_names(params: &[(Pattern, Option<Term>)]) -> Vec<String> {
        let mut out = Vec::new();
        for (pattern, _) in params {
            Self::pattern_names(pattern, &mut out);
        }
        out
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
    fn str_literal(&self, bytes: &[u8]) -> core::Term {
        core::Term::struct_named(
            "/syn/Str/Str",
            Vec::<core::Term>::new(),
            [
                (None, core::Term::prim(core::Prim::Bin(bytes.to_vec()))),
                (None, self.utf8_derivation(bytes, Self::scan_lead())),
            ],
        )
    }

    // A constructor/function `Var` applied to `args` — the absolute core name as the
    // parser would resolve it (privacy is a surface-resolution concern; these are
    // already-resolved core `Var`s, so referencing a private `/syn` helper is fine).
    fn syn_call(name: &str, args: impl IntoIterator<Item = core::Term>) -> core::Term {
        core::Term::apply(
            core::Term::var(core::Var::free(name)),
            args.into_iter().collect::<Vec<_>>(),
        )
    }

    fn scan_lead() -> core::Term {
        Self::syn_call("/syn/Str/Scan/lead", [])
    }

    // The `Utf8(state, bytes)` derivation. `state` is carried as a *symbolic* term —
    // `lead()` at the top, then `step(c, state)` per byte — so each recursive `rest`'s
    // expected index (`Utf8(step(c, state), tail)`) is definitionally the state we
    // thread in, with no metavar/`step`-inversion. The final `stop : Utf8(lead, \\)`
    // matches because `step` of the last byte reduces back to `lead` for valid UTF-8
    // (a string literal is valid UTF-8 by construction).
    fn utf8_derivation(&self, bytes: &[u8], state: core::Term) -> core::Term {
        match bytes.split_first() {
            None => Self::syn_call("/syn/Str/Utf8/stop", []),
            Some((&head, tail)) => {
                let byte: core::Term = core::Term::prim(core::Prim::Nat(core::Nat::new(head as usize)));
                let next = Self::syn_call("/syn/Str/step", [byte.clone(), state.clone()]);
                Self::syn_call(
                    "/syn/Str/Utf8/more",
                    [
                        byte,
                        state,
                        core::Term::prim(core::Prim::Bin(tail.to_vec())),
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
    fn lst_literal(&self, elems: &[Term]) -> Result<core::Term, Error> {
        let mut spine = Self::syn_call("/syn/Lst/Lst/nil", []);
        for elem in elems.iter().rev() {
            spine = Self::syn_call("/syn/Lst/Lst/cons", [self.term(elem)?, spine]);
        }
        Ok(spine)
    }

    // A `/syn` literal — its value is synthesized from `/syn` by the meta-emitter
    // rather than lowered to a core primitive.
    fn syn_literal(&self, syn: &Syn) -> Result<core::Term, Error> {
        match syn {
            Syn::Str(string) => Ok(self.str_literal(string.as_bytes())),
            Syn::Lst(elems) => self.lst_literal(elems),
        }
    }

    fn subterm(&self, term: &Subterm) -> Result<core::Term, Error> {
        Ok(match term {
            Subterm::Type => core::Term::type_(),
            Subterm::Prop => core::Term::prop(),
            Subterm::Hole => core::Term::metavar(self.context.fresh_metavar()),
            // A `/syn` literal (string or list) desugars via the meta-emitter to a
            // `/syn` construction (see `syn_literal`), never a core primitive.
            Subterm::Syn(syn) => self.syn_literal(syn)?,
            Subterm::Prim(prim) => core::Term::prim(self.prim(prim)?),
            Subterm::NumLit(num_lit) => {
                core::Term::num_lit(num_lit.magnitude.clone(), num_lit.signed, num_lit.negative)
            }
            Subterm::Infix(infix) => {
                core::Term::infix(infix.op, self.term(&infix.left)?, self.term(&infix.right)?)
            }
            Subterm::Name(name) => core::Term::var(core::Var::free(self.resolve_name(name)?)),
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
                core::Term::func_type_marked(params, output)
            }
            Subterm::Func(func) => {
                let body =
                    self.scoped(Self::param_names(&func.params), || self.term(&func.body))?;
                let (params, body) = self.lower_func_params(&func.params, body)?;
                core::Term::func(params, body)
            }
            Subterm::Apply(apply) => core::Term::apply_marked(
                self.term(&apply.head)?,
                apply
                    .params
                    .iter()
                    .map(|(plicity, p)| Ok((*plicity, self.term(p)?)))
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            // A dependent Σ-type: each field type sees the preceding fields'
            // labels, so they lower under a progressively-extended scope.
            Subterm::TupleType(tt) => {
                let mut seen = Vec::new();
                let mut fields = Vec::with_capacity(tt.fields.len());
                for param in &tt.fields {
                    let lowered = self.scoped(seen.clone(), || self.term(&param.type_))?;
                    let name = param.label.clone().unwrap_or_default();
                    seen.push(name.clone());
                    fields.push((name, lowered));
                }
                core::Term::tuple_type(fields)
            }
            Subterm::Tuple(tuple) => core::Term::tuple_named(
                tuple
                    .fields
                    .iter()
                    .map(|(name, field)| Ok((name.clone(), self.term(field)?)))
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            Subterm::Proj(proj) => {
                let head = self.term(&proj.head)?;
                match &proj.field {
                    Field::Index(index) => core::Term::proj(head, *index),
                    Field::Label(label) => core::Term::proj_label(head, label.clone()),
                }
            }
            // A struct literal lowers to a `core::Struct` carrying the resolved
            // (qualified) struct name, the head parameters (empty → core
            // elaboration mints metavariables), and the field values with their
            // written names (validated positionally and dropped by elaborate).
            // Construction privacy is enforced in core (`elaborate_struct`),
            // alongside projection privacy.
            Subterm::StructLit(lit) => core::Term::struct_named(
                self.resolve_name(&lit.head)?,
                lit.params
                    .iter()
                    .map(|p| self.term(p))
                    .collect::<Result<Vec<_>, Error>>()?,
                lit.fields
                    .iter()
                    .map(|(name, field)| Ok((name.clone(), self.term(field)?)))
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
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
                    core::Term::nat_match(
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
                    core::Term::switch(
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
                    // An inductive match lowers either to a single-level core `Match`
                    // (the legacy distinct-tag shape) or, when its rows nest
                    // refutable patterns, through the pattern-matrix compiler to a
                    // tree of them. Bodies lower here; `inductive_rows` decides.
                    let head = self.term(&um.head)?;
                    let rows = um
                        .rows
                        .iter()
                        .map(|(pattern, body)| {
                            let body =
                                self.scoped(Self::names_of(pattern), || self.term(body))?;
                            Ok((pattern.clone(), body))
                        })
                        .collect::<Result<Vec<_>, Error>>()?;
                    self.inductive_rows(head, &um.motive, rows)?
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
                    core::Term::arr_match(
                        self.term(head)?,
                        core::Term::metavar(self.context.fresh_metavar()),
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
                    core::Term::bin_match(
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
                let tail = self.scoped(Self::names_of(&let_.binder), || self.term(&let_.tail))?;
                self.lower_let(&let_.binder, type_, value, tail)?
            }
            // A `rec` is mutually recursive: every item label is in scope across
            // all item types, all item bodies, and the tail.
            Subterm::Rec(rec) => {
                let labels = rec.items.iter().map(|it| it.label.clone()).collect::<Vec<_>>();
                self.scoped(labels, || {
                    Ok(core::Term::rec(
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
                let body = self
                    .scoped(Self::param_names(&func.params), || self.region(&func.body, bind))?;
                let (params, body) = self.lower_func_params(&func.params, body)?;
                Ok(core::Term::func(params, body))
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
            Subterm::Tuple(tuple) => core::Term::tuple_named(
                tuple
                    .fields
                    .iter()
                    .map(|(name, f)| Ok((name.clone(), self.collect(f, bind, binds)?)))
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            Subterm::Proj(proj) => {
                let head = self.collect(&proj.head, bind, binds)?;
                match &proj.field {
                    Field::Index(index) => core::Term::proj(head, *index),
                    Field::Label(label) => core::Term::proj_label(head, label.clone()),
                }
            }
            // A struct literal's field values hoist their bangs into this
            // region, exactly like a tuple's.
            Subterm::StructLit(lit) => core::Term::struct_named(
                self.resolve_name(&lit.head)?,
                lit.params
                    .iter()
                    .map(|p| self.collect(p, bind, binds))
                    .collect::<Result<Vec<_>, Error>>()?,
                lit.fields
                    .iter()
                    .map(|(name, field)| Ok((name.clone(), self.collect(field, bind, binds)?)))
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            // An infix operator's operands hoist their bangs into this region,
            // exactly like an application's arguments.
            Subterm::Infix(infix) => core::Term::infix(
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

    /// Builds `let <pattern> = value; tail` for a `let` inside a `let !` region,
    /// collecting the bound expression's bangs into `binds` and desugaring the
    /// tail as the continuation of the same region. A tuple binder's projections
    /// are pure, so they need no region treatment.
    fn build_let(
        &self,
        let_: &Let,
        bind: &Bind,
        binds: &mut Vec<(String, core::Term)>,
    ) -> Result<core::Term, Error> {
        let value = self.collect(&let_.signature.body(), bind, binds)?;
        let tail = self.scoped(Self::names_of(&let_.binder), || self.region(&let_.tail, bind))?;
        let type_ = self.term(&let_.signature.type_())?;
        self.lower_let(&let_.binder, type_, value, tail)
    }

    /// Binds a `let`'s pattern against an already-lowered `value` (typed by
    /// `type_`) with `tail` as its continuation. A plain `Bind` is a single core
    /// `let` (the common case, and the function-definition sugar); a `Tuple`
    /// binds the value to a fresh temp typed by `type_`, then projects each leaf.
    fn lower_let(
        &self,
        binder: &Pattern,
        type_: core::Term,
        value: core::Term,
        tail: core::Term,
    ) -> Result<core::Term, Error> {
        Ok(match binder {
            Pattern::Bind(name) => {
                core::Term::let_(self.pattern_binder_name(name), type_, value, tail)
            }
            Pattern::Tuple(fields) => {
                let temp = self.context.fresh_binder();
                let scrutinee = core::Term::var(core::Var::free(temp.clone()));
                let body = self.project_fields(&scrutinee, fields, tail)?;
                core::Term::let_(temp, type_, value, body)
            }
            // A struct pattern types its temp by the *named* struct (parameters
            // inferred) rather than the written `type_`, so the head forces a
            // nominal check that `value` really is a `head`. The written
            // annotation, if any, is redundant against that.
            Pattern::Struct { head, fields } => {
                let temp = self.context.fresh_binder();
                let scrutinee = core::Term::var(core::Var::free(temp.clone()));
                let body = self.project_struct_fields(&scrutinee, fields, tail)?;
                core::Term::let_(temp, self.struct_head_type(head)?, value, body)
            }
            // The irrefutable `parse_pattern` grammar never yields a refutable
            // pattern in a binder position; match arms route through `inductive_rows`.
            Pattern::Variant { .. } | Pattern::Lit(_) => {
                unreachable!("refutable pattern in a let binder position")
            }
        })
    }

    /// Lowers a function's parameter patterns into core binder `(name, domain)`
    /// pairs plus a body wrapped with the projections each tuple pattern needs.
    /// A plain `Bind` parameter is named directly and adds no wrapping — the
    /// common `(x) => …` case is unchanged. Folds right-to-left so each wrap
    /// nests inside the previous parameter's scope.
    fn lower_func_params(
        &self,
        params: &[(Pattern, Option<Term>)],
        body: core::Term,
    ) -> Result<(Vec<(String, core::Term)>, core::Term), Error> {
        let mut lowered = Vec::with_capacity(params.len());
        let mut body = body;
        for (pattern, annotation) in params.iter().rev() {
            // An un-annotated struct parameter takes its domain from the head
            // (parameters inferred), so the param is nominally a `head` — the
            // analogue of the `let`-temp typing in `lower_let`.
            let domain = match (annotation, pattern) {
                (Some(annotation), _) => self.term(annotation)?,
                (None, Pattern::Struct { head, .. }) => self.struct_head_type(head)?,
                (None, _) => core::Term::metavar(self.context.fresh_metavar()),
            };
            let (name, wrapped) = self.bind_top_pattern(pattern, body)?;
            body = wrapped;
            lowered.push((name, domain));
        }
        lowered.reverse();
        Ok((lowered, body))
    }

    /// Lowers an inductive arm's payload patterns into the core arm's binder names
    /// plus a body wrapped with the projections each tuple pattern needs. Like
    /// [`Self::lower_func_params`] without domains.
    fn bind_arm_patterns(
        &self,
        patterns: &[Pattern],
        body: core::Term,
    ) -> Result<(Vec<String>, core::Term), Error> {
        let mut names = vec![String::new(); patterns.len()];
        let body = patterns
            .iter()
            .enumerate()
            .rev()
            .try_fold(body, |acc, (index, pattern)| {
                let (name, wrapped) = self.bind_top_pattern(pattern, acc)?;
                names[index] = name;
                Ok(wrapped)
            })?;
        Ok((names, body))
    }

    /// Resolves a single binder pattern to the core binder name that holds the
    /// whole value, wrapping `body` with projections when the pattern is a tuple.
    /// Projecting directly off this name avoids the redundant `let` a fresh
    /// [`Self::bind_pattern`] temp would introduce.
    fn bind_top_pattern(
        &self,
        pattern: &Pattern,
        body: core::Term,
    ) -> Result<(String, core::Term), Error> {
        Ok(match pattern {
            Pattern::Bind(name) => (self.pattern_binder_name(name), body),
            Pattern::Tuple(fields) => {
                let name = self.context.fresh_binder();
                let scrutinee = core::Term::var(core::Var::free(name.clone()));
                (name, self.project_fields(&scrutinee, fields, body)?)
            }
            // The whole-value binder is typed by its binding site (a Π-domain or
            // a constructor's payload telescope), so the head needs no separate
            // nominal check here — just project the named fields by label.
            Pattern::Struct { head: _, fields } => {
                let name = self.context.fresh_binder();
                let scrutinee = core::Term::var(core::Var::free(name.clone()));
                (name, self.project_struct_fields(&scrutinee, fields, body)?)
            }
            Pattern::Variant { .. } | Pattern::Lit(_) => {
                unreachable!("refutable pattern in a top binder position")
            }
        })
    }

    /// Binds each field of a tuple pattern by projecting off `scrutinee` (already
    /// a variable). Folds right-to-left so field 0's bindings end up outermost.
    fn project_fields(
        &self,
        scrutinee: &core::Term,
        fields: &[Pattern],
        body: core::Term,
    ) -> Result<core::Term, Error> {
        fields
            .iter()
            .enumerate()
            .rev()
            .try_fold(body, |acc, (index, pattern)| {
                self.bind_pattern(pattern, core::Term::proj(scrutinee.clone(), index), acc)
            })
    }

    /// Binds each named field of a struct pattern by *label*-projecting off
    /// `scrutinee` (already a variable). Folds right-to-left so the first field's
    /// bindings end up outermost. Partial: only the listed labels are projected,
    /// so the rest of the struct's fields are simply ignored.
    fn project_struct_fields(
        &self,
        scrutinee: &core::Term,
        fields: &[(String, Pattern)],
        body: core::Term,
    ) -> Result<core::Term, Error> {
        fields.iter().rev().try_fold(body, |acc, (label, pattern)| {
            let field = core::Term::proj_label(scrutinee.clone(), label.clone());
            self.bind_pattern(pattern, field, acc)
        })
    }

    /// The struct type a `head` denotes with its parameters inferred:
    /// `StructType { head, [] }`. Core elaboration mints one fresh metavariable
    /// per declared parameter for the empty list (`elaborate_struct_type`),
    /// exactly as the bare-name struct literal does.
    fn struct_head_type(&self, head: &Name) -> Result<core::Term, Error> {
        Ok(core::Term::struct_type(
            self.resolve_name(head)?,
            std::iter::empty::<core::Term>(),
        ))
    }

    /// Binds `pattern` against an arbitrary `scrutinee` term. A `Bind` is a plain
    /// `let`; a `Tuple`/`Struct` binds the scrutinee to a fresh temp and projects.
    /// Used for nested patterns, where the scrutinee is itself a projection.
    fn bind_pattern(
        &self,
        pattern: &Pattern,
        scrutinee: core::Term,
        body: core::Term,
    ) -> Result<core::Term, Error> {
        Ok(match pattern {
            Pattern::Bind(name) => {
                let hole = core::Term::metavar(self.context.fresh_metavar());
                core::Term::let_(self.pattern_binder_name(name), hole, scrutinee, body)
            }
            Pattern::Tuple(fields) => {
                let hole = core::Term::metavar(self.context.fresh_metavar());
                let temp = self.context.fresh_binder();
                let inner = core::Term::var(core::Var::free(temp.clone()));
                let body = self.project_fields(&inner, fields, body)?;
                core::Term::let_(temp, hole, scrutinee, body)
            }
            // A nested struct pattern types its temp by the named head (like the
            // top-level `let`), so the nominal check holds at every depth.
            Pattern::Struct { head, fields } => {
                let temp = self.context.fresh_binder();
                let inner = core::Term::var(core::Var::free(temp.clone()));
                let body = self.project_struct_fields(&inner, fields, body)?;
                core::Term::let_(temp, self.struct_head_type(head)?, scrutinee, body)
            }
            // Nested refutable patterns are bound by the matrix compiler, which
            // dispatches on them rather than binding them irrefutably here.
            Pattern::Variant { .. } | Pattern::Lit(_) => {
                unreachable!("refutable pattern reached irrefutable nested binding")
            }
        })
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
                core::Term::nat_match(
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
                core::Term::switch(
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
                // effects); `inductive_rows` then dispatches simple-vs-matrix.
                let head = self.collect(&um.head, bind, binds)?;
                let rows = um
                    .rows
                    .iter()
                    .map(|(pattern, body)| {
                        let body =
                            self.scoped(Self::names_of(pattern), || self.region(body, bind))?;
                        Ok((pattern.clone(), body))
                    })
                    .collect::<Result<Vec<_>, Error>>()?;
                self.inductive_rows(head, &um.motive, rows)?
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
                core::Term::arr_match(
                    self.collect(head, bind, binds)?,
                    core::Term::metavar(self.context.fresh_metavar()),
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
                core::Term::bin_match(
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
    /// The annotated form is inductive-only and goes through `inductive_match` instead.
    fn motive_parts<'m>(
        &self,
        motive: &'m Option<Motive>,
    ) -> Result<(Option<&'m str>, core::Term), Error> {
        match motive {
            Some(Motive::Constant(body)) => Ok((None, self.term(body)?)),
            // The scrutinee label binds the matched value inside the motive body.
            Some(Motive::Scrutinee { label, body }) => {
                Ok((Some(label), self.scoped([label.clone()], || self.term(body))?))
            }
            Some(Motive::Annotated { .. }) => Err(Error::AnnotatedMotiveNotInductive),
            None => Ok((None, core::Term::metavar(self.context.fresh_metavar()))),
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
        head: core::Term,
        motive: &Option<Motive>,
        cases: Vec<InductiveCase>,
    ) -> Result<core::Term, Error> {
        let Some(Motive::Annotated {
            label,
            name,
            slots,
            body,
        }) = motive
        else {
            let (label, body) = self.motive_parts(motive)?;
            return Ok(core::Term::inductive_match(head, label, body, cases));
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
                    pattern_slots.push(core::MotiveSlot::Binder);
                }
                _ => pattern_slots.push(core::MotiveSlot::Term(self.term(slot)?)),
            }
        }

        // The index binders are in scope inside the motive body.
        let motive_body = self.scoped(binders.clone(), || self.term(body))?;

        Ok(core::Term::inductive_match_motive(
            head,
            binders,
            label,
            motive_body,
            core::MotivePattern {
                name: resolved,
                slots: pattern_slots,
            },
            cases,
        ))
    }

    /// Lowers an inductive match's arm rows (bodies already lowered). The legacy
    /// single-level shape — distinct-tag [`Pattern::Variant`] rows whose args are
    /// all irrefutable — goes through [`Self::inductive_match`] unchanged (identical
    /// core, including the dependent/annotated motive forms). Anything richer —
    /// nested refutable patterns, a repeated head, a literal, or a `_`/binder
    /// catch-all — is compiled by the pattern matrix into a tree of the same
    /// single-level nodes.
    fn inductive_rows(
        &self,
        head: core::Term,
        motive: &Option<Motive>,
        rows: Vec<(Pattern, core::Term)>,
    ) -> Result<core::Term, Error> {
        if let Some(cases) = self.simple_inductive_cases(&rows)? {
            return self.inductive_match(head, motive, cases);
        }

        // Bind the scrutinee to a temp so every dispatch (and every catch-all
        // binder) reads a plain variable, then compile the one-column matrix.
        let temp = self.context.fresh_binder();
        let scrutinee = core::Term::var(core::Var::free(temp.clone()));
        let matrix = rows
            .into_iter()
            .map(|(pattern, body)| MatrixRow {
                columns: vec![pattern],
                bindings: vec![],
                body,
            })
            .collect();
        let motive = self.matrix_motive(motive)?;
        let compiled = self.compile_matrix(&[scrutinee], matrix, motive)?;
        let temp_type = core::Term::metavar(self.context.fresh_metavar());
        Ok(core::Term::let_(temp, temp_type, head, compiled))
    }

    /// The legacy single-level cases, or `None` if the rows need the matrix. Rows
    /// qualify only when each is a distinct-tag constructor pattern with
    /// irrefutable arguments — exactly what the pre-matrix lowering accepted.
    fn simple_inductive_cases(
        &self,
        rows: &[(Pattern, core::Term)],
    ) -> Result<Option<Vec<InductiveCase>>, Error> {
        let mut seen = std::collections::HashSet::new();
        let mut cases = Vec::with_capacity(rows.len());

        for (pattern, body) in rows {
            let Pattern::Variant { tag, args } = pattern else {
                return Ok(None);
            };

            if !seen.insert(tag.clone()) || args.iter().any(pattern_is_refutable) {
                return Ok(None);
            }

            let (binders, body) = self.bind_arm_patterns(args, body.clone())?;
            cases.push((core::Atom::from(tag.as_str()), binders, body));
        }

        Ok(Some(cases))
    }

    /// Splits the user motive into the `(label, body)` the outermost synthesized
    /// dispatch node carries. An omitted motive yields `None` (the node then
    /// elides to a fresh metavariable, like every inner node). The annotated
    /// index-pattern form has no place in a nested match — its refinement cannot
    /// thread through the inferred inner motives.
    fn matrix_motive(
        &self,
        motive: &Option<Motive>,
    ) -> Result<Option<(Option<String>, core::Term)>, Error> {
        Ok(match motive {
            None => None,
            Some(Motive::Constant(body)) => Some((None, self.term(body)?)),
            Some(Motive::Scrutinee { label, body }) => {
                let body = self.scoped([label.clone()], || self.term(body))?;
                Some((Some(label.clone()), body))
            }
            Some(Motive::Annotated { .. }) => return Err(Error::DependentMatrixMotive),
        })
    }

    /// The pattern-matrix compiler (Maranget-style). `occurrences` are the core
    /// terms (variables/projections) being scrutinized, one per matrix column;
    /// each row carries one pattern per column, the bindings accumulated as
    /// irrefutable/catch-all columns were peeled, and its already-lowered body.
    /// `motive` rides on the *first* dispatch node emitted (the user's, for the
    /// outermost match); every nested node infers its motive.
    fn compile_matrix(
        &self,
        occurrences: &[core::Term],
        rows: Vec<MatrixRow>,
        motive: Option<(Option<String>, core::Term)>,
    ) -> Result<core::Term, Error> {
        // The leftmost column that any row refutes; if none, every row is a pure
        // binding and the first one wins (first-match semantics).
        let column = (0..occurrences.len())
            .find(|&i| rows.iter().any(|row| pattern_is_refutable(&row.columns[i])));

        let Some(column) = column else {
            let row = rows
                .into_iter()
                .next()
                .expect("a matrix never has zero rows");
            return self.bind_matrix_leaf(occurrences, row);
        };

        // A tuple/struct column has a single shape, so it never dispatches — it
        // expands into its fields as sub-columns (the motive passes through to the
        // first real dispatch beneath it). Only constructors/literals branch.
        let refutable = rows
            .iter()
            .map(|row| &row.columns[column])
            .find(|pattern| pattern_is_refutable(pattern))
            .expect("the chosen column is refutable in some row");

        match refutable {
            Pattern::Tuple(_) => self.expand_tuple_column(occurrences, rows, column, motive),
            Pattern::Struct { .. } => self.expand_struct_column(occurrences, rows, column, motive),
            _ => match self.column_kind(&rows, column)? {
                ColumnKind::Inductive => self.compile_inductive_column(occurrences, rows, column, motive),
                ColumnKind::Nat => self.compile_nat_column(occurrences, rows, column, motive),
                ColumnKind::Bln => self.compile_bln_column(occurrences, rows, column, motive),
            },
        }
    }

    /// A leaf: bind the (now all-irrefutable) columns against their occurrences
    /// and the accumulated catch-all bindings around the row's body.
    fn bind_matrix_leaf(
        &self,
        occurrences: &[core::Term],
        row: MatrixRow,
    ) -> Result<core::Term, Error> {
        let MatrixRow {
            columns,
            bindings,
            mut body,
        } = row;

        for (pattern, scrutinee) in bindings.into_iter().rev() {
            body = self.bind_pattern(&pattern, scrutinee, body)?;
        }

        for (pattern, scrutinee) in columns.into_iter().zip(occurrences).rev() {
            body = self.bind_pattern(&pattern, scrutinee.clone(), body)?;
        }

        Ok(body)
    }

    /// Expand a tuple column into its fields as sub-columns (positional
    /// projections). A tuple always matches, so no node is emitted and the motive
    /// flows through; tuple rows splice their fields, a catch-all wildcards them
    /// and binds the whole tuple.
    fn expand_tuple_column(
        &self,
        occurrences: &[core::Term],
        rows: Vec<MatrixRow>,
        column: usize,
        motive: Option<(Option<String>, core::Term)>,
    ) -> Result<core::Term, Error> {
        let arity = rows
            .iter()
            .find_map(|row| match &row.columns[column] {
                Pattern::Tuple(fields) => Some(fields.len()),
                _ => None,
            })
            .expect("a tuple column has a tuple pattern");

        let scrutinee = occurrences[column].clone();
        let fields = (0..arity)
            .map(|index| core::Term::proj(scrutinee.clone(), index))
            .collect::<Vec<_>>();
        let sub_occ = splice(occurrences, column, &fields);

        let mut sub_rows = Vec::new();
        for row in &rows {
            match &row.columns[column] {
                Pattern::Tuple(fields) => {
                    if fields.len() != arity {
                        return Err(Error::PatternArityMismatch {
                            tag: "(tuple)".to_string(),
                            expected: arity,
                            found: fields.len(),
                        });
                    }
                    sub_rows.push(row.with_column_replaced(column, fields.clone(), None));
                }
                other if !pattern_is_refutable(other) => {
                    let wildcards = vec![Pattern::Bind("_".to_string()); arity];
                    let binding = (other.clone(), scrutinee.clone());
                    sub_rows.push(row.with_column_replaced(column, wildcards, Some(binding)));
                }
                _ => return Err(Error::PatternColumnConflict),
            }
        }

        self.compile_matrix(&sub_occ, sub_rows, motive)
    }

    /// Expand a struct column into the inductive (in first-seen order) of the labels
    /// its patterns mention, projected by label. Structs are partial and always
    /// match, so — like tuples — no node is emitted and the motive flows through;
    /// a struct row supplies its named sub-patterns (wildcards for the rest), a
    /// catch-all wildcards all and binds the whole value.
    fn expand_struct_column(
        &self,
        occurrences: &[core::Term],
        rows: Vec<MatrixRow>,
        column: usize,
        motive: Option<(Option<String>, core::Term)>,
    ) -> Result<core::Term, Error> {
        let mut labels: Vec<String> = Vec::new();
        for row in &rows {
            if let Pattern::Struct { fields, .. } = &row.columns[column] {
                for (label, _) in fields {
                    if !labels.contains(label) {
                        labels.push(label.clone());
                    }
                }
            }
        }

        let scrutinee = occurrences[column].clone();
        let fields = labels
            .iter()
            .map(|label| core::Term::proj_label(scrutinee.clone(), label.clone()))
            .collect::<Vec<_>>();
        let sub_occ = splice(occurrences, column, &fields);

        let mut sub_rows = Vec::new();
        for row in &rows {
            match &row.columns[column] {
                Pattern::Struct { fields, .. } => {
                    let sub = labels
                        .iter()
                        .map(|label| {
                            fields
                                .iter()
                                .find(|(field, _)| field == label)
                                .map(|(_, pattern)| pattern.clone())
                                .unwrap_or_else(|| Pattern::Bind("_".to_string()))
                        })
                        .collect::<Vec<_>>();
                    sub_rows.push(row.with_column_replaced(column, sub, None));
                }
                other if !pattern_is_refutable(other) => {
                    let wildcards = vec![Pattern::Bind("_".to_string()); labels.len()];
                    let binding = (other.clone(), scrutinee.clone());
                    sub_rows.push(row.with_column_replaced(column, wildcards, Some(binding)));
                }
                _ => return Err(Error::PatternColumnConflict),
            }
        }

        self.compile_matrix(&sub_occ, sub_rows, motive)
    }

    /// What a column dispatches on, read off its refutable patterns. A column
    /// mixing constructors with literals (or `Nat` with `Bln`) is ill-formed.
    fn column_kind(&self, rows: &[MatrixRow], column: usize) -> Result<ColumnKind, Error> {
        let (mut variant, mut nat, mut bln) = (false, false, false);

        for row in rows {
            match &row.columns[column] {
                Pattern::Variant { .. } => variant = true,
                Pattern::Lit(PatternLit::Nat(_)) => nat = true,
                Pattern::Lit(PatternLit::Bln(_)) => bln = true,
                _ => {}
            }
        }

        match (variant, nat, bln) {
            (true, false, false) => Ok(ColumnKind::Inductive),
            (false, true, false) => Ok(ColumnKind::Nat),
            (false, false, true) => Ok(ColumnKind::Bln),
            _ => Err(Error::PatternColumnConflict),
        }
    }

    /// Dispatch on a constructor column. Emits one `inductive_match` arm per
    /// constructor: explicit rows contribute their nested sub-patterns; a
    /// `_`/binder catch-all flows into every arm (and, when present, materializes
    /// the unlisted constructors from the registry roster). Constructors covered
    /// by neither are left out for core's coverage/inversion check to judge.
    fn compile_inductive_column(
        &self,
        occurrences: &[core::Term],
        rows: Vec<MatrixRow>,
        column: usize,
        motive: Option<(Option<String>, core::Term)>,
    ) -> Result<core::Term, Error> {
        let scrutinee = occurrences[column].clone();

        let mut explicit: Vec<(String, usize)> = Vec::new();
        let mut catch_all = false;
        for row in &rows {
            match &row.columns[column] {
                Pattern::Variant { tag, args } => {
                    if !explicit.iter().any(|(t, _)| t == tag) {
                        explicit.push((tag.clone(), args.len()));
                    }
                }
                Pattern::Lit(_) => return Err(Error::PatternColumnConflict),
                _ => catch_all = true,
            }
        }

        // The constructors to emit arms for. A catch-all must cover the inductive's
        // *unlisted* constructors, so their arities come from the registry roster;
        // without one, only the explicit tags are emitted (core checks the rest).
        let roster = if catch_all {
            let tag = explicit
                .first()
                .map(|(tag, _)| tag.clone())
                .expect("a constructor column has at least one constructor pattern");
            let canonical = self.resolve_name(&Name::from(vec![tag.clone()]))?;

            match self.context.inductive_ctors(&canonical) {
                Some(roster) => roster.to_vec(),
                None => return Err(Error::UnresolvedMatchConstructor { tag }),
            }
        } else {
            explicit
        };

        let mut cases = Vec::new();
        for (tag, arity) in &roster {
            let sub_rows = self.specialize_constructor(&rows, column, tag, *arity, &scrutinee)?;

            // No explicit row and no catch-all reaches this constructor: omit the
            // arm so core can report it missing (or prune it as impossible).
            if sub_rows.is_empty() {
                continue;
            }

            let payload = (0..*arity)
                .map(|_| self.context.fresh_binder())
                .collect::<Vec<_>>();
            let payload_occ = payload
                .iter()
                .map(|name| core::Term::var(core::Var::free(name.clone())))
                .collect::<Vec<_>>();
            let sub_occ = splice(occurrences, column, &payload_occ);

            let body = self.compile_matrix(&sub_occ, sub_rows, None)?;
            cases.push((core::Atom::from(tag.as_str()), payload, body));
        }

        let (label, motive) = self.motive_or_hole(motive);
        Ok(core::Term::inductive_match(
            scrutinee,
            label.as_deref(),
            motive,
            cases,
        ))
    }

    /// The rows of one constructor arm: explicit `tag(args…)` rows splice their
    /// arguments in as new columns; a catch-all row wildcards the payload and
    /// binds the whole value; other constructors drop out.
    fn specialize_constructor(
        &self,
        rows: &[MatrixRow],
        column: usize,
        tag: &str,
        arity: usize,
        scrutinee: &core::Term,
    ) -> Result<Vec<MatrixRow>, Error> {
        let mut out = Vec::new();

        for row in rows {
            match &row.columns[column] {
                Pattern::Variant { tag: t, args } if t == tag => {
                    if args.len() != arity {
                        return Err(Error::PatternArityMismatch {
                            tag: tag.to_string(),
                            expected: arity,
                            found: args.len(),
                        });
                    }
                    out.push(row.with_column_replaced(column, args.clone(), None));
                }
                Pattern::Variant { .. } => {}
                other => {
                    let wildcards = vec![Pattern::Bind("_".to_string()); arity];
                    let binding = (other.clone(), scrutinee.clone());
                    out.push(row.with_column_replaced(column, wildcards, Some(binding)));
                }
            }
        }

        Ok(out)
    }

    /// Dispatch on a `Nat` literal column via `switch`: one case per distinct
    /// literal, the catch-all rows forming the (mandatory) default.
    fn compile_nat_column(
        &self,
        occurrences: &[core::Term],
        rows: Vec<MatrixRow>,
        column: usize,
        motive: Option<(Option<String>, core::Term)>,
    ) -> Result<core::Term, Error> {
        let scrutinee = occurrences[column].clone();
        let sub_occ = without_column(occurrences, column);

        if !rows
            .iter()
            .any(|row| !pattern_is_refutable(&row.columns[column]))
        {
            return Err(Error::NonExhaustiveScalarMatch);
        }

        let mut values: Vec<u32> = Vec::new();
        for row in &rows {
            if let Pattern::Lit(PatternLit::Nat(n)) = &row.columns[column]
                && !values.contains(n)
            {
                values.push(*n);
            }
        }

        let mut cases = Vec::new();
        for value in values {
            let predicate = |pattern: &Pattern| matches!(pattern, Pattern::Lit(PatternLit::Nat(n)) if *n == value);
            let sub_rows = self.specialize_scalar(&rows, column, predicate, &scrutinee);
            cases.push((value, self.compile_matrix(&sub_occ, sub_rows, None)?));
        }

        let default_rows = self.specialize_scalar(&rows, column, |_| false, &scrutinee);
        let default = self.compile_matrix(&sub_occ, default_rows, None)?;

        let (label, motive) = self.motive_or_hole(motive);
        Ok(core::Term::switch(
            scrutinee,
            label.as_deref(),
            motive,
            cases,
            default,
        ))
    }

    /// Dispatch on a `Bln` literal column via `bln_match`. Both branches must be
    /// reachable — a literal must appear, or a catch-all must supply it.
    fn compile_bln_column(
        &self,
        occurrences: &[core::Term],
        rows: Vec<MatrixRow>,
        column: usize,
        motive: Option<(Option<String>, core::Term)>,
    ) -> Result<core::Term, Error> {
        let scrutinee = occurrences[column].clone();
        let sub_occ = without_column(occurrences, column);

        let branch = |value: bool| {
            self.specialize_scalar(
                &rows,
                column,
                move |pattern| matches!(pattern, Pattern::Lit(PatternLit::Bln(b)) if *b == value),
                &scrutinee,
            )
        };

        let false_rows = branch(false);
        let true_rows = branch(true);
        if false_rows.is_empty() || true_rows.is_empty() {
            return Err(Error::NonExhaustiveScalarMatch);
        }

        let false_case = self.compile_matrix(&sub_occ, false_rows, None)?;
        let true_case = self.compile_matrix(&sub_occ, true_rows, None)?;

        let (label, motive) = self.motive_or_hole(motive);
        Ok(core::Term::bln_match(
            scrutinee,
            label.as_deref(),
            motive,
            false_case,
            true_case,
        ))
    }

    /// The rows of one scalar branch: rows whose column matches `predicate` drop
    /// the column; catch-all rows drop it too and bind the whole value; the rest
    /// fall away. Scalars have no payload, so no new columns appear.
    fn specialize_scalar(
        &self,
        rows: &[MatrixRow],
        column: usize,
        predicate: impl Fn(&Pattern) -> bool,
        scrutinee: &core::Term,
    ) -> Vec<MatrixRow> {
        let mut out = Vec::new();

        for row in rows {
            let pattern = &row.columns[column];
            if predicate(pattern) {
                out.push(row.with_column_dropped(column, None));
            } else if !pattern_is_refutable(pattern) {
                let binding = (pattern.clone(), scrutinee.clone());
                out.push(row.with_column_dropped(column, Some(binding)));
            }
        }

        out
    }

    /// The motive `(label, body)` a synthesized dispatch node carries: the user's
    /// (passed only to the outermost node), or a fresh metavariable for every
    /// inner node — inferred from its arms, the non-dependent reach.
    fn motive_or_hole(
        &self,
        motive: Option<(Option<String>, core::Term)>,
    ) -> (Option<String>, core::Term) {
        motive.unwrap_or_else(|| (None, core::Term::metavar(self.context.fresh_metavar())))
    }

    pub fn prim(&self, prim: &Prim) -> Result<core::Prim, Error> {
        Ok(match prim {
            Prim::BlnType => core::Prim::BlnType,
            Prim::Bln(b) => core::Prim::Bln(*b),
            Prim::BlnAnd(left, right) => core::Prim::BlnAnd(self.term(left)?, self.term(right)?),
            Prim::BlnOr(left, right) => core::Prim::BlnOr(self.term(left)?, self.term(right)?),
            Prim::BlnXor(left, right) => core::Prim::BlnXor(self.term(left)?, self.term(right)?),
            Prim::BlnEql(left, right) => core::Prim::BlnEql(self.term(left)?, self.term(right)?),
            Prim::BlnNeq(left, right) => core::Prim::BlnNeq(self.term(left)?, self.term(right)?),
            Prim::NatType => core::Prim::NatType,
            Prim::Nat(Nat::Zero) => core::Prim::Nat(core::Nat::Zero),
            Prim::Nat(Nat::Succ(NatLiteral::Number(spine, _), inner)) => {
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
            Prim::NatAnd(left, right) => core::Prim::NatAnd(self.term(left)?, self.term(right)?),
            Prim::NatOr(left, right) => core::Prim::NatOr(self.term(left)?, self.term(right)?),
            Prim::NatXor(left, right) => core::Prim::NatXor(self.term(left)?, self.term(right)?),
            Prim::NatShl(left, right) => core::Prim::NatShl(self.term(left)?, self.term(right)?),
            Prim::NatShr(left, right) => core::Prim::NatShr(self.term(left)?, self.term(right)?),
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
            Prim::IntAnd(left, right) => core::Prim::IntAnd(self.term(left)?, self.term(right)?),
            Prim::IntOr(left, right) => core::Prim::IntOr(self.term(left)?, self.term(right)?),
            Prim::IntXor(left, right) => core::Prim::IntXor(self.term(left)?, self.term(right)?),
            Prim::IntShl(left, right) => core::Prim::IntShl(self.term(left)?, self.term(right)?),
            Prim::IntShr(left, right) => core::Prim::IntShr(self.term(left)?, self.term(right)?),
            Prim::FltType => core::Prim::FltType,
            Prim::Flt(flt) => core::Prim::Flt(core::Flt::from_f32(*flt)),
            Prim::FltAdd(left, right) => core::Prim::flt_add(self.term(left)?, self.term(right)?),
            Prim::FltSub(left, right) => core::Prim::flt_sub(self.term(left)?, self.term(right)?),
            Prim::FltMul(left, right) => core::Prim::flt_mul(self.term(left)?, self.term(right)?),
            Prim::FltDiv(left, right) => core::Prim::flt_div(self.term(left)?, self.term(right)?),
            Prim::FltRem(left, right) => core::Prim::FltRem(self.term(left)?, self.term(right)?),
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
            Prim::FltToLeBin(inner) => core::Prim::flt_to_le_bin(self.term(inner)?),
            Prim::NatToInt(inner) => core::Prim::nat_to_int(self.term(inner)?),
            Prim::IoType => core::Prim::IoType,
            Prim::Io(token) => core::Prim::Io(*token),
            Prim::IoEql(left, right) => core::Prim::io_eql(self.term(left)?, self.term(right)?),
            Prim::IoRead(handle, count) => {
                core::Prim::io_read(self.term(handle)?, self.term(count)?)
            }
            Prim::IoWrite(handle, bytes) => {
                core::Prim::io_write(self.term(handle)?, self.term(bytes)?)
            }
            Prim::IoOpen(path, mode) => core::Prim::IoOpen(self.term(path)?, self.term(mode)?),
            Prim::IoLookup(host, port) => {
                core::Prim::IoLookup(self.term(host)?, self.term(port)?)
            }
            Prim::IoResolve(handle) => core::Prim::IoResolve(self.term(handle)?),
            Prim::IoSocket(addr) => core::Prim::IoSocket(self.term(addr)?),
            Prim::IoBind(handle, addr) => {
                core::Prim::IoBind(self.term(handle)?, self.term(addr)?)
            }
            Prim::IoConnect(handle, addr) => {
                core::Prim::IoConnect(self.term(handle)?, self.term(addr)?)
            }
            Prim::IoListen(handle, backlog) => {
                core::Prim::IoListen(self.term(handle)?, self.term(backlog)?)
            }
            Prim::IoAccept(handle) => core::Prim::IoAccept(self.term(handle)?),
            Prim::IoStartTls(handle, sni) => {
                core::Prim::IoStartTls(self.term(handle)?, self.term(sni)?)
            }
            Prim::IoTlsServerConfig(cert, key) => {
                core::Prim::IoTlsServerConfig(self.term(cert)?, self.term(key)?)
            }
            Prim::IoStartTlsServer(handle, cfg) => {
                core::Prim::IoStartTlsServer(self.term(handle)?, self.term(cfg)?)
            }
            Prim::IoSetNonblocking(handle, on) => {
                core::Prim::IoSetNonblocking(self.term(handle)?, self.term(on)?)
            }
            Prim::IoSetRecvTimeout(handle, ms) => {
                core::Prim::IoSetRecvTimeout(self.term(handle)?, self.term(ms)?)
            }
            Prim::IoSetSendTimeout(handle, ms) => {
                core::Prim::IoSetSendTimeout(self.term(handle)?, self.term(ms)?)
            }
            Prim::IoSetReuseaddr(handle, on) => {
                core::Prim::IoSetReuseaddr(self.term(handle)?, self.term(on)?)
            }
            Prim::IoPoll(handles, events, timeout) => core::Prim::IoPoll(
                self.term(handles)?,
                self.term(events)?,
                self.term(timeout)?,
            ),
            Prim::IoClose(handle) => core::Prim::IoClose(self.term(handle)?),
            Prim::IoClockWall => core::Prim::IoClockWall,
            Prim::IoClockMono => core::Prim::IoClockMono,
            Prim::IoRandom(count) => core::Prim::IoRandom(self.term(count)?),
            Prim::IoArgs => core::Prim::IoArgs,
            Prim::IoEnv(name) => core::Prim::IoEnv(self.term(name)?),
            Prim::IoExit(type_, code) => core::Prim::IoExit(self.term(type_)?, self.term(code)?),
            Prim::NatToFlt(inner) => core::Prim::nat_to_flt(self.term(inner)?),
            Prim::IntToNat(inner) => core::Prim::int_to_nat(self.term(inner)?),
            Prim::IntToFlt(inner) => core::Prim::int_to_flt(self.term(inner)?),
            Prim::FltToNat(inner) => core::Prim::flt_to_nat(self.term(inner)?),
            Prim::FltToInt(inner) => core::Prim::flt_to_int(self.term(inner)?),
            Prim::BinType => core::Prim::BinType,
            // `\hex` is a raw byte sequence.
            Prim::Bin(bytes) => core::Prim::Bin(bytes.clone()),
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
            Prim::BinFlatten(operand) => core::Prim::bin_flatten(self.term(operand)?),
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
            Prim::ArrFlatten(ty, operand) => {
                core::Prim::arr_flatten(self.term(ty)?, self.term(operand)?)
            }
            Prim::ArrMap(a, b, f, arr) => core::Prim::arr_map(
                self.term(a)?,
                self.term(b)?,
                self.term(f)?,
                self.term(arr)?,
            ),
            Prim::CellType(inner) => core::Prim::cell_type(self.term(inner)?),
            Prim::Cell(type_, init) => core::Prim::cell_new(self.term(type_)?, self.term(init)?),
            Prim::CellSet(type_, cell, value) => {
                core::Prim::cell_set(self.term(type_)?, self.term(cell)?, self.term(value)?)
            }
            Prim::CellGet(type_, cell) => core::Prim::cell_get(self.term(type_)?, self.term(cell)?),
        })
    }
}

/// One row of the pattern matrix during compilation.
struct MatrixRow {
    /// One pattern per live column (parallel to the occurrence list).
    columns: Vec<Pattern>,
    /// Irrefutable/catch-all patterns peeled from already-dispatched columns,
    /// each paired with the occurrence it binds; applied around the body at the
    /// leaf. A catch-all binds the *whole* dispatched value, not its payload.
    bindings: Vec<(Pattern, core::Term)>,
    /// The arm body, already lowered to core (cloned when a row fans out into
    /// several constructor arms — branches are mutually exclusive, so sharing the
    /// term across them is sound).
    body: core::Term,
}

impl MatrixRow {
    /// Replace this row's `column` with `replacement` sub-patterns (a
    /// constructor's payload becoming new columns), optionally recording a
    /// binding. Clones the row.
    fn with_column_replaced(
        &self,
        column: usize,
        replacement: Vec<Pattern>,
        binding: Option<(Pattern, core::Term)>,
    ) -> MatrixRow {
        let mut columns = self.columns[..column].to_vec();
        columns.extend(replacement);
        columns.extend_from_slice(&self.columns[column + 1..]);

        let mut bindings = self.bindings.clone();
        bindings.extend(binding);

        MatrixRow {
            columns,
            bindings,
            body: self.body.clone(),
        }
    }

    /// Drop this row's `column` (a scalar branch — scalars carry no payload),
    /// optionally recording a binding. Clones the row.
    fn with_column_dropped(
        &self,
        column: usize,
        binding: Option<(Pattern, core::Term)>,
    ) -> MatrixRow {
        self.with_column_replaced(column, vec![], binding)
    }
}

/// What a matrix column dispatches on.
enum ColumnKind {
    Inductive,
    Nat,
    Bln,
}

/// Whether a pattern forces a runtime test — a constructor or scalar literal,
/// possibly nested inside a tuple/struct. Plain binders/wildcards (and tuples or
/// structs of only such) are irrefutable and never split a column.
fn pattern_is_refutable(pattern: &Pattern) -> bool {
    match pattern {
        Pattern::Bind(_) => false,
        Pattern::Tuple(fields) => fields.iter().any(pattern_is_refutable),
        Pattern::Struct { fields, .. } => fields
            .iter()
            .any(|(_, pattern)| pattern_is_refutable(pattern)),
        Pattern::Variant { .. } | Pattern::Lit(_) => true,
    }
}

/// The occurrence list with `column` replaced by `replacement` (a constructor's
/// payload occurrences spliced in at that position).
fn splice(
    occurrences: &[core::Term],
    column: usize,
    replacement: &[core::Term],
) -> Vec<core::Term> {
    let mut out = occurrences[..column].to_vec();
    out.extend_from_slice(replacement);
    out.extend_from_slice(&occurrences[column + 1..]);
    out
}

/// The occurrence list with `column` removed (a scalar branch consumes its
/// column without introducing any).
fn without_column(occurrences: &[core::Term], column: usize) -> Vec<core::Term> {
    splice(occurrences, column, &[])
}
