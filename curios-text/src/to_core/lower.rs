use {
    super::Context,
    crate::{
        BinMatch, BinPattern, BinSegment, Error, Field, Let, LstEntry, LstMatch, LstPattern, Match,
        MatchPattern, MatrixArm, Motive, Name, Nat, NatLiteral, NatMatch, NatPattern, Pattern,
        PatternField, Prim, Rec, StructLitEntry, Subterm, Syn, Term,
    },
    num_bigint::BigUint,
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet},
        mem,
        sync::Arc,
    },
};

pub(super) struct Lower<'a, 'b> {
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

/// One in-progress row of the matrix compiler's recursion (see
/// [`Lower::compile_matrix`]): the still-unconsumed column patterns (left to
/// right, one per not-yet-retired column, borrowed from the original
/// [`MatrixArm`]), the `let` bindings accumulated so far from retired
/// all-[`MatchPattern::Binder`] columns — applied at the leaf, outermost
/// first, matching [`Lower::lower_pattern_fields`]'s "first field's let ends
/// up outermost" convention — and the row's own (still-surface) body. A
/// name already bound directly by an enclosing core binder (see
/// [`Lower::compile_ctor`]'s single-row fast path) needs no entry here at
/// all — [`Self::scoped`] is called inline, right where that binder's name
/// is decided, instead of threading a second bookkeeping list through the
/// recursion for it.
struct MatrixRow<'t> {
    patterns: Vec<&'t MatchPattern>,
    binds: Vec<(String, curios_core::Term)>,
    body: &'t Term,
}

impl<'a, 'b> Lower<'a, 'b> {
    pub(super) fn new(context: &'a Context<'b>) -> Self {
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

    /// Lowers a *value* body — a top-level `let`/`rec` body, a witness field,
    /// or the entrypoint tail. Every value body is a region root: each `!` in
    /// it hoists here (never past a boundary — a lambda body, match arm, or
    /// `rec` item re-roots) and is rewired through `/syn/Monad/bind`, whose
    /// `use` binder resolves the `Monad` witness per site. Types go through
    /// [`Self::term`], where `!` is rejected.
    pub(super) fn value(&self, term: &Term) -> Result<curios_core::Term, Error> {
        self.region(term)
    }

    pub(super) fn term(&self, term: &Term) -> Result<curios_core::Term, Error> {
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
        curios_core::Term::struct_(
            "/syn/Str/Str",
            Vec::<curios_core::Term>::new(),
            [
                curios_core::Term::prim(curios_core::Prim::Bin(bytes.to_vec())),
                self.utf8_derivation(bytes, Self::scan_lead()),
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

    // A `/syn` literal — its value is synthesized from `/syn` by the meta-emitter
    // rather than lowered to a core primitive.
    fn syn_literal(&self, syn: &Syn) -> Result<curios_core::Term, Error> {
        match syn {
            Syn::Str(string) => Ok(self.str_literal(string.as_bytes())),
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
            // elaboration mints metavariables), and the written entries — plain
            // field values with their names (validated positionally and dropped
            // by elaborate), `use <term>` fills for a concept's `use`-marked
            // positions, and a `..base` spread carrying its base. Construction
            // privacy and spread shape are enforced in core
            // (`elaborate_struct`), alongside projection privacy.
            Subterm::StructLit(lit) => curios_core::Term::struct_entries(
                self.resolve_name(&lit.head)?,
                lit.params
                    .iter()
                    .map(|p| self.term(p))
                    .collect::<Result<Vec<_>, Error>>()?,
                lit.entries
                    .iter()
                    .map(|entry| match entry {
                        StructLitEntry::Field(field) => Ok((
                            curios_core::StructEntry::Field(field.label.clone()),
                            self.term(&field.desugared_value())?,
                        )),
                        StructLitEntry::Use(term) => {
                            Ok((curios_core::StructEntry::Use, self.term(term)?))
                        }
                        StructLitEntry::Spread(term) => {
                            Ok((curios_core::StructEntry::Spread, self.term(term)?))
                        }
                    })
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
                Match::Matrix(um) => {
                    // The matrix compiler recursively decomposes (possibly
                    // nested, across constructors/tuples/structs) arm
                    // patterns into single-level core `Match`/projection
                    // forms — see `Self::compile_matrix`.
                    let head = self.term(&um.head)?;
                    self.compile_matrix(head, &um.motive, &um.arms, Self::term)?
                }
                Match::Lst(LstMatch {
                    head,
                    motive,
                    empty_case,
                    head_label,
                    tail_label,
                    ih_label,
                    cons_case,
                }) => {
                    let (label, body) = self.motive_parts(motive)?;
                    let head_label = self.pattern_binder_name(head_label);
                    let tail_label = self.pattern_binder_name(tail_label);
                    let ih_label = self.cons_ih_name(ih_label);
                    // The element type is type-directed (read off the scrutinee
                    // during elaboration), so lowering leaves it a hole.
                    curios_core::Term::lst_match(
                        self.term(head)?,
                        curios_core::Term::metavar(self.context.fresh_metavar()),
                        label,
                        body,
                        self.term(empty_case)?,
                        head_label.clone(),
                        tail_label.clone(),
                        ih_label.clone(),
                        self.scoped([head_label, tail_label, ih_label], || self.term(cons_case))?,
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
                    let head_label = self.pattern_binder_name(head_label);
                    let tail_label = self.pattern_binder_name(tail_label);
                    let ih_label = self.cons_ih_name(ih_label);
                    curios_core::Term::bin_match(
                        self.term(head)?,
                        label,
                        body,
                        self.term(empty_case)?,
                        head_label.clone(),
                        tail_label.clone(),
                        ih_label.clone(),
                        self.scoped([head_label, tail_label, ih_label], || self.term(cons_case))?,
                    )
                }
            },
            // A `let` is non-recursive: its binder is in scope only in the tail,
            // never in its own type or value.
            Subterm::Let(let_) => {
                let type_ = self.term(&let_.signature.type_())?;
                let value = self.term(&let_.signature.body())?;
                let tail =
                    self.scoped(Self::pattern_names(&let_.binder), || self.term(&let_.tail))?;
                self.bind_pattern(&let_.binder, type_, value, tail)
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
            // A bang here was reached through a *type* lowering (an annotation,
            // a motive, a Π/Σ component): types have no region to hoist to.
            // Value bodies enter through `value`/`region`, which eliminates
            // every `Bang` before this arm could see it.
            Subterm::Bang(_) => return Err(Error::BangInTypePosition),
        })
    }

    /// Desugars `term` as a single **region**. A region is a stretch of a value
    /// body that shares one continuation; each `!` in it hoists to the top of
    /// the region, never past a boundary (lambda body, match arm, `rec` item).
    /// Boundaries re-root a region. Every hoisted action is sequenced through
    /// `/syn/Monad/bind` — see `wrap`.
    fn region(&self, term: &Term) -> Result<curios_core::Term, Error> {
        match term.as_subterm() {
            // A `let`'s bound expression evaluates in place (its bangs hoist to
            // this region); the tail continues the same region (a bang there
            // hoists after `x` is bound, not above the `let`).
            Subterm::Let(let_) => {
                let mut binds = Vec::new();
                let let_term = self.build_let(let_, &mut binds)?;
                self.wrap(binds, let_term)
            }
            // The scrutinee evaluates before branching (its bangs hoist here);
            // each arm is its own region (branch-local effects).
            Subterm::Match(match_) => {
                let mut binds = Vec::new();
                let match_term = self.match_region(match_, &mut binds)?;
                self.wrap(binds, match_term)
            }
            // A lambda re-roots the region.
            Subterm::Func(func) => {
                let body =
                    self.scoped(Self::param_names(&func.params), || self.region(&func.body))?;
                let (params, body) = self.lower_func_params(&func.params, body)?;
                Ok(curios_core::Term::func(params, body))
            }
            // A `rec`'s item bodies are their own regions (hoisting an action
            // out of a recursive binding would change how often it runs); the
            // tail continues like a `let` tail.
            Subterm::Rec(rec) => self.build_rec(rec),
            // Spine forms (atomic / apply / tuple / proj): collect bangs in
            // left-to-right evaluation order, then wrap.
            _ => {
                let mut binds = Vec::new();
                let body = self.collect(term, &mut binds)?;
                self.wrap(binds, body)
            }
        }
    }

    /// Builds the core `rec` for a `rec` inside a value body: item types are
    /// types, item bodies are fresh region roots, and the tail continues as its
    /// own region (a bang there hoists after the bindings, not above them).
    fn build_rec(&self, rec: &Rec) -> Result<curios_core::Term, Error> {
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
                            self.region(&it.signature.body())?,
                        ))
                    })
                    .collect::<Result<Vec<_>, Error>>()?,
                self.region(&rec.tail)?,
            ))
        })
    }

    /// Walks a non-boundary expression, elaborating to core and accumulating each
    /// `Bang` into `binds` (in evaluation order) replaced by a fresh variable.
    /// Boundary/binding forms desugar as their own nested region; `let`/`match`
    /// hoist their bound-expression/scrutinee bangs into the *enclosing* `binds`.
    fn collect(
        &self,
        term: &Term,
        binds: &mut Vec<(String, curios_core::Term)>,
    ) -> Result<curios_core::Term, Error> {
        Ok(match term.as_subterm() {
            Subterm::Bang(action) => {
                // The action is itself desugared first, so its inner bangs
                // evaluate before this one (left-to-right).
                let action = self.collect(action, binds)?;
                let name = self.context.fresh_binder();
                let var = curios_core::Term::var(curios_core::Var::free(name.clone()));
                binds.push((name, action));
                var
            }
            Subterm::Apply(apply) => curios_core::Term::apply_marked(
                self.collect(&apply.head, binds)?,
                apply
                    .params
                    .iter()
                    .map(|(plicity, p)| Ok((*plicity, self.collect(p, binds)?)))
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            Subterm::Tuple(tuple) => curios_core::Term::tuple_named(
                tuple
                    .fields
                    .iter()
                    .map(|field| {
                        let value = field.desugared_value();
                        Ok((field.label.clone(), self.collect(&value, binds)?))
                    })
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            Subterm::Proj(proj) => {
                let head = self.collect(&proj.head, binds)?;
                match &proj.field {
                    Field::Index(index) => curios_core::Term::proj(head, *index),
                    Field::Label(label) => curios_core::Term::proj_label(head, label.clone()),
                }
            }
            // A struct literal's entry values hoist their bangs into this
            // region, exactly like a tuple's fields.
            Subterm::StructLit(lit) => curios_core::Term::struct_entries(
                self.resolve_name(&lit.head)?,
                lit.params
                    .iter()
                    .map(|p| self.collect(p, binds))
                    .collect::<Result<Vec<_>, Error>>()?,
                lit.entries
                    .iter()
                    .map(|entry| match entry {
                        StructLitEntry::Field(field) => {
                            let value = field.desugared_value();
                            Ok((
                                curios_core::StructEntry::Field(field.label.clone()),
                                self.collect(&value, binds)?,
                            ))
                        }
                        StructLitEntry::Use(term) => {
                            Ok((curios_core::StructEntry::Use, self.collect(term, binds)?))
                        }
                        StructLitEntry::Spread(term) => {
                            Ok((curios_core::StructEntry::Spread, self.collect(term, binds)?))
                        }
                    })
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            // An infix operator's operands hoist their bangs into this region,
            // exactly like an application's arguments.
            Subterm::Infix(infix) => curios_core::Term::infix(
                infix.op,
                self.collect(&infix.left, binds)?,
                self.collect(&infix.right, binds)?,
            ),
            // An array literal's elements and spread operands hoist their
            // bangs into this region, like an application's arguments.
            Subterm::Prim(Prim::Lst(entries)) => curios_core::Term::prim(
                self.lower_lst_literal(entries, |term| self.collect(term, binds))?,
            ),
            // A `Bin` literal's spread operands hoist likewise (a spread-free
            // `Bin` has no subterms and lowers unchanged).
            Subterm::Prim(Prim::Bin(segments)) => curios_core::Term::prim(
                self.lower_bin_literal(segments, |term| self.collect(term, binds))?,
            ),
            // A `let`/`match` sub-expression hoists its bound-expression /
            // scrutinee bangs into the enclosing region (this `binds`).
            Subterm::Let(let_) => self.build_let(let_, binds)?,
            Subterm::Match(match_) => self.match_region(match_, binds)?,
            // A lambda is a value and a `rec` binds recursively: neither hoists
            // anything outward, so desugar each as its own region.
            Subterm::Func(_) | Subterm::Rec(_) => self.region(term)?,
            // Leaves elaborate normally. A `Bang` reachable here (e.g. nested in a
            // type position) hits `self.term`'s `Bang` arm and is rejected.
            _ => self.term(term)?,
        })
    }

    /// Builds `let x = value; tail` for a `let` inside a region, collecting the
    /// bound expression's bangs into `binds` and desugaring the tail as the
    /// continuation of the same region.
    fn build_let(
        &self,
        let_: &Let,
        binds: &mut Vec<(String, curios_core::Term)>,
    ) -> Result<curios_core::Term, Error> {
        let value = self.collect(&let_.signature.body(), binds)?;
        let tail = self.scoped(Self::pattern_names(&let_.binder), || {
            self.region(&let_.tail)
        })?;
        let type_ = self.term(&let_.signature.type_())?;
        Ok(self.bind_pattern(&let_.binder, type_, value, tail))
    }

    /// Lowers a function's parameters into core binder `(name, domain)` pairs.
    /// A plain-name parameter binds its name directly, unchanged; an
    /// un-annotated parameter takes a fresh metavar domain. A compound
    /// pattern's core binder is a fresh synthetic name, and the (already
    /// lowered) `body` is wrapped with its field-`let` chain — processed in
    /// reverse so each pattern's chain wraps the body *before* an earlier
    /// pattern's chain wraps that, giving the declaration-order nesting the
    /// spec's motivating example expects, then reversed back so the returned
    /// param list stays in declaration order.
    fn lower_func_params(
        &self,
        params: &[(Pattern, Option<Term>)],
        mut body: curios_core::Term,
    ) -> Result<(Vec<(String, curios_core::Term)>, curios_core::Term), Error> {
        let mut lowered = Vec::with_capacity(params.len());
        for (pattern, annotation) in params.iter().rev() {
            let domain = match annotation {
                Some(annotation) => self.term(annotation)?,
                None => curios_core::Term::metavar(self.context.fresh_metavar()),
            };
            match pattern {
                Pattern::Binder(Some(name)) => {
                    lowered.push((self.pattern_binder_name(name), domain))
                }
                Pattern::Binder(None) => lowered.push((self.context.fresh_binder(), domain)),
                Pattern::Tuple(fields) | Pattern::Struct { fields, .. } => {
                    let synthetic = self.context.fresh_binder();
                    body = self.lower_pattern_fields(fields, &synthetic, body);
                    lowered.push((synthetic, domain));
                }
            }
        }
        lowered.reverse();
        Ok((lowered, body))
    }

    /// Builds `let pat = value : type_; tail` for a pattern in any of the
    /// three binder positions: `Pattern::Binder` is today's single core
    /// `let_` call, unchanged — the whole reason the plain-name path stays a
    /// zero-cost passthrough. A compound pattern mints one fresh synthetic
    /// binder (via [`Context::fresh_binder`]) carrying `type_` (the caller's
    /// own annotation, so it is still checked), then projects each field off
    /// it via [`Self::lower_pattern_fields`]. The synthetic binder is minted
    /// unconditionally, even when `value` is already a bare variable
    /// reference: reusing it directly would risk silently dropping `type_`'s
    /// check (e.g. `let (x, y) : Point = pair;` must still check
    /// `pair : Point`). The extra trivial `let` this occasionally emits is
    /// exactly the shape `cont`'s copy-threading optimization already
    /// collapses, so it costs nothing at runtime.
    fn bind_pattern(
        &self,
        pattern: &Pattern,
        type_: curios_core::Term,
        value: curios_core::Term,
        tail: curios_core::Term,
    ) -> curios_core::Term {
        match pattern {
            Pattern::Binder(Some(name)) => {
                curios_core::Term::let_(self.pattern_binder_name(name), type_, value, tail)
            }
            Pattern::Binder(None) => {
                curios_core::Term::let_(self.context.fresh_binder(), type_, value, tail)
            }
            Pattern::Tuple(fields) | Pattern::Struct { fields, .. } => {
                let synthetic = self.context.fresh_binder();
                let inner = self.lower_pattern_fields(fields, &synthetic, tail);
                curios_core::Term::let_(synthetic, type_, value, inner)
            }
        }
    }

    /// Projects each field of a compound pattern off the (already-bound) core
    /// variable `scrutinee_name`, in field order — folded right-to-left so
    /// the first field's `let` ends up outermost, matching the order a person
    /// would hand-write (`let x = p0.0; let y = p0.1; …`) — recursing into
    /// [`Self::bind_pattern`] for nested patterns. Each field's own type is a
    /// fresh metavar hole: there is never a per-field annotation to give,
    /// exactly like a hand-written `let x = p.0;`.
    fn lower_pattern_fields(
        &self,
        fields: &[PatternField],
        scrutinee_name: &str,
        tail: curios_core::Term,
    ) -> curios_core::Term {
        let mut tail = tail;
        for (index, field) in fields.iter().enumerate().rev() {
            let scrutinee = curios_core::Term::var(curios_core::Var::free(scrutinee_name));
            let proj = match &field.label {
                Some(label) => curios_core::Term::proj_label(scrutinee, label.clone()),
                None => curios_core::Term::proj(scrutinee, index),
            };
            let hole = curios_core::Term::metavar(self.context.fresh_metavar());
            tail = self.bind_pattern(&field.value, hole, proj, tail);
        }
        tail
    }

    /// The binder names a parameter list introduces — every leaf binder name
    /// in each parameter's pattern, flattened, all in scope across the body.
    /// These shadow like-named module bindings; the wildcard `_` rides along
    /// but is ignored by [`Self::scoped`].
    fn param_names(params: &[(Pattern, Option<Term>)]) -> Vec<String> {
        params
            .iter()
            .flat_map(|(pattern, _)| Self::pattern_names(pattern))
            .collect()
    }

    /// Every `Pattern::Binder` leaf name in `pattern`, recursing through
    /// nested tuple/struct fields in field order.
    fn pattern_names(pattern: &Pattern) -> Vec<String> {
        match pattern {
            Pattern::Binder(Some(name)) => vec![name.clone()],
            // No source name at all — nothing to shadow-track.
            Pattern::Binder(None) => vec![],
            Pattern::Tuple(fields) | Pattern::Struct { fields, .. } => fields
                .iter()
                .flat_map(|field| Self::pattern_names(&field.value))
                .collect(),
        }
    }

    /// A pattern binder's core name: `_` mints a fresh internal name (so repeated
    /// wildcards never collide), any other identifier is used verbatim.
    fn pattern_binder_name(&self, name: &str) -> String {
        match name {
            "_" => self.context.fresh_binder(),
            name => name.to_string(),
        }
    }

    /// An `Lst`/`Bin` cons arm's induction-hypothesis binder name: an omitted
    /// `; ih` (`None` — there is no source name at all) mints a fresh internal
    /// name directly; a written one gets the same wildcard-safe treatment as
    /// [`Self::pattern_binder_name`].
    fn cons_ih_name(&self, ih_label: &Option<String>) -> String {
        match ih_label {
            Some(name) => self.pattern_binder_name(name),
            None => self.context.fresh_binder(),
        }
    }

    /// Desugars a `match` inside a region: the scrutinee's bangs are collected
    /// into `binds` (hoisted out — the scrutinee runs unconditionally), while
    /// each arm is desugared as its own region (branch-local effects). This
    /// mirrors the `Match` arm of `subterm`, swapping `self.term` for `collect`
    /// on heads and `region` on arm bodies.
    fn match_region(
        &self,
        match_: &Match,
        binds: &mut Vec<(String, curios_core::Term)>,
    ) -> Result<curios_core::Term, Error> {
        Ok(match match_ {
            Match::Bln(bm) => {
                let (label, body) = self.motive_parts(&bm.motive)?;
                curios_core::Term::bln_match(
                    self.collect(&bm.head, binds)?,
                    label,
                    body,
                    self.region(&bm.false_case)?,
                    self.region(&bm.true_case)?,
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
                    self.collect(head, binds)?,
                    label,
                    body,
                    self.region(zero_case)?,
                    pred_label.clone(),
                    ih_label.clone(),
                    self.scoped([pred_label.clone(), ih_label.clone()], || {
                        self.region(succ_case)
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
                    self.collect(head, binds)?,
                    label,
                    motive_body,
                    cases
                        .iter()
                        .map(|(&nat, body)| Ok((nat, self.region(body)?)))
                        .collect::<Result<Vec<_>, Error>>()?,
                    self.region(default)?,
                )
            }
            Match::Matrix(um) => {
                // Mirrors the `Match::Matrix` arm of `subterm` (see there and
                // `Self::compile_matrix`), swapping `collect` on the head and
                // `region` on the arm bodies (branch-local effects).
                let head = self.collect(&um.head, binds)?;
                self.compile_matrix(head, &um.motive, &um.arms, Self::region)?
            }
            Match::Lst(LstMatch {
                head,
                motive,
                empty_case,
                head_label,
                tail_label,
                ih_label,
                cons_case,
            }) => {
                let (label, body) = self.motive_parts(motive)?;
                let head_label = self.pattern_binder_name(head_label);
                let tail_label = self.pattern_binder_name(tail_label);
                let ih_label = self.cons_ih_name(ih_label);
                curios_core::Term::lst_match(
                    self.collect(head, binds)?,
                    curios_core::Term::metavar(self.context.fresh_metavar()),
                    label,
                    body,
                    self.region(empty_case)?,
                    head_label.clone(),
                    tail_label.clone(),
                    ih_label.clone(),
                    self.scoped([head_label, tail_label, ih_label], || {
                        self.region(cons_case)
                    })?,
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
                let head_label = self.pattern_binder_name(head_label);
                let tail_label = self.pattern_binder_name(tail_label);
                let ih_label = self.cons_ih_name(ih_label);
                curios_core::Term::bin_match(
                    self.collect(head, binds)?,
                    label,
                    body,
                    self.region(empty_case)?,
                    head_label.clone(),
                    tail_label.clone(),
                    ih_label.clone(),
                    self.scoped([head_label, tail_label, ih_label], || {
                        self.region(cons_case)
                    })?,
                )
            }
        })
    }

    /// Wraps `body` in one `/syn/Monad/bind` application per collected bang.
    /// The first-collected bang (`binds[0]`) becomes the outermost bind, preserving
    /// left-to-right evaluation order. Continuation lambdas are built with
    /// `curios_core::Term::func` over the gensym'd free name, whose `capture` closes it
    /// robustly under nesting; the domain is a fresh hole, inference-solved.
    /// Elaborating each application inserts fresh implicits and a fresh `use`
    /// witness slot per `!` site: the action's type pins the constructor (via
    /// the flex-apply imitation rule), which resolves the `Monad` witness — so a
    /// region can sequence actions of differing result types, and different
    /// regions can use different monads.
    fn wrap(
        &self,
        binds: Vec<(String, curios_core::Term)>,
        body: curios_core::Term,
    ) -> Result<curios_core::Term, Error> {
        binds
            .into_iter()
            .rev()
            .try_fold(body, |acc, (name, action)| {
                let domain = curios_core::Term::metavar(self.context.fresh_metavar());
                let cont = curios_core::Term::func([(name, domain)], acc);
                // The already-resolved core name: the `Monad` concept at
                // `/syn`'s top level, method wrapper `bind`.
                Ok(Self::syn_call("/syn/Monad/bind", [action, cont]))
            })
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

    /// The entry point for a match whose arm patterns may nest across
    /// constructors, tuples, and structs (see [`MatchPattern`]) — compiled
    /// down into the single-level core forms above, exactly what a person
    /// would get from hand-nesting matches today (proven end to end by
    /// `BigNat.crs`'s style of code). `leaf` is the per-body lowering —
    /// [`Self::term`] on the plain path, [`Self::region`] on the region path
    /// — so both share this compiler, mirroring every other `Match` arm's
    /// `term`/`region` split.
    ///
    /// Zero arms (a vacuous elimination, e.g. of `False`) needs no
    /// recursion at all — there is nothing to infer a dispatch kind from, so
    /// it goes straight to [`Self::inductive_match`] exactly as today.
    ///
    /// A dependent motive (`Motive::Scrutinee`/`Motive::Annotated`) is only
    /// meaningful when the head itself dispatches on a constructor tag
    /// directly — every arm's *top-level* pattern being [`MatchPattern::Ctor`]
    /// — since that is the only case where a core `Match` node exists for
    /// the *original* scrutinee to attach the motive to (a
    /// tuple/struct-headed or plain-binder match never builds one; it just
    /// projects). Every deeper/inner split the recursion synthesizes never
    /// needs its own motive at all: an absent motive lowers to a fresh
    /// metavariable ([`Self::motive_parts`]'s `None` case), which core
    /// elaboration unifies against whatever expected type flows in from the
    /// enclosing checking context — no currying needed for a single head.
    fn compile_matrix(
        &self,
        head: curios_core::Term,
        motive: &Option<Motive>,
        arms: &[MatrixArm],
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Term, Error> {
        if arms.is_empty() {
            return self.inductive_match(head, motive, Vec::new());
        }

        let homogeneous_dispatch = arms[0].pattern.is_dispatchable()
            && arms
                .iter()
                .all(|arm| mem::discriminant(&arm.pattern) == mem::discriminant(&arms[0].pattern));
        if !homogeneous_dispatch
            && matches!(
                motive,
                Some(Motive::Scrutinee { .. } | Motive::Annotated { .. })
            )
        {
            return Err(Error::MatrixMotiveRequiresCtorHead);
        }

        let rows = arms
            .iter()
            .map(|arm| MatrixRow {
                patterns: vec![&arm.pattern],
                binds: Vec::new(),
                body: &arm.body,
            })
            .collect();

        self.compile(vec![head], rows, Some(motive), leaf)
    }

    /// The recursive step: classifies column 0 across every row and either
    /// retires it (every row a plain binder — never splits), explodes it
    /// (every row a tuple/struct — exactly one shape, so this is projection,
    /// not dispatch, via [`Self::compile_fields`]), or groups by constructor
    /// tag and recurses per group (via [`Self::compile_ctor`]). Mixing a
    /// plain binder with a concrete shape in the same column is the "Path A"
    /// full-enumeration boundary this grammar doesn't support (no
    /// wildcard/catch-all) — a hard error, not a panic. `top_motive` is
    /// `Some` only on [`Self::compile_matrix`]'s own initial call; every
    /// recursive call passes `None` (see that method's doc comment).
    fn compile(
        &self,
        mut columns: Vec<curios_core::Term>,
        rows: Vec<MatrixRow<'_>>,
        top_motive: Option<&Option<Motive>>,
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Term, Error> {
        if columns.is_empty() {
            return match rows.len() {
                1 => self.finish_row(rows.into_iter().next().unwrap(), leaf),
                _ => Err(Error::MatrixDuplicateRow),
            };
        }

        if rows
            .iter()
            .all(|row| matches!(row.patterns[0], MatchPattern::Binder(_)))
        {
            let scrutinee = columns.remove(0);
            let rows = rows
                .into_iter()
                .map(|mut row| {
                    let MatchPattern::Binder(name) = row.patterns.remove(0) else {
                        unreachable!("every row classified as Binder")
                    };
                    row.binds.push((name.clone(), scrutinee.clone()));
                    row
                })
                .collect();
            return self.compile(columns, rows, None, leaf);
        }

        if rows
            .iter()
            .any(|row| matches!(row.patterns[0], MatchPattern::Binder(_)))
        {
            return Err(Error::MatrixInconsistentShape);
        }

        match rows[0].patterns[0] {
            MatchPattern::Ctor { .. } => {
                if rows
                    .iter()
                    .any(|row| !matches!(row.patterns[0], MatchPattern::Ctor { .. }))
                {
                    return Err(Error::MatrixInconsistentShape);
                }
                self.compile_ctor(columns, rows, top_motive, leaf)
            }
            MatchPattern::Tuple(fields) => {
                let arity = fields.len();
                if rows.iter().any(
                    |row| !matches!(row.patterns[0], MatchPattern::Tuple(f) if f.len() == arity),
                ) {
                    return Err(Error::MatrixInconsistentShape);
                }
                self.compile_fields(columns, rows, leaf)
            }
            MatchPattern::Struct { head, fields } => {
                if rows.iter().any(|row| {
                    !matches!(row.patterns[0], MatchPattern::Struct { head: h, fields: f }
                        if h == head && f.len() == fields.len())
                }) {
                    return Err(Error::MatrixInconsistentShape);
                }
                self.compile_fields(columns, rows, leaf)
            }
            // The four hardcoded-carrier leaves. `Nat`/`Lst`/`Bin` each nest
            // their own two-case sub-pattern (`NatPattern::{Zero,Succ}` and
            // friends), so matching the outer variant alone already treats
            // both sub-cases as one shape here — no separate classifier
            // needed (see `is_dispatchable`'s doc comment).
            MatchPattern::Bln(_) => {
                if rows
                    .iter()
                    .any(|row| !matches!(row.patterns[0], MatchPattern::Bln(_)))
                {
                    return Err(Error::MatrixInconsistentShape);
                }
                self.compile_bln(columns, rows, top_motive, leaf)
            }
            MatchPattern::Nat(_) => {
                if rows
                    .iter()
                    .any(|row| !matches!(row.patterns[0], MatchPattern::Nat(_)))
                {
                    return Err(Error::MatrixInconsistentShape);
                }
                self.compile_nat(columns, rows, top_motive, leaf)
            }
            MatchPattern::Lst(_) => {
                if rows
                    .iter()
                    .any(|row| !matches!(row.patterns[0], MatchPattern::Lst(_)))
                {
                    return Err(Error::MatrixInconsistentShape);
                }
                self.compile_lst(columns, rows, top_motive, leaf)
            }
            MatchPattern::Bin(_) => {
                if rows
                    .iter()
                    .any(|row| !matches!(row.patterns[0], MatchPattern::Bin(_)))
                {
                    return Err(Error::MatrixInconsistentShape);
                }
                self.compile_bin(columns, rows, top_motive, leaf)
            }
            MatchPattern::Binder(_) => unreachable!("handled above"),
        }
    }

    /// Groups rows by their column-0 constructor tag (distinct tags freely
    /// coexist — that's the whole grouping mechanism; two rows sharing a tag
    /// recurse together, further split by their own sub-patterns). Two rows
    /// that end up identical in every column (including a literal repeated
    /// tag with no further distinguishing sub-pattern) are caught by
    /// [`Self::compile`]'s leaf case, not here.
    ///
    /// A tag with exactly one row needs no synthetic binder at all for a
    /// plain-binder slot: its own written name (wildcard-safe via
    /// [`Self::pattern_binder_name`]) becomes the core arm's own binder
    /// directly, exactly matching today's flat lowering — this is the
    /// overwhelmingly common case (every constructor tag appears once). A
    /// slot needing further decomposition (a nested sub-pattern), or a slot
    /// in a group with more than one row (which may need to rebind it
    /// differently per row), still gets a fresh synthetic column, handled by
    /// the general recursion. This distinction matters beyond style: minting
    /// a synthetic name for a slot that didn't need one, then immediately
    /// `let`-renaming it back to the written name, produces a core binder
    /// whose only label is that gensym — which the erasure pass's hint-based
    /// fresh-naming (`Context::fresh`) then chains into another gensym,
    /// compounding across nested lets until a reference outruns its own
    /// binding.
    fn compile_ctor(
        &self,
        mut columns: Vec<curios_core::Term>,
        rows: Vec<MatrixRow<'_>>,
        top_motive: Option<&Option<Motive>>,
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Term, Error> {
        let scrutinee = columns.remove(0);
        let rest = columns;

        let mut groups: BTreeMap<String, Vec<(&[MatchPattern], MatrixRow<'_>)>> = BTreeMap::new();
        for mut row in rows {
            let MatchPattern::Ctor { tag, args } = row.patterns.remove(0) else {
                unreachable!("every row classified as Ctor")
            };
            groups
                .entry(tag.clone())
                .or_default()
                .push((args.as_slice(), row));
        }

        let mut cases = Vec::with_capacity(groups.len());
        for (tag, mut group) in groups {
            let arity = group[0].0.len();
            if group.iter().any(|(args, _)| args.len() != arity) {
                return Err(Error::MatrixInconsistentShape);
            }

            // The single-row fast path: a plain-binder slot's own name
            // becomes the core arm's binder directly, with `self.scoped`
            // called right here — exactly where the name is decided —
            // rather than deferred through `MatrixRow`. Only a slot needing
            // further decomposition still gets a fresh synthetic column.
            if group.len() == 1 {
                let (args, mut row) = group.pop().unwrap();
                let mut binder_names = Vec::with_capacity(arity);
                let mut direct_names = Vec::new();
                let mut sub_columns = Vec::new();
                let mut sub_patterns = Vec::new();
                for pattern in args {
                    match pattern {
                        MatchPattern::Binder(name) => {
                            let bound = self.pattern_binder_name(name);
                            direct_names.push(bound.clone());
                            binder_names.push(bound);
                        }
                        other => {
                            let synthetic = self.context.fresh_binder();
                            sub_columns.push(curios_core::Term::var(curios_core::Var::free(
                                synthetic.clone(),
                            )));
                            sub_patterns.push(other);
                            binder_names.push(synthetic);
                        }
                    }
                }
                sub_patterns.extend(row.patterns);
                row.patterns = sub_patterns;
                sub_columns.extend(rest.clone());

                let body = self.scoped(direct_names, || {
                    self.compile(sub_columns, vec![row], None, leaf)
                })?;
                cases.push((curios_core::Atom::from(tag.as_str()), binder_names, body));
                continue;
            }

            let synthetic = (0..arity)
                .map(|_| self.context.fresh_binder())
                .collect::<Vec<_>>();
            let sub_rows = group
                .into_iter()
                .map(|(args, mut row)| {
                    let mut patterns = args.iter().collect::<Vec<_>>();
                    patterns.extend(row.patterns);
                    row.patterns = patterns;
                    row
                })
                .collect::<Vec<_>>();
            let mut sub_columns = synthetic
                .iter()
                .map(|name| curios_core::Term::var(curios_core::Var::free(name.clone())))
                .collect::<Vec<_>>();
            sub_columns.extend(rest.clone());

            let body = self.compile(sub_columns, sub_rows, None, leaf)?;
            cases.push((curios_core::Atom::from(tag.as_str()), synthetic, body));
        }

        self.inductive_match(scrutinee, top_motive.unwrap_or(&None), cases)
    }

    /// Groups rows into `Bln`'s two literal shapes and emits
    /// [`curios_core::Term::bln_match`] directly — never `inductive_match`
    /// (`Cases::Bln` is its own hardcoded core node, not a tag dispatch; see
    /// this module's own notes on hardcoded-primitive carriers). `Bln`
    /// carries no payload at all, so — unlike [`Self::compile_ctor`] — there
    /// is no single-row/multi-row naming discipline needed here.
    ///
    /// Unlike a user inductive (whose omitted tags `compile_ctor` defers to
    /// `inductive_match`'s Rung-C vacuity inversion), `Cases::Bln` has no
    /// core-side exhaustiveness escape hatch (`elaborate_bln_match`) — both
    /// groups must be present here, checked eagerly before recursing on
    /// either.
    fn compile_bln(
        &self,
        mut columns: Vec<curios_core::Term>,
        rows: Vec<MatrixRow<'_>>,
        top_motive: Option<&Option<Motive>>,
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Term, Error> {
        let scrutinee = columns.remove(0);
        let rest = columns;

        let mut false_rows = Vec::new();
        let mut true_rows = Vec::new();
        for mut row in rows {
            let MatchPattern::Bln(value) = row.patterns.remove(0) else {
                unreachable!("every row classified as Bln")
            };
            match value {
                false => false_rows.push(row),
                true => true_rows.push(row),
            }
        }

        if false_rows.is_empty() || true_rows.is_empty() {
            return Err(Error::MatrixIncompleteCarrierMatch { carrier: "Bln" });
        }

        let (label, motive_body) = self.motive_parts(top_motive.unwrap_or(&None))?;
        let false_case = self.compile(rest.clone(), false_rows, None, leaf)?;
        let true_case = self.compile(rest, true_rows, None, leaf)?;

        Ok(curios_core::Term::bln_match(
            scrutinee,
            label,
            motive_body,
            false_case,
            true_case,
        ))
    }

    /// Groups rows into `NatZero`/`NatSucc` — the nested-pattern counterpart
    /// of `NatMatch::Induction`'s own `0`/`n+1; ih` arms — and emits
    /// [`curios_core::Term::nat_match`] directly.
    ///
    /// Mirrors [`Self::compile_ctor`]'s single-row/multi-row naming
    /// discipline exactly, for the same reason: `curios-core`'s erasure pass
    /// reads a `Nat` succ arm's stored binder labels as naming hints too
    /// (`erase_nat_match`, the same `Context::fresh` hint-compounding
    /// mechanism `erase_inductive_match` has) — unconditionally minting a
    /// synthetic name here would resurrect the exact regression class
    /// `compile_ctor`'s fast path exists to avoid. A `NatSucc` group of
    /// exactly one row therefore reuses that row's own written
    /// `pred_label`/`ih_label` directly; only a group with more than one row
    /// mints synthetic names.
    ///
    /// `pred`/`ih` are always plain binder names, never a further
    /// sub-pattern (deep peeling stays out of scope), so — unlike a
    /// constructor argument slot — the multi-row case never needs a new
    /// column for them at all: each row's own written name is just bound to
    /// one shared synthetic variable via `row.binds`, exactly like
    /// [`Self::compile`]'s own all-`Binder`-column-retirement path.
    ///
    /// Both groups must be present — `Cases::FreeMonoid` has no vacuity
    /// escape hatch either (same point as [`Self::compile_bln`]'s doc
    /// comment). Checking this eagerly, before recursing on either group,
    /// also avoids indexing into an empty `rows` slice or a misleading
    /// [`Error::MatrixDuplicateRow`] from [`Self::compile`]'s base case.
    fn compile_nat(
        &self,
        mut columns: Vec<curios_core::Term>,
        rows: Vec<MatrixRow<'_>>,
        top_motive: Option<&Option<Motive>>,
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Term, Error> {
        let scrutinee = columns.remove(0);
        let rest = columns;

        let mut zero_rows = Vec::new();
        let mut succ_rows: Vec<(String, String, MatrixRow<'_>)> = Vec::new();
        for mut row in rows {
            match row.patterns.remove(0) {
                MatchPattern::Nat(NatPattern::Zero) => zero_rows.push(row),
                MatchPattern::Nat(NatPattern::Succ {
                    pred_label,
                    ih_label,
                }) => succ_rows.push((pred_label.clone(), ih_label.clone(), row)),
                _ => unreachable!("every row classified as Nat"),
            }
        }

        if zero_rows.is_empty() || succ_rows.is_empty() {
            return Err(Error::MatrixIncompleteCarrierMatch { carrier: "Nat" });
        }

        let (label, motive_body) = self.motive_parts(top_motive.unwrap_or(&None))?;
        let zero_case = self.compile(rest.clone(), zero_rows, None, leaf)?;

        let (pred_label, ih_label, succ_case) = if succ_rows.len() == 1 {
            let (pred_name, ih_name, row) = succ_rows.pop().unwrap();
            let pred_bound = self.pattern_binder_name(&pred_name);
            let ih_bound = self.pattern_binder_name(&ih_name);
            let succ_case = self.scoped([pred_bound.clone(), ih_bound.clone()], || {
                self.compile(rest, vec![row], None, leaf)
            })?;
            (pred_bound, ih_bound, succ_case)
        } else {
            let pred_synth = self.context.fresh_binder();
            let ih_synth = self.context.fresh_binder();
            let sub_rows = succ_rows
                .into_iter()
                .map(|(pred_name, ih_name, mut row)| {
                    row.binds.push((
                        pred_name,
                        curios_core::Term::var(curios_core::Var::free(pred_synth.clone())),
                    ));
                    row.binds.push((
                        ih_name,
                        curios_core::Term::var(curios_core::Var::free(ih_synth.clone())),
                    ));
                    row
                })
                .collect();
            let succ_case = self.compile(rest, sub_rows, None, leaf)?;
            (pred_synth, ih_synth, succ_case)
        };

        Ok(curios_core::Term::nat_match(
            scrutinee,
            label,
            motive_body,
            zero_case,
            pred_label,
            ih_label,
            succ_case,
        ))
    }

    /// Groups rows into `LstNil`/`LstCons` and emits
    /// [`curios_core::Term::lst_match`] directly. Structurally identical to
    /// [`Self::compile_nat`] but with three names (`head`/`tail`/optional
    /// `ih`) instead of two, reusing [`Self::cons_ih_name`] for the
    /// single-row case's `ih` (already handles "written name → bound,
    /// omitted → fresh" correctly). In the multi-row case, a row whose own
    /// `ih_label` was `None` never references any ih name, so no bind is
    /// pushed for it — only rows that wrote `; ih` get one, sharing the one
    /// synthetic `ih` variable the emitted core node itself always needs.
    fn compile_lst(
        &self,
        mut columns: Vec<curios_core::Term>,
        rows: Vec<MatrixRow<'_>>,
        top_motive: Option<&Option<Motive>>,
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Term, Error> {
        let scrutinee = columns.remove(0);
        let rest = columns;

        let mut nil_rows = Vec::new();
        let mut cons_rows: Vec<(String, String, Option<String>, MatrixRow<'_>)> = Vec::new();
        for mut row in rows {
            match row.patterns.remove(0) {
                MatchPattern::Lst(LstPattern::Nil) => nil_rows.push(row),
                MatchPattern::Lst(LstPattern::Cons {
                    head_label,
                    tail_label,
                    ih_label,
                }) => cons_rows.push((
                    head_label.clone(),
                    tail_label.clone(),
                    ih_label.clone(),
                    row,
                )),
                _ => unreachable!("every row classified as Lst"),
            }
        }

        if nil_rows.is_empty() || cons_rows.is_empty() {
            return Err(Error::MatrixIncompleteCarrierMatch { carrier: "Lst" });
        }

        let (label, motive_body) = self.motive_parts(top_motive.unwrap_or(&None))?;
        let empty_case = self.compile(rest.clone(), nil_rows, None, leaf)?;

        let (head_label, tail_label, ih_label, cons_case) = if cons_rows.len() == 1 {
            let (head_name, tail_name, ih_name, row) = cons_rows.pop().unwrap();
            let head_bound = self.pattern_binder_name(&head_name);
            let tail_bound = self.pattern_binder_name(&tail_name);
            let ih_bound = self.cons_ih_name(&ih_name);
            let cons_case = self.scoped(
                [head_bound.clone(), tail_bound.clone(), ih_bound.clone()],
                || self.compile(rest, vec![row], None, leaf),
            )?;
            (head_bound, tail_bound, ih_bound, cons_case)
        } else {
            let head_synth = self.context.fresh_binder();
            let tail_synth = self.context.fresh_binder();
            let ih_synth = self.context.fresh_binder();
            let sub_rows = cons_rows
                .into_iter()
                .map(|(head_name, tail_name, ih_name, mut row)| {
                    row.binds.push((
                        head_name,
                        curios_core::Term::var(curios_core::Var::free(head_synth.clone())),
                    ));
                    row.binds.push((
                        tail_name,
                        curios_core::Term::var(curios_core::Var::free(tail_synth.clone())),
                    ));
                    if let Some(ih_name) = ih_name {
                        row.binds.push((
                            ih_name,
                            curios_core::Term::var(curios_core::Var::free(ih_synth.clone())),
                        ));
                    }
                    row
                })
                .collect();
            let cons_case = self.compile(rest, sub_rows, None, leaf)?;
            (head_synth, tail_synth, ih_synth, cons_case)
        };

        Ok(curios_core::Term::lst_match(
            scrutinee,
            curios_core::Term::metavar(self.context.fresh_metavar()),
            label,
            motive_body,
            empty_case,
            head_label,
            tail_label,
            ih_label,
            cons_case,
        ))
    }

    /// Groups rows into `BinEnd`/`BinByte` and emits
    /// [`curios_core::Term::bin_match`] directly — identical to
    /// [`Self::compile_lst`] minus the `elem` metavar argument `Lst` needs
    /// for its polymorphic element type (`Bin` has none).
    fn compile_bin(
        &self,
        mut columns: Vec<curios_core::Term>,
        rows: Vec<MatrixRow<'_>>,
        top_motive: Option<&Option<Motive>>,
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Term, Error> {
        let scrutinee = columns.remove(0);
        let rest = columns;

        let mut end_rows = Vec::new();
        let mut byte_rows: Vec<(String, String, Option<String>, MatrixRow<'_>)> = Vec::new();
        for mut row in rows {
            match row.patterns.remove(0) {
                MatchPattern::Bin(BinPattern::End) => end_rows.push(row),
                MatchPattern::Bin(BinPattern::Byte {
                    head_label,
                    tail_label,
                    ih_label,
                }) => byte_rows.push((
                    head_label.clone(),
                    tail_label.clone(),
                    ih_label.clone(),
                    row,
                )),
                _ => unreachable!("every row classified as Bin"),
            }
        }

        if end_rows.is_empty() || byte_rows.is_empty() {
            return Err(Error::MatrixIncompleteCarrierMatch { carrier: "Bin" });
        }

        let (label, motive_body) = self.motive_parts(top_motive.unwrap_or(&None))?;
        let empty_case = self.compile(rest.clone(), end_rows, None, leaf)?;

        let (head_label, tail_label, ih_label, cons_case) = if byte_rows.len() == 1 {
            let (head_name, tail_name, ih_name, row) = byte_rows.pop().unwrap();
            let head_bound = self.pattern_binder_name(&head_name);
            let tail_bound = self.pattern_binder_name(&tail_name);
            let ih_bound = self.cons_ih_name(&ih_name);
            let cons_case = self.scoped(
                [head_bound.clone(), tail_bound.clone(), ih_bound.clone()],
                || self.compile(rest, vec![row], None, leaf),
            )?;
            (head_bound, tail_bound, ih_bound, cons_case)
        } else {
            let head_synth = self.context.fresh_binder();
            let tail_synth = self.context.fresh_binder();
            let ih_synth = self.context.fresh_binder();
            let sub_rows = byte_rows
                .into_iter()
                .map(|(head_name, tail_name, ih_name, mut row)| {
                    row.binds.push((
                        head_name,
                        curios_core::Term::var(curios_core::Var::free(head_synth.clone())),
                    ));
                    row.binds.push((
                        tail_name,
                        curios_core::Term::var(curios_core::Var::free(tail_synth.clone())),
                    ));
                    if let Some(ih_name) = ih_name {
                        row.binds.push((
                            ih_name,
                            curios_core::Term::var(curios_core::Var::free(ih_synth.clone())),
                        ));
                    }
                    row
                })
                .collect();
            let cons_case = self.compile(rest, sub_rows, None, leaf)?;
            (head_synth, tail_synth, ih_synth, cons_case)
        };

        Ok(curios_core::Term::bin_match(
            scrutinee,
            label,
            motive_body,
            empty_case,
            head_label,
            tail_label,
            ih_label,
            cons_case,
        ))
    }

    /// Explodes a `Tuple`/`Struct` column into one new leftmost column per
    /// field, via [`curios_core::Term::proj`]/[`curios_core::Term::proj_label`]
    /// on the current (always already-bound) scrutinee variable — this is
    /// the same code path whether the exploded column is the outer head or
    /// several levels deep, and it never needs a core `Match` node at all: a
    /// tuple/struct value has exactly one shape, so "matching" one is just
    /// sequential projection, exactly like a hand-written `p.0`/`p.label`.
    /// Struct privacy is inherited automatically and unmodified, since
    /// `proj_label` is the same function `elaborate_proj` already checks it
    /// against. Every row's field list was already validated (by
    /// [`Self::compile`]) to share this column's arity/head; here they're
    /// further checked to agree, position by position, on whether each field
    /// is labeled — an irrefutable `Pattern` never needed this, since a
    /// `let`/lambda site only ever destructures a value once.
    fn compile_fields(
        &self,
        mut columns: Vec<curios_core::Term>,
        rows: Vec<MatrixRow<'_>>,
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Term, Error> {
        let scrutinee = columns.remove(0);
        let rest = columns;

        let canonical_labels = match rows[0].patterns[0] {
            MatchPattern::Tuple(fields) | MatchPattern::Struct { fields, .. } => fields
                .iter()
                .map(|f| f.label.as_deref())
                .collect::<Vec<_>>(),
            _ => unreachable!("every row classified as Tuple/Struct"),
        };

        let mut new_rows = Vec::with_capacity(rows.len());
        for mut row in rows {
            let fields = match row.patterns.remove(0) {
                MatchPattern::Tuple(fields) | MatchPattern::Struct { fields, .. } => fields,
                _ => unreachable!("every row classified as Tuple/Struct"),
            };
            let labels = fields
                .iter()
                .map(|f| f.label.as_deref())
                .collect::<Vec<_>>();
            if labels != canonical_labels {
                return Err(Error::MatrixInconsistentShape);
            }
            let mut patterns = fields.iter().map(|f| &f.value).collect::<Vec<_>>();
            patterns.extend(row.patterns);
            row.patterns = patterns;
            new_rows.push(row);
        }

        let mut new_columns = canonical_labels
            .into_iter()
            .enumerate()
            .map(|(index, label)| match label {
                Some(label) => curios_core::Term::proj_label(scrutinee.clone(), label.to_string()),
                None => curios_core::Term::proj(scrutinee.clone(), index),
            })
            .collect::<Vec<_>>();
        new_columns.extend(rest);

        self.compile(new_columns, new_rows, None, leaf)
    }

    /// The leaf of the matrix compiler's recursion: exactly one row remains
    /// once every column is consumed. Lowers the row's body under every
    /// accumulated binder name (so a reference resolves to the binder rather
    /// than a like-named module binding, exactly like [`Self::scoped`]'s
    /// other callers), then wraps it in the accumulated `let`s, outermost
    /// first.
    fn finish_row(
        &self,
        row: MatrixRow<'_>,
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Term, Error> {
        let names = row.binds.iter().map(|(name, _)| name.clone());
        let body = self.scoped(names, || leaf(self, row.body))?;
        Ok(row
            .binds
            .into_iter()
            .rev()
            .fold(body, |tail, (name, value)| {
                let hole = curios_core::Term::metavar(self.context.fresh_metavar());
                curios_core::Term::let_(self.pattern_binder_name(&name), hole, value, tail)
            }))
    }

    /// Lowers a list literal's entries. A spread-free literal lowers to a
    /// plain `Lst` — exactly the pre-spread lowering, `[]` included. With
    /// spreads, consecutive elements group into `Lst` literal chunks and the
    /// whole literal becomes an n-ary `LstConcat`; its element-type slot is a
    /// fresh metavar (an implicit the literal cannot name), solved by
    /// elaboration — bidirectionally from the expected type when checking
    /// (see the `LstConcat` case in `curios_core`'s `elaborate_prim`).
    /// `lower` is the per-term lowering — [`Self::term`] on the plain path,
    /// the bang-collector on the region path — so both share this grouping.
    fn lower_lst_literal(
        &self,
        entries: &[LstEntry],
        mut lower: impl FnMut(&Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Prim, Error> {
        let mut operands = Vec::new();
        let mut run = Vec::new();

        for entry in entries {
            match entry {
                LstEntry::Elem(term) => run.push(lower(term)?),
                LstEntry::Spread(term) => {
                    if !run.is_empty() {
                        operands.push(curios_core::Term::prim(curios_core::Prim::Lst(
                            std::mem::take(&mut run),
                        )));
                    }

                    operands.push(lower(term)?);
                }
            }
        }

        if operands.is_empty() {
            return Ok(curios_core::Prim::Lst(run));
        }

        if !run.is_empty() {
            operands.push(curios_core::Term::prim(curios_core::Prim::Lst(run)));
        }

        Ok(curios_core::Prim::LstConcat(
            curios_core::Term::metavar(self.context.fresh_metavar()),
            operands,
        ))
    }

    /// The `Bin` sibling of [`Self::lower_lst_literal`]: a spread-free literal
    /// lowers to a plain `Bin`, and spreads splice their byte runs into an
    /// n-ary `BinConcat` (no element-type slot — the bytes are `Bin`'s own).
    fn lower_bin_literal(
        &self,
        segments: &[BinSegment],
        mut lower: impl FnMut(&Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Prim, Error> {
        if segments
            .iter()
            .all(|segment| matches!(segment, BinSegment::Bytes(_)))
        {
            // Zero or one run in practice (the parser coalesces); flattening
            // keeps this robust for hand-built literals too.
            let bytes = segments
                .iter()
                .flat_map(|segment| match segment {
                    BinSegment::Bytes(run) => run.iter().copied(),
                    BinSegment::Spread(_) => unreachable!("all segments are byte runs"),
                })
                .collect();

            return Ok(curios_core::Prim::Bin(bytes));
        }

        let operands = segments
            .iter()
            .map(|segment| match segment {
                BinSegment::Bytes(run) => {
                    Ok(curios_core::Term::prim(curios_core::Prim::Bin(run.clone())))
                }
                BinSegment::Spread(term) => lower(term),
            })
            .collect::<Result<Vec<_>, Error>>()?;

        Ok(curios_core::Prim::BinConcat(operands))
    }

    pub(super) fn prim(&self, prim: &Prim) -> Result<curios_core::Prim, Error> {
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
            // `\hex` is a raw byte sequence; `\..` segments splice other `Bin`s.
            Prim::Bin(segments) => self.lower_bin_literal(segments, |term| self.term(term))?,
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
            Prim::LstType(inner) => curios_core::Prim::lst_type(self.term(inner)?),
            Prim::Lst(entries) => self.lower_lst_literal(entries, |term| self.term(term))?,
            Prim::LstLen(ty, inner) => {
                curios_core::Prim::lst_len(self.term(ty)?, self.term(inner)?)
            }
            Prim::LstGet(ty, list, index) => {
                curios_core::Prim::lst_get(self.term(ty)?, self.term(list)?, self.term(index)?)
            }
            Prim::LstSlice(ty, list, start, end) => curios_core::Prim::lst_slice(
                self.term(ty)?,
                self.term(list)?,
                self.term(start)?,
                self.term(end)?,
            ),
            Prim::LstAppend(ty, list, elem) => {
                curios_core::Prim::lst_append(self.term(ty)?, self.term(list)?, self.term(elem)?)
            }
            Prim::LstConcat(ty, left, right) => {
                curios_core::Prim::lst_concat(self.term(ty)?, [self.term(left)?, self.term(right)?])
            }
            Prim::LstMap(a, b, f, lst) => curios_core::Prim::lst_map(
                self.term(a)?,
                self.term(b)?,
                self.term(f)?,
                self.term(lst)?,
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
