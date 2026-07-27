use {
    super::{Context, MatchCompiler},
    crate::{
        BinSegment, Choose, ChooseTest, Error, Field, FuncParam, Let, LetBinding, LstEntry, Name,
        Nat, NatLiteral, Pattern, PatternField, Prim, Rec, StructLitEntry, Subterm, Syn, Term,
    },
    curios_base::{Grain, PackedBin, Plicity},
    curios_core::{Free, UniverseRole},
    num_bigint::BigUint,
    std::{cell::RefCell, sync::Arc},
};

/// A lowered function's binders: `(plicity, core binder, domain)` per slot,
/// paralleling the surface parameter list.
type LoweredParams = Vec<(Plicity, Free, curios_core::Term)>;

/// A source binder brought into lexical scope: what it was written as, and the
/// identity every reference to it lowers to.
///
/// Minted once, where the binder is introduced, and reused everywhere that
/// binder is in scope — a progressively-extended parameter list re-enters the
/// same identities rather than re-minting them, or an earlier domain and the
/// body would disagree about which binder a name meant.
pub(super) type Bound = (String, Free);

pub(super) struct Lowerer<'a, 'b> {
    pub(super) context: &'a Context<'b>,
    /// The enclosing local binders (function and `let`/`rec` binders, match-arm
    /// patterns, motive labels), innermost last. A bare reference whose spelling
    /// appears here resolves to the innermost such binder rather than a
    /// like-named module binding — see [`Self::resolve_name`]. Compiler-minted
    /// binders are never registered: nothing can write their name.
    scope: RefCell<Vec<Bound>>,
}

impl<'a, 'b> Lowerer<'a, 'b> {
    pub(super) fn new(context: &'a Context<'b>) -> Self {
        Self {
            context,
            scope: RefCell::new(Vec::new()),
        }
    }

    /// Mint one binder identity per written name, in order. Nothing is brought
    /// into scope: the caller decides where each binder is visible, and the
    /// identity it holds is the one every such region re-enters.
    ///
    /// An unwritten (`_` or empty) name still gets an identity — it occupies a
    /// binder position — but no reference can reach it.
    pub(super) fn mint(&self, names: impl IntoIterator<Item = String>) -> Vec<Bound> {
        names
            .into_iter()
            .map(|name| {
                let id = self.context.fresh_binder(bindable(&name).then_some(&name));
                (name, id)
            })
            .collect()
    }

    /// Lower `body` with already-minted `binders` in scope, then restore the
    /// previous scope.
    pub(super) fn bound<T>(
        &self,
        binders: &[Bound],
        body: impl FnOnce() -> Result<T, Error>,
    ) -> Result<T, Error> {
        let mark = self.enter_scope(binders);
        let result = body();
        self.leave_scope(mark);
        result
    }

    /// The two steps [`Self::bound`] runs around one bounded closure call,
    /// exposed separately for a walk that must hold several nested scopes open
    /// across a loop instead — see [`Self::lower_let_region`], which enters one
    /// scope per binding of a `let` block and leaves them in reverse. The scope
    /// is a stack, so a shadowing inner binder simply sits above the outer one
    /// and [`Self::resolve_name`]'s innermost-first scan finds it.
    fn enter_scope(&self, binders: &[Bound]) -> usize {
        let mut scope = self.scope.borrow_mut();
        let mark = scope.len();
        scope.extend(binders.iter().filter(|(name, _)| bindable(name)).cloned());
        mark
    }

    fn leave_scope(&self, mark: usize) {
        self.scope.borrow_mut().truncate(mark);
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
                .subterm(term.as_subterm(), Some(s))
                .map_err(|error| error.at(s.clone()))?,
            None => self.subterm(term.as_subterm(), None)?,
        };
        Ok(match span {
            Some(s) => curios_core::Term::spanned(s, elaborated),
            None => elaborated,
        })
    }

    /// Lower a type in an input position. The role is lexical, so every
    /// written `Type` inside a nested higher-kinded domain remains eligible
    /// for declaration generalization.
    pub(super) fn input_type(&self, term: &Term) -> Result<curios_core::Term, Error> {
        self.context
            .with_universe_role(UniverseRole::Generalizable, || self.term(term))
    }

    /// Resolve a surface name to its qualified (joined) core name — the same
    /// rule the `Subterm::Name` term-reference arm uses.
    pub(super) fn resolve_name(&self, name: &Name) -> Result<Free, Error> {
        if name.is_abs() || !name.is_single() {
            return Ok(Free::global(self.context.resolve_term_name(name)?));
        }
        // A local binder shadows any like-named module binding, and the
        // innermost one wins. Resolving here — rather than emitting the
        // spelling for a later stage to re-resolve — is what makes shadowing
        // exact: two binders written `go` are two identities from the start.
        if let Some((_, id)) = self
            .scope
            .borrow()
            .iter()
            .rev()
            .find(|(bound, _)| bound == name.head())
        {
            return Ok(id.clone());
        }
        match self.context.bindings().get(name.head()) {
            Some(full) => Ok(Free::global(full.clone())),
            // Unresolved, and `curios-core` is what reports it — so this must
            // lower to something no definition can ever be. A binder identity
            // is unbound by construction (nothing closes over it) and carries
            // the written name as its hint, so the diagnostic still names it.
            //
            // A root-level global would *not* do: `Qualifier::from([head])` is
            // exactly what an entry-module `let helper` lowers to, so an
            // unresolvable reference in a nested module would silently capture
            // it. The old spelling-keyed lowering was safe only by accident —
            // it emitted a bare `helper` while every definition carried a
            // leading `/`.
            None => Ok(self.context.fresh_binder(Some(name.head()))),
        }
    }

    // The meta-emitter: a string literal becomes a proof-carrying `/syn/Str/Str`
    // value `Str { bytes = <Bytes>, valid = <Utf8 derivation> }`. The derivation is the
    // canonical `more`-spine (one `more` per byte, ending in `stop`), starting from
    // the `lead` state — `valid`'s type is `Valid(b) = Utf8(lead, b)`. `valid` is
    // erased, so at runtime `Str` collapses to its `Bytes` field — a literal costs
    // exactly what a `Bytes` literal does.
    pub(super) fn str_literal(&self, bytes: &[u8]) -> curios_core::Term {
        curios_core::Term::struct_(
            curios_core::Global::Authored(self.context.syntax().string().string().qualifier()),
            Vec::<curios_core::Term>::new(),
            [
                curios_core::Term::prim(curios_core::Prim::Bin(
                    Grain::X,
                    PackedBin::from_bytes(bytes.to_vec()),
                )),
                self.utf8_derivation(bytes, self.scan_lead()),
            ],
        )
    }

    /// A Rust `char` is already a Unicode scalar, so the literal meta-emitter selects the corresponding `/syn/Char/Scalar` range constructor and supplies a closed reflected proof. The proof erases and the single relevant `code` field collapses to the ordinary Nat carrier.
    pub(super) fn char_literal(&self, character: char) -> curios_core::Term {
        let code = curios_core::Term::prim(curios_core::Prim::Nat(curios_core::Nat::Succ(
            BigUint::from(character as u32),
            curios_core::Term::prim(curios_core::Prim::Nat(curios_core::Nat::Zero)),
        )));
        let constructor = if (character as u32) < 0xD800 {
            self.context.syntax().character().scalar_below()
        } else {
            self.context.syntax().character().scalar_above()
        };
        let scalar = curios_core::Term::apply_marked(
            curios_core::Term::var(curios_core::Var::free(Free::global(
                constructor.qualifier(),
            ))),
            [
                (Plicity::Implicit, code.clone()),
                (
                    Plicity::Explicit,
                    Self::syn_call(self.context.syntax().proof().true_qed(), []),
                ),
            ],
        );

        curios_core::Term::struct_(
            curios_core::Global::Authored(
                self.context.syntax().character().character().qualifier(),
            ),
            Vec::<curios_core::Term>::new(),
            [code, scalar],
        )
    }

    // A constructor/function `Var` applied to `args` — the absolute core name as the
    // parser would resolve it (privacy is a surface-resolution concern; these are
    // already-resolved core `Var`s, so referencing a private `/syn` helper is fine).
    pub(super) fn syn_call(
        name: crate::SyntaxName,
        args: impl IntoIterator<Item = curios_core::Term>,
    ) -> curios_core::Term {
        curios_core::Term::apply(
            curios_core::Term::var(curios_core::Var::free(Free::global(name.qualifier()))),
            args.into_iter().collect::<Vec<_>>(),
        )
    }

    pub(super) fn scan_lead(&self) -> curios_core::Term {
        Self::syn_call(self.context.syntax().string().scan_lead(), [])
    }

    // The `Utf8(state, bytes)` derivation. `state` is carried as a *symbolic* term —
    // `lead()` at the top, then `step(c, state)` per byte — so each recursive `rest`'s
    // expected index (`Utf8(step(c, state), tail)`) is definitionally the state we
    // thread in, with no metavar/`step`-inversion. The final `stop : Utf8(lead, \\)`
    // matches because `step` of the last byte reduces back to `lead` for valid UTF-8
    // (a string literal is valid UTF-8 by construction).
    pub(super) fn utf8_derivation(
        &self,
        bytes: &[u8],
        state: curios_core::Term,
    ) -> curios_core::Term {
        match bytes.split_first() {
            None => Self::syn_call(self.context.syntax().string().utf8_stop(), []),
            Some((&head, tail)) => {
                let byte: curios_core::Term =
                    curios_core::Term::prim(curios_core::Prim::Byte(head));
                let next = Self::syn_call(
                    self.context.syntax().string().step(),
                    [byte.clone(), state.clone()],
                );
                Self::syn_call(
                    self.context.syntax().string().utf8_more(),
                    [
                        byte,
                        state,
                        curios_core::Term::prim(curios_core::Prim::Bin(
                            Grain::X,
                            PackedBin::from_bytes(tail.to_vec()),
                        )),
                        self.utf8_derivation(tail, next),
                    ],
                )
            }
        }
    }

    // A `/syn` literal — its value is synthesized from `/syn` by the meta-emitter
    // rather than lowered to a core primitive.
    pub(super) fn syn_literal(&self, syn: &Syn) -> Result<curios_core::Term, Error> {
        match syn {
            Syn::Char(character) => Ok(self.char_literal(*character)),
            Syn::Str(string) => Ok(self.str_literal(string.as_bytes())),
        }
    }

    pub(super) fn subterm(
        &self,
        term: &Subterm,
        span: Option<&curios_base::Span>,
    ) -> Result<curios_core::Term, Error> {
        Ok(match term {
            Subterm::Type => curios_core::Term::type_at(self.context.fresh_universe(span)),
            Subterm::Prop => curios_core::Term::prop(),
            Subterm::Hole => curios_core::Term::metavar(self.context.fresh_metavar()),
            // A written goal `?`: same fresh metavariable, but marked so zonk
            // reports what elaboration determined for it instead of splicing.
            Subterm::Goal => curios_core::Term::goal(self.context.fresh_metavar()),
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
                let binders = self.mint(
                    ft.params
                        .iter()
                        .map(|p| p.label.clone().unwrap_or_default()),
                );
                let mut params = Vec::with_capacity(ft.params.len());
                for (index, param) in ft.params.iter().enumerate() {
                    let domain = self.bound(&binders[..index], || self.input_type(&param.type_))?;
                    params.push((param.plicity, binders[index].1.clone(), domain));
                }
                let output = self.bound(&binders, || self.term(&ft.output))?;
                curios_core::Term::func_type_marked(params, output)
            }
            Subterm::Func(func) => {
                let binders = self.mint(param_names(&func.params));
                let body = self.bound(&binders, || self.term(&func.body))?;
                let (params, body) = self.lower_func_params(&func.params, &binders, body)?;
                curios_core::Term::func_marked(params, body)
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
                let binders = self.mint(
                    tt.fields
                        .iter()
                        .map(|f| f.label.clone().unwrap_or_default()),
                );
                let mut fields = Vec::with_capacity(tt.fields.len());
                for (index, param) in tt.fields.iter().enumerate() {
                    let type_ = param.desugared_type();
                    let lowered = self.bound(&binders[..index], || self.term(&type_))?;
                    fields.push((binders[index].1.clone(), lowered));
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
                self.resolve_nominal(&lit.head)?,
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
            // A `choose` right-folds into nested `Bool` matches: each
            // `cond => body` becomes `match cond | false => <rest> | true =>
            // body end`, the `_` default sitting at the innermost false
            // branch. No motive at any level (a fresh hole each), matching
            // the surface form's absence of one. Arms inherit the
            // definitional refinement of their conditions for free — that is
            // exactly what nesting `Bool` matches buys.
            Subterm::Choose(Choose { arms, default }) => {
                let mut acc = self.term(default)?;
                for arm in arms.iter().rev() {
                    acc = match &arm.test {
                        ChooseTest::Cond(condition) => curios_core::Term::bool_match(
                            self.term(condition)?,
                            None,
                            curios_core::Term::metavar(self.context.fresh_metavar()),
                            acc,
                            self.term(&arm.body)?,
                        ),
                        ChooseTest::Bind { pattern, value } => {
                            let value = self.term(value)?;
                            MatchCompiler::new(self).lower_bind_arm(
                                pattern,
                                value,
                                &arm.body,
                                acc,
                                MatchCompiler::term,
                            )?
                        }
                    };
                }
                acc
            }
            Subterm::Match(match_) => {
                // The matrix compiler recursively decomposes (possibly
                // nested, across constructors/tuples/structs) arm patterns
                // into single-level core `Match`/projection forms — see
                // `MatchCompiler::compile_matrix`. A final `| _ =>` catch-all
                // is split off as the dispatch default.
                let head = self.term(&match_.head)?;
                MatchCompiler::new(self).compile_matrix_headed(
                    head,
                    &match_.motive,
                    &match_.arms,
                    MatchCompiler::term,
                )?
            }
            // A `let` block is non-recursive: each binding is in scope for the
            // bindings after it and the tail, never for its own type or value.
            // Lower each binding's type/value in the scope of the bindings
            // before it, then wrap them back (reverse) around the tail.
            Subterm::Let(let_) => {
                let mut pending = Vec::new();

                for binding in &let_.bindings {
                    let type_ = self.term(&binding.signature.type_())?;
                    let value = self.term(&binding.signature.body())?;
                    let binders = self.mint(pattern_names(&binding.binder));
                    let mark = self.enter_scope(&binders);
                    pending.push((mark, binders, &binding.binder, type_, value));
                }

                let mut tail = self.term(&let_.tail)?;

                for (mark, binders, binder, type_, value) in pending.into_iter().rev() {
                    self.leave_scope(mark);
                    tail = self.bind_pattern(binder, &binders, type_, value, tail);
                }

                tail
            }
            // A `rec` is mutually recursive: every item label is in scope across
            // all item types, all item bodies, and the tail.
            Subterm::Rec(rec) => {
                let binders = self.mint(rec.items.iter().map(|it| it.label.clone()));
                self.bound(&binders, || {
                    Ok(curios_core::Term::rec(
                        rec.items
                            .iter()
                            .zip(&binders)
                            .map(|(it, (_, id))| {
                                Ok((
                                    id.clone(),
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
    pub(super) fn region(&self, term: &Term) -> Result<curios_core::Term, Error> {
        match term.as_subterm() {
            // A `let`'s bound expression evaluates in place (its bangs hoist to
            // this region); the tail continues the same region (a bang there
            // hoists after `x` is bound, not above the `let`).
            Subterm::Let(let_) => self.lower_let_region(&let_.bindings, &let_.tail),
            // The scrutinee evaluates before branching (its bangs hoist here);
            // each arm is its own region (branch-local effects).
            Subterm::Match(match_) => {
                let mut binds = Vec::new();
                let match_term = MatchCompiler::new(self).match_region(match_, &mut binds)?;
                self.wrap(binds, match_term)
            }
            // The `choose` head arm's test runs unconditionally (its bangs
            // hoist here); deeper arms and the default are branch-local.
            Subterm::Choose(choose) => {
                let mut binds = Vec::new();
                let choose_term = MatchCompiler::new(self).choose_region(choose, &mut binds)?;
                self.wrap(binds, choose_term)
            }
            // A lambda re-roots the region.
            Subterm::Func(func) => {
                let binders = self.mint(param_names(&func.params));
                let body = self.bound(&binders, || self.region(&func.body))?;
                let (params, body) = self.lower_func_params(&func.params, &binders, body)?;
                Ok(curios_core::Term::func_marked(params, body))
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

    /// Lowers a run of `let` `bindings` and its `tail` as one region: each
    /// binding's bound expression is a value in this region (its bangs hoist
    /// here, sequenced by `wrap`), the tail continues the same region. Loops
    /// over `bindings` rather than recursing once per binding — a `let` block is
    /// flat, so a long straight-line sequence costs one loop, not a stack of
    /// native frames. Shared by `region`'s `Let` arm (the whole block) and
    /// `build_let` (the bindings after the first, whose own bangs hoist to the
    /// enclosing region instead).
    pub(super) fn lower_let_region(
        &self,
        bindings: &[LetBinding],
        tail: &Term,
    ) -> Result<curios_core::Term, Error> {
        struct PendingLet<'t> {
            mark: usize,
            binders: Vec<Bound>,
            binds: Vec<(Free, curios_core::Term)>,
            binder: &'t Pattern,
            type_: curios_core::Term,
            value: curios_core::Term,
        }

        let mut pending = Vec::new();

        for binding in bindings {
            let mut binds = Vec::new();
            let value = self.collect(&binding.signature.body(), &mut binds)?;
            let type_ = self.term(&binding.signature.type_())?;
            let binders = self.mint(pattern_names(&binding.binder));
            let mark = self.enter_scope(&binders);

            pending.push(PendingLet {
                mark,
                binders,
                binds,
                binder: &binding.binder,
                type_,
                value,
            });
        }

        let mut tail = self.region(tail)?;

        for pending_let in pending.into_iter().rev() {
            self.leave_scope(pending_let.mark);
            let let_term = self.bind_pattern(
                pending_let.binder,
                &pending_let.binders,
                pending_let.type_,
                pending_let.value,
                tail,
            );
            tail = self.wrap(pending_let.binds, let_term)?;
        }

        Ok(tail)
    }

    /// Builds the core `rec` for a `rec` inside a value body: item types are
    /// types, item bodies are fresh region roots, and the tail continues as its
    /// own region (a bang there hoists after the bindings, not above them).
    pub(super) fn build_rec(&self, rec: &Rec) -> Result<curios_core::Term, Error> {
        let binders = self.mint(rec.items.iter().map(|it| it.label.clone()));
        self.bound(&binders, || {
            Ok(curios_core::Term::rec(
                rec.items
                    .iter()
                    .zip(&binders)
                    .map(|(it, (_, id))| {
                        Ok((
                            id.clone(),
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
    pub(super) fn collect(
        &self,
        term: &Term,
        binds: &mut Vec<(Free, curios_core::Term)>,
    ) -> Result<curios_core::Term, Error> {
        Ok(match term.as_subterm() {
            Subterm::Bang(action) => {
                // The action is itself desugared first, so its inner bangs
                // evaluate before this one (left-to-right).
                let action = self.collect(action, binds)?;
                let binder = self.context.fresh_binder(None);
                let var = curios_core::Term::var(curios_core::Var::free(binder.clone()));
                binds.push((binder, action));
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
                self.resolve_nominal(&lit.head)?,
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
            // An `Lst` literal's elements and spread operands hoist their
            // bangs into this region, like an application's arguments.
            Subterm::Prim(Prim::Lst(entries)) => curios_core::Term::prim(
                self.lower_lst_literal(entries, |term| self.collect(term, binds))?,
            ),
            // A `Bits`/`Bytes` literal's spread operands hoist likewise (a
            // spread-free literal has no subterms and lowers unchanged).
            Subterm::Prim(Prim::Bin(grain, segments)) => {
                curios_core::Term::prim(Self::lower_bin_literal(*grain, segments, |term| {
                    self.collect(term, binds)
                })?)
            }
            // A `let`/`match`/`choose` sub-expression hoists its
            // bound-expression / scrutinee / head-test bangs into the
            // enclosing region (this `binds`).
            Subterm::Let(let_) => self.build_let(let_, binds)?,
            Subterm::Match(match_) => MatchCompiler::new(self).match_region(match_, binds)?,
            Subterm::Choose(choose) => MatchCompiler::new(self).choose_region(choose, binds)?,
            // A lambda is a value and a `rec` binds recursively: neither hoists
            // anything outward, so desugar each as its own region.
            Subterm::Func(_) | Subterm::Rec(_) => self.region(term)?,
            // Leaves elaborate normally. A `Bang` reachable here (e.g. nested in a
            // type position) hits `self.term`'s `Bang` arm and is rejected.
            _ => self.term(term)?,
        })
    }

    /// Builds a `let` block reached inside a `collect` (spine) context. The
    /// *first* binding's bangs hoist to the enclosing region (`binds`); the
    /// bindings after it and the tail form their own region via
    /// `lower_let_region`, scoped under the first binder.
    pub(super) fn build_let(
        &self,
        let_: &Let,
        binds: &mut Vec<(Free, curios_core::Term)>,
    ) -> Result<curios_core::Term, Error> {
        let (first, rest) = let_
            .bindings
            .split_first()
            .expect("a `let` block has at least one binding");

        let value = self.collect(&first.signature.body(), binds)?;
        let binders = self.mint(pattern_names(&first.binder));
        let tail = self.bound(&binders, || self.lower_let_region(rest, &let_.tail))?;
        let type_ = self.term(&first.signature.type_())?;
        Ok(self.bind_pattern(&first.binder, &binders, type_, value, tail))
    }

    /// Lowers a function's parameters into core binder `(name, domain)` pairs.
    /// A plain-name parameter binds its name directly, unchanged; an
    /// un-annotated parameter takes a fresh metavar domain. A compound
    /// pattern's core binder is a fresh synthetic name, and the (already
    /// lowered) `body` is wrapped with its field-`let` chain.
    ///
    /// Each annotation sees the *preceding* parameters' binders, exactly as a
    /// dependent Π-type's domains do (the `Subterm::FuncType` arm), so a lambda
    /// may be written `(s, t, q : Eq(s, t)) => …`. That is why the walk runs in
    /// declaration order under a progressively-extended scope: `Telescope::build`
    /// captures each earlier binder in every later domain, so the core side
    /// needs nothing further. A compound pattern binds no leaf name at the core
    /// binder — its leaves are projections off the synthetic binder — so a later
    /// annotation naming one of those leaves gets that pattern's field-`let`
    /// chain wrapped around the *domain* as well, mirroring what the body gets.
    ///
    /// The chains wrap body and domains alike in reverse, so each pattern's
    /// chain wraps *before* an earlier pattern's chain wraps that, giving the
    /// declaration-order nesting the spec's motivating example expects.
    pub(super) fn lower_func_params(
        &self,
        params: &[FuncParam],
        binders: &[Bound],
        body: curios_core::Term,
    ) -> Result<(LoweredParams, curios_core::Term), Error> {
        let mut lowered = Vec::with_capacity(params.len());
        // The binders already minted for the leaves, consumed in the same
        // pre-order `param_names` produced them, plus the field-`let` chains
        // that put the compound patterns in scope — both advance with the walk.
        let mut seen = 0;
        let mut chains: Vec<(&[PatternField], Free, &[Bound])> = Vec::new();

        for param in params {
            let FuncParam {
                plicity,
                pattern,
                annotation,
            } = param;
            let domain = match annotation {
                Some(annotation) => {
                    let annotation =
                        self.bound(&binders[..seen], || self.input_type(annotation))?;
                    self.wrap_pattern_chains(&chains, annotation)
                }
                None => curios_core::Term::metavar(self.context.fresh_metavar()),
            };
            // The mark applies to the outer function slot the parameter
            // occupies, whatever the pattern shape: a compound pattern's fresh
            // core binder still claims a slot of the written plicity.
            let leaves = &binders[seen..seen + pattern_names(pattern).len()];
            match pattern {
                Pattern::Binder(Some(_)) => lowered.push((*plicity, leaves[0].1.clone(), domain)),
                Pattern::Binder(None) => {
                    lowered.push((*plicity, self.context.fresh_binder(None), domain))
                }
                Pattern::Tuple(fields) | Pattern::Struct { fields, .. } => {
                    let synthetic = self.context.fresh_binder(None);
                    chains.push((fields, synthetic.clone(), leaves));
                    lowered.push((*plicity, synthetic, domain));
                }
            }
            seen += leaves.len();
        }

        let body = chains
            .iter()
            .rev()
            .fold(body, |tail, (fields, synthetic, leaves)| {
                self.lower_pattern_fields(fields, synthetic, leaves, tail)
            });

        Ok((lowered, body))
    }

    /// Wraps a parameter annotation in the field-`let` chain of every preceding
    /// compound pattern, outermost chain first. Only a chain whose leaf names
    /// the annotation actually mentions is emitted — every other domain keeps
    /// its written shape, so projections off an unrelated tuple parameter never
    /// show up in its error messages. (The *body* is wrapped unconditionally:
    /// it is the chain's original consumer and its shape is settled.)
    fn wrap_pattern_chains(
        &self,
        chains: &[(&[PatternField], Free, &[Bound])],
        annotation: curios_core::Term,
    ) -> curios_core::Term {
        chains
            .iter()
            .rev()
            .fold(annotation, |tail, (fields, synthetic, leaves)| {
                let free = tail.free_vars();
                // A leaf is mentioned iff one of the identities this chain binds
                // occurs free — an exact test, where matching by spelling could
                // only ever approximate one.
                match leaves.iter().any(|(_, id)| free.contains(id)) {
                    true => self.lower_pattern_fields(fields, synthetic, leaves, tail),
                    false => tail,
                }
            })
    }

    /// One pattern-leaf binder: its written spelling and the identity it lowers
    /// to. `_` gets an identity nothing can name, so repeated wildcards never
    /// collide.
    pub(super) fn pattern_binder(&self, name: &str) -> Bound {
        (
            name.to_string(),
            self.context.fresh_binder(bindable(name).then_some(name)),
        )
    }

    /// A nominal head's resolved name. Only a global can head a nominal
    /// literal; a local in that position is a resolution error the core stage
    /// reports, so an unresolved one keeps its own unbindable identity.
    pub(super) fn resolve_nominal(&self, name: &Name) -> Result<curios_core::Global, Error> {
        Ok(match self.resolve_name(name)?.as_global() {
            Some(global) => global.clone(),
            None => curios_core::Global::Authored(curios_base::Qualifier::from([name.head()])),
        })
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
    /// `binders` are the identities minted for this pattern's written leaves,
    /// in `pattern_names` order — the same ones the scope this `let` opened was
    /// entered with, so the tail's references land on them.
    pub(super) fn bind_pattern(
        &self,
        pattern: &Pattern,
        binders: &[Bound],
        type_: curios_core::Term,
        value: curios_core::Term,
        tail: curios_core::Term,
    ) -> curios_core::Term {
        self.bind_pattern_from(pattern, &mut binders.iter(), type_, value, tail)
    }

    fn bind_pattern_from<'i>(
        &self,
        pattern: &Pattern,
        binders: &mut impl Iterator<Item = &'i Bound>,
        type_: curios_core::Term,
        value: curios_core::Term,
        tail: curios_core::Term,
    ) -> curios_core::Term {
        match pattern {
            Pattern::Binder(Some(_)) => {
                let (_, id) = binders.next().expect("one mint per written leaf");
                curios_core::Term::let_(id, type_, value, tail)
            }
            Pattern::Binder(None) => {
                curios_core::Term::let_(&self.context.fresh_binder(None), type_, value, tail)
            }
            Pattern::Tuple(fields) | Pattern::Struct { fields, .. } => {
                let synthetic = self.context.fresh_binder(None);
                let inner = self.lower_pattern_fields_from(fields, &synthetic, binders, tail);
                curios_core::Term::let_(&synthetic, type_, value, inner)
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
    /// The chain over a compound pattern already in scope: its leaves' minted
    /// identities are pulled from the surrounding walk, which produced them in
    /// this very order.
    pub(super) fn lower_pattern_fields(
        &self,
        fields: &[PatternField],
        scrutinee: &Free,
        leaves: &[Bound],
        tail: curios_core::Term,
    ) -> curios_core::Term {
        self.lower_pattern_fields_from(fields, scrutinee, &mut leaves.iter(), tail)
    }

    fn lower_pattern_fields_from<'i>(
        &self,
        fields: &[PatternField],
        scrutinee_name: &Free,
        binders: &mut impl Iterator<Item = &'i Bound>,
        tail: curios_core::Term,
    ) -> curios_core::Term {
        // Right-to-left so the first field's `let` ends up outermost, but the
        // binders were minted left-to-right, so they are consumed in a forward
        // pass first.
        let mut bound = Vec::with_capacity(fields.len());
        for field in fields {
            let taken = pattern_names(&field.value)
                .iter()
                .filter_map(|_| binders.next().cloned())
                .collect::<Vec<_>>();
            bound.push(taken);
        }

        let mut tail = tail;
        for ((index, field), taken) in fields.iter().enumerate().zip(&bound).rev() {
            let scrutinee = curios_core::Term::var(curios_core::Var::free(scrutinee_name.clone()));
            let proj = match &field.label {
                Some(label) => curios_core::Term::proj_label(scrutinee, label.clone()),
                None => curios_core::Term::proj(scrutinee, index),
            };
            let hole = curios_core::Term::metavar(self.context.fresh_metavar());
            tail = self.bind_pattern(&field.value, taken, hole, proj, tail);
        }
        tail
    }

    /// A `Nat` succ or `Lst`/`Bin` cons arm's induction-hypothesis binder: an
    /// omitted `; ih` (`None` — there is no source name at all) mints an
    /// unwritten binder; a written one is minted with its spelling as the hint.
    pub(super) fn cons_ih_binder(&self, ih_label: &Option<String>) -> Bound {
        match ih_label {
            Some(name) => (
                name.clone(),
                self.context.fresh_binder(bindable(name).then_some(name)),
            ),
            None => (String::new(), self.context.fresh_binder(None)),
        }
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
    pub(super) fn wrap(
        &self,
        binds: Vec<(Free, curios_core::Term)>,
        body: curios_core::Term,
    ) -> Result<curios_core::Term, Error> {
        binds
            .into_iter()
            .rev()
            .try_fold(body, |acc, (binder, action)| {
                let domain = curios_core::Term::metavar(self.context.fresh_metavar());
                let cont = curios_core::Term::func([(binder, domain)], acc);
                // The already-resolved core name: the `Monad` concept at
                // `/syn`'s top level, method wrapper `bind`.
                Ok(Self::syn_call(
                    self.context.syntax().monad().bind(),
                    [action, cont],
                ))
            })
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
    pub(super) fn lower_lst_literal(
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

    /// The `Bits`/`Bytes` sibling of [`Self::lower_lst_literal`]: a spread-free
    /// literal lowers to one packed value, and spreads splice their packed runs
    /// into an n-ary `BinConcat` (the shared internal primitive has no element-type slot).
    pub(super) fn lower_bin_literal(
        grain: Grain,
        segments: &[BinSegment],
        mut lower: impl FnMut(&Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Prim, Error> {
        if segments
            .iter()
            .all(|segment| matches!(segment, BinSegment::Bytes(_)))
        {
            // Zero or one run in practice (the parser coalesces); flattening
            // keeps this robust for hand-built literals too.
            let atoms: Vec<u8> = segments
                .iter()
                .flat_map(|segment| match segment {
                    BinSegment::Bytes(run) => run.iter().copied(),
                    BinSegment::Spread(_) => unreachable!("all segments are byte runs"),
                })
                .collect();

            let value = match grain {
                Grain::B => PackedBin::from_bits(atoms.into_iter().map(|atom| atom != 0)),
                Grain::X => PackedBin::from_bytes(atoms),
            };
            return Ok(curios_core::Prim::Bin(grain, value));
        }

        let operands = segments
            .iter()
            .map(|segment| match segment {
                BinSegment::Bytes(run) => {
                    let value = match grain {
                        Grain::B => PackedBin::from_bits(run.iter().map(|atom| *atom != 0)),
                        Grain::X => PackedBin::from_bytes(run.clone()),
                    };
                    Ok(curios_core::Term::prim(curios_core::Prim::Bin(
                        grain, value,
                    )))
                }
                BinSegment::Spread(term) => lower(term),
            })
            .collect::<Result<Vec<_>, Error>>()?;

        Ok(curios_core::Prim::BinConcat(grain, operands))
    }

    pub(super) fn prim(&self, prim: &Prim) -> Result<curios_core::Prim, Error> {
        Ok(match prim {
            Prim::BoolType => curios_core::Prim::BoolType,
            Prim::Bool(b) => curios_core::Prim::Bool(*b),
            Prim::BoolAnd(left, right) => {
                curios_core::Prim::BoolAnd(self.term(left)?, self.term(right)?)
            }
            Prim::BoolOr(left, right) => {
                curios_core::Prim::BoolOr(self.term(left)?, self.term(right)?)
            }
            Prim::BoolXor(left, right) => {
                curios_core::Prim::BoolXor(self.term(left)?, self.term(right)?)
            }
            Prim::BoolEql(left, right) => {
                curios_core::Prim::BoolEql(self.term(left)?, self.term(right)?)
            }
            Prim::BoolNeq(left, right) => {
                curios_core::Prim::BoolNeq(self.term(left)?, self.term(right)?)
            }
            Prim::NatType => curios_core::Prim::NatType,
            Prim::Nat(Nat::Zero) => curios_core::Prim::Nat(curios_core::Nat::Zero),
            Prim::Nat(Nat::Succ(NatLiteral(spine, _), inner)) => {
                curios_core::Prim::Nat(curios_core::Nat::Succ(spine.clone(), self.term(inner)?))
            }
            Prim::ByteType => curios_core::Prim::ByteType,
            Prim::Byte(value) => curios_core::Prim::Byte(*value),
            Prim::ByteToNat(inner) => curios_core::Prim::ByteToNat(self.term(inner)?),
            Prim::NatToByte(inner) => curios_core::Prim::NatToByte(self.term(inner)?),
            Prim::ByteEql(left, right) => {
                curios_core::Prim::ByteEql(self.term(left)?, self.term(right)?)
            }
            Prim::ByteLt(left, right) => {
                curios_core::Prim::ByteLt(self.term(left)?, self.term(right)?)
            }
            Prim::ByteLte(left, right) => {
                curios_core::Prim::ByteLte(self.term(left)?, self.term(right)?)
            }
            Prim::ByteGt(left, right) => {
                curios_core::Prim::ByteGt(self.term(left)?, self.term(right)?)
            }
            Prim::ByteGte(left, right) => {
                curios_core::Prim::ByteGte(self.term(left)?, self.term(right)?)
            }
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
            Prim::NatRotl(left, right) => {
                curios_core::Prim::NatRotl(self.term(left)?, self.term(right)?)
            }
            Prim::NatRotr(left, right) => {
                curios_core::Prim::NatRotr(self.term(left)?, self.term(right)?)
            }
            Prim::NatClz(inner) => curios_core::Prim::NatClz(self.term(inner)?),
            Prim::NatCtz(inner) => curios_core::Prim::NatCtz(self.term(inner)?),
            Prim::NatPopcnt(inner) => curios_core::Prim::NatPopcnt(self.term(inner)?),
            Prim::IntType => curios_core::Prim::IntType,
            Prim::Int(value) => curios_core::Prim::Int(value.clone()),
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
            Prim::IntRotl(left, right) => {
                curios_core::Prim::IntRotl(self.term(left)?, self.term(right)?)
            }
            Prim::IntRotr(left, right) => {
                curios_core::Prim::IntRotr(self.term(left)?, self.term(right)?)
            }
            Prim::IntClz(inner) => curios_core::Prim::IntClz(self.term(inner)?),
            Prim::IntCtz(inner) => curios_core::Prim::IntCtz(self.term(inner)?),
            Prim::IntPopcnt(inner) => curios_core::Prim::IntPopcnt(self.term(inner)?),
            Prim::FltType => curios_core::Prim::FltType,
            Prim::Flt(flt) => curios_core::Prim::Flt(*flt),
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
            Prim::FltCopysign(left, right) => {
                curios_core::Prim::FltCopysign(self.term(left)?, self.term(right)?)
            }
            Prim::FltSqrt(inner) => curios_core::Prim::flt_sqrt(self.term(inner)?),
            Prim::FltFloor(inner) => curios_core::Prim::flt_floor(self.term(inner)?),
            Prim::FltCeil(inner) => curios_core::Prim::flt_ceil(self.term(inner)?),
            Prim::FltTrunc(inner) => curios_core::Prim::flt_trunc(self.term(inner)?),
            Prim::FltNearest(inner) => curios_core::Prim::flt_nearest(self.term(inner)?),
            Prim::FltToLeBytes(inner) => curios_core::Prim::flt_to_le_bytes(self.term(inner)?),
            Prim::FltOfLeBytes(inner) => curios_core::Prim::flt_of_le_bytes(self.term(inner)?),
            Prim::NatToInt(inner) => curios_core::Prim::nat_to_int(self.term(inner)?),
            Prim::HandleType => curios_core::Prim::HandleType,
            Prim::Handle(token) => curios_core::Prim::Handle(*token),
            Prim::HandleEql(left, right) => {
                curios_core::Prim::io_eql(self.term(left)?, self.term(right)?)
            }
            Prim::Foreign(function, args) => curios_core::Prim::Foreign(
                Arc::clone(function),
                args.iter()
                    .map(|arg| self.term(arg))
                    .collect::<Result<_, _>>()?,
            ),
            Prim::Exit(type_, code) => curios_core::Prim::Exit(self.term(type_)?, self.term(code)?),
            Prim::NatToFlt(inner) => curios_core::Prim::nat_to_flt(self.term(inner)?),
            Prim::IntToNat(inner) => curios_core::Prim::int_to_nat(self.term(inner)?),
            Prim::IntToFlt(inner) => curios_core::Prim::int_to_flt(self.term(inner)?),
            Prim::FltToNat(inner) => curios_core::Prim::flt_to_nat(self.term(inner)?),
            Prim::FltToInt(inner) => curios_core::Prim::flt_to_int(self.term(inner)?),
            Prim::BinType(grain) => curios_core::Prim::BinType(*grain),
            // `\hex` is a raw byte sequence; `\..` segments splice other `Bin`s.
            Prim::Bin(grain, segments) => {
                Self::lower_bin_literal(*grain, segments, |term| self.term(term))?
            }
            Prim::BinLen(grain, inner) => curios_core::Prim::bin_len(*grain, self.term(inner)?),
            Prim::BinEql(grain, left, right) => {
                curios_core::Prim::bin_eql(*grain, self.term(left)?, self.term(right)?)
            }
            Prim::BinGet(grain, bin, index) => {
                curios_core::Prim::bin_get(*grain, self.term(bin)?, self.term(index)?)
            }
            Prim::BinSlice(grain, bin, start, end) => curios_core::Prim::bin_slice(
                *grain,
                self.term(bin)?,
                self.term(start)?,
                self.term(end)?,
            ),
            Prim::BinAppend(grain, bin, atom) => {
                curios_core::Prim::bin_append(*grain, self.term(bin)?, self.term(atom)?)
            }
            Prim::BinConcat(grain, left, right) => {
                curios_core::Prim::bin_concat(*grain, [self.term(left)?, self.term(right)?])
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
            Prim::LstMap(a, b, lst, f) => curios_core::Prim::lst_map(
                self.term(a)?,
                self.term(b)?,
                self.term(lst)?,
                self.term(f)?,
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

/// The binder names a parameter list introduces — every leaf binder name
/// in each parameter's pattern, flattened, all in scope across the body.
/// These shadow like-named module bindings; the wildcard `_` rides along
/// but is ignored by [`Lowerer::scoped`].
/// Whether a written binder name can be referred to. `_` and the empty label
/// occupy a binder position but name nothing.
fn bindable(name: &str) -> bool {
    !(name.is_empty() || name == "_")
}

fn param_names(params: &[FuncParam]) -> Vec<String> {
    params
        .iter()
        .flat_map(|param| pattern_names(&param.pattern))
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
            .flat_map(|field| pattern_names(&field.value))
            .collect(),
    }
}
