use curios_elab::{IntrinsicBuilders, TermBuilders};
use {
    super::{Context, MatchCompiler},
    crate::{
        BinSegment, Choose, ChooseTest, Error, Field, FuncParam, Intrinsic, Let, LetBinding,
        ListEntry, Name, Nat, NatLiteral, NumLit, Pattern, PatternField, Rec, StructLitEntry,
        Subterm, Syn, Term,
    },
    curios_utilities::{Grain, PackedBin, Plicity, Qualifier, Span, SyntaxName, recurse},
    std::{cell::RefCell, sync::Arc},
};

/// A lowered function's binders: `(plicity, core binder, domain)` per slot, paralleling the surface parameter list.
type LoweredParams = Vec<(Plicity, curios_core::Free, curios_core::Term)>;

/// A source binder brought into lexical scope: what it was written as, and the identity every reference to it lowers to.
///
/// Minted once, where the binder is introduced, and reused everywhere that binder is in scope — a progressively-extended parameter list re-enters the same identities rather than re-minting them, or an earlier domain and the body would disagree about which binder a name meant.
pub(super) type Binder = (String, curios_core::Free);

/// One `!` hoisted out of a region: the binder its result lands in, the action to sequence, and the span of the `!` itself.
///
/// The span is what [`Lowerer::wrap`] stamps onto the synthesized `/syn/Monad/bind` application. Without it the sequencing is the one node in a lowered value body that no source location reaches, so a `!` the region cannot accept — an annotated top-level `let`, a non-monadic helper — reports its type error with no `-->` line at all.
pub(super) struct Hoisted {
    pub(super) binder: curios_core::Free,
    pub(super) action: curios_core::Term,
    pub(super) span: Option<Span>,
}

pub(super) struct Lowerer<'a, 'b> {
    pub(super) context: &'a Context<'b>,
    /// The enclosing local binders (function and `let`/`rec` binders, match-arm patterns, motive labels), innermost last. A bare reference whose spelling appears here resolves to the innermost such binder rather than a like-named module binding — see [`Self::resolve_name`]. Compiler-minted binders are never registered: nothing can write their name.
    scope: RefCell<Vec<Binder>>,
}

impl<'a, 'b> Lowerer<'a, 'b> {
    pub(super) fn new(context: &'a Context<'b>) -> Self {
        Self {
            context,
            scope: RefCell::new(Vec::new()),
        }
    }

    /// Mint one binder identity per written name, in order. Nothing is brought into scope: the caller decides where each binder is visible, and the identity it holds is the one every such region re-enters.
    ///
    /// An unwritten (`_` or empty) name still gets an identity — it occupies a binder position — but no reference can reach it.
    pub(super) fn mint(&self, names: impl IntoIterator<Item = String>) -> Vec<Binder> {
        names
            .into_iter()
            .map(|name| {
                let id = self.context.fresh_binder(bindable(&name).then_some(&name));
                (name, id)
            })
            .collect()
    }

    /// Lower `body` with already-minted `binders` in scope, then restore the previous scope.
    ///
    /// The scope is a stack, so a shadowing inner binder simply sits above the outer one and [`Self::resolve_name`]'s innermost-first scan finds it. A `let` block nests one of these per binding, by recursing rather than by holding a mark per binding across a loop.
    pub(super) fn bound<T>(
        &self,
        binders: &[Binder],
        body: impl FnOnce() -> Result<T, Error>,
    ) -> Result<T, Error> {
        let mark = {
            let mut scope = self.scope.borrow_mut();
            let mark = scope.len();
            scope.extend(binders.iter().filter(|(name, _)| bindable(name)).cloned());
            mark
        };

        let result = body();
        self.scope.borrow_mut().truncate(mark);
        result
    }

    /// Lowers a *value* body — a top-level `let`/`rec` body, a witness field, or the entrypoint tail. Every value body is a region root: each `!` in it hoists here (never past a boundary — a lambda body, match arm, or `rec` item re-roots) and is rewired through `/syn/Monad/bind`, whose `use` binder resolves the `Monad` witness per site. Types go through [`Self::term`], where `!` is rejected.
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

    /// Lower a type in an input position. The role is lexical, so every written `Type` inside a nested higher-kinded domain remains eligible for declaration generalization.
    pub(super) fn input_type(&self, term: &Term) -> Result<curios_core::Term, Error> {
        self.context
            .with_universe_role(curios_core::UniverseRole::Generalizable, || self.term(term))
    }

    /// Resolve a surface name to its qualified (joined) core name — the same rule the `Subterm::Name` term-reference arm uses.
    pub(super) fn resolve_name(&self, name: &Name) -> Result<curios_core::Free, Error> {
        if name.is_abs() || !name.is_single() {
            return Ok(curios_core::Free::global(
                self.context.resolve_term_name(name)?,
            ));
        }
        // A local binder shadows any like-named module binding, and the innermost one wins. Resolving here — rather than emitting the spelling for a later stage to re-resolve — is what makes shadowing exact: two binders written `go` are two identities from the start.
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
            Some(full) => Ok(curios_core::Free::global(full.clone())),
            // Unresolved, and `curios-elab` is what reports it — so this must lower to something no definition can ever be. A binder identity is unbound by construction (nothing closes over it) and carries the written name as its hint, so the diagnostic still names it; what this stage adds beside it is what the name could have meant, which only this stage can say.
            //
            // A root-level global would *not* do: `Qualifier::from([head])` is exactly what an entry-module `let helper` lowers to, so an unresolvable reference in a nested module would silently capture it. The old spelling-keyed lowering was safe only by accident — it emitted a bare `helper` while every definition carried a leading `/`.
            None => Ok(self.context.unbound_binder(name.head())),
        }
    }

    // The meta-emitter: a string literal becomes a proof-carrying `/syn/Str/Str` value `Str { bytes = <Bytes>, valid = <proof> }`. `valid` is erased, so at runtime `Str` collapses to its `Bytes` field — a literal costs exactly what a `Bytes` literal does.
    //
    // # Why the proof is a computation and not a derivation
    //
    // `Valid(b)` is `Utf8(lead, b)`, an inductive family whose canonical inhabitant is one `more` link per byte. Writing that out made the *term* linear in the data, and everything that walks a term inherited it: elaboration, zonking, both erasure obligations, the printer, and the kernel's typing judgment. Five separate stack-overflow or quadratic defects traced to that one shape, and the reduction budget capped a literal near 23KiB regardless.
    //
    // So the proof emitted here is `of_scan_eq(b, refl_scan(b))`: constant size, discharged by *running* the `scan_from` fold rather than by traversing a derivation. `/syn/Str/of_scan_eq` rebuilds the derivation by reduction for the lemmas in `/std/Str/utf8` that genuinely eliminate it, so none of them changed.
    //
    // # What bounds a literal now
    //
    // Reduction of the scan is linear in the literal's length, and it runs on `curios-core`'s closed machine — the explicit-stack evaluator both checkers enter for closed terms — so a character costs transitions and machine frames rather than the native reduction level the scan used to nest per byte, and guarded depth is flat in the length. No figure is quoted here, because a figure quoted here has decayed twice; `curios`' `str_literal_cost_measurements` carries the per-character price and the ceiling with their dates, and `a_str_literal_costs_transitions_rather_than_frames` is the ordinary assertion that holds the shape.
    //
    // The two remedies an earlier version of this note deferred are settled by that machine rather than taken. A native scan intrinsic is refused where `documentation/design/toolchain/evaluating-a-closed-term-is-representation-not-judgment.md` records — it would bless one type's fold where the machine accelerates every closed fold on the same terms, `Str`'s and a user's alike. And full reflection — restating `Valid` as an equation on `scan_from` and rewriting `/std/Str/utf8`'s lemmas as fold algebra — stays unnecessary, because the derivation this bridge preserves now costs what the machine prices it rather than a frame per link.
    pub(super) fn str_literal(&self, bytes: &[u8]) -> curios_core::Term {
        let packed = curios_core::Term::intrinsic(curios_core::Intrinsic::Bin(
            Grain::X,
            PackedBin::from_bytes(bytes.to_vec()),
        ));

        let syntax = self.context.syntax().string;
        let valid = Self::syn_call(
            syntax.of_scan_eq,
            [
                packed.clone(),
                Self::syn_call(syntax.refl_scan, [packed.clone()]),
            ],
        );

        curios_core::Term::struct_(
            curios_core::Global::Authored(syntax.string.qualifier()),
            Vec::<curios_core::Term>::new(),
            [packed, valid],
        )
    }

    /// A Rust `char` is already a Unicode scalar, so the literal meta-emitter selects the corresponding `/syn/Char/Scalar` range constructor and supplies a closed reflected proof. The proof erases and the single relevant `code` field collapses to the ordinary Nat carrier.
    pub(super) fn char_literal(&self, character: char) -> curios_core::Term {
        let code = curios_core::Term::intrinsic(curios_core::Intrinsic::Nat(
            curios_core::Nat::new(character as u32),
        ));
        let constructor = if (character as u32) < 0xD800 {
            self.context.syntax().character.scalar_below
        } else {
            self.context.syntax().character.scalar_above
        };
        let scalar = curios_core::Term::apply_marked(
            curios_core::Term::var(curios_core::Var::free(curios_core::Free::global(
                constructor.qualifier(),
            ))),
            [
                (Plicity::Implicit, code.clone()),
                (
                    Plicity::Explicit,
                    Self::syn_call(self.context.syntax().proof.true_qed, []),
                ),
            ],
        );

        curios_core::Term::struct_(
            curios_core::Global::Authored(self.context.syntax().character.character.qualifier()),
            Vec::<curios_core::Term>::new(),
            [code, scalar],
        )
    }

    // A constructor/function `Var` applied to `args` — the absolute core name as the parser would resolve it (privacy is a surface-resolution concern; these are already-resolved core `Var`s, so referencing a private `/syn` helper is fine).
    pub(super) fn syn_call(
        name: SyntaxName,
        args: impl IntoIterator<Item = curios_core::Term>,
    ) -> curios_core::Term {
        curios_core::Term::apply(
            curios_core::Term::var(curios_core::Var::free(curios_core::Free::global(
                name.qualifier(),
            ))),
            args.into_iter().collect::<Vec<_>>(),
        )
    }

    // The `Utf8(state, bytes)` derivation. `state` is carried as a *symbolic* term — `lead()` at the top, then `step(c, state)` per byte — so each recursive `rest`'s expected index (`Utf8(step(c, state), tail)`) is definitionally the state we thread in, with no metavar/`step`-inversion. The final `stop : Utf8(lead, x[])` matches because `step` of the last byte reduces back to `lead` for valid UTF-8 (a string literal is valid UTF-8 by construction). A `/syn` literal — its value is synthesized from `/syn` by the meta-emitter rather than lowered to a core intrinsic.
    pub(super) fn syn_literal(&self, syn: &Syn) -> Result<curios_core::Term, Error> {
        match syn {
            Syn::Char(character) => Ok(self.char_literal(*character)),
            Syn::Str(string) => Ok(self.str_literal(string.as_bytes())),
        }
    }

    pub(super) fn subterm(
        &self,
        term: &Subterm,
        span: Option<&Span>,
    ) -> Result<curios_core::Term, Error> {
        Ok(match term {
            Subterm::Type => curios_core::Term::type_at(self.context.fresh_universe(span)),
            Subterm::Prop => curios_core::Term::prop(),
            Subterm::Hole => curios_core::Term::metavar(self.context.fresh_metavar()),
            // A written goal `?`: same fresh metavariable, but marked so zonk reports what elaboration determined for it instead of splicing.
            Subterm::Goal => curios_core::Term::goal(self.context.fresh_metavar()),
            // A `/syn` literal (string or list) desugars via the meta-emitter to a `/syn` construction (see `syn_literal`), never a core intrinsic.
            Subterm::Syn(syn) => self.syn_literal(syn)?,
            Subterm::Intrinsic(intrinsic) => {
                curios_core::Term::intrinsic(self.intrinsic(intrinsic)?)
            }
            Subterm::Foreign(function, args) => curios_core::Term::foreign(
                Arc::clone(function),
                args.iter()
                    .map(|arg| self.term(arg))
                    .collect::<Result<_, _>>()?,
            ),
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
            // Each parameter type sees the *preceding* parameters' binders, and the output sees them all (a dependent Π-type), so they lower under a progressively-extended scope.
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
            // A dependent Σ-type: each field type sees the preceding fields' labels, so they lower under a progressively-extended scope. The signature sugar `f(params) -> T` is undone here.
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
            // A struct literal lowers to a `curios_core::Struct` carrying the resolved (qualified) struct name, the head parameters (empty → core elaboration mints metavariables), and the written entries — plain field values with their names (validated positionally and dropped by elaborate), `use <term>` fills for a concept's `use`-marked positions, and a `..base` spread carrying its base. Construction privacy and spread shape are enforced in core (`elaborate_struct`), alongside projection privacy.
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
            // A `choose` right-folds into nested `Bool` matches: each `cond => body` becomes `match cond | false => <rest> | true => body end`, the `_` default sitting at the innermost false branch. No motive at any level (a fresh hole each), matching the surface form's absence of one. Arms inherit the definitional refinement of their conditions for free — that is exactly what nesting `Bool` matches buys.
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
                // The matrix compiler recursively decomposes (possibly nested, across constructors/tuples/structs) arm patterns into single-level core `Match`/projection forms — see `MatchCompiler::compile_matrix`. A final `| _ =>` catch-all is split off as the dispatch default.
                let head = self.term(&match_.head)?;
                MatchCompiler::new(self).compile_matrix_headed(
                    head,
                    &match_.motive,
                    &match_.arms,
                    MatchCompiler::term,
                )?
            }
            // A `let` block is non-recursive: each binding is in scope for the bindings after it and the tail, never for its own type or value. Lower each binding's type/value in the scope of the bindings before it, then wrap them back (reverse) around the tail.
            Subterm::Let(let_) => self.lower_let(&let_.bindings, &let_.tail)?,
            // A `rec` is mutually recursive: every item label is in scope across all item types, all item bodies, and the tail.
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
            // A bang here was reached through a *type* lowering (an annotation, a motive, a Π/Σ component): types have no region to hoist to. Value bodies enter through `value`/`region`, which eliminates every `Bang` before this arm could see it.
            Subterm::Bang(_) => return Err(Error::BangInTypePosition),
        })
    }

    /// Desugars `term` as a single **region**. A region is a stretch of a value body that shares one continuation; each `!` in it hoists to the top of the region, never past a boundary (lambda body, match arm, `rec` item). Boundaries re-root a region. Every hoisted action is sequenced through `/syn/Monad/bind` — see `wrap`.
    pub(super) fn region(&self, term: &Term) -> Result<curios_core::Term, Error> {
        match term.as_subterm() {
            // A `let`'s bound expression evaluates in place (its bangs hoist to this region); the tail continues the same region (a bang there hoists after `x` is bound, not above the `let`).
            Subterm::Let(let_) => self.lower_let_region(&let_.bindings, &let_.tail),
            // The scrutinee evaluates before branching (its bangs hoist here); each arm is its own region (branch-local effects).
            Subterm::Match(match_) => {
                let mut binds = Vec::new();
                let match_term = MatchCompiler::new(self).match_region(match_, &mut binds)?;
                self.wrap(binds, match_term)
            }
            // The `choose` head arm's test runs unconditionally (its bangs hoist here); deeper arms and the default are branch-local.
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
            // A `rec`'s item bodies are their own regions (hoisting an action out of a recursive binding would change how often it runs); the tail continues like a `let` tail.
            Subterm::Rec(rec) => self.build_rec(rec),
            // Spine forms (atomic / apply / tuple / proj): collect bangs in left-to-right evaluation order, then wrap.
            _ => {
                let mut binds = Vec::new();
                let body = self.collect(term, &mut binds)?;
                self.wrap(binds, body)
            }
        }
    }

    /// Lowers a run of `let` `bindings` and its `tail` as one region: each binding's bound expression is a value in this region (its bangs hoist here, sequenced by `wrap`), the tail continues the same region. Loops over `bindings` rather than recursing once per binding — a `let` block is flat, so a long straight-line sequence costs one loop, not a stack of native frames. Shared by `region`'s `Let` arm (the whole block) and `build_let` (the bindings after the first, whose own bangs hoist to the enclosing region instead).
    pub(super) fn lower_let_region(
        &self,
        bindings: &[LetBinding],
        tail: &Term,
    ) -> Result<curios_core::Term, Error> {
        recurse(|| {
            let Some((first, rest)) = bindings.split_first() else {
                return self.region(tail);
            };

            let mut binds = Vec::new();
            let value = self.collect(&first.signature.body(), &mut binds)?;
            let type_ = self.term(&first.signature.type_())?;
            let binders = self.mint(pattern_names(&first.binder));
            let inner = self.bound(&binders, || self.lower_let_region(rest, tail))?;
            let let_term = self.bind_pattern(&first.binder, &binders, type_, value, inner);

            self.wrap(binds, let_term)
        })
    }

    /// [`Self::lower_let_region`] for a `let` reached through [`Self::term`] — a type, an annotation, a motive — where there is no region to hoist a bang into, so a binding is a plain value and nothing wraps.
    ///
    /// Its bindings lower type-before-value, where the region form lowers value-before-type. The two orders mint identities in different sequences and are each what their site has always done; they are deliberately not unified here.
    fn lower_let(&self, bindings: &[LetBinding], tail: &Term) -> Result<curios_core::Term, Error> {
        recurse(|| {
            let Some((first, rest)) = bindings.split_first() else {
                return self.term(tail);
            };

            let type_ = self.term(&first.signature.type_())?;
            let value = self.term(&first.signature.body())?;
            let binders = self.mint(pattern_names(&first.binder));
            let inner = self.bound(&binders, || self.lower_let(rest, tail))?;

            Ok(self.bind_pattern(&first.binder, &binders, type_, value, inner))
        })
    }

    /// Builds the core `rec` for a `rec` inside a value body: item types are types, item bodies are fresh region roots, and the tail continues as its own region (a bang there hoists after the bindings, not above them).
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

    /// Walks a non-boundary expression, elaborating to core and accumulating each `Bang` into `binds` (in evaluation order) replaced by a fresh variable. Boundary/binding forms desugar as their own nested region; `let`/`match` hoist their bound-expression/scrutinee bangs into the *enclosing* `binds`.
    ///
    /// Span stamping happens here rather than inside the walk for the same reason [`Self::term`] stamps at its own boundary: every spine arm *rebuilds* its node (`apply_marked`, `proj`, `infix`, …) instead of routing through `term`, so a node lowered in a value body would otherwise reach elaboration with no span and report its errors unlocated. `with_span` is innermost-wins, so the arms that do delegate — leaves through `term`, lambdas through `region` — keep the span they already carry.
    pub(super) fn collect(
        &self,
        term: &Term,
        binds: &mut Vec<Hoisted>,
    ) -> Result<curios_core::Term, Error> {
        let lowered = self.collect_spine(term, binds)?;
        Ok(match term.span() {
            Some(span) => lowered.with_span(span.clone()),
            None => lowered,
        })
    }

    fn collect_spine(
        &self,
        term: &Term,
        binds: &mut Vec<Hoisted>,
    ) -> Result<curios_core::Term, Error> {
        Ok(match term.as_subterm() {
            Subterm::Bang(action) => {
                // The action is itself desugared first, so its inner bangs evaluate before this one (left-to-right).
                let action = self.collect(action, binds)?;
                let binder = self.context.fresh_binder(None);
                let var = curios_core::Term::var(curios_core::Var::free(binder.clone()));
                binds.push(Hoisted {
                    binder,
                    action,
                    span: term.span().cloned(),
                });
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
            // A struct literal's entry values hoist their bangs into this region, exactly like a tuple's fields.
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
            // An infix operator's operands hoist their bangs into this region, exactly like an application's arguments.
            Subterm::Infix(infix) => curios_core::Term::infix(
                infix.op,
                self.collect(&infix.left, binds)?,
                self.collect(&infix.right, binds)?,
            ),
            // An `List` literal's elements and spread operands hoist their bangs into this region, like an application's arguments.
            Subterm::Intrinsic(Intrinsic::List(entries)) => curios_core::Term::intrinsic(
                self.lower_list_literal(entries, |term| self.collect(term, binds))?,
            ),
            // A `Bits`/`Bytes` literal's spread operands hoist likewise (a spread-free literal has no subterms and lowers unchanged).
            Subterm::Intrinsic(Intrinsic::Bin(grain, segments)) => {
                curios_core::Term::intrinsic(Self::lower_bin_literal(*grain, segments, |term| {
                    self.collect(term, binds)
                })?)
            }
            // A `let`/`match`/`choose` sub-expression hoists its bound-expression / scrutinee / head-test bangs into the enclosing region (this `binds`).
            Subterm::Let(let_) => self.build_let(let_, binds)?,
            Subterm::Match(match_) => MatchCompiler::new(self).match_region(match_, binds)?,
            Subterm::Choose(choose) => MatchCompiler::new(self).choose_region(choose, binds)?,
            // A lambda is a value and a `rec` binds recursively: neither hoists anything outward, so desugar each as its own region.
            Subterm::Func(_) | Subterm::Rec(_) => self.region(term)?,
            // Leaves elaborate normally. A `Bang` reachable here (e.g. nested in a type position) hits `self.term`'s `Bang` arm and is rejected.
            _ => self.term(term)?,
        })
    }

    /// Builds a `let` block reached inside a `collect` (spine) context. The *first* binding's bangs hoist to the enclosing region (`binds`); the bindings after it and the tail form their own region via `lower_let_region`, scoped under the first binder.
    pub(super) fn build_let(
        &self,
        let_: &Let,
        binds: &mut Vec<Hoisted>,
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

    /// Lowers a function's parameters into core binder `(name, domain)` pairs. A plain-name parameter binds its name directly, unchanged; an un-annotated parameter takes a fresh metavar domain. A compound pattern's core binder is a fresh synthetic name, and the (already lowered) `body` is wrapped with its field-`let` chain.
    ///
    /// Each annotation sees the *preceding* parameters' binders, exactly as a dependent Π-type's domains do (the `Subterm::FuncType` arm), so a lambda may be written `(s, t, q : Eq(s, t)) => …`. That is why the walk runs in declaration order under a progressively-extended scope: `Telescope::build` captures each earlier binder in every later domain, so the core side needs nothing further. A compound pattern binds no leaf name at the core binder — its leaves are projections off the synthetic binder — so a later annotation naming one of those leaves gets that pattern's field-`let` chain wrapped around the *domain* as well, mirroring what the body gets.
    ///
    /// The chains wrap body and domains alike in reverse, so each pattern's chain wraps *before* an earlier pattern's chain wraps that, giving the declaration-order nesting the spec's motivating example expects.
    pub(super) fn lower_func_params(
        &self,
        params: &[FuncParam],
        binders: &[Binder],
        body: curios_core::Term,
    ) -> Result<(LoweredParams, curios_core::Term), Error> {
        let mut lowered = Vec::with_capacity(params.len());
        // The binders already minted for the leaves, consumed in the same pre-order `param_names` produced them, plus the field-`let` chains that put the compound patterns in scope — both advance with the walk.
        let mut seen = 0;
        let mut chains: Vec<(&[PatternField], curios_core::Free, &[Binder])> = Vec::new();

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
            // The mark applies to the outer function slot the parameter occupies, whatever the pattern shape: a compound pattern's fresh core binder still claims a slot of the written plicity.
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

    /// Wraps a parameter annotation in the field-`let` chain of every preceding compound pattern, outermost chain first. Only a chain whose leaf names the annotation actually mentions is emitted — every other domain keeps its written shape, so projections off an unrelated tuple parameter never show up in its error messages. (The *body* is wrapped unconditionally: it is the chain's original consumer and its shape is settled.)
    fn wrap_pattern_chains(
        &self,
        chains: &[(&[PatternField], curios_core::Free, &[Binder])],
        annotation: curios_core::Term,
    ) -> curios_core::Term {
        chains
            .iter()
            .rev()
            .fold(annotation, |tail, (fields, synthetic, leaves)| {
                let free = tail.free_vars();
                // A leaf is mentioned iff one of the identities this chain binds occurs free — an exact test, where matching by spelling could only ever approximate one.
                match leaves.iter().any(|(_, id)| free.contains(id)) {
                    true => self.lower_pattern_fields(fields, synthetic, leaves, tail),
                    false => tail,
                }
            })
    }

    /// One pattern-leaf binder: its written spelling and the identity it lowers to. `_` gets an identity nothing can name, so repeated wildcards never collide.
    pub(super) fn pattern_binder(&self, name: &str) -> Binder {
        (
            name.to_string(),
            self.context.fresh_binder(bindable(name).then_some(name)),
        )
    }

    /// A nominal head's resolved name. Only a global can head a nominal literal; a local in that position is a resolution error the core stage reports, so an unresolved one keeps its own unbindable identity.
    pub(super) fn resolve_nominal(&self, name: &Name) -> Result<curios_core::Global, Error> {
        Ok(match self.resolve_name(name)?.as_global() {
            Some(global) => global.clone(),
            None => curios_core::Global::Authored(Qualifier::from([name.head()])),
        })
    }

    /// Builds `let pat = value : type_; tail` for a pattern in any of the three binder positions: `Pattern::Binder` is today's single core `let_` call, unchanged — the whole reason the plain-name path stays a zero-cost passthrough. A compound pattern mints one fresh synthetic binder (via [`Context::fresh_binder`]) carrying `type_` (the caller's own annotation, so it is still checked), then projects each field off it via [`Self::lower_pattern_fields`]. The synthetic binder is minted unconditionally, even when `value` is already a bare variable reference: reusing it directly would risk silently dropping `type_`'s check (e.g. `let (x, y) : Point = pair;` must still check `pair : Point`). The extra trivial `let` this occasionally emits is exactly the shape `cont`'s copy-threading optimization already collapses, so it costs nothing at runtime. `binders` are the identities minted for this pattern's written leaves, in `pattern_names` order — the same ones the scope this `let` opened was entered with, so the tail's references land on them.
    pub(super) fn bind_pattern(
        &self,
        pattern: &Pattern,
        binders: &[Binder],
        type_: curios_core::Term,
        value: curios_core::Term,
        tail: curios_core::Term,
    ) -> curios_core::Term {
        self.bind_pattern_from(pattern, &mut binders.iter(), type_, value, tail)
    }

    fn bind_pattern_from<'i>(
        &self,
        pattern: &Pattern,
        binders: &mut impl Iterator<Item = &'i Binder>,
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

    /// Projects each field of a compound pattern off the (already-bound) core variable `scrutinee_name`, in field order — folded right-to-left so the first field's `let` ends up outermost, matching the order a person would hand-write (`let x = p0.0; let y = p0.1; …`) — recursing into [`Self::bind_pattern`] for nested patterns. Each field's own type is a fresh metavar hole: there is never a per-field annotation to give, exactly like a hand-written `let x = p.0;`. The chain over a compound pattern already in scope: its leaves' minted identities are pulled from the surrounding walk, which produced them in this very order.
    pub(super) fn lower_pattern_fields(
        &self,
        fields: &[PatternField],
        scrutinee: &curios_core::Free,
        leaves: &[Binder],
        tail: curios_core::Term,
    ) -> curios_core::Term {
        self.lower_pattern_fields_from(fields, scrutinee, &mut leaves.iter(), tail)
    }

    fn lower_pattern_fields_from<'i>(
        &self,
        fields: &[PatternField],
        scrutinee_name: &curios_core::Free,
        binders: &mut impl Iterator<Item = &'i Binder>,
        tail: curios_core::Term,
    ) -> curios_core::Term {
        // Right-to-left so the first field's `let` ends up outermost, but the binders were minted left-to-right, so they are consumed in a forward pass first.
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

    /// A `Nat` succ or `List`/`Bin` cons arm's induction-hypothesis binder: an omitted `; ih` (`None` — there is no source name at all) mints an unwritten binder; a written one is minted with its spelling as the hint.
    pub(super) fn cons_ih_binder(&self, ih_label: &Option<String>) -> Binder {
        match ih_label {
            Some(name) => (
                name.clone(),
                self.context.fresh_binder(bindable(name).then_some(name)),
            ),
            None => (String::new(), self.context.fresh_binder(None)),
        }
    }

    /// Wraps `body` in one [`curios_core::Bang`] transient per collected bang. The first-collected bang (`binds[0]`) becomes the outermost node, preserving left-to-right evaluation order. Continuation lambdas are built with `curios_core::Term::func` over the gensym'd free name, whose `capture` closes it robustly under nesting; the domain is a fresh hole, inference-solved. `elaborate_bang` later replaces each node with its `/syn/Monad/bind` application, inserting fresh implicits and a fresh `use` witness slot per `!` site: the action's type pins the constructor (via the flex-apply imitation rule), which resolves the `Monad` witness — so a region can sequence actions of differing result types, and different regions can use different monads.
    ///
    /// Each node carries the span of the `!` that produced it (see [`Hoisted`]), so a region that cannot accept the sequencing reports against the written `!` rather than against nothing.
    pub(super) fn wrap(
        &self,
        binds: Vec<Hoisted>,
        body: curios_core::Term,
    ) -> Result<curios_core::Term, Error> {
        binds.into_iter().rev().try_fold(body, |acc, hoisted| {
            let Hoisted {
                binder,
                action,
                span,
            } = hoisted;
            let domain = curios_core::Term::metavar(self.context.fresh_metavar());
            let cont = curios_core::Term::func([(binder, domain)], acc);
            let bang = curios_core::Term::bang(action, cont);
            Ok(match span {
                Some(span) => curios_core::Term::spanned(span, bang),
                None => bang,
            })
        })
    }

    /// Flush the pending elements onto `operands`.
    ///
    /// A single element following an operand is an *append* onto it: the surface wrote one generator, and `ListAppend` is what one generator is — the same reading `\.` takes on the packed side, so `[..xs, y]` and `x[..xs, y]` lower alike. Two or more go into an `List` chunk, which the carrier holds directly; the packed literal chunks its atoms the same way whenever it can represent them, and reaches for `BinAppend` only where it cannot.
    fn flush_list_run(
        &self,
        operands: &mut Vec<curios_core::Term>,
        run: &mut Vec<curios_core::Term>,
    ) {
        let element = || curios_core::Term::metavar(self.context.fresh_metavar());

        match (run.len(), operands.last()) {
            (0, _) => {}
            (1, Some(_)) => {
                let base = operands.pop().expect("the operand just matched");
                let elem = run.pop().expect("the run just measured one");
                operands.push(curios_core::Term::intrinsic(
                    curios_core::Intrinsic::list_append(element(), base, elem),
                ));
            }
            _ => operands.push(curios_core::Term::intrinsic(curios_core::Intrinsic::List {
                element: element(),
                items: std::mem::take(run),
            })),
        }
    }

    /// Lowers a list literal's entries. A spread-free literal lowers to a plain `List` — exactly the pre-spread lowering, `[]` included. With spreads, elements join the literal through [`Self::flush_list_run`] and the whole becomes an n-ary `ListConcat`; its element-type slot is a fresh metavar (an implicit the literal cannot name), solved by elaboration — bidirectionally from the expected type when checking (see the `ListConcat` case in `curios_elab`'s `elaborate_intrinsic`). `lower` is the per-term lowering — [`Self::term`] on the plain path, the bang-collector on the region path — so both share this grouping.
    pub(super) fn lower_list_literal(
        &self,
        entries: &[ListEntry],
        mut lower: impl FnMut(&Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Intrinsic, Error> {
        // The literal's element-type slot: an implicit the literal cannot name, minted fresh and solved by elaboration — bidirectionally from the expected type when checking, from the elements otherwise.
        let element = || curios_core::Term::metavar(self.context.fresh_metavar());

        let mut operands = Vec::new();
        let mut run = Vec::new();

        for entry in entries {
            match entry {
                ListEntry::Elem(term) => run.push(lower(term)?),
                ListEntry::Spread(term) => {
                    self.flush_list_run(&mut operands, &mut run);
                    operands.push(lower(term)?);
                }
            }
        }

        if operands.is_empty() {
            return Ok(curios_core::Intrinsic::List {
                element: element(),
                items: run,
            });
        }

        self.flush_list_run(&mut operands, &mut run);

        match operands.len() {
            // A lone list-shaped operand is the value itself; the concatenation would only be normalised away. Only the family the literal builds may collapse: any other lone operand keeps its wrapper, which is what makes elaboration check a spread (`[..b]`) against a list type instead of adopting the operand's own — `[..true]` once collapsed to `true` and typechecked as `Bool`.
            1 => match &*operands[0] {
                curios_core::Subterm::Intrinsic(
                    intrinsic @ (curios_core::Intrinsic::List { .. }
                    | curios_core::Intrinsic::ListAppend { .. }
                    | curios_core::Intrinsic::ListConcat { .. }),
                ) => Ok(intrinsic.clone()),
                _ => Ok(curios_core::Intrinsic::ListConcat {
                    element: element(),
                    operands,
                }),
            },
            _ => Ok(curios_core::Intrinsic::ListConcat {
                element: element(),
                operands,
            }),
        }
    }

    /// A constant element folded back into the literal's byte run. Escaped as `\48`/`\1` it is already a run; written as a term (`0x48`, `true`) the parser cannot tell it from a computed one, and left as an atom it would build an append chain where the escaped spelling builds a single packed value. `core::spine` decodes a concrete appended atom as a length-1 literal run, so conversion equates the two spellings either way — this is compaction, not meaning.
    ///
    /// A written sign excludes `Byte` (see `syntax.md`), and a magnitude past `255` is a type error elaboration should report against the expected element type rather than one this fold silently truncates; both stay atoms.
    fn bin_constant_atom(grain: Grain, term: &Term) -> Option<u8> {
        match (grain, term.as_subterm()) {
            (Grain::B, Subterm::Intrinsic(Intrinsic::Bool(bit))) => Some(u8::from(*bit)),
            // Only `0` and `1` are bits; anything else stays an atom term, so elaboration owns the range refusal exactly as it does for an out-of-range byte below.
            (
                Grain::B,
                Subterm::NumLit(NumLit {
                    magnitude, signed, ..
                }),
            ) => match signed {
                true => None,
                false => magnitude.to_u8().filter(|bit| *bit <= 1),
            },
            (Grain::X, Subterm::Intrinsic(Intrinsic::Byte(byte))) => Some(*byte),
            (
                Grain::X,
                Subterm::NumLit(NumLit {
                    magnitude, signed, ..
                }),
            ) => match signed {
                true => None,
                false => magnitude.to_u8(),
            },
            _ => None,
        }
    }

    /// The `Bits`/`Bytes` sibling of [`Self::lower_list_literal`]: a constant literal lowers to one packed value, and atom and spread segments splice into an n-ary `BinConcat` (the shared internal intrinsic has no element-type slot). Adjacent constant atoms — the values [`Self::bin_constant_atom`] recognizes — fold into packed runs as they are met, so `x[0x48, 0x69]` is the one packed value it is rather than a chain of appends.
    ///
    /// A non-constant atom is the free monoid's generator at a value lowering cannot know, so it lowers to a `BinAppend` onto whatever precedes it, with no carve-out: `x[0x48, b]` is one append rather than a two-operand concatenation, `x[..acc, b]` is the append it spells out, and adjacent atoms chain. Leading a literal it appends onto the empty packed value — the singleton spelling `curios_elab`'s packed-match refinement builds for a cons scrutinee, so `b[h, ..t]` meets a refined motive without unfolding anything.
    pub(super) fn lower_bin_literal(
        grain: Grain,
        segments: &[BinSegment],
        mut lower: impl FnMut(&Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Intrinsic, Error> {
        let packed = |run: Vec<u8>| match grain {
            Grain::B => PackedBin::from_bits(run.into_iter().map(|atom| atom != 0)),
            Grain::X => PackedBin::from_bytes(run),
        };
        let flush = |operands: &mut Vec<curios_core::Term>, run: &mut Vec<u8>| {
            if !run.is_empty() {
                let value = packed(std::mem::take(run));
                operands.push(curios_core::Term::intrinsic(curios_core::Intrinsic::Bin(
                    grain, value,
                )));
            }
        };

        let mut operands: Vec<curios_core::Term> = Vec::new();
        let mut run: Vec<u8> = Vec::new();
        for segment in segments {
            match segment {
                BinSegment::Atom(term) => {
                    if let Some(atom) = Self::bin_constant_atom(grain, term) {
                        run.push(atom);
                        continue;
                    }
                    flush(&mut operands, &mut run);
                    let base = operands.pop().unwrap_or_else(|| {
                        curios_core::Term::intrinsic(curios_core::Intrinsic::Bin(
                            grain,
                            PackedBin::empty(),
                        ))
                    });
                    let atom = lower(term)?;
                    operands.push(curios_core::Term::intrinsic(
                        curios_core::Intrinsic::bin_append(grain, base, atom),
                    ));
                }
                BinSegment::Spread(term) => {
                    flush(&mut operands, &mut run);
                    operands.push(lower(term)?);
                }
            }
        }

        // A literal of constants alone — the empty literal included — is the packed value itself.
        if operands.is_empty() {
            return Ok(curios_core::Intrinsic::Bin(grain, packed(run)));
        }
        flush(&mut operands, &mut run);

        // A lone packed-shaped operand at this literal's own grain is the value itself; wrapping it in a concatenation only leaves reduction something to normalise away. Only that family may collapse: any other lone operand keeps its wrapper, which is what makes elaboration check a spread (`x[..b]`) against the packed type instead of adopting the operand's own — `x[..true]` once collapsed to `true`, and a bits value spread into a bytes literal adopted the wrong grain.
        if operands.len() == 1
            && let curios_core::Subterm::Intrinsic(intrinsic) = &*operands[0]
            && matches!(
                intrinsic,
                curios_core::Intrinsic::Bin(g, _)
                | curios_core::Intrinsic::BinAppend { grain: g, .. }
                | curios_core::Intrinsic::BinConcat { grain: g, .. }
                if *g == grain
            )
        {
            return Ok(intrinsic.clone());
        }

        Ok(curios_core::Intrinsic::BinConcat { grain, operands })
    }

    pub(super) fn intrinsic(&self, intrinsic: &Intrinsic) -> Result<curios_core::Intrinsic, Error> {
        Ok(match intrinsic {
            Intrinsic::BoolType => curios_core::Intrinsic::BoolType,
            Intrinsic::Bool(b) => curios_core::Intrinsic::Bool(*b),
            Intrinsic::BoolAnd(left, right) => {
                curios_core::Intrinsic::BoolAnd(self.term(left)?, self.term(right)?)
            }
            Intrinsic::BoolOr(left, right) => {
                curios_core::Intrinsic::BoolOr(self.term(left)?, self.term(right)?)
            }
            Intrinsic::BoolXor(left, right) => {
                curios_core::Intrinsic::BoolXor(self.term(left)?, self.term(right)?)
            }
            Intrinsic::BoolEql(left, right) => {
                curios_core::Intrinsic::BoolEql(self.term(left)?, self.term(right)?)
            }
            Intrinsic::BoolNeq(left, right) => {
                curios_core::Intrinsic::BoolNeq(self.term(left)?, self.term(right)?)
            }
            Intrinsic::NatType => curios_core::Intrinsic::NatType,
            Intrinsic::Nat(Nat::Zero) => curios_core::Intrinsic::Nat(curios_core::Nat::Zero),
            Intrinsic::Nat(Nat::Succ(NatLiteral(spine, _), inner)) => curios_core::Intrinsic::Nat(
                curios_core::Nat::Succ(spine.clone(), self.term(inner)?),
            ),
            Intrinsic::ByteType => curios_core::Intrinsic::ByteType,
            Intrinsic::Byte(value) => curios_core::Intrinsic::Byte(*value),
            Intrinsic::ByteToNat(inner) => curios_core::Intrinsic::ByteToNat(self.term(inner)?),
            Intrinsic::NatToByte(inner) => curios_core::Intrinsic::NatToByte(self.term(inner)?),
            Intrinsic::ByteEql(left, right) => {
                curios_core::Intrinsic::ByteEql(self.term(left)?, self.term(right)?)
            }
            Intrinsic::ByteLt(left, right) => {
                curios_core::Intrinsic::ByteLt(self.term(left)?, self.term(right)?)
            }
            Intrinsic::ByteLe(left, right) => {
                curios_core::Intrinsic::ByteLe(self.term(left)?, self.term(right)?)
            }
            Intrinsic::ByteGt(left, right) => {
                curios_core::Intrinsic::ByteGt(self.term(left)?, self.term(right)?)
            }
            Intrinsic::ByteGe(left, right) => {
                curios_core::Intrinsic::ByteGe(self.term(left)?, self.term(right)?)
            }
            Intrinsic::NatEql(left, right) => {
                curios_core::Intrinsic::nat_eql(self.term(left)?, self.term(right)?)
            }
            Intrinsic::NatNeq(left, right) => {
                curios_core::Intrinsic::nat_neq(self.term(left)?, self.term(right)?)
            }
            Intrinsic::NatAdd(left, right) => {
                curios_core::Intrinsic::nat_add(self.term(left)?, self.term(right)?)
            }
            Intrinsic::NatSub(left, right) => {
                curios_core::Intrinsic::nat_sub(self.term(left)?, self.term(right)?)
            }
            Intrinsic::NatMul(left, right) => {
                curios_core::Intrinsic::nat_mul(self.term(left)?, self.term(right)?)
            }
            Intrinsic::NatLt(left, right) => {
                curios_core::Intrinsic::nat_lt(self.term(left)?, self.term(right)?)
            }
            Intrinsic::NatDiv {
                dividend,
                divisor,
                non_zero,
            } => curios_core::Intrinsic::nat_div(
                self.term(dividend)?,
                self.term(divisor)?,
                self.term(non_zero)?,
            ),
            Intrinsic::NatRem {
                dividend,
                divisor,
                non_zero,
            } => curios_core::Intrinsic::nat_rem(
                self.term(dividend)?,
                self.term(divisor)?,
                self.term(non_zero)?,
            ),
            Intrinsic::NatGt(left, right) => {
                curios_core::Intrinsic::nat_gt(self.term(left)?, self.term(right)?)
            }
            Intrinsic::NatLe(left, right) => {
                curios_core::Intrinsic::nat_lte(self.term(left)?, self.term(right)?)
            }
            Intrinsic::NatGe(left, right) => {
                curios_core::Intrinsic::nat_gte(self.term(left)?, self.term(right)?)
            }
            Intrinsic::NatAnd(left, right) => {
                curios_core::Intrinsic::NatAnd(self.term(left)?, self.term(right)?)
            }
            Intrinsic::NatOr(left, right) => {
                curios_core::Intrinsic::NatOr(self.term(left)?, self.term(right)?)
            }
            Intrinsic::NatXor(left, right) => {
                curios_core::Intrinsic::NatXor(self.term(left)?, self.term(right)?)
            }
            Intrinsic::NatShl(left, right) => {
                curios_core::Intrinsic::NatShl(self.term(left)?, self.term(right)?)
            }
            Intrinsic::NatShr(left, right) => {
                curios_core::Intrinsic::NatShr(self.term(left)?, self.term(right)?)
            }
            Intrinsic::IntType => curios_core::Intrinsic::IntType,
            Intrinsic::Int(value) => curios_core::Intrinsic::Int(value.clone()),
            Intrinsic::IntEql(left, right) => {
                curios_core::Intrinsic::int_eql(self.term(left)?, self.term(right)?)
            }
            Intrinsic::IntNeq(left, right) => {
                curios_core::Intrinsic::int_neq(self.term(left)?, self.term(right)?)
            }
            Intrinsic::IntAdd(left, right) => {
                curios_core::Intrinsic::int_add(self.term(left)?, self.term(right)?)
            }
            Intrinsic::IntSub(left, right) => {
                curios_core::Intrinsic::int_sub(self.term(left)?, self.term(right)?)
            }
            Intrinsic::IntMul(left, right) => {
                curios_core::Intrinsic::int_mul(self.term(left)?, self.term(right)?)
            }
            Intrinsic::IntDiv {
                dividend,
                divisor,
                non_zero,
            } => curios_core::Intrinsic::int_div(
                self.term(dividend)?,
                self.term(divisor)?,
                self.term(non_zero)?,
            ),
            Intrinsic::IntRem {
                dividend,
                divisor,
                non_zero,
            } => curios_core::Intrinsic::int_rem(
                self.term(dividend)?,
                self.term(divisor)?,
                self.term(non_zero)?,
            ),
            Intrinsic::IntLt(left, right) => {
                curios_core::Intrinsic::int_lt(self.term(left)?, self.term(right)?)
            }
            Intrinsic::IntGt(left, right) => {
                curios_core::Intrinsic::int_gt(self.term(left)?, self.term(right)?)
            }
            Intrinsic::IntLe(left, right) => {
                curios_core::Intrinsic::int_lte(self.term(left)?, self.term(right)?)
            }
            Intrinsic::IntGe(left, right) => {
                curios_core::Intrinsic::int_gte(self.term(left)?, self.term(right)?)
            }
            Intrinsic::IntAnd(left, right) => {
                curios_core::Intrinsic::IntAnd(self.term(left)?, self.term(right)?)
            }
            Intrinsic::IntOr(left, right) => {
                curios_core::Intrinsic::IntOr(self.term(left)?, self.term(right)?)
            }
            Intrinsic::IntXor(left, right) => {
                curios_core::Intrinsic::IntXor(self.term(left)?, self.term(right)?)
            }
            Intrinsic::IntShl(left, right) => {
                curios_core::Intrinsic::IntShl(self.term(left)?, self.term(right)?)
            }
            Intrinsic::IntShr(left, right) => {
                curios_core::Intrinsic::IntShr(self.term(left)?, self.term(right)?)
            }
            Intrinsic::FltType => curios_core::Intrinsic::FltType,
            Intrinsic::Flt(flt) => curios_core::Intrinsic::Flt(*flt),
            Intrinsic::FltAdd(left, right) => {
                curios_core::Intrinsic::flt_add(self.term(left)?, self.term(right)?)
            }
            Intrinsic::FltSub(left, right) => {
                curios_core::Intrinsic::flt_sub(self.term(left)?, self.term(right)?)
            }
            Intrinsic::FltMul(left, right) => {
                curios_core::Intrinsic::flt_mul(self.term(left)?, self.term(right)?)
            }
            Intrinsic::FltDiv(left, right) => {
                curios_core::Intrinsic::flt_div(self.term(left)?, self.term(right)?)
            }
            Intrinsic::FltRem(left, right) => {
                curios_core::Intrinsic::FltRem(self.term(left)?, self.term(right)?)
            }
            Intrinsic::FltEql(left, right) => {
                curios_core::Intrinsic::flt_eql(self.term(left)?, self.term(right)?)
            }
            Intrinsic::FltNeq(left, right) => {
                curios_core::Intrinsic::flt_neq(self.term(left)?, self.term(right)?)
            }
            Intrinsic::FltLt(left, right) => {
                curios_core::Intrinsic::flt_lt(self.term(left)?, self.term(right)?)
            }
            Intrinsic::FltGt(left, right) => {
                curios_core::Intrinsic::flt_gt(self.term(left)?, self.term(right)?)
            }
            Intrinsic::FltLe(left, right) => {
                curios_core::Intrinsic::flt_lte(self.term(left)?, self.term(right)?)
            }
            Intrinsic::FltGe(left, right) => {
                curios_core::Intrinsic::flt_gte(self.term(left)?, self.term(right)?)
            }
            Intrinsic::FltMin(left, right) => {
                curios_core::Intrinsic::flt_min(self.term(left)?, self.term(right)?)
            }
            Intrinsic::FltMax(left, right) => {
                curios_core::Intrinsic::flt_max(self.term(left)?, self.term(right)?)
            }
            Intrinsic::FltNeg(inner) => curios_core::Intrinsic::flt_neg(self.term(inner)?),
            Intrinsic::FltAbs(inner) => curios_core::Intrinsic::flt_abs(self.term(inner)?),
            Intrinsic::FltCopysign(left, right) => {
                curios_core::Intrinsic::FltCopysign(self.term(left)?, self.term(right)?)
            }
            Intrinsic::FltSqrt(inner) => curios_core::Intrinsic::flt_sqrt(self.term(inner)?),
            Intrinsic::FltFloor(inner) => curios_core::Intrinsic::flt_floor(self.term(inner)?),
            Intrinsic::FltCeil(inner) => curios_core::Intrinsic::flt_ceil(self.term(inner)?),
            Intrinsic::FltTrunc(inner) => curios_core::Intrinsic::flt_trunc(self.term(inner)?),
            Intrinsic::FltNearest(inner) => curios_core::Intrinsic::flt_nearest(self.term(inner)?),
            Intrinsic::FltToLeBytes(inner) => {
                curios_core::Intrinsic::flt_to_le_bytes(self.term(inner)?)
            }
            Intrinsic::FltOfLeBytes { bin, four_bytes } => {
                curios_core::Intrinsic::flt_of_le_bytes(self.term(bin)?, self.term(four_bytes)?)
            }
            Intrinsic::NatToInt(inner) => curios_core::Intrinsic::nat_to_int(self.term(inner)?),
            Intrinsic::HandleType => curios_core::Intrinsic::HandleType,
            Intrinsic::Handle(token) => curios_core::Intrinsic::Handle(*token),
            Intrinsic::HandleEql(left, right) => {
                curios_core::Intrinsic::handle_eql(self.term(left)?, self.term(right)?)
            }
            Intrinsic::ProcExit(code) => curios_core::Intrinsic::ProcExit(self.term(code)?),
            Intrinsic::NatToFlt(inner) => curios_core::Intrinsic::nat_to_flt(self.term(inner)?),
            Intrinsic::IntToNat { int, non_neg } => {
                curios_core::Intrinsic::int_to_nat(self.term(int)?, self.term(non_neg)?)
            }
            Intrinsic::IntToFlt(inner) => curios_core::Intrinsic::int_to_flt(self.term(inner)?),
            Intrinsic::FltToNat { flt, non_neg } => {
                curios_core::Intrinsic::flt_to_nat(self.term(flt)?, self.term(non_neg)?)
            }
            Intrinsic::FltToInt { flt, finite } => {
                curios_core::Intrinsic::flt_to_int(self.term(flt)?, self.term(finite)?)
            }
            Intrinsic::BinType(grain) => curios_core::Intrinsic::BinType(*grain),
            // `\hex` is a raw byte sequence; `\..` segments splice other `Bin`s.
            Intrinsic::Bin(grain, segments) => {
                Self::lower_bin_literal(*grain, segments, |term| self.term(term))?
            }
            Intrinsic::BinLen(grain, inner) => {
                curios_core::Intrinsic::bin_len(*grain, self.term(inner)?)
            }
            Intrinsic::BinEql(grain, left, right) => {
                curios_core::Intrinsic::bin_eql(*grain, self.term(left)?, self.term(right)?)
            }
            Intrinsic::BinGet {
                grain,
                bin,
                index,
                in_range,
            } => curios_core::Intrinsic::bin_get(
                *grain,
                self.term(bin)?,
                self.term(index)?,
                self.term(in_range)?,
            ),
            Intrinsic::BinSlice {
                grain,
                bin,
                start,
                length,
                within,
            } => curios_core::Intrinsic::bin_slice(
                *grain,
                self.term(bin)?,
                self.term(start)?,
                self.term(length)?,
                self.term(within)?,
            ),
            Intrinsic::BinAppend {
                grain,
                bin,
                element: atom,
            } => curios_core::Intrinsic::bin_append(*grain, self.term(bin)?, self.term(atom)?),
            Intrinsic::BinConcat { grain, left, right } => {
                curios_core::Intrinsic::bin_concat(*grain, [self.term(left)?, self.term(right)?])
            }
            Intrinsic::ListType(inner) => curios_core::Intrinsic::list_type(self.term(inner)?),
            Intrinsic::List(entries) => self.lower_list_literal(entries, |term| self.term(term))?,
            Intrinsic::ListLen {
                element: ty,
                list: inner,
            } => curios_core::Intrinsic::list_len(self.term(ty)?, self.term(inner)?),
            Intrinsic::ListGet {
                element: ty,
                list,
                index,
                in_range,
            } => curios_core::Intrinsic::list_get(
                self.term(ty)?,
                self.term(list)?,
                self.term(index)?,
                self.term(in_range)?,
            ),
            Intrinsic::ListSlice {
                element: ty,
                list,
                start,
                length,
                within,
            } => curios_core::Intrinsic::list_slice(
                self.term(ty)?,
                self.term(list)?,
                self.term(start)?,
                self.term(length)?,
                self.term(within)?,
            ),
            Intrinsic::ListAppend {
                element: ty,
                list,
                item: elem,
            } => curios_core::Intrinsic::list_append(
                self.term(ty)?,
                self.term(list)?,
                self.term(elem)?,
            ),
            Intrinsic::ListConcat {
                element: ty,
                left,
                right,
            } => curios_core::Intrinsic::list_concat(
                self.term(ty)?,
                [self.term(left)?, self.term(right)?],
            ),
            Intrinsic::ListMap {
                from: a,
                to: b,
                list,
                function: f,
            } => curios_core::Intrinsic::list_map(
                self.term(a)?,
                self.term(b)?,
                self.term(list)?,
                self.term(f)?,
            ),
            Intrinsic::CellType(inner) => curios_core::Intrinsic::cell_type(self.term(inner)?),
            Intrinsic::Cell {
                element: type_,
                initial: init,
            } => curios_core::Intrinsic::cell_new(self.term(type_)?, self.term(init)?),
            Intrinsic::CellSet {
                element: type_,
                cell,
                value,
            } => curios_core::Intrinsic::cell_set(
                self.term(type_)?,
                self.term(cell)?,
                self.term(value)?,
            ),
            Intrinsic::CellGet {
                element: type_,
                cell,
            } => curios_core::Intrinsic::cell_get(self.term(type_)?, self.term(cell)?),
            Intrinsic::IoType(result) => curios_core::Intrinsic::io_type(self.term(result)?),
            Intrinsic::IoPure {
                result: type_,
                value,
            } => curios_core::Intrinsic::io_pure(self.term(type_)?, self.term(value)?),
            Intrinsic::IoBind {
                from,
                to,
                action,
                continuation: f,
            } => curios_core::Intrinsic::io_bind(
                self.term(from)?,
                self.term(to)?,
                self.term(action)?,
                self.term(f)?,
            ),
        })
    }
}

/// Whether a written binder name can be referred to. `_` and the empty label occupy a binder position but name nothing.
fn bindable(name: &str) -> bool {
    !(name.is_empty() || name == "_")
}

/// The binder names a parameter list introduces — every leaf binder name in each parameter's pattern, flattened, all in scope across the body. These shadow like-named module bindings; the wildcard `_` rides along but is ignored by [`Lowerer::bound`].
fn param_names(params: &[FuncParam]) -> Vec<String> {
    params
        .iter()
        .flat_map(|param| pattern_names(&param.pattern))
        .collect()
}

/// Every `Pattern::Binder` leaf name in `pattern`, recursing through nested tuple/struct fields in field order.
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
