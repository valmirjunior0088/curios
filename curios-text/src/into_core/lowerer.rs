use curios_elab::{IntrinsicBuilders, TermBuilders};
use {
    super::{Context, MatchCompiler},
    crate::{
        BinSegment, Choose, ChooseTest, Error, Field, FuncParam, FuncTypeParam, Intrinsic, Label,
        Let, LetBinding, LetGroup, LetSignature, Lint, ListEntry, Name, Nat, NatLiteral, NumLit,
        Pattern, PatternField, StructLitEntry, Subterm, Syn, Term,
    },
    curios_utilities::{Grain, PackedBin, Plicity, Qualifier, Span, recurse},
    std::{
        cell::{Cell, RefCell},
        collections::HashSet,
        sync::Arc,
    },
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
    /// The enclosing local binders (function and `let` binders, match-arm patterns, motive labels), innermost last. A bare reference whose spelling appears here resolves to the innermost such binder rather than a like-named module binding — see [`Self::resolve_name`]. Compiler-minted binders are never registered: nothing can write their name.
    scope: RefCell<Vec<Binder>>,
    /// Every written binder a reference could reach, with where it was written — the `unused-binder` lint's candidates, decided by [`Self::flush`] once the whole declaration is lowered, since a goal anywhere in it or a mention in its declared type is decided after the binder's scope closes.
    candidates: RefCell<Vec<Candidate>>,
    /// The binder identities some reference resolved to.
    used: RefCell<HashSet<curios_core::Free>>,
    /// Whether a written `?` was lowered: a declaration holding one is exempt, its binders being the goal's scope.
    saw_goal: Cell<bool>,
    /// The function-definition sugar being lowered, when one is — see [`Self::enter_signature`].
    signature: Cell<Option<usize>>,
    signatures: RefCell<Vec<Signature>>,
}

/// One written binder the lint may report: its spelling, its identity, its span, and the signature whose outermost lambda bound it, when one did.
struct Candidate {
    name: String,
    id: curios_core::Free,
    span: Span,
    signature: Option<usize>,
}

/// One function-definition sugar `f(params) -> output = body`. Its telescope is lowered twice, as the Π-type's binders and as the body lambda's, with distinct identities; a parameter the result type mentions is used by the declaration whatever the body does, so the names the output mentions exempt the lambda's parameters of that spelling.
#[derive(Default)]
struct Signature {
    output_mentions: HashSet<String>,
    telescope_seen: bool,
    lambda_seen: bool,
}

impl<'a, 'b> Lowerer<'a, 'b> {
    pub(super) fn new(context: &'a Context<'b>) -> Self {
        Self {
            context,
            scope: RefCell::new(Vec::new()),
            candidates: RefCell::new(Vec::new()),
            used: RefCell::new(HashSet::new()),
            saw_goal: Cell::new(false),
            signature: Cell::new(None),
            signatures: RefCell::new(Vec::new()),
        }
    }

    /// Lower a declaration's type and body as the function-definition sugar they came from, when they did: the first Π-type met is its telescope and the first lambda its parameters, and the two are read together by [`Self::flush`]. A plain `let x : T = e` enters nothing. Returns what was current, for a local `let` to restore.
    pub(super) fn enter_signature(&self, signature: &LetSignature) -> Option<usize> {
        match signature {
            LetSignature::Func { .. } => self.enter_sugar(),
            LetSignature::Name { .. } => self.signature.replace(None),
        }
    }

    pub(super) fn enter_sugar(&self) -> Option<usize> {
        let mut signatures = self.signatures.borrow_mut();
        signatures.push(Signature::default());
        self.signature.replace(Some(signatures.len() - 1))
    }

    pub(super) fn leave_signature(&self, previous: Option<usize>) {
        self.signature.set(previous);
    }

    /// The lints of the declaration this lowered, decided now that every binder's scope has closed and every mention has been seen. A declaration holding a written goal reports none: its binders are what the goal's report lists for the author to use next.
    fn flush(&self) {
        if self.saw_goal.get() {
            return;
        }
        let used = self.used.borrow();
        let signatures = self.signatures.borrow();
        let lints = self
            .candidates
            .borrow()
            .iter()
            .filter(|candidate| !used.contains(&candidate.id))
            .filter(|candidate| {
                !candidate.signature.is_some_and(|signature| {
                    signatures[signature]
                        .output_mentions
                        .contains(&candidate.name)
                })
            })
            .map(|candidate| Lint::unused_binder(&candidate.name, &candidate.span))
            .collect::<Vec<_>>();
        self.context.report(lints);
    }

    /// Mint one binder identity per written name, in order. Nothing is brought into scope: the caller decides where each binder is visible, and the identity it holds is the one every such region re-enters.
    ///
    /// An unwritten (`_` or empty) name still gets an identity — it occupies a binder position — but no reference can reach it. A binder minted here is never linted: it is a declaration telescope's, or a Π-type's, which a reference in the declaration reads as part of its type.
    pub(super) fn mint(&self, names: impl IntoIterator<Item = String>) -> Vec<Binder> {
        names
            .into_iter()
            .map(|name| {
                let id = self.context.fresh_binder(bindable(&name).then_some(&name));
                (name, id)
            })
            .collect()
    }

    /// [`Self::mint`] for written binders: one that carries a span, can be referred to and is not `_`-prefixed becomes a lint candidate. `signature` is the sugar whose outermost lambda these are, when they are.
    fn mint_written(
        &self,
        labels: impl IntoIterator<Item = (String, Option<Span>)>,
        signature: Option<usize>,
    ) -> Vec<Binder> {
        labels
            .into_iter()
            .map(|(name, span)| {
                let id = self.context.fresh_binder(bindable(&name).then_some(&name));
                if let Some(span) = span
                    && bindable(&name)
                    && !name.starts_with('_')
                {
                    self.candidates.borrow_mut().push(Candidate {
                        name: name.clone(),
                        id: id.clone(),
                        span,
                        signature,
                    });
                }
                (name, id)
            })
            .collect()
    }

    /// The binders of a lambda's parameters. The first lambda lowered under a signature is its sugar's, and its parameters are decided together with the telescope — see [`Signature`].
    fn mint_params(&self, params: &[FuncParam]) -> Vec<Binder> {
        let signature = self.signature.get().filter(|&signature| {
            let mut signatures = self.signatures.borrow_mut();
            !std::mem::replace(&mut signatures[signature].lambda_seen, true)
        });
        self.mint_written(param_labels(params), signature)
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

    /// Lowers a *value* body — a top-level `let` body, a witness field, or the entrypoint tail. Every value body is a region root: each `!` in it hoists here (never past a boundary — a lambda body, match arm, or recursive-group member re-roots) and is rewired through `/syn/Monad/bind`, whose `use` binder resolves the `Monad` witness per site. Types go through [`Self::term`], where `!` is rejected.
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

    /// A dependent Π-type over `params`: each parameter type sees the *preceding* parameters' binders and the output sees them all, so they lower under a progressively-extended scope. The output is produced by the caller under those binders rather than lowered from a written term, so a declaration whose result the compiler fixes — a `test`'s `/syn/Test` — closes an already-resolved core term under the written telescope instead of spelling a surface name it may not be able to import.
    pub(super) fn func_type_under(
        &self,
        params: &[FuncTypeParam],
        output: impl FnOnce() -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Term, Error> {
        let binders = self.mint(params.iter().map(|p| p.label.clone().unwrap_or_default()));
        let mut lowered = Vec::with_capacity(params.len());
        for (index, param) in params.iter().enumerate() {
            let domain = self.bound(&binders[..index], || self.input_type(&param.type_))?;
            lowered.push((param.plicity, binders[index].1.clone(), domain));
        }
        let output = self.bound(&binders, output)?;
        // The first Π-type lowered under a signature is its sugar's telescope: the parameters its result mentions are used by the declaration — see [`Signature`].
        if let Some(signature) = self.signature.get() {
            let mut signatures = self.signatures.borrow_mut();
            let signature = &mut signatures[signature];
            if !std::mem::replace(&mut signature.telescope_seen, true) {
                let mentioned = output.free_vars_shared();
                signature.output_mentions.extend(
                    binders
                        .iter()
                        .filter(|(_, id)| mentioned.contains(id))
                        .map(|(name, _)| name.clone()),
                );
            }
        }
        Ok(curios_core::Term::func_type_marked(lowered, output))
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
            self.used.borrow_mut().insert(id.clone());
            return Ok(id.clone());
        }
        match self.context.bindings().get(name.head()) {
            Some(full) => {
                self.context.note_binding_use(name.head());
                Ok(curios_core::Free::global(full.clone()))
            }
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
    // The two remedies an earlier version of this note deferred are settled by that machine rather than taken. A native scan intrinsic is refused (see `documentation/design/toolchain/evaluating-a-closed-term-is-representation-not-judgment.md`) — it would bless one type's fold where the machine accelerates every closed fold on the same terms, `Str`'s and a user's alike. And full reflection — restating `Valid` as an equation on `scan_from` and rewriting `/std/Str/utf8`'s lemmas as fold algebra — stays unnecessary, because the derivation this bridge preserves now costs what the machine prices it rather than a frame per link.
    pub(super) fn str_literal(&self, bytes: &[u8]) -> curios_core::Term {
        curios_elab::str_literal(&self.context.syntax().string, bytes)
    }

    // The `Utf8(state, bytes)` derivation. `state` is carried as a *symbolic* term — `lead()` at the top, then `step(c, state)` per byte — so each recursive `rest`'s expected index (`Utf8(step(c, state), tail)`) is definitionally the state we thread in, with no metavar/`step`-inversion. The final `stop : Utf8(lead, x[])` matches because `step` of the last byte reduces back to `lead` for valid UTF-8 (a string literal is valid UTF-8 by construction). A `/syn` literal — its value is synthesized from `/syn` by the meta-emitter rather than lowered to a core intrinsic.
    pub(super) fn syn_literal(&self, syn: &Syn) -> Result<curios_core::Term, Error> {
        match syn {
            // A character literal is a polymorphic literal like a numeral: elaboration realizes it — `/syn/Char` by default, a numeric carrier where one is expected — so the certified value is built there, not here.
            Syn::Char(character) => Ok(curios_core::Term::num_lit_char(*character)),
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
            Subterm::Hole => curios_core::Term::hole(self.context.fresh_metavar()),
            // A written goal `?`: same fresh metavariable, but marked so zonk reports what elaboration determined for it instead of splicing.
            Subterm::Goal => {
                self.saw_goal.set(true);
                curios_core::Term::goal(self.context.fresh_metavar())
            }
            Subterm::Derive => curios_core::Term::derive(),
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
            Subterm::NumLit(num_lit) => {
                curios_core::Term::num_lit(num_lit.magnitude.clone(), num_lit.sign)
            }
            Subterm::Infix(infix) => curios_core::Term::infix(
                infix.op,
                self.term(&infix.left)?,
                self.term(&infix.right)?,
            ),
            Subterm::Name(name) => {
                curios_core::Term::var(curios_core::Var::free(self.resolve_name(name)?))
            }
            Subterm::FuncType(ft) => self.func_type_under(&ft.params, || self.term(&ft.output))?,
            Subterm::Func(func) => {
                let binders = self.mint_params(&func.params);
                let body = self.bound(&binders, || self.term(&func.body))?;
                let (params, body) = self.lower_func_params(&func.params, &binders, body)?;
                curios_core::Term::func_marked(params, body)
            }
            Subterm::Apply(apply) => curios_core::Term::apply_marked(
                self.term(&apply.head)?,
                apply
                    .arguments
                    .iter()
                    .map(|argument| Ok((argument.plicity, self.term(&argument.term)?)))
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
                            curios_core::Term::hole(self.context.fresh_metavar()),
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
            // A `let` block: each statement is in scope for the statements after it and the tail, and for itself — a lone binding that names itself, or a `let … and …;` group, lowers to a core `rec`; see [`Self::lower_let_group`].
            Subterm::Let(let_) => self.lower_let(&let_.groups, &let_.tail)?,
            // A bang here was reached through a *type* lowering (an annotation, a motive, a Π/Σ component): types have no region to hoist to. Value bodies enter through `value`/`region`, which eliminates every `Bang` before this arm could see it.
            Subterm::Bang(_) => return Err(Error::BangInTypePosition),
        })
    }

    /// An error at `term`'s span, where `term` has one and the error is not already placed — the counterpart of the stamping `region` and `collect` perform on the term they return. Only [`Self::term`] placed its errors, so a refusal the matrix compiler raised while lowering a `match` or `choose` in a value body — every arm-shape error it has — reached the reader with no location at all.
    fn located(error: Error, term: &Term) -> Error {
        match term.span() {
            Some(span) => error.at(span.clone()),
            None => error,
        }
    }

    /// Desugars `term` as a single **region**. A region is a stretch of a value body that shares one continuation; each `!` in it hoists to the top of the region, never past a boundary (lambda body, match arm, recursive-group member). Boundaries re-root a region. Every hoisted action is sequenced through `/syn/Monad/bind` — see `wrap`.
    ///
    /// Span stamping happens here for the reason [`Self::collect`] states, and for the arms that are not spines: `Let`, `Match`, `Choose` and `Func` each *rebuild* their node below, so a value body rooted at a whole-term form reached elaboration with no span — its errors unlocated, and the `test` declaration's recorded body empty, since the runner slices that body from this very span. `with_span` is innermost-wins, so the spine arm keeps the one [`Self::collect`] already stamped.
    pub(super) fn region(&self, term: &Term) -> Result<curios_core::Term, Error> {
        let lowered = self
            .region_root(term)
            .map_err(|error| Self::located(error, term))?;

        Ok(match term.span() {
            Some(span) => lowered.with_span(span.clone()),
            None => lowered,
        })
    }

    fn region_root(&self, term: &Term) -> Result<curios_core::Term, Error> {
        match term.as_subterm() {
            // A `let`'s bound expression evaluates in place (its bangs hoist to this region); the tail continues the same region (a bang there hoists after `x` is bound, not above the `let`).
            Subterm::Let(let_) => self.lower_let_region(&let_.groups, &let_.tail),
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
                let binders = self.mint_params(&func.params);
                let body = self.bound(&binders, || self.region(&func.body))?;
                let (params, body) = self.lower_func_params(&func.params, &binders, body)?;
                Ok(curios_core::Term::func_marked(params, body))
            }
            // Spine forms (atomic / apply / tuple / proj): collect bangs in left-to-right evaluation order, then wrap.
            _ => {
                let mut binds = Vec::new();
                let body = self.collect(term, &mut binds)?;
                self.wrap(binds, body)
            }
        }
    }

    /// Lowers a run of `let` statements and their `tail` as one region: each statement's bound expressions are values in this region (their bangs hoist here, sequenced by `wrap`), the tail continues the same region. Loops over `groups` rather than recursing once per statement — a `let` block is flat, so a long straight-line sequence costs one loop, not a stack of native frames. Shared by `region`'s `Let` arm (the whole block) and `build_let` (the statements after the first, whose own bangs hoist to the enclosing region instead).
    pub(super) fn lower_let_region(
        &self,
        groups: &[LetGroup],
        tail: &Term,
    ) -> Result<curios_core::Term, Error> {
        recurse(|| {
            let Some((first, rest)) = groups.split_first() else {
                return self.region(tail);
            };

            let (binds, let_term) = self.lower_let_group(
                first,
                |term, binds| self.collect(term, binds),
                || self.lower_let_region(rest, tail),
            )?;

            self.wrap(binds, let_term)
        })
    }

    /// [`Self::lower_let_region`] for a `let` reached through [`Self::term`] — a type, an annotation, a motive — where there is no region to hoist a bang into, so a binding is a plain value and nothing wraps.
    fn lower_let(&self, groups: &[LetGroup], tail: &Term) -> Result<curios_core::Term, Error> {
        recurse(|| {
            let Some((first, rest)) = groups.split_first() else {
                return self.term(tail);
            };

            let (_, let_term) = self.lower_let_group(
                first,
                |term, _| self.term(term),
                || self.lower_let(rest, tail),
            )?;

            Ok(let_term)
        })
    }

    /// Lowers one `let` statement — a lone binding or a `let … and …;` group — around `inner`, the lowering of what follows it in the statement's scope. `lower_value` lowers a member's value in that scope: `collect`, hoisting bangs into the binds this hands back, or `term` where there is no region to hoist into.
    ///
    /// A statement is recursive when it has more than one member or when a member's type or value mentions a member — read off the lowered terms, never declared. It then becomes a core `rec`, whose member bodies are their own regions (hoisting an action out of a recursive binding would change how often it runs), and every member must be a plain, typed name whose value hoists nothing: a pattern binds no name its own value could use, a type cannot be inferred from a body that mentions it, and an action cannot name the result it is still producing. Each is refused by name. Otherwise the statement is the plain `let` it always was, its pattern desugared by [`Self::bind_pattern`].
    ///
    /// The binders are minted before any member is lowered — the whole point, since that is what puts a binding in scope of its own value — so a `let n = n + 1` names the binding it declares rather than an outer `n`, and is refused as the recursive value it now is.
    fn lower_let_group(
        &self,
        group: &LetGroup,
        lower_value: impl Fn(&Term, &mut Vec<Hoisted>) -> Result<curios_core::Term, Error>,
        inner: impl FnOnce() -> Result<curios_core::Term, Error>,
    ) -> Result<(Vec<Hoisted>, curios_core::Term), Error> {
        let binders = self.mint_written(
            group
                .members
                .iter()
                .flat_map(|member| pattern_labels(&member.binder)),
            None,
        );

        let mut binds = Vec::new();
        let (types, values) = self.bound(&binders, || {
            let mut types = Vec::with_capacity(group.members.len());
            let mut values = Vec::with_capacity(group.members.len());
            for member in &group.members {
                let enclosing = self.enter_signature(&member.signature);
                values.push(lower_value(&member.signature.body(), &mut binds)?);
                types.push(self.term(&member.signature.type_())?);
                self.leave_signature(enclosing);
            }
            Ok((types, values))
        })?;

        // The hoisted actions count: a `!` lifts its operand out of the value, and the self-reference travels with it.
        let mentioned = binders.iter().any(|(_, id)| {
            types
                .iter()
                .chain(&values)
                .chain(binds.iter().map(|hoisted| &hoisted.action))
                .any(|term| term.free_vars_shared().contains(id))
        });
        if group.members.len() == 1 && !mentioned {
            let member = &group.members[0];
            let (type_, value) = (types[0].clone(), values[0].clone());
            let tail = self.bound(&binders, inner)?;
            let let_term = self.bind_pattern(&member.binder, &binders, type_, value, tail);
            return Ok((binds, let_term));
        }

        let located = |error: Error, member: &LetBinding| match member.signature.body().span() {
            Some(span) => error.at(span.clone()),
            None => error,
        };
        for member in &group.members {
            let Pattern::Binder(Some(label)) = &member.binder else {
                return Err(located(Error::RecursivePatternBinding, member));
            };
            if matches!(member.signature, LetSignature::Name { type_: None, .. }) {
                return Err(located(
                    Error::RecursiveBindingNeedsType {
                        label: label.to_string(),
                    },
                    member,
                ));
            }
            if !binds.is_empty() {
                return Err(located(
                    Error::RecursiveBangBinding {
                        label: label.to_string(),
                    },
                    member,
                ));
            }
        }

        let tail = self.bound(&binders, inner)?;
        let members = binders
            .iter()
            .zip(types)
            .zip(values)
            .map(|(((_, id), type_), value)| (id.clone(), type_, value));

        Ok((Vec::new(), curios_core::Term::rec(members, tail)))
    }

    /// Walks a non-boundary expression, elaborating to core and accumulating each `Bang` into `binds` (in evaluation order) replaced by a fresh variable. Boundary/binding forms desugar as their own nested region; `let`/`match` hoist their bound-expression/scrutinee bangs into the *enclosing* `binds`.
    ///
    /// Span stamping happens here rather than inside the walk for the same reason [`Self::term`] stamps at its own boundary: every spine arm *rebuilds* its node (`apply_marked`, `proj`, `infix`, …) instead of routing through `term`, so a node lowered in a value body would otherwise reach elaboration with no span and report its errors unlocated. `with_span` is innermost-wins, so the arms that do delegate — leaves through `term`, lambdas through `region` — keep the span they already carry.
    pub(super) fn collect(
        &self,
        term: &Term,
        binds: &mut Vec<Hoisted>,
    ) -> Result<curios_core::Term, Error> {
        let lowered = self
            .collect_spine(term, binds)
            .map_err(|error| Self::located(error, term))?;
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
                    .arguments
                    .iter()
                    .map(|argument| Ok((argument.plicity, self.collect(&argument.term, binds)?)))
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
            // A lambda is a value and hoists nothing outward, so it desugars as its own region.
            Subterm::Func(_) => self.region(term)?,
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
            .groups
            .split_first()
            .expect("a `let` block has at least one statement");

        let (hoisted, let_term) = self.lower_let_group(
            first,
            |term, binds| self.collect(term, binds),
            || self.lower_let_region(rest, &let_.tail),
        )?;
        binds.extend(hoisted);
        Ok(let_term)
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
                None => curios_core::Term::hole(self.context.fresh_metavar()),
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
    pub(super) fn pattern_binder(&self, name: &Label) -> Binder {
        self.mint_written([(name.to_string(), name.span().cloned())], None)
            .remove(0)
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
            let hole = curios_core::Term::hole(self.context.fresh_metavar());
            tail = self.bind_pattern(&field.value, taken, hole, proj, tail);
        }
        tail
    }

    /// A `Nat` succ or `List`/`Bin` cons arm's induction-hypothesis binder: an omitted `; ih` (`None` — there is no source name at all) mints an unwritten binder; a written one is minted with its spelling as the hint.
    pub(super) fn cons_ih_binder(&self, ih_label: &Option<Label>) -> Binder {
        match ih_label {
            Some(name) => self.pattern_binder(name),
            None => (String::new(), self.context.fresh_binder(None)),
        }
    }

    /// Wraps `body` in one [`curios_core::Bang`] transient per collected bang. The first-collected bang (`binds[0]`) becomes the outermost node, preserving left-to-right evaluation order. Continuation lambdas are built with `curios_core::Term::func` over the gensym'd free name, whose `capture` closes it robustly under nesting; the domain is a fresh hole, inference-solved. `elaborate_bang` later replaces each node with its `/syn/Monad/bind` application, handing the wrapper the region's monad as its `@M` (read off the region's type by the flex-apply imitation rule) and inserting fresh implicits and a fresh `use` witness slot per `!` site: the region pins the constructor, which resolves the `Monad` witness, and every action is checked against it — so a region can sequence actions of differing result types, and different regions can use different monads.
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
            let domain = curios_core::Term::hole(self.context.fresh_metavar());
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
        let element = || curios_core::Term::hole(self.context.fresh_metavar());

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
        let element = || curios_core::Term::hole(self.context.fresh_metavar());

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
                    magnitude, sign, ..
                }),
            ) => match sign.is_marked() {
                true => None,
                false => magnitude.to_u8().filter(|bit| *bit <= 1),
            },
            (Grain::X, Subterm::Intrinsic(Intrinsic::Byte(byte))) => Some(*byte),
            (
                Grain::X,
                Subterm::NumLit(NumLit {
                    magnitude, sign, ..
                }),
            ) => match sign.is_marked() {
                true => None,
                false => magnitude.to_u8(),
            },
            // A character-spelled atom folds as its code point when it fits the byte; past that it stays an atom term and elaboration refuses the range exactly as for a numeral.
            (Grain::X, Subterm::Syn(Syn::Char(character))) => u8::try_from(*character as u32).ok(),
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
            Intrinsic::NatLe(left, right) => {
                curios_core::Intrinsic::nat_lte(self.term(left)?, self.term(right)?)
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
            Intrinsic::IntLe(left, right) => {
                curios_core::Intrinsic::int_lte(self.term(left)?, self.term(right)?)
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
            Intrinsic::FltLe(left, right) => {
                curios_core::Intrinsic::flt_lte(self.term(left)?, self.term(right)?)
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
            Intrinsic::ProcExit { result, code } => {
                curios_core::Intrinsic::proc_exit(self.term(result)?, self.term(code)?)
            }
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

/// A lowerer is one declaration's, and the declaration is done when its lowerer is dropped — which is where the decision every candidate waited on is made, so no lowering site can forget to make it. A lowering that failed reports too, harmlessly: the unit it belonged to reports its error and nothing else.
impl Drop for Lowerer<'_, '_> {
    fn drop(&mut self) {
        self.flush();
    }
}

/// Whether a written binder name can be referred to. `_` and the empty label occupy a binder position but name nothing.
fn bindable(name: &str) -> bool {
    !(name.is_empty() || name == "_")
}

/// The binder names a parameter list introduces, each with where it was written — every leaf binder in each parameter's pattern, flattened, all in scope across the body. These shadow like-named module bindings; the wildcard `_` rides along but is ignored by [`Lowerer::bound`].
fn param_labels(params: &[FuncParam]) -> Vec<(String, Option<Span>)> {
    params
        .iter()
        .flat_map(|param| pattern_labels(&param.pattern))
        .collect()
}

/// Every `Pattern::Binder` leaf in `pattern` with its span, recursing through nested tuple/struct fields in field order.
fn pattern_labels(pattern: &Pattern) -> Vec<(String, Option<Span>)> {
    match pattern {
        Pattern::Binder(Some(name)) => vec![(name.to_string(), name.span().cloned())],
        Pattern::Binder(None) => vec![],
        Pattern::Tuple(fields) | Pattern::Struct { fields, .. } => fields
            .iter()
            .flat_map(|field| pattern_labels(&field.value))
            .collect(),
    }
}

/// Every `Pattern::Binder` leaf name in `pattern`, recursing through nested tuple/struct fields in field order.
fn pattern_names(pattern: &Pattern) -> Vec<String> {
    match pattern {
        Pattern::Binder(Some(name)) => vec![name.to_string()],
        // No source name at all — nothing to shadow-track.
        Pattern::Binder(None) => vec![],
        Pattern::Tuple(fields) | Pattern::Struct { fields, .. } => fields
            .iter()
            .flat_map(|field| pattern_names(&field.value))
            .collect(),
    }
}
