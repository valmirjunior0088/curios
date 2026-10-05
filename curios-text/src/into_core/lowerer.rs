use {
    super::{Context, MatchCompiler},
    crate::{
        Apply, BinSegment, Choose, ChooseTest, Error, Field, FuncParam, FuncTypeParam, Intrinsic,
        Label, Let, LetBinding, LetGroup, LetSignature, Lint, LintedBinder, ListEntry, Name, Nat,
        NatLiteral, NumLit, Pattern, PatternField, ProofLiteral, StructLit, StructLitEntry,
        Subterm, Term, func_sugar_params, func_sugar_type_params,
    },
    curios_num::{Binary, Grain},
    curios_utilities::{Plicity, Span, recurse},
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
/// The span is what [`Lowerer::wrap`] stamps onto the synthesized `/std/Monad/bind` application. Without it the sequencing is the one node in a lowered value body that no source location reaches, so a `!` the region cannot accept — an annotated top-level `let`, a non-monadic helper — reports its type error with no `-->` line at all.
pub(super) struct Hoisted {
    pub(super) binder: curios_core::Free,
    pub(super) action: curios_core::Term,
    pub(super) span: Option<Span>,
}

pub(super) struct Lowerer<'a, 'b> {
    pub(super) context: &'a Context<'b>,
    /// The declaration this lowers, which a lint names its binders by: the one identity a later lowering of the declaration unchanged gives them too. `None` for an entry's final term.
    declaration: Option<curios_core::Global>,
    /// The enclosing local binders (function and `let` binders, match-arm patterns, motive labels), innermost last. A bare reference whose spelling appears here resolves to the innermost such binder rather than a like-named module binding — see [`Self::resolve_name`]. Compiler-minted binders are never registered: nothing can write their name.
    scope: RefCell<Vec<Binder>>,
    /// Every written binder a reference could reach, with where it was written — the `unused-binder` lint's candidates, decided by [`Self::flush`] once the whole declaration is lowered, since a goal anywhere in it or a mention in its declared type is decided after the binder's scope closes.
    candidates: RefCell<Vec<Candidate>>,
    /// The binder identities some reference resolved to.
    used: RefCell<HashSet<curios_core::Free>>,
    /// Whether a written `?` was lowered: a declaration holding one is exempt, its binders being the goal's scope.
    saw_goal: Cell<bool>,
}

/// One written binder the lint may report: its spelling, its identity and its span.
struct Candidate {
    name: String,
    id: curios_core::Free,
    span: Span,
}

/// A `let`-like signature as the lowering holds it: what was written, and the binders of its parameters when it is the function-definition sugar `f(params) -> output = body`.
///
/// The sugar's telescope becomes both a Π-type's binders and the body lambda's, and the leaves are minted once, here, so a parameter is one identity in both. A mention in the result type is then a use of the binder the body binds, with nothing to pair afterwards, and the Π's binder carries the written place the lambda's does, so a proof the elaborator writes in the type credits the same binder.
pub(super) struct Signature<'s> {
    written: &'s LetSignature,
    leaves: Vec<Binder>,
}

impl<'a, 'b> Lowerer<'a, 'b> {
    pub(super) fn new(context: &'a Context<'b>, declaration: Option<curios_core::Global>) -> Self {
        Self {
            context,
            declaration,
            scope: RefCell::new(Vec::new()),
            candidates: RefCell::new(Vec::new()),
            used: RefCell::new(HashSet::new()),
            saw_goal: Cell::new(false),
        }
    }

    /// `written` with its parameters minted, when it is the function-definition sugar: what [`Self::signature_type`] and [`Self::signature_value`] lower, in whichever order the site needs.
    pub(super) fn signature<'s>(&self, written: &'s LetSignature) -> Signature<'s> {
        let leaves = match written {
            LetSignature::Func { params, .. } => {
                self.mint_written(param_labels(&func_sugar_params(params)))
            }
            LetSignature::Name { .. } => Vec::new(),
        };
        Signature { written, leaves }
    }

    /// The type a signature declares, under `declared` where the site spans it. The sugar's is the Π-type over its parameters: a plain name is the binder its lambda binds, while a compound pattern has no one name to give, so its slot is anonymous and its leaves are out of every domain's and the output's scope.
    pub(super) fn signature_type(
        &self,
        signature: &Signature,
        declared: Option<&Span>,
    ) -> Result<curios_core::Term, Error> {
        let LetSignature::Func { params, output, .. } = signature.written else {
            return self.term(&signature.written.type_());
        };
        // Read off the lambda the sugar's body is, whose patterns are the ones `signature` minted its leaves from: a member with no binder is that lambda's `_`, one leaf nothing can name.
        let mut seen = 0;
        let binders = func_sugar_params(params)
            .iter()
            .map(|param| {
                let leaves = pattern_names(&param.pattern).len();
                let binder = match &param.pattern {
                    Pattern::Binder(_) => signature.leaves[seen].clone(),
                    Pattern::Tuple(_) | Pattern::Struct { .. } => {
                        (String::new(), self.context.fresh_binder(None))
                    }
                };
                seen += leaves;
                binder
            })
            .collect::<Vec<_>>();
        let lowered = self.func_type_over(&func_sugar_type_params(params), &binders, || {
            self.term(output)
        });
        match declared {
            Some(span) => lowered
                .map(|type_| curios_core::Term::spanned(span.clone(), type_))
                .map_err(|error| error.at(span.clone())),
            None => lowered,
        }
    }

    /// The value a signature binds: the sugar's is the lambda over its parameters, and a plain one is its body, lowered as `lower_value` lowers a value at the site.
    pub(super) fn signature_value(
        &self,
        signature: &Signature,
        lower_value: impl FnOnce(&Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Term, Error> {
        match signature.written {
            LetSignature::Func { params, body, .. } => {
                self.lambda(&func_sugar_params(params), &signature.leaves, body)
            }
            LetSignature::Name { body, .. } => lower_value(body),
        }
    }

    /// The lints of the declaration this lowered, decided now that every binder's scope has closed and every mention has been seen. A declaration holding a written goal reports none: its binders are what the goal's report lists for the author to use next.
    fn flush(&self) {
        if self.saw_goal.get() {
            return;
        }
        let used = self.used.borrow();
        let lints = self
            .candidates
            .borrow()
            .iter()
            .enumerate()
            .filter(|(_, candidate)| !used.contains(&candidate.id))
            .map(|(ordinal, candidate)| {
                let binder = LintedBinder {
                    declaration: self.declaration,
                    ordinal: u32::try_from(ordinal).expect("a declaration's binders fit a u32"),
                };
                Lint::unused_binder(&candidate.name, &candidate.span, binder)
            })
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

    /// [`Self::mint`] for written binders: one that carries a span, can be referred to and is not `_`-prefixed becomes a lint candidate.
    fn mint_written(
        &self,
        labels: impl IntoIterator<Item = (String, Option<Span>)>,
    ) -> Vec<Binder> {
        labels
            .into_iter()
            .map(|(name, span)| {
                let hint = bindable(&name).then_some(name.as_str());
                let candidate = span.filter(|_| bindable(&name) && !name.starts_with('_'));
                let Some(span) = candidate else {
                    let id = self.context.fresh_binder(hint);
                    return (name, id);
                };
                // A candidate is minted with its place among the declaration's candidates, which every local opened from it inherits: how a read no written name makes is traced back to the lint it answers.
                let mut candidates = self.candidates.borrow_mut();
                let written =
                    u32::try_from(candidates.len()).expect("a declaration's binders fit a u32");
                let id = self.context.fresh_written_binder(hint, written);
                candidates.push(Candidate {
                    name: name.clone(),
                    id,
                    span,
                });
                (name, id)
            })
            .collect()
    }

    /// The binders of a written lambda's parameters.
    fn mint_params(&self, params: &[FuncParam]) -> Vec<Binder> {
        self.mint_written(param_labels(params))
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

    /// Lowers a *value* body — a top-level `let` body, a witness field, or the entrypoint tail. Every value body is a region root: each `!` in it hoists here (never past a boundary — a lambda body, match arm, or recursive-group member re-roots) and is rewired through `/std/Monad/bind`, whose `use` binder resolves the `Monad` witness per site. Types go through [`Self::term`], where `!` is rejected.
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

    /// A dependent Π-type over `params`: each parameter type sees the *preceding* parameters' binders and the output sees them all, so they lower under a progressively-extended scope. The output is produced by the caller under those binders rather than lowered from a written term, so a declaration whose result the compiler fixes — a `test`'s `/std/Test` — closes an already-resolved core term under the written telescope instead of spelling a surface name it may not be able to import.
    pub(super) fn func_type_under(
        &self,
        params: &[FuncTypeParam],
        output: impl FnOnce() -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Term, Error> {
        let binders = self.mint(params.iter().map(|p| p.label.clone().unwrap_or_default()));
        self.func_type_over(params, &binders, output)
    }

    /// [`Self::func_type_under`] over binders already minted, one per parameter.
    fn func_type_over(
        &self,
        params: &[FuncTypeParam],
        binders: &[Binder],
        output: impl FnOnce() -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Term, Error> {
        let mut lowered = Vec::with_capacity(params.len());
        for (index, param) in params.iter().enumerate() {
            let domain = self.bound(&binders[..index], || self.input_type(&param.type_))?;
            lowered.push((param.plicity, binders[index].1, domain));
        }
        let output = self.bound(binders, output)?;
        Ok(curios_core::Term::func_type_marked(lowered, output))
    }

    /// A lambda over `params`, whose leaves' binders are `binders`: its body is a region of its own.
    fn lambda(
        &self,
        params: &[FuncParam],
        binders: &[Binder],
        body: &Term,
    ) -> Result<curios_core::Term, Error> {
        let body = self.bound(binders, || self.region(body))?;
        let (params, body) = self.lower_func_params(params, binders, body)?;
        Ok(curios_core::Term::func_marked(params, body))
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
            self.used.borrow_mut().insert(*id);
            return Ok(*id);
        }
        match self.context.bindings().get(name.head()) {
            Some(full) => {
                self.context.note_binding_use(name.head());
                Ok(curios_core::Free::global(*full))
            }
            // Unresolved, and `curios-elab` is what reports it — so this must lower to something no definition can ever be. A binder identity is unbound by construction (nothing closes over it) and carries the written name as its hint, so the diagnostic still names it; what this stage adds beside it is what the name could have meant, which only this stage can say.
            //
            // A root-level global would *not* do: `Qualifier::from([head])` is exactly what an entry-module `let helper` lowers to, so an unresolvable reference in a nested module would silently capture it.
            None => Ok(self.context.unbound_binder(name.head())),
        }
    }

    // The meta-emitter: a string literal becomes a proof-carrying `/std/Str/Str` value `Str { bytes = <Bytes>, valid = True/qed() }`. `valid` is erased, so at runtime `Str` collapses to its `Bytes` field — a literal costs exactly what a `Bytes` literal does.
    //
    // # Why the proof is one constant
    //
    // `/std/Str/Valid` is a decided proposition: reading the bytes from the first one lands back between characters. So the only inhabitant it has is `True/qed()`, which checks by *running* the scan over the literal rather than by traversing a derivation. An inductive family whose canonical inhabitant is one link per byte would make the *term* linear in the data, and elaboration, zonking, both erasure obligations, the printer and the kernel's typing judgment would all inherit it. With `Valid` decided, the compiler has nothing to know about how the library proves a literal valid.
    //
    // # What bounds a literal
    //
    // Reduction of the scan is linear in the literal's length, and it runs on `curios-core`'s closed machine — the explicit-stack evaluator both checkers enter for closed terms — so a character costs transitions and machine frames rather than a native reduction level, and guarded depth is flat in the length. No figure is quoted here; `curios`' `str_literal_cost_measurements` carries the per-character price and the ceiling, and `a_str_literal_costs_transitions_rather_than_frames` is the ordinary assertion that holds the shape. A native scan intrinsic is refused (see `documentation/design/soundness/an-independent-kernel-re-checks-what-the-elaborator-accepts.md`): it would bless one type's fold where the machine accelerates every closed fold on the same terms, `Str`'s and a user's alike.
    pub(super) fn str_literal(&self, bytes: &[u8]) -> curios_core::Term {
        curios_core::str_literal(&self.context.syntax().string, bytes)
    }

    // A registry-synthesized literal — its value is synthesized from the registry by the meta-emitter rather than lowered to a core intrinsic.
    pub(super) fn proof_literal(&self, literal: &ProofLiteral) -> Result<curios_core::Term, Error> {
        match literal {
            // A character literal is a polymorphic literal like a numeral: elaboration realizes it — `/std/Char` by default, a numeric carrier where one is expected — so the certified value is built there, not here.
            ProofLiteral::Char(character) => Ok(curios_core::Term::num_lit_char(*character)),
            ProofLiteral::Str(string) => Ok(self.str_literal(string.value.as_bytes())),
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
            // A registry-synthesized literal (string or list) desugars via the meta-emitter to a proof-carrying construction (see `proof_literal`), never a core intrinsic.
            Subterm::ProofLiteral(literal) => self.proof_literal(literal)?,
            Subterm::Intrinsic(intrinsic) => {
                curios_core::Term::intrinsic(self.intrinsic(intrinsic)?)
            }
            Subterm::Foreign(function, args) => curios_core::Term::foreign(
                Arc::clone(function),
                args.iter()
                    .map(|arg| self.term(arg))
                    .collect::<Result<_, _>>()?,
            ),
            // The row as the function of its operands: one binder for each, with no name, in the row's order. A row that takes none is the call itself, a description being a constant already.
            Subterm::ForeignRow(function) => {
                let params = &function.signature().params;
                let binders = self.mint(params.iter().map(|_| String::new()));
                let call = curios_core::Term::foreign(
                    Arc::clone(function),
                    binders
                        .iter()
                        .map(|(_, binder)| curios_core::Term::free_var(binder))
                        .collect(),
                );

                match binders.is_empty() {
                    true => call,
                    false => curios_core::Term::func(
                        binders.iter().zip(params).map(|((_, binder), wire_type)| {
                            (*binder, curios_core::wire_term(wire_type))
                        }),
                        call,
                    ),
                }
            }
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
                    .map(
                        |argument| match self.placeholder(argument.plicity, &argument.term) {
                            Some(hole) => Ok((argument.plicity, hole)),
                            None => Ok((argument.plicity, self.term(&argument.term)?)),
                        },
                    )
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            // A dependent Σ-type: each field type sees the preceding fields' labels, so they lower under a progressively-extended scope. The signature sugar `f(params) -> T` is undone here.
            Subterm::TupleType(tt) => {
                let binders = self.mint(
                    tt.fields
                        .iter()
                        .map(|f| f.label.as_deref().unwrap_or_default().to_string()),
                );
                let mut fields = Vec::with_capacity(tt.fields.len());
                for (index, param) in tt.fields.iter().enumerate() {
                    let type_ = param.desugared_type();
                    let lowered = self.bound(&binders[..index], || self.term(&type_))?;
                    fields.push((binders[index].1, lowered));
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
                    Field::Index(index) => curios_core::Term::proj_position(head, *index),
                    Field::Label(label) => curios_core::Term::proj_label(head, label.clone()),
                }
            }
            // A struct literal lowers to a `curios_core::Struct` carrying the resolved (qualified) struct name, no parameters (core elaboration mints metavariables for them), and the written entries — plain field values with their names (validated positionally and dropped by elaborate), `use <term>` fills for a concept's `use`-marked positions, and a `..base` spread carrying its base — stated at its head's type where the head is applied ([`Self::headed`]). Construction privacy and spread shape are enforced in core (`elaborate_struct`), alongside projection privacy.
            Subterm::StructLit(lit) => self.headed(
                lit,
                curios_core::Term::struct_entries(
                    self.resolve_nominal(&lit.head)?,
                    Vec::<curios_core::Term>::new(),
                    lit.entries
                        .iter()
                        .map(|entry| match entry {
                            StructLitEntry::Field(field) => Ok((
                                curios_core::StructEntry::Field(field.label.clone()),
                                self.term(&field.desugared_value())?,
                            )),
                            StructLitEntry::Implicit(field) => {
                                let value = field.desugared_value();
                                Ok((
                                    curios_core::StructEntry::Implicit(field.label.clone()),
                                    match self.placeholder(Plicity::Implicit, &value) {
                                        Some(hole) => hole,
                                        None => self.term(&value)?,
                                    },
                                ))
                            }
                            StructLitEntry::Use(term) => Ok((
                                curios_core::StructEntry::Use,
                                match self.placeholder(Plicity::Witness, term) {
                                    Some(hole) => hole,
                                    None => self.term(term)?,
                                },
                            )),
                            StructLitEntry::Spread(term) => {
                                Ok((curios_core::StructEntry::Spread, self.term(term)?))
                            }
                        })
                        .collect::<Result<Vec<_>, Error>>()?,
                ),
            )?,
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

    /// An error at `term`'s span, where `term` has one and the error is not already placed — the counterpart of the stamping `region` and `collect` perform on the term they return. Without it only [`Self::term`] would place its errors, and a refusal the matrix compiler raises while lowering a `match` or `choose` in a value body — every arm-shape error it has — would reach the reader with no location at all.
    fn located(error: Error, term: &Term) -> Error {
        match term.span() {
            Some(span) => error.at(span.clone()),
            None => error,
        }
    }

    /// Desugars `term` as a single **region**. A region is a stretch of a value body that shares one continuation; each `!` in it hoists to the top of the region, never past a boundary (lambda body, match arm, recursive-group member). Boundaries re-root a region. Every hoisted action is sequenced through `/std/Monad/bind` — see `wrap`.
    ///
    /// Span stamping happens here for the reason [`Self::collect`] states, and for the arms that are not spines: `Let`, `Match`, `Choose` and `Func` each *rebuild* their node below, so unstamped, a value body rooted at a whole-term form would reach elaboration with no span — its errors unlocated, and the `test` declaration's recorded body empty, since the runner slices that body from this very span. `with_span` is innermost-wins, so the spine arm keeps the one [`Self::collect`] already stamped.
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
                self.lambda(&func.params, &self.mint_params(&func.params), &func.body)
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
    /// A statement is recursive when it has more than one member or when a member's type or value mentions a member — read off the lowered terms, never declared. It then becomes a core `rec`, whose member bodies are their own regions (hoisting an action out of a recursive binding would change how often it runs), and every member must be a plain, typed name whose value hoists nothing: a pattern binds no name its own value could use, a type cannot be inferred from a body that mentions it, and an action cannot name the result it is still producing. Each is refused by name. Otherwise the statement is a plain `let`, its pattern desugared by [`Self::bind_pattern`].
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
        );

        let mut binds = Vec::new();
        let (types, values) = self.bound(&binders, || {
            let mut types = Vec::with_capacity(group.members.len());
            let mut values = Vec::with_capacity(group.members.len());
            for member in &group.members {
                let signature = self.signature(&member.signature);
                values
                    .push(self.signature_value(&signature, |body| lower_value(body, &mut binds))?);
                types.push(self.signature_type(&signature, None)?);
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
            let Pattern::Binder(label) = &member.binder else {
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
            .map(|(((_, id), type_), value)| (*id, type_, value));

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
                let var = curios_core::Term::var(curios_core::Var::free(binder));
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
                    .map(
                        |argument| match self.placeholder(argument.plicity, &argument.term) {
                            Some(hole) => Ok((argument.plicity, hole)),
                            None => Ok((argument.plicity, self.collect(&argument.term, binds)?)),
                        },
                    )
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
                    Field::Index(index) => curios_core::Term::proj_position(head, *index),
                    Field::Label(label) => curios_core::Term::proj_label(head, label.clone()),
                }
            }
            // A struct literal's entry values hoist their bangs into this region, exactly like a tuple's fields. Its head is a type, which has no region to hoist into.
            Subterm::StructLit(lit) => self.headed(
                lit,
                curios_core::Term::struct_entries(
                    self.resolve_nominal(&lit.head)?,
                    Vec::<curios_core::Term>::new(),
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
                            StructLitEntry::Implicit(field) => {
                                let value = field.desugared_value();
                                Ok((
                                    curios_core::StructEntry::Implicit(field.label.clone()),
                                    match self.placeholder(Plicity::Implicit, &value) {
                                        Some(hole) => hole,
                                        None => self.collect(&value, binds)?,
                                    },
                                ))
                            }
                            StructLitEntry::Use(term) => Ok((
                                curios_core::StructEntry::Use,
                                match self.placeholder(Plicity::Witness, term) {
                                    Some(hole) => hole,
                                    None => self.collect(term, binds)?,
                                },
                            )),
                            StructLitEntry::Spread(term) => {
                                Ok((curios_core::StructEntry::Spread, self.collect(term, binds)?))
                            }
                        })
                        .collect::<Result<Vec<_>, Error>>()?,
                ),
            )?,
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
    /// Each annotation sees the *preceding* parameters' binders, exactly as a dependent Π-type's domains do (the `Subterm::FuncType` arm), so a lambda may be written `(s, t, q : Eq()(s, t)) => …`. That is why the walk runs in declaration order under a progressively-extended scope: `Telescope::build` captures each earlier binder in every later domain, so the core side needs nothing further. A compound pattern binds no leaf name at the core binder — its leaves are projections off the synthetic binder — so a later annotation naming one of those leaves gets that pattern's field-`let` chain wrapped around the *domain* as well, mirroring what the body gets.
    ///
    /// The chains wrap body and domains alike in reverse, so each pattern's chain wraps *before* an earlier pattern's chain wraps that, giving declaration-order nesting.
    pub(super) fn lower_func_params(
        &self,
        params: &[FuncParam],
        binders: &[Binder],
        body: curios_core::Term,
    ) -> Result<(LoweredParams, curios_core::Term), Error> {
        let mut lowered = Vec::with_capacity(params.len());
        // The binders already minted for the leaves, consumed in the same pre-order `pattern_names` produced them, plus the field-`let` chains that put the compound patterns in scope — both advance with the walk.
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
                Pattern::Binder(_) => lowered.push((*plicity, leaves[0].1, domain)),
                Pattern::Tuple(fields) | Pattern::Struct { fields, .. } => {
                    let synthetic = self.context.fresh_binder(None);
                    chains.push((fields, synthetic, leaves));
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
        self.mint_written([(name.to_string(), name.span().cloned())])
            .remove(0)
    }

    /// The silent hole a hidden member written `_` lowers to, where a value is supplied: `@_` and `use _` hold a slot's place and say nothing, and elaboration fills the slot as one left out is. A plain `_` is not one — a plain member is always written — and stays the name it is, unbound.
    fn placeholder(&self, plicity: Plicity, term: &Term) -> Option<curios_core::Term> {
        let Subterm::Name(name) = term.as_subterm() else {
            return None;
        };
        let written = plicity != Plicity::Explicit && name.is_single() && name.head() == "_";

        written.then(|| {
            let hole = curios_core::Term::hole(self.context.fresh_metavar());
            match term.span() {
                Some(span) => curios_core::Term::spanned(span.clone(), hole),
                None => hole,
            }
        })
    }

    /// A lowered struct literal under the head it was written with. A bare head leaves the parameters to elaboration. An applied one is the type former applied as any call applies it — marks, omitted hidden arguments and `?` holes included — so it lowers as that application and the literal is stated at it, rather than the literal carrying a second argument list with rules of its own.
    fn headed(
        &self,
        lit: &StructLit,
        literal: curios_core::Term,
    ) -> Result<curios_core::Term, Error> {
        if lit.params.is_empty() {
            return Ok(literal);
        }

        let former: Term = Subterm::Name(lit.head.clone()).into();
        let applied: Term = Subterm::Apply(Apply {
            head: former,
            arguments: lit.params.clone(),
        })
        .into();

        Ok(curios_core::Term::ascribed(literal, self.term(&applied)?))
    }

    /// A nominal head's resolved name. Only a global declares a structure, so a head resolving to a local or to nothing is refused here, where the bindings it could have meant are known — lowering it to the root-level global its spelling names would let it capture an entry module's binding of that name, as [`Self::resolve_name`] records for a bare reference.
    pub(super) fn resolve_nominal(&self, name: &Name) -> Result<curios_core::Global, Error> {
        match self.resolve_name(name)?.as_global() {
            Some(global) => Ok(*global),
            None => Err(Error::UnresolvedNominal {
                name: name.head().to_string(),
                candidates: self.context.binding_candidates(name.head()),
            }),
        }
    }

    /// Builds `let pat = value : type_; tail` for a pattern in any of the three binder positions: `Pattern::Binder` is a single core `let_` call — the plain-name path is a zero-cost passthrough. A compound pattern mints one fresh synthetic binder (via [`Context::fresh_binder`]) carrying `type_` (the caller's own annotation, so it is still checked), then projects each field off it via [`Self::lower_pattern_fields`]. The synthetic binder is minted unconditionally, even when `value` is already a bare variable reference: reusing it directly would risk silently dropping `type_`'s check (e.g. `let (x, y) : Point = pair;` must still check `pair : Point`). The extra trivial `let` this occasionally emits is exactly the shape `cont`'s copy-threading optimization already collapses, so it costs nothing at runtime. `binders` are the identities minted for this pattern's written leaves, in `pattern_names` order — the same ones the scope this `let` opened was entered with, so the tail's references land on them.
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
            Pattern::Binder(_) => {
                let (_, id) = binders.next().expect("one mint per written leaf");
                curios_core::Term::let_(id, type_, value, tail)
            }
            Pattern::Tuple(fields) | Pattern::Struct { fields, .. } => {
                let synthetic = self.context.fresh_binder(None);
                let inner = self.lower_pattern_fields_from(fields, &synthetic, binders, tail);
                curios_core::Term::let_(&synthetic, type_, value, inner)
            }
        }
    }

    /// Projects each field of a compound pattern off the (already-bound) core variable `scrutinee_name`, in field order — folded right-to-left so the first field's `let` ends up outermost, matching the order a person would hand-write (`let x = p0.0; let y = p0.1; …`) — recursing into [`Self::bind_pattern`] for nested patterns. Each field's own type is a fresh metavar hole: there is never a per-field annotation to give, exactly like a hand-written `let x = p.0;`. Each projection is located at its field's pattern ([`pattern_span`]), so a report about reading the field — a head whose type never became a tuple — points at the destructuring rather than at whatever encloses it. The chain over a compound pattern already in scope: its leaves' minted identities are pulled from the surrounding walk, which produced them in this very order.
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
            let scrutinee = curios_core::Term::var(curios_core::Var::free(*scrutinee_name));
            let proj = match &field.label {
                Some(label) => curios_core::Term::proj_label(scrutinee, label.clone()),
                None => curios_core::Term::proj_position(scrutinee, index),
            };
            let proj = match pattern_span(&field.value) {
                Some(span) => proj.with_span(span),
                None => proj,
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

    /// Wraps `body` in one [`curios_core::Bang`] transient per collected bang. The first-collected bang (`binds[0]`) becomes the outermost node, preserving left-to-right evaluation order. Continuation lambdas are built with `curios_core::Term::func` over the gensym'd free name, whose `capture` closes it robustly under nesting; the domain is a fresh hole, inference-solved. `elaborate_bang` later replaces each node with its `/std/Monad/bind` application, handing the wrapper the region's monad as its `@M` (read off the region's type by the flex-apply imitation rule) and inserting fresh implicits and a fresh `use` witness slot per `!` site: the region pins the constructor, which resolves the `Monad` witness, and every action is checked against it — so a region can sequence actions of differing result types, and different regions can use different monads.
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

    /// Lowers a list literal's entries. A spread-free literal lowers to a plain `List`, `[]` included. With spreads, elements join the literal through [`Self::flush_list_run`] and the whole becomes an n-ary `ListConcat`; its element-type slot is a fresh metavar (an implicit the literal cannot name), solved by elaboration — bidirectionally from the expected type when checking (see the `ListConcat` case in `curios_elab`'s `elaborate_intrinsic`). `lower` is the per-term lowering — [`Self::term`] on the plain path, the bang-collector on the region path — so both share this grouping.
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
            // A lone list-shaped operand is the value itself; the concatenation would only be normalised away. Only the family the literal builds may collapse: any other lone operand keeps its wrapper, which is what makes elaboration check a spread (`[..b]`) against a list type instead of adopting the operand's own — `[..true]` would collapse to `true` and typecheck as `Bool`.
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
                false => u8::try_from(magnitude).ok().filter(|bit| *bit <= 1),
            },
            (Grain::X, Subterm::Intrinsic(Intrinsic::Byte(byte))) => Some(*byte),
            (
                Grain::X,
                Subterm::NumLit(NumLit {
                    magnitude, sign, ..
                }),
            ) => match sign.is_marked() {
                true => None,
                false => u8::try_from(magnitude).ok(),
            },
            // A character-spelled atom folds as its code point when it fits the byte; past that it stays an atom term and elaboration refuses the range exactly as for a numeral.
            (Grain::X, Subterm::ProofLiteral(ProofLiteral::Char(character))) => {
                u8::try_from(*character as u32).ok()
            }
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
            Grain::B => Binary::from_bits(run.into_iter().map(|atom| atom != 0)),
            Grain::X => Binary::from_bytes(run),
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
                            Binary::empty(),
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

        // A lone packed-shaped operand at this literal's own grain is the value itself; wrapping it in a concatenation only leaves reduction something to normalise away. Only that family may collapse: any other lone operand keeps its wrapper, which is what makes elaboration check a spread (`x[..b]`) against the packed type instead of adopting the operand's own — `x[..true]` would collapse to `true`, and a bits value spread into a bytes literal would adopt the wrong grain.
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
            Intrinsic::NatToByte { nat, below } => {
                curios_core::Intrinsic::nat_to_byte(self.term(nat)?, self.term(below)?)
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
            Intrinsic::FltAdd(rounding, left, right) => {
                curios_core::Intrinsic::flt_add(*rounding, self.term(left)?, self.term(right)?)
            }
            Intrinsic::FltSub(rounding, left, right) => {
                curios_core::Intrinsic::flt_sub(*rounding, self.term(left)?, self.term(right)?)
            }
            Intrinsic::FltMul(rounding, left, right) => {
                curios_core::Intrinsic::flt_mul(*rounding, self.term(left)?, self.term(right)?)
            }
            Intrinsic::FltDiv(rounding, left, right) => {
                curios_core::Intrinsic::flt_div(*rounding, self.term(left)?, self.term(right)?)
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
            Intrinsic::FltFma(rounding, a, b, c) => curios_core::Intrinsic::flt_fma(
                *rounding,
                self.term(a)?,
                self.term(b)?,
                self.term(c)?,
            ),
            Intrinsic::FltSqrt(rounding, inner) => {
                curios_core::Intrinsic::flt_sqrt(*rounding, self.term(inner)?)
            }
            Intrinsic::FltRoundIntegral(rounding, inner) => {
                curios_core::Intrinsic::flt_round_integral(*rounding, self.term(inner)?)
            }
            Intrinsic::FltToLeBytes(inner) => {
                curios_core::Intrinsic::flt_to_le_bytes(self.term(inner)?)
            }
            Intrinsic::FltOfLeBytes { bin, eight_bytes } => {
                curios_core::Intrinsic::flt_of_le_bytes(self.term(bin)?, self.term(eight_bytes)?)
            }
            Intrinsic::NatToInt(inner) => curios_core::Intrinsic::nat_to_int(self.term(inner)?),
            Intrinsic::HandleType => curios_core::Intrinsic::HandleType,
            Intrinsic::Handle(token) => curios_core::Intrinsic::Handle(*token),
            Intrinsic::NatToFlt(rounding, inner) => {
                curios_core::Intrinsic::nat_to_flt(*rounding, self.term(inner)?)
            }
            Intrinsic::IntToNat { int, non_neg } => {
                curios_core::Intrinsic::int_to_nat(self.term(int)?, self.term(non_neg)?)
            }
            Intrinsic::IntToFlt(rounding, inner) => {
                curios_core::Intrinsic::int_to_flt(*rounding, self.term(inner)?)
            }
            Intrinsic::FltToNat { flt, non_neg } => {
                curios_core::Intrinsic::flt_to_nat(self.term(flt)?, self.term(non_neg)?)
            }
            Intrinsic::FltToInt { flt, finite } => {
                curios_core::Intrinsic::flt_to_int(self.term(flt)?, self.term(finite)?)
            }
            Intrinsic::FltMantissa { flt, finite } => {
                curios_core::Intrinsic::flt_mantissa(self.term(flt)?, self.term(finite)?)
            }
            Intrinsic::FltExponent { flt, finite } => {
                curios_core::Intrinsic::flt_exponent(self.term(flt)?, self.term(finite)?)
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
            Intrinsic::BinReplicate { grain, count, atom } => {
                curios_core::Intrinsic::bin_replicate(*grain, self.term(count)?, self.term(atom)?)
            }
            Intrinsic::BinReinterp {
                grain,
                bin,
                aligned,
            } => curios_core::Intrinsic::bin_reinterp(*grain, self.term(bin)?, self.term(aligned)?),
            Intrinsic::BinAnd {
                grain,
                left,
                right,
                same_length,
            } => curios_core::Intrinsic::bin_and(
                *grain,
                self.term(left)?,
                self.term(right)?,
                self.term(same_length)?,
            ),
            Intrinsic::BinOr {
                grain,
                left,
                right,
                same_length,
            } => curios_core::Intrinsic::bin_or(
                *grain,
                self.term(left)?,
                self.term(right)?,
                self.term(same_length)?,
            ),
            Intrinsic::BinXor {
                grain,
                left,
                right,
                same_length,
            } => curios_core::Intrinsic::bin_xor(
                *grain,
                self.term(left)?,
                self.term(right)?,
                self.term(same_length)?,
            ),
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
            Intrinsic::ListFold {
                element,
                result,
                list,
                init,
                function,
            } => curios_core::Intrinsic::list_fold(
                self.term(element)?,
                self.term(result)?,
                self.term(list)?,
                self.term(init)?,
                self.term(function)?,
            ),
            Intrinsic::CellType(inner) => curios_core::Intrinsic::cell_type(self.term(inner)?),
            Intrinsic::ChannelType(element) => {
                curios_core::Intrinsic::ChannelType(self.term(element)?)
            }
            Intrinsic::Cell { element } => curios_core::Intrinsic::Cell {
                element: self.term(element)?,
            },
            Intrinsic::CellFill {
                element,
                cell,
                value,
            } => curios_core::Intrinsic::CellFill {
                element: self.term(element)?,
                cell: self.term(cell)?,
                value: self.term(value)?,
            },
            Intrinsic::CellPoll { element, cell } => curios_core::Intrinsic::CellPoll {
                element: self.term(element)?,
                cell: self.term(cell)?,
                universes: Vec::new(),
            },
            Intrinsic::Channel {
                element,
                capacity,
                positive,
            } => curios_core::Intrinsic::Channel {
                element: self.term(element)?,
                capacity: self.term(capacity)?,
                positive: self.term(positive)?,
            },
            Intrinsic::ChannelPush {
                element,
                channel,
                value,
            } => curios_core::Intrinsic::ChannelPush {
                element: self.term(element)?,
                channel: self.term(channel)?,
                value: self.term(value)?,
            },
            Intrinsic::ChannelTake { element, channel } => curios_core::Intrinsic::ChannelTake {
                element: self.term(element)?,
                channel: self.term(channel)?,
                universes: Vec::new(),
            },
            Intrinsic::ChannelClose { element, channel } => curios_core::Intrinsic::ChannelClose {
                element: self.term(element)?,
                channel: self.term(channel)?,
            },
            Intrinsic::ChannelClosed { element, channel } => {
                curios_core::Intrinsic::ChannelClosed {
                    element: self.term(element)?,
                    channel: self.term(channel)?,
                }
            }
            Intrinsic::ChannelCount { element, channel } => curios_core::Intrinsic::ChannelCount {
                element: self.term(element)?,
                channel: self.term(channel)?,
            },
            Intrinsic::ChannelCapacity { element, channel } => {
                curios_core::Intrinsic::ChannelCapacity {
                    element: self.term(element)?,
                    channel: self.term(channel)?,
                }
            }
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
        .flat_map(|param| {
            let labels = pattern_labels(&param.pattern);
            // A `use` member has no binder to name, and resolution uses it whether or not the body does, so it is never a candidate.
            match param.plicity {
                Plicity::Witness => labels.into_iter().map(|(name, _)| (name, None)).collect(),
                _ => labels,
            }
        })
        .collect()
}

/// Every `Pattern::Binder` leaf in `pattern` with its span, recursing through nested tuple/struct fields in field order.
fn pattern_labels(pattern: &Pattern) -> Vec<(String, Option<Span>)> {
    match pattern {
        Pattern::Binder(name) => vec![(name.to_string(), name.span().cloned())],
        Pattern::Tuple(fields) | Pattern::Struct { fields, .. } => fields
            .iter()
            .flat_map(|field| pattern_labels(&field.value))
            .collect(),
    }
}

/// Where `pattern` was written, as far as its leaves record it: from its first leaf's start to its last leaf's end. A compound pattern keeps no span of its own, so its brackets are not covered.
fn pattern_span(pattern: &Pattern) -> Option<Span> {
    let spans = pattern_labels(pattern)
        .into_iter()
        .filter_map(|(_, span)| span)
        .collect::<Vec<_>>();
    let (first, last) = (spans.first()?, spans.last()?);
    Some(Span::new(first.source.clone(), first.start, last.end))
}

/// Every `Pattern::Binder` leaf name in `pattern`, recursing through nested tuple/struct fields in field order.
fn pattern_names(pattern: &Pattern) -> Vec<String> {
    match pattern {
        Pattern::Binder(name) => vec![name.to_string()],
        Pattern::Tuple(fields) | Pattern::Struct { fields, .. } => fields
            .iter()
            .flat_map(|field| pattern_names(&field.value))
            .collect(),
    }
}
