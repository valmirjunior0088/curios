use {
    super::Lowerer,
    crate::{
        BinPattern, Choose, ChooseArm, ChooseTest, Error, LstPattern, Match, MatchPattern,
        MatrixArm, Motive, NatPattern, Subterm, Term,
    },
    curios_base::{Grain, Plicity},
    std::{collections::BTreeMap, mem},
};

/// One elaborated arm of a single-level core inductive match: the constructor
/// tag, its payload binders (each with its written plicity mark), and the
/// (already-lowered) body. The marks travel to the Core arm; core elaboration
/// checks them against the constructor's canonical payload plicities.
type InductCase = (
    curios_core::Atom,
    Vec<(curios_base::Plicity, String)>,
    curios_core::Term,
);

/// One in-progress row of the matrix compiler's recursion (see
/// [`MatchCompiler::compile_matrix`]): the still-unconsumed column patterns (left to
/// right, one per not-yet-retired column, borrowed from the original
/// [`MatrixArm`]), the `let` bindings accumulated so far from retired
/// all-[`MatchPattern::Binder`] columns — applied at the leaf, outermost
/// first, matching [`Lowerer::lower_pattern_fields`]'s "first field's let ends
/// up outermost" convention — and the row's own (still-surface) body. A
/// name already bound directly by an enclosing core binder (see
/// [`MatchCompiler::compile_ctor`]'s single-row fast path) needs no entry here at
/// all — [`Lowerer::scoped`] is called inline, right where that binder's name
/// is decided, instead of threading a second bookkeeping list through the
/// recursion for it.
struct MatrixRow<'t> {
    patterns: Vec<&'t MatchPattern>,
    binds: Vec<(String, curios_core::Term)>,
    body: &'t Term,
}

/// One row grouped under a constructor tag in [`MatchCompiler::compile_ctor`]:
/// its still-borrowed, plicity-marked argument patterns and the row itself.
type VariantRow<'t> = (&'t [(Plicity, MatchPattern)], MatrixRow<'t>);

/// One unit of match-matrix compilation: borrows the [`Lowerer`] doing the
/// surrounding term lowering (for name resolution, scoping, and recursing back
/// into [`Lowerer::term`]/[`Lowerer::region`] at each leaf) so the matrix
/// recursion's own bookkeeping (columns, rows, motive threading) stays
/// separate from term lowering itself — mirroring `into_cont::Work` borrowing
/// its `Lowerer` counterpart there.
pub(super) struct MatchCompiler<'l, 'a, 'b> {
    lowerer: &'l Lowerer<'a, 'b>,
    /// The fallthrough arm for constructors/cases no row covers, already
    /// lowered — a headed match's `| _ =>` catch-all, or a `choose` bind-arm's
    /// rest-of-ladder (see [`Lowerer::lower_bind_arm`]). `None` means
    /// full enumeration: `compile_ctor` emits a plain `induct_match` (leaning
    /// on core's Rung-C vacuity for any pruned tag) and the hardcoded carriers
    /// require both of their shapes. Constant across one matrix's whole
    /// recursion — a nested match inside a body re-enters through `leaf`, which
    /// builds a fresh `MatchCompiler` with its own (absent) default.
    default: Option<curios_core::Term>,
}

impl<'l, 'a, 'b> MatchCompiler<'l, 'a, 'b> {
    pub(super) fn new(lowerer: &'l Lowerer<'a, 'b>) -> Self {
        Self {
            lowerer,
            default: None,
        }
    }

    /// A [`MatchCompiler`] whose uncovered-case fallthrough is `default`. Used
    /// for a headed `| _ =>` catch-all and for `choose` bind-arms.
    pub(super) fn with_default(
        lowerer: &'l Lowerer<'a, 'b>,
        default: Option<curios_core::Term>,
    ) -> Self {
        Self { lowerer, default }
    }

    /// Leaf shim for the plain (non-region) lowering path — matches
    /// [`Self::compile_matrix`]'s `leaf: fn(&Self, &Term)` shape. Also passed
    /// as a bare function pointer from [`Lowerer::subterm`], so it needs to be
    /// visible there too.
    pub(super) fn term(&self, term: &Term) -> Result<curios_core::Term, Error> {
        self.lowerer.term(term)
    }

    /// Leaf shim for the region (bang-hoisting) lowering path.
    fn region(&self, term: &Term) -> Result<curios_core::Term, Error> {
        self.lowerer.region(term)
    }

    /// Desugars a `match` inside a region: the scrutinee's bangs are collected
    /// into `binds` (hoisted out — the scrutinee runs unconditionally), while
    /// each arm is desugared as its own region (branch-local effects). This
    /// mirrors the `Match` arm of `subterm`, swapping `self.term` for `collect`
    /// on the head and `region` on arm bodies.
    pub(super) fn match_region(
        &self,
        match_: &Match,
        binds: &mut Vec<(String, curios_core::Term)>,
    ) -> Result<curios_core::Term, Error> {
        let head = self.collect(&match_.head, binds)?;
        self.compile_matrix_headed(head, &match_.motive, &match_.arms, Self::region)
    }

    /// Desugars a `choose` inside a region: the head arm's test mirrors the
    /// `subterm` path's right-fold, but only the *first* condition runs
    /// unconditionally, so only its bangs hoist into `binds`. Every deeper
    /// condition evaluates just when its predecessor is false, so each lowers
    /// as its own sub-region inside that false branch (see [`Self::ladder_region`]).
    pub(super) fn choose_region(
        &self,
        choose: &Choose,
        binds: &mut Vec<(String, curios_core::Term)>,
    ) -> Result<curios_core::Term, Error> {
        self.ladder_region(&choose.arms, &choose.default, binds)
    }

    /// Lowers a `choose`'s arms inside a region. The head arm's test (a
    /// condition or a bind scrutinee) collects its bangs into `binds` (the
    /// caller's region — it runs unconditionally at this level); its body is its
    /// own region; the rest of the ladder — reached only when this test fails —
    /// is lowered as a *fresh* region so deeper tests' bangs stay branch-local.
    /// With no arms left, the ladder is just its default, run unconditionally in
    /// the current region.
    fn ladder_region(
        &self,
        arms: &[ChooseArm],
        default: &Term,
        binds: &mut Vec<(String, curios_core::Term)>,
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
                Ok(curios_core::Term::bool_match(
                    head,
                    None,
                    curios_core::Term::metavar(self.context.fresh_metavar()),
                    rest_wrapped,
                    true_case,
                ))
            }
            ChooseTest::Bind { pattern, value } => {
                let value = self.collect(value, binds)?;
                self.lower_bind_arm(
                    pattern,
                    value,
                    &arm.body,
                    rest_wrapped,
                    MatchCompiler::region,
                )
            }
        }
    }

    /// Lowers a `choose` bind arm `| pattern = value => body` — with
    /// `value` and `rest` (the fallthrough) already lowered — as a single-row
    /// matrix over `value` whose fallthrough for every unmatched shape is `rest`
    /// (via [`Self::default`]). A refutable pattern only: a bare-binder LHS is
    /// irrefutable (a plain `let`) and rejected. When the pattern is refutable
    /// at more than one point, `rest` is shared through a nullary thunk (see
    /// [`Self::lower_defaulted_matrix`]).
    pub(super) fn lower_bind_arm(
        &self,
        pattern: &MatchPattern,
        value: curios_core::Term,
        body: &Term,
        rest: curios_core::Term,
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Term, Error> {
        if matches!(pattern, MatchPattern::Binder(_)) {
            return Err(Error::BindArmIrrefutable);
        }
        let arm = MatrixArm {
            pattern: pattern.clone(),
            body: body.clone(),
        };
        self.lower_defaulted_matrix(
            value,
            &None,
            std::slice::from_ref(&arm),
            rest,
            refutation_count(pattern) > 1,
            leaf,
        )
    }

    /// Lowers a headed match after splitting off a `| _ =>` catch-all: with no
    /// catch-all it is the plain full-enumeration matrix; with one the arms plus
    /// the (already-`leaf`-lowered) catch-all become a defaulted matrix, shared
    /// through a thunk when a nested arm would emit the default at more than one
    /// dispatch point.
    pub(super) fn compile_matrix_headed(
        &self,
        head: curios_core::Term,
        motive: &Option<Motive>,
        arms: &[MatrixArm],
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Term, Error> {
        let (arms, default_body) = split_catch_all(arms)?;
        match default_body {
            None => MatchCompiler::new(self.lowerer).compile_matrix(head, motive, arms, leaf),
            Some(body) => {
                let default = leaf(&MatchCompiler::new(self.lowerer), body)?;
                self.lower_defaulted_matrix(head, motive, arms, default, !arms_all_flat(arms), leaf)
            }
        }
    }

    /// Compiles `arms` against `head` with `default` as the uncovered-case
    /// fallthrough. When `share`, `default` is bound once as a nullary by-name
    /// thunk `k = () => default` and every fallthrough site calls `k()` — no
    /// duplication, and (by-name) `default`'s effects run only if a branch
    /// reaches it. Otherwise the default is emitted directly (a single site).
    fn lower_defaulted_matrix(
        &self,
        head: curios_core::Term,
        motive: &Option<Motive>,
        arms: &[MatrixArm],
        default: curios_core::Term,
        share: bool,
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Term, Error> {
        if !share {
            return MatchCompiler::with_default(self.lowerer, Some(default))
                .compile_matrix(head, motive, arms, leaf);
        }
        let k = self.context.fresh_binder();
        let call = curios_core::Term::apply(
            curios_core::Term::var(curios_core::Var::free(k.clone())),
            Vec::<curios_core::Term>::new(),
        );
        let matrix = MatchCompiler::with_default(self.lowerer, Some(call))
            .compile_matrix(head, motive, arms, leaf)?;
        let thunk = curios_core::Term::func(Vec::<(String, curios_core::Term)>::new(), default);
        let hole = curios_core::Term::metavar(self.context.fresh_metavar());
        Ok(curios_core::Term::let_(k, hole, thunk, matrix))
    }

    /// Splits an optional match motive into its `(label, body)` for the core
    /// match constructors. An omitted motive (`None`) lowers to an unlabelled
    /// fresh metavariable body — the same as writing `: _` — so a non-dependent
    /// match infers its motive by unifying the arms against that metavariable.
    /// The annotated form is inductive-only and goes through `induct_match` instead.
    pub(super) fn motive_parts<'m>(
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
            Some(Motive::Annotated { .. }) => Err(Error::AnnotatedMotiveNotInduct),
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
    fn induct_match(
        &self,
        head: curios_core::Term,
        motive: &Option<Motive>,
        cases: Vec<InductCase>,
    ) -> Result<curios_core::Term, Error> {
        let Some(Motive::Annotated {
            label,
            name,
            slots,
            body,
        }) = motive
        else {
            let (label, body) = self.motive_parts(motive)?;
            // A `| _ =>` catch-all (or bind-arm fallthrough) becomes the core
            // match's default; otherwise the arms enumerate every constructor
            // (core's Rung-C vacuity covers any pruned tag).
            return Ok(match &self.default {
                Some(default) => curios_core::Term::induct_match_default_marked(
                    head,
                    label,
                    body,
                    cases,
                    default.clone(),
                ),
                None => curios_core::Term::induct_match_marked(head, label, body, cases),
            });
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

        Ok(curios_core::Term::induct_match_motive_marked(
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
    /// it goes straight to [`Self::induct_match`] exactly as today.
    ///
    /// A dependent motive (`Motive::Scrutinee`/`Motive::Annotated`) is only
    /// meaningful when the head itself dispatches on a constructor tag
    /// directly — every arm's *top-level* pattern being [`MatchPattern::Variant`]
    /// — since that is the only case where a core `Match` node exists for
    /// the *original* scrutinee to attach the motive to (a
    /// tuple/struct-headed or plain-binder match never builds one; it just
    /// projects). Every deeper/inner split the recursion synthesizes never
    /// needs its own motive at all: an absent motive lowers to a fresh
    /// metavariable ([`Self::motive_parts`]'s `None` case), which core
    /// elaboration unifies against whatever expected type flows in from the
    /// enclosing checking context — no currying needed for a single head.
    pub(super) fn compile_matrix(
        &self,
        head: curios_core::Term,
        motive: &Option<Motive>,
        arms: &[MatrixArm],
        leaf: fn(&Self, &Term) -> Result<curios_core::Term, Error>,
    ) -> Result<curios_core::Term, Error> {
        if arms.is_empty() {
            return self.induct_match(head, motive, Vec::new());
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
            MatchPattern::Variant { .. } => {
                if rows
                    .iter()
                    .any(|row| !matches!(row.patterns[0], MatchPattern::Variant { .. }))
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
            MatchPattern::Bool(_) => {
                if rows
                    .iter()
                    .any(|row| !matches!(row.patterns[0], MatchPattern::Bool(_)))
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

        let mut groups: BTreeMap<String, Vec<VariantRow<'_>>> = BTreeMap::new();
        for mut row in rows {
            let MatchPattern::Variant { tag, args } = row.patterns.remove(0) else {
                unreachable!("every row classified as a variant")
            };
            groups
                .entry(tag.clone())
                .or_default()
                .push((args.as_slice(), row));
        }

        let mut cases = Vec::with_capacity(groups.len());
        for (tag, mut group) in groups {
            // Rows merging under one constructor must agree structurally on
            // arity *and* on the written plicity of each payload slot — a
            // source-shape check independent of constructor typing.
            let plicities = group[0]
                .0
                .iter()
                .map(|(plicity, _)| *plicity)
                .collect::<Vec<_>>();
            let arity = plicities.len();
            if group
                .iter()
                .any(|(args, _)| args.iter().map(|(p, _)| *p).ne(plicities.iter().copied()))
            {
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
                for (plicity, pattern) in args {
                    match pattern {
                        MatchPattern::Binder(name) => {
                            let bound = self.pattern_binder_name(name);
                            direct_names.push(bound.clone());
                            binder_names.push((*plicity, bound));
                        }
                        other => {
                            let synthetic = self.context.fresh_binder();
                            sub_columns.push(curios_core::Term::var(curios_core::Var::free(
                                synthetic.clone(),
                            )));
                            sub_patterns.push(other);
                            binder_names.push((*plicity, synthetic));
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
                    let mut patterns = args.iter().map(|(_, p)| p).collect::<Vec<_>>();
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
            let marked = plicities.into_iter().zip(synthetic).collect::<Vec<_>>();
            cases.push((curios_core::Atom::from(tag.as_str()), marked, body));
        }

        self.induct_match(scrutinee, top_motive.unwrap_or(&None), cases)
    }

    /// Groups rows into `Bool`'s two literal shapes and emits
    /// [`curios_core::Term::bool_match`] directly — never `induct_match`
    /// (`Cases::Bool` is its own hardcoded core node, not a tag dispatch; see
    /// this module's own notes on hardcoded-primitive carriers). `Bool`
    /// carries no payload at all, so — unlike [`Self::compile_ctor`] — there
    /// is no single-row/multi-row naming discipline needed here.
    ///
    /// Unlike a user inductive (whose omitted tags `compile_ctor` defers to
    /// `induct_match`'s Rung-C vacuity inversion), `Cases::Bool` has no
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
            let MatchPattern::Bool(value) = row.patterns.remove(0) else {
                unreachable!("every row classified as Bool")
            };
            match value {
                false => false_rows.push(row),
                true => true_rows.push(row),
            }
        }

        // With a fallthrough default, a missing group's arm is the default
        // (see [`Self::default`]); without one, both shapes are still required.
        if (false_rows.is_empty() || true_rows.is_empty()) && self.default.is_none() {
            return Err(Error::MatrixIncompleteCarrierMatch { carrier: "Bool" });
        }

        let (label, motive_body) = self.motive_parts(top_motive.unwrap_or(&None))?;
        let false_case = match false_rows.is_empty() {
            true => self
                .default
                .clone()
                .expect("default present when a group is missing"),
            false => self.compile(rest.clone(), false_rows, None, leaf)?,
        };
        let true_case = match true_rows.is_empty() {
            true => self
                .default
                .clone()
                .expect("default present when a group is missing"),
            false => self.compile(rest, true_rows, None, leaf)?,
        };

        Ok(curios_core::Term::bool_match(
            scrutinee,
            label,
            motive_body,
            false_case,
            true_case,
        ))
    }

    /// Compiles a `Nat` column in one of two modes, chosen by its leaves —
    /// the induction/dispatch split the surface once drew with two separate
    /// `Nat` match forms, now decided here so both are ordinary matrix leaves:
    ///
    /// - **Induction** (a `Succ` leaf present): `0`/`n+1; ih` structural
    ///   peeling, emitted as [`curios_core::Term::nat_match`]. A `Lit` leaf
    ///   mixed in is [`Error::MatrixMixedNatDispatch`] — no single core form
    ///   peels a successor *and* dispatches on a value.
    /// - **Dispatch** (no `Succ`): value dispatch over `0`/`Lit(k)` cases,
    ///   emitted as [`curios_core::Term::switch`] with the matrix default as
    ///   its fallthrough. A `switch` over `Nat` is never exhaustive, so the
    ///   default is mandatory (else [`Error::MatrixIncompleteCarrierMatch`]).
    ///   Rows sharing a literal group and recurse together, exactly like
    ///   [`Self::compile_ctor`]'s tags.
    ///
    /// The induction path mirrors [`Self::compile_ctor`]'s single-row/multi-row naming
    /// discipline exactly, for the same reason: `curios-core`'s erasure pass
    /// reads a `Nat` succ arm's stored binder labels as naming hints too
    /// (`erase_nat_match`, the same `Context::fresh` hint-compounding
    /// mechanism `erase_induct_match` has) — unconditionally minting a
    /// synthetic name here would resurrect the exact regression class
    /// `compile_ctor`'s fast path exists to avoid. A `NatSucc` group of
    /// exactly one row therefore reuses that row's own written
    /// `pred_label`/`ih_label` directly (the optional `ih` through
    /// [`Self::cons_ih_name`], like [`Self::compile_lst`]); only a group
    /// with more than one row mints synthetic names, and — as in
    /// [`Self::compile_lst`] — a multi-row member that omitted `; ih` gets
    /// no ih bind pushed.
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
        let mut succ_rows: Vec<(String, Option<String>, MatrixRow<'_>)> = Vec::new();
        let mut lit_rows: Vec<(u32, MatrixRow<'_>)> = Vec::new();
        for mut row in rows {
            match row.patterns.remove(0) {
                MatchPattern::Nat(NatPattern::Zero) => zero_rows.push(row),
                MatchPattern::Nat(NatPattern::Succ {
                    pred_label,
                    ih_label,
                }) => succ_rows.push((pred_label.clone(), ih_label.clone(), row)),
                MatchPattern::Nat(NatPattern::Lit(value)) => lit_rows.push((*value, row)),
                _ => unreachable!("every row classified as Nat"),
            }
        }

        let (label, motive_body) = self.motive_parts(top_motive.unwrap_or(&None))?;

        // Dispatch mode: no successor peeling, so `0`/`Lit(k)` are `switch`
        // cases and the matrix default is the mandatory fallthrough (a `switch`
        // over `Nat` is never exhaustive). Rows sharing a literal group and
        // recurse together — a genuinely duplicated row falls to
        // [`Self::compile`]'s leaf case, not silent last-wins.
        if succ_rows.is_empty() {
            let Some(default) = self.default.clone() else {
                return Err(Error::MatrixIncompleteCarrierMatch { carrier: "Nat" });
            };
            let mut groups: BTreeMap<u32, Vec<MatrixRow<'_>>> = BTreeMap::new();
            for row in zero_rows {
                groups.entry(0).or_default().push(row);
            }
            for (value, row) in lit_rows {
                groups.entry(value).or_default().push(row);
            }
            let mut cases = Vec::with_capacity(groups.len());
            for (value, group) in groups {
                cases.push((value, self.compile(rest.clone(), group, None, leaf)?));
            }
            return Ok(curios_core::Term::switch(
                scrutinee,
                label,
                motive_body,
                cases,
                default,
            ));
        }

        // Induction mode: `0`/`n+1; ih`. A literal case here mixes value
        // dispatch with successor peeling — no core form does both.
        if !lit_rows.is_empty() {
            return Err(Error::MatrixMixedNatDispatch);
        }
        if zero_rows.is_empty() && self.default.is_none() {
            return Err(Error::MatrixIncompleteCarrierMatch { carrier: "Nat" });
        }

        let zero_case = match zero_rows.is_empty() {
            true => self
                .default
                .clone()
                .expect("default present when a group is missing"),
            false => self.compile(rest.clone(), zero_rows, None, leaf)?,
        };

        let (pred_label, ih_label, succ_case) = if succ_rows.len() == 1 {
            let (pred_name, ih_name, row) = succ_rows.pop().unwrap();
            let pred_bound = self.pattern_binder_name(&pred_name);
            let ih_bound = self.cons_ih_name(&ih_name);
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
                    if let Some(ih_name) = ih_name {
                        row.binds.push((
                            ih_name,
                            curios_core::Term::var(curios_core::Var::free(ih_synth.clone())),
                        ));
                    }
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

        if (nil_rows.is_empty() || cons_rows.is_empty()) && self.default.is_none() {
            return Err(Error::MatrixIncompleteCarrierMatch { carrier: "Lst" });
        }

        let (label, motive_body) = self.motive_parts(top_motive.unwrap_or(&None))?;
        let empty_case = match nil_rows.is_empty() {
            true => self
                .default
                .clone()
                .expect("default present when a group is missing"),
            false => self.compile(rest.clone(), nil_rows, None, leaf)?,
        };

        let (head_label, tail_label, ih_label, cons_case) = if cons_rows.is_empty() {
            // The default fills the cons arm; its binders go unused.
            (
                self.context.fresh_binder(),
                self.context.fresh_binder(),
                self.context.fresh_binder(),
                self.default
                    .clone()
                    .expect("default present when a group is missing"),
            )
        } else if cons_rows.len() == 1 {
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

        let mut grain: Option<&Grain> = None;
        let mut end_rows = Vec::new();
        let mut byte_rows: Vec<(String, String, Option<String>, MatrixRow<'_>)> = Vec::new();
        for mut row in rows {
            match row.patterns.remove(0) {
                MatchPattern::Bin(BinPattern::End(row_grain)) => {
                    if grain.is_some_and(|grain| *grain != *row_grain) {
                        return Err(Error::MatrixMixedBinGrain);
                    }
                    grain.get_or_insert(row_grain);
                    end_rows.push(row)
                }
                MatchPattern::Bin(BinPattern::Atom {
                    grain: row_grain,
                    head_label,
                    tail_label,
                    ih_label,
                }) => {
                    if grain.is_some_and(|grain| *grain != *row_grain) {
                        return Err(Error::MatrixMixedBinGrain);
                    }
                    grain.get_or_insert(row_grain);
                    byte_rows.push((
                        head_label.clone(),
                        tail_label.clone(),
                        ih_label.clone(),
                        row,
                    ))
                }
                _ => unreachable!("every row classified as Bin"),
            }
        }

        if (end_rows.is_empty() || byte_rows.is_empty()) && self.default.is_none() {
            return Err(Error::MatrixIncompleteCarrierMatch { carrier: "Bin" });
        }

        let (label, motive_body) = self.motive_parts(top_motive.unwrap_or(&None))?;
        let empty_case = match end_rows.is_empty() {
            true => self
                .default
                .clone()
                .expect("default present when a group is missing"),
            false => self.compile(rest.clone(), end_rows, None, leaf)?,
        };

        let (head_label, tail_label, ih_label, cons_case) = if byte_rows.is_empty() {
            // The default fills the byte arm; its binders go unused.
            (
                self.context.fresh_binder(),
                self.context.fresh_binder(),
                self.context.fresh_binder(),
                self.default
                    .clone()
                    .expect("default present when a group is missing"),
            )
        } else if byte_rows.len() == 1 {
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
            *grain.expect("a Bin group has at least one row"),
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
}

/// How many distinct dispatch points a bind pattern is refutable at — one per
/// constructor/carrier shape it constrains, summed across nested arguments.
/// Drives fallthrough-sharing: >1 point means the rest-of-ladder would be
/// emitted at more than one core default site, so it is shared through a thunk
/// rather than duplicated. Irrefutable binders and exhaustive tuple/struct
/// shells contribute none of their own.
fn refutation_count(pattern: &MatchPattern) -> usize {
    match pattern {
        MatchPattern::Binder(_) => 0,
        MatchPattern::Variant { args, .. } => {
            1 + args.iter().map(|(_, p)| refutation_count(p)).sum::<usize>()
        }
        MatchPattern::Tuple(fields) | MatchPattern::Struct { fields, .. } => {
            fields.iter().map(|f| refutation_count(&f.value)).sum()
        }
        MatchPattern::Bool(_)
        | MatchPattern::Nat(_)
        | MatchPattern::Lst(_)
        | MatchPattern::Bin(_) => 1,
    }
}

/// Splits a headed match's arms into its concrete arms and an optional
/// `| _ =>` catch-all body. A final top-level bare `_` after *dispatching* arms
/// (a constructor tag or any of the four hardcoded-carrier leaves —
/// [`MatchPattern::is_dispatchable`]) is the catch-all: its body becomes the
/// core match's default (a `Cases::Induct` default, or a `Nat`/`Bool`
/// carrier's fallthrough — this is what makes a `Nat` literal dispatch's
/// mandatory `_` legal). A *named* final binder in that position is a mistake
/// ([`Error::MatchNamedCatchAll`]). A lone `_` with no concrete arms is not a
/// catch-all — it stays the all-binder retirement path (a `let`) — and neither
/// is a `_` after tuple/struct arms, which project exhaustively and need no
/// default.
fn split_catch_all(arms: &[MatrixArm]) -> Result<(&[MatrixArm], Option<&Term>), Error> {
    match arms.split_last() {
        Some((last, init))
            if !init.is_empty() && init.iter().all(|arm| arm.pattern.is_dispatchable()) =>
        {
            match &last.pattern {
                MatchPattern::Binder(name) if name == "_" => Ok((init, Some(&last.body))),
                MatchPattern::Binder(_) => Err(Error::MatchNamedCatchAll),
                _ => Ok((arms, None)),
            }
        }
        _ => Ok((arms, None)),
    }
}

/// Whether every constructor arm is *flat* — each argument a plain binder, no
/// nested shape. A flat set emits the catch-all default at exactly one dispatch
/// point (the head), so it needs no thunk; any nesting propagates the default
/// into a payload sub-match, a second site worth sharing.
fn arms_all_flat(arms: &[MatrixArm]) -> bool {
    arms.iter().all(|arm| match &arm.pattern {
        MatchPattern::Variant { args, .. } => args
            .iter()
            .all(|(_, p)| matches!(p, MatchPattern::Binder(_))),
        _ => true,
    })
}

impl<'l, 'a, 'b> std::ops::Deref for MatchCompiler<'l, 'a, 'b> {
    type Target = Lowerer<'a, 'b>;

    fn deref(&self) -> &Lowerer<'a, 'b> {
        self.lowerer
    }
}
