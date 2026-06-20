use {
    super::{
        Apply, Bound, Carrier, Cases, Context, Field, Func, FuncType, Match, Metavar, Proj, Rec,
        ReduceError, Struct, StructType, Subterm, Telescope, Term, Tuple, TupleType, UnionType,
        Var, Variant, Visit, check, convert_prim, reduce, unfold_rec,
    },
    std::{
        collections::{HashSet, VecDeque},
        time::Instant,
    },
};

/// The outcome of attempting to solve a metavariable against a candidate.
enum Solved {
    /// Committed `m := t`.
    Done,
    /// Not yet solvable (embedded unsolved metavariable): the goal is parked on
    /// the `blocked` queue and retried after later progress.
    Postponed,
    /// Unsolvable: occurs-check, scope-check, or re-validation failure.
    Failed,
}

pub fn convert(
    context: &mut Context,
    type_: &Term,
    this: &Term,
    that: &Term,
) -> Result<bool, ReduceError> {
    // The strict boolean-oracle view: "not yet decidable" is *not* "equal".
    // Everything that needs a definite yes/no (re-validation, the inverter,
    // type-level dispatch) uses this; only the elaboration turnaround
    // (`expect`) may treat `Blocked` as provisional success by parking it.
    Ok(matches!(
        convert_outcome(context, type_, this, that)?,
        Outcome::Converts
    ))
}

pub fn convert_outcome(
    context: &mut Context,
    type_: &Term,
    this: &Term,
    that: &Term,
) -> Result<Outcome, ReduceError> {
    Convert::new(type_.clone(), this.clone(), that.clone()).outcome(context)
}

/// The verdict of a conversion run, distinguishing "provably unequal" from
/// "not yet decidable" (§8).
#[derive(Debug)]
pub enum Outcome {
    /// Definitionally equal.
    Converts,
    /// A hard structural mismatch — provably unequal, no solution can help.
    Mismatch,
    /// Quiesced with constraints still blocked on unsolved metavariables:
    /// undecided either way. The blocked goals are surrendered to the caller
    /// — the elaboration turnaround parks them on the `Context` to be retried
    /// when a watched metavariable is solved.
    Blocked(Vec<Goal>),
}

/// Synthesize the type of a neutral (a `Var`/`Apply`/`Proj` spine) *without* validating
/// its subterms. Returns `None` when the head is out of scope or the spine is not a
/// typeable neutral — callers fall back conservatively. Built only from the same
/// primitives `infer` uses (`Context::assumption`, `reduce`, `Telescope::open`/`nth`), so
/// there is no duplicated typing judgment to drift from `infer`.
fn synth_neutral(context: &mut Context, term: &Term) -> Result<Option<Term>, ReduceError> {
    match &**term {
        Subterm::Var(var) => Ok(context.assumption(var.unwrap()).cloned()),
        Subterm::Apply(Apply { head, params, .. }) => {
            let Some(head_type) = synth_neutral(context, head)? else {
                return Ok(None);
            };

            match Term::unwrap_or_clone(reduce(context, head_type)?) {
                Subterm::FuncType(FuncType { telescope, .. })
                    if telescope.len() == params.len() =>
                {
                    let refs = params.iter().collect::<Vec<_>>();
                    Ok(Some(telescope.open(&refs)))
                }
                _ => Ok(None),
            }
        }
        Subterm::Proj(Proj {
            head,
            field: Field::Index(index),
        }) => {
            let Some(head_type) = synth_neutral(context, head)? else {
                return Ok(None);
            };

            match Term::unwrap_or_clone(reduce(context, head_type)?) {
                Subterm::TupleType(TupleType { telescope }) => {
                    Ok(telescope.nth(*index, |j| Term::proj(head.clone(), j)))
                }
                _ => Ok(None),
            }
        }
        _ => Ok(None),
    }
}

/// Recover the parameter types of an application from the head's function type, opening
/// each successive entry with the actual arguments (dependency). `None` when the head's
/// type is unavailable or not a `FuncType` of matching arity — callers fall back to
/// comparing arguments at `Term::type_()`.
fn apply_param_types(
    context: &mut Context,
    head: &Term,
    params: &[Term],
) -> Result<Option<Vec<Term>>, ReduceError> {
    let Some(head_type) = synth_neutral(context, head)? else {
        return Ok(None);
    };

    let telescope = match Term::unwrap_or_clone(reduce(context, head_type)?) {
        Subterm::FuncType(FuncType { telescope, .. }) if telescope.len() == params.len() => {
            telescope
        }
        _ => return Ok(None),
    };

    let mut types = Vec::with_capacity(params.len());
    telescope.walk(params, |_, ty| {
        types.push(ty.clone());
        Ok(())
    })?;

    Ok(Some(types))
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Goal {
    pub type_: Term,
    pub this: Term,
    pub that: Term,
}

#[derive(Debug)]
pub struct Convert {
    history: HashSet<Goal>,
    pending: VecDeque<Goal>,
    // Constraints postponed because a side is flexible but not yet solvable
    // (flex–flex with distinct heads, or a candidate carrying an unsolved
    // metavariable). Retried whenever a fresh solution lands (§8).
    blocked: Vec<Goal>,
    // Whether a metavariable was solved since the last `blocked` sweep — the
    // signal that retrying `blocked` could make further progress.
    progress: bool,
}

impl Convert {
    fn new(type_: Term, this: Term, that: Term) -> Self {
        Self {
            history: HashSet::new(),
            pending: VecDeque::from([Goal { type_, this, that }]),
            blocked: Vec::new(),
            progress: false,
        }
    }

    fn in_history(&mut self, goal: &Goal) -> bool {
        !self.history.insert(goal.clone())
    }

    pub fn enqueue(&mut self, type_: Term, this: Term, that: Term) {
        self.pending.push_back(Goal { type_, this, that });
    }

    fn dequeue(&mut self, context: &Context) -> Result<Option<Goal>, ReduceError> {
        if Instant::now() > context.deadline() {
            return Err(ReduceError::Preempted);
        }

        Ok(self.pending.pop_front())
    }

    fn compare_func_type(
        &mut self,
        context: &mut Context,
        this: FuncType,
        that: FuncType,
    ) -> Result<bool, ReduceError> {
        fn walk(
            cmp: &mut Convert,
            context: &mut Context,
            this: &Telescope<Term>,
            that: &Telescope<Term>,
        ) -> Result<bool, ReduceError> {
            match (this, that) {
                (Telescope::Cons(ty_a, rest_a), Telescope::Cons(ty_b, rest_b)) => {
                    cmp.enqueue(Term::type_(), ty_a.clone(), ty_b.clone());
                    let v = Term::var(Var::free(context.fresh(rest_a.first_label())));
                    let inner_a = rest_a.open(&[&v]);
                    let inner_b = rest_b.open(&[&v]);
                    walk(cmp, context, &inner_a, &inner_b)
                }
                (Telescope::Done(out_a), Telescope::Done(out_b)) => {
                    cmp.enqueue(Term::type_(), (**out_a).clone(), (**out_b).clone());
                    Ok(true)
                }
                _ => Ok(false),
            }
        }
        walk(self, context, &this.telescope, &that.telescope)
    }

    fn compare_func(
        &mut self,
        context: &mut Context,
        this: Func,
        that: Func,
        type_: Term,
    ) -> Result<bool, ReduceError> {
        let n = this.telescope.len();
        let ys: Vec<Term> = (0..n)
            .map(|_| Term::var(Var::free(context.fresh(None))))
            .collect();
        let y_refs: Vec<&Term> = ys.iter().collect();
        let output_type = match Term::unwrap_or_clone(reduce(context, type_)?) {
            Subterm::FuncType(FuncType { telescope, .. }) => telescope.open(&y_refs),
            _ => Term::type_(),
        };
        self.enqueue(
            output_type,
            this.telescope.open(&y_refs),
            that.telescope.open(&y_refs),
        );

        Ok(true)
    }

    fn compare_apply(
        &mut self,
        context: &mut Context,
        this: Apply,
        that: Apply,
    ) -> Result<bool, ReduceError> {
        if this.params.len() != that.params.len() {
            return Ok(false);
        }

        // Recover the real argument types from the head's function type so η fires at the
        // correct type (e.g. a unit-typed argument is compared at `()`, where proof
        // irrelevance makes distinct neutrals equal). Falls back to `Term::type_()` when
        // the head's type is unavailable.
        let param_types = apply_param_types(context, &this.head, &this.params)?;

        self.enqueue(Term::type_(), this.head, that.head);

        for (i, (a, b)) in this.params.into_iter().zip(that.params).enumerate() {
            let type_ = param_types
                .as_ref()
                .and_then(|types| types.get(i).cloned())
                .unwrap_or_else(Term::type_);
            self.enqueue(type_, a, b);
        }

        Ok(true)
    }

    fn compare_tuple_type(
        &mut self,
        context: &mut Context,
        this: TupleType,
        that: TupleType,
    ) -> Result<bool, ReduceError> {
        fn walk(
            cmp: &mut Convert,
            context: &mut Context,
            this: &Telescope<()>,
            that: &Telescope<()>,
        ) -> Result<bool, ReduceError> {
            match (this, that) {
                (Telescope::Cons(ty_a, rest_a), Telescope::Cons(ty_b, rest_b)) => {
                    // Field labels are part of a tuple type's identity:
                    // `{ a : Nat } ≢ { Nat } ≢ { b : Nat }` (the unlabeled ""
                    // is just another label). This is deliberately tuple-only —
                    // function-type parameter names stay alpha-convertible
                    // (see `compare_func_type`, where `first_label` feeds
                    // freshness, never equality).
                    if rest_a.first_label().unwrap_or_default()
                        != rest_b.first_label().unwrap_or_default()
                    {
                        return Ok(false);
                    }
                    cmp.enqueue(Term::type_(), ty_a.clone(), ty_b.clone());
                    let v = Term::var(Var::free(context.fresh(rest_a.first_label())));
                    let inner_a = rest_a.open(&[&v]);
                    let inner_b = rest_b.open(&[&v]);
                    walk(cmp, context, &inner_a, &inner_b)
                }
                (Telescope::Done(_), Telescope::Done(_)) => Ok(true),
                _ => Ok(false),
            }
        }
        walk(self, context, &this.telescope, &that.telescope)
    }

    fn compare_tuple(
        &mut self,
        context: &mut Context,
        this: Tuple,
        that: Tuple,
        type_: Term,
    ) -> Result<bool, ReduceError> {
        let n = this.fields.len();
        if n != that.fields.len() {
            return Ok(false);
        }

        let mut cur = match Term::unwrap_or_clone(reduce(context, type_)?) {
            Subterm::TupleType(TupleType { telescope }) if telescope.len() == n => Some(telescope),
            _ => None,
        };

        for (a, b) in this.fields.iter().zip(that.fields.iter()) {
            let ft = match cur.take() {
                Some(Telescope::Cons(ty, rest)) => {
                    cur = Some(rest.open(&[a]));
                    ty
                }
                _ => Term::type_(),
            };
            self.enqueue(ft, a.clone(), b.clone());
        }

        Ok(true)
    }

    fn compare_proj(&mut self, this: Proj, that: Proj) -> Result<bool, ReduceError> {
        if this.field != that.field {
            return Ok(false);
        }
        self.enqueue(Term::type_(), this.head, that.head);
        Ok(true)
    }

    fn compare_union_type(
        &mut self,
        this: UnionType,
        that: UnionType,
    ) -> Result<bool, ReduceError> {
        if this.name != that.name
            || this.params.len() != that.params.len()
            || this.indices.len() != that.indices.len()
        {
            return Ok(false);
        }

        for (a, b) in this.params.into_iter().zip(that.params) {
            self.enqueue(Term::type_(), a, b);
        }

        for (a, b) in this.indices.into_iter().zip(that.indices) {
            self.enqueue(Term::type_(), a, b);
        }

        Ok(true)
    }

    fn compare_variant(
        &mut self,
        context: &mut Context,
        this: Variant,
        that: Variant,
    ) -> Result<bool, ReduceError> {
        if this.name != that.name
            || this.tag != that.tag
            || this.params.len() != that.params.len()
            || this.payload.len() != that.payload.len()
        {
            return Ok(false);
        }

        // Recover the payload types from the constructor's registry telescope
        // (instantiated at this side's parameters) so each payload field
        // compares at its own type — η and proof irrelevance fire per field,
        // instead of a structural compare at `Type`. Same recovery `erase`
        // uses; falls back to `Type` if the inductive is somehow absent.
        let mut telescope = context
            .inductive(&this.name)
            .and_then(|inductive| inductive.instantiate(&this.tag, &this.params));

        for (a, b) in this.params.into_iter().zip(that.params) {
            self.enqueue(Term::type_(), a, b);
        }

        for (a, b) in this.payload.into_iter().zip(that.payload) {
            let payload_type = match telescope.take() {
                Some(Telescope::Cons(ty, rest)) => {
                    telescope = Some(rest.open(&[&a]));
                    ty
                }
                _ => Term::type_(),
            };
            self.enqueue(payload_type, a, b);
        }

        Ok(true)
    }

    fn compare_struct_type(
        &mut self,
        this: StructType,
        that: StructType,
    ) -> Result<bool, ReduceError> {
        if this.name != that.name || this.params.len() != that.params.len() {
            return Ok(false);
        }

        for (a, b) in this.params.into_iter().zip(that.params) {
            self.enqueue(Term::type_(), a, b);
        }

        Ok(true)
    }

    fn compare_struct(
        &mut self,
        context: &mut Context,
        this: Struct,
        that: Struct,
    ) -> Result<bool, ReduceError> {
        if this.name != that.name
            || this.params.len() != that.params.len()
            || this.fields.len() != that.fields.len()
        {
            return Ok(false);
        }

        // Recover the field types from the registry (instantiated at this side's
        // parameters) so each field compares at its own type — η and proof
        // irrelevance fire per field, as `compare_tuple` does with the tuple
        // type's telescope. Falls back to `Type` if the struct is somehow absent.
        let mut telescope = context
            .structure(&this.name)
            .map(|structure| structure.fields_at(&this.params));

        for (a, b) in this.params.into_iter().zip(that.params) {
            self.enqueue(Term::type_(), a, b);
        }

        for (a, b) in this.fields.into_iter().zip(that.fields) {
            let field_type = match telescope.take() {
                Some(Telescope::Cons(ty, rest)) => {
                    telescope = Some(rest.open(&[&a]));
                    ty
                }
                _ => Term::type_(),
            };
            self.enqueue(field_type, a, b);
        }

        Ok(true)
    }

    fn compare_match(
        &mut self,
        context: &mut Context,
        this: Match,
        that: Match,
    ) -> Result<bool, ReduceError> {
        self.enqueue(Term::type_(), this.head, that.head);

        // The motive's arity is 1 except for an annotated union-match motive
        // (pattern binders then the scrutinee); different arities are
        // structurally distinct.
        if this.motive.arity() != that.motive.arity() {
            return Ok(false);
        }

        let labels = (0..this.motive.arity())
            .map(|_| Term::var(Var::free(context.fresh(None))))
            .collect::<Vec<_>>();
        let label_refs = labels.iter().collect::<Vec<_>>();
        self.enqueue(
            Term::type_(),
            this.motive.open(&label_refs),
            that.motive.open(&label_refs),
        );

        match (this.cases, that.cases) {
            (
                Cases::Bln {
                    false_case: this_false,
                    true_case: this_true,
                },
                Cases::Bln {
                    false_case: that_false,
                    true_case: that_true,
                },
            ) => {
                self.enqueue(Term::type_(), this_false, that_false);
                self.enqueue(Term::type_(), this_true, that_true);
                Ok(true)
            }

            (
                Cases::Nat {
                    zero_case: this_zero,
                    succ_case: this_succ,
                },
                Cases::Nat {
                    zero_case: that_zero,
                    succ_case: that_succ,
                },
            ) => {
                self.enqueue(Term::type_(), this_zero, that_zero);

                let pred_label: Term = Term::var(Var::free(context.fresh(None)));
                let ih_label: Term = Term::var(Var::free(context.fresh(None)));
                self.enqueue(
                    Term::type_(),
                    this_succ.open(&[&pred_label, &ih_label]),
                    that_succ.open(&[&pred_label, &ih_label]),
                );

                Ok(true)
            }

            (
                Cases::Switch {
                    cases: this_cases,
                    default: this_default,
                },
                Cases::Switch {
                    cases: that_cases,
                    default: that_default,
                },
            ) => {
                if this_cases.len() != that_cases.len() {
                    return Ok(false);
                }

                for ((kl, vl), (kr, vr)) in this_cases.into_iter().zip(that_cases) {
                    if kl != kr {
                        return Ok(false);
                    }
                    self.enqueue(Term::type_(), vl, vr);
                }

                self.enqueue(Term::type_(), this_default, that_default);
                Ok(true)
            }

            // The pattern is elaboration-time data, fully reflected in the
            // motive scope once checked — convertibility ignores it.
            (
                Cases::Union {
                    cases: this_cases, ..
                },
                Cases::Union {
                    cases: that_cases, ..
                },
            ) => {
                if this_cases.len() != that_cases.len() {
                    return Ok(false);
                }

                for ((this_atom, this_scope), (that_atom, that_scope)) in
                    this_cases.into_iter().zip(that_cases)
                {
                    if this_atom != that_atom || this_scope.arity() != that_scope.arity() {
                        return Ok(false);
                    }

                    let binders = (0..this_scope.arity())
                        .map(|_| Term::var(Var::free(context.fresh(None))))
                        .collect::<Vec<_>>();
                    let binder_refs = binders.iter().collect::<Vec<_>>();

                    self.enqueue(
                        Term::type_(),
                        this_scope.open(&binder_refs),
                        that_scope.open(&binder_refs),
                    );
                }

                Ok(true)
            }

            (
                Cases::Inductive {
                    carrier: this_carrier,
                    empty_case: this_empty,
                    cons_case: this_cons,
                },
                Cases::Inductive {
                    carrier: that_carrier,
                    empty_case: that_empty,
                    cons_case: that_cons,
                },
            ) => {
                match (this_carrier, that_carrier) {
                    (Carrier::Arr(this_elem), Carrier::Arr(that_elem)) => {
                        self.enqueue(Term::type_(), this_elem, that_elem);
                    }
                    (Carrier::Bin, Carrier::Bin) => {}
                    // Distinct carriers are never structurally convertible.
                    _ => return Ok(false),
                }
                self.enqueue(Term::type_(), this_empty, that_empty);

                let head_label: Term = Term::var(Var::free(context.fresh(None)));
                let tail_label: Term = Term::var(Var::free(context.fresh(None)));
                let ih_label: Term = Term::var(Var::free(context.fresh(None)));
                self.enqueue(
                    Term::type_(),
                    this_cons.open(&[&head_label, &tail_label, &ih_label]),
                    that_cons.open(&[&head_label, &tail_label, &ih_label]),
                );

                Ok(true)
            }

            // Unreachable under the dispatch guard (same `Cases` discriminant);
            // distinct kinds are never structurally convertible.
            _ => Ok(false),
        }
    }

    fn compare_rec(
        &mut self,
        context: &mut Context,
        this: Rec,
        that: Rec,
    ) -> Result<bool, ReduceError> {
        if this.items.len() != that.items.len() {
            return Ok(false);
        }

        let labels = (0..this.items.len())
            .map(|_| Term::var(Var::free(context.fresh(None))))
            .collect::<Vec<_>>();

        let labels = labels.iter().collect::<Vec<_>>();

        for ((this_type, this_body), (that_type, that_body)) in
            this.items.into_iter().zip(that.items)
        {
            self.enqueue(
                Term::type_(),
                this_type.open(&labels),
                that_type.open(&labels),
            );
            self.enqueue(
                Term::type_(),
                this_body.open(&labels),
                that_body.open(&labels),
            );
        }

        self.enqueue(
            Term::type_(),
            this.tail.open(&labels),
            that.tail.open(&labels),
        );

        Ok(true)
    }

    fn eta_expand_func(
        &mut self,
        context: &mut Context,
        func: Func,
        other: Term,
        type_: Term,
    ) -> Result<bool, ReduceError> {
        let n = func.telescope.len();
        let ys: Vec<Term> = (0..n)
            .map(|_| Term::var(Var::free(context.fresh(None))))
            .collect();
        let y_refs: Vec<&Term> = ys.iter().collect();
        let output_type = match Term::unwrap_or_clone(reduce(context, type_)?) {
            Subterm::FuncType(FuncType { telescope, .. }) => telescope.open(&y_refs),
            _ => Term::type_(),
        };
        self.enqueue(
            output_type,
            func.telescope.open(&y_refs),
            Term::apply(other, ys),
        );
        Ok(true)
    }

    fn eta_expand_tuple(
        &mut self,
        context: &mut Context,
        tuple: Tuple,
        other: Term,
        type_: Term,
    ) -> Result<bool, ReduceError> {
        let n = tuple.fields.len();

        let mut cur = match Term::unwrap_or_clone(reduce(context, type_)?) {
            Subterm::TupleType(TupleType { telescope }) if telescope.len() == n => Some(telescope),
            _ => None,
        };

        for (i, field) in tuple.fields.iter().enumerate() {
            let ft = match cur.take() {
                Some(Telescope::Cons(ty, rest)) => {
                    cur = Some(rest.open(&[field]));
                    ty
                }
                _ => Term::type_(),
            };
            self.enqueue(ft, field.clone(), Term::proj(other.clone(), i));
        }

        Ok(true)
    }

    fn eta_expand_neutral(
        &mut self,
        context: &mut Context,
        this: Term,
        that: Term,
        type_: Term,
    ) -> Result<bool, ReduceError> {
        match Term::unwrap_or_clone(reduce(context, type_)?) {
            Subterm::FuncType(FuncType { telescope, .. }) => {
                let n = telescope.len();
                let ys: Vec<Term> = (0..n)
                    .map(|_| Term::var(Var::free(context.fresh(None))))
                    .collect();
                let y_refs: Vec<&Term> = ys.iter().collect();
                let output_type = telescope.open(&y_refs);
                self.enqueue(
                    output_type,
                    Term::apply(this, ys.clone()),
                    Term::apply(that, ys),
                );
                Ok(true)
            }
            Subterm::TupleType(TupleType { telescope }) => {
                for i in 0..telescope.len() {
                    self.enqueue(
                        Term::type_(),
                        Term::proj(this.clone(), i),
                        Term::proj(that.clone(), i),
                    );
                }
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    /// Replace every occurrence of a subject term in `t` — matched by the
    /// same term equality conversion uses, at any depth (binder names are
    /// entropy-fresh, so a free-named subject cannot be captured by an inner
    /// scope) — with its birth binder's name. Top-down: an outer match wins
    /// and is not descended into. Subjects are pairwise distinct by
    /// construction, so the match is unambiguous. (A subject that is exactly
    /// a scope's whole body is missed — scope bodies bypass `visit_subterm` —
    /// which the round-trip verification in `solve` catches conservatively.)
    fn abstract_occurrences(t: &Term, subjects: &[(Term, String)]) -> Term {
        if let Some((_, name)) = subjects.iter().find(|(s, _)| s == t) {
            return Term::var(Var::free(name));
        }

        let owned = subjects.to_vec();
        t.traverse(&mut Visit::rewriting(
            |_, _| None,
            Box::new(move |_, term: &Term| {
                owned
                    .iter()
                    .find(|(s, _)| s == term)
                    .map(|(_, name)| Term::var(Var::free(name.as_str())))
            }),
        ))
    }

    /// `Some(metavar)` iff `term` is an unsolved bare metavariable head.
    /// (`reduce` already resolves solved metavariables, so a metavariable
    /// surviving to weak-head normal form is necessarily unsolved.)
    fn as_metavar(term: &Term) -> Option<&Metavar> {
        match &**term {
            Subterm::Metavar(metavar) => Some(metavar),
            _ => None,
        }
    }

    /// Solve `?id[spine] ≈ t` (the rigid side, already in weak-head normal
    /// form). Implements §7.3 in the pattern fragment: embedded-metavariable
    /// guard, occurs check, spine-as-renaming inversion (which subsumes the
    /// scope check), and re-validation against the frozen birth context,
    /// before committing the solution in birth-named form.
    fn solve(
        &mut self,
        context: &mut Context,
        metavar: &Metavar,
        t: &Term,
    ) -> Result<Solved, ReduceError> {
        let id = metavar.id;
        let metavars = t.metavars();

        // Occurs check: a candidate mentioning `id` itself is an infinite solution.
        if metavars.contains(&id) {
            return Ok(Solved::Failed);
        }

        // Embedded-metavariable guard: any *other* unsolved metavariable in the
        // candidate may carry a wider context than `id`'s, so solving now could
        // let the solution escape its scope. Postpone (the stand-in for pruning).
        if metavars
            .iter()
            .any(|other| context.metavar_solution(*other).is_none())
        {
            return Ok(Solved::Postponed);
        }

        let Some(entry) = context.metavar_entry(id) else {
            // No birth record (e.g. a synthesis-position hole that never reached
            // a checking site): nothing to validate against, cannot solve.
            return Ok(Solved::Failed);
        };
        let telescope = entry.telescope.clone();
        let result = entry.result.clone();

        // Every birthed occurrence carries its full spine: `elaborate_apply`
        // opens telescopes with rebuilt arguments, so no lowered bare copy of
        // a birthed hole survives into compared types. (An empty telescope's
        // identity spine is legitimately empty.)
        assert_eq!(
            metavar.spine.len(),
            telescope.len(),
            "metavariable spine arity diverged from its birth telescope"
        );

        // Resolve *solved-metavariable* entries to their values first — a
        // solved entry stands for its (possibly variable) value, and chains
        // terminate because the occurs check forbids solution cycles. Entries
        // are otherwise deliberately unreduced: a name backed by a definition
        // must stay that name, or an obviously-invertible renaming looks
        // flexible.
        let entries = metavar
            .spine
            .iter()
            .map(|term| {
                let mut entry = term.clone();
                while let Subterm::Metavar(m) = &*entry {
                    match context.resolve_metavar(m) {
                        Some(resolved) => entry = resolved,
                        None => break,
                    }
                }
                entry
            })
            .collect::<Vec<_>>();

        // Invert the spine through its *pattern* entries — a syntactic free
        // variable whose name no other entry shares. A non-variable or
        // duplicated entry is simply not invertible; the solution then may not
        // depend on that slot, which the scope check below enforces — pruning
        // in its simplest form.
        let image: Vec<_> = {
            let names = entries
                .iter()
                .map(|term| match &**term {
                    Subterm::Var(var) => var.as_free(),
                    _ => None,
                })
                .collect::<Vec<_>>();

            names
                .iter()
                .zip(telescope.iter())
                .filter_map(|(name, (birth, _))| {
                    let name = (*name)?;
                    // A duplicated image name is ambiguous to invert.
                    let unique = names.iter().filter(|n| **n == Some(name)).count() == 1;
                    unique.then(|| (name.to_string(), birth.as_str()))
                })
                .collect()
        };

        // Non-pattern entries that are meta-free and pairwise distinct become
        // *abstraction subjects*: every occurrence of the entry inside the
        // candidate rewrites to the entry's birth binder, extending inversion
        // beyond the pattern fragment (the practical "abstracting over
        // non-variable terms" move; the choice of all occurrences is checked
        // by the round-trip verification below and by re-validation). An
        // entry embedding a metavariable, or equal to another entry, stays
        // ambiguous, and the candidate may not depend on it. Each subject
        // contributes both its spellings — as written, and as the reducer
        // exposes it at a whnf position (the candidate's root arrives reduced
        // while deep positions do not) — except a reduced form that is a bare
        // variable, which would collide with the renaming machinery.
        let mut subjects = Vec::new();
        for (entry, (birth, _)) in entries.iter().zip(telescope.iter()) {
            if matches!(&**entry, Subterm::Var(_))
                || !entry.metavars().is_empty()
                || entries.iter().filter(|e| *e == entry).count() != 1
            {
                continue;
            }

            subjects.push((entry.clone(), birth.clone()));

            // An entry the type level may not reduce (an effectful scrutinee,
            // say) simply contributes no reduced spelling — only preemption
            // propagates.
            let reduced = match reduce(context, entry.clone()) {
                Ok(reduced) => reduced,
                Err(ReduceError::Preempted) => return Err(ReduceError::Preempted),
                Err(_) => continue,
            };
            let ambiguous = matches!(&*reduced, Subterm::Var(_))
                || entries.contains(&reduced)
                || subjects.iter().any(|(s, _)| *s == reduced);
            if !ambiguous {
                subjects.push((reduced, birth.clone()));
            }
        }

        let abstracted = match subjects.is_empty() {
            true => t.clone(),
            false => Self::abstract_occurrences(t, &subjects),
        };

        // Scope check, through the inversion: every free variable of the
        // (abstracted) candidate must correspond to exactly one birth binder.
        // A name that is no entry at all can never become one — out of scope;
        // a name only reachable through a non-pattern or duplicated slot is
        // not provably determined — postpone.
        let allowed = image
            .iter()
            .map(|(name, _)| name.clone())
            .chain(subjects.iter().map(|(_, birth)| birth.clone()))
            .collect::<HashSet<_>>();
        for name in abstracted.free_vars() {
            if allowed.contains(&name) {
                continue;
            }
            let mentioned = entries.is_empty()
                || entries
                    .iter()
                    .any(|entry| entry.free_vars().contains(&name));
            return Ok(match mentioned {
                true => Solved::Postponed,
                false => Solved::Failed,
            });
        }

        // Invert, storing the solution in birth-named form. The identity
        // renaming (every invertible entry still its own birth binder, and
        // nothing abstracted) skips the rewrite.
        let inverted = if subjects.is_empty() && image.iter().all(|(img, birth)| img == birth) {
            t.clone()
        } else {
            let labels = image
                .iter()
                .map(|(name, _)| name.as_str())
                .collect::<Vec<_>>();
            let birth_vars = image
                .iter()
                .map(|(_, birth)| Term::var(Var::free(*birth)))
                .collect::<Vec<_>>();
            let refs = birth_vars.iter().collect::<Vec<_>>();
            abstracted.capture(&labels).release(&refs)
        };

        // The equation must hold by construction: resolving the candidate
        // solution back through this occurrence's spine must reproduce the
        // candidate. This guards the whole abstraction/inversion pair — a
        // missed occurrence or an unfaithful rename postpones instead of
        // committing a wrong solution. Syntactic equality is the fast path;
        // an abstraction that matched a *reduced* spelling resolves back to
        // the raw one, so the fallback criterion is definitional — a strict
        // conversion, which cannot solve anything here since both sides are
        // meta-free past the embedded guard.
        if !metavar.spine.is_empty() {
            let labels = telescope
                .iter()
                .map(|(name, _)| name.as_str())
                .collect::<Vec<_>>();
            let refs = entries.iter().collect::<Vec<_>>();
            let resolved = inverted.capture(&labels).release(&refs);
            if resolved != *t && !convert(context, &Term::type_(), &resolved, t)? {
                return Ok(Solved::Postponed);
            }
        }

        // Re-validation (§7.4): the (inverted) candidate must *check* against the
        // metavariable's frozen result type, under its birth context Γ, as an
        // *oracle* — counterfactual refinements and constraint parking both
        // suppressed (see `Context::with_oracle`). Stable definitions are kept.
        // Checking (rather than synthesizing then converting) admits candidates
        // that are checkable but not inferable — a bare lambda whose domain only
        // `result` knows, an unannotated tuple — which are still the correct
        // solution at the frozen type. The validation run itself can solve
        // *other* metavariables (inference may mint and pin fresh implicits); the
        // mark/rollback bracket unwinds those if the candidate is rejected, so a
        // failed oracle leaves no fingerprints.
        let mark = context.solution_mark();
        let revalidated = context.with_frame(|context| {
            for (name, ty) in telescope.iter() {
                context.assume(name, ty);
            }

            context.with_oracle(|context| match check(context, &inverted, result.clone()) {
                Ok(_) => Ok(true),
                // A meta-free, well-scoped candidate that fails to check against
                // the frozen type is not validly typed here — reject the
                // solution. (Under the oracle's suppressed parking an undecided
                // check surfaces as an error too, and likewise rejects.)
                Err(_) => Ok(false),
            })
        })?;

        if !revalidated {
            context.rollback_solutions(mark);
            return Ok(Solved::Failed);
        }

        context.solve_metavar(id, inverted);
        self.progress = true;
        Ok(Solved::Done)
    }

    /// Solve flex–rigid with a *refinement-free* candidate. The drain reduces
    /// goals under the live frame, where counterfactual match-arm refinements
    /// apply — sound for discharging a goal, but not for committing a
    /// solution: a metavariable must not be pinned to a value that holds only
    /// counterfactually inside an arm (`?k := 0` because the nil arm refined
    /// `n := 0`). When refinements are in scope, re-reduce the original rigid
    /// term with them suppressed and solve against that spelling; refinements
    /// only ever *add* reductions, so a solution found this way still
    /// discharges the refined goal. A goal whose verdict changes under
    /// suppression is the refinement's doing — nothing is globally forced, so
    /// it postpones rather than failing or committing.
    fn solve_refinement_free(
        &mut self,
        context: &mut Context,
        metavar: &Metavar,
        rigid: &Term,
        rigid_raw: &Term,
    ) -> Result<Solved, ReduceError> {
        if !context.has_refinements() {
            return self.solve(context, metavar, rigid);
        }

        let suppressed =
            context.with_suppressed_refinements(|context| reduce(context, rigid_raw.clone()))?;

        // Refinements made no difference for this term: the unguarded path,
        // hard verdicts included.
        if suppressed == *rigid {
            return self.solve(context, metavar, rigid);
        }

        // The unrefined spelling is itself flexible — only the refinement
        // made the side look rigid. Undecided.
        if Self::as_metavar(&suppressed).is_some() {
            return Ok(Solved::Postponed);
        }

        Ok(match self.solve(context, metavar, &suppressed)? {
            Solved::Done => Solved::Done,
            // A verdict the refinement-free spelling cannot reach (out of
            // scope, ill-typed at the birth context) is not a hard failure of
            // the goal — the refined spelling may still discharge it once the
            // metavariable is pinned elsewhere.
            Solved::Postponed | Solved::Failed => Solved::Postponed,
        })
    }

    /// `true` iff `term` — already in weak-head normal form, so a foldable
    /// literal would have folded — is a primitive operation still carrying an
    /// unsolved metavariable: the stuck-on-a-metavariable shape whose
    /// structural mismatches are undecided rather than definite.
    fn prim_blocked_on_metavar(context: &Context, term: &Term) -> bool {
        matches!(&**term, Subterm::Prim(_))
            && term
                .metavars()
                .iter()
                .any(|id| context.metavar_solution(*id).is_none())
    }

    fn outcome(&mut self, context: &mut Context) -> Result<Outcome, ReduceError> {
        loop {
            if !self.drain(context)? {
                return Ok(Outcome::Mismatch);
            }

            // Fixpoint: retry postponed constraints only when a fresh solution
            // since the last sweep could have unblocked them.
            if self.progress && !self.blocked.is_empty() {
                self.pending = std::mem::take(&mut self.blocked).into();
                self.progress = false;
            } else {
                break;
            }
        }

        // A constraint still blocked at quiescence is undecided, not unequal:
        // surrender it to the caller rather than conflating the two.
        Ok(match self.blocked.is_empty() {
            true => Outcome::Converts,
            false => Outcome::Blocked(std::mem::take(&mut self.blocked)),
        })
    }

    /// Drain `pending` once. Returns `Ok(false)` on a hard mismatch; `Ok(true)`
    /// when the queue empties (possibly leaving `blocked` constraints).
    fn drain(&mut self, context: &mut Context) -> Result<bool, ReduceError> {
        while let Some(Goal { type_, this, that }) = self.dequeue(context)? {
            // The unreduced spellings, kept for the flex–rigid case: the
            // reductions below apply counterfactual match-arm refinements,
            // and a candidate *solution* must be derived without them (see
            // `solve_refinement_free`).
            let this_raw = this.clone();
            let that_raw = that.clone();

            let this = reduce(context, this)?;
            let that = reduce(context, that)?;
            let type_ = reduce(context, type_)?;

            if this == that {
                continue;
            }

            // Flexible heads are dispatched before history and before the
            // structural/η fallthrough — a flexible head must never be
            // η-expanded into a spine (§7.1).
            match (
                Self::as_metavar(&this).cloned(),
                Self::as_metavar(&that).cloned(),
            ) {
                (Some(this_m), Some(that_m)) => {
                    // Same head, different spines (node duplication under
                    // different openings): entrywise spine agreement is a
                    // *sufficient* congruence condition. Probed only when
                    // both spines are meta-free — a probe that could solve
                    // metavariables would overcommit, since agreement is not
                    // necessary for the goal to hold.
                    if this_m.id == that_m.id
                        && this_m
                            .spine
                            .iter()
                            .chain(that_m.spine.iter())
                            .all(|entry| entry.metavars().is_empty())
                    {
                        let mut entrywise = true;
                        for (a, b) in this_m.spine.iter().zip(that_m.spine.iter()) {
                            if !convert(context, &Term::type_(), a, b)? {
                                entrywise = false;
                                break;
                            }
                        }
                        if entrywise {
                            continue;
                        }
                    }

                    // Distinct heads (or an undecided probe): v1 flex–flex
                    // does no intersection — postpone.
                    self.blocked.push(Goal { type_, this, that });
                    continue;
                }
                (Some(metavar), None) => {
                    match self.solve_refinement_free(context, &metavar, &that, &that_raw)? {
                        Solved::Done => continue,
                        Solved::Postponed => {
                            self.blocked.push(Goal { type_, this, that });
                            continue;
                        }
                        Solved::Failed => return Ok(false),
                    }
                }
                (None, Some(metavar)) => {
                    match self.solve_refinement_free(context, &metavar, &this, &this_raw)? {
                        Solved::Done => continue,
                        Solved::Postponed => {
                            self.blocked.push(Goal { type_, this, that });
                            continue;
                        }
                        Solved::Failed => return Ok(false),
                    }
                }
                (None, None) => {}
            }

            let goal = Goal {
                type_: type_.clone(),
                this: this.clone(),
                that: that.clone(),
            };

            if self.in_history(&goal) {
                continue;
            }

            let ok = match (Term::unwrap_or_clone(this), Term::unwrap_or_clone(that)) {
                (Subterm::Prim(this), Subterm::Prim(that)) => convert_prim(self, this, that)?,
                // Same-kind matches compare structurally; cross-kind pairs fall
                // through to `eta_expand_neutral` (e.g. proof irrelevance at unit).
                (Subterm::Match(this), Subterm::Match(that))
                    if std::mem::discriminant(&this.cases)
                        == std::mem::discriminant(&that.cases) =>
                {
                    self.compare_match(context, this, that)?
                }
                (Subterm::FuncType(this), Subterm::FuncType(that)) => {
                    self.compare_func_type(context, this, that)?
                }
                (Subterm::Func(this), Subterm::Func(that)) => {
                    self.compare_func(context, this, that, type_.clone())?
                }
                (Subterm::Func(func), other) => {
                    self.eta_expand_func(context, func, other.into(), type_.clone())?
                }
                (other, Subterm::Func(func)) => {
                    self.eta_expand_func(context, func, other.into(), type_.clone())?
                }
                (Subterm::Apply(this), Subterm::Apply(that)) => {
                    self.compare_apply(context, this, that)?
                }
                (Subterm::TupleType(this), Subterm::TupleType(that)) => {
                    self.compare_tuple_type(context, this, that)?
                }
                (Subterm::Tuple(this), Subterm::Tuple(that)) => {
                    self.compare_tuple(context, this, that, type_.clone())?
                }
                (Subterm::Tuple(tuple), other) => {
                    self.eta_expand_tuple(context, tuple, other.into(), type_.clone())?
                }
                (other, Subterm::Tuple(tuple)) => {
                    self.eta_expand_tuple(context, tuple, other.into(), type_.clone())?
                }
                (Subterm::Proj(this), Subterm::Proj(that)) => self.compare_proj(this, that)?,
                (Subterm::UnionType(this), Subterm::UnionType(that)) => {
                    self.compare_union_type(this, that)?
                }
                (Subterm::Variant(this), Subterm::Variant(that)) => {
                    self.compare_variant(context, this, that)?
                }
                (Subterm::StructType(this), Subterm::StructType(that)) => {
                    self.compare_struct_type(this, that)?
                }
                (Subterm::Struct(this), Subterm::Struct(that)) => {
                    self.compare_struct(context, this, that)?
                }
                (Subterm::Rec(this), Subterm::Rec(that)) => {
                    self.compare_rec(context, this, that)?
                }
                (Subterm::Rec(rec), other) => {
                    let tail = unfold_rec(context, rec);
                    self.enqueue(type_, tail, other.into());
                    true
                }
                (other, Subterm::Rec(rec)) => {
                    let tail = unfold_rec(context, rec);
                    self.enqueue(type_, other.into(), tail);
                    true
                }
                (this_n, that_n) => {
                    self.eta_expand_neutral(context, this_n.into(), that_n.into(), type_)?
                }
            };

            if !ok {
                // A structural mismatch where a side is a primitive stuck on
                // an unsolved metavariable is undecided, not provably unequal:
                // solving the metavariable may fold the operation (`?m - 1`
                // against `0` folds once `?m := 1` lands), which no structural
                // rule anticipates. Park the goal instead of failing — rigid-
                // head disagreements, which no solution can repair, still
                // mismatch here. The goal leaves `history` so a retry after
                // fresh progress is not skipped as already-handled.
                if Self::prim_blocked_on_metavar(context, &goal.this)
                    || Self::prim_blocked_on_metavar(context, &goal.that)
                {
                    self.history.remove(&goal);
                    self.blocked.push(goal);
                    continue;
                }

                return Ok(false);
            }
        }

        Ok(true)
    }
}
