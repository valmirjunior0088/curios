mod compare_prim;
use compare_prim::*;

use {
    super::{
        Apply, Atom, AtomType, BlnMatch, Context, Func, FuncType, Match, NatMatch, Preempted, Proj,
        Rec, Subterm, Telescope, Term, Tuple, TupleType, Type, Var, reduce,
    },
    std::{
        collections::{HashSet, VecDeque},
        time::Instant,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Goal {
    pub type_: Term,
    pub this: Term,
    pub that: Term,
}

pub fn convert(
    context: &mut Context,
    type_: &Term,
    this: &Term,
    that: &Term,
) -> Result<bool, Preempted> {
    Convert::new(type_.clone(), this.clone(), that.clone()).convert(context)
}

#[derive(Debug)]
struct Convert {
    history: HashSet<Goal>,
    pending: VecDeque<Goal>,
}

impl Convert {
    fn new(type_: Term, this: Term, that: Term) -> Self {
        Self {
            history: HashSet::new(),
            pending: VecDeque::from([Goal { type_, this, that }]),
        }
    }

    fn in_history(&mut self, goal: &Goal) -> bool {
        !self.history.insert(goal.clone())
    }

    fn enqueue(&mut self, type_: Term, this: Term, that: Term) {
        self.pending.push_back(Goal { type_, this, that });
    }

    fn dequeue(&mut self, context: &Context) -> Result<Option<Goal>, Preempted> {
        if Instant::now() > context.deadline() {
            return Err(Preempted);
        }

        Ok(self.pending.pop_front())
    }

    fn compare_func_type(
        &mut self,
        context: &mut Context,
        this: FuncType,
        that: FuncType,
    ) -> Result<bool, Preempted> {
        fn walk(
            cmp: &mut Convert,
            context: &mut Context,
            this: &Telescope<Term>,
            that: &Telescope<Term>,
        ) -> Result<bool, Preempted> {
            match (this, that) {
                (Telescope::Cons(ty_a, rest_a), Telescope::Cons(ty_b, rest_b)) => {
                    cmp.enqueue(Type.into(), ty_a.clone(), ty_b.clone());
                    let v = Term::from(Var::free(context.fresh(rest_a.first_label())));
                    let inner_a = rest_a.open(&[&v]);
                    let inner_b = rest_b.open(&[&v]);
                    walk(cmp, context, &inner_a, &inner_b)
                }
                (Telescope::Done(out_a), Telescope::Done(out_b)) => {
                    cmp.enqueue(Type.into(), (**out_a).clone(), (**out_b).clone());
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
    ) -> Result<bool, Preempted> {
        let n = this.body.arity();
        let ys: Vec<Term> = (0..n)
            .map(|_| Var::free(context.fresh(None)).into())
            .collect();
        let y_refs: Vec<&Term> = ys.iter().collect();
        let output_type = match Term::unwrap_or_clone(reduce(context, type_)?) {
            Subterm::FuncType(FuncType { telescope }) => telescope.open(&y_refs),
            _ => Type.into(),
        };
        self.enqueue(
            output_type,
            this.body.open(&y_refs),
            that.body.open(&y_refs),
        );

        Ok(true)
    }

    fn compare_apply(&mut self, this: Apply, that: Apply) -> Result<bool, Preempted> {
        if this.params.len() != that.params.len() {
            return Ok(false);
        }
        self.enqueue(Type.into(), this.head, that.head);
        for (a, b) in this.params.into_iter().zip(that.params) {
            self.enqueue(Type.into(), a, b);
        }
        Ok(true)
    }

    fn compare_tuple_type(
        &mut self,
        context: &mut Context,
        this: TupleType,
        that: TupleType,
    ) -> Result<bool, Preempted> {
        fn walk(
            cmp: &mut Convert,
            context: &mut Context,
            this: &Telescope<()>,
            that: &Telescope<()>,
        ) -> Result<bool, Preempted> {
            match (this, that) {
                (Telescope::Cons(ty_a, rest_a), Telescope::Cons(ty_b, rest_b)) => {
                    cmp.enqueue(Type.into(), ty_a.clone(), ty_b.clone());
                    let v = Term::from(Var::free(context.fresh(rest_a.first_label())));
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
    ) -> Result<bool, Preempted> {
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
                _ => Type.into(),
            };
            self.enqueue(ft, a.clone(), b.clone());
        }

        Ok(true)
    }

    fn compare_proj(&mut self, this: Proj, that: Proj) -> Result<bool, Preempted> {
        if this.index != that.index {
            return Ok(false);
        }
        self.enqueue(Type.into(), this.head, that.head);
        Ok(true)
    }

    fn compare_atom_type(&mut self, this: AtomType, that: AtomType) -> Result<bool, Preempted> {
        Ok(this == that)
    }

    fn compare_atom(&mut self, this: Atom, that: Atom) -> Result<bool, Preempted> {
        Ok(this == that)
    }

    fn compare_bln_match(
        &mut self,
        context: &mut Context,
        this: BlnMatch,
        that: BlnMatch,
    ) -> Result<bool, Preempted> {
        self.enqueue(Type.into(), this.head, that.head);

        let label = Var::free(context.fresh(None)).into();
        self.enqueue(
            Type.into(),
            this.motive.open(&[&label]),
            that.motive.open(&[&label]),
        );

        self.enqueue(Type.into(), this.false_case, that.false_case);
        self.enqueue(Type.into(), this.true_case, that.true_case);
        Ok(true)
    }

    fn compare_nat_match(
        &mut self,
        context: &mut Context,
        this: NatMatch,
        that: NatMatch,
    ) -> Result<bool, Preempted> {
        match (this, that) {
            (
                NatMatch::Induction {
                    head: this_head,
                    motive: this_motive,
                    zero_case: this_zero,
                    succ_case: this_succ,
                },
                NatMatch::Induction {
                    head: that_head,
                    motive: that_motive,
                    zero_case: that_zero,
                    succ_case: that_succ,
                },
            ) => {
                self.enqueue(Type.into(), this_head, that_head);

                let motive_label = Var::free(context.fresh(None)).into();
                self.enqueue(
                    Type.into(),
                    this_motive.open(&[&motive_label]),
                    that_motive.open(&[&motive_label]),
                );

                self.enqueue(Type.into(), this_zero, that_zero);

                let pred_label: Term = Var::free(context.fresh(None)).into();
                let ih_label: Term = Var::free(context.fresh(None)).into();
                self.enqueue(
                    Type.into(),
                    this_succ.open(&[&pred_label, &ih_label]),
                    that_succ.open(&[&pred_label, &ih_label]),
                );

                Ok(true)
            }
            (
                NatMatch::Dispatch {
                    head: this_head,
                    motive: this_motive,
                    cases: this_cases,
                    default: this_default,
                },
                NatMatch::Dispatch {
                    head: that_head,
                    motive: that_motive,
                    cases: that_cases,
                    default: that_default,
                },
            ) => {
                self.enqueue(Type.into(), this_head, that_head);

                let label = Var::free(context.fresh(None)).into();
                self.enqueue(
                    Type.into(),
                    this_motive.open(&[&label]),
                    that_motive.open(&[&label]),
                );

                if this_cases.len() != that_cases.len() {
                    return Ok(false);
                }

                for ((kl, vl), (kr, vr)) in this_cases.into_iter().zip(that_cases) {
                    if kl != kr {
                        return Ok(false);
                    }
                    self.enqueue(Type.into(), vl, vr);
                }

                self.enqueue(Type.into(), this_default, that_default);
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn compare_match(
        &mut self,
        context: &mut Context,
        this: Match,
        that: Match,
    ) -> Result<bool, Preempted> {
        self.enqueue(Type.into(), this.head, that.head);

        let label = Var::free(context.fresh(None)).into();
        self.enqueue(
            Type.into(),
            this.motive.open(&[&label]),
            that.motive.open(&[&label]),
        );

        if this.cases.len() != that.cases.len() {
            return Ok(false);
        }

        for ((this_atom, this_body), (that_atom, that_body)) in
            this.cases.into_iter().zip(that.cases)
        {
            if this_atom != that_atom {
                return Ok(false);
            }

            self.enqueue(Type.into(), this_body, that_body);
        }

        Ok(true)
    }

    fn compare_rec(
        &mut self,
        context: &mut Context,
        this: Rec,
        that: Rec,
    ) -> Result<bool, Preempted> {
        if this.items.len() != that.items.len() {
            return Ok(false);
        }

        let labels = (0..this.items.len())
            .map(|_| Var::free(context.fresh(None)).into())
            .collect::<Vec<_>>();

        let labels = labels.iter().collect::<Vec<_>>();

        for ((this_type, this_body), (that_type, that_body)) in
            this.items.into_iter().zip(that.items)
        {
            self.enqueue(
                Type.into(),
                this_type.open(&labels),
                that_type.open(&labels),
            );
            self.enqueue(
                Type.into(),
                this_body.open(&labels),
                that_body.open(&labels),
            );
        }

        self.enqueue(
            Type.into(),
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
    ) -> Result<bool, Preempted> {
        let n = func.body.arity();
        let ys: Vec<Term> = (0..n)
            .map(|_| Var::free(context.fresh(None)).into())
            .collect();
        let y_refs: Vec<&Term> = ys.iter().collect();
        let output_type = match Term::unwrap_or_clone(reduce(context, type_)?) {
            Subterm::FuncType(FuncType { telescope }) => telescope.open(&y_refs),
            _ => Type.into(),
        };
        self.enqueue(output_type, func.body.open(&y_refs), Term::apply(other, ys));
        Ok(true)
    }

    fn eta_expand_tuple(
        &mut self,
        context: &mut Context,
        tuple: Tuple,
        other: Term,
        type_: Term,
    ) -> Result<bool, Preempted> {
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
                _ => Type.into(),
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
    ) -> Result<bool, Preempted> {
        match Term::unwrap_or_clone(reduce(context, type_)?) {
            Subterm::FuncType(FuncType { telescope }) => {
                let n = telescope.len();
                let ys: Vec<Term> = (0..n)
                    .map(|_| Var::free(context.fresh(None)).into())
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
                        Type.into(),
                        Term::proj(this.clone(), i),
                        Term::proj(that.clone(), i),
                    );
                }
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn convert(&mut self, context: &mut Context) -> Result<bool, Preempted> {
        while let Some(Goal { type_, this, that }) = self.dequeue(context)? {
            let this = reduce(context, this)?;
            let that = reduce(context, that)?;
            let type_ = reduce(context, type_)?;

            let goal = Goal {
                type_: type_.clone(),
                this: this.clone(),
                that: that.clone(),
            };

            if this == that || self.in_history(&goal) {
                continue;
            }

            let ok = match (Term::unwrap_or_clone(this), Term::unwrap_or_clone(that)) {
                (Subterm::Prim(this), Subterm::Prim(that)) => compare_prim(self, this, that)?,
                (Subterm::BlnMatch(this), Subterm::BlnMatch(that)) => {
                    self.compare_bln_match(context, this, that)?
                }
                (Subterm::NatMatch(this), Subterm::NatMatch(that)) => {
                    self.compare_nat_match(context, this, that)?
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
                (Subterm::Apply(this), Subterm::Apply(that)) => self.compare_apply(this, that)?,
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
                (Subterm::AtomType(this), Subterm::AtomType(that)) => {
                    self.compare_atom_type(this, that)?
                }
                (Subterm::Atom(this), Subterm::Atom(that)) => self.compare_atom(this, that)?,
                (Subterm::Match(this), Subterm::Match(that)) => {
                    self.compare_match(context, this, that)?
                }
                (Subterm::Rec(this), Subterm::Rec(that)) => {
                    self.compare_rec(context, this, that)?
                }
                (this_n, that_n) => {
                    self.eta_expand_neutral(context, this_n.into(), that_n.into(), type_)?
                }
            };

            if !ok {
                return Ok(false);
            }
        }

        Ok(true)
    }
}

#[cfg(test)]
mod tests;
