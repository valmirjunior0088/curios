use {
    super::{
        Apply, Atom, AtomType, BlnMatch, Context, Func, FuncType, Match, Nat, NatMatch, Preempted,
        Prim, Proj, Rec, Subterm, Telescope, Term, Tuple, TupleType, Type, Var, reduce,
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

    fn compare_prim(&mut self, this: Prim, that: Prim) -> Result<bool, Preempted> {
        match (this, that) {
            (Prim::NatType, Prim::NatType)
            | (Prim::IntType, Prim::IntType)
            | (Prim::FltType, Prim::FltType)
            | (Prim::BinType, Prim::BinType) => Ok(true),
            (Prim::Nat(Nat::Zero), Prim::Nat(Nat::Zero)) => Ok(true),
            (Prim::Nat(Nat::Succ(spine_l, il)), Prim::Nat(Nat::Succ(spine_r, ir))) => {
                if spine_l != spine_r {
                    return Ok(false);
                }
                self.enqueue(Type.into(), il, ir);
                Ok(true)
            }
            (Prim::Int(this), Prim::Int(that)) => Ok(this == that),
            (Prim::Flt(this), Prim::Flt(that)) => Ok(this == that),
            (Prim::Bin(this), Prim::Bin(that)) => Ok(this == that),
            (Prim::ArrType(this), Prim::ArrType(that)) => {
                self.enqueue(Type.into(), this, that);

                Ok(true)
            }
            (Prim::NatEql(this_left, this_right), Prim::NatEql(that_left, that_right))
            | (Prim::NatNeq(this_left, this_right), Prim::NatNeq(that_left, that_right))
            | (Prim::NatAdd(this_left, this_right), Prim::NatAdd(that_left, that_right))
            | (Prim::NatSub(this_left, this_right), Prim::NatSub(that_left, that_right))
            | (Prim::NatMul(this_left, this_right), Prim::NatMul(that_left, that_right))
            | (Prim::NatLt(this_left, this_right), Prim::NatLt(that_left, that_right))
            | (Prim::NatDiv(this_left, this_right), Prim::NatDiv(that_left, that_right))
            | (Prim::NatRem(this_left, this_right), Prim::NatRem(that_left, that_right))
            | (Prim::NatGt(this_left, this_right), Prim::NatGt(that_left, that_right))
            | (Prim::NatLte(this_left, this_right), Prim::NatLte(that_left, that_right))
            | (Prim::NatGte(this_left, this_right), Prim::NatGte(that_left, that_right))
            | (Prim::IntEql(this_left, this_right), Prim::IntEql(that_left, that_right))
            | (Prim::IntNeq(this_left, this_right), Prim::IntNeq(that_left, that_right))
            | (Prim::IntAdd(this_left, this_right), Prim::IntAdd(that_left, that_right))
            | (Prim::IntSub(this_left, this_right), Prim::IntSub(that_left, that_right))
            | (Prim::IntMul(this_left, this_right), Prim::IntMul(that_left, that_right))
            | (Prim::IntDiv(this_left, this_right), Prim::IntDiv(that_left, that_right))
            | (Prim::IntRem(this_left, this_right), Prim::IntRem(that_left, that_right))
            | (Prim::IntLt(this_left, this_right), Prim::IntLt(that_left, that_right))
            | (Prim::IntGt(this_left, this_right), Prim::IntGt(that_left, that_right))
            | (Prim::IntLte(this_left, this_right), Prim::IntLte(that_left, that_right))
            | (Prim::IntGte(this_left, this_right), Prim::IntGte(that_left, that_right))
            | (Prim::FltAdd(this_left, this_right), Prim::FltAdd(that_left, that_right))
            | (Prim::FltSub(this_left, this_right), Prim::FltSub(that_left, that_right))
            | (Prim::FltMul(this_left, this_right), Prim::FltMul(that_left, that_right))
            | (Prim::FltDiv(this_left, this_right), Prim::FltDiv(that_left, that_right))
            | (Prim::FltEql(this_left, this_right), Prim::FltEql(that_left, that_right))
            | (Prim::FltNeq(this_left, this_right), Prim::FltNeq(that_left, that_right))
            | (Prim::FltLt(this_left, this_right), Prim::FltLt(that_left, that_right))
            | (Prim::FltGt(this_left, this_right), Prim::FltGt(that_left, that_right))
            | (Prim::FltLte(this_left, this_right), Prim::FltLte(that_left, that_right))
            | (Prim::FltGte(this_left, this_right), Prim::FltGte(that_left, that_right))
            | (Prim::FltMin(this_left, this_right), Prim::FltMin(that_left, that_right))
            | (Prim::FltMax(this_left, this_right), Prim::FltMax(that_left, that_right))
            | (Prim::BinEql(this_left, this_right), Prim::BinEql(that_left, that_right))
            | (Prim::BinGet(this_left, this_right), Prim::BinGet(that_left, that_right))
            | (Prim::BinAppend(this_left, this_right), Prim::BinAppend(that_left, that_right)) => {
                self.enqueue(Type.into(), this_left, that_left);
                self.enqueue(Type.into(), this_right, that_right);

                Ok(true)
            }
            (Prim::FltNeg(this), Prim::FltNeg(that))
            | (Prim::FltAbs(this), Prim::FltAbs(that))
            | (Prim::FltSqrt(this), Prim::FltSqrt(that))
            | (Prim::FltFloor(this), Prim::FltFloor(that))
            | (Prim::FltCeil(this), Prim::FltCeil(that))
            | (Prim::FltTrunc(this), Prim::FltTrunc(that))
            | (Prim::FltNearest(this), Prim::FltNearest(that))
            | (Prim::NatToStr(this), Prim::NatToStr(that))
            | (Prim::IntToStr(this), Prim::IntToStr(that))
            | (Prim::FltToStr(this), Prim::FltToStr(that))
            | (Prim::NatToInt(this), Prim::NatToInt(that))
            | (Prim::NatToFlt(this), Prim::NatToFlt(that))
            | (Prim::IntToNat(this), Prim::IntToNat(that))
            | (Prim::IntToFlt(this), Prim::IntToFlt(that))
            | (Prim::FltToNat(this), Prim::FltToNat(that))
            | (Prim::FltToInt(this), Prim::FltToInt(that))
            | (Prim::BinLen(this), Prim::BinLen(that)) => {
                self.enqueue(Type.into(), this, that);

                Ok(true)
            }
            (
                Prim::BinSlice(this_bin, this_start, this_end),
                Prim::BinSlice(that_bin, that_start, that_end),
            ) => {
                self.enqueue(Type.into(), this_bin, that_bin);
                self.enqueue(Type.into(), this_start, that_start);
                self.enqueue(Type.into(), this_end, that_end);

                Ok(true)
            }
            (
                Prim::ArrSlice(this_ty, this_list, this_start, this_end),
                Prim::ArrSlice(that_ty, that_list, that_start, that_end),
            ) => {
                self.enqueue(Type.into(), this_ty, that_ty);
                self.enqueue(Type.into(), this_list, that_list);
                self.enqueue(Type.into(), this_start, that_start);
                self.enqueue(Type.into(), this_end, that_end);

                Ok(true)
            }
            (
                Prim::ArrGet(this_ty, this_list, this_index),
                Prim::ArrGet(that_ty, that_list, that_index),
            ) => {
                self.enqueue(Type.into(), this_ty, that_ty);
                self.enqueue(Type.into(), this_list, that_list);
                self.enqueue(Type.into(), this_index, that_index);

                Ok(true)
            }
            (Prim::ArrLen(this_ty, this_list), Prim::ArrLen(that_ty, that_list)) => {
                self.enqueue(Type.into(), this_ty, that_ty);
                self.enqueue(Type.into(), this_list, that_list);

                Ok(true)
            }
            (
                Prim::ArrAppend(this_ty, this_list, this_elem),
                Prim::ArrAppend(that_ty, that_list, that_elem),
            ) => {
                self.enqueue(Type.into(), this_ty, that_ty);
                self.enqueue(Type.into(), this_list, that_list);
                self.enqueue(Type.into(), this_elem, that_elem);

                Ok(true)
            }
            (Prim::Arr(this_elems), Prim::Arr(that_elems)) => {
                if this_elems.len() != that_elems.len() {
                    return Ok(false);
                }

                for (this, that) in this_elems.into_iter().zip(that_elems) {
                    self.enqueue(Type.into(), this, that);
                }

                Ok(true)
            }
            (Prim::BinConcat(this_ops), Prim::BinConcat(that_ops)) => {
                if this_ops.len() != that_ops.len() {
                    return Ok(false);
                }
                for (this, that) in this_ops.into_iter().zip(that_ops) {
                    self.enqueue(Type.into(), this, that);
                }
                Ok(true)
            }
            (Prim::ArrConcat(this_ty, this_ops), Prim::ArrConcat(that_ty, that_ops)) => {
                if this_ops.len() != that_ops.len() {
                    return Ok(false);
                }
                self.enqueue(Type.into(), this_ty, that_ty);
                for (this, that) in this_ops.into_iter().zip(that_ops) {
                    self.enqueue(Type.into(), this, that);
                }
                Ok(true)
            }
            (_, _) => Ok(false),
        }
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
                (Subterm::Prim(this), Subterm::Prim(that)) => self.compare_prim(this, that)?,
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
mod tests {
    use {
        super::*,
        crate::core::{Atom, Nat, Type, Var},
        std::time::Duration,
    };

    fn context() -> Context {
        Context::new(Duration::from_millis(10))
    }

    fn conv(context: &mut Context, this: &Term, that: &Term) -> Result<bool, Preempted> {
        convert(context, &Type.into(), this, that)
    }

    #[test]
    fn convert_func_type_is_alpha_equivalent() {
        let mut context = context();

        let this = Term::func_type([("x", Type)], Var::free("x"));

        let that = Term::func_type([("y", Type)], Var::free("y"));

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_func_is_alpha_equivalent() {
        let mut context = context();

        let this = Term::func(["x"], Var::free("x"));

        let that = Term::func(["y"], Var::free("y"));

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_match_compares_matches_and_motive() {
        let mut context = context();

        let this = Term::match_(
            Atom::from("a"),
            Some("m"),
            Type,
            vec![("a", Atom::from("yes")), ("b", Atom::from("no"))],
        );

        let that = Term::match_(
            Atom::from("a"),
            Some("n"),
            Type,
            vec![("a", Atom::from("yes")), ("b", Atom::from("no"))],
        );

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_recurses_into_operands() {
        let mut context = context();

        let this = Term::func(
            ["x"],
            Subterm::Prim(Prim::int_add(Var::free("x"), Subterm::Prim(Prim::Int(1)))),
        );

        let that = Term::func(
            ["y"],
            Subterm::Prim(Prim::int_add(Var::free("y"), Subterm::Prim(Prim::Int(1)))),
        );

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_distinguishes_operator_kind() {
        let mut context = context();

        let this = Term::func(
            ["x"],
            Subterm::Prim(Prim::int_add(Var::free("x"), Subterm::Prim(Prim::Int(1)))),
        );

        let that = Term::func(
            ["x"],
            Subterm::Prim(Prim::int_sub(Var::free("x"), Subterm::Prim(Prim::Int(1)))),
        );

        assert_eq!(conv(&mut context, &this, &that), Ok(false));
    }

    #[test]
    fn convert_rec_is_alpha_equivalent() {
        let mut context = context();

        let this = Term::rec(vec![("x", Type, Var::free("x"))], Var::free("x"));

        let that = Term::rec(vec![("y", Type, Var::free("y"))], Var::free("y"));

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_nat_add_recurses_into_operands() {
        let mut context = context();

        let this = Term::func(
            ["x"],
            Subterm::Prim(Prim::nat_add(
                Var::free("x"),
                Subterm::Prim(Prim::Nat(Nat::new(1))),
            )),
        );

        let that = Term::func(
            ["y"],
            Subterm::Prim(Prim::nat_add(
                Var::free("y"),
                Subterm::Prim(Prim::Nat(Nat::new(1))),
            )),
        );

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_flt_neg_recurses_into_operand() {
        let mut context = context();

        let this = Term::func(["x"], Subterm::Prim(Prim::flt_neg(Var::free("x"))));

        let that = Term::func(["y"], Subterm::Prim(Prim::flt_neg(Var::free("y"))));

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_nat_to_int_recurses_into_operand() {
        let mut context = context();

        let this = Term::func(["x"], Subterm::Prim(Prim::nat_to_int(Var::free("x"))));

        let that = Term::func(["y"], Subterm::Prim(Prim::nat_to_int(Var::free("y"))));

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_arr_compares_element_wise() {
        let mut context = context();

        let this = Subterm::Prim(Prim::from(vec![
            Subterm::Prim(Prim::Nat(Nat::new(1))),
            Subterm::Prim(Prim::Nat(Nat::new(2))),
        ]))
        .into();

        let that = Subterm::Prim(Prim::from(vec![
            Subterm::Prim(Prim::Nat(Nat::new(1))),
            Subterm::Prim(Prim::Nat(Nat::new(2))),
        ]))
        .into();

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_arr_rejects_different_lengths() {
        let mut context = context();

        let this = Subterm::Prim(Prim::from(vec![Subterm::Prim(Prim::Nat(Nat::new(1)))])).into();

        let that = Subterm::Prim(Prim::from(vec![
            Subterm::Prim(Prim::Nat(Nat::new(1))),
            Subterm::Prim(Prim::Nat(Nat::new(2))),
        ]))
        .into();

        assert_eq!(conv(&mut context, &this, &that), Ok(false));
    }

    #[test]
    fn convert_prim_bin_type_is_equal_to_itself() {
        let mut context = context();

        let this = Subterm::Prim(Prim::BinType).into();
        let that = Subterm::Prim(Prim::BinType).into();

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_bin_literal_compares_bytes() {
        let mut context = context();

        assert_eq!(
            conv(
                &mut context,
                &Subterm::Prim(Prim::Bin(vec![1, 2])).into(),
                &Subterm::Prim(Prim::Bin(vec![1, 2])).into(),
            ),
            Ok(true)
        );

        assert_eq!(
            conv(
                &mut context,
                &Subterm::Prim(Prim::Bin(vec![1, 2])).into(),
                &Subterm::Prim(Prim::Bin(vec![1, 3])).into(),
            ),
            Ok(false)
        );
    }

    #[test]
    fn convert_prim_bin_len_recurses_into_operand() {
        let mut context = context();

        let this = Term::func(["x"], Subterm::Prim(Prim::bin_len(Var::free("x"))));
        let that = Term::func(["y"], Subterm::Prim(Prim::bin_len(Var::free("y"))));

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_bin_get_recurses_into_operands() {
        let mut context = context();

        let this = Term::func(
            ["x"],
            Term::func(
                ["a"],
                Subterm::Prim(Prim::bin_get(Var::free("x"), Var::free("a"))),
            ),
        );

        let that = Term::func(
            ["y"],
            Term::func(
                ["b"],
                Subterm::Prim(Prim::bin_get(Var::free("y"), Var::free("b"))),
            ),
        );

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_bin_concat_recurses_into_operands() {
        let mut context = context();

        let this = Term::func(
            ["x"],
            Term::func(
                ["a"],
                Subterm::Prim(Prim::bin_concat([Var::free("x"), Var::free("a")])),
            ),
        );

        let that = Term::func(
            ["y"],
            Term::func(
                ["b"],
                Subterm::Prim(Prim::bin_concat([Var::free("y"), Var::free("b")])),
            ),
        );

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_bin_slice_recurses_into_operands() {
        let mut context = context();

        let this = Term::func(
            ["x"],
            Term::func(
                ["a"],
                Term::func(
                    ["p"],
                    Subterm::Prim(Prim::bin_slice(
                        Var::free("x"),
                        Var::free("a"),
                        Var::free("p"),
                    )),
                ),
            ),
        );

        let that = Term::func(
            ["y"],
            Term::func(
                ["b"],
                Term::func(
                    ["q"],
                    Subterm::Prim(Prim::bin_slice(
                        Var::free("y"),
                        Var::free("b"),
                        Var::free("q"),
                    )),
                ),
            ),
        );

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_tuple_equal() {
        let mut context = context();

        let this = Term::tuple([Term::from(Atom::from("x")), Term::from(Atom::from("y"))]);
        let that = Term::tuple([Term::from(Atom::from("x")), Term::from(Atom::from("y"))]);

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_tuple_unequal_field() {
        let mut context = context();

        let this = Term::tuple([Term::from(Atom::from("x")), Term::from(Atom::from("y"))]);
        let that = Term::tuple([Term::from(Atom::from("x")), Term::from(Atom::from("z"))]);

        assert_eq!(conv(&mut context, &this, &that), Ok(false));
    }

    #[test]
    fn convert_proj_same_index_and_head() {
        let mut context = context();

        let this = Term::proj(Var::free("r"), 0);
        let that = Term::proj(Var::free("r"), 0);

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_proj_different_index_is_false() {
        let mut context = context();

        let this = Term::proj(Var::free("r"), 0);
        let that = Term::proj(Var::free("r"), 1);

        assert_eq!(conv(&mut context, &this, &that), Ok(false));
    }

    #[test]
    fn convert_eta_tuple_neutral_with_known_type() {
        let mut context = context();

        let tuple_type: Term = Term::tuple_type([
            ("x", Term::atom_type(["a", "b"])),
            ("y", Term::atom_type(["c", "d"])),
        ]);

        let r: Term = Var::free("r").into();
        let s: Term = Var::free("s").into();

        assert_eq!(convert(&mut context, &tuple_type, &r, &r), Ok(true));

        assert_eq!(convert(&mut context, &tuple_type, &r, &s), Ok(false));
    }

    #[test]
    fn convert_partial_projection_tuple_at_narrow_type() {
        let mut context = context();

        // p = (a, b), q = (a, c) — both 2-tuples agreeing on field 0, differing on field 1.
        context.define("p", &Term::tuple([Atom::from("a"), Atom::from("b")]).into());
        context.define("q", &Term::tuple([Atom::from("a"), Atom::from("c")]).into());

        // Type is a 1-field tuple type {A : {a}}.
        let type_: Term = Term::tuple_type([("x", Term::atom_type(["a"]))]);

        // this = (p.0), that = (q.0). At the 1-field type both denote (a),
        // so conversion should return true.
        let this: Term = Term::tuple([Term::proj(Var::free("p"), 0)]);
        let that: Term = Term::tuple([Term::proj(Var::free("q"), 0)]);

        // Even though eta_reduce_tuple widens each 1-tuple to its bare base
        // (`Var p`, `Var q`), the convert loop then routes the neutral pair
        // through `eta_expand_neutral`, which re-projects according to the
        // TRUE type telescope (1 field). Each `proj(_, 0)` then reduces to
        // `a`, so the comparison succeeds — the bug is masked here.
        assert_eq!(convert(&mut context, &type_, &this, &that), Ok(true));
    }

    #[test]
    fn convert_times_out_on_pathological_inputs() {
        let mut context = context();

        context.define("loop", &Var::free("loop").into());

        let this = Term::tuple_type([
            (
                "x",
                Term::apply(Term::func(["z"], Var::free("z")), [Var::free("loop")]),
            ),
            ("y", Term::from(Var::free("x"))),
        ]);

        let that = Term::tuple_type([("x", Var::free("loop")), ("y", Var::free("x"))]);

        assert_eq!(conv(&mut context, &this, &that), Err(Preempted));
    }
}
