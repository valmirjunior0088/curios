use {
    super::{
        Apply, Atom, AtomType, BlnMatch, Context, Func, FuncType, Match, NatFold, NatMatch,
        Preempted, Prim, Proj, Rec, Seal, Sealed, Term, Tuple, TupleType, Type, Unseal, Var,
        reduce,
    },
    std::{
        collections::{HashSet, VecDeque},
        time::{Duration, Instant},
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
    Convert::new(context.timeout(), type_.clone(), this.clone(), that.clone()).convert(context)
}

#[derive(Debug)]
struct Convert {
    deadline: Instant,
    history: HashSet<Goal>,
    pending: VecDeque<Goal>,
}

impl Convert {
    fn new(timeout: Duration, type_: Term, this: Term, that: Term) -> Self {
        Self {
            deadline: Instant::now() + timeout,
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

    fn dequeue(&mut self) -> Result<Option<Goal>, Preempted> {
        if Instant::now() > self.deadline {
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
            (Prim::Nat(this), Prim::Nat(that)) => Ok(this == that),
            (Prim::Int(this), Prim::Int(that)) => Ok(this == that),
            (Prim::Flt(this), Prim::Flt(that)) => Ok(this == that),
            (Prim::Bin(this), Prim::Bin(that)) => Ok(this == that),
            (Prim::ArrType(this), Prim::ArrType(that)) => {
                self.enqueue(Type.into(), *this, *that);

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
            | (Prim::ArrGet(this_left, this_right), Prim::ArrGet(that_left, that_right))
            | (Prim::ArrAppend(this_left, this_right), Prim::ArrAppend(that_left, that_right))
            | (Prim::BinEql(this_left, this_right), Prim::BinEql(that_left, that_right))
            | (Prim::BinGet(this_left, this_right), Prim::BinGet(that_left, that_right))
            | (Prim::BinAppend(this_left, this_right), Prim::BinAppend(that_left, that_right)) => {
                self.enqueue(Type.into(), *this_left, *that_left);
                self.enqueue(Type.into(), *this_right, *that_right);

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
            | (Prim::ArrLen(this), Prim::ArrLen(that))
            | (Prim::BinLen(this), Prim::BinLen(that)) => {
                self.enqueue(Type.into(), *this, *that);

                Ok(true)
            }
            (
                Prim::BinSlice(this_bin, this_start, this_end),
                Prim::BinSlice(that_bin, that_start, that_end),
            ) => {
                self.enqueue(Type.into(), *this_bin, *that_bin);
                self.enqueue(Type.into(), *this_start, *that_start);
                self.enqueue(Type.into(), *this_end, *that_end);

                Ok(true)
            }
            (
                Prim::ArrSlice(this_list, this_start, this_end),
                Prim::ArrSlice(that_list, that_start, that_end),
            ) => {
                self.enqueue(Type.into(), *this_list, *that_list);
                self.enqueue(Type.into(), *this_start, *that_start);
                self.enqueue(Type.into(), *this_end, *that_end);

                Ok(true)
            }
            (Prim::Arr(this_elems), Prim::Arr(that_elems)) => {
                if this_elems.len() != that_elems.len() {
                    return Ok(false);
                }

                for (this, that) in this_elems.into_iter().zip(that_elems) {
                    self.enqueue(Type.into(), *this, *that);
                }

                Ok(true)
            }
            (Prim::BinConcat(this_ops), Prim::BinConcat(that_ops)) => {
                if this_ops.len() != that_ops.len() {
                    return Ok(false);
                }
                for (this, that) in this_ops.into_iter().zip(that_ops) {
                    self.enqueue(Type.into(), *this, *that);
                }
                Ok(true)
            }
            (Prim::ArrConcat(this_ops), Prim::ArrConcat(that_ops)) => {
                if this_ops.len() != that_ops.len() {
                    return Ok(false);
                }
                for (this, that) in this_ops.into_iter().zip(that_ops) {
                    self.enqueue(Type.into(), *this, *that);
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
        self.enqueue(Type.into(), *this.input, *that.input);

        let label = Var::free(context.fresh()).into();
        self.enqueue(
            Type.into(),
            this.output.open(&[&label]),
            that.output.open(&[&label]),
        );

        Ok(true)
    }

    fn compare_func(
        &mut self,
        context: &mut Context,
        this: Func,
        that: Func,
        type_: Term,
    ) -> Result<bool, Preempted> {
        let y: Term = Var::free(context.fresh()).into();
        let output_type = match reduce(context, &type_)? {
            Term::FuncType(FuncType { output, .. }) => output.open(&[&y]),
            _ => Type.into(),
        };
        self.enqueue(output_type, this.body.open(&[&y]), that.body.open(&[&y]));

        Ok(true)
    }

    fn compare_apply(&mut self, this: Apply, that: Apply) -> Result<bool, Preempted> {
        self.enqueue(Type.into(), *this.head, *that.head);
        self.enqueue(Type.into(), *this.param, *that.param);

        Ok(true)
    }

    fn compare_tuple_type(
        &mut self,
        context: &mut Context,
        this: TupleType,
        that: TupleType,
    ) -> Result<bool, Preempted> {
        if this.fields.len() != that.fields.len() {
            return Ok(false);
        }

        let n = this.fields.len();
        let labels = (0..n)
            .map(|_| Term::from(Var::free(context.fresh())))
            .collect::<Vec<_>>();
        let label_refs = labels.iter().collect::<Vec<_>>();

        for i in 0..n {
            self.enqueue(
                Type.into(),
                this.fields[i].open(&label_refs[..i]),
                that.fields[i].open(&label_refs[..i]),
            );
        }

        Ok(true)
    }

    fn compare_tuple(&mut self, this: Tuple, that: Tuple) -> Result<bool, Preempted> {
        if this.fields.len() != that.fields.len() {
            return Ok(false);
        }

        for (a, b) in this.fields.into_iter().zip(that.fields) {
            self.enqueue(Type.into(), *a, *b);
        }

        Ok(true)
    }

    fn compare_proj(&mut self, this: Proj, that: Proj) -> Result<bool, Preempted> {
        if this.index != that.index {
            return Ok(false);
        }
        self.enqueue(Type.into(), *this.head, *that.head);
        Ok(true)
    }

    fn compare_nat_fold(
        &mut self,
        context: &mut Context,
        this: NatFold,
        that: NatFold,
    ) -> Result<bool, Preempted> {
        self.enqueue(Type.into(), *this.head, *that.head);

        let motive_label = Var::free(context.fresh()).into();
        self.enqueue(
            Type.into(),
            this.motive.open(&[&motive_label]),
            that.motive.open(&[&motive_label]),
        );

        self.enqueue(Type.into(), *this.zero_case, *that.zero_case);

        let pred_label: Term = Var::free(context.fresh()).into();
        let ih_label: Term = Var::free(context.fresh()).into();
        self.enqueue(
            Type.into(),
            this.succ_case.open(&[&pred_label, &ih_label]),
            that.succ_case.open(&[&pred_label, &ih_label]),
        );

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
        self.enqueue(Type.into(), *this.head, *that.head);

        let label = Var::free(context.fresh()).into();
        self.enqueue(
            Type.into(),
            this.motive.open(&[&label]),
            that.motive.open(&[&label]),
        );

        self.enqueue(Type.into(), *this.false_case, *that.false_case);
        self.enqueue(Type.into(), *this.true_case, *that.true_case);
        Ok(true)
    }

    fn compare_nat_match(
        &mut self,
        context: &mut Context,
        this: NatMatch,
        that: NatMatch,
    ) -> Result<bool, Preempted> {
        self.enqueue(Type.into(), *this.head, *that.head);

        let label = Var::free(context.fresh()).into();
        self.enqueue(
            Type.into(),
            this.motive.open(&[&label]),
            that.motive.open(&[&label]),
        );

        if this.cases.len() != that.cases.len() {
            return Ok(false);
        }

        for ((kl, vl), (kr, vr)) in this.cases.into_iter().zip(that.cases) {
            if kl != kr {
                return Ok(false);
            }
            self.enqueue(Type.into(), *vl, *vr);
        }

        self.enqueue(Type.into(), *this.default, *that.default);
        Ok(true)
    }

    fn compare_match(
        &mut self,
        context: &mut Context,
        this: Match,
        that: Match,
    ) -> Result<bool, Preempted> {
        self.enqueue(Type.into(), *this.head, *that.head);

        let label = Var::free(context.fresh()).into();
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

            self.enqueue(Type.into(), *this_body, *that_body);
        }

        Ok(true)
    }

    fn compare_sealed(
        &mut self,
        context: &mut Context,
        this: Sealed,
        that: Sealed,
    ) -> Result<bool, Preempted> {
        let label = Var::free(context.fresh()).into();
        self.enqueue(Type.into(), *this.witness, *that.witness);
        self.enqueue(
            Type.into(),
            this.tail.open(&[&label]),
            that.tail.open(&[&label]),
        );
        Ok(true)
    }

    fn compare_seal(&mut self, this: Seal, that: Seal) -> Result<bool, Preempted> {
        self.enqueue(Type.into(), *this.witness, *that.witness);
        self.enqueue(Type.into(), *this.value, *that.value);
        Ok(true)
    }

    fn compare_unseal(&mut self, this: Unseal, that: Unseal) -> Result<bool, Preempted> {
        self.enqueue(Type.into(), *this.witness, *that.witness);
        self.enqueue(Type.into(), *this.value, *that.value);
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
            .map(|_| Var::free(context.fresh()).into())
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
        let y: Term = Var::free(context.fresh()).into();
        let output_type = match reduce(context, &type_)? {
            Term::FuncType(FuncType { output, .. }) => output.open(&[&y]),
            _ => Type.into(),
        };
        self.enqueue(
            output_type,
            func.body.open(&[&y]),
            Apply::new(other, y).into(),
        );
        Ok(true)
    }

    fn eta_expand_tuple(&mut self, tuple: Tuple, other: Term) -> Result<bool, Preempted> {
        for (i, field) in tuple.fields.into_iter().enumerate() {
            self.enqueue(Type.into(), *field, Proj::new(other.clone(), i).into());
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
        match reduce(context, &type_)? {
            Term::FuncType(FuncType { output, .. }) => {
                let y: Term = Var::free(context.fresh()).into();
                let output_type = output.open(&[&y]);
                self.enqueue(
                    output_type,
                    Apply::new(this, y.clone()).into(),
                    Apply::new(that, y).into(),
                );
                Ok(true)
            }
            Term::TupleType(TupleType { fields }) => {
                for i in 0..fields.len() {
                    self.enqueue(
                        Type.into(),
                        Proj::new(this.clone(), i).into(),
                        Proj::new(that.clone(), i).into(),
                    );
                }
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn convert(&mut self, context: &mut Context) -> Result<bool, Preempted> {
        while let Some(Goal { type_, this, that }) = self.dequeue()? {
            let this = reduce(context, &this)?;
            let that = reduce(context, &that)?;

            let goal = Goal {
                type_: type_.clone(),
                this: this.clone(),
                that: that.clone(),
            };

            if this == that || self.in_history(&goal) {
                continue;
            }

            let ok = match (this, that) {
                (Term::Prim(this), Term::Prim(that)) => self.compare_prim(this, that)?,
                (Term::BlnMatch(this), Term::BlnMatch(that)) => {
                    self.compare_bln_match(context, this, that)?
                }
                (Term::NatFold(this), Term::NatFold(that)) => {
                    self.compare_nat_fold(context, this, that)?
                }
                (Term::NatMatch(this), Term::NatMatch(that)) => {
                    self.compare_nat_match(context, this, that)?
                }
                (Term::FuncType(this), Term::FuncType(that)) => {
                    self.compare_func_type(context, this, that)?
                }
                (Term::Func(this), Term::Func(that)) => {
                    self.compare_func(context, this, that, type_.clone())?
                }
                (Term::Func(func), other) => {
                    self.eta_expand_func(context, func, other, type_.clone())?
                }
                (other, Term::Func(func)) => {
                    self.eta_expand_func(context, func, other, type_.clone())?
                }
                (Term::Apply(this), Term::Apply(that)) => self.compare_apply(this, that)?,
                (Term::TupleType(this), Term::TupleType(that)) => {
                    self.compare_tuple_type(context, this, that)?
                }
                (Term::Tuple(this), Term::Tuple(that)) => self.compare_tuple(this, that)?,
                (Term::Tuple(tuple), other) => self.eta_expand_tuple(tuple, other)?,
                (other, Term::Tuple(tuple)) => self.eta_expand_tuple(tuple, other)?,
                (Term::Proj(this), Term::Proj(that)) => self.compare_proj(this, that)?,
                (Term::AtomType(this), Term::AtomType(that)) => {
                    self.compare_atom_type(this, that)?
                }
                (Term::Atom(this), Term::Atom(that)) => self.compare_atom(this, that)?,
                (Term::Match(this), Term::Match(that)) => {
                    self.compare_match(context, this, that)?
                }
                (Term::Rec(this), Term::Rec(that)) => self.compare_rec(context, this, that)?,
                (Term::Sealed(this), Term::Sealed(that)) => {
                    self.compare_sealed(context, this, that)?
                }
                (Term::Seal(this), Term::Seal(that)) => self.compare_seal(this, that)?,
                (Term::Unseal(this), Term::Unseal(that)) => self.compare_unseal(this, that)?,
                (this_n, that_n) => self.eta_expand_neutral(context, this_n, that_n, type_)?,
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
        crate::core::{
            Apply, Atom, AtomType, Func, FuncType, Match, Proj, Rec, Sealed, Tuple, TupleType,
            Type, Var,
        },
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

        let this = Term::from(FuncType::new("x", Type, Var::free("x")));

        let that = Term::from(FuncType::new("y", Type, Var::free("y")));

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_func_is_alpha_equivalent() {
        let mut context = context();

        let this = Term::from(Func::new("x", Var::free("x")));

        let that = Term::from(Func::new("y", Var::free("y")));

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_match_compares_matches_and_motive() {
        let mut context = context();

        let this = Term::from(Match::new(
            Atom::from("a"),
            "m",
            Type,
            vec![("a", Atom::from("yes")), ("b", Atom::from("no"))],
        ));

        let that = Term::from(Match::new(
            Atom::from("a"),
            "n",
            Type,
            vec![("a", Atom::from("yes")), ("b", Atom::from("no"))],
        ));

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_recurses_into_operands() {
        let mut context = context();

        let this = Term::from(Func::new(
            "x",
            Term::Prim(Prim::int_add(Var::free("x"), Term::Prim(Prim::Int(1)))),
        ));

        let that = Term::from(Func::new(
            "y",
            Term::Prim(Prim::int_add(Var::free("y"), Term::Prim(Prim::Int(1)))),
        ));

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_distinguishes_operator_kind() {
        let mut context = context();

        let this = Term::from(Func::new(
            "x",
            Term::Prim(Prim::int_add(Var::free("x"), Term::Prim(Prim::Int(1)))),
        ));

        let that = Term::from(Func::new(
            "x",
            Term::Prim(Prim::int_sub(Var::free("x"), Term::Prim(Prim::Int(1)))),
        ));

        assert_eq!(conv(&mut context, &this, &that), Ok(false));
    }

    #[test]
    fn convert_rec_is_alpha_equivalent() {
        let mut context = context();

        let this = Term::from(Rec::new(vec![("x", Type, Var::free("x"))], Var::free("x")));

        let that = Term::from(Rec::new(vec![("y", Type, Var::free("y"))], Var::free("y")));

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_nat_add_recurses_into_operands() {
        let mut context = context();

        let this = Term::from(Func::new(
            "x",
            Term::Prim(Prim::nat_add(Var::free("x"), Term::Prim(Prim::Nat(1)))),
        ));

        let that = Term::from(Func::new(
            "y",
            Term::Prim(Prim::nat_add(Var::free("y"), Term::Prim(Prim::Nat(1)))),
        ));

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_flt_neg_recurses_into_operand() {
        let mut context = context();

        let this = Term::from(Func::new("x", Term::Prim(Prim::flt_neg(Var::free("x")))));

        let that = Term::from(Func::new("y", Term::Prim(Prim::flt_neg(Var::free("y")))));

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_nat_to_int_recurses_into_operand() {
        let mut context = context();

        let this = Term::from(Func::new("x", Term::Prim(Prim::nat_to_int(Var::free("x")))));

        let that = Term::from(Func::new("y", Term::Prim(Prim::nat_to_int(Var::free("y")))));

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_arr_compares_element_wise() {
        let mut context = context();

        let this = Term::Prim(Prim::from(vec![
            Term::Prim(Prim::Nat(1)),
            Term::Prim(Prim::Nat(2)),
        ]));

        let that = Term::Prim(Prim::from(vec![
            Term::Prim(Prim::Nat(1)),
            Term::Prim(Prim::Nat(2)),
        ]));

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_arr_rejects_different_lengths() {
        let mut context = context();

        let this = Term::Prim(Prim::from(vec![Term::Prim(Prim::Nat(1))]));

        let that = Term::Prim(Prim::from(vec![
            Term::Prim(Prim::Nat(1)),
            Term::Prim(Prim::Nat(2)),
        ]));

        assert_eq!(conv(&mut context, &this, &that), Ok(false));
    }

    #[test]
    fn convert_prim_bin_type_is_equal_to_itself() {
        let mut context = context();

        let this = Term::Prim(Prim::BinType);
        let that = Term::Prim(Prim::BinType);

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_bin_literal_compares_bytes() {
        let mut context = context();

        assert_eq!(
            conv(
                &mut context,
                &Term::Prim(Prim::Bin(vec![1, 2])),
                &Term::Prim(Prim::Bin(vec![1, 2])),
            ),
            Ok(true)
        );

        assert_eq!(
            conv(
                &mut context,
                &Term::Prim(Prim::Bin(vec![1, 2])),
                &Term::Prim(Prim::Bin(vec![1, 3])),
            ),
            Ok(false)
        );
    }

    #[test]
    fn convert_prim_bin_len_recurses_into_operand() {
        let mut context = context();

        let this = Term::from(Func::new("x", Term::Prim(Prim::bin_len(Var::free("x")))));
        let that = Term::from(Func::new("y", Term::Prim(Prim::bin_len(Var::free("y")))));

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_bin_get_recurses_into_operands() {
        let mut context = context();

        let this = Term::from(Func::new(
            "x",
            Func::new(
                "a",
                Term::Prim(Prim::bin_get(Var::free("x"), Var::free("a"))),
            ),
        ));

        let that = Term::from(Func::new(
            "y",
            Func::new(
                "b",
                Term::Prim(Prim::bin_get(Var::free("y"), Var::free("b"))),
            ),
        ));

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_bin_concat_recurses_into_operands() {
        let mut context = context();

        let this = Term::from(Func::new(
            "x",
            Func::new(
                "a",
                Term::Prim(Prim::bin_concat([Var::free("x"), Var::free("a")])),
            ),
        ));

        let that = Term::from(Func::new(
            "y",
            Func::new(
                "b",
                Term::Prim(Prim::bin_concat([Var::free("y"), Var::free("b")])),
            ),
        ));

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_bin_slice_recurses_into_operands() {
        let mut context = context();

        let this = Term::from(Func::new(
            "x",
            Func::new(
                "a",
                Func::new(
                    "p",
                    Term::Prim(Prim::bin_slice(
                        Var::free("x"),
                        Var::free("a"),
                        Var::free("p"),
                    )),
                ),
            ),
        ));

        let that = Term::from(Func::new(
            "y",
            Func::new(
                "b",
                Func::new(
                    "q",
                    Term::Prim(Prim::bin_slice(
                        Var::free("y"),
                        Var::free("b"),
                        Var::free("q"),
                    )),
                ),
            ),
        ));

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_tuple_equal() {
        let mut context = context();

        let this = Term::from(Tuple::new([
            Term::from(Atom::from("x")),
            Term::from(Atom::from("y")),
        ]));
        let that = Term::from(Tuple::new([
            Term::from(Atom::from("x")),
            Term::from(Atom::from("y")),
        ]));

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_tuple_unequal_field() {
        let mut context = context();

        let this = Term::from(Tuple::new([
            Term::from(Atom::from("x")),
            Term::from(Atom::from("y")),
        ]));
        let that = Term::from(Tuple::new([
            Term::from(Atom::from("x")),
            Term::from(Atom::from("z")),
        ]));

        assert_eq!(conv(&mut context, &this, &that), Ok(false));
    }

    #[test]
    fn convert_proj_same_index_and_head() {
        let mut context = context();

        let this = Term::from(Proj::new(Var::free("r"), 0));
        let that = Term::from(Proj::new(Var::free("r"), 0));

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_proj_different_index_is_false() {
        let mut context = context();

        let this = Term::from(Proj::new(Var::free("r"), 0));
        let that = Term::from(Proj::new(Var::free("r"), 1));

        assert_eq!(conv(&mut context, &this, &that), Ok(false));
    }

    #[test]
    fn convert_eta_tuple_neutral_with_known_type() {
        let mut context = context();

        let tuple_type: Term = TupleType::new([
            ("x", AtomType::new(["a", "b"])),
            ("y", AtomType::new(["c", "d"])),
        ])
        .into();

        let r: Term = Var::free("r").into();
        let s: Term = Var::free("s").into();

        assert_eq!(convert(&mut context, &tuple_type, &r, &r), Ok(true));

        assert_eq!(convert(&mut context, &tuple_type, &r, &s), Ok(false));
    }

    #[test]
    fn convert_times_out_on_pathological_inputs() {
        let mut context = context();

        context.define("loop", &Var::free("loop").into());

        let this = Term::from(TupleType::new([
            (
                "x",
                Apply::many(Func::new("z", Var::free("z")), [Var::free("loop")]),
            ),
            ("y", Term::from(Var::free("x"))),
        ]));

        let that = Term::from(TupleType::new([
            ("x", Var::free("loop")),
            ("y", Var::free("x")),
        ]));

        assert_eq!(conv(&mut context, &this, &that), Err(Preempted));
    }

    #[test]
    fn convert_sealed_is_alpha_equivalent() {
        let mut context = context();

        let this = Term::from(Sealed::new("x", Type, Var::free("x")));
        let that = Term::from(Sealed::new("y", Type, Var::free("y")));

        assert_eq!(conv(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_sealed_different_witnesses_not_convertible() {
        let mut context = context();

        let this = Term::from(Sealed::new("x", Type, Var::free("x")));
        let that = Term::from(Sealed::new("x", AtomType::new(["a"]), Var::free("x")));

        assert_eq!(conv(&mut context, &this, &that), Ok(false));
    }
}
