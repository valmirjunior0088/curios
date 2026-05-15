use {
    super::{
        Apply, Atom, AtomType, Case, Context, Func, FuncType, LetRec, Pair, PairType, Preempted,
        Prim, Split, Term, Var, reduce,
    },
    std::{
        collections::{HashSet, VecDeque},
        time::{Duration, Instant},
    },
};

pub fn convert(context: &mut Context, this: &Term, that: &Term) -> Result<bool, Preempted> {
    Convert::new(context.timeout(), this.clone(), that.clone()).convert(context)
}

#[derive(Debug)]
struct Convert {
    deadline: Instant,
    history: HashSet<(Term, Term)>,
    pending: VecDeque<(Term, Term)>,
}

impl Convert {
    fn new(timeout: Duration, this: Term, that: Term) -> Self {
        Self {
            deadline: Instant::now() + timeout,
            history: HashSet::new(),
            pending: VecDeque::from([(this, that)]),
        }
    }

    fn in_history(&mut self, this: &Term, that: &Term) -> bool {
        !self.history.insert((this.clone(), that.clone()))
    }

    fn enqueue(&mut self, this: Term, that: Term) {
        self.pending.push_back((this, that));
    }

    fn dequeue(&mut self) -> Result<Option<(Term, Term)>, Preempted> {
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
                self.enqueue(*this, *that);

                Ok(true)
            }
            (Prim::NatEql(this_left, this_right), Prim::NatEql(that_left, that_right))
            | (Prim::NatNeq(this_left, this_right), Prim::NatNeq(that_left, that_right))
            | (Prim::NatAdd(this_left, this_right), Prim::NatAdd(that_left, that_right))
            | (Prim::NatSub(this_left, this_right), Prim::NatSub(that_left, that_right))
            | (Prim::NatMul(this_left, this_right), Prim::NatMul(that_left, that_right))
            | (Prim::NatDiv(this_left, this_right), Prim::NatDiv(that_left, that_right))
            | (Prim::NatRem(this_left, this_right), Prim::NatRem(that_left, that_right))
            | (Prim::NatLt(this_left, this_right), Prim::NatLt(that_left, that_right))
            | (Prim::NatGt(this_left, this_right), Prim::NatGt(that_left, that_right))
            | (Prim::NatLte(this_left, this_right), Prim::NatLte(that_left, that_right))
            | (Prim::NatGte(this_left, this_right), Prim::NatGte(that_left, that_right))
            | (Prim::IntEql(this_left, this_right), Prim::IntEql(that_left, that_right))
            | (Prim::IntAdd(this_left, this_right), Prim::IntAdd(that_left, that_right))
            | (Prim::IntSub(this_left, this_right), Prim::IntSub(that_left, that_right))
            | (Prim::IntMul(this_left, this_right), Prim::IntMul(that_left, that_right))
            | (Prim::IntNeq(this_left, this_right), Prim::IntNeq(that_left, that_right))
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
            | (Prim::FltMin(this_left, this_right), Prim::FltMin(that_left, that_right))
            | (Prim::FltMax(this_left, this_right), Prim::FltMax(that_left, that_right))
            | (Prim::FltEql(this_left, this_right), Prim::FltEql(that_left, that_right))
            | (Prim::FltNeq(this_left, this_right), Prim::FltNeq(that_left, that_right))
            | (Prim::FltLt(this_left, this_right), Prim::FltLt(that_left, that_right))
            | (Prim::FltGt(this_left, this_right), Prim::FltGt(that_left, that_right))
            | (Prim::FltLte(this_left, this_right), Prim::FltLte(that_left, that_right))
            | (Prim::FltGte(this_left, this_right), Prim::FltGte(that_left, that_right))
            | (Prim::ArrGet(this_left, this_right), Prim::ArrGet(that_left, that_right))
            | (Prim::ArrAppend(this_left, this_right), Prim::ArrAppend(that_left, that_right))
            | (Prim::BinGet(this_left, this_right), Prim::BinGet(that_left, that_right))
            | (Prim::BinAppend(this_left, this_right), Prim::BinAppend(that_left, that_right)) => {
                self.enqueue(*this_left, *that_left);
                self.enqueue(*this_right, *that_right);

                Ok(true)
            }
            (Prim::FltNeg(this), Prim::FltNeg(that))
            | (Prim::FltAbs(this), Prim::FltAbs(that))
            | (Prim::FltSqrt(this), Prim::FltSqrt(that))
            | (Prim::FltFloor(this), Prim::FltFloor(that))
            | (Prim::FltCeil(this), Prim::FltCeil(that))
            | (Prim::FltTrunc(this), Prim::FltTrunc(that))
            | (Prim::FltNearest(this), Prim::FltNearest(that))
            | (Prim::NatToInt(this), Prim::NatToInt(that))
            | (Prim::IntToNat(this), Prim::IntToNat(that))
            | (Prim::IntToFlt(this), Prim::IntToFlt(that))
            | (Prim::NatToFlt(this), Prim::NatToFlt(that))
            | (Prim::FltToInt(this), Prim::FltToInt(that))
            | (Prim::FltToNat(this), Prim::FltToNat(that))
            | (Prim::ArrLen(this), Prim::ArrLen(that))
            | (Prim::BinLen(this), Prim::BinLen(that)) => {
                self.enqueue(*this, *that);

                Ok(true)
            }
            (
                Prim::BinSlice(this_bin, this_start, this_end),
                Prim::BinSlice(that_bin, that_start, that_end),
            ) => {
                self.enqueue(*this_bin, *that_bin);
                self.enqueue(*this_start, *that_start);
                self.enqueue(*this_end, *that_end);

                Ok(true)
            }
            (
                Prim::ArrSlice(this_list, this_start, this_end),
                Prim::ArrSlice(that_list, that_start, that_end),
            ) => {
                self.enqueue(*this_list, *that_list);
                self.enqueue(*this_start, *that_start);
                self.enqueue(*this_end, *that_end);

                Ok(true)
            }
            (Prim::Arr(this_elems), Prim::Arr(that_elems)) => {
                if this_elems.len() != that_elems.len() {
                    return Ok(false);
                }

                for (this, that) in this_elems.into_iter().zip(that_elems) {
                    self.enqueue(*this, *that);
                }

                Ok(true)
            }
            (Prim::BinConcat(this_ops), Prim::BinConcat(that_ops)) => {
                if this_ops.len() != that_ops.len() {
                    return Ok(false);
                }
                for (this, that) in this_ops.into_iter().zip(that_ops) {
                    self.enqueue(*this, *that);
                }
                Ok(true)
            }
            (Prim::ArrConcat(this_ops), Prim::ArrConcat(that_ops)) => {
                if this_ops.len() != that_ops.len() {
                    return Ok(false);
                }
                for (this, that) in this_ops.into_iter().zip(that_ops) {
                    self.enqueue(*this, *that);
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
        self.enqueue(*this.input, *that.input);

        let label = Var::free(context.fresh()).into();
        self.enqueue(this.output.open(&[&label]), that.output.open(&[&label]));

        Ok(true)
    }

    fn compare_func(
        &mut self,
        context: &mut Context,
        this: Func,
        that: Func,
    ) -> Result<bool, Preempted> {
        let label = Var::free(context.fresh()).into();
        self.enqueue(this.body.open(&[&label]), that.body.open(&[&label]));

        Ok(true)
    }

    fn compare_apply(&mut self, this: Apply, that: Apply) -> Result<bool, Preempted> {
        self.enqueue(*this.head, *that.head);
        self.enqueue(*this.param, *that.param);

        Ok(true)
    }

    fn compare_pair_type(
        &mut self,
        context: &mut Context,
        this: PairType,
        that: PairType,
    ) -> Result<bool, Preempted> {
        self.enqueue(*this.input, *that.input);

        let label = Var::free(context.fresh()).into();
        self.enqueue(this.output.open(&[&label]), that.output.open(&[&label]));

        Ok(true)
    }

    fn compare_pair(&mut self, this: Pair, that: Pair) -> Result<bool, Preempted> {
        self.enqueue(*this.fst, *that.fst);
        self.enqueue(*this.snd, *that.snd);

        Ok(true)
    }

    fn compare_split(
        &mut self,
        context: &mut Context,
        this: Split,
        that: Split,
    ) -> Result<bool, Preempted> {
        self.enqueue(*this.head, *that.head);

        let motive_label = Var::free(context.fresh()).into();

        self.enqueue(
            this.motive.open(&[&motive_label]),
            that.motive.open(&[&motive_label]),
        );

        let fst_label = Var::free(context.fresh()).into();
        let snd_label = Var::free(context.fresh()).into();

        self.enqueue(
            this.tail.open(&[&fst_label, &snd_label]),
            that.tail.open(&[&fst_label, &snd_label]),
        );

        Ok(true)
    }

    fn compare_atom_type(&mut self, this: AtomType, that: AtomType) -> Result<bool, Preempted> {
        Ok(this == that)
    }

    fn compare_atom(&mut self, this: Atom, that: Atom) -> Result<bool, Preempted> {
        Ok(this == that)
    }

    fn compare_case(
        &mut self,
        context: &mut Context,
        this: Case,
        that: Case,
    ) -> Result<bool, Preempted> {
        self.enqueue(*this.head, *that.head);

        let label = Var::free(context.fresh()).into();
        self.enqueue(this.motive.open(&[&label]), that.motive.open(&[&label]));

        if this.cases.len() != that.cases.len() {
            return Ok(false);
        }

        for ((this_atom, this_body), (that_atom, that_body)) in
            this.cases.into_iter().zip(that.cases)
        {
            if this_atom != that_atom {
                return Ok(false);
            }

            self.enqueue(*this_body, *that_body);
        }

        Ok(true)
    }

    fn compare_letrec(
        &mut self,
        context: &mut Context,
        this: LetRec,
        that: LetRec,
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
            self.enqueue(this_type.open(&labels), that_type.open(&labels));
            self.enqueue(this_body.open(&labels), that_body.open(&labels));
        }

        self.enqueue(this.tail.open(&labels), that.tail.open(&labels));

        Ok(true)
    }

    fn convert(&mut self, context: &mut Context) -> Result<bool, Preempted> {
        while let Some((this, that)) = self.dequeue()? {
            let this = reduce(context, &this)?;
            let that = reduce(context, &that)?;

            if this == that || self.in_history(&this, &that) {
                continue;
            }

            let ok = match (this, that) {
                (Term::Prim(this), Term::Prim(that)) => self.compare_prim(this, that)?,
                (Term::FuncType(this), Term::FuncType(that)) => {
                    self.compare_func_type(context, this, that)?
                }
                (Term::Func(this), Term::Func(that)) => self.compare_func(context, this, that)?,
                (Term::Apply(this), Term::Apply(that)) => self.compare_apply(this, that)?,
                (Term::PairType(this), Term::PairType(that)) => {
                    self.compare_pair_type(context, this, that)?
                }
                (Term::Pair(this), Term::Pair(that)) => self.compare_pair(this, that)?,
                (Term::Split(this), Term::Split(that)) => {
                    self.compare_split(context, this, that)?
                }
                (Term::AtomType(this), Term::AtomType(that)) => {
                    self.compare_atom_type(this, that)?
                }
                (Term::Atom(this), Term::Atom(that)) => self.compare_atom(this, that)?,
                (Term::Case(this), Term::Case(that)) => self.compare_case(context, this, that)?,
                (Term::LetRec(this), Term::LetRec(that)) => {
                    self.compare_letrec(context, this, that)?
                }
                (_, _) => return Ok(false),
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
        crate::core::{Atom, Case, Func, FuncType, LetRec, PairType, Type, Var},
        std::time::Duration,
    };

    fn context() -> Context {
        Context::new(Duration::from_millis(10))
    }

    #[test]
    fn convert_func_type_is_alpha_equivalent() {
        let mut context = context();

        let this = Term::from(FuncType::new("x", Type, Var::free("x")));

        let that = Term::from(FuncType::new("y", Type, Var::free("y")));

        assert_eq!(convert(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_func_is_alpha_equivalent() {
        let mut context = context();

        let this = Term::from(Func::new("x", Var::free("x")));

        let that = Term::from(Func::new("y", Var::free("y")));

        assert_eq!(convert(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_case_compares_cases_and_motive() {
        let mut context = context();

        let this = Term::from(Case::new(
            Atom::from("a"),
            "m",
            Type,
            vec![("a", Atom::from("yes")), ("b", Atom::from("no"))],
        ));

        let that = Term::from(Case::new(
            Atom::from("a"),
            "n",
            Type,
            vec![("a", Atom::from("yes")), ("b", Atom::from("no"))],
        ));

        assert_eq!(convert(&mut context, &this, &that), Ok(true));
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

        assert_eq!(convert(&mut context, &this, &that), Ok(true));
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

        assert_eq!(convert(&mut context, &this, &that), Ok(false));
    }

    #[test]
    fn convert_letrec_is_alpha_equivalent() {
        let mut context = context();

        let this = Term::from(LetRec::new(
            vec![("x", Type, Var::free("x"))],
            Var::free("x"),
        ));

        let that = Term::from(LetRec::new(
            vec![("y", Type, Var::free("y"))],
            Var::free("y"),
        ));

        assert_eq!(convert(&mut context, &this, &that), Ok(true));
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

        assert_eq!(convert(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_flt_neg_recurses_into_operand() {
        let mut context = context();

        let this = Term::from(Func::new("x", Term::Prim(Prim::flt_neg(Var::free("x")))));

        let that = Term::from(Func::new("y", Term::Prim(Prim::flt_neg(Var::free("y")))));

        assert_eq!(convert(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_nat_to_int_recurses_into_operand() {
        let mut context = context();

        let this = Term::from(Func::new("x", Term::Prim(Prim::nat_to_int(Var::free("x")))));

        let that = Term::from(Func::new("y", Term::Prim(Prim::nat_to_int(Var::free("y")))));

        assert_eq!(convert(&mut context, &this, &that), Ok(true));
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

        assert_eq!(convert(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_arr_rejects_different_lengths() {
        let mut context = context();

        let this = Term::Prim(Prim::from(vec![Term::Prim(Prim::Nat(1))]));

        let that = Term::Prim(Prim::from(vec![
            Term::Prim(Prim::Nat(1)),
            Term::Prim(Prim::Nat(2)),
        ]));

        assert_eq!(convert(&mut context, &this, &that), Ok(false));
    }

    #[test]
    fn convert_prim_bin_type_is_equal_to_itself() {
        let mut context = context();

        let this = Term::Prim(Prim::BinType);
        let that = Term::Prim(Prim::BinType);

        assert_eq!(convert(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_bin_literal_compares_bytes() {
        let mut context = context();

        assert_eq!(
            convert(
                &mut context,
                &Term::Prim(Prim::Bin(vec![1, 2])),
                &Term::Prim(Prim::Bin(vec![1, 2])),
            ),
            Ok(true)
        );

        assert_eq!(
            convert(
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

        assert_eq!(convert(&mut context, &this, &that), Ok(true));
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

        assert_eq!(convert(&mut context, &this, &that), Ok(true));
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

        assert_eq!(convert(&mut context, &this, &that), Ok(true));
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

        assert_eq!(convert(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_times_out_on_pathological_inputs() {
        let mut context = context();

        context.define("loop", &Var::free("loop").into());

        let this = Term::from(PairType::new(
            "x",
            Apply::many(Func::new("z", Var::free("z")), [Var::free("loop")]),
            Var::free("x"),
        ));

        let that = Term::from(PairType::new("y", Var::free("loop"), Var::free("y")));

        assert_eq!(convert(&mut context, &this, &that), Err(Preempted));
    }
}
