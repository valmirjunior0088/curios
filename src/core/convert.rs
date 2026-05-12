use {
    super::{
        Apply, Atom, AtomType, Context, Func, FuncType, LetRec, Match, Pair, PairType, Preempted,
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
            (Prim::IntType, Prim::IntType) | (Prim::FltType, Prim::FltType) => Ok(true),
            (Prim::IntValue(this), Prim::IntValue(that)) => Ok(this == that),
            (Prim::FltValue(this), Prim::FltValue(that)) => Ok(this == that),
            (Prim::IntEql(this_l, this_r), Prim::IntEql(that_l, that_r))
            | (Prim::IntAdd(this_l, this_r), Prim::IntAdd(that_l, that_r))
            | (Prim::IntSub(this_l, this_r), Prim::IntSub(that_l, that_r))
            | (Prim::IntMul(this_l, this_r), Prim::IntMul(that_l, that_r))
            | (Prim::FltAdd(this_l, this_r), Prim::FltAdd(that_l, that_r))
            | (Prim::FltSub(this_l, this_r), Prim::FltSub(that_l, that_r))
            | (Prim::FltMul(this_l, this_r), Prim::FltMul(that_l, that_r)) => {
                self.enqueue(*this_l, *that_l);
                self.enqueue(*this_r, *that_r);
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

    fn compare_match(
        &mut self,
        context: &mut Context,
        this: Match,
        that: Match,
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
                (Term::Match(this), Term::Match(that)) => {
                    self.compare_match(context, this, that)?
                }
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
        crate::core::{Atom, Func, FuncType, LetRec, Match, PairType, Type, Var},
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
    fn convert_match_compares_cases_and_motive() {
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

        assert_eq!(convert(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_recurses_into_operands() {
        let mut context = context();

        let this = Term::from(Func::new(
            "x",
            Term::Prim(Prim::int_add(Var::free("x"), Term::Prim(Prim::IntValue(1)))),
        ));

        let that = Term::from(Func::new(
            "y",
            Term::Prim(Prim::int_add(Var::free("y"), Term::Prim(Prim::IntValue(1)))),
        ));

        assert_eq!(convert(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_distinguishes_operator_kind() {
        let mut context = context();

        let this = Term::from(Func::new(
            "x",
            Term::Prim(Prim::int_add(Var::free("x"), Term::Prim(Prim::IntValue(1)))),
        ));

        let that = Term::from(Func::new(
            "x",
            Term::Prim(Prim::int_sub(Var::free("x"), Term::Prim(Prim::IntValue(1)))),
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
