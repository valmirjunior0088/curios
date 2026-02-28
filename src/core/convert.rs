use {
    super::{
        Apply, AtomType, Context, Func, FuncType, LetRec, Match, Name, Pair, PairType, Preempted,
        Prim, Split, Term, reduce,
    },
    std::{
        collections::{HashSet, VecDeque},
        time::{Duration, Instant},
    },
};

pub fn convert(context: &mut Context, this: &Term, that: &Term) -> Result<bool, Preempted> {
    Convert::new(context.timeout(), this.clone(), that.clone()).convert(context)
}

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

    fn convert(&mut self, context: &mut Context) -> Result<bool, Preempted> {
        while let Some((this, that)) = self.dequeue()? {
            let this = reduce(context, &this)?;
            let that = reduce(context, &that)?;

            if this == that || self.in_history(&this, &that) {
                continue;
            }

            match (this, that) {
                (Term::Prim(this), Term::Prim(that)) => match (this, that) {
                    (Prim::IntType, Prim::IntType) | (Prim::FltType, Prim::FltType) => {}
                    (Prim::Int(this), Prim::Int(that)) => {
                        if this != that {
                            return Ok(false);
                        }
                    }
                    (Prim::Flt(this), Prim::Flt(that)) => {
                        if this != that {
                            return Ok(false);
                        }
                    }
                    (
                        Prim::IntEql(this_first, this_second),
                        Prim::IntEql(that_first, that_second),
                    )
                    | (
                        Prim::IntAdd(this_first, this_second),
                        Prim::IntAdd(that_first, that_second),
                    )
                    | (
                        Prim::IntSub(this_first, this_second),
                        Prim::IntSub(that_first, that_second),
                    )
                    | (
                        Prim::IntMul(this_first, this_second),
                        Prim::IntMul(that_first, that_second),
                    )
                    | (
                        Prim::FltAdd(this_first, this_second),
                        Prim::FltAdd(that_first, that_second),
                    )
                    | (
                        Prim::FltSub(this_first, this_second),
                        Prim::FltSub(that_first, that_second),
                    )
                    | (
                        Prim::FltMul(this_first, this_second),
                        Prim::FltMul(that_first, that_second),
                    ) => {
                        self.enqueue(*this_first, *that_first);
                        self.enqueue(*this_second, *that_second);
                    }
                    (_, _) => {
                        return Ok(false);
                    }
                },
                (
                    Term::FuncType(FuncType {
                        input: this_input,
                        output: this_output,
                    }),
                    Term::FuncType(FuncType {
                        input: that_input,
                        output: that_output,
                    }),
                ) => {
                    self.enqueue(*this_input, *that_input);

                    let label = Name::label(context.fresh()).into();

                    self.enqueue(this_output.open(&[&label]), that_output.open(&[&label]));
                }
                (Term::Func(Func { body: this }), Term::Func(Func { body: that })) => {
                    let label = Name::label(context.fresh()).into();

                    self.enqueue(this.open(&[&label]), that.open(&[&label]));
                }
                (
                    Term::Apply(Apply {
                        head: this_head,
                        param: this_param,
                    }),
                    Term::Apply(Apply {
                        head: that_head,
                        param: that_param,
                    }),
                ) => {
                    self.enqueue(*this_head, *that_head);
                    self.enqueue(*this_param, *that_param);
                }
                (
                    Term::PairType(PairType {
                        input: this_input,
                        output: this_output,
                    }),
                    Term::PairType(PairType {
                        input: that_input,
                        output: that_output,
                    }),
                ) => {
                    self.enqueue(*this_input, *that_input);

                    let label = Name::label(context.fresh()).into();

                    self.enqueue(this_output.open(&[&label]), that_output.open(&[&label]));
                }
                (
                    Term::Pair(Pair {
                        first: this_first,
                        second: this_second,
                    }),
                    Term::Pair(Pair {
                        first: that_first,
                        second: that_second,
                    }),
                ) => {
                    self.enqueue(*this_first, *that_first);
                    self.enqueue(*this_second, *that_second);
                }
                (
                    Term::Split(Split {
                        head: this_head,
                        motive: this_motive,
                        tail: this_tail,
                    }),
                    Term::Split(Split {
                        head: that_head,
                        motive: that_motive,
                        tail: that_tail,
                    }),
                ) => {
                    self.enqueue(*this_head, *that_head);

                    let motive_label = Name::label(context.fresh()).into();

                    self.enqueue(
                        this_motive.open(&[&motive_label]),
                        that_motive.open(&[&motive_label]),
                    );

                    let first_label = Name::label(context.fresh()).into();
                    let second_label = Name::label(context.fresh()).into();

                    self.enqueue(
                        this_tail.open(&[&first_label, &second_label]),
                        that_tail.open(&[&first_label, &second_label]),
                    );
                }
                (
                    Term::AtomType(AtomType { atoms: this }),
                    Term::AtomType(AtomType { atoms: that }),
                ) => {
                    if this != that {
                        return Ok(false);
                    }
                }
                (Term::Atom(this), Term::Atom(that)) => {
                    if this != that {
                        return Ok(false);
                    }
                }
                (
                    Term::Match(Match {
                        head: this_head,
                        motive: this_motive,
                        cases: this_cases,
                    }),
                    Term::Match(Match {
                        head: that_head,
                        motive: that_motive,
                        cases: that_cases,
                    }),
                ) => {
                    self.enqueue(*this_head, *that_head);

                    let label = Name::label(context.fresh()).into();

                    self.enqueue(this_motive.open(&[&label]), that_motive.open(&[&label]));

                    if this_cases.len() != that_cases.len() {
                        return Ok(false);
                    }

                    for ((this_atom, this_body), (that_atom, that_body)) in
                        this_cases.into_iter().zip(that_cases)
                    {
                        if this_atom != that_atom {
                            return Ok(false);
                        }

                        self.enqueue(*this_body, *that_body);
                    }
                }
                (
                    Term::LetRec(LetRec {
                        items: this_items,
                        tail: this_tail,
                    }),
                    Term::LetRec(LetRec {
                        items: that_items,
                        tail: that_tail,
                    }),
                ) => {
                    if this_items.len() != that_items.len() {
                        return Ok(false);
                    }

                    let labels = (0..this_items.len())
                        .map(|_| Name::label(context.fresh()).into())
                        .collect::<Vec<_>>();

                    let labels = labels.iter().collect::<Vec<_>>();

                    for ((this_type, this_body), (that_type, that_body)) in
                        this_items.into_iter().zip(that_items)
                    {
                        self.enqueue(this_type.open(&labels), that_type.open(&labels));
                        self.enqueue(this_body.open(&labels), that_body.open(&labels));
                    }

                    self.enqueue(this_tail.open(&labels), that_tail.open(&labels));
                }
                (_, _) => {
                    return Ok(false);
                }
            }
        }

        Ok(true)
    }
}

#[cfg(test)]
mod tests {
    use {
        super::*,
        crate::core::{Atom, Func, FuncType, LetRec, Match, Name, PairType, Prim, Type},
        std::time::Duration,
    };

    fn context() -> Context {
        Context::new(Duration::from_millis(10))
    }

    #[test]
    fn convert_func_type_is_alpha_equivalent() {
        let mut context = context();

        let this = Term::from(FuncType::new("x", Type, Name::label("x")));

        let that = Term::from(FuncType::new("y", Type, Name::label("y")));

        assert_eq!(convert(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_func_is_alpha_equivalent() {
        let mut context = context();

        let this = Term::from(Func::new("x", Name::label("x")));

        let that = Term::from(Func::new("y", Name::label("y")));

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
            Prim::int_add(Name::label("x"), Prim::from(1)),
        ));

        let that = Term::from(Func::new(
            "y",
            Prim::int_add(Name::label("y"), Prim::from(1)),
        ));

        assert_eq!(convert(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_prim_distinguishes_operator_kind() {
        let mut context = context();

        let this = Term::from(Func::new(
            "x",
            Prim::int_add(Name::label("x"), Prim::from(1)),
        ));

        let that = Term::from(Func::new(
            "x",
            Prim::int_sub(Name::label("x"), Prim::from(1)),
        ));

        assert_eq!(convert(&mut context, &this, &that), Ok(false));
    }

    #[test]
    fn convert_letrec_is_alpha_equivalent() {
        let mut context = context();

        let this = Term::from(LetRec::new(
            vec![("x", Type, Name::label("x"))],
            Name::label("x"),
        ));

        let that = Term::from(LetRec::new(
            vec![("y", Type, Name::label("y"))],
            Name::label("y"),
        ));

        assert_eq!(convert(&mut context, &this, &that), Ok(true));
    }

    #[test]
    fn convert_times_out_on_pathological_inputs() {
        let mut context = context();

        context.define("loop", &Name::label("loop").into());

        let this = Term::from(PairType::new(
            "x",
            Apply::many(Func::new("z", Name::label("z")), [Name::label("loop")]),
            Name::label("x"),
        ));

        let that = Term::from(PairType::new("y", Name::label("loop"), Name::label("y")));

        assert_eq!(convert(&mut context, &this, &that), Err(Preempted));
    }
}
