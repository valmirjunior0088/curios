use {
    super::{Context, Term},
    std::{
        collections::{HashSet, VecDeque},
        time::{Duration, Instant},
    },
};

pub fn convert(context: &mut Context, this: Term, that: Term) -> Result<bool, ()> {
    Convert::new(context.timeout(), this, that).convert(context)
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

    fn dequeue(&mut self) -> Result<Option<(Term, Term)>, ()> {
        if Instant::now() > self.deadline {
            return Err(());
        }

        Ok(self.pending.pop_front())
    }

    fn convert(&mut self, context: &mut Context) -> Result<bool, ()> {
        while let Some((this, that)) = self.dequeue()? {
            let this = super::reduce(context, this)?;
            let that = super::reduce(context, that)?;

            if this == that || self.in_history(&this, &that) {
                continue;
            }

            match (this, that) {
                (
                    Term::FuncType {
                        input: this_input,
                        output: this_output,
                    },
                    Term::FuncType {
                        input: that_input,
                        output: that_output,
                    },
                ) => {
                    self.enqueue(*this_input, *that_input);

                    let label = Term::label(context.fresh());

                    self.enqueue(this_output.open(&[&label]), that_output.open(&[&label]));
                }
                (Term::Func { body: this }, Term::Func { body: that }) => {
                    let label = Term::label(context.fresh());

                    self.enqueue(this.open(&[&label]), that.open(&[&label]));
                }
                (
                    Term::Apply {
                        head: this_head,
                        param: this_param,
                    },
                    Term::Apply {
                        head: that_head,
                        param: that_param,
                    },
                ) => {
                    self.enqueue(*this_head, *that_head);
                    self.enqueue(*this_param, *that_param);
                }
                (
                    Term::PairType {
                        input: this_input,
                        output: this_output,
                    },
                    Term::PairType {
                        input: that_input,
                        output: that_output,
                    },
                ) => {
                    self.enqueue(*this_input, *that_input);

                    let label = Term::label(context.fresh());

                    self.enqueue(this_output.open(&[&label]), that_output.open(&[&label]));
                }
                (
                    Term::Pair {
                        first: this_first,
                        second: this_second,
                    },
                    Term::Pair {
                        first: that_first,
                        second: that_second,
                    },
                ) => {
                    self.enqueue(*this_first, *that_first);
                    self.enqueue(*this_second, *that_second);
                }
                (
                    Term::Split {
                        head: this_head,
                        motive: this_motive,
                        tail: this_tail,
                    },
                    Term::Split {
                        head: that_head,
                        motive: that_motive,
                        tail: that_tail,
                    },
                ) => {
                    self.enqueue(*this_head, *that_head);

                    let motive_label = Term::label(context.fresh());

                    self.enqueue(
                        this_motive.open(&[&motive_label]),
                        that_motive.open(&[&motive_label]),
                    );

                    let first_label = Term::label(context.fresh());
                    let second_label = Term::label(context.fresh());

                    self.enqueue(
                        this_tail.open(&[&first_label, &second_label]),
                        that_tail.open(&[&first_label, &second_label]),
                    );
                }
                (Term::AtomType { atoms: this }, Term::AtomType { atoms: that }) => {
                    if this != that {
                        return Ok(false);
                    }
                }
                (Term::Atom { atom: this }, Term::Atom { atom: that }) => {
                    if this != that {
                        return Ok(false);
                    }
                }
                (
                    Term::Match {
                        head: this_head,
                        motive: this_motive,
                        cases: this_cases,
                    },
                    Term::Match {
                        head: that_head,
                        motive: that_motive,
                        cases: that_cases,
                    },
                ) => {
                    self.enqueue(*this_head, *that_head);

                    let label = Term::label(context.fresh());

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
                    Term::LetRec {
                        items: this_items,
                        tail: this_tail,
                    },
                    Term::LetRec {
                        items: that_items,
                        tail: that_tail,
                    },
                ) => {
                    if this_items.len() != that_items.len() {
                        return Ok(false);
                    }

                    let labels = (0..this_items.len())
                        .map(|_| Term::label(context.fresh()))
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
    use {super::*, std::time::Duration};

    fn context() -> Context {
        Context::new(Duration::from_millis(10))
    }

    #[test]
    fn convert_func_type_is_alpha_equivalent() {
        let mut context = context();

        let this = Term::func_type("x", Term::Type, Term::label("x"));

        let that = Term::func_type("y", Term::Type, Term::label("y"));

        assert_eq!(convert(&mut context, this, that), Ok(true));
    }

    #[test]
    fn convert_func_is_alpha_equivalent() {
        let mut context = context();

        let this = Term::func("x", Term::label("x"));

        let that = Term::func("y", Term::label("y"));

        assert_eq!(convert(&mut context, this, that), Ok(true));
    }

    #[test]
    fn convert_match_compares_cases_and_motive() {
        let mut context = context();

        let this = Term::match_(
            Term::atom("a"),
            "m",
            Term::Type,
            vec![
                ("a".into(), Term::atom("yes")),
                ("b".into(), Term::atom("no")),
            ],
        );

        let that = Term::match_(
            Term::atom("a"),
            "n",
            Term::Type,
            vec![
                ("a".into(), Term::atom("yes")),
                ("b".into(), Term::atom("no")),
            ],
        );

        assert_eq!(convert(&mut context, this, that), Ok(true));
    }

    #[test]
    fn convert_letrec_is_alpha_equivalent() {
        let mut context = context();

        let this = Term::let_rec(vec![("x", Term::Type, Term::label("x"))], Term::label("x"));

        let that = Term::let_rec(vec![("y", Term::Type, Term::label("y"))], Term::label("y"));

        assert_eq!(convert(&mut context, this, that), Ok(true));
    }

    #[test]
    fn convert_times_out_on_pathological_inputs() {
        let mut context = context();

        context.define("loop", Term::label("loop"));

        let this = Term::pair_type(
            "x",
            Term::apply(Term::func("z", Term::label("z")), [Term::label("loop")]),
            Term::label("x"),
        );

        let that = Term::pair_type("y", Term::label("loop"), Term::label("y"));

        assert_eq!(convert(&mut context, this, that), Err(()));
    }
}
