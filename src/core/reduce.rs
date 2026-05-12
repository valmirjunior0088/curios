use {
    super::{
        Apply, Context, Func, Let, Match, Name, Pair, Preempted, Prim, Split, Term,
    },
    std::time::{Duration, Instant},
};

pub fn reduce(context: &mut Context, term: &Term) -> Result<Term, Preempted> {
    Reduce::new(context.timeout()).reduce(context, term.clone())
}

enum Step {
    Continue(Term),
    Break(Term),
}

#[derive(Debug)]
struct Reduce {
    deadline: Instant,
}

impl Reduce {
    fn new(timeout: Duration) -> Self {
        Self {
            deadline: Instant::now() + timeout,
        }
    }

    fn reduce_prim(&mut self, context: &mut Context, prim: &Prim) -> Result<Term, Preempted> {
        match prim {
            Prim::IntType => Ok(Term::Prim(Prim::IntType)),
            Prim::IntValue(value) => Ok(Term::Prim(Prim::IntValue(*value))),
            Prim::IntEql(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::IntValue(left)), Term::Prim(Prim::IntValue(right))) => {
                        Term::Prim(Prim::IntValue(if left == right { 1 } else { 0 }))
                    }
                    (left, right) => Term::Prim(Prim::int_eql(left, right)),
                })
            }
            Prim::IntAdd(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::IntValue(left)), Term::Prim(Prim::IntValue(right))) => {
                        Term::Prim(Prim::IntValue(left.wrapping_add(right)))
                    }
                    (left, right) => Term::Prim(Prim::int_add(left, right)),
                })
            }
            Prim::IntSub(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::IntValue(left)), Term::Prim(Prim::IntValue(right))) => {
                        Term::Prim(Prim::IntValue(left.wrapping_sub(right)))
                    }
                    (left, right) => Term::Prim(Prim::int_sub(left, right)),
                })
            }
            Prim::IntMul(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::IntValue(left)), Term::Prim(Prim::IntValue(right))) => {
                        Term::Prim(Prim::IntValue(left.wrapping_mul(right)))
                    }
                    (left, right) => Term::Prim(Prim::int_mul(left, right)),
                })
            }
            Prim::FltType => Ok(Term::Prim(Prim::FltType)),
            Prim::FltValue(bits) => Ok(Term::Prim(Prim::FltValue(*bits))),
            Prim::FltAdd(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::FltValue(left)), Term::Prim(Prim::FltValue(right))) => {
                        Term::Prim(Prim::FltValue(
                            (f32::from_bits(left) + f32::from_bits(right)).to_bits(),
                        ))
                    }
                    (left, right) => Term::Prim(Prim::flt_add(left, right)),
                })
            }
            Prim::FltSub(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::FltValue(left)), Term::Prim(Prim::FltValue(right))) => {
                        Term::Prim(Prim::FltValue(
                            (f32::from_bits(left) - f32::from_bits(right)).to_bits(),
                        ))
                    }
                    (left, right) => Term::Prim(Prim::flt_sub(left, right)),
                })
            }
            Prim::FltMul(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::FltValue(left)), Term::Prim(Prim::FltValue(right))) => {
                        Term::Prim(Prim::FltValue(
                            (f32::from_bits(left) * f32::from_bits(right)).to_bits(),
                        ))
                    }
                    (left, right) => Term::Prim(Prim::flt_mul(left, right)),
                })
            }
        }
    }

    fn reduce_apply(&mut self, context: &mut Context, apply: Apply) -> Result<Step, Preempted> {
        let Apply { head, param } = apply;
        match self.reduce(context, *head)? {
            Term::Func(Func { body }) => Ok(Step::Continue(body.open(&[param.as_ref()]))),
            head => Ok(Step::Break(
                Apply {
                    head: head.into(),
                    param,
                }
                .into(),
            )),
        }
    }

    fn reduce_split(&mut self, context: &mut Context, split: Split) -> Result<Step, Preempted> {
        let Split { head, motive, tail } = split;
        match self.reduce(context, *head)? {
            Term::Pair(Pair { fst, snd }) => {
                Ok(Step::Continue(tail.open(&[fst.as_ref(), snd.as_ref()])))
            }
            head => Ok(Step::Break(
                Split {
                    head: head.into(),
                    motive,
                    tail,
                }
                .into(),
            )),
        }
    }

    fn reduce_match(&mut self, context: &mut Context, match_: Match) -> Result<Step, Preempted> {
        let Match {
            head,
            motive,
            cases,
        } = match_;
        let atom = match self.reduce(context, *head)? {
            Term::Atom(atom) => atom,
            head => {
                return Ok(Step::Break(
                    Match {
                        head: head.into(),
                        motive,
                        cases,
                    }
                    .into(),
                ));
            }
        };

        match cases.get(&atom) {
            Some(body) => Ok(Step::Continue(body.as_ref().clone())),
            None => Ok(Step::Break(
                Match {
                    head: Term::from(atom).into(),
                    motive,
                    cases,
                }
                .into(),
            )),
        }
    }

    fn reduce_let(&self, let_: Let) -> Step {
        Step::Continue(let_.tail.open(&[let_.body.as_ref()]))
    }

    fn reduce_name(&self, context: &Context, name: Name) -> Step {
        match context.definition(name.unwrap()) {
            Some(next) => Step::Continue(next.clone()),
            None => Step::Break(name.into()),
        }
    }

    fn reduce(&mut self, context: &mut Context, mut term: Term) -> Result<Term, Preempted> {
        loop {
            if Instant::now() > self.deadline {
                break Err(Preempted);
            }

            let step = match term {
                Term::Apply(apply) => self.reduce_apply(context, apply)?,
                Term::Split(split) => self.reduce_split(context, split)?,
                Term::Match(match_) => self.reduce_match(context, match_)?,
                Term::Let(let_) => self.reduce_let(let_),
                Term::Prim(prim) => Step::Break(self.reduce_prim(context, &prim)?),
                Term::Name(name) => self.reduce_name(context, name),
                term => Step::Break(term),
            };

            match step {
                Step::Continue(next) => term = next,
                Step::Break(result) => break Ok(result),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use {
        super::*,
        crate::core::{Atom, Let, Name, Type},
        std::time::Duration,
    };

    fn context() -> Context {
        Context::new(Duration::from_millis(10))
    }

    #[test]
    fn reduce_apply_beta_reduces() {
        let mut context = context();

        let term = Apply::many(Func::new("x", Name::label("x")), [Atom::from("ok")]);

        assert_eq!(reduce(&mut context, &term), Ok(Atom::from("ok").into()));
    }

    #[test]
    fn reduce_split_opens_pair_tail() {
        let mut context = context();

        let term = Split::new(
            Pair::new(Atom::from("left"), Atom::from("right")),
            "p",
            Type,
            "x",
            "y",
            Pair::new(Name::label("x"), Name::label("y")),
        )
        .into();

        assert_eq!(
            reduce(&mut context, &term),
            Ok(Pair::new(Atom::from("left"), Atom::from("right")).into())
        );
    }

    #[test]
    fn reduce_match_selects_case() {
        let mut context = context();

        let term = Match::new(
            Atom::from("a"),
            "m",
            Type,
            vec![("a", Atom::from("yes")), ("b", Atom::from("no"))],
        )
        .into();

        assert_eq!(reduce(&mut context, &term), Ok(Atom::from("yes").into()));
    }

    #[test]
    fn reduce_let_then_name_unfolds_definition() {
        let mut context = context();

        context.define("y", &Atom::from("done").into());

        let term = Let::new("x", Type, Name::label("y"), Name::label("x")).into();

        assert_eq!(reduce(&mut context, &term), Ok(Atom::from("done").into()));
    }

    #[test]
    fn reduce_name_cycle_times_out() {
        let mut context = context();

        context.define("loop", &Name::label("loop").into());

        assert_eq!(
            reduce(&mut context, &Name::label("loop").into()),
            Err(Preempted)
        );
    }

    #[test]
    fn reduce_int_add_computes() {
        let mut context = context();

        assert_eq!(
            reduce(
                &mut context,
                &Term::Prim(Prim::int_add(
                    Term::Prim(Prim::IntValue(1)),
                    Term::Prim(Prim::IntValue(2))
                ))
            ),
            Ok(Term::Prim(Prim::IntValue(3)))
        );
    }

    #[test]
    fn reduce_int_eql_returns_one_for_true_and_zero_for_false() {
        let mut context = context();

        assert_eq!(
            reduce(
                &mut context,
                &Term::Prim(Prim::int_eql(
                    Term::Prim(Prim::IntValue(4)),
                    Term::Prim(Prim::IntValue(4))
                ))
            ),
            Ok(Term::Prim(Prim::IntValue(1)))
        );
        assert_eq!(
            reduce(
                &mut context,
                &Term::Prim(Prim::int_eql(
                    Term::Prim(Prim::IntValue(4)),
                    Term::Prim(Prim::IntValue(5))
                ))
            ),
            Ok(Term::Prim(Prim::IntValue(0)))
        );
    }

    #[test]
    fn reduce_flt_mul_computes() {
        let mut context = context();

        assert_eq!(
            reduce(
                &mut context,
                &Term::Prim(Prim::flt_mul(
                    Term::Prim(Prim::FltValue(1.5_f32.to_bits())),
                    Term::Prim(Prim::FltValue(2.0_f32.to_bits()))
                ))
            ),
            Ok(Term::Prim(Prim::FltValue(3.0_f32.to_bits())))
        );
    }
}
