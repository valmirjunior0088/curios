use {
    super::{
        Apply, Context, FltPrim, Func, IntPrim, Let, Match, Name, Pair, Preempted, Prim, Split,
        Term,
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

    fn reduce_int_prim(
        &mut self,
        context: &mut Context,
        int_prim: &IntPrim,
    ) -> Result<Term, Preempted> {
        match int_prim {
            IntPrim::Type => Ok(Term::Prim(Prim::Int(IntPrim::Type))),
            IntPrim::Value(value) => Ok(Term::Prim(Prim::Int(IntPrim::Value(*value)))),
            IntPrim::Eql(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (
                        Term::Prim(Prim::Int(IntPrim::Value(left))),
                        Term::Prim(Prim::Int(IntPrim::Value(right))),
                    ) => Term::Prim(IntPrim::Value(if left == right { 1 } else { 0 }).into()),
                    (left, right) => Term::Prim(IntPrim::eql(left, right).into()),
                })
            }
            IntPrim::Add(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (
                        Term::Prim(Prim::Int(IntPrim::Value(left))),
                        Term::Prim(Prim::Int(IntPrim::Value(right))),
                    ) => Term::Prim(IntPrim::Value(left.wrapping_add(right)).into()),
                    (left, right) => Term::Prim(IntPrim::add(left, right).into()),
                })
            }
            IntPrim::Sub(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (
                        Term::Prim(Prim::Int(IntPrim::Value(left))),
                        Term::Prim(Prim::Int(IntPrim::Value(right))),
                    ) => Term::Prim(IntPrim::Value(left.wrapping_sub(right)).into()),
                    (left, right) => Term::Prim(IntPrim::sub(left, right).into()),
                })
            }
            IntPrim::Mul(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (
                        Term::Prim(Prim::Int(IntPrim::Value(left))),
                        Term::Prim(Prim::Int(IntPrim::Value(right))),
                    ) => Term::Prim(IntPrim::Value(left.wrapping_mul(right)).into()),
                    (left, right) => Term::Prim(IntPrim::mul(left, right).into()),
                })
            }
        }
    }

    fn reduce_flt_prim(
        &mut self,
        context: &mut Context,
        flt_prim: &FltPrim,
    ) -> Result<Term, Preempted> {
        match flt_prim {
            FltPrim::Type => Ok(Term::Prim(Prim::Flt(FltPrim::Type))),
            FltPrim::Value(bits) => Ok(Term::Prim(Prim::Flt(FltPrim::Value(*bits)))),
            FltPrim::Add(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (
                        Term::Prim(Prim::Flt(FltPrim::Value(left))),
                        Term::Prim(Prim::Flt(FltPrim::Value(right))),
                    ) => Term::Prim(
                        FltPrim::Value((f32::from_bits(left) + f32::from_bits(right)).to_bits())
                            .into(),
                    ),
                    (left, right) => Term::Prim(FltPrim::add(left, right).into()),
                })
            }
            FltPrim::Sub(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (
                        Term::Prim(Prim::Flt(FltPrim::Value(left))),
                        Term::Prim(Prim::Flt(FltPrim::Value(right))),
                    ) => Term::Prim(
                        FltPrim::Value((f32::from_bits(left) - f32::from_bits(right)).to_bits())
                            .into(),
                    ),
                    (left, right) => Term::Prim(FltPrim::sub(left, right).into()),
                })
            }
            FltPrim::Mul(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (
                        Term::Prim(Prim::Flt(FltPrim::Value(left))),
                        Term::Prim(Prim::Flt(FltPrim::Value(right))),
                    ) => Term::Prim(
                        FltPrim::Value((f32::from_bits(left) * f32::from_bits(right)).to_bits())
                            .into(),
                    ),
                    (left, right) => Term::Prim(FltPrim::mul(left, right).into()),
                })
            }
        }
    }

    fn reduce_prim(&mut self, context: &mut Context, prim: &Prim) -> Result<Term, Preempted> {
        match prim {
            Prim::Int(int_prim) => self.reduce_int_prim(context, int_prim),
            Prim::Flt(flt_prim) => self.reduce_flt_prim(context, flt_prim),
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
                &Term::Prim(
                    IntPrim::add(
                        Term::Prim(IntPrim::Value(1).into()),
                        Term::Prim(IntPrim::Value(2).into())
                    )
                    .into()
                )
            ),
            Ok(Term::Prim(IntPrim::Value(3).into()))
        );
    }

    #[test]
    fn reduce_int_eql_returns_one_for_true_and_zero_for_false() {
        let mut context = context();

        assert_eq!(
            reduce(
                &mut context,
                &Term::Prim(
                    IntPrim::eql(
                        Term::Prim(IntPrim::Value(4).into()),
                        Term::Prim(IntPrim::Value(4).into())
                    )
                    .into()
                )
            ),
            Ok(Term::Prim(IntPrim::Value(1).into()))
        );
        assert_eq!(
            reduce(
                &mut context,
                &Term::Prim(
                    IntPrim::eql(
                        Term::Prim(IntPrim::Value(4).into()),
                        Term::Prim(IntPrim::Value(5).into())
                    )
                    .into()
                )
            ),
            Ok(Term::Prim(IntPrim::Value(0).into()))
        );
    }

    #[test]
    fn reduce_flt_mul_computes() {
        let mut context = context();

        assert_eq!(
            reduce(
                &mut context,
                &Term::Prim(
                    FltPrim::mul(
                        Term::Prim(FltPrim::Value(1.5_f32.to_bits()).into()),
                        Term::Prim(FltPrim::Value(2.0_f32.to_bits()).into())
                    )
                    .into()
                )
            ),
            Ok(Term::Prim(FltPrim::Value(3.0_f32.to_bits()).into()))
        );
    }
}
