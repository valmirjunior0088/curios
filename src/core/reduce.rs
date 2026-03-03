use {
    super::{Apply, Context, Func, Match, Pair, Prim, Split, Term},
    std::time::{Duration, Instant},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Preempted;

pub fn reduce(context: &mut Context, term: &Term) -> Result<Term, Preempted> {
    Reduce::new(context.timeout()).reduce(context, term.clone())
}

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
            Prim::IntType => Ok(Prim::IntType.into()),
            Prim::Int(value) => Ok(Prim::Int(*value).into()),
            Prim::IntEql(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Int(left)), Term::Prim(Prim::Int(right))) => {
                        Prim::from(if left == right { 1 } else { 0 }).into()
                    }
                    (left, right) => Prim::IntEql(left.into(), right.into()).into(),
                })
            }
            Prim::IntAdd(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Int(left)), Term::Prim(Prim::Int(right))) => {
                        Prim::from(left.wrapping_add(right)).into()
                    }
                    (left, right) => Prim::IntAdd(left.into(), right.into()).into(),
                })
            }
            Prim::IntSub(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Int(left)), Term::Prim(Prim::Int(right))) => {
                        Prim::from(left.wrapping_sub(right)).into()
                    }
                    (left, right) => Prim::IntSub(left.into(), right.into()).into(),
                })
            }
            Prim::IntMul(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Int(left)), Term::Prim(Prim::Int(right))) => {
                        Prim::from(left.wrapping_mul(right)).into()
                    }
                    (left, right) => Prim::IntMul(left.into(), right.into()).into(),
                })
            }
            Prim::FltType => Ok(Prim::FltType.into()),
            Prim::Flt(bits) => Ok(Prim::Flt(*bits).into()),
            Prim::FltAdd(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => {
                        Prim::from(f32::from_bits(left) + f32::from_bits(right)).into()
                    }
                    (left, right) => Prim::FltAdd(left.into(), right.into()).into(),
                })
            }
            Prim::FltSub(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => {
                        Prim::from(f32::from_bits(left) - f32::from_bits(right)).into()
                    }
                    (left, right) => Prim::FltSub(left.into(), right.into()).into(),
                })
            }
            Prim::FltMul(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => {
                        Prim::from(f32::from_bits(left) * f32::from_bits(right)).into()
                    }
                    (left, right) => Prim::FltMul(left.into(), right.into()).into(),
                })
            }
        }
    }

    fn reduce(&mut self, context: &mut Context, mut term: Term) -> Result<Term, Preempted> {
        loop {
            if Instant::now() > self.deadline {
                break Err(Preempted);
            }

            match term {
                Term::Apply(Apply { head, param }) => {
                    let body = match self.reduce(context, *head)? {
                        Term::Func(Func { body }) => body,
                        head => {
                            break Ok(Apply {
                                head: head.into(),
                                param,
                            }
                            .into());
                        }
                    };

                    term = body.open(&[param.as_ref()]);
                }
                Term::Split(Split { head, motive, tail }) => {
                    let (fst, snd) = match self.reduce(context, *head)? {
                        Term::Pair(Pair { fst, snd }) => (fst, snd),
                        head => {
                            break Ok(Split {
                                head: head.into(),
                                motive,
                                tail,
                            }
                            .into());
                        }
                    };

                    term = tail.open(&[fst.as_ref(), snd.as_ref()]);
                }
                Term::Match(Match {
                    head,
                    motive,
                    cases,
                }) => {
                    let atom = match self.reduce(context, *head)? {
                        Term::Atom(atom) => atom,
                        head => {
                            break Ok(Match {
                                head: head.into(),
                                motive,
                                cases,
                            }
                            .into());
                        }
                    };

                    term = match cases.get(&atom) {
                        Some(body) => body.as_ref().clone(),
                        None => {
                            break Ok(Match {
                                head: Term::from(atom).into(),
                                motive,
                                cases,
                            }
                            .into());
                        }
                    };
                }
                Term::Let(super::Let { body, tail, .. }) => {
                    term = tail.open(&[body.as_ref()]);
                }
                Term::Prim(prim) => {
                    break Ok(self.reduce_prim(context, &prim)?);
                }
                Term::Name(name) => match context.definition(name.unwrap()) {
                    Some(next) => term = next.clone(),
                    None => break Ok(name.into()),
                },
                term => break Ok(term),
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
                &Prim::int_add(Prim::from(1), Prim::from(2)).into()
            ),
            Ok(Prim::from(3).into())
        );
    }

    #[test]
    fn reduce_int_eql_returns_one_for_true_and_zero_for_false() {
        let mut context = context();

        assert_eq!(
            reduce(
                &mut context,
                &Prim::int_eql(Prim::from(4), Prim::from(4)).into()
            ),
            Ok(Prim::from(1).into())
        );
        assert_eq!(
            reduce(
                &mut context,
                &Prim::int_eql(Prim::from(4), Prim::from(5)).into()
            ),
            Ok(Prim::from(0).into())
        );
    }

    #[test]
    fn reduce_flt_mul_computes() {
        let mut context = context();

        assert_eq!(
            reduce(
                &mut context,
                &Prim::flt_mul(Prim::from(1.5), Prim::from(2.0)).into()
            ),
            Ok(Prim::from(3.0).into())
        );
    }
}
