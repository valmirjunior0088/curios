use {
    super::{Context, Prim, Term},
    std::time::{Duration, Instant},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Preempted;

pub fn reduce(context: &mut Context, term: &Term) -> Result<Term, Preempted> {
    Reduce::new(context.timeout()).reduce(context, term.into())
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

    fn reduce_prim(
        &mut self,
        context: &mut Context,
        prim: &Prim,
    ) -> Result<Term, Preempted> {
        match prim {
            Prim::IntType => Ok(Prim::IntType.into()),
            Prim::Int(value) => Ok(Prim::Int(*value).into()),
            Prim::IntEql(first, second) => {
                let first = self.reduce(context, first.as_ref().into())?;
                let second = self.reduce(context, second.as_ref().into())?;

                Ok(match (first, second) {
                    (
                        Term::Prim {
                            prim: Prim::Int(first),
                        },
                        Term::Prim {
                            prim: Prim::Int(second),
                        },
                    ) => Term::int(if first == second { 1 } else { 0 }),
                    (first, second) => Prim::IntEql(first.into(), second.into()).into(),
                })
            }
            Prim::IntAdd(first, second) => {
                let first = self.reduce(context, first.as_ref().into())?;
                let second = self.reduce(context, second.as_ref().into())?;

                Ok(match (first, second) {
                    (
                        Term::Prim {
                            prim: Prim::Int(first),
                        },
                        Term::Prim {
                            prim: Prim::Int(second),
                        },
                    ) => Term::int(first.wrapping_add(second)),
                    (first, second) => Prim::IntAdd(first.into(), second.into()).into(),
                })
            }
            Prim::IntSub(first, second) => {
                let first = self.reduce(context, first.as_ref().into())?;
                let second = self.reduce(context, second.as_ref().into())?;

                Ok(match (first, second) {
                    (
                        Term::Prim {
                            prim: Prim::Int(first),
                        },
                        Term::Prim {
                            prim: Prim::Int(second),
                        },
                    ) => Term::int(first.wrapping_sub(second)),
                    (first, second) => Prim::IntSub(first.into(), second.into()).into(),
                })
            }
            Prim::IntMul(first, second) => {
                let first = self.reduce(context, first.as_ref().into())?;
                let second = self.reduce(context, second.as_ref().into())?;

                Ok(match (first, second) {
                    (
                        Term::Prim {
                            prim: Prim::Int(first),
                        },
                        Term::Prim {
                            prim: Prim::Int(second),
                        },
                    ) => Term::int(first.wrapping_mul(second)),
                    (first, second) => Prim::IntMul(first.into(), second.into()).into(),
                })
            }
            Prim::FltType => Ok(Prim::FltType.into()),
            Prim::Flt(bits) => Ok(Prim::Flt(*bits).into()),
            Prim::FltAdd(first, second) => {
                let first = self.reduce(context, first.as_ref().into())?;
                let second = self.reduce(context, second.as_ref().into())?;

                Ok(match (first, second) {
                    (
                        Term::Prim {
                            prim: Prim::Flt(first),
                        },
                        Term::Prim {
                            prim: Prim::Flt(second),
                        },
                    ) => Term::flt(f32::from_bits(first) + f32::from_bits(second)),
                    (first, second) => Prim::FltAdd(first.into(), second.into()).into(),
                })
            }
            Prim::FltSub(first, second) => {
                let first = self.reduce(context, first.as_ref().into())?;
                let second = self.reduce(context, second.as_ref().into())?;

                Ok(match (first, second) {
                    (
                        Term::Prim {
                            prim: Prim::Flt(first),
                        },
                        Term::Prim {
                            prim: Prim::Flt(second),
                        },
                    ) => Term::flt(f32::from_bits(first) - f32::from_bits(second)),
                    (first, second) => Prim::FltSub(first.into(), second.into()).into(),
                })
            }
            Prim::FltMul(first, second) => {
                let first = self.reduce(context, first.as_ref().into())?;
                let second = self.reduce(context, second.as_ref().into())?;

                Ok(match (first, second) {
                    (
                        Term::Prim {
                            prim: Prim::Flt(first),
                        },
                        Term::Prim {
                            prim: Prim::Flt(second),
                        },
                    ) => Term::flt(f32::from_bits(first) * f32::from_bits(second)),
                    (first, second) => Prim::FltMul(first.into(), second.into()).into(),
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
                Term::Apply { head, param } => {
                    let body = match self.reduce(context, *head)? {
                        Term::Func { body } => body,
                        head => {
                            break Ok(Term::Apply {
                                head: head.into(),
                                param,
                            });
                        }
                    };

                    term = body.open(&[param.as_ref()]);
                }
                Term::Split { head, motive, tail } => {
                    let (first, second) = match self.reduce(context, *head)? {
                        Term::Pair { first, second } => (first, second),
                        head => {
                            break Ok(Term::Split {
                                head: head.into(),
                                motive,
                                tail,
                            });
                        }
                    };

                    term = tail.open(&[first.as_ref(), second.as_ref()]);
                }
                Term::Match {
                    head,
                    motive,
                    cases,
                } => {
                    let atom = match self.reduce(context, *head)? {
                        Term::Atom { atom } => atom,
                        head => {
                            break Ok(Term::Match {
                                head: head.into(),
                                motive,
                                cases,
                            });
                        }
                    };

                    term = match cases.get(&atom) {
                        Some(body) => body.as_ref().into(),
                        None => {
                            break Ok(Term::Match {
                                head: Term::atom(atom).into(),
                                motive,
                                cases,
                            });
                        }
                    };
                }
                Term::Let { body, tail, .. } => {
                    term = tail.open(&[body.as_ref()]);
                }
                Term::Prim { prim } => {
                    break Ok(self.reduce_prim(context, &prim)?);
                }
                Term::Name { name } => match context.definition(name.unwrap()) {
                    Some(next) => term = next.into(),
                    None => break Ok(name.into()),
                },
                term => break Ok(term),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use {super::*, std::time::Duration};

    fn context() -> Context {
        Context::new(Duration::from_millis(10))
    }

    #[test]
    fn reduce_apply_beta_reduces() {
        let mut context = context();

        let term = Term::apply(Term::func("x", Term::label("x")), [Term::atom("ok")]);

        assert_eq!(reduce(&mut context, &term), Ok(Term::atom("ok")));
    }

    #[test]
    fn reduce_split_opens_pair_tail() {
        let mut context = context();

        let term = Term::split(
            Term::pair(Term::atom("left"), Term::atom("right")),
            "p",
            Term::Type,
            "x",
            "y",
            Term::pair(Term::label("x"), Term::label("y")),
        );

        assert_eq!(
            reduce(&mut context, &term),
            Ok(Term::pair(Term::atom("left"), Term::atom("right")))
        );
    }

    #[test]
    fn reduce_match_selects_case() {
        let mut context = context();

        let term = Term::match_(
            Term::atom("a"),
            "m",
            Term::Type,
            vec![
                ("a", Term::atom("yes")),
                ("b", Term::atom("no")),
            ],
        );

        assert_eq!(reduce(&mut context, &term), Ok(Term::atom("yes")));
    }

    #[test]
    fn reduce_let_then_name_unfolds_definition() {
        let mut context = context();

        context.define("y", &Term::atom("done"));

        let term = Term::let_("x", Term::Type, Term::label("y"), Term::label("x"));

        assert_eq!(reduce(&mut context, &term), Ok(Term::atom("done")));
    }

    #[test]
    fn reduce_name_cycle_times_out() {
        let mut context = context();

        context.define("loop", &Term::label("loop"));

        assert_eq!(reduce(&mut context, &Term::label("loop")), Err(Preempted));
    }

    #[test]
    fn reduce_int_add_computes() {
        let mut context = context();

        assert_eq!(
            reduce(&mut context, &Term::int_add(Term::int(1), Term::int(2))),
            Ok(Term::int(3))
        );
    }

    #[test]
    fn reduce_int_eql_returns_one_for_true_and_zero_for_false() {
        let mut context = context();

        assert_eq!(
            reduce(&mut context, &Term::int_eql(Term::int(4), Term::int(4))),
            Ok(Term::int(1))
        );
        assert_eq!(
            reduce(&mut context, &Term::int_eql(Term::int(4), Term::int(5))),
            Ok(Term::int(0))
        );
    }

    #[test]
    fn reduce_flt_mul_computes() {
        let mut context = context();

        assert_eq!(
            reduce(&mut context, &Term::flt_mul(Term::flt(1.5), Term::flt(2.0))),
            Ok(Term::flt(3.0))
        );
    }
}
