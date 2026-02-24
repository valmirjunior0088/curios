use {
    super::{Context, Term},
    std::time::{Duration, Instant},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Preempted;

pub fn reduce(context: &mut Context, term: Term) -> Result<Term, Preempted> {
    Reduce::new(context.timeout()).reduce(context, term)
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

    fn timed_out(&self) -> bool {
        Instant::now() > self.deadline
    }

    fn reduce(&mut self, context: &mut Context, mut term: Term) -> Result<Term, Preempted> {
        loop {
            if self.timed_out() {
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

                    term = body.open(&[&*param]);
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

                    term = tail.open(&[&*first, &*second]);
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
                        Some(body) => body.as_ref().clone(),
                        None => {
                            break Ok(Term::Match {
                                head: Term::Atom { atom }.into(),
                                motive,
                                cases,
                            });
                        }
                    };
                }
                Term::Let { body, tail, .. } => {
                    term = tail.open(&[&*body]);
                }
                Term::Name { name } => match context.definition(name.unwrap()) {
                    Some(next) => term = next,
                    None => break Ok(Term::Name { name }),
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

        assert_eq!(reduce(&mut context, term), Ok(Term::atom("ok")));
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
            reduce(&mut context, term),
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
                ("a".into(), Term::atom("yes")),
                ("b".into(), Term::atom("no")),
            ],
        );

        assert_eq!(reduce(&mut context, term), Ok(Term::atom("yes")));
    }

    #[test]
    fn reduce_let_then_name_unfolds_definition() {
        let mut context = context();

        context.define("y", Term::atom("done"));

        let term = Term::let_("x", Term::Type, Term::label("y"), Term::label("x"));

        assert_eq!(reduce(&mut context, term), Ok(Term::atom("done")));
    }

    #[test]
    fn reduce_name_cycle_times_out() {
        let mut context = context();

        context.define("loop", Term::label("loop"));

        assert_eq!(reduce(&mut context, Term::label("loop")), Err(Preempted));
    }
}
