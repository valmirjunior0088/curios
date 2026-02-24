use super::{Context, Term};

pub enum Error {
    ReducePreempted { term: Term },
    ConvertPreempted { this: Term, that: Term },
    CannotInfer { term: Term },
    TypeMismatch { term: Term, type_: Term },
}

fn infer(context: &mut Context, term: Term) -> Result<Term, Error> {
    match term {
        Term::Type => Ok(Term::Type),
        Term::FuncType { input, output } => {
            check(context, (*input).clone(), Term::Type)?;

            let label = context.fresh();

            context.with_frame(|context| {
                context.assume(&label, *input);

                check(context, output.open(&[&Term::label(&label)]), Term::Type)
            })?;

            Ok(Term::Type)
        }
        Term::Apply { head, param } => {
            let head_type = infer(context, (*head).clone())?;

            let head_type = super::reduce(context, head_type.clone())
                .map_err(|()| Error::ReducePreempted { term: head_type })?;

            let (input, output) = if let Term::FuncType { input, output } = head_type {
                (input, output)
            } else {
                return Err(Error::CannotInfer {
                    term: Term::Apply {
                        head: head.clone(),
                        param: param.clone(),
                    },
                });
            };

            check(context, (*param).clone(), *input)?;

            Ok(output.open(&[&*param]))
        }
        Term::PairType { input, output } => {
            check(context, (*input).clone(), Term::Type)?;

            let label = context.fresh();

            context.with_frame(|context| {
                context.assume(&label, *input);

                check(context, output.open(&[&Term::label(&label)]), Term::Type)
            })?;

            Ok(Term::Type)
        }
        Term::Split { head, motive, tail } => {
            let head_type = infer(context, (*head).clone())?;

            let head_type = super::reduce(context, head_type.clone())
                .map_err(|()| Error::ReducePreempted { term: head_type })?;

            let (input, output) = if let Term::PairType { input, output } = head_type {
                (input, output)
            } else {
                return Err(Error::CannotInfer {
                    term: Term::Split {
                        head: head.clone(),
                        motive: motive.clone(),
                        tail: tail.clone(),
                    },
                });
            };

            let head_label = context.fresh();

            context.with_frame(|context| {
                context.assume(
                    &head_label,
                    Term::PairType {
                        input: input.clone(),
                        output: output.clone(),
                    },
                );

                check(
                    context,
                    motive.clone().open(&[&Term::label(&head_label)]),
                    Term::Type,
                )
            })?;

            let first_label = context.fresh();
            let second_label = context.fresh();

            let type_ = motive.clone().open(&[&*head]);

            context.with_frame(|context| {
                context.assume(&first_label, (*input).clone());

                context.assume(
                    &second_label,
                    output.clone().open(&[&Term::label(&first_label)]),
                );

                check(
                    context,
                    tail.open(&[&Term::label(&first_label), &Term::label(&second_label)]),
                    motive.open(&[&Term::pair(
                        Term::label(&first_label),
                        Term::label(&second_label),
                    )]),
                )
            })?;

            Ok(type_)
        }
        Term::AtomType { .. } => Ok(Term::Type),
        Term::Match {
            head,
            motive,
            cases,
        } => {
            let head_type = infer(context, (*head).clone())?;

            let head_type = super::reduce(context, head_type.clone())
                .map_err(|()| Error::ReducePreempted { term: head_type })?;

            let atoms = if let Term::AtomType { atoms } = head_type {
                atoms
            } else {
                return Err(Error::CannotInfer {
                    term: Term::Match {
                        head: head.clone(),
                        motive: motive.clone(),
                        cases: cases.clone(),
                    },
                });
            };

            let head_label = context.fresh();

            context.with_frame(|context| {
                context.assume(
                    &head_label,
                    Term::AtomType {
                        atoms: atoms.clone(),
                    },
                );

                check(
                    context,
                    motive.clone().open(&[&Term::label(&head_label)]),
                    Term::Type,
                )
            })?;

            if cases.len() != atoms.len() {
                return Err(Error::CannotInfer {
                    term: Term::Match {
                        head: head.clone(),
                        motive: motive.clone(),
                        cases: cases.clone(),
                    },
                });
            }

            for atom in &atoms {
                let body = if let Some(body) = cases.get(atom) {
                    body
                } else {
                    return Err(Error::CannotInfer {
                        term: Term::Match {
                            head: head.clone(),
                            motive: motive.clone(),
                            cases: cases.clone(),
                        },
                    });
                };

                check(
                    context,
                    body.as_ref().clone(),
                    motive.clone().open(&[&Term::Atom { atom: atom.clone() }]),
                )?;
            }

            Ok(motive.open(&[&*head]))
        }
        Term::Let { type_, body, tail } => {
            check(context, (*type_).clone(), Term::Type)?;
            check(context, (*body).clone(), (*type_).clone())?;

            let label = context.fresh();

            context.with_frame(|context| {
                context.define_assuming(&label, (*type_).clone(), (*body).clone());

                infer(context, tail.open(&[&Term::label(&label)]))
            })
        }
        Term::LetRec { items, tail } => {
            let labels = (0..items.len())
                .map(|_| context.fresh())
                .collect::<Vec<_>>();

            let label_terms = labels.iter().map(Term::label).collect::<Vec<_>>();
            let label_terms = label_terms.iter().collect::<Vec<_>>();

            let items = items
                .into_iter()
                .map(|(type_, body)| (type_.open(&label_terms), body.open(&label_terms)))
                .collect::<Vec<_>>();

            let tail = tail.open(&label_terms);

            context.with_frame(|context| {
                for (label, (type_, _)) in labels.iter().zip(items.iter()) {
                    context.assume(label, type_.clone());
                }

                for (type_, _) in &items {
                    check(context, type_.clone(), Term::Type)?;
                }

                for (_, (type_, body)) in labels.iter().zip(items.iter()) {
                    check(context, body.clone(), type_.clone())?;
                }

                for (label, (_, body)) in labels.iter().zip(items.iter()) {
                    context.define(label, body.clone());
                }

                infer(context, tail)
            })
        }
        Term::Name { name } => match context.assumption(name.unwrap()) {
            Some(type_) => Ok(type_),
            None => Err(Error::CannotInfer {
                term: Term::Name { name },
            }),
        },
        term => Err(Error::CannotInfer { term }),
    }
}

pub fn check(context: &mut Context, term: Term, type_: Term) -> Result<(), Error> {
    match term {
        Term::Func { body } => {
            let type_reduced =
                super::reduce(context, type_.clone()).map_err(|()| Error::ReducePreempted {
                    term: type_.clone(),
                })?;

            let (input, output) = if let Term::FuncType { input, output } = type_reduced {
                (input, output)
            } else {
                return Err(Error::TypeMismatch {
                    term: Term::Func { body },
                    type_,
                });
            };

            let label = context.fresh();

            context.with_frame(|context| {
                context.assume(&label, *input);
                check(
                    context,
                    body.open(&[&Term::label(&label)]),
                    output.open(&[&Term::label(&label)]),
                )
            })
        }
        Term::Pair { first, second } => {
            let type_reduced =
                super::reduce(context, type_.clone()).map_err(|()| Error::ReducePreempted {
                    term: type_.clone(),
                })?;

            let (input, output) = if let Term::PairType { input, output } = type_reduced {
                (input, output)
            } else {
                return Err(Error::TypeMismatch {
                    term: Term::Pair { first, second },
                    type_,
                });
            };

            check(context, (*first).clone(), *input)?;
            check(context, *second, output.open(&[&*first]))
        }
        Term::Atom { atom } => {
            let type_reduced =
                super::reduce(context, type_.clone()).map_err(|()| Error::ReducePreempted {
                    term: type_.clone(),
                })?;

            let atoms = if let Term::AtomType { atoms } = type_reduced {
                atoms
            } else {
                return Err(Error::TypeMismatch {
                    term: Term::Atom { atom },
                    type_,
                });
            };

            if atoms.contains(&atom) {
                Ok(())
            } else {
                Err(Error::TypeMismatch {
                    term: Term::Atom { atom },
                    type_,
                })
            }
        }
        term => {
            let type_inferred = infer(context, term.clone())?;

            let type_converted = super::convert(context, type_inferred.clone(), type_.clone())
                .map_err(|()| Error::ConvertPreempted {
                    this: type_inferred.clone(),
                    that: type_.clone(),
                })?;

            match type_converted {
                true => Ok(()),
                false => Err(Error::TypeMismatch { term, type_ }),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use {super::*, std::time::Duration};

    fn context() -> Context {
        Context::new(Duration::from_secs(1))
    }

    #[test]
    fn check_dependent_pair_type_over_atom_match_and_pair_value() {
        let mut context = context();

        let pair_type = Term::pair_type(
            "x",
            Term::atom_type(["left", "right"]),
            Term::match_(
                Term::label("x"),
                "m",
                Term::Type,
                vec![
                    ("left".into(), Term::atom_type(["hot"])),
                    ("right".into(), Term::atom_type(["cold"])),
                ],
            ),
        );

        assert!(check(&mut context, pair_type.clone(), Term::Type).is_ok());

        let pair = Term::pair(Term::atom("left"), Term::atom("hot"));

        assert!(check(&mut context, pair, pair_type.clone()).is_ok());

        let pair = Term::pair(Term::atom("right"), Term::atom("cold"));

        assert!(check(&mut context, pair, pair_type).is_ok());
    }

    #[test]
    fn check_dependent_pair_type_rejects_wrong_branch_atom() {
        let mut context = context();

        let pair_type = Term::pair_type(
            "x",
            Term::atom_type(["left", "right"]),
            Term::match_(
                Term::label("x"),
                "m",
                Term::Type,
                vec![
                    ("left".into(), Term::atom_type(["hot"])),
                    ("right".into(), Term::atom_type(["cold"])),
                ],
            ),
        );

        let pair = Term::pair(Term::atom("left"), Term::atom("cold"));

        assert!(matches!(
            check(&mut context, pair, pair_type),
            Err(Error::TypeMismatch { .. })
        ));
    }

    #[test]
    fn check_letrec_single_identity_function() {
        let mut context = context();

        let func_type = Term::func_type("x", Term::atom_type(["a"]), Term::atom_type(["a"]));

        let term = Term::let_rec(
            vec![("f", func_type.clone(), Term::func("x", Term::label("x")))],
            Term::label("f"),
        );

        assert!(check(&mut context, term, func_type).is_ok());
    }

    #[test]
    fn check_preempts_on_cyclic_expected_type() {
        let mut context = context();

        context.define("loop", Term::label("loop"));

        assert!(matches!(
            check(&mut context, Term::Type, Term::label("loop")),
            Err(Error::ConvertPreempted { .. })
        ));
    }

    #[test]
    fn check_accepts_term_level_loop_with_stable_type() {
        let mut context = context();

        let type_ = Term::atom_type(["a"]);

        let term = Term::let_rec(
            vec![("loop", type_.clone(), Term::label("loop"))],
            Term::label("loop"),
        );

        assert!(check(&mut context, term, type_).is_ok());
    }
}
