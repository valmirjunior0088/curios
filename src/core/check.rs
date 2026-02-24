use super::{Context, Intrinsic, Term};

pub enum Error {
    ReducePreempted { term: Term },
    ConvertPreempted { this: Term, that: Term },
    CannotInfer { term: Term },
    TypeMismatch { term: Term, type_: Term },
}

impl Error {
    fn reduce_preempted(term: &Term) -> Self {
        Self::ReducePreempted { term: term.into() }
    }

    fn convert_preempted(this: &Term, that: &Term) -> Self {
        Self::ConvertPreempted {
            this: this.into(),
            that: that.into(),
        }
    }

    fn cannot_infer(term: &Term) -> Self {
        Self::CannotInfer { term: term.into() }
    }

    fn type_mismatch(term: &Term, type_: &Term) -> Self {
        Self::TypeMismatch {
            term: term.into(),
            type_: type_.into(),
        }
    }
}

fn reduce(context: &mut Context, term: &Term) -> Result<Term, Error> {
    super::reduce(context, term).map_err(|super::Preempted| Error::reduce_preempted(term))
}

fn convert(context: &mut Context, this: &Term, that: &Term) -> Result<bool, Error> {
    super::convert(context, this, that)
        .map_err(|super::Preempted| Error::convert_preempted(this, that))
}

fn infer(context: &mut Context, term: &Term) -> Result<Term, Error> {
    match term {
        Term::Type => Ok(Term::Type),
        Term::Intrinsic { intrinsic } => match intrinsic {
            Intrinsic::IntType | Intrinsic::FltType => Ok(Term::Type),
            Intrinsic::Int(_) => Ok(Term::int_type()),
            Intrinsic::IntEql(first, second)
            | Intrinsic::IntAdd(first, second)
            | Intrinsic::IntSub(first, second)
            | Intrinsic::IntMul(first, second) => {
                check(context, first, &Term::int_type())?;
                check(context, second, &Term::int_type())?;

                Ok(Term::int_type())
            }
            Intrinsic::Flt(_) => Ok(Term::flt_type()),
            Intrinsic::FltAdd(first, second)
            | Intrinsic::FltSub(first, second)
            | Intrinsic::FltMul(first, second) => {
                check(context, first, &Term::flt_type())?;
                check(context, second, &Term::flt_type())?;

                Ok(Term::flt_type())
            }
        },
        Term::FuncType { input, output } => {
            check(context, input, &Term::Type)?;

            let label = context.fresh();

            context.with_frame(|context| {
                context.assume(&label, input);

                check(context, &output.open(&[&Term::label(label)]), &Term::Type)
            })?;

            Ok(Term::Type)
        }
        Term::Apply { head, param } => {
            let head_type = infer(context, head)?;
            let head_type = reduce(context, &head_type)?;

            let (input, output) = if let Term::FuncType { input, output } = head_type {
                (input, output)
            } else {
                return Err(Error::cannot_infer(term));
            };

            check(context, param, &input)?;

            Ok(output.open(&[param.as_ref()]))
        }
        Term::PairType { input, output } => {
            check(context, input, &Term::Type)?;

            let label = context.fresh();

            context.with_frame(|context| {
                context.assume(&label, input);

                check(context, &output.open(&[&Term::label(label)]), &Term::Type)
            })?;

            Ok(Term::Type)
        }
        Term::Split { head, motive, tail } => {
            let head_type = infer(context, head)?;
            let head_type = reduce(context, &head_type)?;

            let (input, output) = if let Term::PairType { input, output } = head_type {
                (input, output)
            } else {
                return Err(Error::cannot_infer(term));
            };

            let head_label = context.fresh();

            context.with_frame(|context| {
                context.assume(
                    &head_label,
                    &Term::PairType {
                        input: input.clone(),
                        output: output.clone(),
                    },
                );

                check(
                    context,
                    &motive.open(&[&Term::label(head_label)]),
                    &Term::Type,
                )
            })?;

            let first_label = context.fresh();
            let second_label = context.fresh();

            let type_ = motive.open(&[head.as_ref()]);

            context.with_frame(|context| {
                context.assume(&first_label, &input);

                context.assume(&second_label, &output.open(&[&Term::label(&first_label)]));

                check(
                    context,
                    &tail.open(&[&Term::label(&first_label), &Term::label(&second_label)]),
                    &motive.open(&[&Term::pair(
                        Term::label(first_label),
                        Term::label(second_label),
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
            let head_type = infer(context, head)?;
            let head_type = reduce(context, &head_type)?;

            let atoms = if let Term::AtomType { atoms } = head_type {
                atoms
            } else {
                return Err(Error::cannot_infer(term));
            };

            let head_label = context.fresh();

            context.with_frame(|context| {
                context.assume(&head_label, &Term::atom_type(&atoms));

                check(
                    context,
                    &motive.open(&[&Term::label(head_label)]),
                    &Term::Type,
                )
            })?;

            if cases.len() != atoms.len() {
                return Err(Error::cannot_infer(term));
            }

            for atom in &atoms {
                let body = if let Some(body) = cases.get(atom) {
                    body
                } else {
                    return Err(Error::cannot_infer(term));
                };

                check(context, body, &motive.open(&[&Term::atom(atom)]))?;
            }

            Ok(motive.open(&[head.as_ref()]))
        }
        Term::Let { type_, body, tail } => {
            check(context, type_, &Term::Type)?;
            check(context, body, type_)?;

            let label = context.fresh();

            context.with_frame(|context| {
                context.define_assuming(&label, type_, body);

                infer(context, &tail.open(&[&Term::label(label)]))
            })
        }
        Term::LetRec { items, tail } => {
            let labels = (0..items.len())
                .map(|_| context.fresh())
                .collect::<Vec<_>>();

            let label_terms = labels.iter().map(Term::label).collect::<Vec<_>>();
            let label_terms = label_terms.iter().collect::<Vec<_>>();

            let items = items
                .iter()
                .map(|(type_, body)| (type_.open(&label_terms), body.open(&label_terms)))
                .collect::<Vec<_>>();

            let tail = tail.open(&label_terms);

            context.with_frame(|context| {
                for (label, (type_, _)) in labels.iter().zip(items.iter()) {
                    context.assume(label, type_);
                }

                for (type_, _) in &items {
                    check(context, type_, &Term::Type)?;
                }

                for (_, (type_, body)) in labels.iter().zip(items.iter()) {
                    check(context, body, type_)?;
                }

                for (label, (_, body)) in labels.iter().zip(items.iter()) {
                    context.define(label, body);
                }

                infer(context, &tail)
            })
        }
        Term::Name { name } => match context.assumption(name.unwrap()) {
            Some(type_) => Ok(type_.into()),
            None => Err(Error::cannot_infer(&name.into())),
        },
        _ => Err(Error::cannot_infer(term)),
    }
}

pub fn check(context: &mut Context, term: &Term, type_: &Term) -> Result<(), Error> {
    match term {
        Term::Func { body } => {
            let (input, output) = if let Term::FuncType { input, output } = reduce(context, type_)?
            {
                (input, output)
            } else {
                return Err(Error::type_mismatch(term, type_));
            };

            let label = context.fresh();

            context.with_frame(|context| {
                context.assume(&label, &input);
                check(
                    context,
                    &body.open(&[&Term::label(&label)]),
                    &output.open(&[&Term::label(label)]),
                )
            })
        }
        Term::Pair { first, second } => {
            let (input, output) = if let Term::PairType { input, output } = reduce(context, type_)?
            {
                (input, output)
            } else {
                return Err(Error::type_mismatch(term, type_));
            };

            check(context, first, &input)?;
            check(context, second, &output.open(&[first.as_ref()]))
        }
        Term::Atom { atom } => {
            let atoms = if let Term::AtomType { atoms } = reduce(context, type_)? {
                atoms
            } else {
                return Err(Error::type_mismatch(term, type_));
            };

            if atoms.contains(&atom) {
                Ok(())
            } else {
                Err(Error::type_mismatch(term, type_))
            }
        }
        _ => {
            let type_inferred = infer(context, term)?;
            let type_inferred = convert(context, &type_inferred, type_)?;

            match type_inferred {
                true => Ok(()),
                false => Err(Error::type_mismatch(term, type_)),
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
                    ("left", Term::atom_type(["hot"])),
                    ("right", Term::atom_type(["cold"])),
                ],
            ),
        );

        assert!(check(&mut context, &pair_type, &Term::Type).is_ok());

        let pair = Term::pair(Term::atom("left"), Term::atom("hot"));

        assert!(check(&mut context, &pair, &pair_type).is_ok());

        let pair = Term::pair(Term::atom("right"), Term::atom("cold"));

        assert!(check(&mut context, &pair, &pair_type).is_ok());
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
                    ("left", Term::atom_type(["hot"])),
                    ("right", Term::atom_type(["cold"])),
                ],
            ),
        );

        let pair = Term::pair(Term::atom("left"), Term::atom("cold"));

        assert!(matches!(
            check(&mut context, &pair, &pair_type),
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

        assert!(check(&mut context, &term, &func_type).is_ok());
    }

    #[test]
    fn check_preempts_on_cyclic_expected_type() {
        let mut context = context();

        context.define("loop", &Term::label("loop"));

        assert!(matches!(
            check(&mut context, &Term::Type, &Term::label("loop")),
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

        assert!(check(&mut context, &term, &type_).is_ok());
    }

    #[test]
    fn check_intrinsic_ops_typecheck() {
        let mut context = context();

        assert!(
            check(
                &mut context,
                &Term::int_eql(Term::int(1), Term::int(1)),
                &Term::int_type(),
            )
            .is_ok()
        );

        assert!(
            check(
                &mut context,
                &Term::flt_add(Term::flt(1.5), Term::flt(2.0)),
                &Term::flt_type(),
            )
            .is_ok()
        );
    }

    #[test]
    fn check_intrinsic_ops_reject_wrong_operand_types() {
        let mut context = context();

        assert!(matches!(
            check(
                &mut context,
                &Term::int_add(Term::int(1), Term::flt(2.0)),
                &Term::int_type(),
            ),
            Err(Error::TypeMismatch { .. })
        ));
    }
}
