use super::{
    AtomType, Context, FltType, Func, FuncType, IntType, Name, Pair, PairType, Prim, Term, Type,
};

pub enum Error {
    ReducePreempted { term: Term },
    ConvertPreempted { this: Term, that: Term },
    CannotInfer { term: Term },
    TypeMismatch { term: Term, type_: Term },
}

impl Error {
    fn reduce_preempted(term: &Term) -> Self {
        Self::ReducePreempted { term: term.clone() }
    }

    fn convert_preempted(this: &Term, that: &Term) -> Self {
        Self::ConvertPreempted {
            this: this.clone(),
            that: that.clone(),
        }
    }

    fn cannot_infer(term: &Term) -> Self {
        Self::CannotInfer { term: term.clone() }
    }

    fn type_mismatch(term: &Term, type_: &Term) -> Self {
        Self::TypeMismatch {
            term: term.clone(),
            type_: type_.clone(),
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
        Term::Type => Ok(Type.into()),
        Term::Prim(prim) => match prim {
            Prim::IntType | Prim::FltType => Ok(Type.into()),
            Prim::Int(_) => Ok(IntType.into()),
            Prim::IntEql(first, second)
            | Prim::IntAdd(first, second)
            | Prim::IntSub(first, second)
            | Prim::IntMul(first, second) => {
                check(context, first, &IntType.into())?;
                check(context, second, &IntType.into())?;

                Ok(IntType.into())
            }
            Prim::Flt(_) => Ok(FltType.into()),
            Prim::FltAdd(first, second)
            | Prim::FltSub(first, second)
            | Prim::FltMul(first, second) => {
                check(context, first, &FltType.into())?;
                check(context, second, &FltType.into())?;

                Ok(FltType.into())
            }
        },
        Term::FuncType(FuncType { input, output }) => {
            check(context, input, &Type.into())?;

            let label = context.fresh();

            context.with_frame(|context| {
                context.assume(&label, input);

                check(
                    context,
                    &output.open(&[&Name::label(label).into()]),
                    &Type.into(),
                )
            })?;

            Ok(Type.into())
        }
        Term::Apply(super::Apply { head, param }) => {
            let head_type = infer(context, head)?;
            let head_type = reduce(context, &head_type)?;

            let (input, output) = if let Term::FuncType(FuncType { input, output }) = head_type {
                (input, output)
            } else {
                return Err(Error::cannot_infer(term));
            };

            check(context, param, &input)?;

            Ok(output.open(&[param.as_ref()]))
        }
        Term::PairType(PairType { input, output }) => {
            check(context, input, &Type.into())?;

            let label = context.fresh();

            context.with_frame(|context| {
                context.assume(&label, input);

                check(
                    context,
                    &output.open(&[&Name::label(label).into()]),
                    &Type.into(),
                )
            })?;

            Ok(Type.into())
        }
        Term::Split(super::Split { head, motive, tail }) => {
            let head_type = infer(context, head)?;
            let head_type = reduce(context, &head_type)?;

            let (input, output) = if let Term::PairType(PairType { input, output }) = head_type {
                (input, output)
            } else {
                return Err(Error::cannot_infer(term));
            };

            let head_label = context.fresh();

            context.with_frame(|context| {
                context.assume(
                    &head_label,
                    &PairType {
                        input: input.clone(),
                        output: output.clone(),
                    }
                    .into(),
                );

                check(
                    context,
                    &motive.open(&[&Name::label(head_label).into()]),
                    &Type.into(),
                )
            })?;

            let first_label = context.fresh();
            let second_label = context.fresh();

            let type_ = motive.open(&[head.as_ref()]);

            context.with_frame(|context| {
                context.assume(&first_label, &input);

                context.assume(
                    &second_label,
                    &output.open(&[&Name::label(&first_label).into()]),
                );

                check(
                    context,
                    &tail.open(&[
                        &Name::label(&first_label).into(),
                        &Name::label(&second_label).into(),
                    ]),
                    &motive.open(&[&Pair::new(
                        Name::label(first_label),
                        Name::label(second_label),
                    )
                    .into()]),
                )
            })?;

            Ok(type_)
        }
        Term::AtomType(AtomType { .. }) => Ok(Type.into()),
        Term::Match(super::Match {
            head,
            motive,
            cases,
        }) => {
            let head_type = infer(context, head)?;
            let head_type = reduce(context, &head_type)?;

            let atoms = if let Term::AtomType(AtomType { atoms }) = head_type {
                atoms
            } else {
                return Err(Error::cannot_infer(term));
            };

            let head_label = context.fresh();

            context.with_frame(|context| {
                context.assume(&head_label, &AtomType::new(atoms.iter().cloned()).into());

                check(
                    context,
                    &motive.open(&[&Name::label(head_label).into()]),
                    &Type.into(),
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

                check(context, body, &motive.open(&[&atom.clone().into()]))?;
            }

            Ok(motive.open(&[head.as_ref()]))
        }
        Term::Let(super::Let { type_, body, tail }) => {
            check(context, type_, &Type.into())?;
            check(context, body, type_)?;

            let label = context.fresh();

            context.with_frame(|context| {
                context.define_assuming(&label, type_, body);

                infer(context, &tail.open(&[&Name::label(label).into()]))
            })
        }
        Term::LetRec(super::LetRec { items, tail }) => {
            let labels = (0..items.len())
                .map(|_| context.fresh())
                .collect::<Vec<_>>();

            let label_terms = labels
                .iter()
                .map(Name::label)
                .map(Into::into)
                .collect::<Vec<_>>();

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
                    check(context, type_, &Type.into())?;
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
        Term::Name(name) => match context.assumption(name.unwrap()) {
            Some(type_) => Ok(type_.clone()),
            None => Err(Error::cannot_infer(&name.clone().into())),
        },
        _ => Err(Error::cannot_infer(term)),
    }
}

pub fn check(context: &mut Context, term: &Term, type_: &Term) -> Result<(), Error> {
    match term {
        Term::Func(Func { body }) => {
            let (input, output) =
                if let Term::FuncType(FuncType { input, output }) = reduce(context, type_)? {
                    (input, output)
                } else {
                    return Err(Error::type_mismatch(term, type_));
                };

            let label = context.fresh();

            context.with_frame(|context| {
                context.assume(&label, &input);
                check(
                    context,
                    &body.open(&[&Name::label(&label).into()]),
                    &output.open(&[&Name::label(label).into()]),
                )
            })
        }
        Term::Pair(Pair { first, second }) => {
            let (input, output) =
                if let Term::PairType(PairType { input, output }) = reduce(context, type_)? {
                    (input, output)
                } else {
                    return Err(Error::type_mismatch(term, type_));
                };

            check(context, first, &input)?;
            check(context, second, &output.open(&[first.as_ref()]))
        }
        Term::Atom(atom) => {
            let atoms = if let Term::AtomType(AtomType { atoms }) = reduce(context, type_)? {
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
        term => {
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
    use {
        super::*,
        crate::core::{
            Atom, AtomType, Func, FuncType, LetRec, Match, Name, Pair, PairType, Prim, Type,
        },
        std::time::Duration,
    };

    fn context() -> Context {
        Context::new(Duration::from_secs(1))
    }

    #[test]
    fn check_dependent_pair_type_over_atom_match_and_pair_value() {
        let mut context = context();

        let pair_type = Term::from(PairType::new(
            "x",
            AtomType::new(["left", "right"]),
            Match::new(
                Name::label("x"),
                "m",
                Type,
                vec![
                    ("left", AtomType::new(["hot"])),
                    ("right", AtomType::new(["cold"])),
                ],
            ),
        ));

        assert!(check(&mut context, &pair_type, &Type.into()).is_ok());

        let pair = Term::from(Pair::new(Atom::from("left"), Atom::from("hot")));

        assert!(check(&mut context, &pair, &pair_type).is_ok());

        let pair = Term::from(Pair::new(Atom::from("right"), Atom::from("cold")));

        assert!(check(&mut context, &pair, &pair_type).is_ok());
    }

    #[test]
    fn check_dependent_pair_type_rejects_wrong_branch_atom() {
        let mut context = context();

        let pair_type = Term::from(PairType::new(
            "x",
            AtomType::new(["left", "right"]),
            Match::new(
                Name::label("x"),
                "m",
                Type,
                vec![
                    ("left", AtomType::new(["hot"])),
                    ("right", AtomType::new(["cold"])),
                ],
            ),
        ));

        let pair = Term::from(Pair::new(Atom::from("left"), Atom::from("cold")));

        assert!(matches!(
            check(&mut context, &pair, &pair_type),
            Err(Error::TypeMismatch { .. })
        ));
    }

    #[test]
    fn check_letrec_single_identity_function() {
        let mut context = context();

        let func_type = Term::from(FuncType::new(
            "x",
            AtomType::new(["a"]),
            AtomType::new(["a"]),
        ));

        let term = Term::from(LetRec::new(
            vec![("f", func_type.clone(), Func::new("x", Name::label("x")))],
            Name::label("f"),
        ));

        assert!(check(&mut context, &term, &func_type).is_ok());
    }

    #[test]
    fn check_preempts_on_cyclic_expected_type() {
        let mut context = context();

        context.define("loop", &Name::label("loop").into());

        assert!(matches!(
            check(&mut context, &Type.into(), &Name::label("loop").into()),
            Err(Error::ConvertPreempted { .. })
        ));
    }

    #[test]
    fn check_accepts_term_level_loop_with_stable_type() {
        let mut context = context();

        let type_ = Term::from(AtomType::new(["a"]));

        let term = Term::from(LetRec::new(
            vec![("loop", type_.clone(), Name::label("loop"))],
            Name::label("loop"),
        ));

        assert!(check(&mut context, &term, &type_).is_ok());
    }

    #[test]
    fn check_prim_ops_typecheck() {
        let mut context = context();

        assert!(
            check(
                &mut context,
                &Prim::int_eql(Prim::from(1), Prim::from(1)).into(),
                &IntType.into(),
            )
            .is_ok()
        );

        assert!(
            check(
                &mut context,
                &Prim::flt_add(Prim::from(1.5), Prim::from(2.0)).into(),
                &FltType.into(),
            )
            .is_ok()
        );
    }

    #[test]
    fn check_prim_ops_reject_wrong_operand_types() {
        let mut context = context();

        assert!(matches!(
            check(
                &mut context,
                &Prim::int_add(Prim::from(1), Prim::from(2.0)).into(),
                &IntType.into(),
            ),
            Err(Error::TypeMismatch { .. })
        ));
    }
}
