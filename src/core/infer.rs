use super::{
    AtomType, Context, Error, FltType, FuncType, IntType, Name, Pair, PairType, Prim, Term, Type,
    erase, typing::reduce,
};

pub fn infer(context: &mut Context, term: &Term) -> Result<Term, Error> {
    match term {
        Term::Type => Ok(Type.into()),
        Term::Prim(prim) => match prim {
            Prim::IntType | Prim::FltType => Ok(Type.into()),
            Prim::Int(_) => Ok(IntType.into()),
            Prim::IntEql(left, right)
            | Prim::IntAdd(left, right)
            | Prim::IntSub(left, right)
            | Prim::IntMul(left, right) => {
                erase(context, left, &IntType.into())?;
                erase(context, right, &IntType.into())?;

                Ok(IntType.into())
            }
            Prim::Flt(_) => Ok(FltType.into()),
            Prim::FltAdd(left, right) | Prim::FltSub(left, right) | Prim::FltMul(left, right) => {
                erase(context, left, &FltType.into())?;
                erase(context, right, &FltType.into())?;

                Ok(FltType.into())
            }
        },
        Term::FuncType(FuncType { input, output }) => {
            erase(context, input, &Type.into())?;

            let label = context.fresh();

            context.with_frame(|context| {
                context.assume(&label, input);

                erase(
                    context,
                    &output.open(&[&Name::label(label).into()]),
                    &Type.into(),
                )
                .map(|_| ())
            })?;

            Ok(Type.into())
        }
        Term::Apply(super::Apply { head, param }) => {
            let head_type = infer(context, head)?;
            let head_type = reduce(context, &head_type)?;

            let (input, output) = if let Term::FuncType(FuncType { input, output }) = head_type {
                (input, output)
            } else {
                return Err(Error::cannot_infer(term.clone()));
            };

            erase(context, param, &input)?;

            Ok(output.open(&[param.as_ref()]))
        }
        Term::PairType(PairType { input, output }) => {
            erase(context, input, &Type.into())?;

            let label = context.fresh();

            context.with_frame(|context| {
                context.assume(&label, input);

                erase(
                    context,
                    &output.open(&[&Name::label(label).into()]),
                    &Type.into(),
                )
                .map(|_| ())
            })?;

            Ok(Type.into())
        }
        Term::Split(super::Split { head, motive, tail }) => {
            let head_type = infer(context, head)?;
            let head_type = reduce(context, &head_type)?;

            let (input, output) = if let Term::PairType(PairType { input, output }) = head_type {
                (input, output)
            } else {
                return Err(Error::cannot_infer(term.clone()));
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

                erase(
                    context,
                    &motive.open(&[&Name::label(head_label).into()]),
                    &Type.into(),
                )
                .map(|_| ())
            })?;

            let fst_label = context.fresh();
            let snd_label = context.fresh();

            let type_ = motive.open(&[head.as_ref()]);

            context.with_frame(|context| {
                context.assume(&fst_label, &input);

                context.assume(&snd_label, &output.open(&[&Name::label(&fst_label).into()]));

                erase(
                    context,
                    &tail.open(&[
                        &Name::label(&fst_label).into(),
                        &Name::label(&snd_label).into(),
                    ]),
                    &motive
                        .open(&[&Pair::new(Name::label(fst_label), Name::label(snd_label)).into()]),
                )
                .map(|_| ())
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
                return Err(Error::cannot_infer(term.clone()));
            };

            let head_label = context.fresh();

            context.with_frame(|context| {
                context.assume(&head_label, &AtomType::new(atoms.iter().cloned()).into());

                erase(
                    context,
                    &motive.open(&[&Name::label(head_label).into()]),
                    &Type.into(),
                )
                .map(|_| ())
            })?;

            if cases.len() != atoms.len() {
                return Err(Error::cannot_infer(term.clone()));
            }

            for atom in &atoms {
                let body = if let Some(body) = cases.get(atom) {
                    body
                } else {
                    return Err(Error::cannot_infer(term.clone()));
                };

                erase(context, body, &motive.open(&[&atom.clone().into()]))?;
            }

            Ok(motive.open(&[head.as_ref()]))
        }
        Term::Let(super::Let { type_, body, tail }) => {
            erase(context, type_, &Type.into())?;
            erase(context, body, type_)?;

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
                    erase(context, type_, &Type.into())?;
                }

                for (_, (type_, body)) in labels.iter().zip(items.iter()) {
                    erase(context, body, type_)?;
                }

                for (label, (_, body)) in labels.iter().zip(items.iter()) {
                    context.define(label, body);
                }

                infer(context, &tail)
            })
        }
        Term::Name(name) => match context.assumption(name.unwrap()) {
            Some(type_) => Ok(type_.clone()),
            None => Err(Error::cannot_infer(name.clone())),
        },
        _ => Err(Error::cannot_infer(term.clone())),
    }
}
