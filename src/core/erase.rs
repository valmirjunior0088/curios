use super::{
    Apply, AtomType, Context, ErasedApply, ErasedAtom, ErasedFunc, ErasedLet, ErasedLetRec,
    ErasedMatch, ErasedName, ErasedPair, ErasedPrim, ErasedSplit, ErasedTerm, Error, FltType, Func,
    FuncType, IntType, Let, LetRec, Match, Name, Pair, PairType, Prim, Split, Term, Type, infer,
    typing::{convert, reduce},
};

fn expect(
    context: &mut Context,
    term: &Term,
    inferred: &Term,
    expected: &Term,
) -> Result<(), Error> {
    match convert(context, inferred, expected)? {
        true => Ok(()),
        false => Err(Error::type_mismatch(term, expected)),
    }
}

fn erase_prim(
    context: &mut Context,
    term: &Term,
    prim: &Prim,
    expected: &Term,
) -> Result<ErasedPrim, Error> {
    match prim {
        Prim::IntType => {
            expect(context, term, &Type.into(), expected)?;

            Ok(().into())
        }
        &Prim::Int(value) => {
            expect(context, term, &IntType.into(), expected)?;

            Ok(value.into())
        }
        Prim::IntEql(left, right) => {
            expect(context, term, &IntType.into(), expected)?;

            Ok(ErasedPrim::IntEql(
                erase(context, left, &IntType.into())?.into(),
                erase(context, right, &IntType.into())?.into(),
            ))
        }
        Prim::IntAdd(left, right) => {
            expect(context, term, &IntType.into(), expected)?;

            Ok(ErasedPrim::IntAdd(
                erase(context, left, &IntType.into())?.into(),
                erase(context, right, &IntType.into())?.into(),
            ))
        }
        Prim::IntSub(left, right) => {
            expect(context, term, &IntType.into(), expected)?;

            Ok(ErasedPrim::IntSub(
                erase(context, left, &IntType.into())?.into(),
                erase(context, right, &IntType.into())?.into(),
            ))
        }
        Prim::IntMul(left, right) => {
            expect(context, term, &IntType.into(), expected)?;

            Ok(ErasedPrim::IntMul(
                erase(context, left, &IntType.into())?.into(),
                erase(context, right, &IntType.into())?.into(),
            ))
        }
        Prim::FltType => {
            expect(context, term, &Type.into(), expected)?;

            Ok(().into())
        }
        &Prim::Flt(bits) => {
            expect(context, term, &FltType.into(), expected)?;

            Ok(f32::from_bits(bits).into())
        }
        Prim::FltAdd(left, right) => {
            expect(context, term, &FltType.into(), expected)?;

            Ok(ErasedPrim::FltAdd(
                erase(context, left, &FltType.into())?.into(),
                erase(context, right, &FltType.into())?.into(),
            ))
        }
        Prim::FltSub(left, right) => {
            expect(context, term, &FltType.into(), expected)?;

            Ok(ErasedPrim::FltSub(
                erase(context, left, &FltType.into())?.into(),
                erase(context, right, &FltType.into())?.into(),
            ))
        }
        Prim::FltMul(left, right) => {
            expect(context, term, &FltType.into(), expected)?;

            Ok(ErasedPrim::FltMul(
                erase(context, left, &FltType.into())?.into(),
                erase(context, right, &FltType.into())?.into(),
            ))
        }
    }
}

pub fn erase(context: &mut Context, term: &Term, expected: &Term) -> Result<ErasedTerm, Error> {
    match term {
        Term::Prim(prim) => Ok(erase_prim(context, term, prim, expected)?.into()),
        Term::Type => {
            expect(context, term, &Type.into(), expected)?;

            Ok(().into())
        }
        Term::FuncType(_) => {
            let inferred = infer(context, term)?;
            expect(context, term, &inferred, expected)?;

            Ok(().into())
        }
        Term::Func(Func { body }) => {
            let Term::FuncType(FuncType { input, output }) = reduce(context, expected)? else {
                return Err(Error::type_mismatch(term, expected));
            };

            let captures = body.collect().into_iter().collect::<Vec<_>>();
            let param = context.fresh();
            let param_term = Name::label(&param).into();
            let body = body.open(&[&param_term]);

            let body = context.with_frame(|context| {
                context.assume(&param, &input);

                erase(context, &body, &output.open(&[&param_term]))
            })?;

            Ok(ErasedFunc {
                captures,
                param,
                body: body.into(),
            }
            .into())
        }
        Term::Apply(Apply { head, param }) => {
            let head_type = infer(context, head)?;
            let head_type = reduce(context, &head_type)?;

            let Term::FuncType(FuncType { input, output }) = &head_type else {
                return Err(Error::cannot_infer(term.clone()));
            };

            let erased = ErasedApply {
                head: erase(context, head, &head_type)?.into(),
                param: erase(context, param, input)?.into(),
            };

            expect(context, term, &output.open(&[param.as_ref()]), expected)?;

            Ok(erased.into())
        }
        Term::PairType(_) => {
            let inferred = infer(context, term)?;
            expect(context, term, &inferred, expected)?;

            Ok(().into())
        }
        Term::Pair(Pair { fst, snd }) => {
            let Term::PairType(PairType { input, output }) = reduce(context, expected)? else {
                return Err(Error::type_mismatch(term, expected));
            };

            Ok(ErasedPair {
                fst: erase(context, fst, &input)?.into(),
                snd: erase(context, snd, &output.open(&[fst.as_ref()]))?.into(),
            }
            .into())
        }
        Term::Split(Split { head, motive, tail }) => {
            let head_type = infer(context, head)?;
            let head_type = reduce(context, &head_type)?;

            let (input, output) = if let Term::PairType(PairType { input, output }) = &head_type {
                (input.clone(), output.clone())
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
            })?;

            let fst = context.fresh();
            let snd = context.fresh();
            let fst_term = Term::from(Name::label(&fst));
            let snd_term = Term::from(Name::label(&snd));
            let tail = tail.open(&[&fst_term, &snd_term]);
            let tail_type = motive.open(&[&Pair::new(Name::label(&fst), Name::label(&snd)).into()]);

            let erased = context.with_frame(|context| {
                context.assume(&fst, &input);
                context.assume(&snd, &output.open(&[&fst_term]));

                Ok::<_, Error>(ErasedSplit {
                    head: erase(context, head, &head_type)?.into(),
                    fst,
                    snd,
                    tail: erase(context, &tail, &tail_type)?.into(),
                })
            })?;

            expect(context, term, &motive.open(&[head.as_ref()]), expected)?;

            Ok(erased.into())
        }
        Term::AtomType(_) => {
            let inferred = infer(context, term)?;
            expect(context, term, &inferred, expected)?;

            Ok(().into())
        }
        Term::Atom(atom) => {
            let Term::AtomType(AtomType { atoms }) = reduce(context, expected)? else {
                return Err(Error::type_mismatch(term, expected));
            };

            let atom = atoms
                .iter()
                .position(|candidate| candidate == atom)
                .ok_or_else(|| Error::type_mismatch(term, expected))?;

            Ok(ErasedAtom { index: atom }.into())
        }
        Term::Match(Match {
            head,
            motive,
            cases,
        }) => {
            let head_type = infer(context, head)?;
            let head_type = reduce(context, &head_type)?;

            let atoms = if let Term::AtomType(AtomType { atoms }) = &head_type {
                atoms.clone()
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
            })?;

            if cases.len() != atoms.len() {
                return Err(Error::cannot_infer(term.clone()));
            }

            let cases = atoms
                .iter()
                .map(|atom| {
                    let body = if let Some(body) = cases.get(atom) {
                        body
                    } else {
                        return Err(Error::cannot_infer(term.clone()));
                    };

                    erase(context, body, &motive.open(&[&atom.clone().into()])).map(Into::into)
                })
                .collect::<Result<Vec<_>, Error>>()?;

            expect(context, term, &motive.open(&[head.as_ref()]), expected)?;

            Ok(ErasedMatch {
                head: erase(context, head, &head_type)?.into(),
                cases,
            }
            .into())
        }
        Term::Let(Let {
            type_: body_type,
            body,
            tail,
        }) => {
            erase(context, body_type, &Type.into())?;

            let name = context.fresh();
            let erased_body = erase(context, body, body_type)?;
            let name_term = Name::label(&name).into();
            let tail = tail.open(&[&name_term]);

            let tail = context.with_frame(|context| {
                context.define_assuming(&name, body_type, body);

                erase(context, &tail, expected)
            })?;

            Ok(ErasedLet {
                name,
                body: erased_body.into(),
                tail: tail.into(),
            }
            .into())
        }
        Term::LetRec(LetRec { items, tail }) => {
            let names = (0..items.len())
                .map(|_| context.fresh())
                .collect::<Vec<_>>();

            let label_terms = names
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

            let erased = context.with_frame(|context| {
                for (name, (type_, _)) in names.iter().zip(items.iter()) {
                    context.assume(name, type_);
                }

                for (type_, _) in &items {
                    erase(context, type_, &Type.into())?;
                }

                let erased_items = items
                    .iter()
                    .map(|(type_, body)| erase(context, body, type_).map(Into::into))
                    .collect::<Result<Vec<_>, Error>>()?;

                for (name, (_, body)) in names.iter().zip(items.iter()) {
                    context.define(name, body);
                }

                Ok(ErasedLetRec {
                    names,
                    items: erased_items,
                    tail: erase(context, &tail, expected)?.into(),
                })
            })?;

            Ok(erased.into())
        }
        Term::Name(name) => {
            let inferred = infer(context, term)?;
            expect(context, term, &inferred, expected)?;

            Ok(ErasedName::from(name.unwrap()).into())
        }
    }
}

#[cfg(test)]
mod tests {
    use {
        super::*,
        crate::core::{
            Atom, AtomType, ErasedAtom, ErasedFunc, ErasedLet, ErasedMatch, ErasedName, ErasedTerm,
            Func, FuncType, LetRec, Match, Pair, PairType, Prim, Term, Type, parse,
        },
        std::time::Duration,
    };

    fn context() -> Context {
        Context::new(Duration::from_secs(1))
    }

    #[test]
    fn erase_dependent_pair_type_over_atom_match_and_pair_value() {
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

        assert!(erase(&mut context, &pair_type, &Type.into()).is_ok());

        let pair = Term::from(Pair::new(Atom::from("left"), Atom::from("hot")));

        assert!(erase(&mut context, &pair, &pair_type).is_ok());

        let pair = Term::from(Pair::new(Atom::from("right"), Atom::from("cold")));

        assert!(erase(&mut context, &pair, &pair_type).is_ok());
    }

    #[test]
    fn erase_dependent_pair_type_rejects_wrong_branch_atom() {
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
            erase(&mut context, &pair, &pair_type),
            Err(Error::TypeMismatch { .. })
        ));
    }

    #[test]
    fn erase_letrec_single_identity_function() {
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

        assert!(erase(&mut context, &term, &func_type).is_ok());
    }

    #[test]
    fn erase_preempts_on_cyclic_expected_type() {
        let mut context = context();

        context.define("loop", &Name::label("loop").into());

        assert!(matches!(
            erase(&mut context, &Type.into(), &Name::label("loop").into()),
            Err(Error::ConvertPreempted { .. })
        ));
    }

    #[test]
    fn erase_accepts_term_level_loop_with_stable_type() {
        let mut context = context();

        let type_ = Term::from(AtomType::new(["a"]));

        let term = Term::from(LetRec::new(
            vec![("loop", type_.clone(), Name::label("loop"))],
            Name::label("loop"),
        ));

        assert!(erase(&mut context, &term, &type_).is_ok());
    }

    #[test]
    fn erase_prim_ops_typecheck() {
        let mut context = context();

        assert!(
            erase(
                &mut context,
                &Prim::int_eql(Prim::from(1), Prim::from(1)).into(),
                &IntType.into(),
            )
            .is_ok()
        );

        assert!(
            erase(
                &mut context,
                &Prim::flt_add(Prim::from(1.5), Prim::from(2.0)).into(),
                &FltType.into(),
            )
            .is_ok()
        );
    }

    #[test]
    fn erase_func_captures_free_variables_before_opening_body() {
        let atom_type = Term::from(AtomType::new(["a"]));
        let pair_type = Term::from(PairType::new("z", atom_type.clone(), atom_type.clone()));
        let type_ = Term::from(FuncType::new("x", atom_type.clone(), pair_type));
        let term = Term::from(Func::new(
            "x",
            Pair::new(Name::label("x"), Name::label("y")),
        ));

        let mut context = Context::new(Duration::from_secs(1));
        context.assume("y", &atom_type);

        erase(&mut context, &term, &type_).unwrap();

        let mut context = Context::new(Duration::from_secs(1));
        context.assume("y", &atom_type);

        let erased = erase(&mut context, &term, &type_).unwrap();

        let ErasedTerm::Func(ErasedFunc { captures, .. }) = erased else {
            panic!("expected erased func");
        };

        assert_eq!(captures.len(), 1);
        assert!(captures.contains(&"y".to_string()));
    }

    #[test]
    fn erase_rejects_wrong_prim_operand_types() {
        assert!(matches!(
            erase(
                &mut Context::new(Duration::from_secs(1)),
                &Prim::int_add(Prim::from(1), Prim::from(2.0)).into(),
                &IntType.into(),
            ),
            Err(Error::TypeMismatch { .. })
        ));
    }

    #[test]
    fn erase_match_and_atom_stress_test() {
        let type_ = parse("{:zeta, :alpha, :mu}").unwrap();

        let term = parse(
            r#"
                let outer : {:zeta, :alpha, :mu} = :mu;
                let alpha_case : {:zeta, :alpha, :mu} = :alpha;
                let mu_case : {:zeta, :alpha, :mu} = :mu;
                let zeta_case : {:zeta, :alpha, :mu} = :zeta;
                match outer with subject => {:zeta, :alpha, :mu};
                case :zeta =>
                    match alpha_case with nested => {:zeta, :alpha, :mu};
                    case :zeta => :alpha;
                    case :alpha => :mu;
                    case :mu => :zeta;;
                case :alpha =>
                    match zeta_case with nested => {:zeta, :alpha, :mu};
                    case :zeta => :mu;
                    case :alpha => :zeta;
                    case :mu => :alpha;;
                case :mu =>
                    match mu_case with nested => {:zeta, :alpha, :mu};
                    case :zeta => :zeta;
                    case :alpha => :alpha;
                    case :mu => :mu;;
            "#,
        )
        .unwrap();

        erase(&mut Context::new(Duration::from_secs(1)), &term, &type_).unwrap();
        let erased = erase(&mut Context::new(Duration::from_secs(1)), &term, &type_).unwrap();

        let ErasedTerm::Let(ErasedLet {
            name: outer_name,
            body: outer_body,
            tail,
        }) = erased
        else {
            panic!("expected outer let");
        };

        assert_eq!(outer_name, "0");
        assert!(matches!(
            *outer_body,
            ErasedTerm::Atom(ErasedAtom { index: 1 })
        ));

        let ErasedTerm::Let(ErasedLet {
            name: alpha_name,
            body: alpha_body,
            tail,
        }) = *tail
        else {
            panic!("expected alpha_case let");
        };

        assert_eq!(alpha_name, "1");
        assert!(matches!(
            *alpha_body,
            ErasedTerm::Atom(ErasedAtom { index: 0 })
        ));

        let ErasedTerm::Let(ErasedLet {
            name: mu_name,
            body: mu_body,
            tail,
        }) = *tail
        else {
            panic!("expected mu_case let");
        };

        assert_eq!(mu_name, "2");
        assert!(matches!(
            *mu_body,
            ErasedTerm::Atom(ErasedAtom { index: 1 })
        ));

        let ErasedTerm::Let(ErasedLet {
            name: zeta_name,
            body: zeta_body,
            tail,
        }) = *tail
        else {
            panic!("expected zeta_case let");
        };

        assert_eq!(zeta_name, "3");
        assert!(matches!(
            *zeta_body,
            ErasedTerm::Atom(ErasedAtom { index: 2 })
        ));

        let ErasedTerm::Match(ErasedMatch { head, cases }) = *tail else {
            panic!("expected outer erased match");
        };

        assert!(matches!(
            *head,
            ErasedTerm::Name(ErasedName { string }) if string == "0"
        ));

        assert_eq!(cases.len(), 3);

        let ErasedTerm::Match(ErasedMatch {
            head: alpha_head,
            cases: alpha_cases,
        }) = &*cases[0]
        else {
            panic!("expected nested match for :alpha case");
        };

        assert!(matches!(
            &**alpha_head,
            ErasedTerm::Name(ErasedName { string }) if string == "3"
        ));

        assert_eq!(alpha_cases.len(), 3);
        assert!(matches!(
            *alpha_cases[0],
            ErasedTerm::Atom(ErasedAtom { index: 2 })
        ));
        assert!(matches!(
            *alpha_cases[1],
            ErasedTerm::Atom(ErasedAtom { index: 0 })
        ));
        assert!(matches!(
            *alpha_cases[2],
            ErasedTerm::Atom(ErasedAtom { index: 1 })
        ));

        let ErasedTerm::Match(ErasedMatch {
            head: mu_head,
            cases: mu_cases,
        }) = &*cases[1]
        else {
            panic!("expected nested match for :mu case");
        };

        assert!(matches!(
            &**mu_head,
            ErasedTerm::Name(ErasedName { string }) if string == "2"
        ));

        assert_eq!(mu_cases.len(), 3);

        assert!(matches!(
            *mu_cases[0],
            ErasedTerm::Atom(ErasedAtom { index: 0 })
        ));

        assert!(matches!(
            *mu_cases[1],
            ErasedTerm::Atom(ErasedAtom { index: 1 })
        ));

        assert!(matches!(
            *mu_cases[2],
            ErasedTerm::Atom(ErasedAtom { index: 2 })
        ));

        let ErasedTerm::Match(ErasedMatch {
            head: zeta_head,
            cases: zeta_cases,
        }) = &*cases[2]
        else {
            panic!("expected nested match for :zeta case");
        };

        assert!(matches!(
            &**zeta_head,
            ErasedTerm::Name(ErasedName { string }) if string == "1"
        ));

        assert_eq!(zeta_cases.len(), 3);

        assert!(matches!(
            *zeta_cases[0],
            ErasedTerm::Atom(ErasedAtom { index: 1 })
        ));

        assert!(matches!(
            *zeta_cases[1],
            ErasedTerm::Atom(ErasedAtom { index: 2 })
        ));

        assert!(matches!(
            *zeta_cases[2],
            ErasedTerm::Atom(ErasedAtom { index: 0 })
        ));
    }
}
