use {
    super::{
        Apply, Atom, AtomType, FltPrim, FltType, Func, FuncType, IntPrim, IntType, Let, LetRec,
        Match, Name, Pair, PairType, Split, Term, Type,
    },
    crate::parser::{
        Parser, ParserError, catch, fail, lazy, many0, many1, pure, run_parser, sep_by0,
        sep_by1, take_eof, take_exact, take_while,
    },
    std::str::FromStr,
};

fn parse_whitespace<'a>() -> Parser<'a, &'a str> {
    take_while(|char| char.is_whitespace())
}

fn parse_literal<'a>(expected: &'static str) -> Parser<'a, ()> {
    take_exact(expected).and_drop(parse_whitespace())
}

fn parse_identifier<'a>() -> Parser<'a, &'a str> {
    take_while(|char| "._-@#$!%&*".contains(char) || char.is_alphanumeric())
        .flat_map(|identifier| match identifier.is_empty() {
            true => fail("Expected identifier"),
            false => pure(identifier),
        })
        .and_drop(parse_whitespace())
}

const KEYWORDS: &[&str] = &["let", "match", "with", "case"];

fn parse_label<'a>() -> Parser<'a, Term> {
    parse_identifier()
        .flat_map(|identifier| match KEYWORDS.contains(&identifier) {
            true => fail(format!("'{identifier}' is a reserved keyword")),
            false => pure(Name::label(identifier).into()),
        })
}

fn parse_keyword<'a>(expected: &'static str) -> Parser<'a, ()> {
    parse_identifier().flat_map(move |obtained| match expected == obtained {
        true => pure(()),
        false => fail(format!(
            "Expected keyword '{expected}', obtained '{obtained}'"
        )),
    })
}

fn parse_type<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("Type")).map(|()| Type.into())
}

fn parse_int_type<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("Int")).map(|()| Term::Prim(IntType.into()))
}

fn parse_flt_type<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("Flt")).map(|()| Term::Prim(FltType.into()))
}

fn parse_int_value<'a>() -> Parser<'a, Term> {
    take_while(|char| char == '-' || char.is_ascii_digit())
        .flat_map::<i32, _>(|digits| match digits.parse() {
            Ok(value) => pure(value),
            Err(_) => fail("Expected integer literal"),
        })
        .and_drop(parse_whitespace())
        .map(|value| Term::Prim(IntPrim::Value(value).into()))
}

fn parse_flt_value<'a>() -> Parser<'a, Term> {
    take_while(|char| ".-+eE".contains(char) || char.is_ascii_digit())
        .flat_map::<f32, _>(|digits| {
            let has_dot = digits.contains('.');

            let has_decimal = digits
                .split_once('.')
                .map(|(_, suffix)| {
                    suffix
                        .chars()
                        .next()
                        .is_some_and(|char| char.is_ascii_digit())
                })
                .unwrap_or(false);

            if !has_dot || !has_decimal {
                return fail("Expected float literal with dot and decimal");
            }

            match digits.parse() {
                Ok(value) => pure(value),
                Err(_) => fail("Expected float literal"),
            }
        })
        .and_drop(parse_whitespace())
        .map(|value| Term::Prim(FltPrim::Value(value.to_bits()).into()))
}

fn parse_int_prim<'a>() -> Parser<'a, Term> {
    parse_int_type()
        .or(parse_int_value())
        .or(catch(parse_keyword("Int.eql"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(IntPrim::eql(left, right).into())))
        .or(catch(parse_keyword("Int.add"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(IntPrim::add(left, right).into())))
        .or(catch(parse_keyword("Int.sub"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(IntPrim::sub(left, right).into())))
        .or(catch(parse_keyword("Int.mul"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(IntPrim::mul(left, right).into())))
}

fn parse_flt_prim<'a>() -> Parser<'a, Term> {
    parse_flt_type()
        .or(parse_flt_value())
        .or(catch(parse_keyword("Flt.add"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(FltPrim::add(left, right).into())))
        .or(catch(parse_keyword("Flt.sub"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(FltPrim::sub(left, right).into())))
        .or(catch(parse_keyword("Flt.mul"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(FltPrim::mul(left, right).into())))
}

fn parse_prim<'a>() -> Parser<'a, Term> {
    parse_flt_prim().or(parse_int_prim())
}

fn parse_atom_label<'a>() -> Parser<'a, Atom> {
    take_exact("'").and_keep(parse_identifier()).map(Atom::from)
}

fn parse_atom<'a>() -> Parser<'a, Term> {
    parse_atom_label().map(Into::into)
}

fn parse_atom_type<'a>() -> Parser<'a, Term> {
    parse_literal("'{")
        .and_keep(sep_by0(
            || parse_identifier().map(Atom::from),
            || parse_literal(","),
        ))
        .and_drop(parse_literal("}"))
        .map(|atoms| AtomType::new(atoms).into())
}

fn parse_parens<'a>() -> Parser<'a, Term> {
    parse_literal("(")
        .and_keep(lazy(parse_term))
        .and_drop(parse_literal(")"))
}

fn parse_pair_type<'a>() -> Parser<'a, Term> {
    catch(
        parse_literal("(")
            .and_keep(parse_identifier())
            .and_drop(parse_literal(":"))
            .and(lazy(parse_term))
            .and_drop(parse_literal(","))
            .and(lazy(parse_term))
            .and_drop(parse_literal(")")),
    )
    .map(|((label, input), output)| PairType::new(label, input, output).into())
}

fn parse_pair<'a>() -> Parser<'a, Term> {
    catch(
        parse_literal("(")
            .and_keep(lazy(parse_term))
            .and_drop(parse_literal(","))
            .and(lazy(parse_term))
            .and_drop(parse_literal(")")),
    )
    .map(|(fst, snd)| Pair::new(fst, snd).into())
}

fn parse_func_type<'a>() -> Parser<'a, Term> {
    catch(
        parse_literal("(")
            .and_keep(parse_identifier())
            .and_drop(parse_literal(":"))
            .and(lazy(parse_term))
            .and_drop(parse_literal(")"))
            .and_drop(parse_literal("->")),
    )
    .and(lazy(parse_term))
    .map(|((label, input), output)| FuncType::new(label, input, output).into())
}

fn parse_func<'a>() -> Parser<'a, Term> {
    catch(parse_identifier().and_drop(parse_literal("=>")))
        .and(lazy(parse_term))
        .map(|(label, body)| Func::new(label, body).into())
}

fn parse_match_case<'a>() -> Parser<'a, (Atom, Term)> {
    catch(
        parse_keyword("case")
            .and_keep(parse_atom_label())
            .and_drop(parse_literal("=>")),
    )
    .and(lazy(parse_term))
    .and_drop(parse_literal(";"))
}

fn parse_match<'a>() -> Parser<'a, Term> {
    catch(
        parse_keyword("match")
            .and_keep(lazy(parse_term))
            .and_drop(parse_keyword("with"))
            .and(parse_identifier())
            .and_drop(parse_literal("=>")),
    )
    .and(lazy(parse_term))
    .and_drop(parse_literal(";"))
    .and(many1(parse_match_case))
    .map(|(((head, motive_label), motive), cases)| {
        Match::new(head, motive_label, motive, cases).into()
    })
}

fn parse_binding<'a>() -> Parser<'a, (&'a str, Term, Term)> {
    parse_identifier()
        .and_drop(parse_literal(":"))
        .and(lazy(parse_term))
        .and_drop(parse_literal("="))
        .and(lazy(parse_term))
        .map(|((label, type_), body)| (label, type_, body))
}

fn parse_let_rec<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("let").and_drop(parse_literal("{")))
        .and_keep(sep_by1(parse_binding, || parse_literal(";")))
        .and_drop(parse_literal("}"))
        .and_drop(parse_literal(";"))
        .and(lazy(parse_term))
        .map(|(items, tail)| LetRec::new(items, tail).into())
}

fn parse_split<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("let").and_drop(parse_literal("(")))
        .and_keep(parse_identifier())
        .and_drop(parse_literal(","))
        .and(parse_identifier())
        .and_drop(parse_literal(")"))
        .and_drop(parse_keyword("with"))
        .and(parse_identifier())
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
        .and_drop(parse_literal("="))
        .and(lazy(parse_term))
        .and_drop(parse_literal(";"))
        .and(lazy(parse_term))
        .map(
            |(((((fst_label, snd_label), motive_label), motive), head), tail)| {
                Split::new(head, motive_label, motive, fst_label, snd_label, tail).into()
            },
        )
}

fn parse_let<'a>() -> Parser<'a, Term> {
    catch(
        parse_keyword("let")
            .and_keep(parse_identifier())
            .and_drop(parse_literal(":"))
            .and(lazy(parse_term))
            .and_drop(parse_literal("=")),
    )
    .and(lazy(parse_term))
    .and_drop(parse_literal(";"))
    .and(lazy(parse_term))
    .map(|(((label, type_), body), tail)| Let::new(label, type_, body, tail).into())
}

fn parse_atomic_term<'a>() -> Parser<'a, Term> {
    parse_type()
        .or(parse_prim())
        .or(parse_atom_type())
        .or(parse_atom())
        .or(parse_pair_type())
        .or(parse_pair())
        .or(parse_parens())
        .or(parse_label())
}

fn parse_term<'a>() -> Parser<'a, Term> {
    parse_let_rec()
        .or(parse_split())
        .or(parse_let())
        .or(parse_match())
        .or(parse_func_type())
        .or(parse_func())
        .or(parse_atomic_term()
            .and(many0(parse_atomic_term))
            .map(|(head, params)| Apply::many(head, params)))
}

impl FromStr for Term {
    type Err = ParserError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        run_parser(
            parse_whitespace()
                .and_keep(parse_term())
                .and_drop(take_eof()),
            input,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_let_rec_func_and_apply() {
        let term = "let { id : (x : Type) -> Type = x => x }; id a"
            .parse::<Term>()
            .unwrap();

        assert_eq!(
            term,
            LetRec::new(
                vec![(
                    "id",
                    FuncType::new("x", Type, Type),
                    Func::new("x", Name::label("x"))
                )],
                Apply::many(Name::label("id"), [Name::label("a")]),
            )
            .into(),
        );
    }

    #[test]
    fn parse_let_pair_and_atoms() {
        let term = "let x : '{hot, cold} = 'hot; (x, 'cold)"
            .parse::<Term>()
            .unwrap();

        assert_eq!(
            term,
            Let::new(
                "x",
                AtomType::new(["hot", "cold"]),
                Atom::from("hot"),
                Pair::new(Name::label("x"), Atom::from("cold")),
            )
            .into(),
        );
    }

    #[test]
    fn parse_split_with_motive() {
        let term = "let (x, y) with p => Type = ('left, 'right); p"
            .parse::<Term>()
            .unwrap();

        assert_eq!(
            term,
            Split::new(
                Pair::new(Atom::from("left"), Atom::from("right")),
                "p",
                Type,
                "x",
                "y",
                Name::label("p"),
            )
            .into(),
        );
    }

    #[test]
    fn parse_match_single_case() {
        let term = "match 'foo with k => '{foo}; case 'foo => 'foo;"
            .parse::<Term>()
            .unwrap();

        assert_eq!(
            term,
            Match::new(
                Atom::from("foo"),
                "k",
                AtomType::new(["foo"]),
                [(Atom::from("foo"), Atom::from("foo"))],
            )
            .into(),
        );
    }

    #[test]
    fn parse_int_literal_and_flt_literal_are_disambiguated() {
        assert_eq!(
            "42".parse::<Term>().unwrap(),
            Term::Prim(IntPrim::Value(42).into())
        );

        assert_eq!(
            "42.0".parse::<Term>().unwrap(),
            Term::Prim(FltPrim::Value(42.0_f32.to_bits()).into())
        );
    }

    #[test]
    fn parse_prim() {
        assert_eq!("Int".parse::<Term>().unwrap(), Term::Prim(IntType.into()));
        assert_eq!("Flt".parse::<Term>().unwrap(), Term::Prim(FltType.into()));
        assert_eq!(
            "42".parse::<Term>().unwrap(),
            Term::Prim(IntPrim::Value(42).into())
        );
        assert_eq!(
            "1.5".parse::<Term>().unwrap(),
            Term::Prim(FltPrim::Value(1.5_f32.to_bits()).into())
        );

        assert_eq!(
            "Int.add 1 2".parse::<Term>().unwrap(),
            Term::Prim(
                IntPrim::add(
                    Term::Prim(IntPrim::Value(1).into()),
                    Term::Prim(IntPrim::Value(2).into())
                )
                .into()
            )
        );

        assert_eq!(
            "Flt.mul 1.5 2.0".parse::<Term>().unwrap(),
            Term::Prim(
                FltPrim::mul(
                    Term::Prim(FltPrim::Value(1.5_f32.to_bits()).into()),
                    Term::Prim(FltPrim::Value(2.0_f32.to_bits()).into())
                )
                .into()
            )
        );
    }
}
