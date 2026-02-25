use {
    super::{Atom, Term},
    crate::monads::parser::{
        Parser, ParserError, catch, fail, lazy, many_until, many1, pure, run_parser, sep_by0,
        sep_by1, take_eof, take_exact, take_while,
    },
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

fn parse_label<'a>() -> Parser<'a, Term> {
    parse_identifier().map(Term::label)
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
    catch(parse_keyword("Type")).map(|()| Term::Type)
}

fn parse_int_type<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("Int")).map(|()| Term::int_type())
}

fn parse_flt_type<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("Flt")).map(|()| Term::flt_type())
}

fn parse_int_literal_value<'a>() -> Parser<'a, i32> {
    take_while(|char| char == '-' || char.is_ascii_digit())
        .flat_map(|digits| match digits.parse() {
            Ok(value) => pure(value),
            Err(_) => fail("Expected integer literal"),
        })
        .and_drop(parse_whitespace())
}

fn parse_flt_literal_value<'a>() -> Parser<'a, f32> {
    take_while(|char| ".-+eE".contains(char) || char.is_ascii_digit())
        .flat_map(|digits| {
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
}

fn parse_int_literal<'a>() -> Parser<'a, Term> {
    parse_int_literal_value().map(Term::int)
}

fn parse_flt_literal<'a>() -> Parser<'a, Term> {
    parse_flt_literal_value().map(Term::flt)
}

fn parse_prim_op<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("Int.eql"))
        .and_keep(lazy(parse_atomic_term))
        .and(lazy(parse_atomic_term))
        .map(|(first, second)| Term::int_eql(first, second))
        .or(catch(parse_keyword("Int.add"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(first, second)| Term::int_add(first, second)))
        .or(catch(parse_keyword("Int.sub"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(first, second)| Term::int_sub(first, second)))
        .or(catch(parse_keyword("Int.mul"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(first, second)| Term::int_mul(first, second)))
        .or(catch(parse_keyword("Flt.add"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(first, second)| Term::flt_add(first, second)))
        .or(catch(parse_keyword("Flt.sub"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(first, second)| Term::flt_sub(first, second)))
        .or(catch(parse_keyword("Flt.mul"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(first, second)| Term::flt_mul(first, second)))
}

fn parse_atom_label<'a>() -> Parser<'a, Atom> {
    take_exact(":").and_keep(parse_identifier()).map(Atom::from)
}

fn parse_atom<'a>() -> Parser<'a, Term> {
    parse_atom_label().map(Term::atom)
}

fn parse_atom_type<'a>() -> Parser<'a, Term> {
    parse_literal("{")
        .and_keep(sep_by0(parse_atom_label, || parse_literal(",")))
        .and_drop(parse_literal("}"))
        .map(Term::atom_type)
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
    .map(|((label, input), output)| Term::pair_type(label, input, output))
}

fn parse_pair<'a>() -> Parser<'a, Term> {
    catch(
        parse_literal("(")
            .and_keep(lazy(parse_term))
            .and_drop(parse_literal(","))
            .and(lazy(parse_term))
            .and_drop(parse_literal(")")),
    )
    .map(|(first, second)| Term::pair(first, second))
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
    .map(|((label, input), output)| Term::func_type(label, input, output))
}

fn parse_func<'a>() -> Parser<'a, Term> {
    catch(parse_identifier().and_drop(parse_literal("=>")))
        .and(lazy(parse_term))
        .map(|(label, body)| Term::func(label, body))
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
            .and_keep(lazy(|| parse_term_until(|| catch(parse_keyword("with")))))
            .and(parse_identifier())
            .and_drop(parse_literal("=>")),
    )
    .and(lazy(parse_term))
    .and_drop(parse_literal(";"))
    .and(many1(parse_match_case))
    .map(|(((head, motive_label), motive), cases)| Term::match_(head, motive_label, motive, cases))
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
        .map(|(items, tail)| Term::let_rec(items, tail))
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
            |(((((first_label, second_label), motive_label), motive), head), tail)| {
                Term::split(head, motive_label, motive, first_label, second_label, tail)
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
    .map(|(((label, type_), body), tail)| Term::let_(label, type_, body, tail))
}

fn parse_atomic_term<'a>() -> Parser<'a, Term> {
    parse_type()
        .or(parse_int_type())
        .or(parse_flt_type())
        .or(parse_flt_literal())
        .or(parse_int_literal())
        .or(parse_prim_op())
        .or(parse_atom_type())
        .or(parse_atom())
        .or(parse_pair_type())
        .or(parse_pair())
        .or(parse_parens())
        .or(parse_label())
}

fn parse_apply_until<'a, G>(g: G) -> Parser<'a, Term>
where
    G: FnMut() -> Parser<'a, ()> + 'a,
{
    parse_atomic_term()
        .and(many_until(parse_atomic_term, g))
        .map(|(head, params)| Term::apply(head, params))
}

fn parse_term<'a>() -> Parser<'a, Term> {
    parse_term_until(|| fail(""))
}

fn parse_term_until<'a, G>(g: G) -> Parser<'a, Term>
where
    G: FnMut() -> Parser<'a, ()> + 'a,
{
    parse_let_rec()
        .or(parse_split())
        .or(parse_let())
        .or(parse_match())
        .or(parse_func_type())
        .or(parse_func())
        .or(parse_apply_until(g))
}

pub fn parse(input: &str) -> Result<Term, ParserError> {
    run_parser(
        parse_whitespace()
            .and_keep(parse_term())
            .and_drop(take_eof()),
        input,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_let_rec_func_and_apply() {
        let term = parse("let { id : (x : Type) -> Type = x => x }; id a").unwrap();

        assert_eq!(
            term,
            Term::let_rec(
                vec![(
                    "id",
                    Term::func_type("x", Term::Type, Term::Type),
                    Term::func("x", Term::label("x"))
                )],
                Term::apply(Term::label("id"), [Term::label("a")]),
            ),
        );
    }

    #[test]
    fn parse_let_pair_and_atoms() {
        let term = parse("let x : {:hot, :cold} = :hot; (x, :cold)").unwrap();

        assert_eq!(
            term,
            Term::let_(
                "x",
                Term::atom_type(["hot", "cold"]),
                Term::atom("hot"),
                Term::pair(Term::label("x"), Term::atom("cold")),
            ),
        );
    }

    #[test]
    fn parse_split_with_motive() {
        let term = parse("let (x, y) with p => Type = (:left, :right); p").unwrap();

        assert_eq!(
            term,
            Term::split(
                Term::pair(Term::atom("left"), Term::atom("right")),
                "p",
                Term::Type,
                "x",
                "y",
                Term::label("p"),
            ),
        );
    }

    #[test]
    fn parse_match_single_case() {
        let term = parse("match :foo with k => {:foo}; case :foo => :foo;").unwrap();

        assert_eq!(
            term,
            Term::match_(
                Term::atom("foo"),
                "k",
                Term::atom_type(["foo"]),
                [(Atom::from("foo"), Term::atom("foo"))],
            ),
        );
    }

    #[test]
    fn parse_prim() {
        assert_eq!(parse("Int").unwrap(), Term::int_type());
        assert_eq!(parse("Flt").unwrap(), Term::flt_type());
        assert_eq!(parse("42").unwrap(), Term::int(42));
        assert_eq!(parse("1.5").unwrap(), Term::flt(1.5));
        assert_eq!(
            parse("Int.add 1 2").unwrap(),
            Term::int_add(Term::int(1), Term::int(2))
        );
        assert_eq!(
            parse("Flt.mul 1.5 2.0").unwrap(),
            Term::flt_mul(Term::flt(1.5), Term::flt(2.0))
        );
    }
}
