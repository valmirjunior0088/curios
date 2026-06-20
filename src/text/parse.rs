use {
    super::{
        Apply, ArrMatch, BlnMatch, Entrypoint, Field, Func, FuncType, GroupItem, Let, LetBang,
        LetSignature, LoadError, Match, Module, Motive, Name, Nat, NatLiteral, NatMatch, Pattern,
        PatternLit,
        Plicity, Prim, Proj, Qualifier, Rec, RecItem, StructLit, Subterm, Term, TopCase, TopItem,
        TopLet, TopMod, TopStruct, TopUnion, TopUse, Tuple, TupleType, UnionMatch, UseGroup,
    },
    crate::{
        Source,
        parser::{
            Parser, ParserError, catch, fail, lazy, many0, many1, memoize, pure, run_parser,
            sep_by0, sep_by1, spanned, take_eof, take_exact, take_n, take_while,
        },
    },
    num_bigint::BigUint,
    num_traits::{ToPrimitive, Zero},
    std::{iter, path::Path, rc::Rc, str::FromStr},
};

const CHARACTERS: &[char] = &['_'];

const KEYWORDS: &[&str] = &[
    "let", "match", "rec", "mod", "use", "pub", "end", "false", "true", "union", "struct",
];

fn parse_whitespace<'a>() -> Parser<'a, ()> {
    take_while(|char| char.is_whitespace())
        .and(
            catch(
                take_exact("--")
                    .and_keep(take_while(|char| char != '\n'))
                    .and_keep(lazy(parse_whitespace)),
            )
            .or(pure(())),
        )
        .map(|_| ())
}

fn parse_literal<'a>(expected: &'static str) -> Parser<'a, ()> {
    take_exact(expected).and_drop(parse_whitespace())
}

fn parse_identifier<'a>() -> Parser<'a, &'a str> {
    take_while(|char| CHARACTERS.contains(&char) || char.is_alphanumeric())
        .flat_map(|identifier| match identifier.is_empty() {
            true => fail("Expected identifier"),
            false => pure(identifier),
        })
        .and_drop(parse_whitespace())
}

fn parse_name<'a>() -> Parser<'a, Name> {
    spanned(
        catch(take_exact("/"))
            .map(|()| true)
            .or(pure(false))
            .and(parse_identifier().and(many0(|| {
                catch(take_exact("/").and_keep(parse_identifier()))
            })))
            .flat_map(|(is_abs, (first, rest))| {
                let segments = iter::once(first)
                    .chain(rest)
                    .map(str::to_string)
                    .collect::<Vec<_>>();

                match segments
                    .iter()
                    .any(|segment| KEYWORDS.contains(&segment.as_str()))
                {
                    true => fail(format!(
                        "path '{}' contains a reserved keyword",
                        segments.join("/")
                    )),
                    false => pure(Name::new(is_abs, Qualifier::from(segments))),
                }
            }),
    )
    .map(|(span, name)| name.with_span(span))
}

fn parse_qualified_name<'a>() -> Parser<'a, Name> {
    catch(parse_name().flat_map(|name| match name.is_single() {
        true => fail("expected a qualified path"),
        false => pure(name),
    }))
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
    catch(parse_keyword("Type"))
        .map(|()| Subterm::Type)
        .map(Into::into)
}

fn parse_int_value<'a>() -> Parser<'a, Term> {
    catch(
        take_n(1)
            .flat_map(|sign| match sign {
                "+" | "-" => pure(sign.to_string()),
                _ => fail("Expected '+' or '-'"),
            })
            .and(take_while(|char| char.is_ascii_digit()))
            .flat_map::<i32, _>(|(sign, digits)| match format!("{sign}{digits}").parse() {
                Ok(value) => pure(value),
                Err(_) => fail("Expected integer literal"),
            })
            .and_drop(parse_whitespace()),
    )
    .map(|value| Subterm::Prim(Prim::Int(value)))
    .map(Into::into)
}

fn parse_usize<'a>() -> Parser<'a, usize> {
    take_while(|char: char| char.is_ascii_digit())
        .flat_map(|digits| match digits.parse::<usize>() {
            Ok(value) => pure(value),
            Err(_) => fail("expected usize"),
        })
        .and_drop(parse_whitespace())
}

fn parse_nat_digits<'a>() -> Parser<'a, BigUint> {
    take_while(|char: char| char.is_ascii_digit())
        .flat_map(|digits| match digits.parse::<BigUint>() {
            Ok(value) => pure(value),
            Err(_) => fail("expected nat"),
        })
        .and_drop(parse_whitespace())
}

fn parse_nat<'a>() -> Parser<'a, NatLiteral> {
    catch(
        take_exact("'")
            .and_keep(parse_char_value())
            .and_drop(take_exact("'"))
            .and_drop(parse_whitespace()),
    )
    .map(NatLiteral::Char)
    .or(catch(parse_nat_digits()).map(NatLiteral::number))
}

fn parse_nat_value<'a>() -> Parser<'a, Term> {
    parse_nat()
        .map(|nat| match nat {
            NatLiteral::Number(n) if n.is_zero() => Subterm::Prim(Prim::Nat(Nat::Zero)),
            nat => Subterm::Prim(Prim::Nat(Nat::Succ(
                nat,
                Subterm::Prim(Prim::Nat(Nat::Zero)).into(),
            ))),
        })
        .map(Into::into)
}

fn parse_nat_literal<'a>() -> Parser<'a, NatLiteral> {
    catch(
        take_exact("'")
            .and_keep(parse_char_value())
            .and_drop(take_exact("'"))
            .and_drop(parse_whitespace()),
    )
    .map(NatLiteral::Char)
    .or(catch(parse_nat_digits()).map(NatLiteral::number))
}

fn parse_nat_literal_u32<'a>() -> Parser<'a, u32> {
    parse_nat_literal().flat_map(|lit| {
        let n = match lit {
            NatLiteral::Number(n) => n,
            NatLiteral::Char(c) => BigUint::from(c as u32),
        };
        match n.to_u32() {
            Some(k) => pure(k),
            None => fail("nat literal too large for u32"),
        }
    })
}

fn parse_flt_value<'a>() -> Parser<'a, Term> {
    catch(
        take_n(1)
            .flat_map(|sign| match sign {
                "+" | "-" => pure(sign.to_string()),
                _ => fail("Expected '+' or '-'"),
            })
            .and(take_while(|char| {
                ".-+eE".contains(char) || char.is_ascii_digit()
            }))
            .flat_map::<f32, _>(|(sign, digits)| {
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

                match format!("{sign}{digits}").parse() {
                    Ok(value) => pure(value),
                    Err(_) => fail("Expected float literal"),
                }
            })
            .and_drop(parse_whitespace()),
    )
    .map(|value| Subterm::Prim(Prim::Flt(value)))
    .map(Into::into)
}

fn parse_hex_byte<'a>() -> Parser<'a, u8> {
    take_exact("\\").and_keep(take_while(|char| char.is_ascii_hexdigit()).flat_map(|hex| {
        match hex.len() {
            2 => pure(u8::from_str_radix(hex, 16).expect("valid hex pair")),
            _ => fail("Expected exactly 2 hex digits after \\"),
        }
    }))
}

fn parse_string_chunk<'a>() -> Parser<'a, String> {
    catch(
        take_while(|char| char != '\\' && char != '"').flat_map(|chunk| match chunk.is_empty() {
            true => fail("empty chunk"),
            false => pure(chunk.to_string()),
        }),
    )
    .or(catch(take_exact("\\")).and_keep(
        take_exact("n")
            .map(|_| "\n".to_string())
            .or(take_exact("t").map(|()| "\t".to_string()))
            .or(take_exact("r").map(|()| "\r".to_string()))
            .or(take_exact("\\").map(|()| "\\".to_string()))
            .or(take_exact("\"").map(|()| "\"".to_string()))
            .or(fail("Unknown string escape sequence")),
    ))
}

fn parse_char_value<'a>() -> Parser<'a, char> {
    catch(take_exact("\\"))
        .and_keep(
            take_exact("n")
                .map(|_| '\n')
                .or(take_exact("t").map(|_| '\t'))
                .or(take_exact("r").map(|_| '\r'))
                .or(take_exact("\\").map(|_| '\\'))
                .or(take_exact("'").map(|_| '\''))
                .or(fail("Unknown char escape sequence")),
        )
        .or(
            take_n(1).flat_map(|string| match string.chars().next().unwrap() {
                '\'' => fail("use \\' to include a single quote in a char literal"),
                char => pure(char),
            }),
        )
}

fn parse_string_literal<'a>() -> Parser<'a, Term> {
    catch(take_exact("\""))
        .and_keep(many0(parse_string_chunk))
        .and_drop(take_exact("\""))
        .and_drop(parse_whitespace())
        .map(|chunks| Subterm::Prim(Prim::Str(chunks.concat())))
        .map(Into::into)
}

fn parse_bin_literal<'a>() -> Parser<'a, Term> {
    catch(take_exact("\\\\"))
        .map(|()| Vec::<u8>::new())
        .or(catch(many1(parse_hex_byte)))
        .and_drop(parse_whitespace())
        .map(|bytes| Subterm::Prim(Prim::Bin(bytes)))
        .map(Into::into)
}

fn parse_arr_literal<'a>() -> Parser<'a, Term> {
    catch(parse_literal("["))
        .and_keep(sep_by0(|| lazy(parse_term), || parse_literal(",")))
        .and_drop(parse_literal("]"))
        .map(|elems| Subterm::Prim(Prim::Arr(elems.into_iter().collect())))
        .map(Into::into)
}

fn parse_bln_prim<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("false"))
        .map(|()| Subterm::Prim(Prim::Bln(false)))
        .or(catch(parse_keyword("true")).map(|()| Subterm::Prim(Prim::Bln(true))))
        .map(Into::into)
}

// Primitive types and operations are no longer surface syntax — they live in the
// `sys` module (see `prelude.rs`) and parse as ordinary names. Only genuine
// literals (and the boolean keywords) remain here.
fn parse_prim<'a>() -> Parser<'a, Term> {
    parse_bln_prim()
        .or(parse_flt_value())
        .or(parse_int_value())
        .or(parse_nat_value())
        .or(parse_string_literal())
        .or(parse_bin_literal())
        .or(parse_arr_literal())
}

fn parse_parens<'a>() -> Parser<'a, Term> {
    parse_literal("(")
        .and_keep(lazy(parse_term))
        .and_drop(parse_literal(")"))
}

fn parse_tuple_type_field<'a>() -> Parser<'a, (Option<String>, Term)> {
    catch(parse_identifier().and_drop(parse_literal(":")))
        .and(lazy(parse_term))
        .map(|(label, term): (&str, Term)| (Some(label.to_string()), term))
        .or(lazy(parse_term).map(|term| (None, term)))
}

fn parse_tuple_type<'a>() -> Parser<'a, Term> {
    catch(parse_literal("{"))
        .and_keep(sep_by0(parse_tuple_type_field, || parse_literal(",")))
        .and_drop(parse_literal("}"))
        .map(|fields| {
            Subterm::TupleType(TupleType {
                fields: fields.into_iter().collect(),
            })
        })
        .map(Into::into)
}

fn parse_tuple_field<'a>() -> Parser<'a, (Option<String>, Term)> {
    catch(parse_identifier().and_drop(parse_literal("=")))
        .and(lazy(parse_term))
        .map(|(name, term): (&str, Term)| (Some(name.to_string()), term))
        .or(lazy(parse_term).map(|term| (None, term)))
}

fn parse_tuple<'a>() -> Parser<'a, Term> {
    // Two committing prefixes distinguish a tuple literal from a parenthesized
    // term: a first field followed by a comma (`(x,` / `(a = 1,`), or a named
    // first field alone (`(a = 1)` — the `=` already disambiguates, so the
    // one-element form needs no trailing comma).
    catch(
        parse_literal("(")
            .and_keep(parse_tuple_field())
            .and_drop(parse_literal(",")),
    )
    .and(sep_by0(parse_tuple_field, || parse_literal(",")))
    .map(|(first, rest)| iter::once(first).chain(rest).collect::<Vec<_>>())
    .or(
        catch(parse_literal("(").and_keep(parse_identifier().and_drop(parse_literal("="))))
            .and(lazy(parse_term))
            .map(|(name, term): (&str, Term)| vec![(Some(name.to_string()), term)]),
    )
    .and_drop(parse_literal(")"))
    .map(|fields| Subterm::Tuple(Tuple { fields }))
    .map(Into::into)
}

// A struct literal: `Name { … }` or `Name(args) { … }`. The trailing `{` inside
// the `catch` is the commit point — it distinguishes the literal from a bare
// name / name-application (no brace) and from a Σ-type `{ x : A }` (no head
// name), so there is no grammar conflict. Fields reuse the tuple-value parser
// (`= value` or positional); the head's arguments are plain terms (`@`-pinning
// is not the struct idiom — the head type pins instead).
fn parse_struct_lit<'a>() -> Parser<'a, Term> {
    catch(
        parse_name()
            .and(
                catch(
                    parse_literal("(")
                        .and_keep(sep_by0(|| lazy(parse_term), || parse_literal(",")))
                        .and_drop(parse_literal(")")),
                )
                .or(pure(vec![])),
            )
            .and_drop(parse_literal("{")),
    )
    .and(sep_by0(parse_tuple_field, || parse_literal(",")))
    .and_drop(parse_literal("}"))
    .map(|((head, params), fields)| {
        Subterm::StructLit(StructLit {
            head,
            params,
            fields,
        })
        .into()
    })
}

// A leading `@` marks a binder (or call-site argument) implicit.
fn parse_plicity<'a>() -> Parser<'a, Plicity> {
    catch(parse_literal("@"))
        .map(|()| Plicity::Implicit)
        .or(pure(Plicity::Explicit))
}

fn parse_func_type_param<'a>() -> Parser<'a, (Plicity, Option<String>, Term)> {
    parse_plicity()
        .and(
            catch(parse_identifier().and_drop(parse_literal(":")))
                .and(lazy(parse_term))
                .map(|(label, ty): (&str, Term)| (Some(label.to_string()), ty))
                .or(lazy(parse_term).map(|ty| (None, ty))),
        )
        .map(|(plicity, (label, ty))| (plicity, label, ty))
}

fn parse_func_type<'a>() -> Parser<'a, Term> {
    catch(
        parse_literal("(")
            .and_keep(sep_by0(parse_func_type_param, || parse_literal(",")))
            .and_drop(parse_literal(")"))
            .and_drop(parse_literal("->")),
    )
    .and(lazy(parse_term))
    .map(
        |(params, output): (Vec<(Plicity, Option<String>, Term)>, Term)| {
            Subterm::FuncType(FuncType {
                params: params.into_iter().collect(),
                output,
            })
        },
    )
    .map(Into::into)
}

// A lambda parameter with an optional domain annotation. `(x)` is sugar for
// `(x : _)`; the annotation, when present, parses as an arbitrary term and stops
// at the closing `)` (mirrors `parse_func_type_param`).
// A binder pattern: a plain identifier, or a parenthesized positional tuple
// pattern. A leading `(` always commits to a tuple pattern — patterns never need
// grouping, so there is no `(p)`-as-grouping ambiguity.
// A positional tuple pattern `(p, …)`, its fields drawn from `elem` — the
// irrefutable (`parse_pattern`) or refutable (`parse_match_pattern`) sub-grammar.
fn tuple_pattern<'a>(elem: fn() -> Parser<'a, Pattern>) -> Parser<'a, Pattern> {
    catch(parse_literal("("))
        .and_keep(sep_by0(move || lazy(elem), || parse_literal(",")))
        .and_drop(parse_literal(")"))
        .map(Pattern::Tuple)
}

// One field of a struct pattern: a rename `bar = p` (binding the nested pattern
// `p`, drawn from `elem`), or a pun `bar` (binding the field's own label). The
// `=` commits to the rename, so a bare label falls through to the pun.
fn struct_field_pattern<'a>(
    elem: fn() -> Parser<'a, Pattern>,
) -> Parser<'a, (String, Pattern)> {
    catch(parse_identifier().and_drop(parse_literal("=")))
        .and(lazy(elem))
        .map(|(label, pattern): (&str, Pattern)| (label.to_string(), pattern))
        .or(parse_identifier()
            .map(|label: &str| (label.to_string(), Pattern::Bind(label.to_string()))))
}

// A nominal struct pattern `Name { field, … }`, its fields drawn from `elem`. The
// trailing `{` after the head is the commit point — it distinguishes the pattern
// from a bare `Bind` (no brace), exactly as `parse_struct_lit` distinguishes a
// literal from a name reference. Parameters are never written here; they are
// inferred from the head.
fn struct_pattern<'a>(elem: fn() -> Parser<'a, Pattern>) -> Parser<'a, Pattern> {
    catch(parse_name().and_drop(parse_literal("{")))
        .and(sep_by0(move || struct_field_pattern(elem), || parse_literal(",")))
        .and_drop(parse_literal("}"))
        .map(|(head, fields)| Pattern::Struct { head, fields })
}

// The irrefutable tuple/struct entry points (also used at `let`-binder sites),
// binding their fields with the irrefutable grammar.
fn parse_tuple_pattern<'a>() -> Parser<'a, Pattern> {
    tuple_pattern(parse_pattern)
}

fn parse_struct_pattern<'a>() -> Parser<'a, Pattern> {
    struct_pattern(parse_pattern)
}

fn parse_pattern<'a>() -> Parser<'a, Pattern> {
    parse_tuple_pattern()
        .or(parse_struct_pattern())
        .or(parse_identifier().map(|name: &str| Pattern::Bind(name.to_string())))
}

// The *refutable* pattern grammar, used only in match-arm rows. It extends the
// irrefutable grammar with constructor patterns (`tag(p, …)`) and scalar literal
// patterns (`0`, `true`), and recurses into itself through tuples and structs so
// a literal/constructor may sit at any depth (`cons((0, x), _)`) — hence the
// tuple/struct parsers recurse into `parse_match_pattern`, not `parse_pattern`.
fn parse_match_pattern<'a>() -> Parser<'a, Pattern> {
    tuple_pattern(parse_match_pattern)
        .or(struct_pattern(parse_match_pattern))
        .or(parse_variant_pattern())
        .or(parse_lit_pattern())
        .or(parse_identifier().map(|name: &str| Pattern::Bind(name.to_string())))
}

// A constructor pattern `tag(p, …)` — `nil()` for the nullary case. The `(`
// immediately after the tag is the commit point, distinguishing it from a bare
// `Bind` (which has no paren), exactly as `parse_struct_pattern` commits on `{`.
fn parse_variant_pattern<'a>() -> Parser<'a, Pattern> {
    catch(parse_identifier().and_drop(parse_literal("(")))
        .and(sep_by0(|| lazy(parse_match_pattern), || parse_literal(",")))
        .and_drop(parse_literal(")"))
        .map(|(tag, args): (&str, Vec<Pattern>)| Pattern::Variant {
            tag: tag.to_string(),
            args,
        })
}

// A scalar literal pattern: `true`/`false` (Bln) or a Nat literal (`0`, `'c'`).
// `Int`/`Flt`/`Str` have no dispatch node, so they are not pattern-matchable.
fn parse_lit_pattern<'a>() -> Parser<'a, Pattern> {
    catch(parse_keyword("true"))
        .map(|()| Pattern::Lit(PatternLit::Bln(true)))
        .or(catch(parse_keyword("false")).map(|()| Pattern::Lit(PatternLit::Bln(false))))
        .or(catch(parse_nat_literal_u32()).map(|n| Pattern::Lit(PatternLit::Nat(n))))
}

fn parse_func_param<'a>() -> Parser<'a, (Pattern, Option<Term>)> {
    parse_pattern().and(
        catch(parse_literal(":").and_keep(lazy(parse_term)))
            .map(Some)
            .or(pure(None)),
    )
}

fn parse_func<'a>() -> Parser<'a, Term> {
    catch(
        parse_literal("(")
            .and_keep(sep_by0(parse_func_param, || parse_literal(",")))
            .and_drop(parse_literal(")"))
            .and_drop(parse_literal("=>")),
    )
    .and(lazy(parse_term))
    .map(|(params, body)| Subterm::Func(Func { params, body }).into())
}

// The motive ladder, binder parenthesized in every form (the bare-label
// `x => P` form is retired): `(x) => P`, `(x : Vec(T, k)) => P`, or a
// constant term. The catches span through `=>` so a constant motive that
// merely *starts* with a paren (a tuple type, a Π type) backtracks cleanly.
fn parse_motive<'a>() -> Parser<'a, Motive> {
    catch(
        parse_literal("(")
            .and_keep(parse_identifier())
            .and_drop(parse_literal(")"))
            .and_drop(parse_literal("=>")),
    )
    .and(lazy(parse_term))
    .map(|(label, body): (&str, Term)| Motive::Scrutinee {
        label: label.to_string(),
        body,
    })
    .or(catch(
        parse_literal("(")
            .and_keep(parse_identifier())
            .and_drop(parse_literal(":"))
            .and(parse_name())
            .and(
                catch(
                    parse_literal("(")
                        .and_keep(sep_by0(|| lazy(parse_term), || parse_literal(",")))
                        .and_drop(parse_literal(")")),
                )
                .or(pure(vec![])),
            )
            .and_drop(parse_literal(")"))
            .and_drop(parse_literal("=>")),
    )
    .and(lazy(parse_term))
    .map(
        |(((label, name), slots), body): (((&str, Name), Vec<Term>), Term)| Motive::Annotated {
            label: label.to_string(),
            name,
            slots,
            body,
        },
    ))
    .or(lazy(parse_term).map(Motive::Constant))
}

fn parse_match_prefix<'a>() -> Parser<'a, (Term, Option<Motive>)> {
    catch(parse_keyword("match"))
        .and_keep(lazy(parse_term))
        .and(
            catch(parse_literal(":").and_keep(parse_motive()))
                .map(Some)
                .or(pure(None)),
        )
}

fn parse_bln_false_branch<'a>() -> Parser<'a, Term> {
    parse_literal("|")
        .and_keep(parse_keyword("false"))
        .and_drop(parse_literal("=>"))
        .and_keep(lazy(parse_term))
}

fn parse_bln_true_branch<'a>() -> Parser<'a, Term> {
    parse_literal("|")
        .and_keep(parse_keyword("true"))
        .and_drop(parse_literal("=>"))
        .and_keep(lazy(parse_term))
}

fn parse_bln_match<'a>() -> Parser<'a, Term> {
    catch(parse_match_prefix())
        .and(
            catch(parse_bln_false_branch())
                .and(parse_bln_true_branch())
                .or(parse_bln_true_branch()
                    .and(parse_bln_false_branch())
                    .map(|(tc, fc)| (fc, tc))),
        )
        .and_drop(parse_keyword("end"))
        .map(|((head, motive), (false_case, true_case))| {
            Subterm::Match(Match::Bln(BlnMatch {
                head,
                motive,
                false_case,
                true_case,
            }))
        })
        .map(Into::into)
}

fn parse_nat_match<'a>() -> Parser<'a, Term> {
    catch(parse_match_prefix())
        .and(
            catch(
                parse_literal("|")
                    .and_keep(parse_nat_literal())
                    .flat_map(|lit| match lit {
                        NatLiteral::Number(n) if n.is_zero() => pure(()),
                        _ => fail("expected 0 as NatFold zero case"),
                    })
                    .and_drop(parse_literal("=>"))
                    .and_keep(lazy(parse_term)),
            )
            .and(
                parse_literal("|")
                    .and_keep(parse_identifier())
                    .and_drop(parse_literal("+"))
                    .and_drop(parse_literal("1"))
                    .and_drop(parse_literal(","))
                    .and(parse_identifier())
                    .and_drop(parse_literal("=>"))
                    .and(lazy(parse_term)),
            ),
        )
        .and_drop(parse_keyword("end"))
        .map(
            |((head, motive), (zero_case, ((pred_label, ih_label), succ_case)))| {
                Subterm::Match(Match::Nat(NatMatch::Induction {
                    head,
                    motive,
                    zero_case,
                    pred_label: pred_label.to_string(),
                    ih_label: ih_label.to_string(),
                    succ_case,
                }))
            },
        )
        .map(Into::into)
}

fn parse_nat_case<'a>() -> Parser<'a, (u32, Term)> {
    catch(parse_literal("|").and_keep(parse_nat_literal_u32()))
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
}

fn parse_nat_default<'a>() -> Parser<'a, Term> {
    catch(parse_literal("|").and_keep(parse_literal("_")))
        .and_drop(parse_literal("=>"))
        .and_keep(lazy(parse_term))
}

fn parse_nat_switch<'a>() -> Parser<'a, Term> {
    catch(parse_match_prefix())
        .and(catch(many0(parse_nat_case).and(parse_nat_default())))
        .and_drop(parse_keyword("end"))
        .map(|((head, motive), (cases, default))| {
            Subterm::Match(Match::Nat(NatMatch::Dispatch {
                head,
                motive,
                cases: cases.into_iter().collect(),
                default,
            }))
            .into()
        })
}

// A match-arm row: `| <pattern> => body`, where `<pattern>` is a full refutable
// pattern. The old `| tag(p…) => body` shape is the special case where the
// pattern is a [`Pattern::Variant`]; a bare `| x =>` / `| _ =>` is now a
// catch-all binder row, and rows may nest and repeat constructors.
fn parse_union_match_branch<'a>() -> Parser<'a, (Pattern, Term)> {
    catch(parse_literal("|"))
        .and_keep(parse_match_pattern())
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
}

// Zero arms are legal: under inversion (Rung C) every impossible arm is
// silently omittable, and a scrutinee whose indices clash with *every*
// constructor's target eliminates with no arms at all.
fn parse_union_match<'a>() -> Parser<'a, Term> {
    catch(parse_match_prefix())
        .and(many0(parse_union_match_branch))
        .and_drop(parse_keyword("end"))
        .map(|((head, motive), rows)| {
            Subterm::Match(Match::Union(UnionMatch { head, motive, rows }))
        })
        .map(Into::into)
}

// The `| [] =>` identity arm of an `Arr` fold.
fn parse_arr_empty_branch<'a>() -> Parser<'a, Term> {
    parse_literal("|")
        .and_drop(parse_literal("["))
        .and_drop(parse_literal("]"))
        .and_drop(parse_literal("=>"))
        .and_keep(lazy(parse_term))
}

// The `| (head, tail), ih =>` cons arm of an `Arr` fold.
fn parse_arr_cons_branch<'a>() -> Parser<'a, ((String, String, String), Term)> {
    parse_literal("|")
        .and_drop(parse_literal("("))
        .and_keep(parse_identifier())
        .and_drop(parse_literal(","))
        .and(parse_identifier())
        .and_drop(parse_literal(")"))
        .and_drop(parse_literal(","))
        .and(parse_identifier())
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
        .map(|(((head, tail), ih), cons_case)| {
            (
                (head.to_string(), tail.to_string(), ih.to_string()),
                cons_case,
            )
        })
}

fn parse_arr_match<'a>() -> Parser<'a, Term> {
    catch(parse_match_prefix())
        .and(catch(parse_arr_empty_branch()).and(parse_arr_cons_branch()))
        .and_drop(parse_keyword("end"))
        .map(
            |((head, motive), (empty_case, ((head_label, tail_label, ih_label), cons_case)))| {
                Subterm::Match(Match::Arr(ArrMatch {
                    head,
                    motive,
                    empty_case,
                    head_label,
                    tail_label,
                    ih_label,
                    cons_case,
                }))
            },
        )
        .map(Into::into)
}

fn parse_match<'a>() -> Parser<'a, Term> {
    catch(parse_bln_match())
        .or(catch(parse_nat_match()))
        .or(catch(parse_nat_switch()))
        .or(catch(parse_arr_match()))
        .or(parse_union_match())
}

fn parse_binding<'a>() -> Parser<'a, RecItem> {
    parse_identifier()
        .and(parse_let_signature())
        .map(|(label, signature)| RecItem {
            label: label.to_string(),
            signature,
        })
}

fn parse_rec<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("rec"))
        .and_keep(sep_by1(parse_binding, || parse_keyword("and")))
        .and_drop(parse_literal(";"))
        .and(lazy(parse_term))
        .map(|(items, tail)| Subterm::Rec(Rec { items, tail }))
        .map(Into::into)
}

fn parse_func_sugar_param<'a>() -> Parser<'a, (Plicity, Pattern, Term)> {
    parse_plicity()
        .and(parse_pattern())
        .and_drop(parse_literal(":"))
        .and(lazy(parse_term))
        .map(|((plicity, pattern), ty): ((Plicity, Pattern), Term)| (plicity, pattern, ty))
}

// The function-definition sugar `(p : T, ...) -> R = body`. Shared by both the
// type-required and the local (type-optional) signature parsers.
fn parse_func_let_signature<'a>() -> Parser<'a, LetSignature> {
    catch(
        parse_literal("(")
            .and_keep(sep_by0(parse_func_sugar_param, || parse_literal(",")))
            .and_drop(parse_literal(")"))
            .and_drop(parse_literal("->")),
    )
    .and(lazy(parse_term))
    .and_drop(parse_literal("="))
    .and(lazy(parse_term))
    .map(|((params, output), body)| LetSignature::Func {
        params,
        output,
        body,
    })
}

// The plain `: T = body` form with a mandatory type.
fn parse_required_name_signature<'a>() -> Parser<'a, LetSignature> {
    catch(parse_literal(":"))
        .and_keep(lazy(parse_term))
        .and_drop(parse_literal("="))
        .and(lazy(parse_term))
        .map(|(type_, body)| LetSignature::Name {
            type_: Some(type_),
            body,
        })
}

// The plain `(: T)? = body` form: the type may be omitted (inferred from `body`).
fn parse_optional_name_signature<'a>() -> Parser<'a, LetSignature> {
    catch(parse_literal(":").and_keep(lazy(parse_term)))
        .map(Some)
        .or(pure(None))
        .and_drop(parse_literal("="))
        .and(lazy(parse_term))
        .map(|(type_, body)| LetSignature::Name { type_, body })
}

// Parses the part of a `let`/`rec` binding after its name where a type is
// **required**: the function sugar, or the `: T = body` form. Used for top-level
// `let` and every `rec` binding, whose types cannot be inferred.
fn parse_let_signature<'a>() -> Parser<'a, LetSignature> {
    parse_func_let_signature().or(parse_required_name_signature())
}

// Like `parse_let_signature`, but the plain form's type annotation may be
// omitted. Used only by local `let`, where the body's type can be inferred.
fn parse_local_let_signature<'a>() -> Parser<'a, LetSignature> {
    parse_func_let_signature().or(parse_optional_name_signature())
}

// `let (a, b) = e; tail` / `let (a, b) : T = e; tail`. A `(` right after `let`
// (an identifier can never start there) commits to the destructuring form, so
// `.or(parse_let_name)` only backtracks when there is no tuple pattern. The
// binder is a pattern but the signature is always the plain `(: T)? = value`
// form — a tuple pattern never carries the function-definition sugar.
fn parse_let_tuple<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("let").and_keep(parse_tuple_pattern()))
        .and(parse_optional_name_signature())
        .and_drop(parse_literal(";"))
        .and(lazy(parse_term))
        .map(|((binder, signature), tail)| {
            Subterm::Let(Let {
                binder,
                signature,
                tail,
            })
        })
        .map(Into::into)
}

// `let Foo { bar, baz } = e; tail`. A struct pattern right after `let` commits
// to the destructuring form (the head's trailing `{`), so `.or(parse_let_name)`
// only backtracks when there is no struct pattern. Like the tuple form, the
// signature is always the plain `(: T)? = value` shape — the head already pins
// the struct type, so an annotation is optional.
fn parse_let_struct<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("let").and_keep(parse_struct_pattern()))
        .and(parse_optional_name_signature())
        .and_drop(parse_literal(";"))
        .and(lazy(parse_term))
        .map(|((binder, signature), tail)| {
            Subterm::Let(Let {
                binder,
                signature,
                tail,
            })
        })
        .map(Into::into)
}

fn parse_let_name<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("let"))
        .and_keep(parse_identifier())
        .and(parse_local_let_signature())
        .and_drop(parse_literal(";"))
        .and(lazy(parse_term))
        .map(|((label, signature), tail)| {
            Subterm::Let(Let {
                binder: Pattern::Bind(label.to_string()),
                signature,
                tail,
            })
        })
        .map(Into::into)
}

fn parse_let<'a>() -> Parser<'a, Term> {
    parse_let_tuple()
        .or(parse_let_struct())
        .or(parse_let_name())
}

fn parse_proj_suffix<'a>() -> Parser<'a, Field> {
    catch(
        take_exact(".").and_keep(
            parse_usize()
                .map(Field::Index)
                .or(parse_identifier().map(|label| Field::Label(label.to_string())))
                .map_err("Expected field index or label after '.'"),
        ),
    )
}

enum Suffix {
    Proj(Field),
    Apply(Vec<(Plicity, Term)>),
    Bang,
}

fn parse_apply_argument<'a>() -> Parser<'a, (Plicity, Term)> {
    parse_plicity().and(lazy(parse_term))
}

fn parse_suffix<'a>() -> Parser<'a, Suffix> {
    parse_proj_suffix()
        .map(Suffix::Proj)
        .or(catch(parse_literal("("))
            .and_keep(sep_by0(parse_apply_argument, || parse_literal(",")))
            .and_drop(parse_literal(")"))
            .map(Suffix::Apply))
        .or(catch(parse_literal("!")).map(|()| Suffix::Bang))
}

fn parse_empty_tuple<'a>() -> Parser<'a, Term> {
    catch(parse_literal("(").and_keep(parse_literal(")")))
        .map(|_| Subterm::Tuple(Tuple { fields: vec![] }))
        .map(Into::into)
}

fn with_span<'a>(parser: Parser<'a, Term>) -> Parser<'a, Term> {
    spanned(parser).map(|(span, term)| term.with_span(span))
}

fn parse_hole<'a>() -> Parser<'a, Term> {
    // `?` is not an identifier character, so a plain literal suffices — no
    // token-aware matching needed. (`_` remains the match wildcard binder.)
    catch(parse_literal("?")).map(|()| Subterm::Hole.into())
}

// Grammar keys for the packrat cache (see `parser::memoize`). Only the two
// nonterminals that overlapping alternatives re-probe at the same offset are
// memoized; that is enough to keep parsing linear.
const MEMO_TERM: u32 = 0;
const MEMO_ATOMIC_TERM: u32 = 1;

fn parse_atomic_term<'a>() -> Parser<'a, Term> {
    memoize(MEMO_ATOMIC_TERM, parse_atomic_term_inner())
}

fn parse_atomic_term_inner<'a>() -> Parser<'a, Term> {
    with_span(
        parse_hole()
            .or(parse_struct_lit())
            .or(parse_qualified_name().map(|n| Subterm::Name(n).into()))
            .or(parse_type())
            .or(parse_prim())
            .or(parse_tuple_type())
            .or(parse_empty_tuple())
            .or(parse_tuple())
            .or(parse_parens())
            .or(parse_name().map(|n| Subterm::Name(n).into()))
            .and(many0(parse_suffix))
            .map(|(head, suffixes): (Term, _)| {
                suffixes
                    .into_iter()
                    .fold(head, |head, suffix| match suffix {
                        Suffix::Proj(field) => Subterm::Proj(Proj { head, field }).into(),
                        Suffix::Apply(params) => Subterm::Apply(Apply { head, params }).into(),
                        Suffix::Bang => Subterm::Bang(head).into(),
                    })
            }),
    )
}

// A `let ! = <bind>; <body>` do-notation block. `<bind>` is an *atomic* term
// denoting a binary bind `(M A, A -> M B) -> M B` (e.g. the partially-applied
// `Parse/bind(?, ?)`); it is re-elaborated per `!` site, so its `?` holes get fresh
// metavariables each time, and the desugarer applies it to `(action, continuation)`
// there — keeping the bind's own head (e.g. `Parse/bind`) in head position, so it
// needs no annotation. No `end`: the block ends where the enclosing term ends (like
// a `let`/`rec` tail). The leading `let !` is committed once matched, so a plain
// `let x` backtracks cleanly.
fn parse_let_bang<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("let").and_keep(parse_literal("!")))
        .and_keep(parse_literal("="))
        .and_keep(parse_atomic_term())
        .and_drop(parse_literal(";"))
        .and(lazy(parse_term))
        .map(|(bind, body)| Subterm::LetBang(LetBang { bind, body }))
        .map(Into::into)
}

fn parse_term<'a>() -> Parser<'a, Term> {
    memoize(MEMO_TERM, parse_term_inner())
}

fn parse_term_inner<'a>() -> Parser<'a, Term> {
    with_span(
        parse_let_bang()
            .or(parse_rec())
            .or(parse_let())
            .or(parse_match())
            .or(parse_func_type())
            .or(parse_func())
            .or(parse_atomic_term()),
    )
}

impl Term {
    fn parse(source: &Rc<Source>) -> Result<Self, ParserError> {
        run_parser(
            parse_whitespace()
                .and_keep(parse_term())
                .and_drop(take_eof()),
            source,
        )
    }
}

impl FromStr for Term {
    type Err = ParserError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Term::parse(&Source::inline(input))
    }
}

fn parse_pub<'a>() -> Parser<'a, bool> {
    catch(parse_keyword("pub")).map(|()| true).or(pure(false))
}

fn parse_top_let<'a>() -> Parser<'a, TopItem> {
    catch(parse_pub().and(parse_keyword("let"))).flat_map(|(is_pub, ())| {
        parse_identifier()
            .and(parse_let_signature())
            .and_drop(parse_literal(";"))
            .map(move |(label, signature)| {
                TopItem::Let(TopLet {
                    is_pub,
                    label: label.to_string(),
                    signature,
                })
            })
    })
}

fn parse_top_rec<'a>() -> Parser<'a, TopItem> {
    catch(parse_pub().and(parse_keyword("rec")))
        .flat_map(|(is_pub, ())| {
            parse_binding().map(move |item| TopLet {
                is_pub,
                label: item.label,
                signature: item.signature,
            })
        })
        .and(many0(|| {
            catch(parse_pub().and(parse_keyword("and"))).flat_map(|(is_pub, ())| {
                parse_binding().map(move |item| TopLet {
                    is_pub,
                    label: item.label,
                    signature: item.signature,
                })
            })
        }))
        .and_drop(parse_literal(";"))
        .map(|(first, rest)| iter::once(first).chain(rest).collect())
        .map(TopItem::Rec)
}

fn parse_top_mod<'a>() -> Parser<'a, TopItem> {
    spanned(
        catch(parse_pub().and(parse_keyword("mod"))).flat_map(|(is_pub, ())| {
            parse_identifier().flat_map(move |name| {
                catch(
                    many0(parse_top_item)
                        .and_drop(parse_keyword("end"))
                        .map(|items| Some(Module { items })),
                )
                .or(parse_literal(";").map(|()| None))
                .map(move |module| (is_pub, name.to_string(), module))
            })
        }),
    )
    .map(|(span, (is_pub, label, module))| {
        TopItem::Mod(TopMod {
            span: Some(span),
            is_pub,
            label,
            module,
        })
    })
}

// Like `parse_name`, but additionally accepts an empty absolute path. The
// leading `/` is only consumed when followed by an identifier — so for
// `use /{X};` the path is empty-abs (consumes nothing) and `/` is left for
// `parse_use_group` to consume as its separator.
fn parse_use_path<'a>() -> Parser<'a, Name> {
    spanned(
        catch(take_exact("/").and_keep(parse_identifier()).and(many0(|| {
            catch(take_exact("/").and_keep(parse_identifier()))
        })))
        .map(|(first, rest)| {
            Name::new(
                true,
                Qualifier::from(
                    iter::once(first)
                        .chain(rest)
                        .map(str::to_string)
                        .collect::<Vec<_>>(),
                ),
            )
        })
        .or(catch(parse_identifier().and(many0(|| {
            catch(take_exact("/").and_keep(parse_identifier()))
        })))
        .map(|(first, rest)| {
            Name::new(
                false,
                Qualifier::from(
                    iter::once(first)
                        .chain(rest)
                        .map(str::to_string)
                        .collect::<Vec<_>>(),
                ),
            )
        }))
        .or(pure(Name::new(true, Qualifier::empty())))
        .flat_map(|name| {
            match name
                .qualifier()
                .segments()
                .iter()
                .any(|segment| KEYWORDS.contains(&segment.as_str()))
            {
                true => fail(format!(
                    "path '{}' contains a reserved keyword",
                    name.qualifier().join()
                )),
                false => pure(name),
            }
        }),
    )
    .map(|(span, name)| name.with_span(span))
}

fn parse_group_item<'a>() -> Parser<'a, GroupItem> {
    catch(parse_keyword("mod").and_keep(parse_identifier()))
        .map(|s| GroupItem::Mod(s.to_string()))
        .or(catch(parse_keyword("let").and_keep(parse_identifier()))
            .map(|s| GroupItem::Let(s.to_string())))
        .or(parse_identifier().map(|s| GroupItem::Both(s.to_string())))
}

fn parse_brace_group<'a>() -> Parser<'a, Vec<GroupItem>> {
    catch(parse_literal("{"))
        .and_keep(sep_by0(parse_group_item, || parse_literal(",")))
        .and_drop(parse_literal("}"))
}

fn parse_use_group<'a>() -> Parser<'a, UseGroup> {
    catch(take_exact("/").and_keep(parse_brace_group()))
        .map(UseGroup::Named)
        .or(catch(take_exact("/").and_keep(parse_literal("*"))).map(|()| UseGroup::Glob))
}

fn parse_top_use<'a>() -> Parser<'a, TopItem> {
    catch(parse_pub().and(parse_keyword("use"))).flat_map(|(is_pub, ())| {
        parse_use_path()
            .and(parse_use_group())
            .and_drop(parse_literal(";"))
            .map(move |(name, group)| {
                TopItem::Use(TopUse {
                    is_pub,
                    name,
                    group,
                })
            })
    })
}

// A payload binder: `@m : Nat` (named, implicit at the constructor function),
// `m : Nat` (named), or a bare type (positional). `@` requires a name — a
// positional binder has nothing for a later type or the target to mention.
fn parse_union_payload_field<'a>() -> Parser<'a, (Plicity, Option<String>, Term)> {
    catch(
        parse_plicity()
            .and(parse_identifier())
            .and_drop(parse_literal(":")),
    )
    .and(lazy(parse_term))
    .map(|((plicity, name), ty): ((Plicity, &str), Term)| (plicity, Some(name.to_string()), ty))
    .or(lazy(parse_term).map(|ty| (Plicity::Explicit, None, ty)))
}

fn parse_top_union_case<'a>() -> Parser<'a, TopCase> {
    parse_literal("|")
        .and_keep(parse_identifier())
        .and(
            parse_literal("(")
                .and_keep(sep_by0(parse_union_payload_field, || parse_literal(",")))
                .and_drop(parse_literal(")")),
        )
        // The case target: `: (index-exprs)` — the terminal with its
        // mandatory part (the union name and the parameters) elided.
        .and(
            catch(parse_literal(":"))
                .and_keep(parse_literal("("))
                .and_keep(sep_by0(|| lazy(parse_term), || parse_literal(",")))
                .and_drop(parse_literal(")"))
                .map(Some)
                .or(pure(None)),
        )
        .map(
            |((label, payload), target): ((&str, Vec<_>), Option<Vec<Term>>)| TopCase {
                label: label.to_string(),
                payload,
                target,
            },
        )
}

// A union parameter: `name : type`, or `@name : type` to make it implicit at
// the type-constructor function (it is implicit at the value constructors
// either way — the mark's only job is the type constructor, where unmarked
// parameters are written out).
fn parse_union_param<'a>() -> Parser<'a, (Plicity, String, Term)> {
    parse_plicity()
        .and(parse_identifier())
        .and_drop(parse_literal(":"))
        .and(lazy(parse_term))
        .map(|((plicity, name), ty): ((Plicity, &str), Term)| (plicity, name.to_string(), ty))
}

// A head index-telescope entry: `n : Nat` or a bare `Nat`. The name is
// documentary (and a dependency hook for later entries) — never in scope in
// the cases — so it is optional and never takes `@`.
fn parse_union_index<'a>() -> Parser<'a, (Option<String>, Term)> {
    catch(parse_identifier().and_drop(parse_literal(":")))
        .and(lazy(parse_term))
        .map(|(name, ty): (&str, Term)| (Some(name.to_string()), ty))
        .or(lazy(parse_term).map(|ty| (None, ty)))
}

fn parse_top_union_body<'a>(is_pub: bool) -> Parser<'a, TopUnion> {
    parse_identifier()
        .and(
            catch(
                parse_literal("(")
                    .and_keep(sep_by0(parse_union_param, || parse_literal(",")))
                    .and_drop(parse_literal(")")),
            )
            .or(pure(vec![])),
        )
        // The head's index telescope: `: (n : Nat)` after the parameters.
        .and(
            catch(parse_literal(":"))
                .and_keep(parse_literal("("))
                .and_keep(sep_by0(parse_union_index, || parse_literal(",")))
                .and_drop(parse_literal(")"))
                .or(pure(vec![])),
        )
        .and(many0(parse_top_union_case))
        .flat_map(move |(((label, params), indices), cases)| {
            // Targets are required on every case iff the head declares
            // indices, with arity equal to the index telescope's.
            for case in &cases {
                match (&case.target, indices.len()) {
                    (None, 0) => {}
                    (None, _) => {
                        return fail(format!(
                            "case '{}' of indexed union '{label}' must state its \
                             index target: `{}(...) : (...)`",
                            case.label, case.label,
                        ));
                    }
                    (Some(_), 0) => {
                        return fail(format!(
                            "case '{}' states an index target, but union '{label}' \
                             declares no indices",
                            case.label,
                        ));
                    }
                    (Some(target), arity) if target.len() != arity => {
                        return fail(format!(
                            "case '{}' of union '{label}' states {} index \
                             expression(s), but the head declares {arity}",
                            case.label,
                            target.len(),
                        ));
                    }
                    _ => {}
                }
            }

            let label = label.to_string();
            pure(TopUnion {
                is_pub,
                label,
                params,
                indices,
                cases,
            })
        })
}

fn parse_top_union<'a>() -> Parser<'a, TopItem> {
    catch(parse_pub().and(parse_keyword("union"))).flat_map(|(is_pub, ())| {
        parse_top_union_body(is_pub)
            .and(many0(|| {
                catch(parse_pub().and(parse_keyword("and")))
                    .flat_map(|(is_pub2, ())| parse_top_union_body(is_pub2))
            }))
            .and_drop(parse_keyword("end"))
            .map(|(first, rest)| TopItem::Union(iter::once(first).chain(rest).collect()))
    })
}

fn parse_top_struct<'a>() -> Parser<'a, TopItem> {
    catch(parse_pub().and(parse_keyword("struct"))).flat_map(|(is_pub, ())| {
        parse_identifier()
            .and(
                catch(
                    parse_literal("(")
                        .and_keep(sep_by0(parse_union_param, || parse_literal(",")))
                        .and_drop(parse_literal(")")),
                )
                .or(pure(vec![])),
            )
            // The inner `pub` (representation visibility) sits right before `{`.
            .and(parse_pub())
            .and_drop(parse_literal("{"))
            .and(sep_by0(parse_tuple_type_field, || parse_literal(",")))
            .and_drop(parse_literal("}"))
            .map(move |(((label, params), rep_pub), fields)| {
                TopItem::Struct(TopStruct {
                    is_pub,
                    rep_pub,
                    label: label.to_string(),
                    params,
                    fields,
                })
            })
    })
}

fn parse_top_item<'a>() -> Parser<'a, TopItem> {
    parse_top_mod()
        .or(parse_top_use())
        .or(parse_top_let())
        .or(parse_top_union())
        .or(parse_top_struct())
        .or(parse_top_rec())
}

impl Module {
    fn parse(source: &Rc<Source>) -> Result<Self, ParserError> {
        run_parser(
            parse_whitespace()
                .and_keep(many0(parse_top_item))
                .and_drop(take_eof())
                .map(|items| Module { items }),
            source,
        )
    }

    pub fn from_path(path: impl AsRef<Path>) -> Result<Self, LoadError> {
        let path = path.as_ref();
        let source = Source::read(path).map_err(|error| LoadError::Read {
            path: path.into(),
            error,
        })?;

        Module::parse(&source).map_err(LoadError::Parse)
    }
}

impl FromStr for Module {
    type Err = ParserError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Module::parse(&Source::inline(input))
    }
}

impl Entrypoint {
    fn parse(source: &Rc<Source>) -> Result<Self, ParserError> {
        run_parser(
            parse_whitespace()
                .and_keep(many0(parse_top_item))
                .and(lazy(parse_term))
                .and_drop(take_eof())
                .map(|(items, tail)| Entrypoint::new(items, tail)),
            source,
        )
    }

    pub fn from_path(path: impl AsRef<Path>) -> Result<Self, LoadError> {
        let path = path.as_ref();
        let source = Source::read(path).map_err(|error| LoadError::Read {
            path: path.into(),
            error,
        })?;

        Entrypoint::parse(&source).map_err(LoadError::Parse)
    }
}

impl FromStr for Entrypoint {
    type Err = ParserError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Entrypoint::parse(&Source::inline(input))
    }
}
