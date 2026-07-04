use {
    super::{
        Apply, BinMatch, BinSegment, BlnMatch, CasePayloadParam, ConceptField, ConceptParam,
        Entrypoint, Field, Func, FuncSugarParam, FuncType, FuncTypeParam, GroupItem, InductiveArm,
        InductiveMatch, Infix, Let, LetSignature, LoadError, LstEntry, LstMatch, Match, Module,
        Motive, Name, Nat, NatLiteral, NatMatch, NumLit, NumOp, Plicity, Prim, Proj, Qualifier,
        Radix, Rec, RecItem, StructLit, StructLitEntry, Subterm, Syn, Term, TopCase, TopConcept,
        TopForeign, TopInduct, TopItem, TopLet, TopMod, TopStruct, TopUse, TopWitness, Tuple,
        TupleField, TupleType, TupleTypeParam, UseGroup, WitnessEntry, WitnessField,
    },
    curios_abi::{WireSignature, WireType},
    curios_base::{
        Source,
        parser::{
            Parser, ParserError, catch, fail, lazy, many0, many1, memoize, not_ahead,
            preceded_by_space, pure, run_parser, sep_by0, sep_by0_trailing, sep_by1, spanned,
            take_eof, take_exact, take_n, take_while,
        },
    },
    num_bigint::BigUint,
    num_traits::{ToPrimitive, Zero},
    std::{iter, path::Path, rc::Rc, str::FromStr},
};

const CHARACTERS: &[char] = &['_'];

const KEYWORDS: &[&str] = &[
    "let", "match", "rec", "mod", "use", "pub", "end", "false", "true", "induct", "struct",
    "record", "foreign",
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

// The identifier characters alone, consuming no whitespace — the building
// block of the tight (whitespace-free) positions like a `Bin` literal's
// `\..` spread operand.
fn parse_identifier_raw<'a>() -> Parser<'a, &'a str> {
    take_while(|char| CHARACTERS.contains(&char) || char.is_alphanumeric()).flat_map(|identifier| {
        match identifier.is_empty() {
            true => fail("Expected identifier"),
            false => pure(identifier),
        }
    })
}

fn parse_identifier<'a>() -> Parser<'a, &'a str> {
    parse_identifier_raw().and_drop(parse_whitespace())
}

fn name_from_segments<'a>(is_abs: bool, segments: Vec<String>) -> Parser<'a, Name> {
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

                name_from_segments(is_abs, segments)
            }),
    )
    .map(|(span, name)| name.with_span(span))
}

// A strictly glued name path — no whitespace anywhere, not even trailing. The
// tight sibling of [`parse_name`] (whose segments each eat trailing
// whitespace, so `Foo /bar` is the path `Foo/bar` there), used where the
// surrounding grammar is whitespace-sensitive: a `Bin` literal's `\..` spread
// operand.
fn parse_name_raw<'a>() -> Parser<'a, Name> {
    spanned(
        catch(take_exact("/"))
            .map(|()| true)
            .or(pure(false))
            .and(parse_identifier_raw().and(many0(|| {
                catch(take_exact("/").and_keep(parse_identifier_raw()))
            })))
            .flat_map(|(is_abs, (first, rest))| {
                let segments = iter::once(first)
                    .chain(rest)
                    .map(str::to_string)
                    .collect::<Vec<_>>();

                name_from_segments(is_abs, segments)
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

fn parse_prop<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("Prop"))
        .map(|()| Subterm::Prop)
        .map(Into::into)
}

// A polymorphic integer literal: an optional sign glued to a magnitude
// (decimal, `0x`, or `0b`). Its concrete type (`Nat`/`Int`/`Flt`) is chosen by
// elaboration — a written sign rules out `Nat`. The sign must touch the digits;
// `- 42` (spaced) is the subtraction operator, not a negative literal.
fn parse_num_lit<'a>() -> Parser<'a, Term> {
    catch(
        catch(take_exact("-"))
            .map(|()| (true, true))
            .or(catch(take_exact("+")).map(|()| (true, false)))
            .or(pure((false, false)))
            .and(parse_nat_digits()),
    )
    .map(|((signed, negative), lit)| {
        let (magnitude, radix) = match lit {
            NatLiteral::Number(value, radix) => (value, radix),
            NatLiteral::Char(character) => (BigUint::from(character as u32), Radix::Dec),
        };
        Subterm::NumLit(NumLit {
            magnitude,
            radix,
            signed,
            negative,
        })
    })
    .map(Into::into)
}

// A character literal `'c'` is a fixed `Nat` codepoint — monomorphic, unlike a
// bare integer literal.
fn parse_char_lit<'a>() -> Parser<'a, Term> {
    catch(
        take_exact("'")
            .and_keep(parse_char_value())
            .and_drop(take_exact("'"))
            .and_drop(parse_whitespace()),
    )
    .map(|character| {
        Subterm::Prim(Prim::Nat(Nat::Succ(
            NatLiteral::Char(character),
            Subterm::Prim(Prim::Nat(Nat::Zero)).into(),
        )))
    })
    .map(Into::into)
}

fn parse_usize_raw<'a>() -> Parser<'a, usize> {
    take_while(|char: char| char.is_ascii_digit()).flat_map(|digits| {
        match digits.parse::<usize>() {
            Ok(value) => pure(value),
            Err(_) => fail("expected usize"),
        }
    })
}

fn parse_radix<'a>(prefix: &'static str, radix: u32, tag: Radix) -> Parser<'a, NatLiteral> {
    take_exact(prefix).and_keep(take_while(move |char: char| char.is_digit(radix)).flat_map(
        move |digits| match BigUint::parse_bytes(digits.as_bytes(), radix) {
            Some(value) => pure(NatLiteral::Number(value, tag)),
            None => fail(format!("expected base-{radix} digits after '{prefix}'")),
        },
    ))
}

fn parse_nat_digits<'a>() -> Parser<'a, NatLiteral> {
    catch(parse_radix("0x", 16, Radix::Hex))
        .or(catch(parse_radix("0b", 2, Radix::Bin)))
        .or(
            take_while(|char: char| char.is_ascii_digit()).flat_map(|digits| {
                match digits.parse::<BigUint>() {
                    Ok(value) => pure(NatLiteral::Number(value, Radix::Dec)),
                    Err(_) => fail("expected nat"),
                }
            }),
        )
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
    .or(catch(parse_nat_digits()))
}

fn parse_nat_literal_u32<'a>() -> Parser<'a, u32> {
    parse_nat().flat_map(|lit| {
        let n = match lit {
            NatLiteral::Number(n, _) => n,
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
        catch(take_exact("-"))
            .map(|()| "-".to_string())
            .or(catch(take_exact("+")).map(|()| "+".to_string()))
            .or(pure(String::new()))
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
        .map(|chunks| Subterm::Syn(Syn::Str(chunks.concat())))
        .map(Into::into)
}

// A `Bin` spread operand under the TIGHT rule: it must end without consuming
// any whitespace, so the segment loop can require the next segment to begin
// immediately with `\` (`\..xs \01` ends the literal at `xs`). The operand is
// an atomic term in glued form: a name path or a parenthesized term, followed
// by glued suffixes — projections, calls, `!` (`\..hdr.bytes`, `\..f(x)`,
// `\..read()!`). Self-delimiting parts (a call's argument list, the parens
// form) admit interior whitespace freely; only the operand's edges are tight,
// with every closing delimiter matched raw. Anything else — an infix chain, a
// lambda — takes the parenthesized form.
fn parse_bin_spread_operand<'a>() -> Parser<'a, Term> {
    with_span(
        parse_name_raw()
            .map(|name| Term::from(Subterm::Name(name)))
            .or(take_exact("(")
                .and_drop(parse_whitespace())
                .and_keep(lazy(parse_term))
                .and_drop(take_exact(")")))
            .and(many0(parse_suffix_raw))
            .map(|(head, suffixes)| apply_suffixes(head, suffixes)),
    )
}

// One segment of a `Bin` literal: a literal byte, or a `\..` spread. Adjacent
// bytes are coalesced into `BinSegment::Bytes` runs by the literal parser.
enum RawBinSegment {
    Byte(u8),
    Spread(Term),
}

fn parse_bin_segment<'a>() -> Parser<'a, RawBinSegment> {
    catch(parse_hex_byte())
        .map(RawBinSegment::Byte)
        // Committed after `\..`: an operand failure is fatal (no inner
        // `catch`), so the error points at the segment rather than at
        // whatever the surrounding grammar makes of the leftovers.
        .or(catch(take_exact("\\..")).and_keep(
            parse_bin_spread_operand()
                .map_err("Expected a glued name or parenthesized term after '\\..'")
                .map(RawBinSegment::Spread),
        ))
}

fn coalesce_bin_segments(raw: Vec<RawBinSegment>) -> Vec<BinSegment> {
    let mut segments = Vec::new();

    for segment in raw {
        match segment {
            RawBinSegment::Byte(byte) => match segments.last_mut() {
                Some(BinSegment::Bytes(run)) => run.push(byte),
                _ => segments.push(BinSegment::Bytes(vec![byte])),
            },
            RawBinSegment::Spread(term) => segments.push(BinSegment::Spread(term)),
        }
    }

    segments
}

// A `Bin` literal: `\\` (empty), or one-or-more glued segments — `\HH` bytes
// and `\..operand` spreads. One whitespace-free lexical unit: after a spread
// operand the literal continues only when the very next character is `\`.
fn parse_bin_literal<'a>() -> Parser<'a, Term> {
    catch(take_exact("\\\\"))
        .map(|()| Vec::new())
        .or(catch(many1(parse_bin_segment)).map(coalesce_bin_segments))
        .and_drop(parse_whitespace())
        .map(|segments| Subterm::Prim(Prim::Bin(segments)))
        .map(Into::into)
}

// One entry of an array literal: a `..` spread contributing a whole list, or
// a plain element. Unlike the `Bin` literal, brackets and commas delimit, so
// spreads take full terms and `[.. xs]` may be spaced (as in struct spread).
fn parse_lst_entry<'a>() -> Parser<'a, LstEntry> {
    catch(parse_literal(".."))
        .and_keep(lazy(parse_term))
        .map(LstEntry::Spread)
        .or(lazy(parse_term).map(LstEntry::Elem))
}

// An array literal `[e0, ..rest, e1, …]` (empty `[]`) — the native
// contiguous-sequence sibling of the `Bin` literal `\\`. Builds a `Prim::Lst`
// directly (the element type is an implicit the literal cannot name; core
// elaboration infers it); spreads splice in place, any position and count.
fn parse_arr_literal<'a>() -> Parser<'a, Term> {
    catch(parse_literal("["))
        .and_keep(sep_by0(parse_lst_entry, || parse_literal(",")))
        .and_drop(parse_literal("]"))
        .map(|entries| Subterm::Prim(Prim::Lst(entries)))
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
        // Decimal floats first: `5.0` is a `Flt`, not the integer `5` projected.
        .or(parse_flt_value())
        .or(parse_char_lit())
        .or(parse_num_lit())
        .or(parse_string_literal())
        .or(parse_bin_literal())
        .or(parse_arr_literal())
}

fn parse_parens<'a>() -> Parser<'a, Term> {
    parse_literal("(")
        .and_keep(lazy(parse_term))
        .and_drop(parse_literal(")"))
}

// A Σ-type / struct-declaration field: an optional label and the field type,
// or the signature sugar `label(params) -> type` — kept as written in the AST
// node (`func_params`); `to_core` undoes the sugar. Shared by tuple types and
// `struct` decls. The sugared catch spans through `->`, so a positional field
// that merely starts with an application (`f(x)`) backtracks cleanly.
fn parse_tuple_type_field<'a>() -> Parser<'a, TupleTypeParam> {
    catch(
        parse_identifier()
            .and(
                parse_literal("(")
                    .and_keep(sep_by0(parse_func_type_param, || parse_literal(",")))
                    .and_drop(parse_literal(")")),
            )
            .and_drop(parse_literal("->")),
    )
    .and(lazy(parse_term))
    .map(
        |((label, params), output): ((&str, Vec<FuncTypeParam>), Term)| TupleTypeParam {
            label: Some(label.to_string()),
            func_params: Some(params),
            type_: output,
        },
    )
    .or(catch(parse_identifier().and_drop(parse_literal(":")))
        .and(lazy(parse_term))
        .map(|(label, type_): (&str, Term)| TupleTypeParam {
            label: Some(label.to_string()),
            func_params: None,
            type_,
        }))
    .or(lazy(parse_term).map(|type_| TupleTypeParam {
        label: None,
        func_params: None,
        type_,
    }))
}

fn parse_tuple_type<'a>() -> Parser<'a, Term> {
    catch(parse_literal("{"))
        .and_keep(sep_by0_trailing(parse_tuple_type_field, || {
            parse_literal(",")
        }))
        .and_drop(parse_literal("}"))
        .map(|fields| {
            Subterm::TupleType(TupleType {
                fields: fields.into_iter().collect(),
            })
        })
        .map(Into::into)
}

// A parsed labeled-field prefix: the label and, for the definition sugar, the
// written lambda-parameter list.
type TupleFieldPrefix = (String, Option<Vec<(String, Option<Term>)>>);

// The committing prefix of a labeled tuple/struct-literal field: `label =` or
// the definition sugar `label(params) =`. The caller wraps it in `catch`, so a
// positional field that merely starts with an identifier or an application
// backtracks cleanly.
fn parse_tuple_field_prefix<'a>() -> Parser<'a, TupleFieldPrefix> {
    parse_identifier()
        .and(
            catch(
                parse_literal("(")
                    .and_keep(sep_by0(parse_func_param, || parse_literal(",")))
                    .and_drop(parse_literal(")")),
            )
            .map(Some)
            .or(pure(None)),
        )
        .and_drop(parse_literal("="))
        .map(|(label, func_params): (&str, _)| (label.to_string(), func_params))
}

// A tuple-literal / struct-literal field: `label = value`, the definition
// sugar `label(params) = value` — kept as written in the AST node
// (`func_params`); `to_core` undoes the sugar — or a positional value.
fn parse_tuple_field<'a>() -> Parser<'a, TupleField> {
    catch(parse_tuple_field_prefix())
        .and(lazy(parse_term))
        .map(|((label, func_params), value)| TupleField {
            label: Some(label),
            func_params,
            value,
        })
        .or(lazy(parse_term).map(|value| TupleField {
            label: None,
            func_params: None,
            value,
        }))
}

fn parse_tuple<'a>() -> Parser<'a, Term> {
    // Two committing prefixes distinguish a tuple literal from a parenthesized
    // term: a first field followed by a comma (`(x,` / `(a = 1,`), or a named
    // first field alone (`(a = 1)` / `(f(x) = e)` — the `=` already
    // disambiguates, so the one-element form needs no trailing comma).
    catch(
        parse_literal("(")
            .and_keep(parse_tuple_field())
            .and_drop(parse_literal(",")),
    )
    .and(sep_by0_trailing(parse_tuple_field, || parse_literal(",")))
    .map(|(first, rest)| iter::once(first).chain(rest).collect::<Vec<_>>())
    .or(
        catch(parse_literal("(").and_keep(parse_tuple_field_prefix()))
            .and(lazy(parse_term))
            .map(|((label, func_params), value)| {
                vec![TupleField {
                    label: Some(label),
                    func_params,
                    value,
                }]
            }),
    )
    .and_drop(parse_literal(")"))
    .map(|fields| Subterm::Tuple(Tuple { fields }))
    .map(Into::into)
}

// A struct-literal entry: a `..base` spread (no term begins with `..` — a
// leading-dot float has a single dot — so the prefix commits), a `use <term>`
// fill for a concept's `use`-marked field (mirroring the call-site argument
// form — `use` is reserved, so it can never begin a field label or value), or
// a plain field. Spread position and multiplicity are core elaboration's job.
fn parse_struct_entry<'a>() -> Parser<'a, StructLitEntry> {
    catch(parse_literal(".."))
        .and_keep(lazy(parse_term))
        .map(StructLitEntry::Spread)
        .or(catch(parse_keyword("use"))
            .and_keep(lazy(parse_term))
            .map(StructLitEntry::Use))
        .or(parse_tuple_field().map(StructLitEntry::Field))
}

// A struct literal: `Name { … }` or `Name(args) { … }`. The trailing `{` inside
// the `catch` is the commit point — it distinguishes the literal from a bare
// name / name-application (no brace) and from a Σ-type `{ x : A }` (no head
// name), so there is no grammar conflict. Plain entries reuse the tuple-value
// grammar (`= value` or positional) and `use <term>` fills a concept's
// `use`-marked field; the head's arguments are plain terms (`@`-pinning is not
// the struct idiom — the head type pins instead).
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
    .and(sep_by0_trailing(parse_struct_entry, || parse_literal(",")))
    .and_drop(parse_literal("}"))
    .map(|((head, params), entries)| {
        Subterm::StructLit(StructLit {
            head,
            params,
            entries,
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

// A `use` Π-binder: `use term`. Always anonymous — it binds nothing nameable
// (`_`) but joins the instance scope; an instance is reached by resolution,
// never by name. `use` is a reserved word, so there is no ambiguity with an
// ordinary binder name.
fn parse_use_func_type_param<'a>() -> Parser<'a, FuncTypeParam> {
    catch(parse_keyword("use"))
        .and_keep(lazy(parse_term))
        .map(|type_| FuncTypeParam {
            plicity: Plicity::Witness,
            label: None,
            type_,
        })
}

fn parse_func_type_param<'a>() -> Parser<'a, FuncTypeParam> {
    parse_use_func_type_param().or(parse_plicity()
        .and(
            catch(parse_identifier().and_drop(parse_literal(":")))
                .and(lazy(parse_term))
                .map(|(label, ty): (&str, Term)| (Some(label.to_string()), ty))
                .or(lazy(parse_term).map(|ty| (None, ty))),
        )
        .map(|(plicity, (label, type_))| FuncTypeParam {
            plicity,
            label,
            type_,
        }))
}

fn parse_func_type<'a>() -> Parser<'a, Term> {
    catch(
        parse_literal("(")
            .and_keep(sep_by0(parse_func_type_param, || parse_literal(",")))
            .and_drop(parse_literal(")"))
            .and_drop(parse_literal("->")),
    )
    .and(lazy(parse_term))
    .map(|(params, output): (Vec<FuncTypeParam>, Term)| {
        Subterm::FuncType(FuncType {
            params: params.into_iter().collect(),
            output,
        })
    })
    .map(Into::into)
}

// A lambda parameter with an optional domain annotation. `(x)` is sugar for
// `(x : _)`; the annotation, when present, parses as an arbitrary term and stops
// at the closing `)` (mirrors `parse_func_type_param`).
// A binder name: a plain identifier (`_` to ignore). The language has no
// compound binder patterns — `let`, lambda, function, and constructor-arm binders
// all bind a single name and destructure via projections (`.0`, `.label`).
fn parse_binder<'a>() -> Parser<'a, String> {
    parse_identifier().map(str::to_string)
}

// A match-arm constructor pattern `tag(x, …)` — `nil()` for the nullary case.
// The `(` immediately after the tag is the commit point, distinguishing it from
// a bare name. Arguments are plain binder names: every match arm is one distinct
// constructor binding its payload by name.
fn parse_constructor_arm<'a>() -> Parser<'a, (String, Vec<String>)> {
    catch(parse_identifier().and_drop(parse_literal("(")))
        .and(sep_by0(parse_binder, || parse_literal(",")))
        .and_drop(parse_literal(")"))
        .map(|(tag, args): (&str, Vec<String>)| (tag.to_string(), args))
}

fn parse_func_param<'a>() -> Parser<'a, (String, Option<Term>)> {
    // A leading `use` on a lambda parameter is accepted and dropped: lambdas
    // carry no plicity marks (checking against a Π type supplies them), so the
    // marker is purely documentary here.
    catch(parse_keyword("use"))
        .or(pure(()))
        .and_keep(parse_binder())
        .and(
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
                    .and_keep(parse_nat())
                    .flat_map(|lit| match lit {
                        NatLiteral::Number(n, _) if n.is_zero() => pure(()),
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
                    .and_drop(parse_literal(";"))
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

// A match-arm: `| tag(args…) => body`, one distinct constructor with irrefutable
// payload binders. Nested refutable patterns, scalar literals, and bare
// `| x =>` / `| _ =>` catch-alls are not part of the grammar.
fn parse_inductive_match_branch<'a>() -> Parser<'a, InductiveArm> {
    catch(parse_literal("|"))
        .and_keep(parse_constructor_arm())
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
        .map(|((tag, args), body)| InductiveArm { tag, args, body })
}

// Zero arms are legal: under inversion (Rung C) every impossible arm is
// silently omittable, and a scrutinee whose indices clash with *every*
// constructor's target eliminates with no arms at all.
fn parse_inductive_match<'a>() -> Parser<'a, Term> {
    catch(parse_match_prefix())
        .and(many0(parse_inductive_match_branch))
        .and_drop(parse_keyword("end"))
        .map(|((head, motive), arms)| {
            Subterm::Match(Match::Inductive(InductiveMatch { head, motive, arms }))
        })
        .map(Into::into)
}

// The `| [] =>` identity arm of an `Lst` fold (the empty-array literal).
fn parse_arr_empty_branch<'a>() -> Parser<'a, Term> {
    parse_literal("|")
        .and_drop(parse_literal("[]"))
        .and_drop(parse_literal("=>"))
        .and_keep(lazy(parse_term))
}

// The `| head, ..tail; ih =>` cons arm of a native-inductive fold. Carrier-neutral
// (`Lst` and `Bin` share it); only the empty arm's literal selects the carrier. The
// `,` separates the peeled head from the rest `tail`; the `;` sets `ih` (the
// recursive result on `tail`) apart from the scrutinee's shape. A plain
// case-split needs no induction hypothesis, so `; ih` may be omitted — the
// binder defaults to `_`.
fn parse_cons_branch<'a>() -> Parser<'a, ((String, String, String), Term)> {
    parse_literal("|")
        .and_keep(parse_identifier())
        .and_drop(parse_literal(","))
        .and_drop(parse_literal(".."))
        .and(parse_identifier())
        .and(
            catch(parse_literal(";").and_keep(parse_identifier()))
                .map(str::to_string)
                .or(pure("_".to_string())),
        )
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
        .map(|(((head, tail), ih), cons_case)| {
            ((head.to_string(), tail.to_string(), ih), cons_case)
        })
}

fn parse_arr_match<'a>() -> Parser<'a, Term> {
    catch(parse_match_prefix())
        .and(catch(parse_arr_empty_branch()).and(parse_cons_branch()))
        .and_drop(parse_keyword("end"))
        .map(
            |((head, motive), (empty_case, ((head_label, tail_label, ih_label), cons_case)))| {
                Subterm::Match(Match::Lst(LstMatch {
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

// The `| \\ =>` identity arm of a `Bin` fold (the empty bytestring literal).
fn parse_bin_empty_branch<'a>() -> Parser<'a, Term> {
    parse_literal("|")
        .and_drop(parse_literal("\\\\"))
        .and_drop(parse_literal("=>"))
        .and_keep(lazy(parse_term))
}

fn parse_bin_match<'a>() -> Parser<'a, Term> {
    catch(parse_match_prefix())
        .and(catch(parse_bin_empty_branch()).and(parse_cons_branch()))
        .and_drop(parse_keyword("end"))
        .map(
            |((head, motive), (empty_case, ((head_label, tail_label, ih_label), cons_case)))| {
                Subterm::Match(Match::Bin(BinMatch {
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
        .or(catch(parse_bin_match()))
        .or(parse_inductive_match())
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

// A `use` binder in function-definition sugar (`let`/`rec`/`satisfy` telescopes):
// `use term`. Always anonymous — it binds `_` (lowering mints a fresh name) and
// joins the instance scope; an instance is reached by resolution, never by name.
fn parse_use_func_sugar_param<'a>() -> Parser<'a, FuncSugarParam> {
    catch(parse_keyword("use"))
        .and_keep(lazy(parse_term))
        .map(|type_| FuncSugarParam {
            plicity: Plicity::Witness,
            label: "_".to_string(),
            type_,
        })
}

fn parse_func_sugar_param<'a>() -> Parser<'a, FuncSugarParam> {
    parse_use_func_sugar_param().or(parse_plicity()
        .and(parse_binder())
        .and_drop(parse_literal(":"))
        .and(lazy(parse_term))
        .map(
            |((plicity, label), type_): ((Plicity, String), Term)| FuncSugarParam {
                plicity,
                label,
                type_,
            },
        ))
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

// `let x = e; tail` / `let x : T = e; tail` / `let f(p : T, …) -> R = …; tail`.
// The binder is always a single name; destructuring is done with projections.
fn parse_let<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("let"))
        .and_keep(parse_identifier())
        .and(parse_local_let_signature())
        .and_drop(parse_literal(";"))
        .and(lazy(parse_term))
        .map(|((label, signature), tail)| {
            Subterm::Let(Let {
                binder: label.to_string(),
                signature,
                tail,
            })
        })
        .map(Into::into)
}

// A glued `.index`/`.label` projection, consuming no whitespace — usable both
// as an ordinary term suffix (via the whitespace-eating [`parse_proj_suffix`])
// and inside the tight `Bin`-literal spread operand.
fn parse_proj_suffix_raw<'a>() -> Parser<'a, Field> {
    catch(
        take_exact(".").and_keep(
            parse_usize_raw()
                .map(Field::Index)
                .or(parse_identifier_raw().map(|label| Field::Label(label.to_string())))
                .map_err("Expected field index or label after '.'"),
        ),
    )
}

fn parse_proj_suffix<'a>() -> Parser<'a, Field> {
    parse_proj_suffix_raw().and_drop(parse_whitespace())
}

enum Suffix {
    Proj(Field),
    Apply(Vec<(Plicity, Term)>),
    Bang,
}

// A call-site argument's plicity: `use <term>` fills a witness slot, `@<term>`
// an implicit slot, a plain term an explicit slot. `use` is reserved, so it can
// never begin a plain-argument term.
fn parse_apply_argument<'a>() -> Parser<'a, (Plicity, Term)> {
    catch(parse_keyword("use"))
        .map(|()| Plicity::Witness)
        .or(parse_plicity())
        .and(lazy(parse_term))
}

fn parse_suffix<'a>() -> Parser<'a, Suffix> {
    parse_proj_suffix()
        .map(Suffix::Proj)
        .or(catch(parse_literal("("))
            .and_keep(sep_by0(parse_apply_argument, || parse_literal(",")))
            .and_drop(parse_literal(")"))
            .map(Suffix::Apply))
        // A postfix `!` — but not the `!=` operator, whose `!` would otherwise be
        // eaten here as a bang, stranding the `=`.
        .or(catch(
            take_exact("!")
                .and_drop(not_ahead("="))
                .and_drop(parse_whitespace()),
        )
        .map(|()| Suffix::Bang))
}

// [`parse_suffix`] in glued form for the tight positions: no whitespace is
// consumed after a suffix, so the caller can see whether the next characters
// touch. A call's argument list is self-delimiting, so its interior stays
// whitespace-friendly — only the closing `)` is matched raw.
fn parse_suffix_raw<'a>() -> Parser<'a, Suffix> {
    parse_proj_suffix_raw()
        .map(Suffix::Proj)
        .or(catch(take_exact("("))
            .and_drop(parse_whitespace())
            .and_keep(sep_by0(parse_apply_argument, || parse_literal(",")))
            .and_drop(take_exact(")"))
            .map(Suffix::Apply))
        .or(catch(take_exact("!").and_drop(not_ahead("="))).map(|()| Suffix::Bang))
}

fn apply_suffixes(head: Term, suffixes: Vec<Suffix>) -> Term {
    suffixes
        .into_iter()
        .fold(head, |head, suffix| match suffix {
            Suffix::Proj(field) => Subterm::Proj(Proj { head, field }).into(),
            Suffix::Apply(params) => Subterm::Apply(Apply { head, params }).into(),
            Suffix::Bang => Subterm::Bang(head).into(),
        })
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
            .or(parse_prop())
            .or(parse_prim())
            .or(parse_tuple_type())
            .or(parse_empty_tuple())
            .or(parse_tuple())
            .or(parse_parens())
            .or(parse_name().map(|n| Subterm::Name(n).into()))
            .and(many0(parse_suffix))
            .map(|(head, suffixes): (Term, _)| apply_suffixes(head, suffixes)),
    )
}

// The fixed set of overloaded infix operators, recognised by maximal munch
// (two-character symbols before their one-character prefixes).
fn parse_num_op<'a>() -> Parser<'a, NumOp> {
    fn symbol<'a>(text: &'static str, op: NumOp) -> Parser<'a, NumOp> {
        catch(take_exact(text)).map(move |()| op)
    }

    symbol("==", NumOp::Eql)
        .or(symbol("!=", NumOp::Neq))
        .or(symbol("<=", NumOp::Lte))
        .or(symbol(">=", NumOp::Gte))
        .or(symbol("&&", NumOp::And))
        .or(symbol("||", NumOp::Or))
        .or(symbol("+", NumOp::Add))
        .or(symbol("-", NumOp::Sub))
        .or(symbol("*", NumOp::Mul))
        .or(symbol("/", NumOp::Div))
        .or(symbol("%", NumOp::Rem))
        .or(symbol("<", NumOp::Lt))
        .or(symbol(">", NumOp::Gt))
}

// Operator precedence: higher binds tighter. Every operator is left-associative.
fn op_precedence(op: NumOp) -> u8 {
    match op {
        NumOp::Or => 1,
        NumOp::And => 2,
        NumOp::Eql | NumOp::Neq | NumOp::Lt | NumOp::Gt | NumOp::Lte | NumOp::Gte => 3,
        NumOp::Add | NumOp::Sub => 4,
        NumOp::Mul | NumOp::Div | NumOp::Rem => 5,
    }
}

// At least one whitespace character (then any further whitespace/comments). The
// trailing-space requirement is what distinguishes the operator `-` in `a - 42`
// from the glued sign of the literal `-42`.
fn require_space<'a>() -> Parser<'a, ()> {
    take_while(|char| char.is_whitespace())
        .flat_map(|spaces| match spaces.is_empty() {
            true => fail("expected whitespace after operator"),
            false => pure(()),
        })
        .and_drop(parse_whitespace())
}

// An infix operator with a space on each side, consumed without its operands.
fn parse_infix_op<'a>() -> Parser<'a, NumOp> {
    catch(
        preceded_by_space()
            .and_keep(parse_num_op())
            .and_drop(require_space()),
    )
}

// Precedence-climbing over applied atoms: parse a left operand, then fold in
// every following operator whose precedence is at least `min_prec`. The right
// operand of an operator at precedence `p` is parsed at `p + 1`
// (left-associativity).
fn parse_infix_expr<'a>(min_prec: u8) -> Parser<'a, Term> {
    parse_atomic_term().flat_map(move |left| parse_infix_rest(left, min_prec))
}

fn parse_infix_rest<'a>(left: Term, min_prec: u8) -> Parser<'a, Term> {
    let here = left.clone();

    catch(parse_infix_op().flat_map(move |op| {
        let precedence = op_precedence(op);

        if precedence < min_prec {
            // Binds looser than the caller's level: backtrack, leaving the
            // operator for an enclosing `parse_infix_rest` to consume.
            return fail("operator below current precedence level");
        }

        let left = here;

        parse_infix_expr(precedence + 1).flat_map(move |right| {
            let combined: Term = Subterm::Infix(Infix { op, left, right }).into();
            parse_infix_rest(combined, min_prec)
        })
    }))
    .or(pure(left))
}

fn parse_term<'a>() -> Parser<'a, Term> {
    memoize(MEMO_TERM, parse_term_inner())
}

fn parse_term_inner<'a>() -> Parser<'a, Term> {
    with_span(
        parse_rec()
            .or(parse_let())
            .or(parse_match())
            .or(parse_func_type())
            .or(parse_func())
            .or(parse_infix_expr(0)),
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

// One of the six wire shapes, by its own closed grammar — not an ordinary
// curios type, so this needs no name resolution: `Nat`/`Int`/`Bln`/`Bin`/`Io`
// are literal keywords here, and `Lst(T)` recurses on the same grammar.
fn parse_wire_type<'a>() -> Parser<'a, WireType> {
    parse_identifier().flat_map(|name| match name {
        "Nat" => pure(WireType::Nat),
        "Int" => pure(WireType::Int),
        "Bln" => pure(WireType::Bln),
        "Bin" => pure(WireType::Bin),
        "Io" => pure(WireType::Io),
        "Lst" => catch(parse_literal("("))
            .and_keep(lazy(parse_wire_type))
            .and_drop(parse_literal(")"))
            .map(|element| WireType::Lst(Box::new(element))),
        other => fail(format!(
            "expected a wire type (Nat, Int, Bln, Bin, Io, or Lst(...)), found '{other}'"
        )),
    })
}

// `(T, T, ...) -> T` (a foreign function) or a bare `T` (a zero-argument
// foreign, like `sys_io`'s `io_clock_wall`). Params carry no surface label —
// `a0`, `a1`, … name them positionally; the single result is unnamed (`_`),
// since a `foreign` declaration has no surface syntax for a named record
// result the way `/sys/Io`'s Rust-side rows do.
fn parse_wire_signature<'a>() -> Parser<'a, WireSignature> {
    catch(
        parse_literal("(")
            .and_keep(sep_by0(parse_wire_type, || parse_literal(",")))
            .and_drop(parse_literal(")"))
            .and_drop(parse_literal("->")),
    )
    .and(lazy(parse_wire_type))
    .map(|(params, output)| WireSignature {
        params: params
            .into_iter()
            .enumerate()
            .map(|(index, type_)| (format!("a{index}"), type_))
            .collect(),
        results: vec![("_".to_string(), output)],
    })
    .or(parse_wire_type().map(|output| WireSignature {
        params: vec![],
        results: vec![("_".to_string(), output)],
    }))
}

// `foreign name : T;` — a name and a wire signature with no body, bound to a
// host-provided implementation at link time. Mirrors `parse_top_let`, but ends
// after the signature instead of parsing `= body`.
fn parse_top_foreign<'a>() -> Parser<'a, TopItem> {
    catch(parse_pub().and(parse_keyword("foreign"))).flat_map(|(is_pub, ())| {
        parse_identifier()
            .and_drop(parse_literal(":"))
            .and(parse_wire_signature())
            .and_drop(parse_literal(";"))
            .map(move |(label, signature)| {
                TopItem::Foreign(TopForeign {
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
// `m : Nat` (named), or a bare type (positional). Plicity's `@` (on the name)
// requires a name — a positional binder has nothing for a later type or the
// target to mention.
fn parse_inductive_payload_field<'a>() -> Parser<'a, CasePayloadParam> {
    catch(
        parse_plicity()
            .and(parse_identifier())
            .and_drop(parse_literal(":")),
    )
    .and(lazy(parse_term))
    .map(
        |((plicity, name), type_): ((Plicity, &str), Term)| CasePayloadParam {
            plicity,
            label: Some(name.to_string()),
            type_,
        },
    )
    .or(lazy(parse_term).map(|type_| CasePayloadParam {
        plicity: Plicity::Explicit,
        label: None,
        type_,
    }))
}

fn parse_top_inductive_case<'a>() -> Parser<'a, TopCase> {
    parse_literal("|")
        .and_keep(parse_identifier())
        .and(
            parse_literal("(")
                .and_keep(sep_by0(parse_inductive_payload_field, || {
                    parse_literal(",")
                }))
                .and_drop(parse_literal(")")),
        )
        // The case target: `: (index-exprs)` — the terminal with its
        // mandatory part (the inductive name and the parameters) elided.
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

// An inductive parameter: `name : type`, or `@name : type` to make it implicit at
// the type-constructor function (it is implicit at the value constructors
// either way — the mark's only job is the type constructor, where unmarked
// parameters are written out).
fn parse_inductive_param<'a>() -> Parser<'a, (Plicity, String, Term)> {
    parse_plicity()
        .and(parse_identifier())
        .and_drop(parse_literal(":"))
        .and(lazy(parse_term))
        .map(|((plicity, name), ty): ((Plicity, &str), Term)| (plicity, name.to_string(), ty))
}

// A head index-telescope entry: `n : Nat` or a bare `Nat`. The name is
// documentary (and a dependency hook for later entries) — never in scope in
// the cases — so it is optional and never takes `@`.
fn parse_inductive_index<'a>() -> Parser<'a, (Option<String>, Term)> {
    catch(parse_identifier().and_drop(parse_literal(":")))
        .and(lazy(parse_term))
        .map(|(name, ty): (&str, Term)| (Some(name.to_string()), ty))
        .or(lazy(parse_term).map(|ty| (None, ty)))
}

/// A parsed inductive head arity: the index telescope (each binder optionally
/// named) and the sort it lands in.
type InductiveArity = (Vec<(Option<String>, Term)>, Term);

// The head's arity after the `:` — either an index telescope landing in a sort,
// `(n : Nat) -> Prop`, or a bare sort, `Prop`. The sort is mandatory: an index
// telescope must state where it lands (`-> Sort`), and a sortless head is a
// parse error, never an implicit `Type`.
fn parse_inductive_arity<'a>() -> Parser<'a, InductiveArity> {
    catch(
        parse_literal("(")
            .and_keep(sep_by0(parse_inductive_index, || parse_literal(",")))
            .and_drop(parse_literal(")")),
    )
    .and(parse_literal("->").and_keep(parse_sort()))
    .or(parse_sort().map(|sort| (Vec::new(), sort)))
}

fn parse_top_inductive_body<'a>(is_pub: bool) -> Parser<'a, TopInduct> {
    parse_identifier()
        .and(
            catch(
                parse_literal("(")
                    .and_keep(sep_by0(parse_inductive_param, || parse_literal(",")))
                    .and_drop(parse_literal(")")),
            )
            .or(pure(vec![])),
        )
        // The head's arity: `: (n : Nat) -> Prop` or `: Prop`. The sort is
        // required — there is no implicit `Type`.
        .and(parse_literal(":").and_keep(parse_inductive_arity()))
        .and(many0(parse_top_inductive_case))
        .flat_map(move |(((label, params), (indices, result_sort)), cases)| {
            // Targets are required on every case iff the head declares
            // indices, with arity equal to the index telescope's.
            for case in &cases {
                match (&case.target, indices.len()) {
                    (None, 0) => {}
                    (None, _) => {
                        return fail(format!(
                            "case '{}' of indexed inductive '{label}' must state its \
                             index target: `{}(...) : (...)`",
                            case.label, case.label,
                        ));
                    }
                    (Some(_), 0) => {
                        return fail(format!(
                            "case '{}' states an index target, but inductive '{label}' \
                             declares no indices",
                            case.label,
                        ));
                    }
                    (Some(target), arity) if target.len() != arity => {
                        return fail(format!(
                            "case '{}' of inductive '{label}' states {} index \
                             expression(s), but the head declares {arity}",
                            case.label,
                            target.len(),
                        ));
                    }
                    _ => {}
                }
            }

            let label = label.to_string();
            pure(TopInduct {
                is_pub,
                label,
                params,
                indices,
                result_sort,
                cases,
            })
        })
}

fn parse_top_inductive<'a>() -> Parser<'a, TopItem> {
    catch(parse_pub().and(parse_keyword("induct"))).flat_map(|(is_pub, ())| {
        parse_top_inductive_body(is_pub)
            .and(many0(|| {
                catch(parse_pub().and(parse_keyword("and")))
                    .flat_map(|(is_pub2, ())| parse_top_inductive_body(is_pub2))
            }))
            .and_drop(parse_keyword("end"))
            .map(|(first, rest)| TopItem::Induct(iter::once(first).chain(rest).collect()))
    })
}

/// A universe sort: exactly `Type` or `Prop`. The result sort of a struct or an
/// inductive head is always one of these two — the only universes — so the sort
/// position parses this targeted form rather than a generic `lazy(parse_term)`.
/// A generic term parser is both too loose (admitting terms the elaborator only
/// ever treats as `Type`) and, for a struct, actively wrong: it greedily eats
/// the `{` opening the field block, so `record X : Prop { … }` fails to parse.
fn parse_sort<'a>() -> Parser<'a, Term> {
    parse_prop().or(parse_type())
}

fn parse_top_struct<'a>() -> Parser<'a, TopItem> {
    // `pub`? then the kind keyword: `struct` (rep private) or `record` (rep public).
    let kind = catch(parse_keyword("struct"))
        .map(|()| false)
        .or(parse_keyword("record").map(|()| true));
    catch(parse_pub().and(kind)).flat_map(|(is_pub, rep_pub)| {
        parse_identifier()
            .and(
                catch(
                    parse_literal("(")
                        .and_keep(sep_by0(parse_inductive_param, || parse_literal(",")))
                        .and_drop(parse_literal(")")),
                )
                .or(pure(vec![])),
            )
            // The result sort: `: Type` or `: Prop` after the parameters. Required.
            .and(parse_literal(":").and_keep(parse_sort()))
            // Representation visibility comes from the keyword, not an inner `pub`.
            .and_drop(parse_literal("{"))
            .and(sep_by0_trailing(parse_tuple_type_field, || {
                parse_literal(",")
            }))
            .and_drop(parse_literal("}"))
            .map(move |(((label, params), result_sort), fields)| {
                TopItem::Struct(TopStruct {
                    is_pub,
                    rep_pub,
                    label: label.to_string(),
                    params,
                    result_sort,
                    fields,
                })
            })
    })
}

// A concept field: `use? label : term`, or the signature sugar
// `label(params) -> term` — kept as written in the AST node (`func_params`);
// `to_core` undoes the sugar (mirroring top-level `let`'s function sugar). A
// `use`-prefixed field is a superclass edge — its type must be a concept
// application, checked at lowering.
fn parse_concept_field<'a>() -> Parser<'a, ConceptField> {
    let super_field = catch(parse_keyword("use"))
        .and_keep(lazy(parse_term))
        .map(|type_| ConceptField {
            is_super: true,
            label: String::new(),
            func_params: None,
            type_,
        });

    let plain_or_sugar = parse_identifier()
        .and(
            catch(
                parse_literal("(")
                    .and_keep(sep_by0(parse_func_type_param, || parse_literal(",")))
                    .and_drop(parse_literal(")"))
                    .and_drop(parse_literal("->")),
            )
            .and(lazy(parse_term))
            .map(|(params, output): (Vec<FuncTypeParam>, Term)| (Some(params), output))
            .or(catch(parse_literal(":"))
                .and_keep(lazy(parse_term))
                .map(|type_| (None, type_))),
        )
        .map(|(label, (func_params, type_)): (&str, _)| ConceptField {
            is_super: false,
            label: label.to_string(),
            func_params,
            type_,
        });

    super_field.or(plain_or_sugar)
}

// A concept parameter: an inductive-style `@?name : type` binder, optionally
// prefixed by the contextual `out` marker (an output position — excluded from
// the witness key, pinned by the resolved witness). `out` stays a valid
// parameter name: the marker form needs a binder after it, so when what
// follows `out` fails to parse as a binder (`out : Type`), the whole thing
// re-parses as a parameter named `out`.
fn parse_concept_param<'a>() -> Parser<'a, ConceptParam> {
    let build = |is_out| {
        move |(plicity, label, type_): (Plicity, String, Term)| ConceptParam {
            plicity,
            is_out,
            label,
            type_,
        }
    };

    catch(parse_keyword("out").and_keep(parse_inductive_param()))
        .map(build(true))
        .or(parse_inductive_param().map(build(false)))
}

fn parse_top_concept<'a>() -> Parser<'a, TopItem> {
    catch(parse_pub().and(parse_keyword("concept"))).flat_map(|(is_pub, ())| {
        parse_identifier()
            .and(
                catch(
                    parse_literal("(")
                        .and_keep(sep_by0(parse_concept_param, || parse_literal(",")))
                        .and_drop(parse_literal(")")),
                )
                .or(pure(vec![])),
            )
            .and(parse_literal(":").and_keep(parse_sort()))
            .and_drop(parse_literal("{"))
            .and(sep_by0_trailing(parse_concept_field, || parse_literal(",")))
            .and_drop(parse_literal("}"))
            .map(move |(((label, params), result_sort), fields)| {
                TopItem::Concept(TopConcept {
                    is_pub,
                    label: label.to_string(),
                    params,
                    result_sort,
                    fields,
                })
            })
    })
}

// A witness field: `label = term`, or the definition sugar
// `label(params) = term` — the tuple-field grammar with the label mandatory,
// kept as written in the AST node (`func_params`); `to_core` undoes the sugar.
fn parse_witness_field<'a>() -> Parser<'a, WitnessField> {
    catch(parse_tuple_field_prefix())
        .and(lazy(parse_term))
        .map(|((label, func_params), value)| WitnessField {
            label,
            func_params,
            value,
        })
}

// A witness-body entry: a `use <term>` fill for one of the concept's
// `use`-marked fields, or an implementation field.
fn parse_witness_entry<'a>() -> Parser<'a, WitnessEntry> {
    catch(parse_keyword("use"))
        .and_keep(lazy(parse_term))
        .map(WitnessEntry::Use)
        .or(parse_witness_field().map(WitnessEntry::Field))
}

// A witness declaration is anonymous: `satisfy (params)? Concept(args) { … }`.
// The keyword is the commit point, exactly as before — nothing else at item
// position begins with `satisfy`. No separator sits between the optional
// telescope and the concept: the telescope is a parenthesized group and the
// concept is a name, so the two never run together.
fn parse_top_witness<'a>() -> Parser<'a, TopItem> {
    catch(parse_keyword("satisfy")).flat_map(|()| {
        catch(
            parse_literal("(")
                .and_keep(sep_by0(parse_func_sugar_param, || parse_literal(",")))
                .and_drop(parse_literal(")")),
        )
        .or(pure(vec![]))
        .and(parse_name())
        .and(
            catch(
                parse_literal("(")
                    .and_keep(sep_by0(|| lazy(parse_term), || parse_literal(",")))
                    .and_drop(parse_literal(")")),
            )
            .or(pure(vec![])),
        )
        .and_drop(parse_literal("{"))
        .and(sep_by0_trailing(parse_witness_entry, || parse_literal(",")))
        .and_drop(parse_literal("}"))
        .map(move |(((params, concept), args), entries)| {
            TopItem::Witness(TopWitness {
                params,
                concept,
                args,
                entries,
            })
        })
    })
}

fn parse_top_item<'a>() -> Parser<'a, TopItem> {
    parse_top_mod()
        .or(parse_top_use())
        .or(parse_top_concept())
        .or(parse_top_witness())
        .or(parse_top_let())
        .or(parse_top_inductive())
        .or(parse_top_struct())
        .or(parse_top_rec())
        .or(parse_top_foreign())
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
