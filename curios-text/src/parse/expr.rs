use super::*;

pub(super) fn parse_binding<'a>() -> Parser<'a, RecItem> {
    parse_identifier()
        .and(parse_let_signature())
        .map(|(label, signature)| RecItem {
            label: label.to_string(),
            signature,
        })
}

pub(super) fn parse_rec<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("rec"))
        .and_keep(sep_by1(parse_binding, || parse_keyword("and")))
        .and_drop(parse_literal(";"))
        .and(lazy(parse_term))
        .map(|(items, tail)| Subterm::Rec(Rec { items, tail }))
        .map(Into::into)
}

// A `use` binder in function-definition sugar (`let`/`rec`/`satisfy` telescopes): `use term`. Always anonymous — there is no source binder position at all (lowering mints a fresh name directly) and joins the instance scope; an instance is reached by resolution, never by name.
pub(super) fn parse_use_func_sugar_param<'a>() -> Parser<'a, FuncSugarParam> {
    catch(parse_keyword("use"))
        .and_keep(lazy(parse_term))
        .map(|type_| FuncSugarParam {
            plicity: Plicity::Witness,
            label: Pattern::Binder(None),
            type_,
        })
}

pub(super) fn parse_func_sugar_param<'a>() -> Parser<'a, FuncSugarParam> {
    parse_use_func_sugar_param().or(parse_plicity()
        .and(parse_pattern())
        .and_drop(parse_literal(":"))
        .and(lazy(parse_term))
        .map(
            |((plicity, label), type_): ((Plicity, Pattern), Term)| FuncSugarParam {
                plicity,
                label,
                type_,
            },
        ))
}

// The function-definition sugar `(p : T, ...) -> R = body`. Shared by both the type-required and the local (type-optional) signature parsers.
pub(super) fn parse_func_let_signature<'a>() -> Parser<'a, LetSignature> {
    catch(
        parse_literal("(")
            .and_keep(sep_by0_trailing(parse_func_sugar_param, || {
                parse_literal(",")
            }))
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
pub(super) fn parse_required_name_signature<'a>() -> Parser<'a, LetSignature> {
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
pub(super) fn parse_optional_name_signature<'a>() -> Parser<'a, LetSignature> {
    catch(parse_literal(":").and_keep(lazy(parse_term)))
        .map(Some)
        .or(pure(None))
        .and_drop(parse_literal("="))
        .and(lazy(parse_term))
        .map(|(type_, body)| LetSignature::Name { type_, body })
}

// Parses the part of a `let`/`rec` binding after its name where a type is **required**: the function sugar, or the `: T = body` form. Used for top-level `let` and every `rec` binding, whose types cannot be inferred.
pub(super) fn parse_let_signature<'a>() -> Parser<'a, LetSignature> {
    parse_func_let_signature().or(parse_required_name_signature())
}

// Like `parse_let_signature`, but the plain form's type annotation may be omitted. Used only by local `let`, where the body's type can be inferred.
pub(super) fn parse_local_let_signature<'a>() -> Parser<'a, LetSignature> {
    parse_func_let_signature().or(parse_optional_name_signature())
}

// `let x = e; tail` / `let x : T = e; tail` / `let (x, y) = e; tail` / `let f(p : T, …) -> R = …; tail`. The binder accepts a tuple/struct pattern (see `Pattern`), desugaring at lowering into a fresh binder plus a projection-`let` chain.
//
// `many1` parses the whole run of `let` headers in a loop, then the tail once, and they become a single flat `Let` block — one node for the whole run, not a right-nested chain — so nothing downstream (clone, lowering) recurses once per binding. The leading `mark` on the first header and the trailing `mark` after the tail span the block.
pub(super) fn parse_let<'a>() -> Parser<'a, Term> {
    many1(|| {
        mark().and(
            catch(parse_keyword("let"))
                .and_keep(parse_pattern())
                .and(parse_local_let_signature())
                .and_drop(parse_literal(";")),
        )
    })
    .and(lazy(parse_term).and(mark()))
    .map(|(headers, (tail, end))| {
        // `many1` guarantees at least one header, so the span start is defined.
        let span = headers[0].0.to(&end);

        let bindings = headers
            .into_iter()
            .map(|(_, (binder, signature))| LetBinding { binder, signature })
            .collect();

        Term::from(Subterm::Let(Let { bindings, tail })).with_span(span)
    })
}

// A glued `.index`/`.label` projection, consuming no whitespace — usable both as an ordinary term suffix (via the whitespace-eating [`parse_proj_suffix`]) and inside the tight `Bin`-literal spread operand.
pub(super) fn parse_proj_suffix_raw<'a>() -> Parser<'a, Field> {
    catch(
        take_exact(".").and_keep(
            parse_usize_raw()
                .map(Field::Index)
                .or(parse_identifier_raw().map(|label| Field::Label(label.to_string())))
                .map_err("Expected field index or label after '.'"),
        ),
    )
}

pub(super) fn parse_proj_suffix<'a>() -> Parser<'a, Field> {
    parse_proj_suffix_raw().and_drop(parse_whitespace())
}

pub(super) enum Suffix {
    Proj(Field),
    Apply(Vec<(Plicity, Term)>),
    Bang,
}

// A call-site argument's plicity: `use <term>` fills a witness slot, `@<term>` an implicit slot, a plain term an explicit slot. `use` is reserved, so it can never begin a plain-argument term.
pub(super) fn parse_apply_argument<'a>() -> Parser<'a, (Plicity, Term)> {
    catch(parse_keyword("use"))
        .map(|()| Plicity::Witness)
        .or(parse_plicity())
        .and(lazy(parse_term))
}

pub(super) fn parse_suffix<'a>() -> Parser<'a, Suffix> {
    parse_proj_suffix()
        .map(Suffix::Proj)
        .or(catch(parse_literal("("))
            .and_keep(sep_by0_trailing(parse_apply_argument, || {
                parse_literal(",")
            }))
            .and_drop(parse_literal(")"))
            .map(Suffix::Apply))
        // A postfix `!` — but not the `!=` operator, whose `!` would otherwise be eaten here as a bang, stranding the `=`.
        .or(catch(
            take_exact("!")
                .and_drop(not_ahead("="))
                .and_drop(parse_whitespace()),
        )
        .map(|()| Suffix::Bang))
}

pub(super) fn apply_suffixes(head: Term, suffixes: Vec<Suffix>) -> Term {
    suffixes
        .into_iter()
        .fold(head, |head, suffix| match suffix {
            Suffix::Proj(field) => Subterm::Proj(Proj { head, field }).into(),
            Suffix::Apply(params) => Subterm::Apply(Apply { head, params }).into(),
            Suffix::Bang => Subterm::Bang(head).into(),
        })
}

pub(super) fn parse_empty_tuple<'a>() -> Parser<'a, Term> {
    catch(parse_literal("(").and_keep(parse_literal(")")))
        .map(|_| Subterm::Tuple(Tuple { fields: vec![] }))
        .map(Into::into)
}

pub(super) fn with_span<'a>(parser: Parser<'a, Term>) -> Parser<'a, Term> {
    spanned(parser).map(|(span, term)| term.with_span(span))
}

pub(super) fn parse_goal<'a>() -> Parser<'a, Term> {
    // `?` is not an identifier character, so a plain literal suffices — no token-aware matching needed. (`_` remains the match wildcard binder.) A written `?` is a *goal* — reported at zonk — never a silent `Subterm::Hole`, which only desugars mint.
    catch(parse_literal("?")).map(|()| Subterm::Goal.into())
}

// Grammar keys for the packrat cache (see `parser::memoize`). Only the nonterminals that overlapping alternatives re-probe at the same offset are memoized; that is enough to keep parsing linear. `patterns.rs` mints its own keys (`MEMO_PATTERN`/`MEMO_MATCH_PATTERN`) for the same reason.
const MEMO_TERM: u32 = 0;
const MEMO_ATOMIC_TERM: u32 = 1;

pub(super) fn parse_atomic_term<'a>() -> Parser<'a, Term> {
    memoize(MEMO_ATOMIC_TERM, parse_atomic_term_inner())
}

pub(super) fn parse_atomic_term_inner<'a>() -> Parser<'a, Term> {
    with_span(
        parse_goal()
            .or(parse_struct_lit())
            .or(parse_qualified_name().map(|n| Subterm::Name(n).into()))
            .or(parse_type())
            .or(parse_prop())
            .or(parse_intrinsic())
            .or(parse_tuple_type())
            .or(parse_empty_tuple())
            .or(parse_tuple())
            .or(parse_parens())
            .or(parse_name().map(|n| Subterm::Name(n).into()))
            .and(many0(parse_suffix))
            .map(|(head, suffixes): (Term, _)| apply_suffixes(head, suffixes)),
    )
}

// The fixed set of overloaded infix operators, recognised by maximal munch (two-character symbols before their one-character prefixes).
pub(super) fn parse_num_op<'a>() -> Parser<'a, NumOp> {
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

// Operator precedence: higher binds tighter. Every operator is left-associative. The printer consumes this table too, reinserting exactly the parentheses the climb would need to reparse its output.
pub(crate) fn op_precedence(op: NumOp) -> u8 {
    match op {
        NumOp::Or => 1,
        NumOp::And => 2,
        NumOp::Eql | NumOp::Neq | NumOp::Lt | NumOp::Gt | NumOp::Lte | NumOp::Gte => 3,
        NumOp::Add | NumOp::Sub => 4,
        NumOp::Mul | NumOp::Div | NumOp::Rem => 5,
    }
}

// At least one whitespace character (then any further whitespace/comments). The trailing-space requirement is what distinguishes the operator `-` in `a - 42` from the glued sign of the literal `-42`.
pub(super) fn require_space<'a>() -> Parser<'a, ()> {
    take_while(|char| char.is_whitespace())
        .flat_map(|spaces| match spaces.is_empty() {
            true => fail("expected whitespace after operator"),
            false => pure(()),
        })
        .and_drop(parse_whitespace())
}

// An infix operator with a space on each side, consumed without its operands.
pub(super) fn parse_infix_op<'a>() -> Parser<'a, NumOp> {
    catch(
        preceded_by_space()
            .and_keep(parse_num_op())
            .and_drop(require_space()),
    )
}

// Precedence-climbing over applied atoms: parse a left operand, then fold in every following operator whose precedence is at least `min_prec`. The right operand of an operator at precedence `p` is parsed at `p + 1` (left-associativity).
pub(super) fn parse_infix_expr<'a>(min_prec: u8) -> Parser<'a, Term> {
    parse_atomic_term().flat_map(move |left| parse_infix_rest(left, min_prec))
}

pub(super) fn parse_infix_rest<'a>(left: Term, min_prec: u8) -> Parser<'a, Term> {
    let here = left.clone();

    catch(parse_infix_op().flat_map(move |op| {
        let precedence = op_precedence(op);

        if precedence < min_prec {
            // Binds looser than the caller's level: backtrack, leaving the operator for an enclosing `parse_infix_rest` to consume.
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

pub(crate) fn parse_term<'a>() -> Parser<'a, Term> {
    memoize(MEMO_TERM, parse_term_inner())
}

pub(super) fn parse_term_inner<'a>() -> Parser<'a, Term> {
    with_span(
        parse_rec()
            .or(parse_let())
            .or(parse_match())
            .or(parse_choose())
            .or(parse_func_type())
            .or(parse_func())
            .or(parse_infix_expr(0)),
    )
}
