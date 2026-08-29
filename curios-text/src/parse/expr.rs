use super::*;

// A plain-label binding with a mandatory type: every top-level member, and every member of a local group after the first.
pub(super) fn parse_binding<'a>() -> Parser<'a, (String, LetSignature)> {
    parse_identifier()
        .and(parse_let_signature())
        .map(|(label, signature)| (label.to_string(), signature))
}

// One `let` statement: `let pattern (: T)? = e;`, or the group `let f … and g … and h …;` whose later members are plain labels with mandatory types.
fn parse_let_group<'a>() -> Parser<'a, LetGroup> {
    catch(parse_keyword("let"))
        .and_keep(parse_pattern())
        .and(parse_local_let_signature())
        .map(|(binder, signature)| LetBinding { binder, signature })
        .and(many0(|| {
            catch(parse_keyword("and"))
                .and_keep(parse_binding())
                .map(|(label, signature)| LetBinding {
                    binder: Pattern::Binder(Some(label)),
                    signature,
                })
        }))
        .and_drop(parse_literal(";"))
        .map(|(first, rest)| LetGroup {
            members: iter::once(first).chain(rest).collect(),
        })
}

// A `use` binder in function-definition sugar (`let`/`satisfy` telescopes): `use term`. Always anonymous — there is no source binder position at all (lowering mints a fresh name directly) and joins the instance scope; an instance is reached by resolution, never by name.
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

// Parses the part of a `let` binding after its name where a type is **required**: the function sugar, or the `: T = body` form. Used for top-level `let` and for every member after `and`, whose types cannot be inferred.
pub(super) fn parse_let_signature<'a>() -> Parser<'a, LetSignature> {
    parse_func_let_signature().or(parse_required_name_signature())
}

// Like `parse_let_signature`, but the plain form's type annotation may be omitted. Used only by local `let`, where the body's type can be inferred.
pub(super) fn parse_local_let_signature<'a>() -> Parser<'a, LetSignature> {
    parse_func_let_signature().or(parse_optional_name_signature())
}

// `let x = e; tail` / `let x : T = e; tail` / `let (x, y) = e; tail` / `let f(p : T, …) -> R = …; tail` / `let f … and g …; tail`. The binder accepts a tuple/struct pattern (see `Pattern`), desugaring at lowering into a fresh binder plus a projection-`let` chain.
//
// `many1` parses the whole run of `let` statements in a loop, then the tail once, and they become a single flat `Let` block — one node for the whole run, not a right-nested chain — so nothing downstream (clone, lowering) recurses once per binding. The leading `mark` on the first statement and the trailing `mark` after the tail span the block.
pub(super) fn parse_let<'a>() -> Parser<'a, Term> {
    many1(|| mark().and(parse_let_group()))
        .and(lazy(parse_term).and(mark()))
        .map(|(statements, (tail, end))| {
            // `many1` guarantees at least one statement, so the span start is defined.
            let span = statements[0].0.to(&end);

            let groups = statements.into_iter().map(|(_, group)| group).collect();

            Term::from(Subterm::Let(Let { groups, tail })).with_span(span)
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
pub(super) fn parse_infix_symbol<'a>() -> Parser<'a, InfixOp> {
    fn symbol<'a>(text: &'static str, op: InfixOp) -> Parser<'a, InfixOp> {
        catch(take_exact(text)).map(move |()| op)
    }

    symbol("==", InfixOp::Eql)
        .or(symbol("!=", InfixOp::Neq))
        .or(symbol("<=", InfixOp::Le))
        .or(symbol(">=", InfixOp::Ge))
        .or(symbol("&&", InfixOp::And))
        .or(symbol("||", InfixOp::Or))
        .or(symbol("+", InfixOp::Add))
        .or(symbol("-", InfixOp::Sub))
        .or(symbol("*", InfixOp::Mul))
        .or(symbol("/", InfixOp::Div))
        .or(symbol("%", InfixOp::Rem))
        .or(symbol("<", InfixOp::Lt))
        .or(symbol(">", InfixOp::Gt))
}

// Operator precedence: higher binds tighter. Every operator is left-associative. The printer consumes this table too, reinserting exactly the parentheses the climb would need to reparse its output.
pub(crate) fn op_precedence(op: InfixOp) -> u8 {
    match op {
        InfixOp::Or => 1,
        InfixOp::And => 2,
        InfixOp::Eql | InfixOp::Neq | InfixOp::Lt | InfixOp::Gt | InfixOp::Le | InfixOp::Ge => 3,
        InfixOp::Add | InfixOp::Sub => 4,
        InfixOp::Mul | InfixOp::Div | InfixOp::Rem => 5,
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
pub(super) fn parse_infix_op<'a>() -> Parser<'a, InfixOp> {
    catch(
        preceded_by_space()
            .and_keep(parse_infix_symbol())
            .and_drop(require_space()),
    )
}

// Precedence-climbing over applied atoms: parse a left operand, then fold in every following operator whose precedence is at least `min_prec`. The right operand of an operator at precedence `p` is parsed at `p + 1` (left-associativity).
pub(super) fn parse_infix_expr<'a>(min_prec: u8) -> Parser<'a, Term> {
    parse_atomic_term().flat_map(move |left| parse_infix_rest(left, min_prec))
}

pub(super) fn parse_infix_rest<'a>(left: Term, min_prec: u8) -> Parser<'a, Term> {
    // One `many0` loop per precedence level, folded by move. The previous spelling recursed once per operator *and* deep-cloned the accumulated left spine at every link (`let here = left.clone()` before the catch), so an N-operator chain cost N native frame nests and O(N²) cloned nodes — the same per-element-recursion class the flat `let` block in `parse_let` was rebuilt to avoid. Native depth is now bounded by the precedence table's height (each `parse_infix_expr(precedence + 1)` descends one level), never by chain length, and the left operand is cloned zero times.
    many0(move || {
        catch(parse_infix_op().flat_map(move |op| {
            let precedence = op_precedence(op);

            if precedence < min_prec {
                // Binds looser than the caller's level: backtrack, leaving the operator for an enclosing `parse_infix_rest` to consume.
                return fail("operator below current precedence level");
            }

            parse_infix_expr(precedence + 1).map(move |right| (op, right))
        }))
    })
    .map(move |pairs| {
        pairs.into_iter().fold(left, |left, (op, right)| {
            Subterm::Infix(Infix { op, left, right }).into()
        })
    })
}

pub(crate) fn parse_term<'a>() -> Parser<'a, Term> {
    memoize(MEMO_TERM, parse_term_inner())
}

pub(super) fn parse_term_inner<'a>() -> Parser<'a, Term> {
    with_span(
        parse_let()
            .or(parse_match())
            .or(parse_choose())
            .or(parse_func_type())
            .or(parse_func())
            .or(parse_infix_expr(0)),
    )
}
