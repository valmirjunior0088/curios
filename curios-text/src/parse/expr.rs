use {
    super::{
        DEFINITION_NAMES_ITS_MEMBERS, MEMO_ATOMIC_TERM, MEMO_TERM, MEMO_TERM_BEFORE_ARROW, Read,
        comment_ending_at, members, parse_caller_sort, parse_choose, parse_func, parse_func_type,
        parse_intrinsic, parse_keyword, parse_let_binder, parse_literal, parse_mark, parse_match,
        parse_name, parse_parens, parse_pattern, parse_premise, parse_prop, parse_qualified_name,
        parse_struct_lit, parse_tuple, parse_tuple_type, parse_type, parse_usize_raw, refused,
    },
    crate::{
        Apply, Argument, Field, FuncSugarParam, Infix, Label, Let, LetBinding, LetGroup,
        LetSignature, Pattern, Proj, Subterm, Term, Tuple, declared, parse_identifier_raw,
        parse_whitespace,
    },
    curios_parse::{
        Parsed, Parser, ParserState, commit, fail, fail_from, is_blank, lazy, look_ahead, many0,
        many1, mark, memoize, not_ahead, preceded_by_space, pure, sep_by0_trailing, shown, spanned,
        take_eof, take_exact, take_n, take_while, unseen,
    },
    curios_utilities::{InfixOp, Plicity, Span, is_identifier_char, is_keyword},
    std::iter,
};

/// What refuses a definition with parameters and no result.
const DEFINITION_STATES_ITS_RESULT: &str = "a definition with parameters states its result before its body: `let double(n: Nat) -> Nat = …`";

/// What refuses `:=` where a definition's `=` belongs.
const DEFINITION_TAKES_EQUALS: &str =
    "a definition is written with `=`, as in `let x: Nat = 3;`: `:=` is no token of Curios";

/// What refuses a lambda written with a bare parameter.
pub(super) const LAMBDA_TAKES_PARENTHESES: &str =
    "a lambda's parameters are written in parentheses: `(x) => x + 1`";

/// `refusal` where `literal` stands, and nothing where it does not: the literal is consumed, so the refusal is past the choice point and outranks the token the form around it wanted there, with its caret on the literal.
pub(super) fn refusing<'a>(literal: &'static str, refusal: &'static str) -> impl Parser<'a, ()> {
    look_ahead(take_exact(literal))
        .map(|()| true)
        .or(pure(false))
        .flat_map(move |stands| match stands {
            true => mark()
                .and_drop(take_exact(literal))
                .flat_map(move |start| fail_from(&start, refusal))
                .boxed(),
            false => pure(()).boxed(),
        })
}

// A plain-label binding with a mandatory type: every top-level member, and every member of a local group after the first.
pub(super) fn parse_binding<'a>(state: ParserState<'a>) -> Parsed<'a, (Label, LetSignature)> {
    declared(parse_let_signature).parse(state)
}

// One `let` statement: `let pattern (: T)? = e;`, or the group `let f … and g … and h …;` whose later members are plain labels with mandatory types.
fn parse_let_group<'a>(state: ParserState<'a>) -> Parsed<'a, LetGroup> {
    parse_keyword("let")
        .and_keep(parse_let_binder)
        .and(parse_local_let_signature)
        .map(|(binder, signature)| LetBinding { binder, signature })
        .and(many0(|| {
            parse_keyword("and")
                .and_keep(commit(parse_binding))
                .map(|(label, signature)| LetBinding {
                    binder: Pattern::Binder(label),
                    signature,
                })
        }))
        .and_drop(refusing("=>", LAMBDA_TAKES_PARENTHESES))
        .and_drop(parse_literal(";"))
        .map(|(first, rest)| LetGroup {
            members: iter::once(first).chain(rest).collect(),
        })
        .parse(state)
}

// One member of a definition telescope (`let`, `satisfy`): `use Concept(args)`, which has no binder and joins the witness scope, or under `@` or no mark a binder pattern with its type. A plain or `@` member written as its type alone is read and reported: a parameter the body cannot name is an annotation forgotten far more often than a parameter meant, and `_: T` says the second on purpose — a bound the body uses as a fact and never by name is `@_: T`.
pub(super) fn parse_func_sugar_param<'a>(
    state: ParserState<'a>,
) -> Parsed<'a, Read<FuncSugarParam>> {
    mark()
        .and(parse_mark)
        .flat_map(|(start, plicity)| match plicity {
            Plicity::Witness => parse_premise(start)
                .map(|read| read.map(FuncSugarParam::Premise))
                .boxed(),
            Plicity::Explicit | Plicity::Implicit => parse_pattern
                .and_drop(parse_literal(":"))
                .and(lazy(parse_term))
                .map(move |(binder, type_)| {
                    Ok(FuncSugarParam::Bound {
                        implicit: plicity == Plicity::Implicit,
                        binder,
                        type_,
                    })
                })
                .or(lazy(parse_term)
                    .flat_map(move |_| refused(&start, DEFINITION_NAMES_ITS_MEMBERS)))
                .boxed(),
        })
        .parse(state)
}

// The function-definition sugar `(p : T, ...) -> R = body`. Shared by both the type-required and the local (type-optional) signature parsers.
pub(super) fn parse_func_let_signature<'a>(owns_body: bool) -> impl Parser<'a, LetSignature> {
    parse_literal("(")
        .and_keep(sep_by0_trailing(parse_func_sugar_param, || {
            parse_literal(",")
        }))
        .and_drop(parse_literal(")"))
        .and_drop(parse_literal("->").map_err(DEFINITION_STATES_ITS_RESULT))
        // Past the arrow this is the definition sugar, so a member it refuses is its own to report.
        .flat_map(members)
        .and(lazy(parse_term))
        .and_drop(refusing(":=", DEFINITION_TAKES_EQUALS))
        .and_drop(parse_literal("="))
        .and(body(owns_body))
        .map(|((params, output), body)| LetSignature::Func {
            params,
            output,
            body,
        })
}

// The plain `: T = body` form with a mandatory type.
pub(super) fn parse_required_name_signature<'a>(owns_body: bool) -> impl Parser<'a, LetSignature> {
    refusing(":=", DEFINITION_TAKES_EQUALS)
        .and_keep(parse_literal(":"))
        .and_keep(lazy(parse_term))
        .and_drop(refusing(":=", DEFINITION_TAKES_EQUALS))
        .and_drop(parse_literal("="))
        .and(body(owns_body))
        .map(|(type_, body)| LetSignature::Name {
            type_: Some(type_),
            body,
        })
}

/// A definition's body, committed when the signature before it could have been nothing but a declaration's: past `let name : T =` or `let name(params) -> T =` the local reading is the same text failing the same way, so a mistake in the body is the item's own diagnosis rather than a reason to try the tail — which is what lets the item loop recover past it. A local `let` keeps its body recoverable, since the term grammar around it decides its diagnoses.
fn body<'a>(owned: bool) -> impl Parser<'a, Term> {
    match owned {
        true => commit(lazy(parse_term)).boxed(),
        false => lazy(parse_term).boxed(),
    }
}

// The plain `(: T)? = body` form: the type may be omitted (inferred from `body`).
pub(super) fn parse_optional_name_signature<'a>(
    state: ParserState<'a>,
) -> Parsed<'a, LetSignature> {
    refusing(":=", DEFINITION_TAKES_EQUALS)
        .and_keep(
            parse_literal(":")
                .and_keep(lazy(parse_term))
                .map(Some)
                .or(pure(None)),
        )
        .and_drop(refusing(":=", DEFINITION_TAKES_EQUALS))
        .and_drop(parse_literal("="))
        .and(lazy(parse_term))
        .map(|(type_, body)| LetSignature::Name { type_, body })
        .parse(state)
}

// Parses the part of a `let` binding after its name where a type is **required**: the function sugar, or the `: T = body` form. Used for top-level `let` and for every member after `and`, whose types cannot be inferred.
//
// The local spelling, `= body` with no type, is refused by the rule rather than by the token: both forms above fail at the `=` without consuming it, so the report would be the sugar's `Expected '('`, which names one of the two tokens that would have served and reads as a demand for a parameter list. The `=` is consumed before the failure so this arm is the furthest and wins [`Parser::or`]'s tie-break, as `refuse_declaration_head` does, and read raw so the caret underlines the `=` alone.
pub(super) fn parse_let_signature<'a>(state: ParserState<'a>) -> Parsed<'a, LetSignature> {
    parse_func_let_signature(true)
        .or(parse_required_name_signature(true))
        .or(mark().and_drop(take_exact("=")).flat_map(|start| {
            fail_from(
                &start,
                "this definition states no type; a top-level `let` and every member after `and` state theirs — `: T = …`, or `(params) -> T = …` — and only a local `let` may leave its type out",
            )
        })).parse(state)
}

// Like `parse_let_signature`, but the plain form's type annotation may be omitted. Used only by local `let`, where the body's type can be inferred.
pub(super) fn parse_local_let_signature<'a>(state: ParserState<'a>) -> Parsed<'a, LetSignature> {
    parse_func_let_signature(false)
        .or(parse_optional_name_signature)
        .parse(state)
}

// `let x = e; tail` / `let x : T = e; tail` / `let (x, y) = e; tail` / `let f(p : T, …) -> R = …; tail` / `let f … and g …; tail`. The binder accepts a tuple/struct pattern (see `Pattern`), desugaring at lowering into a fresh binder plus a projection-`let` chain.
//
// `many1` parses the whole run of `let` statements in a loop, then the tail once, and they become a single flat `Let` block — one node for the whole run, not a right-nested chain — so nothing downstream (clone, lowering) recurses once per binding. The leading `mark` on the first statement and the trailing `mark` after the tail span the block.
pub(super) fn parse_let<'a>(state: ParserState<'a>) -> Parsed<'a, Term> {
    many1(|| mark().and(parse_let_group))
        .and(lazy(parse_term).and(mark))
        .map(|(statements, (tail, end))| {
            // `many1` guarantees at least one statement, so the span start is defined.
            let span = statements[0].0.to(&end);

            let groups = statements.into_iter().map(|(_, group)| group).collect();

            Term::from(Subterm::Let(Let { groups, tail })).with_span(trimmed(span))
        })
        .parse(state)
}

// A glued `.index`/`.label` projection, consuming no whitespace — usable both as an ordinary term suffix (via the whitespace-eating [`parse_proj_suffix`]) and inside the tight `Bin`-literal spread operand.
pub(super) fn parse_proj_suffix_raw<'a>(state: ParserState<'a>) -> Parsed<'a, Field> {
    take_exact(".")
        .and_keep(
            parse_usize_raw
                .map(Field::Index)
                .or(parse_identifier_raw.map(|label| Field::Label(label.to_string())))
                .map_err("Expected field index or label after '.'"),
        )
        .parse(state)
}

pub(super) fn parse_proj_suffix<'a>(state: ParserState<'a>) -> Parsed<'a, Field> {
    parse_proj_suffix_raw
        .and_drop(parse_whitespace)
        .parse(state)
}

pub(super) enum Suffix {
    Proj(Field),
    Apply(Vec<Argument>),
    Bang,
}

// A call-site argument under its mark: `use <term>` fills a witness slot, `@<term>` an implicit slot, a plain term an explicit slot. Neither mark can begin a plain-argument term.
pub(super) fn parse_apply_argument<'a>(state: ParserState<'a>) -> Parsed<'a, Argument> {
    parse_mark
        .and(lazy(parse_term))
        .map(|(plicity, term)| Argument { term, plicity })
        .parse(state)
}

pub(super) fn parse_suffix<'a>(state: ParserState<'a>) -> Parsed<'a, Suffix> {
    parse_proj_suffix
        .map(Suffix::Proj)
        .or(parse_literal("(")
            .and_keep(sep_by0_trailing(parse_apply_argument, || {
                parse_literal(",")
            }))
            // A bare parameter and its arrow are a term and then no closer, so the list's own `)` would be the complaint.
            .and_drop(commit(
                refusing("=>", LAMBDA_TAKES_PARENTHESES).and_keep(parse_literal(")")),
            ))
            .map(Suffix::Apply))
        // A postfix `!` — but not the `!=` operator, whose `!` would otherwise be eaten here as a bang, stranding the `=`.
        .or(take_exact("!")
            .and_drop(not_ahead("="))
            .and_drop(parse_whitespace)
            .map(|()| Suffix::Bang))
        .parse(state)
}

/// Folds a postfix chain onto its head, one node per suffix. Every intermediate node takes the span from the head's start to its own suffix's end — `acts.0` inside `acts.0!`, `f(x)` inside `f(x).1` — so an error at a node in the middle of a chain reports there rather than at the nearest spanned ancestor, which for a hoisted action is the outermost `!` of its region. A head without a span (a bare name) leaves the chain's own nodes unspanned, and the caller's `with_span` over the whole chain covers those.
pub(super) fn apply_suffixes(head: Term, suffixes: Vec<(Span, Suffix)>) -> Term {
    suffixes
        .into_iter()
        .fold(head, |head, (suffix_span, suffix)| {
            let span = head
                .span()
                .map(|start| Span::new(start.source.clone(), start.start, suffix_span.end));
            let node: Term = match suffix {
                Suffix::Proj(field) => Subterm::Proj(Proj { head, field }).into(),
                Suffix::Apply(arguments) => Subterm::Apply(Apply { head, arguments }).into(),
                Suffix::Bang => Subterm::Bang(head).into(),
            };
            match span {
                Some(span) => node.with_span(trimmed(span)),
                None => node,
            }
        })
}

pub(super) fn parse_empty_tuple<'a>(state: ParserState<'a>) -> Parsed<'a, Term> {
    parse_literal("(")
        .and_keep(parse_literal(")"))
        .map(|_| Subterm::Tuple(Tuple { fields: vec![] }))
        .map(Into::into)
        .parse(state)
}

pub(super) fn with_span<'a>(parser: impl Parser<'a, Term>) -> impl Parser<'a, Term> {
    spanned(parser).map(|(span, term)| term.with_span(trimmed(span)))
}

/// The span cut back to the term's own text, which every span a term is stamped with passes through — the wrapper's, a postfix chain's and a `let` block's alike. Every atom parser consumes the whitespace after it, so a span built from where the parse stopped would run on to the next operator or delimiter, and a caret underline the blanks after an operand; the same run, comment included, is what the `?` oracle, the language server and the test runner's body slice read. A comment is stepped over through the table `parse_whitespace` records it in, complete for everything the parser consumed by the time this runs. The formatter, which needs the far end — a comment past a separator is paid onto its line by the term that reaches it — recovers it from the source text in `print_term` rather than from the span.
pub(super) fn trimmed(mut span: Span) -> Span {
    loop {
        let text = &span.source.text[span.start..span.end];
        let end = span.start + text.trim_end().len();
        match comment_ending_at(end) {
            Some(comment) if comment.start >= span.start => span.end = comment.start,
            _ => {
                span.end = end;
                return span;
            }
        }
    }
}

pub(super) fn parse_goal<'a>(state: ParserState<'a>) -> Parsed<'a, Term> {
    // `?` is not an identifier character, so a plain literal suffices — no token-aware matching needed. (`_` remains the match wildcard binder.) A written `?` is a *goal* — reported at zonk — never a silent `Subterm::Hole`, which only desugars mint.
    parse_literal("?")
        .map(|()| Subterm::Goal.into())
        .parse(state)
}

pub(super) fn parse_atomic_term<'a>(state: ParserState<'a>) -> Parsed<'a, Term> {
    memoize(MEMO_ATOMIC_TERM, parse_atomic_term_inner).parse(state)
}

pub(super) fn parse_atomic_term_inner<'a>(state: ParserState<'a>) -> Parsed<'a, Term> {
    // The head is spanned on its own, and each suffix with its own extent, so `apply_suffixes` can span every node of the chain; the outer `with_span` still covers the whole chain, innermost-wins. `match` and `choose` are heads because `end` closes them: nothing after one can be read as part of it, so an operator, a call, a projection or a `!` may follow one bare.
    with_span(
        with_span(
            parse_goal
                .or(parse_match)
                .or(parse_choose)
                .or(parse_struct_lit)
                .or(parse_qualified_name.map(|n| Subterm::Name(n).into()))
                .or(parse_type)
                .or(parse_prop)
                .or(parse_caller_sort)
                .or(parse_intrinsic)
                .or(parse_tuple_type)
                .or(parse_empty_tuple)
                .or(parse_tuple)
                .or(parse_parens)
                .or(parse_name.map(|n| Subterm::Name(n).into())),
        )
        .and(many0(|| spanned(parse_suffix)))
        .map(|(head, suffixes): (Term, _)| apply_suffixes(head, suffixes)),
    )
    .parse(state)
}

// The fixed set of overloaded infix operators, recognised by maximal munch (two-character symbols before their one-character prefixes).
pub(super) fn parse_infix_symbol<'a>(state: ParserState<'a>) -> Parsed<'a, InfixOp> {
    fn symbol<'a>(text: &'static str, op: InfixOp) -> impl Parser<'a, InfixOp> {
        take_exact(text).map(move |()| op)
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
        .parse(state)
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
pub(super) fn require_space<'a>(state: ParserState<'a>) -> Parsed<'a, ()> {
    take_while(is_blank)
        .flat_map(|spaces| match spaces.is_empty() {
            true => fail("expected whitespace after operator").boxed(),
            false => pure(()).boxed(),
        })
        .and_drop(parse_whitespace)
        .parse(state)
}

// An infix operator with a space on each side, consumed without its operands.
pub(super) fn parse_infix_op<'a>(state: ParserState<'a>) -> Parsed<'a, InfixOp> {
    preceded_by_space()
        .and_keep(parse_infix_symbol)
        .and_drop(require_space)
        .parse(state)
}

// Precedence-climbing over applied atoms: parse a left operand, then fold in every following operator whose precedence is at least `min_prec`. The right operand of an operator at precedence `p` is parsed at `p + 1` (left-associativity).
pub(super) fn parse_infix_expr<'a>(min_prec: u8) -> impl Parser<'a, Term> {
    parse_atomic_term.flat_map(move |left| parse_infix_rest(left, min_prec))
}

pub(super) fn parse_infix_rest<'a>(left: Term, min_prec: u8) -> impl Parser<'a, Term> {
    // One `many0` loop per precedence level, folded by move. Recursing once per operator and deep-cloning the accumulated left spine at every link would cost an N-operator chain N native frame nests and O(N²) cloned nodes — the per-element-recursion class the flat `let` block in `parse_let` avoids. Native depth is bounded by the precedence table's height (each `parse_infix_expr(precedence + 1)` descends one level), never by chain length, and the left operand is cloned zero times.
    many0(move || {
        parse_infix_op.flat_map(move |op| {
            let precedence = op_precedence(op);

            if precedence < min_prec {
                // Binds looser than the caller's level: backtrack, leaving the operator for an enclosing `parse_infix_rest` to consume.
                return fail("operator below current precedence level").boxed();
            }

            parse_infix_expr(precedence + 1)
                .map(move |right| (op, right))
                .boxed()
        })
    })
    .and_drop(refuse_asymmetric_operator)
    .map(move |pairs| {
        pairs.into_iter().fold(left, |left, (op, right)| {
            Subterm::Infix(Infix { op, left, right }).into()
        })
    })
}

// What begins an operand, as far as this probe needs to tell one from punctuation: a name, a literal, a bracket, a plicity mark. `->`, `--` and `<-` all fail it, so a term followed by one keeps its own report.
fn starts_operand(char: char) -> bool {
    char.is_alphanumeric() || matches!(char, '_' | '(' | '[' | '{' | '"' | '\'' | '@' | '/')
}

// An infix symbol glued to an operand on either side is refused by the rule, once the loop above has ended. `parse_infix_op` refuses it with the reason already — `preceded_by_space` and `require_space` each name the missing whitespace — but the loop's arm is caught so the loop can end at a non-operator, a caught failure is discarded, and the enclosing form would then report the token it wanted: `Expected ';', obtained '/'` for `a/ 2`, the spelling the reference warns about, and for the glued `a+b` every other language writes. Probed without consuming, so a symbol spaced on both sides is left for the loop or an enclosing level; only the refusal consumes, reading the symbol raw so the caret underlines it alone and the failure is past the choice point.
fn refuse_asymmetric_operator<'a>(state: ParserState<'a>) -> Parsed<'a, ()> {
    // The term's own parser has consumed the whitespace after it, so the probe stands at the symbol and asks the byte before it, as `parse_infix_op` does.
    look_ahead(
        preceded_by_space()
            .map(|()| true)
            .or(pure(false))
            .and(mark)
            .and(parse_infix_symbol)
            .and(look_ahead(take_while(|char: char| !is_blank(char)))),
    )
    .map(Some)
    .or(pure(None))
    .flat_map(|probed| {
        let Some((((spaced_before, start), op), after)) = probed else {
            return pure(()).boxed();
        };
        let spaced_after = after.is_empty();
        // A character its reader cannot see where the blank belongs: the operator reads as spaced and is not, so the refusal stands at the character and names it.
        if after.chars().next().is_some_and(unseen) {
            return take_exact(op.symbol())
                .and_keep(fail("expected whitespace after operator"))
                .boxed();
        }
        let glued_to_operand = after.chars().next().is_some_and(starts_operand);
        // `a /std/print(1)` is a term followed by an absolute path — what a missing `end` looks like — so a `/` opening one is left to the enclosing form, which names the keyword it wanted.
        let opens_a_path = op == InfixOp::Div && spaced_before && !spaced_after;
        match (
            spaced_before && spaced_after || opens_a_path,
            spaced_after || glued_to_operand,
        ) {
            (false, true) => take_exact(op.symbol())
                .and_keep(fail_from(
                    &start,
                    format!(
                        "an infix operator takes whitespace on both sides — `a {op} b` — and `{op}` glued to an operand is not one: `a/b` is a path and `-1` a signed literal",
                        op = op.symbol()
                    ),
                ))
                .boxed(),
            _ => pure(()).boxed(),
        }
    }).parse(state)
}

/// What refuses a term that begins on the line another has just ended on.
const TERMS_SIDE_BY_SIDE: &str = "two terms stand side by side with nothing that joins them: a call is written `f(a, b)`, its arguments in one parenthesized list, and an operator takes whitespace on both sides";

// Two terms on one line with nothing between them are no form: a call fills a parenthesized list, a path is whitespace-free and an operator is spaced on both sides. What another language writes by juxtaposition — `add 1 2`, `return x`, `if c then a` — therefore has no reading, and the enclosing form would report the token it wanted where the second term stands, `Expected ';', obtained '1'`. Probed once a whole term has been read: where what follows on the same line begins a term — a name, a literal, a list — the refusal is this one, with its caret on that term's first character.
//
// **Committed, because nothing else can be meant.** Two terms side by side are no form wherever they stand, so no alternative has a better reading to offer; and left recoverable, the refusal is dropped wherever a term is one of a list — an argument, a tuple's field — whose closer then complains where the first term began.
//
// **What is left alone.** A term on a later line is a sequence, which the enclosing form words: the `;` an item owes, a program's end. `and` follows a term and begins none where it opens the next member of a `let` group, and is a contextual word the name reader would take. A `/` opening an absolute path is what a missing `end` looks like, which the enclosing form names as [`refuse_asymmetric_operator`] leaves it to. A `(` never reaches here, being a call's own list wherever it stands, and a sign glued to a literal is that probe's too.
//
// **A word another language reserves is named.** `if c then a` and `return x` are two terms side by side to the grammar, and their author wrote neither a call nor an operator: where the word that ends the first term or begins the second is one another language reserves, a line says what Curios writes in its place ([`written_instead_of`]).
fn refuse_juxtaposed_term<'a>(state: ParserState<'a>) -> Parsed<'a, ()> {
    // A zero-width span: the term's own parser has consumed the whitespace after it, so the probe stands where the next token begins and reads back what was skipped.
    spanned(pure(()))
        .flat_map(|(here, ())| {
            let text = &here.source.text;
            let before = &text[..here.start];
            let skipped = &before[before.trim_end_matches(is_blank).len()..];
            let rest = &text[here.start..];
            let word_length = rest
                .chars()
                .take_while(|char| is_identifier_char(*char))
                .map(char::len_utf8)
                .sum::<usize>();
            // What begins a term, told by the token that stands there and by no parse of it: a term read ahead would raise its own committed refusals — a call's unclosed list — in the place of this one.
            let begins_a_term = match &rest[..word_length] {
                "" => rest
                    .chars()
                    .next()
                    .is_some_and(|char| matches!(char, '[' | '"' | '\'')),
                "and" => false,
                word => !is_keyword(word) || matches!(word, "true" | "false" | "match" | "choose"),
            };
            if !begins_a_term || skipped.contains('\n') {
                return pure(()).boxed();
            }

            let first = before.trim_end_matches(is_blank);
            let ending = first
                .chars()
                .rev()
                .take_while(|char| is_identifier_char(*char))
                .map(char::len_utf8)
                .sum::<usize>();
            let foreign = [&first[first.len() - ending..], &rest[..word_length]]
                .into_iter()
                .find_map(|word| written_instead_of(word).map(|form| (word, form)));
            let refusal = match foreign {
                Some((word, form)) => {
                    format!("{TERMS_SIDE_BY_SIDE}\n  `{word}` is no keyword of Curios: {form}")
                }
                None => TERMS_SIDE_BY_SIDE.to_string(),
            };
            mark()
                .and_drop(take_n(1))
                .flat_map(move |start| commit(fail_from(&start, refusal.clone())))
                .boxed()
        })
        .parse(state)
}

/// What Curios writes where another language writes `word`, for a word one reserves and Curios does not; `None` for every other.
///
/// **A note, and no verdict.** It is read where a refusal already stands and adds a line to it. Each of these words is a name like any other, so a program that declares a function under one and calls it compiles as it did.
fn written_instead_of(word: &str) -> Option<&'static str> {
    Some(match word {
        "if" | "then" | "else" => {
            "a choice is a `match` on its condition, `match c | true => a | false => b end`"
        }
        "return" => "a function's body is its result, and nothing returns it",
        "fn" | "def" | "fun" | "function" => {
            "a function is declared with `let`, as in `let double(n: Nat) -> Nat = n + n;`"
        }
        "var" | "const" => {
            "a name is bound once, with `let name = value;`, and is never assigned again"
        }
        "type" | "enum" | "data" | "class" => {
            "a type is declared with `struct` or `induct`, and named with `let Name: Type = …;`"
        }
        "import" => "a module's names are brought into scope with `use /path/{name};`",
        "for" | "while" => {
            "a loop is a recursive function, or a `match` that folds over its scrutinee, `match n | 0 => … | k + 1; ih => … end`"
        }
        _ => return None,
    })
}

pub(crate) fn parse_term<'a>(state: ParserState<'a>) -> Parsed<'a, Term> {
    memoize(MEMO_TERM, term(true)).parse(state)
}

/// [`parse_term`] where the form around the term owes an arrow before the term that follows it: a `choose` arm's condition, a bind arm's value. A term standing where that arrow belongs is the arm's missing `=>`, which the arm reports, so the term is read without [`refuse_juxtaposed_term`]'s probe.
pub(super) fn parse_term_before_arrow<'a>(state: ParserState<'a>) -> Parsed<'a, Term> {
    memoize(MEMO_TERM_BEFORE_ARROW, term(false)).parse(state)
}

/// A term, with the probe for a second one on its line where `probed`.
fn term<'a>(probed: bool) -> impl Parser<'a, Term> {
    let juxtaposed = move || match probed {
        true => refuse_juxtaposed_term.boxed(),
        false => pure(()).boxed(),
    };

    with_span(
        refuse_declaration_head.and_keep(
            // At end of input there is no character for [`refuse_non_term`] to consume, so every alternative below declines at the same offset and the tie-break reports the first of them. This stands ahead of them so that tie names the input instead.
            look_ahead(take_eof)
                .and_keep(fail("expected a term, obtained 'end-of-file'"))
                .or(parse_let)
                .or(parse_func_type)
                .or(parse_func)
                .or(parse_infix_expr(0).and_drop(juxtaposed))
                .or(refuse_non_term),
        ),
    )
}

// The last resort, for a position that begins no term at all. Every alternative above declines at the choice point, and [`Parser::or`]'s tie-break then reports the first of them — `Expected keyword 'let'`, which names the chain's order rather than the reader's mistake. Consuming the offending character puts this failure one past that point, where it outranks them. It stays uncommitted, so a speculative caller still backtracks past it: the trailing comma of `(a, b,)`, whose `parse_term` at the `)` must fail recoverably for the separator to be read as trailing, above all. A word needs none of this — `parse_keyword` consumes the identifier run before refusing it, so a word already fails past the choice point, and the only words that begin no term are the six [`refuse_declaration_head`] names.
fn refuse_non_term<'a>(state: ParserState<'a>) -> Parsed<'a, Term> {
    mark()
        .and(take_n(1))
        .flat_map(|(start, text)| {
            // The span reaches back over the consumed character so the caret underlines it; the failure itself stays past it, which is what outranks the alternatives.
            fail_from(
                &start,
                format!("expected a term, obtained '{}'", shown(text)),
            )
        })
        .parse(state)
}

// The reserved words that begin a declaration and can never begin a term, refused ahead of the alternatives above. Left to them, the local `let` would read the word as its keyword and fail one token in, which wins [`Parser::or`]'s furthest-failure tie-break: a declaration written after the program's tail — after the unannotated top-level `let` that opened it, above all — would report `Expected keyword 'let'`, naming neither the word read nor why an item is refused there. The word is consumed before the failure so it is past the choice point and stays the diagnosis, with the caret after the word as `parse_keyword` places it. `end`, `concept`, `satisfy` and `test` stay out: the first may follow a term and the other three may begin one.
fn refuse_declaration_head<'a>(state: ParserState<'a>) -> Parsed<'a, ()> {
    look_ahead(take_while(is_identifier_char)).flat_map(|head| match head {
        "mod" | "use" | "pub" | "induct" | "struct" | "foreign" => take_while(is_identifier_char)
            .and_keep(fail(format!(
                "'{head}' begins a declaration, not a term; every declaration precedes the program's final term, and an unannotated top-level 'let' opens that term"
            ))).boxed(),
        _ => pure(()).boxed(),
    }).parse(state)
}
