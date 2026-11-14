//! Definitional proof irrelevance: where it fires, where it must not leak, and what a binder's stand-in type decides.

use {
    crate::certifier::convert::test_support::*,
    crate::{test_support::*, *},
    curios_common::fill,
    curios_core::{Free, Term},
};

/// Proof irrelevance: at a `Prop`-sorted type any two terms convert, *without* either being examined. This is what licenses erasure to drop proofs wholesale.
#[test]
fn any_two_inhabitants_of_a_proposition_convert() {
    let mut certifier = certifier();
    let proposition = declare(&mut certifier, "P", "Prop");
    let [p, q] = terms("(p, q)");

    assert_eq!(
        convert(&mut certifier, &term(&proposition), &p, &q),
        Ok(true)
    );
}

/// `${name}(e: ${family}) -> ${result} = match e end`, an elimination with no arm, stuck on a variable.
const STUCK_ON_ITS_ARGUMENT: &str = "let /${name}: (e: ${family}) -> ${result} = (e: ${family}) => #{result = Ambient(${result})} match e end;";

/// **Two applications of one definition are compared by their spines before either is unfolded**, so two proofs passed to it meet irrelevance at the parameter's own type, and neither call is opened to say so. `tests::rules`'s row of the same name is the program.
///
/// The control is the same definition over a relevant family: the spines disagree there, the pair unfolds, and the two eliminations stay apart. The proposition's half holds by either rule — unfolded, the two proofs are a stuck elimination's scrutinees, which the type a lookup gives them equates (`two_neutrals_converge_where_a_lookup_gives_them_a_type_with_one_inhabitant`) — so what this holds is the verdict, and that the spines reach it without unfolding is what the comparison costs less by.
#[test]
fn a_definition_applied_to_two_proofs_converts_before_unfolding() {
    let applied_to_two = |sort: &str| {
        let mut certifier = certifier();
        let family = declare(&mut certifier, "F", sort);
        declared(
            &mut certifier,
            &fill!(
                STUCK_ON_ITS_ARGUMENT,
                name = "h",
                family,
                result = "#NatType"
            ),
        );
        let [left, right] = under(
            &mut certifier,
            &fill!("p: ${family}, q: ${family}", family),
            "/h(p), /h(q)",
        );

        convert(&mut certifier, &term("#NatType"), &left, &right)
    };

    assert_eq!(applied_to_two("Prop"), Ok(true));
    assert_eq!(
        applied_to_two("Type"),
        Ok(false),
        "two relevant arguments were identified",
    );
}

/// The spines of two calls of one definition are compared on the pair as posed, ahead of eta by the goal's type, and through a curried spine: at a function type, at a record type, and for the definition's call applied once more, `h(p)(3)`, whose head is the call. `tests::rules`'s row of the same name is the programs.
///
/// The control for each is the same definition over a relevant family, where the spines disagree and the pair stays apart. Each held goal holds by either rule: left to eta by the type, both calls unfold and the two proofs are a stuck elimination's scrutinees, which the type a lookup gives them equates. So this holds the verdict at each type, which does not follow where the spine rule sits; trying it first, as the elaborator does, is what keeps the two calls folded.
#[test]
fn two_calls_of_one_definition_convert_by_their_spines_at_any_type() {
    let (arrow, record) = ("(x: #NatType) -> #NatType", "{a: #NatType, b: #NatType}");
    let judged = |sort: &str| {
        let mut certifier = certifier();
        let family = declare(&mut certifier, "F", sort);
        for (name, result) in [("h70", arrow), ("h71", record)] {
            declared(
                &mut certifier,
                &fill!(STUCK_ON_ITS_ARGUMENT, name, family, result),
            );
        }
        let [
            into_a_function,
            again_into_a_function,
            into_a_record,
            again_into_a_record,
            applied,
            again_applied,
        ] = under(
            &mut certifier,
            &fill!("p: ${family}, q: ${family}", family),
            "/h70(p), /h70(q), /h71(p), /h71(q), /h70(p)(3), /h70(q)(3)",
        );

        [
            convert(
                &mut certifier,
                &term(arrow),
                &into_a_function,
                &again_into_a_function,
            ),
            convert(
                &mut certifier,
                &term(record),
                &into_a_record,
                &again_into_a_record,
            ),
            convert(&mut certifier, &term("#NatType"), &applied, &again_applied),
        ]
    };

    assert_eq!(judged("Prop"), [Ok(true), Ok(true), Ok(true)]);
    assert_eq!(judged("Type"), [Ok(false), Ok(false), Ok(false)]);
}

/// A spine's arguments are compared at the types its head assigns under every head a lookup types: past a curried head, `f(0)(p)`, and past a projected one, `r.0(p)`, as past a variable's own.
///
/// The control is the same two spines over a relevant family. Two proofs of one proposition converge compared at `Type` too, by the type a lookup gives each, so this holds the verdict under each head, and the head's telescope is what reaches it without a lookup of every argument.
#[test]
fn a_spines_arguments_are_typed_under_a_curried_and_a_projected_head() {
    let judged = |sort: &str| {
        let mut certifier = certifier();
        let family = declare(&mut certifier, "F", sort);
        // `f : (Nat) -> (F) -> Nat` and `r : {(F) -> Nat, Nat}`.
        let [curried, again_curried, projected, again_projected] = under(
            &mut certifier,
            &fill!(
                "p: ${family}, q: ${family}, f: (n: #NatType) -> (e: ${family}) -> #NatType, r: {a: (e: ${family}) -> #NatType, b: #NatType}",
                family
            ),
            "f(0)(p), f(0)(q), r.0(p), r.0(q)",
        );
        let nat = term("#NatType");

        [
            convert(&mut certifier, &nat, &curried, &again_curried),
            convert(&mut certifier, &nat, &projected, &again_projected),
        ]
    };

    assert_eq!(judged("Prop"), [Ok(true), Ok(true)]);
    assert_eq!(judged("Type"), [Ok(false), Ok(false)]);
}

/// `#{result = Ambient(#NatType)} match ${scrutinee} end`, an elimination with no arm at `Nat`.
const AT_A_NUMBER: &str = "#{result = Ambient(#NatType)} match ${scrutinee} end";

/// Where a child is compared with no type, what a type directs between two neutrals is read off the type a lookup gives both: two proofs of one proposition converge at `Type`, and so do two variables at the empty record, at a record of units, at a function into a unit, at a struct that declares no field, at a struct of a unit and at a recursive definition's call that unfolds to one of these, at whatever depth; and two eliminations of two such proofs, whose scrutinees are compared with no type, are one term. It is what keeps a verdict from following whether a definition's call was unfolded before the pair was posed. The elaborator's twin of this proposition shares the name.
///
/// Neither side is expanded, and the refusals say what that keeps apart: two variables at a record with a relevant field, two proofs of two propositions and two numbers.
///
/// Mutation-checked: without the lookup the nine held goals are refused; and with the two looked-up types left uncompared, two proofs of two propositions converge. Eta taken at the looked-up type in place of reading it is no mutation that returns: the projections' heads ask the pair again, without end.
#[test]
fn two_neutrals_converge_where_a_lookup_gives_them_a_type_with_one_inhabitant() {
    let mut certifier = certifier();
    let proposition = declare(&mut certifier, "P", "Prop");
    let another = declare(&mut certifier, "Q", "Prop");
    declared(
        &mut certifier,
        "struct /U: pub Type {}\nstruct /W: pub Type {\n    u: {},\n}",
    );
    // `rec F : (Nat) -> Type = (n) => match n | 0 => {} | pred + 1 => {a: F(pred)}; F`. `F(0)` unfolds to the unit, and `F(1)` to a record of it.
    let nested = "#{node = Rec} (let F: (n: #NatType) -> Type = (n: #NatType) => match n: (m) => Type | 0 => {} | pred + 1; ih => {a: F(pred)} end; F)";
    let hypotheses = fill!(
        "p: ${proposition}, q: ${proposition}, r: ${another}, u: {}, v: {}, s: #{node = StructType} /U(), t: #{node = StructType} /U(), x: {a: #NatType, b: {}}, y: {a: #NatType, b: {}}, f: (x: #NatType) -> {}, g: (x: #NatType) -> {}, a: #NatType, b: #NatType, c: {a: {}, b: {}}, d: {a: {}, b: {}}, w: #{node = StructType} /W(), z: #{node = StructType} /W(), h: (${nested})(0), k: (${nested})(0), i: (${nested})(1), j: (${nested})(1)",
        proposition,
        another,
        nested
    );
    let [
        p,
        q,
        other,
        u,
        v,
        s,
        t,
        x,
        y,
        f,
        g,
        a,
        b,
        c,
        d,
        w,
        z,
        h,
        k,
        i,
        j,
        eliminated_p,
        eliminated_q,
    ] = under(
        &mut certifier,
        &hypotheses,
        &fill!(
            "p, q, r, u, v, s, t, x, y, f, g, a, b, c, d, w, z, h, k, i, j, ${p}, ${q}",
            p = fill!(AT_A_NUMBER, scrutinee = "p"),
            q = fill!(AT_A_NUMBER, scrutinee = "q")
        ),
    );
    let (ground, number) = (term("Type"), term("#NatType"));
    let mut at =
        |type_: &Term, this: &Term, that: &Term| convert(&mut certifier, type_, this, that);

    assert_eq!(
        [
            at(&ground, &p, &q),
            at(&ground, &u, &v),
            at(&ground, &s, &t),
            at(&ground, &c, &d),
            at(&ground, &f, &g),
            at(&ground, &w, &z),
            at(&ground, &h, &k),
            at(&ground, &i, &j),
            at(&number, &eliminated_p, &eliminated_q),
        ],
        [
            Ok(true),
            Ok(true),
            Ok(true),
            Ok(true),
            Ok(true),
            Ok(true),
            Ok(true),
            Ok(true),
            Ok(true)
        ]
    );
    assert_eq!(
        [
            at(&ground, &x, &y),
            at(&ground, &p, &other),
            at(&ground, &a, &b),
        ],
        [Ok(false), Ok(false), Ok(false)]
    );
}

/// A stuck elimination is typed by its result at its scrutinee: an ambient goal as written, and a family opened at the indices its scrutinee's type carries and then at the scrutinee. So where a child is compared with no type, an elimination at a proposition converges with a variable at it and with an elimination of another scrutinee, one at a record of units with a variable at it, and an indexed family's elimination is read at its scrutinee's index. The elaborator's twin of this proposition shares the name.
///
/// The refusals are what the type does not decide: an elimination at a relevant type against a variable at it, and one at a proposition against a variable at another.
///
/// Mutation-checked: with no type read off an elimination the four held goals are refused, and with a family read at its scrutinee alone the indexed one is.
#[test]
fn a_stuck_elimination_is_typed_by_its_result_at_its_scrutinee() {
    let mut certifier = certifier();
    let proposition = declare(&mut certifier, "P", "Prop");
    let another = declare(&mut certifier, "Q", "Prop");
    let empty = declare(&mut certifier, "E", "Type");
    // `induct Ix : (n : Nat) -> Type`, a family with one index and no constructor.
    declared(&mut certifier, "induct /Ix: (n: #NatType) -> pub Type\nend");
    let units = "{a: {}, b: {}}";
    let eliminated = |scrutinee: &str, goal: &str| {
        fill!(
            "#{result = Ambient(${goal})} match ${scrutinee} end",
            scrutinee,
            goal
        )
    };
    let [
        c_at_p,
        p,
        d_at_p,
        c_at_units,
        r,
        over_its_index,
        c_at_a_number,
        n,
        q,
    ] = under(
        &mut certifier,
        &fill!(
            "c: ${empty}, d: ${empty}, p: ${proposition}, q: ${another}, r: ${units}, n: #NatType, s: #{node = InductType} /Ix()(3)",
            empty,
            proposition,
            another,
            units
        ),
        &[
            eliminated("c", &proposition),
            "p".to_string(),
            eliminated("d", &proposition),
            eliminated("c", units),
            "r".to_string(),
            // `match s : (i, x) => P end`, whose result is a family over the index and the scrutinee.
            fill!("match s: (i, x) => ${proposition} end", proposition),
            eliminated("c", "#NatType"),
            "n".to_string(),
            "q".to_string(),
        ]
        .join(", "),
    );
    let ground = term("Type");
    let mut at = |this: &Term, that: &Term| convert(&mut certifier, &ground, this, that);

    assert_eq!(
        [
            at(&c_at_p, &p),
            at(&c_at_p, &d_at_p),
            at(&c_at_units, &r),
            at(&over_its_index, &p),
        ],
        [Ok(true), Ok(true), Ok(true), Ok(true)]
    );
    assert_eq!(
        [at(&c_at_a_number, &n), at(&c_at_p, &q)],
        [Ok(false), Ok(false)]
    );
}

/// Two eliminations that state the instance they eliminate are compared by it: the two instances, and then the scrutinees at the instance, a typed position where two proofs are one unread and no lookup types either. Two variables nothing was assumed of converge there, and stay apart where the eliminations state no instance, where the two instances differ, and where one states its instance and the other none. The elaborator's twin of this proposition shares the name.
///
/// Mutation-checked: with the scrutinees compared at `Type` whatever the eliminations state, the held goal is refused.
#[test]
fn two_eliminations_are_compared_at_the_instance_they_state() {
    let mut certifier = certifier();
    let proposition = declare(&mut certifier, "P", "Prop");
    let another = declare(&mut certifier, "Q", "Prop");
    let stating = |proof: &str, instance: &str| {
        fill!(
            "#{result = Ambient(#NatType), cases = {stated = ${instance}}} match ${proof} end",
            proof,
            instance
        )
    };
    let [p_at_p, q_at_p, q_at_q, p_bare, q_bare] = terms(&fill!(
        "(${p_at_p}, ${q_at_p}, ${q_at_q}, ${p_bare}, ${q_bare})",
        p_at_p = stating("p", &proposition),
        q_at_p = stating("q", &proposition),
        q_at_q = stating("q", &another),
        p_bare = fill!(AT_A_NUMBER, scrutinee = "p"),
        q_bare = fill!(AT_A_NUMBER, scrutinee = "q")
    ));
    let number = term("#NatType");
    let mut at = |this: &Term, that: &Term| convert(&mut certifier, &number, this, that);

    assert_eq!(at(&p_at_p, &q_at_p), Ok(true));
    assert_eq!(
        [
            at(&p_bare, &q_bare),
            at(&p_at_p, &q_at_q),
            at(&p_at_p, &p_bare),
        ],
        [Ok(false), Ok(false), Ok(false)]
    );
}

/// `Or : Prop | left() | right()`, the same two at `Type`, and `Zero : (n : Nat) -> Prop | zero() : (0)`.
const NULLARY: &str = r#"
induct /Or: pub Prop
| left()
| right()
end
induct /Two: pub Type
| left()
| right()
end
induct /Zero: (n: #NatType) -> pub Prop
| zero(): (0)
end
"#;

/// A constructor's value is typed by its declaration: the family at the value's parameters and at the index targets its constructor states. So where a child is compared with no type, a proof a constructor builds converges with a proof a variable names and with one another constructor builds. The elaborator's twin of this proposition shares the name.
///
/// The refusals: two constructors of a relevant family, a constructor's value against a variable at it, and a proof against a variable at the family's other index.
///
/// Mutation-checked: with no type read off a constructor's value the three held goals are refused, and with the index targets left out the indexed one is.
#[test]
fn a_constructors_value_is_typed_by_its_declaration() {
    let mut certifier = certifier();
    declared(&mut certifier, NULLARY);
    let [
        left,
        right,
        zero,
        two_left,
        two_right,
        p,
        t,
        at_zero,
        at_one,
    ] = under(
        &mut certifier,
        "p: #{node = InductType} /Or(), t: #{node = InductType} /Two(), z: #{node = InductType} /Zero()(0), o: #{node = InductType} /Zero()(1)",
        "#{node = Variant} /Or/left(), #{node = Variant} /Or/right(), #{node = Variant} /Zero/zero(), #{node = Variant} /Two/left(), #{node = Variant} /Two/right(), p, t, z, o",
    );
    let ground = term("Type");
    let mut at = |this: &Term, that: &Term| convert(&mut certifier, &ground, this, that);

    assert_eq!(
        [at(&left, &p), at(&left, &right), at(&zero, &at_zero)],
        [Ok(true), Ok(true), Ok(true)]
    );
    assert_eq!(
        [
            at(&two_left, &two_right),
            at(&two_left, &t),
            at(&zero, &at_one),
        ],
        [Ok(false), Ok(false), Ok(false)]
    );
}

/// An arm's binders are opened at the types its position gives them: a constructor's arm at its telescope over the scrutinee's parameters, and a cons arm at its carrier's domains. Two eliminations whose arms differ in which of two bound proofs they hand on are one term, as are two folds over a list of proofs, one handing on the head it peeled and the other a proof in scope. The elaborator's twin of this proposition shares the name.
///
/// The control for each is the same pair over a relevant family, where the two binders stay apart; and where no lookup types the scrutinee the arm keeps the stand-in, under which the first pair stays apart too.
///
/// Mutation-checked: with every arm at the stand-in the two held goals are refused.
#[test]
fn an_arms_binders_are_opened_at_the_types_their_constructor_gives_them() {
    let judged = |sort: &str| {
        let mut certifier = certifier();
        let held = declare(&mut certifier, "F", sort);
        // `induct Pair : Type | two(a : F, b : F)`.
        declared(
            &mut certifier,
            &fill!(
                "induct /Pair: pub Type\n| two(a: ${held}, b: ${held})\nend",
                held
            ),
        );
        let result = fill!("{x: ${held}, y: #NatType}", held);
        // `match t | two(a, b) => (<chosen>, 1) end`, at the ambient `{F, Nat}`.
        let handing_on = |scrutinee: &str, chosen: &str| {
            fill!(
                "match ${scrutinee}: (m) => ${result} | two(a, b) => (${chosen}, 1) end",
                scrutinee,
                result,
                chosen
            )
        };
        // `match l | [] => (p, 0) | h ++ t; ih => (<chosen>, 1) end`.
        let folding = |chosen: &str| {
            fill!(
                "#{cases = {carrier = {elem = ${held}}}} match l: (m) => ${result} | [] => (p, 0) | [h, ..t]; ih => (${chosen}, 1) end",
                held,
                result,
                chosen
            )
        };
        // `u` is the scrutinee no lookup types: a word no hypothesis binds.
        let [
            typed_first,
            typed_second,
            peeled,
            in_scope,
            untyped_first,
            untyped_second,
        ] = under(
            &mut certifier,
            &fill!(
                "t: #{node = InductType} /Pair(), p: ${held}, l: #ListType(${held})",
                held
            ),
            &[
                handing_on("t", "a"),
                handing_on("t", "b"),
                folding("h"),
                folding("p"),
                handing_on("u", "a"),
                handing_on("u", "b"),
            ]
            .join(", "),
        );
        let ground = term("Type");

        [
            convert(&mut certifier, &ground, &typed_first, &typed_second),
            convert(&mut certifier, &ground, &peeled, &in_scope),
            convert(&mut certifier, &ground, &untyped_first, &untyped_second),
        ]
    };

    assert_eq!(judged("Prop"), [Ok(true), Ok(true), Ok(false)]);
    assert_eq!(judged("Type"), [Ok(false), Ok(false), Ok(false)]);
}

/// The same two terms at a *relevant* type are not interchangeable. Irrelevance is a property of the type, and this is the direction that would be unsound to get wrong.
#[test]
fn does_not_leak_into_a_relevant_type() {
    let mut certifier = certifier();
    let data = declare(&mut certifier, "D", "Type");
    let [p, q] = terms("(p, q)");

    assert_eq!(convert(&mut certifier, &term(&data), &p, &q), Ok(false));
}

/// `S(c)`, the propositions at `true` and the types at `false`, and `F(c) : S(c)`, `P` at `true` and `Nat` at `false`.
const SORT_COMPUTED: &str = r#"
let /S: (c: #BoolType) -> #{level = 1} Type =
    (c: #BoolType) => match c: (_) => #{level = 1} Type | false => Type | true => Prop end;
let /F: (c: #BoolType) -> /S(c) =
    (c: #BoolType) => match c: (d) => /S(d) | false => #NatType | true => ${proposition} end;
"#;

/// A declaration whose codomain is a sort its argument computes lands in `Prop` where the argument decides one and nowhere else. `F(c) : S(c)`, with `S(true)` the propositions and `S(false)` the types, is known for a proposition at `true` by its declaration, before `F(true)` is reduced, and at `false` its two values stay apart: what the declaration says of an application is read at the arguments the application states. `curios`'s `tests::rules::subsumption_tests` holds the program under this name, where a binder at `F(c)` with `c` open is no type at all.
#[test]
fn a_type_whose_sort_its_argument_computes_is_a_proposition_only_where_the_argument_decides_one() {
    let mut certifier = certifier();
    let proposition = declare(&mut certifier, "P", "Prop");
    declared(&mut certifier, &fill!(SORT_COMPUTED, proposition));
    let [p, q] = under(&mut certifier, "p: /F(true), q: /F(true)", "p, q");

    assert_eq!(
        convert(&mut certifier, &term("/F(true)"), &p, &q),
        Ok(true),
        "two proofs at the argument that decides a proposition stayed apart",
    );
    assert_eq!(
        convert(&mut certifier, &term("/F(false)"), &term("0"), &term("1")),
        Ok(false),
        "two numbers were identified at the argument that decides a type",
    );
}

/// A struct literal's fields compare at the declaration's field telescope, so a field at a proposition is discharged without being read: two literals differing only in a proof are one value. Comparing every field at `Type` would refuse it, and `Str/concat` would associate for the elaborator and not for the certifier.
#[test]
fn a_struct_field_at_a_proposition_is_not_read() {
    let mut certifier = certifier();
    let proposition = declare(&mut certifier, "P", "Prop");
    declared(
        &mut certifier,
        &fill!(
            "struct /Wrap: pub Type {\n    n: #NatType,\n    p: ${proposition},\n}",
            proposition
        ),
    );
    let [this, that, other] = under(
        &mut certifier,
        &fill!("p: ${proposition}, q: ${proposition}", proposition),
        "/Wrap { 1, p }, /Wrap { 1, q }, /Wrap { 2, p }",
    );
    let ground = term("Type");

    assert_eq!(
        convert(&mut certifier, &ground, &this, &that),
        Ok(true),
        "two literals differing only in a proof field did not convert",
    );

    // The control: a relevant field is still compared, so irrelevance did not leak past the proposition.
    assert_eq!(
        convert(&mut certifier, &ground, &this, &other),
        Ok(false),
        "a relevant field stopped being compared",
    );
}

/// The constructor twin: a payload compares at the constructor's telescope, so a payload at a proposition is discharged the same way.
#[test]
fn a_constructor_payload_at_a_proposition_is_not_read() {
    let mut certifier = certifier();
    let proposition = declare(&mut certifier, "P", "Prop");
    declared(
        &mut certifier,
        &fill!(
            "induct /Wrap: pub Type\n| wrap(n: #NatType, p: ${proposition})\nend",
            proposition
        ),
    );
    let [this, that, other] = under(
        &mut certifier,
        &fill!("p: ${proposition}, q: ${proposition}", proposition),
        "#{node = Variant} /Wrap/wrap(1, p), #{node = Variant} /Wrap/wrap(1, q), #{node = Variant} /Wrap/wrap(2, p)",
    );
    let ground = term("Type");

    assert_eq!(
        convert(&mut certifier, &ground, &this, &that),
        Ok(true),
        "two constructions differing only in a proof payload did not convert",
    );
    assert_eq!(
        convert(&mut certifier, &ground, &this, &other),
        Ok(false),
        "a relevant payload stopped being compared",
    );
}

/// Two inhabitants of `match b | false => ${false} | true => ${true} end` at `${sort}`, its motive's sort, over a `Bool` `b`: the type, and the two.
fn inhabitants_of_a_computed_type(
    certifier: &mut Certifier,
    sort: &str,
    if_false: &str,
    if_true: &str,
) -> [Term; 3] {
    let computed = fill!(
        "match b: (_) => ${sort} | false => ${if_false} | true => ${if_true} end",
        sort,
        if_false,
        if_true
    );

    under(
        certifier,
        &fill!("b: #BoolType, p: ${computed}, q: ${computed}", computed),
        &fill!("${computed}, p, q", computed),
    )
}

/// Definitional proof irrelevance at a *computed* proposition — the shape every real firing takes.
///
/// `any_two_inhabitants_of_a_proposition_convert` above uses a nominal `Prop`-sorted family, which is the easy rung: the registry says outright that the type is a proposition. The elaborator's firings are at types that only *compute* to one — a stuck `match` at motive `Prop`, which is how `/std/Cli`'s `Clear` states that a command line has no problems. Here the rule is inert over every program, because a proof reaching conversion in this crate does so in an untyped child position compared at `Type`; so nothing in any program brings the rule and the shape together, and only a fixture can.
///
/// The shape is worth its own fixture because of what irrelevance trusts. It accepts *without inspecting either term*, and `Sort::of` classifies a stuck `match` by its **motive** rather than by its arms — a claim the term makes about itself, which is exactly why `check_motive` exists to type a motive under its real binders before any rule reads it. At a computed proposition the motive is therefore the whole of what this rule rests on.
///
/// The control is the identical construction with the motive at `Type` and relevant arms in both, which must *not* converge. It separates "reads the motive" from "accepts every stuck match", and without it a rule that skipped the sort test entirely would satisfy the witness.
#[test]
fn fires_at_a_computed_proposition() {
    let mut certifier = certifier();
    let held = declare(&mut certifier, "Held", "Prop");
    let empty = declare(&mut certifier, "Empty", "Prop");
    let [computed, left, right] =
        inhabitants_of_a_computed_type(&mut certifier, "Prop", &empty, &held);

    assert_eq!(
        convert(&mut certifier, &computed, &left, &right),
        Ok(true),
        "two inhabitants of a proposition the motive computes did not convert",
    );
}

/// The control for the fixture above: the same stuck `match`, with the motive at `Type` and relevant arms, must still distinguish its inhabitants.
#[test]
fn does_not_fire_at_a_computed_relevant_type() {
    let mut certifier = certifier();
    let one = declare(&mut certifier, "One", "Type");
    let two = declare(&mut certifier, "Two", "Type");
    let [computed, left, right] =
        inhabitants_of_a_computed_type(&mut certifier, "Type", &one, &two);

    assert_eq!(
        convert(&mut certifier, &computed, &left, &right),
        Ok(false),
        "irrelevance leaked into a relevant type the motive computes",
    );
}

/// **The stand-in a binder is opened at where no lookup types its elimination's scrutinee, held against the types such a binder could really carry.**
///
/// A motive's and an arm's binders are opened at the types their position gives them, read off the scrutinee's looked-up type. Where it has none, `ground_arm` and `motive_binders` open them at one shared set of binders and assume every one of them at `Type`, whatever it really is. The recorded type is not inert: [`synth_neutral`](crate::certifier::sort::synth_neutral) reads the same recorded type through `Certifier::type_of`, so it reaches `Sort::of`, and `Sort::of` is what [`compare`] asks before *every* goal: the proof-irrelevance test at the top of the rule.
///
/// What actually holds the stand-in up is narrower, and is about the value rather than about the readers: `Type` is the least informative answer `Sort::of` can return for a binder. Irrelevance fires on `Sort::Prop(_)` and on nothing else, and eta dispatches on the goal type's own *shape* rather than on the binder's, so a binder recorded at `Type` can only lose the accepting rules, never gain one. This walks one goal at each type the binder could really carry and records what each decides.
///
/// The grid is two side-pairs against four assumed types, because a single pair cannot separate the two things being asked. Distinct sides expose which types *discharge* the goal without comparing — only `Prop` does — and convertible-but-not-identical sides expose which types get as far as comparing at all. The stand-in's row matches the relevant-sort row in both, which is the null: it decides every goal the way a real relevant type decides it.
///
/// **One row is not a forfeiture, and it is the one to carry forward.** A binder whose real type is not a sort at all leaves `Sort::of` with nothing to decode, and the typed opening refuses the whole certification with `NotASort` — while the stand-in classifies it `Type 0` and goes on to accept. There the stand-in is strictly *more* permissive than the truth. Nothing in the stand-in fences that off; what does is a property of its callers, the same shape as the one `struct_eta`'s neutral restriction rests on. A match motive is typed under its real binders by `infer`'s `check_motive` before any comparison grounds it, so a motive using a `Bool`-typed binder as a type never reaches here. Under the typed opening the row does not arise: the binder carries its real type, and the goal is refused as the typed row is.
#[test]
fn a_binders_stand_in_type_decides_a_goal_the_way_a_relevant_type_does() {
    // Two words nothing is assumed of, and two sides convertible without being syntactically equal, so the goal survives `compare`'s reflexivity fast path and has to be decided by a rule.
    let (distinct, convertible) = ("u, v", "((x: #NatType) => x)(1), 1");

    let at = |assumed: &str, sides: &str| {
        let mut certifier = certifier();
        let [hypothesis, this, that] = under(
            &mut certifier,
            &fill!("h: ${assumed}", assumed),
            &fill!("h, ${sides}", sides),
        );

        convert(&mut certifier, &hypothesis, &this, &that)
    };

    let (relevant, stand_in) = ("#{level = 3} Type", "Type");
    let not_a_sort = || Err(Error::NotASort(term("#NatType")));

    // Distinct sides: only a proposition discharges them, and the stand-in is not one.
    assert_eq!(
        at("Prop", distinct),
        Ok(true),
        "a hypothesis really at `Prop` stopped licensing irrelevance",
    );
    assert_eq!(
        at(relevant, distinct),
        Ok(false),
        "a hypothesis at a relevant sort discharged two distinct inhabitants",
    );
    assert_eq!(
        at(stand_in, distinct),
        Ok(false),
        "the stand-in discharged a goal a relevant type refuses",
    );
    assert_eq!(
        at("#NatType", distinct),
        not_a_sort(),
        "a hypothesis at a non-sort was classified rather than refused",
    );

    // Convertible sides: every sort compares and accepts, and the non-sort still refuses before comparing.
    assert_eq!(
        at("Prop", convertible),
        Ok(true),
        "a proposition stopped discharging its inhabitants",
    );
    assert_eq!(
        at(relevant, convertible),
        Ok(true),
        "a relevant sort refused two convertible terms",
    );
    assert_eq!(
        at(stand_in, convertible),
        Ok(true),
        "the stand-in refused a goal a relevant type accepts",
    );
    assert_eq!(
        at("#NatType", convertible),
        not_a_sort(),
        "the non-sort row stopped being the one place the stand-in is the more permissive of the two",
    );
}

/// `Wit(P : Prop) : (p : P)`, a family whose index type *is* its own parameter, `Q`, a proposition, and `Ix : (R : Prop) -> Type`, a family indexed by a proposition.
///
/// `induct_type_args` compares two index actuals at the type the declaration's telescope assigns, opened at the left instance's preceding actuals, so at `Wit` the index pair is compared at whatever term stands in the parameter position. That makes the parameter's assumed type the whole of what decides the index goal, which is exactly the question a binder opened at a stand-in answers with the stand-in.
const INDEXED_BY_A_PROPOSITION: &str = r#"
induct /Wit(P: Prop): (p: P) -> pub Type
end
induct /Q: pub Prop
end
induct /Ix: (R: Prop) -> pub Type
end
"#;

/// A motive's binders are opened at the types its family gives them: the index domains, and the scrutinee at the family over those binders.
///
/// Two stuck eliminations of one scrutinee at a family indexed by a proposition, differing only inside their motives. Each motive body is `Wit(<index binder>, i)`, and `Wit`'s index type is its own parameter, so the index pair is compared at the motive's index binder — which is opened at `Prop`, its real type, and is a proposition there: the pair is discharged by irrelevance, as it is where the motive was typed.
///
/// The fallback is the second half: over a scrutinee no lookup types, the binders are opened at the stand-in, the index pair is compared at a binder recorded at `Type`, and the two stay apart, which is the grid's stand-in row above. The control beside each is a motive pair that differs only by a beta redex, which converges under either opening.
///
/// The counterfactual is the last: the name the fallback's binder was opened under, assumed at `Prop` once that binder is closed, is a proposition, and the two bodies compared directly are discharged by irrelevance. So the verdict moves with the binder's type, toward refusal at the stand-in; and a sort read under the closed binder does not answer for the name assumed again.
///
/// The locals are written by the identities they are minted at, which is what the reasoning reads.
///
/// Mutation-checked: with every motive binder at the stand-in the first typed goal is refused; and with a remembered sort answering without asking whether its binders stand, the sort the fallback read under its closed binder answers for the name opened again, and a goal after it is refused.
#[test]
fn a_motives_binders_are_opened_at_the_types_its_family_gives_them() {
    let mut certifier = certifier();
    declared(&mut certifier, INDEXED_BY_A_PROPOSITION);
    let local = |index: u32, word: &str| fill!("#{binding = Free(${index})} ${word}", index, word);
    // The names the certifier is not handed sit below the one it is, so no binder it mints meets one of them.
    certifier.assume(
        &Free::local(80, Some("s")),
        &term("#{node = InductType} /Ix()(#{node = InductType} /Q())"),
    );
    let (typed, untyped, u, v) = (
        local(80, "s"),
        local(70, "t"),
        local(71, "u"),
        local(72, "v"),
    );

    // `match s : (R, x) => Wit(R, <index>) end`.
    let elimination = |scrutinee: &str, index: &str| {
        term(&fill!(
            "match ${scrutinee}: (R, x) => #{node = InductType} /Wit(R)(${index}) end",
            scrutinee,
            index
        ))
    };
    let redex = |name: &str| fill!("((x: #NatType) => x)(${name})", name);
    let ground = term("Type");
    let mut at = |scrutinee: &str, this: &str, that: &str| {
        convert(
            &mut certifier,
            &ground,
            &elimination(scrutinee, this),
            &elimination(scrutinee, that),
        )
    };

    // The fallback's goals are put first, so that the first binder the certifier mints — the name after the scrutinee's — is the motive's, opened at the stand-in.
    let fallback = [at(&untyped, &u, &v), at(&untyped, &u, &redex(&u))];
    assert_eq!(
        [at(&typed, &u, &v), at(&typed, &u, &redex(&u))],
        [Ok(true), Ok(true)]
    );
    assert_eq!(fallback, [Ok(false), Ok(true)]);

    // That binder is closed, and its name is handed in again at `Prop`.
    certifier.assume(&Free::local(81, Some("R")), &term("Prop"));
    let body = |index: &str| {
        term(&fill!(
            "#{node = InductType} /Wit(${carried})(${index})",
            carried = local(81, "R"),
            index
        ))
    };

    assert_eq!(
        convert(&mut certifier, &ground, &body(&u), &body(&v)),
        Ok(true)
    );
}

/// A struct's *parameters* compare at the declaration's outer telescope too, so a parameter at a proposition is discharged without being read.
///
/// It is the parameter-side twin of the field rule above: a family's indices, a struct's parameters and a constructor's are all compared at their declared types, so `Wrap(P, p)` and `Wrap(P, q)` are one type exactly as two instances of a family indexed by a proof are. Nothing in `/std` forces it; it holds because the rule is the same rule.
#[test]
fn a_struct_parameter_at_a_proposition_is_not_read() {
    let mut certifier = certifier();
    let proposition = declare(&mut certifier, "P", "Prop");
    // One parameter at the proposition, and a field that does not mention it.
    declared(
        &mut certifier,
        &fill!(
            "#{polarities = [Unused]}\nstruct /Held(p: ${proposition}): pub Type {\n    n: #NatType,\n}",
            proposition
        ),
    );
    let [this, that] = under(
        &mut certifier,
        &fill!("p: ${proposition}, q: ${proposition}", proposition),
        "#{node = StructType} /Held(p), #{node = StructType} /Held(q)",
    );

    assert_eq!(
        convert(&mut certifier, &term("Type"), &this, &that),
        Ok(true),
        "two instances differing only in a proof parameter did not convert",
    );
}
