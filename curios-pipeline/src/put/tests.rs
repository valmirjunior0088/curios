//! A closing term put at `False`, through the real checkers, over the prelude: a program to both, Core text to the certifier alone, and a subject that does not read put to neither.

use {
    super::*, curios_common::Source, curios_elab::DEFAULT_BUDGET, curios_module::SYNTAX,
    curios_prelude::with_prelude,
};

/// A recursion through a callback handed past a member's one parameter to the function the member answers: the program witness of a fixed ticket, which the elaborator refuses by an erasure obligation it reports, and the certifier by its own walk over what the elaborator built.
const HANDED_PAST: &str = "use /std/{Nat};
use /std/Bool/{False};

let hide(n: Nat, f: (Nat) -> False) -> False =
    match n | 0 => wrap(n)(f) | k + 1 => f(k) end
and wrap(m: Nat) -> ((Nat) -> False) -> False =
    match m | 0 => (g) => g(0) | k + 1 => (g) => hide(k, g) end;

let spin(n: Nat) -> False =
    hide(n, (c) => spin(c));

spin(0)
";

/// A recursion graded below its argument by what a payload returns of its scrutinee: the program witness of a fixed ticket, which the elaborator refuses while it builds the module.
const PAYLOAD_RETURNS: &str = "use /std/Bool/{False};

induct Bad: Prop
| c(f: (A: Prop) -> (A) -> A)
end

let bad(b: Bad) -> False =
    match b | c(f) => bad(f(Bad)(b)) end;

bad(Bad/c((A) => (x) => x))
";

/// A recursion handing its own proof back below the index its target refined: the Core text witness of a fixed ticket, which the certifier refuses.
const REFINED_INDEX: &str = "induct /Wit: pub Prop
| base()
| w(inner: #{node = InductType} /Wit())
end

induct /Q: (i: #{node = InductType} /Wit()) -> pub Prop
| c(v: #{node = InductType} /Wit()): (#{node = Variant} /Wit/w(v))
end

#{node = Rec}
(let f: (w0: #{node = InductType} /Wit(), q: #{node = InductType} /Q()(w0)) -> /sys/Bool/False =
    (w02: #{node = InductType} /Wit(), q2: #{node = InductType} /Q()(w02)) =>
        #{goal = /sys/Bool/False} match q2 | c(v) => f(v, q2) end;
f(#{node = Variant} /Wit/base(), #{node = Variant} /Q/c(#{node = Variant} /Wit/base())))
";

/// The program `text` put at `False` over the prelude.
fn program(text: &str) -> Result<Put, CompileError> {
    let entrypoint = text.parse::<Entrypoint>().expect("the program parses");
    with_prelude(|prelude| {
        put_program(
            DEFAULT_BUDGET,
            Predecessors::over(prelude),
            &SYNTAX,
            &entrypoint,
            &RootSource::none(),
        )
    })
}

/// The Core text `text` put at `False` over the prelude.
fn core_text(text: &str) -> Result<Option<Put>, CompileError> {
    with_prelude(|prelude| {
        put_core_text(
            DEFAULT_BUDGET,
            Predecessors::over(prelude),
            &SYNTAX,
            &Source::inline(text),
        )
    })
}

/// A program both checkers refuse names each refusal by its checker's own error, the elaborator's beside the certifier's over the module it built.
#[test]
fn a_program_both_checkers_refuse_names_each_refusal() {
    let put = program(HANDED_PAST).expect("the program lowers");
    let said = put.to_string();

    assert_eq!(put.said(Checker::Elaborator), "refused");
    assert_eq!(put.said(Checker::Certifier), "refused");
    assert!(
        said.starts_with("refused at `False`\n")
            && said.contains("└─ [refused] by elaborator `PartialInErasedPosition`"),
        "{said}"
    );
    assert!(
        said.contains("certifier `NotDescending` in `/spin`"),
        "{said}"
    );
}

/// A program the elaborator refuses while it builds the module reaches no certifier, and says so.
#[test]
fn a_program_refused_while_it_is_built_reaches_no_certifier() {
    let put = program(PAYLOAD_RETURNS).expect("the program lowers");

    assert_eq!(put.said(Checker::Elaborator), "refused");
    assert_eq!(put.said(Checker::Certifier), "not asked");
    assert!(
        put.to_string()
            .contains("└─ [refused] by elaborator `UniverseInconsistency` in `/bad`"),
        "{put}"
    );
}

/// Core text is put to the certifier alone, which names its refusal.
#[test]
fn core_text_is_put_to_the_certifier_alone() {
    let put = core_text(REFINED_INDEX)
        .expect("the text reads")
        .expect("the text closes with a term");

    assert_eq!(put.asked, Checker::CORE_TEXT);
    assert_eq!(put.said(Checker::Certifier), "refused");
    assert!(
        put.to_string()
            .contains("└─ [refused] by certifier `NotDescending` at the closing term"),
        "{put}"
    );
}

/// A subject that does not lower or read is put to no checker, and a text closing with no term is no proof.
#[test]
fn a_subject_that_does_not_read_is_put_to_no_checker() {
    assert!(
        program(
            "use /std/{Nope};
0
"
        )
        .is_err()
    );
    assert!(core_text("let").is_err());
    assert!(matches!(core_text("let /m/x : #NatType = 0;\n"), Ok(None)));
}
