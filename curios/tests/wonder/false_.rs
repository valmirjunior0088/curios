//! `curios wonder --false`: a subject's closing term put at `False`, its answer on standard output and exit 0 whichever way it went, and a subject put to no checker stopped as a build is.

use super::test_support::*;

/// A recursion through a callback handed past a member's one parameter to the function the member answers: a fixed ticket's program witness.
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

/// A recursion handing its own proof back below the index its target refined: a fixed ticket's Core text witness.
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

/// A program the checkers refuse at `False` is answered on standard output, verdict first and by whom, with exit 0.
#[test]
fn a_refused_program_is_answered_with_exit_zero() {
    let root = temporary("false-program");
    write(&root, "spin.crs", HANDED_PAST);

    let output = curios(&root, &["wonder", "--false", "spin.crs"], "");
    let said = stdout(&output);

    assert_eq!(output.status.code(), Some(0), "{output:?}");
    assert!(
        said.starts_with("REFUSED    by elaborator `PartialInErasedPosition`"),
        "{said}"
    );
    assert!(
        said.contains("\n           elaborator  refused\n"),
        "{said}"
    );
}

/// A refusal beneath the verdict closes on its place as the answer's reader spells it: a program of a package, read through its manifest, closes on the file as it is named where the reader stands, which is how a check of the same file closes it.
#[test]
fn a_refusal_closes_on_its_place_as_a_check_spells_it() {
    let root = project("false-place");

    let output = curios(&root, &["wonder", "--false", "app.crs"], "");
    let said = stdout(&output);

    assert_eq!(output.status.code(), Some(0), "{output:?}");
    assert!(
        said.starts_with("REFUSED    by elaborator `TypeMismatch` at the closing term"),
        "{said}"
    );
    assert!(said.contains("\n└─ [error] app.crs:1:1\n"), "{said}");
    assert!(
        !said.contains(&root.display().to_string()),
        "no line spells the file from the root of the disk: {said}"
    );
}

/// A `.crsc` is put to the certifier alone, from a file or from standard input under `--core`, and answered alike.
#[test]
fn core_text_is_put_to_the_certifier_alone() {
    let root = temporary("false-core-text");
    write(&root, "f.crsc", REFINED_INDEX);

    for (arguments, stdin) in [
        (&["wonder", "--false", "f.crsc"][..], ""),
        (&["wonder", "--false", "--core", "-"][..], REFINED_INDEX),
    ] {
        let output = curios(&root, arguments, stdin);
        let said = stdout(&output);

        assert_eq!(output.status.code(), Some(0), "{output:?}");
        assert!(
            said.starts_with("REFUSED    by certifier `NotDescending`"),
            "{said}"
        );
        assert!(!said.contains("elaborator"), "{said}");
    }
}

/// A subject whose text does not read was put to no checker: no answer, its report on standard error, exit 1.
#[test]
fn a_subject_that_does_not_read_is_stopped_as_a_build_is() {
    let root = temporary("false-unread");
    write(&root, "bad.crsc", "let");

    let output = curios(&root, &["wonder", "--false", "bad.crsc"], "");

    assert_eq!(output.status.code(), Some(1), "{output:?}");
    assert_eq!(stdout(&output), "");
}
