//! Wrapping lists keep the surrounding expression breakable and use structural indentation.

use super::{WIDTH, test_support::formatted};

fn canonical_within_width(source: &str) -> String {
    let output = formatted(source);
    assert!(
        output.lines().all(|line| line.chars().count() <= WIDTH),
        "{output}"
    );
    assert_eq!(formatted(&output), output);
    output
}

#[test]
fn a_wrapped_argument_does_not_flatten_the_calls_later_arguments() {
    let output = canonical_within_width(
        "pub let spawn(cmd: Command) -> Try(Io, Io/Error, Child) =\n\
         let _ = env_checked(cmd.env)!;\n\
         let reply = proc_spawn([Str/to_bytes(cmd.program), ..List/map(cmd.args, Str/to_bytes)], match cmd.cwd | some(p) => Path/to_bytes(p) | none() => x[] end, List/map(cmd.env, env_entry), stdio_of(cmd.stdin), stdio_of(cmd.stdout), stdio_of(cmd.stderr))!;\n\
         reply;\n",
    );
    assert!(output.contains("proc_spawn(\n            ["), "{output}");
    assert!(
        output.contains("\n            stdio_of(cmd.stderr)"),
        "{output}"
    );
}

#[test]
fn a_list_in_an_arm_does_not_hide_the_ladders_breaks() {
    let output = canonical_within_width(
        "let each_ready(reports: Bytes) -> List(Flag) =\n\
         match reports | x[] => [] | x[r, ..rest] => [flagged(r, /sys/event/ready), ..each_ready(rest)] end;\n",
    );
    assert!(output.contains("    match reports\n    | x[] => []\n    | x[r, ..rest] => [flagged(r, /sys/event/ready), ..each_ready(rest)]\n    end;"), "{output}");
}

#[test]
fn a_list_inside_a_tuple_and_lambda_does_not_flatten_the_enclosing_call() {
    let output = canonical_within_width(
        "let components(b: Bytes) -> {before: List(Bytes), last: Bytes} =\n\
         let (done, current): {List(Bytes), Bytes} =\n\
         Bytes/fold(b, ([], x[]), (byte, (done, current)) => match byte == separator | true => ([..done, current], x[]) | false => (done, x[..current, byte]) end);\n\
         (before = done, last = current);\n",
    );
    assert!(output.contains("Bytes/fold(\n"), "{output}");
    assert!(output.contains("([..done, current], x[])"), "{output}");
}

#[test]
fn lists_in_witness_arms_do_not_hide_the_ladders_breaks() {
    let output = canonical_within_width(
        "satisfy Show(Exit) {\n\
         show(e) = match e | exited(code) => Str/flatten([\"exited(\", Nat/to_str(code), \")\"]) | signaled(signal) => Str/flatten([\"signaled(\", Nat/to_str(signal), \")\"]) end,\n\
         }\n",
    );
    assert!(output.contains("match e\n"), "{output}");
    assert!(
        output.contains("Str/flatten([\"exited(\", Nat/to_str(code), \")\"])"),
        "{output}"
    );
    assert!(
        output.contains("Str/flatten([\"signaled(\", Nat/to_str(signal), \")\"])"),
        "{output}"
    );
}

#[test]
fn packed_atoms_inside_a_list_do_not_flatten_the_calls_later_arguments() {
    let packed = ["alpha"; 24].join(", ");
    let source = format!(
        "let out: Shape = f([x[{packed}]], long_argument_one, long_argument_two, long_argument_three, long_argument_four);\n"
    );
    let output = canonical_within_width(&source);
    assert!(output.contains("f(\n"), "{output}");
    assert!(output.contains("\n        long_argument_four)"), "{output}");
}

#[test]
fn a_packed_argument_does_not_flatten_a_call_inside_a_list() {
    let packed = ["alpha"; 24].join(", ");
    let source = format!(
        "let out: Shape = [f(x[{packed}], long_argument_one, long_argument_two, long_argument_three, long_argument_four), other(value)];\n"
    );
    let output = canonical_within_width(&source);
    assert!(output.contains("f(\n"), "{output}");
    assert!(output.contains("\n        other(value)"), "{output}");
}

#[test]
fn a_multiline_html_child_ends_before_its_next_sibling_begins() {
    let output = canonical_within_width(
        "pub let document(lang: Str, title: Str, head: List(Html), body: List(Html)) -> Html =\n\
         element(\"html\", [Attr/attr(\"lang\", lang)], [element(\"head\", [], [void(\"meta\", [Attr/attr(\"charset\", \"utf-8\")]), element(\"title\", [], [text(title)]), ..head]), element(\"body\", [], body)]);\n",
    );
    assert!(
        output.contains("\n                [\n                    void(\"meta\""),
        "{output}"
    );
    assert!(
        output.contains("\n                    element(\"title\""),
        "{output}"
    );
    assert!(
        output.contains("\n            element(\"body\", [], body)"),
        "{output}"
    );
}
