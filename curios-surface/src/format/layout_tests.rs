//! Structural layouts keep their surrounding expressions breakable and use structural indentation.

use {super::test_support::*, curios_common::MAX_LINE_WIDTH};

fn canonical_within_width(source: &str) -> String {
    let output = formatted(source);
    assert!(
        output
            .lines()
            .all(|line| line.chars().count() <= MAX_LINE_WIDTH),
        "{output}"
    );
    assert_eq!(formatted(&output), output);
    output
}

#[test]
fn a_wire_literal_packs_byte_runs_and_separates_every_spread() {
    let source = "let wire = 0x[0x00, 0x55, 0xFD, 0xFE, 0x02, 0xFD, 0x55, 0xFE, 0xFD, 0x01, 0x55, 0xFE, ..Bytes/replicate(36, 0), 0x55, 0xFE, 0x03, ..Bytes/drop(fixture, 2), 0xFE, 0x02, ..fixture];\n()\n";
    assert_eq!(
        canonical_within_width(source),
        concat!(
            "let wire =\n",
            "    0x[\n",
            "        0x00, 0x55, 0xFD, 0xFE, 0x02, 0xFD, 0x55, 0xFE, 0xFD, 0x01, 0x55, 0xFE,\n",
            "        ..Bytes/replicate(36, 0),\n",
            "        0x55, 0xFE, 0x03,\n",
            "        ..Bytes/drop(fixture, 2),\n",
            "        0xFE, 0x02,\n",
            "        ..fixture];\n",
            "()\n",
        )
    );
}

#[test]
fn spread_calls_and_nested_packed_literals_remain_breakable() {
    for open in ["[", "0x["] {
        let source = format!(
            "let out: Shape = f({open}1, 2, ..build(long_argument_one, long_argument_two, long_argument_three, long_argument_four, long_argument_five), 0x[3, 4, ..tail], 5, ..rest], another_argument);\n"
        );
        let output = canonical_within_width(&source);
        assert!(output.contains("..build(\n"), "{output}");
        assert!(output.contains("0x[3, 4, ..tail]"), "{output}");
        assert!(
            output.lines().any(|line| line.trim() == "..rest],"),
            "{output}"
        );
    }
}

#[test]
fn a_wrapped_argument_does_not_flatten_the_calls_later_arguments() {
    let output = canonical_within_width(
        "pub let spawn(cmd: Command) -> Try(Io, Io/Error, Child) =\n\
         let _ = env_checked(cmd.env)!;\n\
         let reply = proc_spawn([Str/to_bytes(cmd.program), ..List/map(cmd.args, Str/to_bytes)], match cmd.cwd | some(p) => Path/to_bytes(p) | none() => 0x[] end, List/map(cmd.env, env_entry), stdio_of(cmd.stdin), stdio_of(cmd.stdout), stdio_of(cmd.stderr))!;\n\
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
         match reports | 0x[] => [] | 0x[r, ..rest] => [flagged(r, /sys/event/ready), ..each_ready(rest)] end;\n",
    );
    assert!(output.contains("    match reports\n    | 0x[] => []\n    | 0x[r, ..rest] => [flagged(r, /sys/event/ready), ..each_ready(rest)]\n    end;"), "{output}");
}

#[test]
fn a_list_inside_a_tuple_and_lambda_does_not_flatten_the_enclosing_call() {
    let output = canonical_within_width(
        "let components(b: Bytes) -> {before: List(Bytes), last: Bytes} =\n\
         let (done, current): {List(Bytes), Bytes} =\n\
         Bytes/fold(b, ([], 0x[]), (byte, (done, current)) => match byte == separator | true => ([..done, current], 0x[]) | false => (done, 0x[..current, byte]) end);\n\
         (before = done, last = current);\n",
    );
    assert!(output.contains("Bytes/fold(\n"), "{output}");
    assert!(output.contains("([..done, current], 0x[])"), "{output}");
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
        "let out: Shape = f([0x[{packed}]], long_argument_one, long_argument_two, long_argument_three, long_argument_four);\n"
    );
    let output = canonical_within_width(&source);
    assert!(output.contains("f(\n"), "{output}");
    assert!(output.contains("\n        long_argument_four)"), "{output}");
}

#[test]
fn a_packed_argument_does_not_flatten_a_call_inside_a_list() {
    let packed = ["alpha"; 24].join(", ");
    let source = format!(
        "let out: Shape = [f(0x[{packed}], long_argument_one, long_argument_two, long_argument_three, long_argument_four), other(value)];\n"
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

#[test]
fn an_applied_match_breaks_its_arms_to_keep_the_argument_attached() {
    assert_eq!(
        canonical_within_width(
            "pub let first(a: Bool, b: Bool, p: Holds(a && b)) -> Holds(a) =\n\
             match a | true => (_) => True/qed() | false => (proof_of_a) => proof_of_a end(p);\n",
        ),
        concat!(
            "pub let first(a: Bool, b: Bool, p: Holds(a && b)) -> Holds(a) =\n",
            "    match a\n",
            "    | true => (_) => True/qed()\n",
            "    | false => (proof_of_a) => proof_of_a\n",
            "    end(p);\n",
        )
    );
}

#[test]
fn an_applied_match_with_no_head_breaks_its_arms_to_keep_the_argument_attached() {
    assert_eq!(
        canonical_within_width(
            "let selected: Holds(a) =\n\
             match | a => (_) => first_long_function_name() | _ => (h) => second_long_function_name(h) end(p);\n",
        ),
        concat!(
            "let selected: Holds(a) =\n",
            "    match\n",
            "    | a => (_) => first_long_function_name()\n",
            "    | _ => (h) => second_long_function_name(h)\n",
            "    end(p);\n",
        )
    );
}

// An arm's guards stay on its line where its head fits there, and otherwise each stands on a line of its own at the arm's column, led by its comma, as an alternative after the first is led by its bar. A headed match that writes a guard takes a line for each arm however short it is.
#[test]
fn guards_that_do_not_fit_stand_each_on_a_line_led_by_its_comma() {
    assert_eq!(
        canonical_within_width(
            "let total =\n\
             match | some(first_value) = alpha, some(second_value) = beta, first_value + second_value > 1000000000000, first_value != second_value => first_value + second_value | _ => 0 end;\n\
             ()\n",
        ),
        concat!(
            "let total =\n",
            "    match\n",
            "    | some(first_value) = alpha\n",
            "    , some(second_value) = beta\n",
            "    , first_value + second_value > 1000000000000\n",
            "    , first_value != second_value => first_value + second_value\n",
            "    | _ => 0\n",
            "    end;\n",
            "()\n",
        )
    );
    assert_eq!(
        canonical_within_width("let size = match o | some(n), n > 9 => 2 | _ => 0 end;\n()\n"),
        "let size =\n    match o\n    | some(n), n > 9 => 2\n    | _ => 0\n    end;\n()\n",
    );
}

/// A block string stands where the code around it puts it: its lines and its closer at the opener's column where the opener begins its line, as the reference writes one under a `let`, and one level in from the line where the opener rides it, as a call's last argument does, the parenthesis hugging the closer. Lines indented past the closer keep the difference, so a closer written left of them keeps indentation in the value, and what follows the closer rides it.
#[test]
fn a_block_string_stands_where_the_code_around_it_puts_it() {
    let source = concat!(
        "let page: Str =\n",
        "    \"\"\"\n",
        "    <ul>\n",
        "        <li>one</li>\n",
        "    </ul>\n",
        "    \"\"\";\n",
        "\n",
        "let indented: Str =\n",
        "    \"\"\"\n",
        "      kept two in\n",
        "    \"\"\";\n",
        "\n",
        "let local(x: Nat) -> Str =\n",
        "    let s =\n",
        "        \"\"\"\n",
        "        a\n",
        "        \"\"\";\n",
        "    s;\n",
        "\n",
        "print(\"\"\"\n",
        "    hello\n",
        "    \"\"\")\n",
    );
    assert_eq!(formatted(source), source);

    let moved = source
        .replace(
            "    <ul>\n        <li>one</li>\n    </ul>\n    \"\"\";",
            "        <ul>\n            <li>one</li>\n        </ul>\n        \"\"\";",
        )
        .replace(
            "print(\"\"\"\n    hello\n    \"\"\")",
            "print(\n    \"\"\"\n        hello\n        \"\"\")",
        );
    assert_eq!(formatted(&moved), source);
}
