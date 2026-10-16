//! Complete command declarations are certified before dispatch, help or argument decoding.

use super::super::compile;
use curios_runtime::{ForeignBindings, MockHost};

fn refused(declaration: &str, args: &[&str], expected: &str) {
    let source = format!(
        r#"
        use /std/{{Cli, Option, Io, Result, List, print}};
        use /std/Cli/{{Grammar}};
        use /std/Cli/Grammar/{{Named}};
        {declaration}
        match build() | failure(error) => Cli/specification_error(error) | success(cli) => Cli/main(cli) end
    "#
    );
    let compiled = compile(&source).expect("a dynamic specification can be reported at run time");
    let (host, streams) = MockHost::builder().args(args.iter().copied()).build();
    assert_eq!(compiled.run(host, ForeignBindings::empty()), Ok(1));
    assert_eq!(streams.output(), streams.errors());
    let error = String::from_utf8(streams.errors()).expect("the specification error is UTF-8");
    assert!(error.contains(expected), "{error}");
}

const INVALID_SIBLING: &str = r#"
    let arguments: List(Cli/Arg) = [Cli/flag("same", ""), Cli/flag("same", "")];
    let build() -> Result(Cli/SpecError, Cli) =
        let grammar = Cli/record/checked_record(arguments)!;
        let good = Cli/leaf("good", "", Option/none(), Grammar/pure(()), (_) => print("ran"));
        let bad = Cli/leaf("bad", "", Option/none(), grammar, (_) => print("bad ran"));
        Cli/checked(Cli/group("app", "", Option/some("1"), [good, bad]));
"#;

#[test]
fn an_invalid_unselected_sibling_precedes_a_valid_handler() {
    refused(INVALID_SIBLING, &["app", "good"], "duplicate record field");
}

#[test]
fn an_invalid_unselected_sibling_precedes_group_help() {
    refused(
        INVALID_SIBLING,
        &["app", "--help"],
        "duplicate record field",
    );
}

#[test]
fn an_invalid_unselected_sibling_precedes_child_help() {
    refused(
        INVALID_SIBLING,
        &["app", "good", "--help"],
        "duplicate record field",
    );
}

#[test]
fn an_invalid_unselected_sibling_precedes_version() {
    refused(
        INVALID_SIBLING,
        &["app", "--version"],
        "duplicate record field",
    );
}

#[test]
fn a_duplicate_subcommand_is_a_specification_error() {
    refused(
        r#"
        let child = Cli/leaf("same", "", Option/none(), Grammar/pure(()), (_) => print("ran"));
        let build() -> Result(Cli/SpecError, Cli) = Cli/checked(Cli/group("app", "", Option/none(), [child, child]));
    "#,
        &["app", "--help"],
        "duplicate subcommand",
    );
}

#[test]
fn an_invalid_command_name_is_a_specification_error() {
    refused(
        r#"
        let app = Cli/leaf("bad name", "", Option/none(), Grammar/pure(()), (_) => print("ran"));
        let build() -> Result(Cli/SpecError, Cli) = Cli/checked(app);
    "#,
        &["app", "--help"],
        "invalid command name",
    );
}

#[test]
fn a_long_help_collision_is_a_specification_error() {
    refused(
        r#"
        let app = Cli/leaf("app", "", Option/none(), Grammar/switch(Named { long = "help", short = Option/none(), help = "" }), (_) => print("ran"));
        let build() -> Result(Cli/SpecError, Cli) = Cli/checked(app);
    "#,
        &["app", "--help"],
        "collides with an active built-in",
    );
}

#[test]
fn a_short_help_collision_is_a_specification_error() {
    refused(
        r#"
        let app = Cli/leaf("app", "", Option/none(), Grammar/switch(Named { long = "other", short = Option/some('h'), help = "" }), (_) => print("ran"));
        let build() -> Result(Cli/SpecError, Cli) = Cli/checked(app);
    "#,
        &["app", "--help"],
        "collides with an active built-in",
    );
}

#[test]
fn an_active_version_collision_is_a_specification_error() {
    refused(
        r#"
        let app = Cli/leaf("app", "", Option/some("1"), Grammar/switch(Named { long = "version", short = Option/none(), help = "" }), (_) => print("ran"));
        let build() -> Result(Cli/SpecError, Cli) = Cli/checked(app);
    "#,
        &["app", "--help"],
        "collides with an active built-in",
    );
}

#[test]
fn an_invalid_tree_precedes_non_utf8_input() {
    let source = format!(
        r#"
        use /std/{{Cli, Option, Result, List, print}};
        use /std/Cli/{{Grammar}};
        {INVALID_SIBLING}
        match build() | failure(error) => Cli/specification_error(error) | success(cli) => Cli/main(cli) end
    "#
    );
    let compiled = compile(&source).expect("the dynamic tree compiles");
    let (host, streams) = MockHost::builder()
        .args([b"app".as_slice(), b"\xff".as_slice()])
        .build();
    assert_eq!(compiled.run(host, ForeignBindings::empty()), Ok(1));
    assert!(String::from_utf8_lossy(&streams.errors()).contains("duplicate record field"));
}

#[test]
fn an_invalid_nested_sibling_is_rejected_when_the_root_is_constructed() {
    let declaration = r#"
        let good = Cli/leaf("good", "", Option/none(), Grammar/pure(()), (_) => print("ran"));
        let bad = Cli/leaf("bad name", "", Option/none(), Grammar/pure(()), (_) => print("bad ran"));
        let nested = Cli/group("nested", "", Option/none(), [bad]);
        let build() -> Result(Cli/SpecError, Cli) = Cli/checked(Cli/group("app", "", Option/none(), [good, nested]));
    "#;
    for args in [vec!["app", "good"], vec!["app", "--help"]] {
        refused(
            declaration,
            &args,
            "in command \"nested\": in command \"bad name\"",
        );
    }
}

#[test]
fn builtin_collisions_in_unselected_siblings_are_rejected_during_construction() {
    let declaration = r#"
        let good = Cli/leaf("good", "", Option/none(), Grammar/pure(()), (_) => print("ran"));
        let bad = Cli/leaf("bad", "", Option/none(), Grammar/switch(Named { long = "help", short = Option/none(), help = "" }), (_) => print("bad ran"));
        let build() -> Result(Cli/SpecError, Cli) = Cli/checked(Cli/group("app", "", Option/none(), [good, bad]));
    "#;
    for args in [vec!["app", "good"], vec!["app", "--help"]] {
        refused(
            declaration,
            &args,
            "in command \"bad\": option \"--help\" collides",
        );
    }
}
