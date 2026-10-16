//! Grammar-valued leaves dispatch typed sums and preserve option token ownership.

use super::super::compile;
use curios_runtime::{ForeignBindings, MockHost};

#[test]
fn a_leaf_handler_receives_the_selected_sum_variant() {
    let compiled = compile(r#"
        use /std/{Cli, Str, Option, print};
        use /std/Cli/{Grammar};
        use /std/Cli/Grammar/{Named, Positional};
        induct Mode: Type | path(Str) | mock() end
        let grammar = Grammar/choice(
            Grammar/map(Grammar/positional(Positional { name = "path", help = "Path" }, Cli/str), Mode/path),
            Grammar/map(Grammar/flag(Named { long = "mock", short = Option/none(), help = "Mock" }), (_) => Mode/mock()));
        let app = Cli/leaf("app", "", Option/none(), grammar,
            (mode) => print(match mode | path(value) => value | mock() => "mock" end));
        Cli/main(Cli/Declaration/declare(app))
    "#).expect("the sum grammar compiles");
    for (args, status, output) in [
        (vec!["app", "file.crs"], 0, "file.crs"),
        (vec!["app", "--mock"], 0, "mock"),
        (vec!["app"], 2, ""),
        (vec!["app", "file.crs", "--mock"], 2, ""),
    ] {
        let (host, streams) = MockHost::builder().args(args).build();
        assert_eq!(compiled.run(host, ForeignBindings::empty()), Ok(status));
        if status == 0 {
            assert_eq!(streams.output(), output.as_bytes());
            assert_eq!(streams.errors(), b"");
        } else {
            assert_eq!(streams.output(), streams.errors());
            assert!(!streams.errors().is_empty());
        }
    }
}

#[test]
fn help_and_version_owned_as_values_reach_the_handler() {
    let compiled = compile(r#"
        use /std/{Cli, Option, print};
        use /std/Cli/{Grammar};
        use /std/Cli/Grammar/{Named};
        let grammar = Grammar/option(Named { long = "value", short = Option/some('v'), help = "" }, Cli/str);
        Cli/main(Cli/Declaration/declare(Cli/leaf("app", "", Option/some("1"), grammar, print)))
    "#).expect("the option grammar compiles");
    for (args, output) in [
        (vec!["app", "--value", "--help"], "--help"),
        (vec!["app", "--value=--version"], "--version"),
        (vec!["app", "-v", "--help"], "--help"),
        (vec!["app", "-v--version"], "--version"),
    ] {
        let (host, streams) = MockHost::builder().args(args).build();
        assert_eq!(compiled.run(host, ForeignBindings::empty()), Ok(0));
        assert_eq!(streams.output(), output.as_bytes());
        assert_eq!(streams.errors(), b"");
    }
}

#[test]
fn a_disabled_version_builtin_can_be_a_user_option() {
    let compiled = compile(r#"
        use /std/{Cli, Option, print};
        use /std/Cli/{Grammar};
        use /std/Cli/Grammar/{Named};
        let grammar = Grammar/option(Named { long = "version", short = Option/none(), help = "" }, Cli/str);
        Cli/main(Cli/Declaration/declare(Cli/leaf("app", "", Option/none(), grammar, print)))
    "#).expect("the inactive builtin name is legal");
    let (host, streams) = MockHost::builder()
        .args(["app", "--version", "mine"])
        .build();
    assert_eq!(compiled.run(host, ForeignBindings::empty()), Ok(0));
    assert_eq!(streams.output(), b"mine");
}
