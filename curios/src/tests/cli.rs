//! `/std/Cli`: the specification a program declares, the record computed from it, and the bound that refuses a name the specification does not contain.

use super::{error, run};

/// The five carriers, read back through `get` at the types the specification gives their names. The record is built by hand here because `fill` is `parse`'s; every value is bound at its own type first, since a numeral realizes before an indexed constructor's index is solved.
#[test]
fn get_reads_each_carrier_at_the_type_its_entry_decides() {
    assert_eq!(
        run(r#"
        use /std/{Cli, Str, Nat, Bool, Option, List, print};

        let spec: List(Cli/Arg) =
            [ Cli/Arg { ..Cli/flag("verbose", "Log every request"), short = Option/some('v') }
            , Cli/default("port", Cli/nat, 8080, "Port to listen on")
            , Cli/optional("tag", Cli/str, "An optional tag")
            , Cli/many("header", Cli/str, "Repeatable")
            , Cli/positional("root", Cli/str, "Directory to serve")
            ];

        let loud: Bool = true;
        let port: Nat = 9090;
        let tag: Option(Str) = Option/some("x");
        let headers: List(Str) = ["one", "two"];
        let root: Str = "/srv";

        let sample: Cli/Values(spec) =
            Cli/Values/cons(
                loud,
                Cli/Values/cons(
                    port,
                    Cli/Values/cons(
                        tag,
                        Cli/Values/cons(headers, Cli/Values/cons(root, Cli/Values/nil())))));

        let read_verbose: Bool = Cli/get(sample, "verbose");
        let read_port: Nat = Cli/get(sample, "port");
        let read_tag: Option(Str) = Cli/get(sample, "tag");
        let read_headers: List(Str) = Cli/get(sample, "header");
        let read_root: Str = Cli/get(sample, "root");

        print(
            Str/join(
                " ",
                [ Bool/to_str(read_verbose)
                , Nat/to_str(read_port)
                , Option/unwrap_or(read_tag, "-")
                , Str/join(",", read_headers)
                , read_root
                ]))
        "#),
        b"true 9090 x one,two /srv"
    );
}

/// A handler reads the specification off its own parameter: `run`'s argument is typed by the `args` written two fields above it, so `get` there needs no annotation and answers at the entry's carrier.
#[test]
fn a_handler_infers_the_specification_from_the_record_it_is_handed() {
    assert_eq!(
        run(r#"
        use /std/{Cli, Str, Nat, Option, Io, print};

        let serve: Cli =
            Cli {
                name = "serve",
                about = "Serve a directory over HTTP",
                version = Option/some("0.1.0"),
                args =
                    [ Cli/default("port", Cli/nat, 8080, "Port to listen on")
                    , Cli/positional("root", Cli/str, "Directory to serve")
                    ],
                run(v) =
                    let port = Cli/get(v, "port");
                    let root = Cli/get(v, "root");
                    print(Str/flatten([root, " on ", Nat/to_str(port)])),
                commands = [],
            };

        let demand(c: Cli, @ok: Cli/WellFormed(c)) -> Io({}) =
            print(Str/flatten([c.name, " is well formed"]));

        demand(serve)
        "#),
        b"serve is well formed"
    );
}

/// A name the specification does not contain is refused where it is written: `Has` walks to `False` and the bound holding `get` up cannot be discharged.
#[test]
fn a_misspelled_name_is_refused_rather_than_answered() {
    let refused = error(
        r#"
        use /std/{Cli, Str, Nat, List, print};

        let spec: List(Cli/Arg) = [Cli/default("port", Cli/nat, 8080, "Port to listen on")];
        let port: Nat = 1;
        let sample: Cli/Values(spec) = Cli/Values/cons(port, Cli/Values/nil());

        let read = Cli/get(sample, "prot");
        print("unreachable")
        "#,
    );

    assert!(
        refused.contains("Has"),
        "the refusal names the bound nothing discharged:\n{refused}"
    );
}

/// `WellFormed` is decided by reduction, so a specification naming one long name twice is refused at the entry that demands it rather than at the first line it would misparse.
#[test]
fn a_duplicate_long_name_is_refused_at_well_formed() {
    let refused = error(
        r#"
        use /std/{Cli, Str, Option, Io, print};

        let twice: Cli =
            Cli {
                name = "dup",
                about = "Two entries share a long name",
                version = Option/none(),
                args = [Cli/flag("verbose", "once"), Cli/flag("verbose", "twice")],
                run(_) = print("never"),
                commands = [],
            };

        let demand(c: Cli, @ok: Cli/WellFormed(c)) -> Io({}) = print(c.name);

        demand(twice)
        "#,
    );

    assert!(
        refused.contains("WellFormed"),
        "the refusal names the bound nothing discharged:\n{refused}"
    );
}
