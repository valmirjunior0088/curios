//! `foreign` declarations: written as a function with no body, their results as the wire vocabulary admits, and a type outside it refused by name.

use {
    crate::*,
    curios_abi::{WireLeaf, WireResults, WireReturn, WireSignature, WireType},
};

#[test]
fn top_foreign_without_pub() {
    assert_eq!(
        "foreign frobnicate(Nat, Bytes) -> Nat;"
            .parse::<Module>()
            .unwrap()
            .items,
        vec![TopItem::Foreign(TopForeign {
            doc: None,
            vis_pub: false,
            label: "frobnicate".into(),
            signature: WireSignature {
                params: vec![WireType::Nat, WireType::Bytes],
                returns: WireReturn::Answers(WireResults::single(WireType::Nat)),
            },
        })]
    );
}

#[test]
fn top_foreign_with_pub() {
    assert_eq!(
        "pub foreign frobnicate(Nat, Bytes) -> Nat;"
            .parse::<Module>()
            .unwrap()
            .items,
        vec![TopItem::Foreign(TopForeign {
            doc: None,
            vis_pub: true,
            label: "frobnicate".into(),
            signature: WireSignature {
                params: vec![WireType::Nat, WireType::Bytes],
                returns: WireReturn::Answers(WireResults::single(WireType::Nat)),
            },
        })]
    );
}

#[test]
fn top_foreign_zero_arg() {
    assert_eq!(
        "foreign clock() -> Nat;".parse::<Module>().unwrap().items,
        vec![TopItem::Foreign(TopForeign {
            doc: None,
            vis_pub: false,
            label: "clock".into(),
            signature: WireSignature {
                params: vec![],
                returns: WireReturn::Answers(WireResults::single(WireType::Nat)),
            },
        })]
    );
}

/// `{}` is the unit *type*, which is what a result position holds — `()` is the unit value and is no result. The rows have carried a zero-result shape since `handle_close`; this is the surface finally able to spell one.
#[test]
fn top_foreign_takes_no_result() {
    assert_eq!(
        "foreign close(Handle) -> {};"
            .parse::<Module>()
            .unwrap()
            .items,
        vec![TopItem::Foreign(TopForeign {
            doc: None,
            vis_pub: false,
            label: "close".into(),
            signature: WireSignature {
                params: vec![WireType::Handle],
                returns: WireReturn::Answers(WireResults::none()),
            },
        })]
    );
}

/// Two or more results reach the guest as a tuple type it projects by name, so the surface spells the labels and the row keeps them: `r.status` and `r.bytes` are what `/std` reads off `Handle/read`.
#[test]
fn top_foreign_takes_a_tuple_of_results() {
    assert_eq!(
        "foreign read(Handle, Nat) -> {status: Nat, bytes: Bytes};"
            .parse::<Module>()
            .unwrap()
            .items,
        vec![TopItem::Foreign(TopForeign {
            doc: None,
            vis_pub: false,
            label: "read".into(),
            signature: WireSignature {
                params: vec![WireType::Handle, WireType::Nat],
                returns: WireReturn::Answers(WireResults::record(
                    ("status".to_string(), WireType::Nat),
                    ("bytes".to_string(), WireType::Bytes),
                    Vec::new(),
                )),
            },
        })]
    );
}

/// One result is forwarded through as itself, so a one-field brace would name a tuple no row can carry — and would drop the label, which by `documentation/syntax.md` makes it a different type again. Refused rather than quietly unwrapped.
#[test]
fn top_foreign_refuses_a_single_result_in_braces() {
    let refusal = "foreign f(Nat) -> {value: Nat};"
        .parse::<Module>()
        .unwrap_err()
        .format();

    assert!(
        refusal.contains("spelled bare"),
        "the refusal says how to spell it: {refusal}"
    );
    assert!(
        refusal.ends_with(
            "    1 | foreign f(Nat) -> {value: Nat};\n      |                    ^^^^^^^^^^"
        ),
        "the refusal stands at the field: {refusal}"
    );
}

/// A result is refused as the form its writer began: a brace commits to its fields, a word is `Never` or a wire type's to refuse, and where no form begins the refusal says what a result is. Uncommitted, each fell to whichever alternative was tried first and named its brace.
#[test]
fn top_foreign_refuses_a_result_as_the_form_it_began() {
    for (source, expected, caret) in [
        (
            "foreign m(Nat) -> ();",
            "`()` is the unit value, and a result is a type: a row that answers nothing is written `-> {}`",
            "                  ^^",
        ),
        (
            "foreign m(Nat) -> (Nat) -> Nat;",
            "expected a wire result: a wire type, a braced list of labelled wire types, `{}` for none, or `Never`",
            "                  ^",
        ),
        (
            "foreign m(Nat) -> ;",
            "expected a wire result",
            "                  ^",
        ),
        (
            "foreign m(Nat) -> {Nat, Nat};",
            "a result's field is written `label: type`, as in `status: Nat`; 'Nat' is read as a label and no `:` follows it",
            "                   ^^^",
        ),
        (
            "foreign m(Nat) -> {a: Nat, b};",
            "'b' is read as a label and no `:` follows it",
            "                           ^",
        ),
        (
            "foreign m(Nat) -> {a: };",
            "expected a wire type (Nat, Int, Bool, Byte, Flt, Bytes, Handle, List(...), or Option(...))",
            "                      ^",
        ),
        // A field whose separator is missing is no field standing alone: the brace reports it.
        (
            "foreign m(Nat) -> {a: Nat b: Nat};",
            "Expected '}', obtained 'b'",
            "                          ^",
        ),
        // `List` reaches the wire type's own refusal, as a result and as an operand.
        (
            "foreign m(Nat) -> List;",
            "`List` is written with its element type, as in `List(Nat)`",
            "                      ^",
        ),
        (
            "foreign m(List) -> Nat;",
            "`List` is written with its element type, as in `List(Nat)`",
            "              ^",
        ),
        (
            "foreign m(Nat) -> List();",
            "expected a List or Option element type (Nat, Int, Bool, Bytes, or Handle — neither nests)",
            "                       ^",
        ),
        (
            "foreign m(Nat) -> Option;",
            "`Option` is written with its element type, as in `Option(Nat)`",
            "                        ^",
        ),
        (
            "foreign m(Nat) -> /std/Nat;",
            "a wire type is written bare: `Nat` rather than `/std/Nat`",
            "                  ^",
        ),
    ] {
        let report = source.parse::<Module>().unwrap_err().format();
        assert!(report.contains(expected), "{source:?} reported {report}");
        assert!(
            report.ends_with(&format!("    1 | {source}\n      | {caret}")),
            "{source:?} reported {report}"
        );
    }
}

/// A result's labels are a tuple type's, so a keyword, `_` and a label written twice are each refused, and at the label: the declaration's type is built with no extent, so a refusal left to it holds no position.
#[test]
fn top_foreign_reads_a_results_labels_as_a_tuple_types() {
    for (source, expected, caret) in [
        (
            "foreign g(Nat) -> {a: Nat, a: Nat};",
            "a result names each of its fields once: 'a' labels an earlier one",
            "                           ^",
        ),
        (
            "foreign g(Nat) -> {_: Nat, b: Nat};",
            "a result's field is read by its label, so `_` cannot stand for one",
            "                   ^",
        ),
        (
            "foreign g(Nat) -> {status: Nat, let: Bytes};",
            "'let' is a reserved keyword, so it cannot be a name",
            "                                ^^^",
        ),
    ] {
        let report = source.parse::<Module>().unwrap_err().format();
        assert!(report.contains(expected), "{source:?} reported {report}");
        assert!(
            report.ends_with(&format!("    1 | {source}\n      | {caret}")),
            "{source:?} reported {report}"
        );
    }
}

/// A reference may take any slot, and several may: the results cross in the order written, since `{a: Nat, b: Bytes}` and `{b: Bytes, a: Nat}` are two tuple types and the one declared is the one the program gets.
#[test]
fn top_foreign_keeps_references_in_the_slots_written() {
    assert_eq!(
        "foreign f(Nat) -> {bytes: Bytes, status: Nat, rest: List(Bytes)};"
            .parse::<Module>()
            .unwrap()
            .items,
        vec![TopItem::Foreign(TopForeign {
            doc: None,
            vis_pub: false,
            label: "f".into(),
            signature: WireSignature {
                params: vec![WireType::Nat],
                returns: WireReturn::Answers(WireResults::record(
                    ("bytes".to_string(), WireType::Bytes),
                    ("status".to_string(), WireType::Nat),
                    vec![("rest".to_string(), WireType::List(WireLeaf::Bytes))],
                )),
            },
        })]
    );
}

/// `List` does not nest. Codegen forces and embeds exactly one level at the host boundary — a second level would hand the host rope structs where flat arrays belong — so `WireLeaf` keeps the grammar to what codegen implements, and the parser rejects the nested spelling outright rather than accepting a signature nothing can lower.
#[test]
fn top_foreign_rejects_nested_list() {
    assert!(
        "foreign frobnicate(List(List(Nat))) -> Bool;"
            .parse::<Module>()
            .is_err()
    );
}

/// The wire grammar's own refusals reach the reader: uncommitted, each would leave the parameter list to come back empty and the `)` after it to complain about a paren for a mistake about a type.
#[test]
fn top_foreign_names_the_wire_vocabulary_it_refused() {
    for (source, expected) in [
        (
            "foreign f(Str) -> Nat;",
            "expected a wire type (Nat, Int, Bool, Byte, Flt, Bytes, Handle, List(...), or Option(...)), found 'Str'",
        ),
        // A result goes through the same parser, so it names the same vocabulary.
        (
            "foreign f() -> Str;",
            "expected a wire type (Nat, Int, Bool, Byte, Flt, Bytes, Handle, List(...), or Option(...)), found 'Str'",
        ),
        (
            "foreign f(List(List(Nat))) -> Bool;",
            "expected a List or Option element type (Nat, Int, Bool, Bytes, or Handle — neither nests), found 'List'",
        ),
        (
            "foreign f(Option(List(Nat))) -> Bool;",
            "expected a List or Option element type (Nat, Int, Bool, Bytes, or Handle — neither nests), found 'List'",
        ),
        (
            "foreign f(List(Option(Nat))) -> Bool;",
            "expected a List or Option element type (Nat, Int, Bool, Bytes, or Handle — neither nests), found 'Option'",
        ),
        // A `/` stops `parse_identifier` before either arm is reached, so the qualified spelling is refused by name of its own.
        (
            "foreign f(/std/Nat) -> Nat;",
            "a wire type is written bare: `Nat` rather than `/std/Nat`",
        ),
    ] {
        let report = source.parse::<Module>().unwrap_err().format();
        assert!(report.contains(expected), "{source:?} reported {report}");
    }
    // `parse_identifier` fails on a non-identifier, so no refusal fires at the `)` and the empty parameter list still parses.
    assert!("foreign f() -> Nat;".parse::<Module>().is_ok());
}

/// One level of `List` still parses, over each leaf the wire admits.
#[test]
fn top_foreign_list_of_leaf() {
    assert_eq!(
        "foreign frobnicate(List(Bytes), List(Handle)) -> List(Nat);"
            .parse::<Module>()
            .unwrap()
            .items,
        vec![TopItem::Foreign(TopForeign {
            doc: None,
            vis_pub: false,
            label: "frobnicate".into(),
            signature: WireSignature {
                params: vec![
                    WireType::List(WireLeaf::Bytes),
                    WireType::List(WireLeaf::Handle)
                ],
                returns: WireReturn::Answers(WireResults::single(WireType::List(WireLeaf::Nat))),
            },
        })]
    );
}

/// One level of `Option` parses too, over each leaf the wire admits.
#[test]
fn top_foreign_option_of_leaf() {
    assert_eq!(
        "foreign maybe(Option(Nat), Option(Bytes)) -> Option(Handle);"
            .parse::<Module>()
            .unwrap()
            .items,
        vec![TopItem::Foreign(TopForeign {
            doc: None,
            vis_pub: false,
            label: "maybe".into(),
            signature: WireSignature {
                params: vec![
                    WireType::Option(WireLeaf::Nat),
                    WireType::Option(WireLeaf::Bytes)
                ],
                returns: WireReturn::Answers(WireResults::single(WireType::Option(
                    WireLeaf::Handle
                ))),
            },
        })]
    );
}

#[test]
fn top_foreign_rejects_non_wire_type() {
    assert!("foreign frobnicate() -> Str;".parse::<Module>().is_err());
}

// A list of bytes is the `Bytes` a row already spells, so `Byte` is a wire type but not a list element.
#[test]
fn a_list_of_bytes_is_not_a_wire_type() {
    let refusal = "foreign f(List(Byte)) -> Nat;"
        .parse::<Module>()
        .unwrap_err()
        .format();

    assert!(
        refusal.contains("expected a List or Option element type"),
        "{refusal}"
    );
}

#[test]
fn foreign_declaration_round_trips() {
    for source in [
        "foreign frobnicate(Nat, Bytes) -> Nat;",
        "pub foreign frobnicate(Nat, Bytes) -> Nat;",
        "foreign clock() -> Nat;",
        "foreign frobnicate(List(Bytes), List(Handle)) -> List(Nat);",
        "foreign maybe(Option(Nat), Option(Bytes)) -> {found: Bool, value: Option(Handle)};",
        "foreign close(Handle) -> {};",
        "foreign flip(Byte) -> Byte;",
        "foreign exited() -> {ok: Bool, code: Byte};",
        "foreign wall() -> {secs: Nat, nanos: Nat};",
        "foreign read(Handle, Nat) -> {status: Nat, bytes: Bytes};",
        "foreign abort(Byte) -> Never;",
        "pub foreign gone() -> Never;",
    ] {
        let module = source.parse::<Module>().unwrap();

        assert_eq!(
            module.to_string().parse::<Module>().unwrap(),
            module,
            "round-trip failed for {source:?}"
        );
    }
}

/// A `foreign` is written as a function with no body: its operand list follows its name, empty where it takes none, and no colon stands between them. A result alone and the colon are no spelling of one, and `Never` follows a list as any result does.
#[test]
fn top_foreign_is_written_as_a_function_with_no_body() {
    for source in [
        "foreign clock : Nat;",
        "foreign clock -> Nat;",
        "foreign clock : () -> Nat;",
        "foreign clock : (Nat) -> Nat;",
    ] {
        assert!(
            source.parse::<Module>().is_err(),
            "{source:?} is no declaration"
        );
    }

    assert_eq!(
        "foreign gone() -> Never;".parse::<Module>().unwrap().items,
        vec![TopItem::Foreign(TopForeign {
            doc: None,
            vis_pub: false,
            label: "gone".into(),
            signature: WireSignature {
                params: vec![],
                returns: WireReturn::Never,
            },
        })]
    );
    assert!(
        "foreign clock() -> Nat;"
            .parse::<Module>()
            .unwrap()
            .to_string()
            .contains("foreign clock() -> Nat;")
    );
}
