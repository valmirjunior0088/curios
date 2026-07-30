use super::run;

// A bare `%` is the generic show-slot: each argument renders through its own `Show` witness, so one format string mixes types freely.
#[test]
fn percent_slot_shows_each_argument_through_its_witness() {
    let source = r#"
        use /std/{Fmt};
        Fmt/print("% / % / %")(42)(true)(3.5)
    "#;
    assert_eq!(run(source), b"42 / true / +3.5");
}

// `Show(Str)` is identity, so a `Str` argument renders verbatim.
#[test]
fn percent_slot_shows_a_string_verbatim() {
    let source = r#"
        use /std/{Fmt};
        Fmt/print("hello, %!")("world")
    "#;
    assert_eq!(run(source), b"hello, world!");
}

// `\%` (spelled `\\%` in source) escapes a literal percent, and — because the escape lead character differs from the placeholder — composes unambiguously next to a slot in either order.
#[test]
fn escaped_percent_renders_literally() {
    let slot_then_literal = r#"
        use /std/{Fmt};
        Fmt/print("%\\% off")(50)
    "#;
    assert_eq!(run(slot_then_literal), b"50% off");

    let literal_then_slot = r#"
        use /std/{Fmt};
        Fmt/print("\\%%")(50)
    "#;
    assert_eq!(run(literal_then_slot), b"%50");
}

// The scalar `Show` witnesses (`Byte` as decimal, `Bytes` as lowercase hex, `Order` by constructor) render through the same slot.
#[test]
fn percent_slot_shows_scalar_witnesses() {
    let source = r#"
        use /std/{Fmt, Nat, Str, Ord};
        Fmt/print("byte=% bytes=% ord=%")(Nat/to_byte(65))(Str/to_bytes("ABC"))(Ord/cmp(9, 4))
    "#;
    assert_eq!(run(source), b"byte=65 bytes=414243 ord=gt");
}

// The container `Show` witnesses render structurally, recursing through the element witness resolved from their `use Show(A)` premise.
#[test]
fn percent_slot_shows_containers() {
    let source = r#"
        use /std/{Fmt, Option, Lst};
        Fmt/print("list=% opt=%")([1, 2, 3])(Option/some(7))
    "#;
    assert_eq!(run(source), b"list=[1, 2, 3] opt=some(7)");
}
