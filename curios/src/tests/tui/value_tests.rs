//! Colours, styles and cells: what can be built, what compares equal, and what the palette bound refuses.

use crate::tests::{run, typecheck};

#[test]
fn a_palette_index_inside_the_sixteen_discharges_its_bound() {
    let source = r#"
        use /std/{Nat, Bool, Str, Handle, Tui};
        use /std/Tui/{Color};
        let picked: Color = Color/ansi(15);
        /std/print(Str/flatten([Bool/to_str(picked == Color/bright_white), "\n"]))
        "#;

    assert_eq!(run(source), b"true\n");
}

// The bound is a decided proposition, so a literal past the palette is refused where it is written rather than clamped or wrapped at runtime.
#[test]
fn a_palette_index_past_the_sixteen_is_refused() {
    let source = r#"
        use /std/{Handle, Tui};
        use /std/Tui/{Color};
        let picked: Color = Color/ansi(16);
        /std/print("unreachable")
        "#;

    let report = typecheck(source).expect_err("16 is not one of the sixteen");
    assert!(
        report.contains("Lt"),
        "the refusal should name the bound it could not discharge, got: {report}"
    );
}

// The `ansi` payload carries an irrelevant proof beside its index, so the derived comparison is the index comparison and two spellings of the same colour agree.
#[test]
fn a_style_compares_field_by_field() {
    let source = r#"
        use /std/{Bool, Str, Handle, Tui};
        use /std/Tui/{Color, Style, Cell};
        let bold: Style = Style { ..Style/plain, bold = true };
        let same: Bool = Style/plain == Style { ..Style/plain };
        let differs: Bool = Style/plain == bold;
        let cells: Bool = Cell/blank == Cell { symbol = " ", style = Style/plain };
        /std/print(Str/flatten([Bool/to_str(same), Bool/to_str(differs), Bool/to_str(cells), "\n"]))
        "#;

    assert_eq!(run(source), b"truefalsetrue\n");
}
