//! The `/std` surfaces a program reaches at runtime: randomness, parsing, the clock, and cells.

use {
    crate::tests::{run, run_text},
    curios_runtime::MockHost,
};

// A struct type is nominal: it never converts with a structural tuple type of the same fields.
#[test]
fn random_bin_returns_requested_length() {
    let output = run(r#"
let _ = std/Handle/write(std/Handle/stdout, /std/rand/bytes(8)!)!;
/std/Io/pure(())
"#);
    assert_eq!(output.len(), 8);
}

#[test]
fn nat_of_str_returns_option() {
    // `123` parses; `12a` (non-digit) and the empty string are `none`, taking the `unwrap_or` defaults — `123 + 7 + 9`.
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Option, Handle};
        let ok = Option/unwrap_or(Nat/of_str("123"), 0);
        let bad = Option/unwrap_or(Nat/of_str("12a"), 7);
        let empty = Option/unwrap_or(Nat/of_str(""), 9);
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(Nat/add(Nat/add(ok, bad), empty))))!;
        /std/Io/pure(())
        "#),
        b"139"
    );
}

#[test]
fn int_of_str_returns_option() {
    // `-5` and `+7` parse (compared by magnitude); `x` is `none` → default `+3`.
    assert_eq!(
        run(r#"
        use /std/{Nat, Int, Str, Option, Handle};
        let neg = Int/abs(Option/unwrap_or(Int/of_str("-5"), +0));
        let pos = Int/abs(Option/unwrap_or(Int/of_str("+7"), +0));
        let bad = Int/abs(Option/unwrap_or(Int/of_str("x"), +3));
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(Nat/add(Nat/add(neg, pos), bad))))!;
        /std/Io/pure(())
        "#),
        b"15"
    );
}

#[test]
fn flt_of_str_returns_option() {
    // `12.0`, `.5` (empty integer part), and `1e3` parse; `abc` is `none` → default `+4.0`. Values are truncated to `Nat` for an exact assertion: `12 + (0.5*2) + 1000 + 4`.
    //
    // The bounded `to_nat` over a *computed* subject: discharging `NonNeg` here runs `Flt/of_str`'s decimal narrowing at elaboration time, which is what a decided bound over a computed subject costs. It is affordable because the universe-erased projection a `Nat` comparison takes is memoized — see `curios-core`'s `Mode::ErasingUniverses`. Written the direct way deliberately, so this is the fixture that notices if it stops being affordable.
    assert_eq!(
        run(r#"
        use /std/{Nat, Flt, Str, Option, Handle};
        let whole = Flt/to_nat(Option/unwrap_or(Flt/of_str("12.0"), +0.0));
        let half = Flt/to_nat(Flt/mul(Option/unwrap_or(Flt/of_str(".5"), +0.0), +2.0));
        let exp = Flt/to_nat(Option/unwrap_or(Flt/of_str("1e3"), +0.0));
        let bad = Flt/to_nat(Option/unwrap_or(Flt/of_str("abc"), +4.0));
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(Nat/add(Nat/add(whole, half), Nat/add(exp, bad)))))!;
        /std/Io/pure(())
        "#),
        b"1017"
    );
}

#[test]
fn option_result_char_helpers() {
    assert_eq!(
        run(r#"
        use /std/{Option, Result, Char, Nat, Str, Handle};
        let opt = Option/unwrap_or(Option/map(Option/some(4), (x : Nat) => Nat/add(x, 1)), 0);
        let res0 : Result(Nat, Nat) = Result/success(5);
        let res = Result/unwrap_or(Result/map_success(res0, (x : Nat) => Nat/mul(x, 2)), 0);
        let up = Char/to_ascii_upper('a');
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(Nat/add(Nat/add(opt, res), Char/to_nat(up)))))!;
        /std/Io/pure(())
        "#),
        // opt = 5, res = 10, up = 'A' = 65  ->  80
        b"80",
    );
}

#[test]
fn clock_diff_of_two_distinct_now_readings() {
    // Two scripted wall readings 30 s + 400 ns apart. `time/Instant/now` referenced twice must perform two *distinct* host calls (the nullary-effect distinctness the struct-head reduction relies on), so the diff is the gap between them, not zero.
    let (system, io) = MockHost::builder()
        .wall([(1, 100, 500), (1, 130, 900)])
        .build();
    run_text(r#"
        let a = /std/time/Instant/now()!;
        let b = /std/time/Instant/now()!;
        let d = /std/time/Instant/diff(b, a);
        let _ = std/Handle/write(std/Handle/stdout, /std/Str/to_bytes(/std/Nat/to_str(/std/time/Duration/secs(d))))!;
        /std/Io/pure(())
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(io.output(), b"30");
}

#[test]
fn clock_mono_reads_scripted_elapsed() {
    let (system, io) = MockHost::builder().mono([(2, 7)]).build();
    run_text(r#"
        let e = /std/time/Duration/now()!;
        let _ = std/Handle/write(std/Handle/stdout, /std/Str/to_bytes(/std/Nat/to_str(/std/time/Duration/secs(e))))!;
        /std/Io/pure(())
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(io.output(), b"2");
}

#[test]
fn cell_get_returns_init_value() {
    // Round-trip: mint a cell then read it back.
    assert_eq!(
        run(r#"
            use /std/{Cell, Handle, Nat, Str};
            let n : Nat = 42;
            let cell = Cell/new(n)!;
            /std/print(Nat/to_str(Cell/get(cell)!))
        "#),
        b"42",
    );
}

#[test]
fn cell_set_overwrites_value() {
    // Write then read: the getter sees the new value, not the init.
    assert_eq!(
        run(r#"
            use /std/{Cell, Handle, Nat, Str};
            let z : Nat = 0;
            let cell = Cell/new(z)!;
            let _ = Cell/set(cell, 99)!;
            /std/print(Nat/to_str(Cell/get(cell)!))
        "#),
        b"99",
    );
}

#[test]
fn cell_two_cells_are_distinct() {
    // Two cells minted with the same value are independent heap objects. Setting one must not affect the other.
    assert_eq!(
        run(r#"
            use /std/{Cell, Handle, Nat, Str};
            let n : Nat = 7;
            let a = Cell/new(n)!;
            let b = Cell/new(n)!;
            let _ = Cell/set(a, 1)!;
            /std/print(Nat/to_str(Cell/get(b)!))
        "#),
        b"7",
    );
}
