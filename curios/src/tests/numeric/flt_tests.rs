//! The `Flt` codec against Rust's own encoding: the little-endian round trip byte-for-byte, and the decimal round trip over runtime-tainted values so the pair runs in emitted Wasm rather than folding. The narrowings that answer partiality with an `Option` are the corpus's `/numeric`.

use crate::tests::run;

/// The codec round-trips at runtime: `to_str` prints the shortest decimal that reads back as its input, and `of_str` reads it back exactly. Every value is scaled by a runtime-tainted `1.0` so the whole pair runs in emitted Wasm rather than folding at compile time; a closed program would only exercise the partial evaluator. Before `of_str` was exact, about forty percent of finite values failed this.
#[test]
fn codec_round_trips_on_runtime_values() {
    let source = r#"
        use /std/{Str, Nat, Flt, Bytes, Option, List, Bool, Io};
        let one = Nat/to_flt(Bytes/len(/std/rand/bytes(3)!)) / +3.0;
        let check(x : Flt) -> Str =
            let back = Option/unwrap_or(Flt/of_str(Flt/to_str(x)), Flt/nan);
            match Bytes/eql(Flt/to_le_bytes(back), Flt/to_le_bytes(x))
            | true => "ok"
            | false => Str/concat(Flt/to_str(x), Str/concat(" -> ", Flt/to_str(back)))
            end;
        let values = [
            +1.7976931348623157e308, +2.2250738585072014e-308, +5.0e-324, +1.0e-320, +0.1,
            +123456.789012345, +1.2345678901234567e-5, +9.999999999999998e9,
            +9007199254740992.0, +0.30000000000000004, +7.1551326123456785e37,
            +7.141006123456789e-33, +7.734096123456789e-28, +1.7387574123456789e-25,
            +2.718281828459045, -0.0, +0.0, -1.5e-310,
        ];
        /std/print(Str/join("|", List/map(values, (x) => check(x * one))))
        "#;
    let expected = std::iter::repeat_n("ok", 18).collect::<Vec<_>>().join("|");
    assert_eq!(run(source), expected.into_bytes());
}

#[test]
fn flt_to_le_bytes_prints_raw_bytes() {
    let source = r#"
        let _ = std/Io/write(std/Io/stdout, std/Flt/to_le_bytes(+1.5))!;
        /std/Io/pure(())
        "#;

    assert_eq!(run(source), 1.5f64.to_le_bytes());
}

#[test]
fn flt_of_le_bytes_roundtrips_raw_bytes() {
    // Full-pipeline inverse of `to_le_bytes`: assemble the float back from its eight little-endian bytes, then re-serialize. The program is closed, so this also exercises the type-level and optimizer folds of `of_le_bytes`.
    let source = r#"
        let _ = std/Io/write(std/Io/stdout, std/Flt/to_le_bytes(std/Flt/of_le_bytes(std/Flt/to_le_bytes(+1.5))))!;
        /std/Io/pure(())
        "#;

    assert_eq!(run(source), 1.5f64.to_le_bytes());
}
