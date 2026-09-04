//! The `Flt` codec against Rust's own encoding: the little-endian round trip byte-for-byte, and the decimal round trip over runtime-tainted values so the pair runs in emitted Wasm rather than folding. The narrowings that answer partiality with an `Option` are the corpus's `/numeric`.

use crate::tests::run;

/// The codec round-trips at runtime: `to_str` prints the shortest decimal that reads back as its input, and `of_str` reads it back exactly. Every value is scaled by a runtime-tainted `1.0` so the whole pair runs in emitted Wasm rather than folding at compile time; a closed program would only exercise the partial evaluator. Before `of_str` was exact, about forty percent of finite values failed this.
#[test]
fn codec_round_trips_on_runtime_values() {
    let source = r#"
        use /std/{Handle, Str, Nat, Flt, Bytes, Option, List, Bool, Io};
        let one = Nat/to_flt(Bytes/len(/std/rand/bytes(3)!)) / +3.0;
        let check(x : Flt) -> Str =
            let back = Option/unwrap_or(Flt/of_str(Flt/to_str(x)), Flt/nan);
            match Bytes/eql(Flt/to_le_bytes(back), Flt/to_le_bytes(x))
            | true => "ok"
            | false => Str/concat(Flt/to_str(x), Str/concat(" -> ", Flt/to_str(back)))
            end;
        let values = [
            +3.4028235e38, +1.1754944e-38, +1.0e-45, +2.137381e-39, +0.1, +123456.79,
            +1.2345679e-5, +9.999999e9, +16777216.0, +0.30000001, +7.1551326e37,
            +7.141006e-33, +7.734096e-28, +1.7387574e-25, +2.7182817, -0.0, +0.0, -1.5e-40,
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

    assert_eq!(run(source), 1.5f32.to_le_bytes());
}

#[test]
fn flt_of_le_bytes_roundtrips_raw_bytes() {
    // Full-pipeline inverse of `to_le_bytes`: assemble the float back from its four little-endian bytes, then re-serialize. The program is closed, so this also exercises the type-level and optimizer folds of `of_le_bytes`.
    let source = r#"
        let _ = std/Io/write(std/Io/stdout, std/Flt/to_le_bytes(std/Flt/of_le_bytes(std/Flt/to_le_bytes(+1.5))))!;
        /std/Io/pure(())
        "#;

    assert_eq!(run(source), 1.5f32.to_le_bytes());
}
