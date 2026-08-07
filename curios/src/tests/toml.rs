use super::run;

// Closed decode calls are unrolled by the type-level evaluator, whose Rust stack budget on default test threads caps document size, so closed inputs here stay small. Two rand-derived taints route larger or deliberately runtime-path documents to the emitted wasm instead: `Str/slice` from a runtime-opaque zero for whole small documents, and digit runs repeated a `(opaque + 1) * n` number of times — stuck at compile time because `Nat` multiplication has no definitional laws — for the long binary literals.

#[test]
fn toml_scalar_documents_round_trip_deterministically() {
    let source = r#"
        use /std/{Handle, Str, Toml, Result, Lst};
        let canon(input : Str) -> Str =
            match Toml/decode(input)
            | failure(msg) => Str/concat("reject:", msg)
            | success(root) =>
                match Toml/encode(root)
                | success(out) => out
                | failure(msg) => Str/concat("encode-fail:", msg)
                end
            end;
        /std/print(Str/join("|", [
            canon("i = 42"),
            canon("s = \"hi\""),
            canon("b = true"),
            canon("f = 3.5"),
            canon("neg = -1073741824"),
            canon("arr = [1, \"two\", 3.5]"),
            canon("[t]\nx = 1"),
            canon("\"dot.ted\" = 3"),
            canon("")
        ]))
        "#;

    assert_eq!(
        run(source),
        b"i = 42\n|s = \"hi\"\n|b = true\n|f = 3.5\n|neg = -1073741824\n|arr = [1, \"two\", 3.5]\n|[t]\nx = 1\n|\"dot.ted\" = 3\n|"
    );
}

#[test]
fn toml_string_forms_and_escapes_decode() {
    let source = r#"
        use /std/{Handle, Str, Toml, Result, Lst};
        let canon(input : Str) -> Str =
            match Toml/decode(input)
            | failure(msg) => Str/concat("reject:", msg)
            | success(root) =>
                match Toml/encode(root)
                | success(out) => out
                | failure(msg) => Str/concat("encode-fail:", msg)
                end
            end;
        /std/print(Str/join("|", [
            canon("e = \"a\tb\\u0041\\U0001F600\""),
            canon("l = 'C:\\path'"),
            canon("m = \"\"\"\nhi\n\"\"\""),
            canon("w = \"\"\"one \\\n  two\"\"\""),
            canon("r = '''a'b''c'''"),
            canon("q = \"\"\"a \"\"b\"\"\"\"\"")
        ]))
        "#;

    assert_eq!(
        run(source),
        "e = \"a\\tbA😀\"\n|l = \"C:\\\\path\"\n|m = \"hi\\n\"\n|w = \"one two\"\n|r = \"a'b''c\"\n|q = \"a \\\"\\\"b\\\"\\\"\"\n".as_bytes()
    );
}

#[test]
fn toml_date_times_cover_the_rfc3339_subset() {
    let source = r#"
        use /std/{Handle, Str, Toml, Result, Lst};
        let canon(input : Str) -> Str =
            match Toml/decode(input)
            | failure(msg) => Str/concat("reject:", msg)
            | success(root) =>
                match Toml/encode(root)
                | success(out) => out
                | failure(msg) => Str/concat("encode-fail:", msg)
                end
            end;
        /std/print(Str/join("|", [
            canon("d = 1979-05-27T07:32:00Z"),
            canon("d = 1979-05-27 07:32:00Z"),
            canon("d = 1979-05-27T00:32:00-07:00"),
            canon("d = 07:32:00"),
            canon("d = 1979-05-27t07:32:60+23:59"),
            canon("d = 1979-05-27"),
            canon("d = 2000-02-29"),
            canon("d = 00:00:00.1234567891"),
            canon("d = 00:00:00.500")
        ]))
        "#;

    assert_eq!(
        run(source),
        b"d = 1979-05-27T07:32:00Z\n|d = 1979-05-27T07:32:00Z\n|d = 1979-05-27T00:32:00-07:00\n|d = 07:32:00\n|d = 1979-05-27T07:32:60+23:59\n|d = 1979-05-27\n|d = 2000-02-29\n|d = 00:00:00.123456789\n|d = 00:00:00.5\n"
    );
}

#[test]
fn toml_integer_boundaries_hold_in_every_radix() {
    let source = r#"
        use /std/{Handle, Str, Toml, Result, Lst, Bytes, Nat, rand};
        let taint = Bytes/len(rand/bytes(0)!);
        let opaque_n(n : Nat) -> Nat = (taint + 1) * n;
        rec run_of(ch : Str, k : Nat) -> Str =
            match k == 0
            | true => ""
            | false => Str/concat(ch, run_of(ch, k - 1))
            end;
        let canon(input : Str) -> Str =
            match Toml/decode(input)
            | failure(_) => "reject"
            | success(root) =>
                match Toml/encode(root)
                | success(out) => out
                | failure(msg) => Str/concat("encode-fail:", msg)
                end
            end;
        /std/print(Str/join("|", [
            canon("i = 1073741823"),
            canon("i = -1073741824"),
            canon("i = 0x3fff_ffff"),
            canon("i = 0o7777777777"),
            canon(Str/concat("i = 0b", run_of("1", opaque_n(30)))),
            canon("i = 0x0000_0001"),
            canon("i = +0"),
            canon("i = -0"),
            canon("i = 1073741824"),
            canon("i = -1073741825"),
            canon("i = 0x40000000"),
            canon("i = 0o10000000000"),
            canon(Str/concat("i = 0b1", run_of("0", opaque_n(30)))),
            canon("i = 42949672960"),
            canon("i = 9999999999999999999")
        ]))
        "#;

    assert_eq!(
        run(source),
        b"i = 1073741823\n|i = -1073741824\n|i = 1073741823\n|i = 1073741823\n|i = 1073741823\n|i = 1\n|i = 0\n|i = 0\n|reject|reject|reject|reject|reject|reject|reject"
    );
}

#[test]
fn toml_floats_pin_binary32_bit_patterns() {
    let source = r#"
        use /std/{Handle, Str, Toml, Result, Lst, Map, Option, Byte, Bytes, Char, Nat, Flt};
        use /std/Toml/{flt};
        let hexs(b : Bytes) -> Str =
            Bytes/fold(b, "", (byte, acc) =>
                let n = Byte/to_nat(byte);
                let hi = Option/unwrap_or(Str/of_bytes(Char/to_utf8(Char/hex_digit(n / 16))), "");
                let lo = Option/unwrap_or(Str/of_bytes(Char/to_utf8(Char/hex_digit(n % 16))), "");
                Str/flatten([acc, hi, lo]));
        let fbits(input : Str) -> Str =
            match Toml/decode(input)
            | failure(_) => "reject"
            | success(root) =>
                match Map/get(root, "f")
                | some(v) =>
                    match v : (_) => Str
                    | flt(x) => hexs(Flt/to_le_bytes(x))
                    | _ => "not-flt"
                    end
                | none() => "missing"
                end
            end;
        /std/print(Str/join("|", [
            fbits("f = 0.0"), fbits("f = -0.0"), fbits("f = 1.0"), fbits("f = 1.5"),
            fbits("f = 3.5"), fbits("f = -1.5"), fbits("f = 0.25"),
            fbits("f = inf"), fbits("f = -inf"), fbits("f = nan"), fbits("f = -nan"),
            fbits("f = 1e39"), fbits("f = -1e39"), fbits("f = 1e-50"),
            fbits("f = 123456789.0"), fbits("f = 1234567890.0"), fbits("f = 1234567891.0"),
            fbits("f = 1_2.5e1_0")
        ]))
        "#;

    assert_eq!(
        run(source),
        b"00000000|00000080|0000803f|0000c03f|00006040|0000c0bf|0000803e|0000807f|000080ff|0000c07f|0000c07f|0000807f|000080ff|00000000|a379eb4c|062c934e|062c934e|a5d4e851"
    );
}

#[test]
fn toml_malformed_numbers_and_escapes_reject() {
    let source = r#"
        use /std/{Handle, Str, Toml, Result, Lst};
        let ok(input : Str) -> Str =
            match Toml/decode(input)
            | failure(_) => "reject"
            | success(_) => "accept"
            end;
        /std/print(Str/join("|", [
            ok("i = 1__2"), ok("i = _1"), ok("i = 1_"), ok("i = 01"),
            ok("f = 01.0"), ok("f = 1."), ok("f = .5"), ok("f = 1e"),
            ok("s = \"\\uD800\""), ok("s = \"\\q\""),
            ok("d = 2021-02-30"), ok("t = 24:00:00"), ok("t = 00:00:61"),
            ok("d = 1979-05-27T00:00:00+24:00"), ok("k = truex")
        ]))
        "#;

    assert_eq!(
        run(source),
        b"reject|reject|reject|reject|reject|reject|reject|reject|reject|reject|reject|reject|reject|reject|reject"
    );
}

#[test]
fn toml_table_construction_conflicts_reject() {
    let source = r#"
        use /std/{Handle, Str, Toml, Result, Lst};
        let ok(input : Str) -> Str =
            match Toml/decode(input)
            | failure(_) => "reject"
            | success(_) => "accept"
            end;
        /std/print(Str/join("|", [
            ok("dup = 1\ndup = 2"),
            ok("[a]\n[a]"),
            ok("a.b = 1\n[a]"),
            ok("[t]\na.b = 1\n[t.a]"),
            ok("t = {a = 1}\n[t]"),
            ok("t = {a = 1}\nt.b = 2"),
            ok("a = 1\n[a]"),
            ok("a = []\n[[a]]"),
            ok("[a]\n[[a]]"),
            ok("[[a]]\n[a]"),
            ok("t = {a = 1, a = 2}"),
            ok("[a.b]\n[a]"),
            ok("[[a]]\nb = 1\n[[a]]\nb = 1"),
            ok("p.q = 1\np.r = 2")
        ]))
        "#;

    assert_eq!(
        run(source),
        b"reject|reject|reject|reject|reject|reject|reject|reject|reject|reject|reject|accept|accept|accept"
    );
}

#[test]
fn toml_arrays_and_inline_tables_nest_and_reach_a_fixpoint() {
    let source = r#"
        use /std/{Handle, Str, Toml, Result, Lst, Bool};
        let canon(input : Str) -> Str =
            match Toml/decode(input)
            | failure(msg) => Str/concat("reject:", msg)
            | success(root) =>
                match Toml/encode(root)
                | success(out) => out
                | failure(msg) => Str/concat("encode-fail:", msg)
                end
            end;
        let doc = "[[a]]\nb = { c = 2 }\n[a.d]\ne = 1";
        let c1 = canon(doc);
        let c2 = canon(c1);
        /std/print(Str/flatten([c1, "|", Bool/to_str(Str/eql(c1, c2)), "|", canon("b = [1, [2]]")]))
        "#;

    assert_eq!(
        run(source),
        b"a = [{ b = { c = 2 }, d = { e = 1 } }]\n|true|b = [1, [2]]\n"
    );
}

#[test]
fn toml_decode_and_encode_execute_in_emitted_wasm() {
    let source = r#"
        use /std/{Handle, Str, Toml, Result, Lst, Bytes, rand};
        let taint = Bytes/len(rand/bytes(0)!);
        let opaque(s : Str) -> Str = Str/slice(s, taint, Str/len(s));
        let canon(input : Str) -> Str =
            match Toml/decode(input)
            | failure(_) => "reject"
            | success(root) =>
                match Toml/encode(root)
                | success(out) => out
                | failure(msg) => Str/concat("encode-fail:", msg)
                end
            end;
        /std/print(Str/join("|", [
            canon(opaque("a = 1")),
            canon(opaque("t = true")),
            canon(opaque("b = [1]")),
            canon(opaque("1 = ="))
        ]))
        "#;

    assert_eq!(run(source), b"a = 1\n|t = true\n|b = [1]\n|reject");
}

#[test]
fn toml_encode_rejects_a_non_utf8_key() {
    let source = r#"
        use /std/{Handle, Str, Toml, Result, Map, Nat, Bytes, rand};
        let taint = Bytes/len(rand/bytes(0)!);
        let opaque = Nat/to_int(taint + 1);
        let outcome : Str =
            match Toml/encode(Map/insert(Map/empty(@Toml), x[\ff], Toml/int(opaque)))
            | success(_) => "accepted"
            | failure(msg) => msg
            end;
        /std/print(outcome)
        "#;

    assert_eq!(run(source), b"map key is not valid UTF-8");
}

#[test]
fn toml_comments_line_endings_and_trailing_input() {
    let source = r##"
        use /std/{Handle, Str, Toml, Result, Lst};
        let ok(input : Str) -> Str =
            match Toml/decode(input)
            | failure(_) => "reject"
            | success(_) => "accept"
            end;
        /std/print(Str/join("|", [
            ok("a = 1 # note"),
            ok("# full\r\n\nb = 2"),
            ok("# only a comment"),
            ok("   \n\t\n"),
            ok("a = 1\r\nb = 2\r\n"),
            ok("x = 1 junk"),
            ok("[a] junk"),
            ok("= 1")
        ]))
        "##;

    assert_eq!(
        run(source),
        b"accept|accept|accept|accept|accept|reject|reject|reject"
    );
}

#[test]
fn parse_eof_accepts_only_end_of_input() {
    let source = r#"
        use /std/{Handle, Str, Bytes, Nat, Byte, Result, Parse, rand};
        let doc : Parse(Byte) =
            let b = Parse/any_byte!;
            let _ = Parse/eof!;
            Parse/pure(b);
        let check(input : Bytes) -> Str =
            match Parse/run(doc, input)
            | success(b) => Str/concat("ok:", Nat/to_str(Byte/to_nat(b)))
            | failure(_) => "rejected"
            end;
        /std/print(Str/flatten([
            check(x[..rand/bytes(0)!, \41]), ";",
            check(x[..rand/bytes(0)!, \41, \42]), ";",
            check(x[..rand/bytes(0)!])
        ]))
        "#;

    assert_eq!(run(source), b"ok:65;rejected;rejected");
}
