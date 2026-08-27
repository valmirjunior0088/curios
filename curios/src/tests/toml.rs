//! The TOML codec, decoded and re-encoded in emitted wasm.
//!
//! **Every document in this module is a row of one program, compiled once.** The program is the codec plus a `match` over a row number the host supplies on stdin, and each test runs its own rows against it; `codec()` compiles it the first time any test asks. This is the shape `numeric.rs` uses for its scalar tables, one level up, and it exists for one reason: compiling a program that reaches `Toml/decode` and `Toml/encode` costs the Cont fixpoint seventy-odd rounds over the whole codec — about twenty seconds in release and a minute and more in debug — and that cost is per *program*, not per document. Ten programs of a handful of documents each were ten of those; one program is one.
//!
//! The documents are literals and reach the decoder as written. The evaluator does not unroll a closed decode — a claim this module's header once made, and which the stage profile of such a program refutes: elaboration and `evaluate_closed_terms` together are under a second of it, and the fixpoint is the rest — so nothing here needs a taint to reach the emitted codec. The long binary literals still use one, digit runs repeated a `(opaque + 1) * n` number of times, because a literal of thirty digits is the thing a reader should not have to count.

use {
    super::{Compiled, compile, run},
    curios_runtime::MockHost,
    std::sync::OnceLock,
};

/// One document as a `Str`-valued expression over the program's helpers, and what printing it yields.
struct Row {
    expr: &'static str,
    expected: &'static str,
}

/// Scalar documents through `canon`: decode, encode, and the encoder's output is the document's one spelling.
const SCALARS: &[Row] = &[
    Row {
        expr: r##"canon("i = 42")"##,
        expected: "i = 42\n",
    },
    Row {
        expr: r##"canon("s = \"hi\"")"##,
        expected: "s = \"hi\"\n",
    },
    Row {
        expr: r##"canon("b = true")"##,
        expected: "b = true\n",
    },
    Row {
        expr: r##"canon("f = 3.5")"##,
        expected: "f = 3.5\n",
    },
    Row {
        expr: r##"canon("neg = -1073741824")"##,
        expected: "neg = -1073741824\n",
    },
    Row {
        expr: r##"canon("arr = [1, \"two\", 3.5]")"##,
        expected: "arr = [1, \"two\", 3.5]\n",
    },
    Row {
        expr: r##"canon("[t]\nx = 1")"##,
        expected: "[t]\nx = 1\n",
    },
    Row {
        expr: r##"canon("\"dot.ted\" = 3")"##,
        expected: "\"dot.ted\" = 3\n",
    },
    Row {
        expr: r##"canon("")"##,
        expected: "",
    },
];

/// Every string form and escape, re-encoded to the basic-string spelling.
const STRINGS: &[Row] = &[
    Row {
        expr: r##"canon("e = \"a\tb\\u0041\\U0001F600\"")"##,
        expected: "e = \"a\\tbA😀\"\n",
    },
    Row {
        expr: r##"canon("l = 'C:\\path'")"##,
        expected: "l = \"C:\\\\path\"\n",
    },
    Row {
        expr: r##"canon("m = \"\"\"\nhi\n\"\"\"")"##,
        expected: "m = \"hi\\n\"\n",
    },
    Row {
        expr: r##"canon("w = \"\"\"one \\\n  two\"\"\"")"##,
        expected: "w = \"one two\"\n",
    },
    Row {
        expr: r##"canon("r = '''a'b''c'''")"##,
        expected: "r = \"a'b''c\"\n",
    },
    Row {
        expr: r##"canon("q = \"\"\"a \"\"b\"\"\"\"\"")"##,
        expected: "q = \"a \\\"\\\"b\\\"\\\"\"\n",
    },
];

/// The RFC 3339 subset: offset, local and partial forms, a leap second, and nanosecond truncation.
const DATE_TIMES: &[Row] = &[
    Row {
        expr: r##"canon("d = 1979-05-27T07:32:00Z")"##,
        expected: "d = 1979-05-27T07:32:00Z\n",
    },
    Row {
        expr: r##"canon("d = 1979-05-27 07:32:00Z")"##,
        expected: "d = 1979-05-27T07:32:00Z\n",
    },
    Row {
        expr: r##"canon("d = 1979-05-27T00:32:00-07:00")"##,
        expected: "d = 1979-05-27T00:32:00-07:00\n",
    },
    Row {
        expr: r##"canon("d = 07:32:00")"##,
        expected: "d = 07:32:00\n",
    },
    Row {
        expr: r##"canon("d = 1979-05-27t07:32:60+23:59")"##,
        expected: "d = 1979-05-27T07:32:60+23:59\n",
    },
    Row {
        expr: r##"canon("d = 1979-05-27")"##,
        expected: "d = 1979-05-27\n",
    },
    Row {
        expr: r##"canon("d = 2000-02-29")"##,
        expected: "d = 2000-02-29\n",
    },
    Row {
        expr: r##"canon("d = 00:00:00.1234567891")"##,
        expected: "d = 00:00:00.123456789\n",
    },
    Row {
        expr: r##"canon("d = 00:00:00.500")"##,
        expected: "d = 00:00:00.5\n",
    },
];

/// The i31 envelope in every radix, and the first value past it on each side, rejected.
const INTEGERS: &[Row] = &[
    Row {
        expr: r##"decoded("i = 1073741823")"##,
        expected: "i = 1073741823\n",
    },
    Row {
        expr: r##"decoded("i = -1073741824")"##,
        expected: "i = -1073741824\n",
    },
    Row {
        expr: r##"decoded("i = 0x3fff_ffff")"##,
        expected: "i = 1073741823\n",
    },
    Row {
        expr: r##"decoded("i = 0o7777777777")"##,
        expected: "i = 1073741823\n",
    },
    Row {
        expr: r##"decoded(Str/concat("i = 0b", run_of("1", opaque_n(30))))"##,
        expected: "i = 1073741823\n",
    },
    Row {
        expr: r##"decoded("i = 0x0000_0001")"##,
        expected: "i = 1\n",
    },
    Row {
        expr: r##"decoded("i = +0")"##,
        expected: "i = 0\n",
    },
    Row {
        expr: r##"decoded("i = -0")"##,
        expected: "i = 0\n",
    },
    Row {
        expr: r##"decoded("i = 1073741824")"##,
        expected: "reject",
    },
    Row {
        expr: r##"decoded("i = -1073741825")"##,
        expected: "reject",
    },
    Row {
        expr: r##"decoded("i = 0x40000000")"##,
        expected: "reject",
    },
    Row {
        expr: r##"decoded("i = 0o10000000000")"##,
        expected: "reject",
    },
    Row {
        expr: r##"decoded(Str/concat("i = 0b1", run_of("0", opaque_n(30))))"##,
        expected: "reject",
    },
    Row {
        expr: r##"decoded("i = 42949672960")"##,
        expected: "reject",
    },
    Row {
        expr: r##"decoded("i = 9999999999999999999")"##,
        expected: "reject",
    },
];

/// Float lexemes pinned to their binary32 bit patterns, little-endian hex: the zeros, the specials, overflow and underflow, and the nine-digit boundary.
const FLOAT_BITS: &[Row] = &[
    Row {
        expr: r##"fbits("f = 0.0")"##,
        expected: "00000000",
    },
    Row {
        expr: r##"fbits("f = -0.0")"##,
        expected: "00000080",
    },
    Row {
        expr: r##"fbits("f = 1.0")"##,
        expected: "0000803f",
    },
    Row {
        expr: r##"fbits("f = 1.5")"##,
        expected: "0000c03f",
    },
    Row {
        expr: r##"fbits("f = 3.5")"##,
        expected: "00006040",
    },
    Row {
        expr: r##"fbits("f = -1.5")"##,
        expected: "0000c0bf",
    },
    Row {
        expr: r##"fbits("f = 0.25")"##,
        expected: "0000803e",
    },
    Row {
        expr: r##"fbits("f = inf")"##,
        expected: "0000807f",
    },
    Row {
        expr: r##"fbits("f = -inf")"##,
        expected: "000080ff",
    },
    Row {
        expr: r##"fbits("f = nan")"##,
        expected: "0000c07f",
    },
    Row {
        expr: r##"fbits("f = -nan")"##,
        expected: "0000c07f",
    },
    Row {
        expr: r##"fbits("f = 1e39")"##,
        expected: "0000807f",
    },
    Row {
        expr: r##"fbits("f = -1e39")"##,
        expected: "000080ff",
    },
    Row {
        expr: r##"fbits("f = 1e-50")"##,
        expected: "00000000",
    },
    Row {
        expr: r##"fbits("f = 123456789.0")"##,
        expected: "a379eb4c",
    },
    Row {
        expr: r##"fbits("f = 1234567890.0")"##,
        expected: "062c934e",
    },
    Row {
        expr: r##"fbits("f = 1234567891.0")"##,
        expected: "062c934e",
    },
    Row {
        expr: r##"fbits("f = 1_2.5e1_0")"##,
        expected: "a5d4e851",
    },
];

/// Each malformed input pinned to the *message* it is rejected with, not merely to the fact that it is rejected. The decoder hands these strings to its caller through `Result(_, Str)`, so they are contract rather than debug output — and a reformulation of the scanners into `/std/Parse` combinators can flatten a specific reason into whichever generic message the combinator that happened to fail carries. Accept-versus-reject cannot see that happen; this table can. Every rejection is the refusing parser's own reason: the date-time, fraction and exponent parsers commit once their prefix can be nothing else, so `val_number_digit`'s `or` reports `invalid date` rather than restoring the position, reading `2021` as an integer and failing on the line end — which is what this table showed before `Parse/commit` existed, `expected end of input` for every date-time and `leading zero` for `00:00:61`.
const REASONS: &[Row] = &[
    Row {
        expr: r##"reason("i = 1__2")"##,
        expected: "malformed underscore",
    },
    Row {
        expr: r##"reason("i = _1")"##,
        expected: "malformed underscore",
    },
    Row {
        expr: r##"reason("i = 1_")"##,
        expected: "malformed underscore",
    },
    Row {
        expr: r##"reason("i = 01")"##,
        expected: "leading zero",
    },
    Row {
        expr: r##"reason("f = 01.0")"##,
        expected: "leading zero",
    },
    Row {
        expr: r##"reason("f = 1.")"##,
        expected: "expected digit",
    },
    Row {
        expr: r##"reason("f = .5")"##,
        expected: "expected digit",
    },
    Row {
        expr: r##"reason("f = 1e")"##,
        expected: "expected digit",
    },
    Row {
        expr: r##"reason("s = \"\\uD800\"")"##,
        expected: "invalid Unicode scalar in escape",
    },
    Row {
        expr: r##"reason("s = \"\\q\"")"##,
        expected: "unknown escape",
    },
    Row {
        expr: r##"reason("d = 2021-02-30")"##,
        expected: "invalid date",
    },
    Row {
        expr: r##"reason("t = 24:00:00")"##,
        expected: "invalid time",
    },
    Row {
        expr: r##"reason("t = 00:00:61")"##,
        expected: "invalid time",
    },
    Row {
        expr: r##"reason("d = 1979-05-27T00:00:00+24:00")"##,
        expected: "invalid offset",
    },
    Row {
        expr: r##"reason("k = truex")"##,
        expected: "expected end of input",
    },
];

/// Table construction: a key or table defined twice, a table opened over a dotted key or an inline table, and arrays of tables against tables, against the three that are allowed.
const TABLE_CONFLICTS: &[Row] = &[
    Row {
        expr: r##"verdict("dup = 1\ndup = 2")"##,
        expected: "reject",
    },
    Row {
        expr: r##"verdict("[a]\n[a]")"##,
        expected: "reject",
    },
    Row {
        expr: r##"verdict("a.b = 1\n[a]")"##,
        expected: "reject",
    },
    Row {
        expr: r##"verdict("[t]\na.b = 1\n[t.a]")"##,
        expected: "reject",
    },
    Row {
        expr: r##"verdict("t = {a = 1}\n[t]")"##,
        expected: "reject",
    },
    Row {
        expr: r##"verdict("t = {a = 1}\nt.b = 2")"##,
        expected: "reject",
    },
    Row {
        expr: r##"verdict("a = 1\n[a]")"##,
        expected: "reject",
    },
    Row {
        expr: r##"verdict("a = []\n[[a]]")"##,
        expected: "reject",
    },
    Row {
        expr: r##"verdict("[a]\n[[a]]")"##,
        expected: "reject",
    },
    Row {
        expr: r##"verdict("[[a]]\n[a]")"##,
        expected: "reject",
    },
    Row {
        expr: r##"verdict("t = {a = 1, a = 2}")"##,
        expected: "reject",
    },
    Row {
        expr: r##"verdict("[a.b]\n[a]")"##,
        expected: "accept",
    },
    Row {
        expr: r##"verdict("[[a]]\nb = 1\n[[a]]\nb = 1")"##,
        expected: "accept",
    },
    Row {
        expr: r##"verdict("p.q = 1\np.r = 2")"##,
        expected: "accept",
    },
];

/// Comments, CRLF and blank lines accepted; trailing junk after a value, a header or an empty key rejected.
const COMMENTS: &[Row] = &[
    Row {
        expr: r##"verdict("a = 1 # note")"##,
        expected: "accept",
    },
    Row {
        expr: r##"verdict("# full\r\n\nb = 2")"##,
        expected: "accept",
    },
    Row {
        expr: r##"verdict("# only a comment")"##,
        expected: "accept",
    },
    Row {
        expr: r##"verdict("   \n\t\n")"##,
        expected: "accept",
    },
    Row {
        expr: r##"verdict("a = 1\r\nb = 2\r\n")"##,
        expected: "accept",
    },
    Row {
        expr: r##"verdict("x = 1 junk")"##,
        expected: "reject",
    },
    Row {
        expr: r##"verdict("[a] junk")"##,
        expected: "reject",
    },
    Row {
        expr: r##"verdict("= 1")"##,
        expected: "reject",
    },
];

/// Arrays and inline tables nest, and the encoder's output is a fixed point of decode-then-encode: the second row re-encodes the first row's output and compares.
const NESTING: &[Row] = &[
    Row {
        expr: r##"canon("[[a]]\nb = { c = 2 }\n[a.d]\ne = 1")"##,
        expected: "a = [{ b = { c = 2 }, d = { e = 1 } }]\n",
    },
    Row {
        expr: r##"let c1 = canon("[[a]]\nb = { c = 2 }\n[a.d]\ne = 1"); Bool/to_str(Str/eql(c1, canon(c1)))"##,
        expected: "true",
    },
    Row {
        expr: r##"canon("b = [1, [2]]")"##,
        expected: "b = [1, [2]]\n",
    },
];

/// The float lexemes the old nine-digit significand scaled by repeated `pow10` multiplication got wrong — a mantissa past nine digits, exponents past `10^10`, subnormals, and the overflow boundary, where `3.4028236e38` is above the largest finite value yet rounds down to it — each pinned to the bit pattern Rust's correctly rounded parser gives. These rows' expectations are computed rather than written, so they are not a `Row` table.
const ROUNDED_FLOATS: &[&str] = &[
    "1.2345679e-5",
    "123456.79",
    "7.1551326e37",
    "3.4028235e38",
    "3.4028236e38",
    "3.4028237e38",
    "2.137381e-39",
    "1.0e-45",
    "1.1754942e-38",
    "0.1000000000000000055511151231257827",
    "1234567891.0",
    "9.999999999e9",
    "1.00000006",
    "-1.5e-40",
    "1_2.5e1_0",
];

/// Every table, in the order the program numbers their rows.
const TABLES: &[&[Row]] = &[
    SCALARS,
    STRINGS,
    DATE_TIMES,
    INTEGERS,
    FLOAT_BITS,
    REASONS,
    TABLE_CONFLICTS,
    COMMENTS,
    NESTING,
];

/// The rounded-float rows follow every `Row` table.
fn rounded_float_rows() -> Vec<(String, String)> {
    ROUNDED_FLOATS
        .iter()
        .map(|input| {
            let value = input
                .replace('_', "")
                .parse::<f32>()
                .expect("a float the oracle parses");
            let hex = value
                .to_le_bytes()
                .iter()
                .map(|byte| format!("{byte:02x}"))
                .collect::<String>();
            (format!("fbits(\"f = {input}\")"), hex)
        })
        .collect()
}

/// Every row's expression, in program order.
fn all_rows() -> Vec<String> {
    TABLES
        .iter()
        .flat_map(|table| table.iter().map(|row| row.expr.to_string()))
        .chain(rounded_float_rows().into_iter().map(|(expr, _)| expr))
        .collect()
}

/// The program: the helpers every table is written against, then one arm per row. The row number arrives as a decimal line on stdin.
fn program() -> String {
    let arms = all_rows()
        .iter()
        .enumerate()
        .map(|(index, expr)| format!("| {index} => /std/print({expr})"))
        .collect::<Vec<_>>()
        .join("\n        ");
    format!(
        r##"
        use /std/{{Handle, Str, Toml, Result, List, Map, Option, Byte, Bytes, Char, Nat, Flt, Bool, rand}};
        use /std/Toml/{{flt}};
        let taint = Bytes/len(rand/bytes(0)!);
        let opaque_n(n : Nat) -> Nat = (taint + 1) * n;
        rec run_of(ch : Str, k : Nat) -> Str =
            match k == 0
            | true => ""
            | false => Str/concat(ch, run_of(ch, k - 1))
            end;
        let canon(input : Str) -> Str =
            match Toml/decode(input)
            | failure(msg) => Str/concat("reject:", msg)
            | success(root) =>
                match Toml/encode(root)
                | success(out) => out
                | failure(msg) => Str/concat("encode-fail:", msg)
                end
            end;
        let decoded(input : Str) -> Str =
            match Toml/decode(input)
            | failure(_) => "reject"
            | success(root) =>
                match Toml/encode(root)
                | success(out) => out
                | failure(msg) => Str/concat("encode-fail:", msg)
                end
            end;
        let reason(input : Str) -> Str =
            match Toml/decode(input)
            | failure(msg) => msg
            | success(_) => "accept"
            end;
        let verdict(input : Str) -> Str =
            match Toml/decode(input)
            | failure(_) => "reject"
            | success(_) => "accept"
            end;
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
        let line = match Handle/read(Handle/stdin, 16)! : (_) => Bytes
            | chunk(b) => b
            | eof() => x[]
            | error(_) => x[]
            end;
        let row = Option/unwrap_or(Nat/of_str(Str/trim(Option/unwrap_or(Str/of_bytes(line), ""))), 999);
        match row
        {arms}
        | _ => /std/print("no such row")
        end
        "##
    )
}

/// The compiled program, built once for the module.
fn codec() -> &'static Compiled {
    static CODEC: OnceLock<Compiled> = OnceLock::new();
    CODEC.get_or_init(|| compile(&program()).expect("the codec program compiles"))
}

/// What row `index` prints.
fn run_row(index: usize) -> String {
    let (system, io) = MockHost::builder().stdin_lines([index.to_string()]).build();
    codec().run(system).expect("a row runs");
    String::from_utf8(io.output()).expect("a row prints text")
}

/// The program-order index of `table`'s first row.
fn offset_of(table: &[Row]) -> usize {
    TABLES
        .iter()
        .take_while(|candidate| !std::ptr::eq(**candidate, table))
        .map(|candidate| candidate.len())
        .sum()
}

/// Run every row of `table` and hold each to its expectation.
fn check(table: &'static [Row]) {
    let offset = offset_of(table);
    for (index, row) in table.iter().enumerate() {
        assert_eq!(run_row(offset + index), row.expected, "for: {}", row.expr);
    }
}

#[test]
fn scalar_documents_round_trip_deterministically() {
    check(SCALARS);
}

#[test]
fn string_forms_and_escapes_decode() {
    check(STRINGS);
}

#[test]
fn date_times_cover_the_rfc3339_subset() {
    check(DATE_TIMES);
}

#[test]
fn integer_boundaries_hold_in_every_radix() {
    check(INTEGERS);
}

#[test]
fn floats_pin_binary32_bit_patterns() {
    check(FLOAT_BITS);
}

#[test]
fn floats_are_correctly_rounded() {
    let offset = TABLES.iter().map(|table| table.len()).sum::<usize>();
    for (index, (expr, expected)) in rounded_float_rows().iter().enumerate() {
        assert_eq!(run_row(offset + index), *expected, "for: {expr}");
    }
}

#[test]
fn malformed_numbers_and_escapes_reject() {
    check(REASONS);
}

#[test]
fn table_construction_conflicts_reject() {
    check(TABLE_CONFLICTS);
}

#[test]
fn arrays_and_inline_tables_nest_and_reach_a_fixpoint() {
    check(NESTING);
}

#[test]
fn comments_line_endings_and_trailing_input() {
    check(COMMENTS);
}

/// The encoder on a map built by hand rather than decoded, which is the one path no row above reaches.
#[test]
fn encode_rejects_a_non_utf8_key() {
    let source = r#"
        use /std/{Handle, Str, Toml, Result, Map, Nat, Bytes, rand};
        let taint = Bytes/len(rand/bytes(0)!);
        let opaque = Nat/to_int(taint + 1);
        let outcome : Str =
            match Toml/encode(Map/insert(Map/empty(@Toml), x[0xff], Toml/int(opaque)))
            | success(_) => "accepted"
            | failure(msg) => msg
            end;
        /std/print(outcome)
        "#;

    assert_eq!(run(source), b"map key is not valid UTF-8");
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
            check(x[..rand/bytes(0)!, 0x41]), ";",
            check(x[..rand/bytes(0)!, 0x41, 0x42]), ";",
            check(x[..rand/bytes(0)!])
        ]))
        "#;

    assert_eq!(run(source), b"ok:65;rejected;rejected");
}
