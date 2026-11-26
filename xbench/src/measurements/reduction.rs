//! What a sequence costs to build at the *type* level, measured against what the same loop costs at runtime: the construction `curios`'s `tests::reduction` states, `Bytes/slice` over a computed accumulator running the whole accumulation at elaboration time.
//!
//! Three arms divide that cost, and the division is the point: the middle arm performs the same number of transitions as the last one and constructs nothing, so whatever separates them is construction rather than machinery.
//!
//! Both sequence carriers are measured, `Bytes` and `List`, whose *representations* differ.
//!
//! A second probe reads the same programs the other way round. [`type_level_sequence_cost_measurements`] divides one checker's cost into machinery and construction; [`certifier_memo_charge_measurements`] holds the program fixed and divides the *checkers*, because the compile path puts one budget to both and a user meets whichever demands more.

use {
    crate::{Measure, phase, read, turn},
    curios::{TALLIES, test_support::*},
    curios_common::fill,
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_pipeline::test_support::*,
    curios_surface::Entrypoint,
};

/// The bound read off an opaque parameter: reduction stops at `b`, the guard refines it once and generically, and nothing is evaluated. The control the other two arms are read against, and the workaround `tests::runtime`'s accumulation measurement relies on.
fn bytes_opaque(n: usize) -> String {
    fill!(
        r#"
        use /std/{Bytes, Nat, Str};
        let go(i : Nat, acc : Bytes) -> Bytes =
            match i | 0 => acc | k + 1 => go(k, 0x[..acc, ..Str/to_bytes("0123456789")]) end;
        let head_of(b : Bytes) -> Bytes =
            match 10 <= Bytes/length(b) | true => Bytes/slice(b, 0, 10) | false => 0x[] end;
        let built = go(${n}, 0x[]);
        let head = head_of(built);
        /std/print("ok")
        "#,
        n
    )
}

/// Transitions without payload growth: the accumulator is *replaced* each step rather than extended, so the loop runs its full length against a bound that evaluates it, and the value it builds never exceeds ten bytes. What this arm still costs per transition is the floor any budget default has to respect.
fn bytes_fixed(n: usize) -> String {
    fill!(
        r#"
        use /std/{Bytes, Nat, Str};
        let go(i : Nat, acc : Bytes) -> Bytes =
            match i | 0 => acc | k + 1 => go(k, Str/to_bytes("0123456789")) end;
        let built = go(${n}, 0x[]);
        let head = Bytes/slice(built, 0, 10);
        /std/print("ok")
        "#,
        n
    )
}

/// [`bytes_opaque`] over the `List` carrier, whose fusion flattens element vectors where `Bytes`'s copies packed bytes.
fn list_opaque(n: usize) -> String {
    fill!(
        r#"
        use /std/{List, Nat, Str};
        let go(i : Nat, acc : List(Nat)) -> List(Nat) =
            match i | 0 => acc | k + 1 => go(k, [..acc, ..[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]]) end;
        let head_of(a : List(Nat)) -> List(Nat) =
            match 10 <= List/length(a) | true => List/slice(a, 0, 10) | false => [] end;
        let built = go(${n}, []);
        let head = head_of(built);
        /std/print("ok")
        "#,
        n
    )
}

/// [`bytes_fixed`] over the `List` carrier.
fn list_fixed(n: usize) -> String {
    fill!(
        r#"
        use /std/{List, Nat, Str};
        let go(i : Nat, acc : List(Nat)) -> List(Nat) =
            match i | 0 => acc | k + 1 => go(k, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]) end;
        let built = go(${n}, []);
        let head = List/slice(built, 0, 10);
        /std/print("ok")
        "#,
        n
    )
}

/// The smallest power-of-two budget `accepts` answers `true` at, as `Ok`; `Err` carries the largest budget tried when none of them sufficed.
///
/// The budget is per declaration and restored at every item boundary, so this reports the *heaviest declaration's* spend rather than a total — which is the quantity a budget default has to clear. A power of two rather than a bisection because the question is whether the count grows linearly in the iteration count, and a factor of two answers that; the failing probes abort as soon as the budget is spent, so only the succeeding one costs full price.
///
/// The `Err` payload is the largest budget *tried*, not [`DEFAULT_BUDGET`]: the sweep stops at the last power of two below the default, so a program needing more than that but less than the default elaborates fine while every probe here fails. Reporting the default in that case would claim the program does not elaborate, which is the opposite of true.
fn floor(mut accepts: impl FnMut(u64) -> bool) -> Result<u64, u64> {
    let mut largest = 0;

    for budget in std::iter::successors(Some(1024u64), |budget| budget.checked_mul(2))
        .take_while(|budget| *budget <= DEFAULT_BUDGET)
    {
        largest = budget;
        if accepts(budget) {
            return Ok(budget);
        }
    }

    Err(largest)
}

/// [`floor`] for the whole compile path, which puts the same budget to the elaborator and then to the certifier — so this reports whichever of the two demands more.
fn budget_floor(source: &str) -> Result<u64, u64> {
    floor(|budget| typecheck_within(budget, source).is_ok())
}

/// [`floor`] for elaboration alone, with the certifier not asked.
fn elaborator_floor(entrypoint: &Entrypoint) -> Result<u64, u64> {
    floor(|budget| typecheck_with_prelude(budget, entrypoint, &RootSource::none()).is_ok())
}

/// [`floor`] for the certifier alone, over a module elaboration already produced.
///
/// Elaborating once at the default budget and re-certifying the result is what separates the two counters: the module does not change with the budget the certifier is then given, so the sweep measures the certifier's own spend rather than a compile that fails earlier.
fn certifier_floor(entrypoint: &Entrypoint) -> Result<u64, u64> {
    let module = typecheck_with_prelude(DEFAULT_BUDGET, entrypoint, &RootSource::none())
        .expect("the arm elaborates within the default budget")
        .program;

    floor(|budget| recheck_with_prelude(&module, budget).is_empty())
}

/// Read a [`floor`] outcome as the figure `name` where a budget accepted, and as `<name> above`, the largest budget tried, where none did.
fn read_floor(name: &str, size: u64, floor: Result<u64, u64>) {
    match floor {
        Ok(budget) => read(name, Measure::Count, Some(size), budget),
        Err(largest) => read(
            &format!("{name} above"),
            Measure::Count,
            Some(size),
            largest,
        ),
    }
}

/// Elaborate `source` at the default budget, which every arm elaborates within.
fn elaborated(source: &str) {
    typecheck_within(DEFAULT_BUDGET, source).expect("the arm elaborates within the default budget");
}

/// What a type-level accumulation costs, divided three ways per carrier.
///
/// `cargo xbench measure type_level_sequence_cost_measurements` takes it: `<carrier>/<arm> at <n>`, the time an arm's elaboration takes, every arm at every rung turned round-robin three times after an untimed round, so the excesses `fixed − opaque` and `growing − fixed` are differences of medians; and `<carrier>/floor fixed at <n>` and `<carrier>/floor growing at <n>`, the least power of two the compile path accepts each at. It asserts nothing beyond each arm elaborating at all — a measurement that fails is a measurement with an opinion. What it does *not* cover is peak process memory, which no figure taken inside this process would be honest about: the allocator has already returned intermediates to the pool by the time a test could read a high-water mark. That half is taken from outside, with its command recorded below.
///
/// # Its records
///
/// `cargo xbench report type_level_sequence_cost_measurements` reads them: each figure's readings by group, and what each sitting was taken under.
///
/// # Peak memory
///
/// Taken from outside the process, because a high-water mark read from inside it would already have been returned to the allocator:
///
/// ```sh
/// /usr/bin/time -l target/release/curios compile bytes_growing_6400.crs -o out
/// ```
pub fn type_level_sequence_cost_measurements() {
    type Build = fn(usize) -> String;
    // Each carrier carries its own ladder, because one ladder cannot show both shapes: a packed byte copy is a `memcpy` and an element copy is a reference-count increment per element, so `List` reaches the same construction volume two orders of magnitude sooner. Both ladders stop where the default budget does — the floors below are linear in the iteration count, so the last rung of each is about the largest that still elaborates.
    let carriers: [(&str, [usize; 4], [Build; 3]); 2] = [
        (
            "Bytes",
            [800, 1600, 3200, 6400],
            [bytes_opaque, bytes_fixed, bytes_growing],
        ),
        (
            "List",
            [250, 500, 1000, 2000],
            [list_opaque, list_fixed, list_growing],
        ),
    ];

    let mut arms = Vec::new();
    for (carrier, ladder, [opaque, fixed, growing]) in carriers {
        for n in ladder {
            read_floor(
                &format!("{carrier}/floor fixed at {n}"),
                n as u64,
                budget_floor(&fixed(n)),
            );
            read_floor(
                &format!("{carrier}/floor growing at {n}"),
                n as u64,
                budget_floor(&growing(n)),
            );
            // The three arms elaborate the same loop the same number of times, so the excesses subtract everything they share: the prelude restore, backend and Wasm emission that the opaque arm is made of, and then the transition machinery the fixed arm adds. What survives both subtractions is construction.
            for (arm, build) in [("opaque", opaque), ("fixed", fixed), ("growing", growing)] {
                arms.push((format!("{carrier}/{arm} at {n}"), n as u64, build(n)));
            }
        }
    }

    let mut elaborations = arms
        .iter()
        .map(|(name, n, source)| move || phase(TALLIES, || elaborated(source)).read(name, Some(*n)))
        .collect::<Vec<_>>();
    let mut turned = elaborations
        .iter_mut()
        .map(|elaboration| elaboration as &mut dyn FnMut())
        .collect::<Vec<_>>();
    turn(3, &mut turned);
}

/// What the certifier charges for a memo hit, read off the budget it forces.
///
/// `cargo xbench measure certifier_memo_charge_measurements` takes it: `<carrier>/floor elaborator at <n>` and `<carrier>/floor certifier at <n>`. The two floors are the same program put to the two checkers separately, at a budget swept independently for each: [`elaborator_floor`] does not ask the certifier, and [`certifier_floor`] re-certifies a module elaboration already produced. Their ratio is the quantity of interest. Both checkers reduce the same terms and neither is the reference implementation of the other, so a *small* divergence is expected and says only that two evaluators differ; a large one says the two are pricing differently, and the compile path takes the larger of the two, so it is the certifier's number a user meets.
///
/// It asserts nothing beyond each arm elaborating at all — a measurement that fails is a measurement with an opinion.
///
/// # Its records
///
/// `cargo xbench report certifier_memo_charge_measurements` reads them: each figure's readings by group, and what each sitting was taken under.
pub fn certifier_memo_charge_measurements() {
    type Build = fn(usize) -> String;
    let carriers: [(&str, [usize; 4], Build); 2] = [
        ("Bytes", [800, 1600, 3200, 6400], bytes_growing),
        ("List", [250, 500, 1000, 2000], list_growing),
    ];

    for (carrier, ladder, growing) in carriers {
        for n in ladder {
            let entrypoint = growing(n).parse::<Entrypoint>().expect("the arm parses");
            read_floor(
                &format!("{carrier}/floor elaborator at {n}"),
                n as u64,
                elaborator_floor(&entrypoint),
            );
            read_floor(
                &format!("{carrier}/floor certifier at {n}"),
                n as u64,
                certifier_floor(&entrypoint),
            );
        }
    }
}

/// The same `n` bytes written as a raw `Bytes` literal — the proof-free control, and the whole of what a `Str` literal would cost if its validity were not checked by running a fold.
fn bytes_literal(n: usize) -> String {
    let entries = (0..n)
        .map(|index| format!("0x{:02x}", b'0' + (index % 10) as u8))
        .collect::<Vec<_>>()
        .join(", ");

    fill!(
        r#"
        use /std/{Bytes, Nat};
        let b : Bytes = 0x[${entries}];
        /std/print(Nat/to_str(Bytes/length(b)))
        "#,
        entries
    )
}

/// Read what `source` cost each checker as the figures of `label`, at `size` where it is taken at one, split into depth and everything else: `<label>/<checker>/units`, `/depth` and `/other`, for the elaborator and the certifier.
fn read_cost(label: &str, size: Option<u64>, source: &str) {
    let (elaborator, certifier) = declaration_cost(source);
    for (checker, consumed) in [("elaborator", elaborator), ("certifier", certifier)] {
        for (figure, value) in [
            ("units", consumed.units()),
            ("depth", consumed.peak_depth() as u64),
            ("other", consumed.other_units()),
        ] {
            read(
                &format!("{label}/{checker}/{figure}"),
                Measure::Count,
                size,
                value,
            );
        }
    }
}

/// What a `Str` literal costs to check, and which row of the price list it spends on.
///
/// `cargo xbench measure str_literal_cost_measurements` takes it, each row's figures as [`read_cost`] reads them. This is the probe `curios`'s `a_str_literal_costs_transitions_rather_than_frames` guards. It asserts only that each arm checks at all — a measurement that fails is a measurement with an opinion — and the assertion that a regression has to trip lives in that ordinary test instead.
///
/// # Its records
///
/// `cargo xbench report str_literal_cost_measurements` reads them: each figure's readings by group, and what each sitting was taken under.
pub fn str_literal_cost_measurements() {
    for n in [250, 500, 1000, 2000, 4000, 8000] {
        read_cost(
            &format!("Str literal at {n}"),
            Some(n as u64),
            &str_literal(n, 0),
        );
    }

    // Flat in use count: a regression here is a literal checked once per mention.
    for uses in [1, 3] {
        read_cost(
            &format!("Str literal at 500, {uses} uses"),
            Some(500),
            &str_literal(500, uses),
        );
    }

    // The control: the same bytes with no proof over them.
    read_cost("Bytes literal at 500", Some(500), &bytes_literal(500));

    // A cut at a position decides its boundary by the one byte there, discharged by reduction, so this should sit within a few percent of the bare literal — the check is the cost, not the cut.
    read_cost(
        "Str literal at 500, cut",
        Some(500),
        &str_literal(500, 0).replace(
            r#"/std/print("ok")"#,
            r#"/std/print(Str/before(s, Str/At/of_offset(s, 10)))"#,
        ),
    );
}

/// What a web of combinator definitions costs each checker, and what consuming its value does to that.
///
/// `cargo xbench measure combinator_web_cost_measurements` takes it, `web at <n>, <consumption>`'s figures as [`read_cost`] reads them. The third of the parity probes. [`str_literal_cost_measurements`] holds a proof fixed and divides one checker's cost; [`certifier_memo_charge_measurements`] divides the two checkers by budget floor; this one divides them on a program shape where they can part by an *exponent* without disagreeing about any rule — a scrutinee whose subject mentions a binder, which a checker reducing it once per arm to key its case refinement would pay for per definition in the web.
///
/// Three rows per size, differing only in what demands the web's value: nothing, a `match` at a binder, a `match` at a literal. The middle row is the one such keying would grow; the last is the control that says the trigger is the binder rather than the `match`.
///
/// It asserts nothing beyond each arm checking at all. `scrutinee_refinement_measurements` carries the wall clocks beside it.
///
/// # Its records
///
/// `cargo xbench report combinator_web_cost_measurements` reads them: each figure's readings by group, and what each sitting was taken under.
pub fn combinator_web_cost_measurements() {
    for rules in [8usize, 12, 13, 14, 20] {
        for (consumed, label) in [
            (Consumed::Applied, "applied"),
            (Consumed::Scrutinized, "scrutinized"),
            (Consumed::ScrutinizedClosed, "closed"),
        ] {
            read_cost(
                &format!("web at {rules}, {label}"),
                Some(rules as u64),
                &predicates(rules, consumed, true),
            );
        }
    }
}

/// A program whose claim is how many bytes of `input` are `left` once `parser` has read it, read back by the carrier's `rest` and stated over a declaration bound to the read, so no member is left to inference. A claim over the success alone counts no walk where a primitive answers its success before its walk is run, as one that builds its position where its walk stops does.
fn primitive_claim(carrier: &str, parser: &str, input: &str, left: usize) -> String {
    let (rest, length) = match carrier {
        "Str" => ("Parse/str/rest()", "Bytes/length(Str/to_bytes(tail))"),
        _ => ("Parse/bytes/rest()", "Bytes/length(tail)"),
    };
    fill!(
        "use /std/{Str, Bytes, Byte, Char, Nat, Parse, Result, Option, Eq};\n\nlet left(r: Result(Parse/Error, Nat)) -> Option(Nat) =\n    match r\n    | success(n) => Option/some(n)\n    | failure(_) => Option/none()\n    end;\n\nlet times(@I: Type, use Parse/Input(I), @A: Type, n: Nat, p: Parse(I, A)) -> Parse(I, {}) =\n    match n\n    | 0 => Parse/pure(())\n    | k + 1 =>\n        let _ = p!;\n        times(k, p)\n    end;\n\nlet reader() -> Parse(${carrier}, Nat) =\n    let _ = ${parser}!;\n    let tail = ${rest}!;\n    Parse/pure(${length});\n\nlet read: Result(Parse/Error, Nat) = Parse/run(reader(), ${input});\n\nlet _claim: Eq()(left(read), Option/some(${left})) = Eq/refl();\n\n/std/print(\"ok\")\n",
        carrier,
        parser,
        rest,
        length,
        input,
        left
    )
}

/// What each primitive of `/std/Parse/str` costs each checker beside its twin in `/std/Parse/bytes`, over 32 letters: a text primitive steps by a character and hands on a position that is a boundary of valid text, where its twin steps by a byte.
///
/// `cargo xbench measure parse_primitive_measurements` takes it, each primitive's figures `<primitive>/text` and its twin's `<primitive>/bytes` as [`read_cost`] reads them. Counted, so a reading transports between machines. A change to `/std/Parse`, to `/std/Str/At` or to what the closed machine evaluates retakes it.
///
/// # Its records
///
/// `cargo xbench report parse_primitive_measurements` reads them: each figure's readings by group, and what each sitting was taken under.
pub fn parse_primitive_measurements() {
    let letters = "a".repeat(32);
    let text = format!("\"{letters}\"");
    let bytes = format!("Str/to_bytes(\"{letters}\")");
    let rows: [(&str, String, Option<String>); 7] = [
        (
            "literal",
            format!("Parse/str/literal({text})"),
            Some(format!("Parse/bytes/literal({text})")),
        ),
        (
            "take_while",
            "Parse/str/take_while((c) => Char/to_nat(c) == 'a')".to_string(),
            Some("Parse/bytes/take_while((b) => Byte/to_nat(b) == 'a')".to_string()),
        ),
        (
            "char, byte",
            "times(32, Parse/str/char())".to_string(),
            Some("times(32, Parse/bytes/byte())".to_string()),
        ),
        (
            "char_if, byte_if",
            "times(32, Parse/str/char_if((c) => Char/to_nat(c) == 'a'))".to_string(),
            Some("times(32, Parse/bytes/byte_if((b) => Byte/to_nat(b) == 'a'))".to_string()),
        ),
        (
            "expect",
            "times(32, Parse/str/expect('a'))".to_string(),
            Some("times(32, Parse/bytes/expect('a'))".to_string()),
        ),
        (
            "rest",
            "Parse/str/rest()".to_string(),
            Some("Parse/bytes/rest()".to_string()),
        ),
        ("until", "Parse/str/until(\"b\")".to_string(), None),
    ];

    for (label, over_text, over_bytes) in rows {
        let (input, left) = match label {
            "until" => (format!("\"{letters}b\""), 1),
            _ => (text.clone(), 0),
        };
        read_cost(
            &format!("{label}/text"),
            None,
            &primitive_claim("Str", &over_text, &input, left),
        );
        if let Some(parser) = over_bytes {
            read_cost(
                &format!("{label}/bytes"),
                None,
                &primitive_claim("Bytes", &parser, &bytes, 0),
            );
        }
    }
}

/// What each claim of the type-level benchmark set costs each checker: an equality a type states and `Eq/refl()` proves, so that checking the program is reducing the claim's left side to its right. Six take a `Str` apart, one reads a `Flt`, one searches a `Str` and maps the position found.
///
/// `cargo xbench measure type_level_claim_measurements` takes it, each claim's figures as [`read_cost`] reads them. Counted, so a reading transports between machines. A change to how a `Str` or a `Flt` is read at the type level retakes it and accounts for each row that moved. Each checker reports its heaviest declaration, and a program that states no claim reads under 2 300 units ([`str_literal_cost_measurements`]'s `Bytes` row), so each row is its claim's.
///
/// # Its records
///
/// `cargo xbench report type_level_claim_measurements` reads them: each figure's readings by group, and what each sitting was taken under.
pub fn type_level_claim_measurements() {
    let claims = [
        (
            "Str/split",
            r#"Str/split("a€€b€c", "€")"#,
            r#"["a", "", "b", "c"]"#,
        ),
        (
            "Str/split_once",
            r#"Str/split_once("key=value=x", "=")"#,
            r#"Option/some(("key", "value=x"))"#,
        ),
        ("Str/lines", r#"Str/lines("a\r\nb\n")"#, r#"["a", "b"]"#),
        (
            "Str/replace",
            r#"Str/replace("a€b€", "€", "-")"#,
            r#""a-b-""#,
        ),
        ("Str/trim", r#"Str/trim(" \té \n")"#, r#""é""#),
        (
            "Str/strip_suffix",
            r#"Str/strip_suffix("héllo", "llo")"#,
            r#"Option/some("hé")"#,
        ),
        (
            "Flt/of_str",
            r#"Flt/of_str("-Infinity")"#,
            "Option/some(-inf.0)",
        ),
        (
            "Str/index_of",
            r#"Option/map(Str/index_of("abcdefgh", 'h'), Str/At/to_offset)"#,
            "Option/some(7)",
        ),
    ];

    for (label, left, right) in claims {
        read_cost(
            label,
            None,
            &fill!(
                "use /std/{Str, Option, Eq, Nat, Flt, List};\n\nlet _claim: Eq()(${left}, ${right}) = Eq/refl();\n\n/std/print(\"ok\")\n",
                left,
                right
            ),
        );
    }
}
