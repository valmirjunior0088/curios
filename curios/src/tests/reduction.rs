//! What a sequence, a literal and a claim cost to build and decide at the *type* level, held to the bounds each test states; what the same work costs against a run, and between the checkers, is the bench's (`xbench/src/measurements/reduction.rs`).
//!
//! `Bytes/slice` states `10 <= Bytes/length(b)`, a decided proposition, so its subject stands in a type and the obligation is discharged by reducing that subject. Writing `Bytes/slice(built, 0, 10)` over a computed accumulator therefore runs the whole accumulation at elaboration time. `curios-core`'s `FUSION_CAP` keeps `normalize_concat` from fusing an all-literal concatenation into one packed value — which would recopy everything accumulated so far on every step, a quadratic — and its measure keeps a length over the resulting spine a single fold.

use {
    crate::test_support::*, curios_common::fill, curios_core::Cost, curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource, curios_pipeline::test_support::*, curios_surface::Entrypoint,
};

/// **The regression guard the measurement above cannot be.** A probe is ignored, so nothing runs it; and a cache hit charges nothing in either checker, so both absorb this entire class of defect and stay silent until a budget runs out. A quadratic length would go unnoticed that way, so the guard has to be an ordinary assertion at the ordinary budget.
///
/// The two checkers price a memo hit alike — see [`certifier_memo_charge_measurements`] — so a construction defect surfaces as either checker's honest cost, and this guard is what catches it.
///
/// Both carriers, at an iteration count that costs a small multiple of the default budget when a length is quadratic in the spine's depth and a small fraction of it when a length is a fold.
#[test]
fn an_accumulated_sequence_is_bounded_when_a_window_is_taken_of_it() {
    for source in [bytes_growing(2000), list_growing(2000)] {
        typecheck_within(DEFAULT_BUDGET, &source)
            .expect("an accumulation and a window over it fit the ordinary budget");
    }
}

/// A literal folded at the type level is folded once however many types mention it. Its certificate, `True/qed()`, has no universe to instantiate, so every mention is one term to the reduction cache; a certificate instantiated at a fresh level per mention would make the four mentions here four keys and four full folds.
#[test]
fn a_literal_mentioned_in_several_types_is_folded_once() {
    let literal = "0123456789".repeat(30);
    let program = |uses: usize| {
        let types = vec![format!("Eq()(Str/length(\"{literal}\"), 300)"); uses].join(", ");
        let proofs = vec!["Eq/refl()"; uses].join(", ");
        fill!(
            "use /std/{Str, Eq, Nat};\n\nlet q: {${types}} = (${proofs},);\n\n/std/print(\"ok\")\n",
            types,
            proofs
        )
    };

    let (one, ..) = declaration_cost(&program(1));
    let (four, ..) = declaration_cost(&program(4));

    assert!(
        four.units() < one.units() * 2,
        "one mention costs {} units and four cost {}",
        one.units(),
        four.units()
    );
}

/// An item's verdict is the same compiled alone and after its neighbours. Item `_a` states one type-level claim over a literal; item `_b` states the same claim and a second one, so a reduct `_a` left behind would answer half of `_b`'s work for nothing. `_b` is the heaviest declaration either way; each checker spends the same on it in both programs, and one unit short of that, each refuses it in both.
///
/// **A guard on this tree rather than a regression test.** Every item's finalization rewrites its universe levels and clears the reducts with them, so `_b` meets a cold table after `_a` whether or not the table outlives a declaration. What keeps it from outliving one is held where it is kept: `curios-elab`'s `a_closed_reduct_does_not_outlive_its_declaration`, and in each checker `what_a_declaration_spends_does_not_depend_on_what_was_reduced_before_it`.
#[test]
fn an_items_verdict_is_the_same_compiled_alone_and_after_its_neighbours() {
    let claim = |literal: String| format!("Eq()(Str/length(\"{literal}\"), 300)");
    let (first, second) = (
        claim("0123456789".repeat(30)),
        claim("abcdefghij".repeat(30)),
    );
    let neighbour = format!("let _a: {first} = Eq/refl();\n\n");
    let program = |neighbours: &str| {
        fill!(
            "use /std/{Str, Eq};\n\n${neighbours}let _b: {${first}, ${second}} = (Eq/refl(), Eq/refl(),);\n\n/std/print(\"ok\")\n",
            neighbours,
            first,
            second
        )
    };
    let (alone, after) = (program(""), program(&neighbour));

    let elaborated = |source: &str, budget: u64| {
        let entrypoint = source.parse::<Entrypoint>().expect("the program parses");
        typecheck_with_prelude(budget, &entrypoint, &RootSource::none())
            .map(|checked| (checked.program, checked.consumption.units()))
    };
    let (alone_module, alone_units) =
        elaborated(&alone, DEFAULT_BUDGET).expect("`_b` elaborates alone");
    let (after_module, after_units) =
        elaborated(&after, DEFAULT_BUDGET).expect("`_b` elaborates after `_a`");

    assert_eq!(
        after_units, alone_units,
        "the elaborator spends the same on `_b`"
    );
    assert!(
        elaborated(&alone, alone_units - 1).is_err(),
        "one unit short, the elaborator refuses `_b` alone"
    );
    assert!(
        elaborated(&after, alone_units - 1).is_err(),
        "and after `_a`"
    );

    let certified = |module: &curios_core::Program, budget: u64| {
        let (verdicts, heaviest) = recheck_with_prelude_measured(module, budget, |certifier| {
            certifier.heaviest_declaration()
        });
        (verdicts.is_empty(), heaviest.units())
    };
    let (accepted, alone_units) = certified(&alone_module, DEFAULT_BUDGET);
    assert!(accepted, "the certifier accepts `_b` alone");
    let (accepted, after_units) = certified(&after_module, DEFAULT_BUDGET);
    assert!(accepted, "the certifier accepts `_b` after `_a`");

    assert_eq!(
        after_units, alone_units,
        "the certifier spends the same on `_b`"
    );
    assert!(
        !certified(&alone_module, alone_units - 1).0,
        "one unit short, the certifier refuses `_b` alone"
    );
    assert!(
        !certified(&after_module, alone_units - 1).0,
        "and after `_a`"
    );
}

/// **The guard [`str_literal_cost_measurements`] cannot be**, because a probe is ignored and nothing runs it.
///
/// What it holds is the shape of a literal's cost rather than a number, and the closed machine is what set the shape: guarded reduction depth *flat* in the literal's length on both checkers, a per-character price in transitions and machine frames far below [`Cost::FRAME`], and neither checker paying a multiple of the other for the same reduction. The first two are the machine's yield — a recursive strategy nests one native reduction level per byte and pays the frame row per character; the third is what a checker's memo stopping short of its internal levels breaks.
///
/// The bound is stated against [`Cost::FRAME`] rather than as a literal because the quantity asserted is *that no per-character native frame is being paid at all*: a per-character price within even a quarter of the frame row means closed evaluation has fallen off the machine and back onto the recursive strategy, which is the silent cliff this guard exists to catch.
#[test]
fn a_str_literal_costs_transitions_rather_than_frames() {
    let (elaborator_small, certifier_small) = declaration_cost(&str_literal(500, 0));
    let (elaborator_large, certifier_large) = declaration_cost(&str_literal(1000, 0));

    // The literal's scan runs on the machine's explicit stack, so doubling the literal moves guarded depth not at all.
    assert_eq!(certifier_large.peak_depth(), certifier_small.peak_depth());
    assert_eq!(elaborator_large.peak_depth(), elaborator_small.peak_depth());

    let per_character = (certifier_large.units() - certifier_small.units()) / 500;
    let frame = Cost::FRAME.get();
    assert!(
        per_character < frame / 4,
        "a character costs {per_character} units against a {frame}-unit frame, so the ceiling is about {} characters",
        DEFAULT_BUDGET / per_character.max(1),
    );

    // The two checkers run the same machine on the same closed terms, so agreement here is structural; the factor-two slack covers what each checker's own strategy spends around the machine.
    assert!(
        certifier_large.units() < elaborator_large.units() * 2,
        "certifier {} against elaborator {}",
        certifier_large.units(),
        elaborator_large.units(),
    );

    // Flat in use count: a literal is checked once however many types mention it.
    let (_, certifier_used) = declaration_cost(&str_literal(500, 3));
    assert_eq!(certifier_used.units(), certifier_small.units());
}

/// A user's own refinement over a packed carrier, `n` bytes long: an authored `rec` fold decides ASCII-ness, and a literal's proof of it is discharged by conversion running the closed fold — exactly the shape of `Str`'s validity check with nothing of `Str` in it.
///
/// The proof is bound by a `let` rather than packed into a dependent record deliberately: this fixture measures the machine, and a *struct declaration* whose field applies a fold to an earlier field is measured by nothing here. What it costs is the strict-positivity walk's, spent after every item over the declarations alone, where no reduction budget reaches and no closed machine applies. That shape is `a_refinement_field_over_a_self_calling_fold_is_admitted` in `positivity.rs`.
fn ascii_refinement(n: usize) -> String {
    let entries = (0..n)
        .map(|index| format!("0x{:02x}", b'a' + (index % 26) as u8))
        .collect::<Vec<_>>()
        .join(", ");

    fill!(
        r#"
        use /std/{Bytes, Byte, Nat, Bool, Eq};
        let all_ascii(b : Bytes) -> Bool =
            match b
            | 0x[] => true
            | 0x[h, ..t] =>
                match Byte/to_nat(h) < 0x80
                | true => all_ascii(t)
                | false => false
                end
            end;
        let ok : Eq()(all_ascii(0x[${entries}]), true) = Eq/refl();
        /std/print("ok")
        "#,
        entries
    )
}

/// **The fixture the closed machine's acceptance asks for: a refinement that is not `Str`.** The machine fires on closedness, not on anything about strings, so a user's fold over a packed carrier gets the same flat depth and sub-frame per-element price a `Str` literal gets — asserted with the same two bounds as [`a_str_literal_costs_transitions_rather_than_frames`], so this cannot quietly hold for the prelude's type and not for a user's.
#[test]
fn a_user_refinement_over_a_packed_carrier_takes_the_same_machine() {
    let (_, certifier_small) = declaration_cost(&ascii_refinement(500));
    let (_, certifier_large) = declaration_cost(&ascii_refinement(1000));

    assert_eq!(certifier_large.peak_depth(), certifier_small.peak_depth());

    let per_element = (certifier_large.units() - certifier_small.units()) / 500;
    assert!(
        per_element < Cost::FRAME.get() / 4,
        "an element costs {per_element} units against a {}-unit frame",
        Cost::FRAME.get(),
    );
}

/// **The construction-dominated fixture the acceptance criteria ask for**, and it is deliberately not the accumulate-then-slice shape above: with fusion capped that program's construction is linear, so it refuses on ordinary step cost like any other long computation and would test the wrong thing.
///
/// A shift is the shape no representation change can flatten. It has no loop, so nothing amortizes it; its result is `bits(value) + amount` wide and the amount is a numeral the program writes, so no operand size bounds it. Priced by transitions alone it would compile in well under a second while building fifty megabytes of magnitude.
///
/// The paired control is [`a_bound_behind_a_parameter_evaluates_nothing`](super::numeric) in spirit and the second arm here in fact: the same term at an amount the budget affords still folds, so what the first arm demonstrates is a refusal about *size* rather than about the operation.
///
/// **The subject stands under an obligation rather than in a match scrutinee.** `Bytes/drop` states `Le(k, len(b))`, a decided proposition whose subject stands in a type, so discharging it *is* reducing the shift — which is what makes this fixture about construction at all. Nothing in either checker demands a top-level match's scrutinee, so the same shift under `match Nat/le(1, big)` would evaluate nothing and test nothing; the demander here is one a user would actually write.
#[test]
fn an_oversized_construction_is_refused_before_it_is_allocated() {
    let shift = |amount: u64| {
        fill!(
            r#"
            use /std/{Nat, Bytes, Str};
            let big : Nat = Nat/shl(1, ${amount});
            let b : Bytes = Str/to_bytes("0123456789");
            let rest : Bytes = Bytes/drop(b, big);
            /std/print("ok")
            "#,
            amount
        )
    };

    let refusal = typecheck_within(DEFAULT_BUDGET, &shift(1 << 40))
        .expect_err("a shift whose result no budget affords is refused");
    assert!(
        refusal.contains("budget spent"),
        "expected a spent-budget refusal, got: {refusal}"
    );

    typecheck_within(DEFAULT_BUDGET, &shift(3))
        .expect("the same operation at an affordable size still folds");
}

/// Repeated concatenation is bounded by *cumulative* charges even though every individual result fits: the budget is never refunded, so a loop that builds a growing value pays for each of them and runs out on the total.
///
/// The two arms differ only in how many iterations they run, and the small one establishes that the shape itself is affordable — so the large one's refusal is about the accumulation rather than about the program. The property held is that a count exists past which cumulative construction refuses, not where it sits: a hundred thousand iterations fit under the closed machine.
///
/// The budget is stated, at a thirtieth of the default, because the refusing arm's cost is the budget it spends before refusing: two million iterations against the default's thirty million steps would be among the suite's slowest tests, all of it the wait for exhaustion. The measured floors above put two thousand iterations under `2^19`, so `2^20` affords the fitting arm with the same headroom the default gave it, and a hundred times the iterations exhausts it as surely as a thousand times exhausted the default.
#[test]
fn a_growing_accumulation_is_bounded_by_what_it_has_already_built() {
    const BUDGET: u64 = 1 << 20;

    typecheck_within(BUDGET, &bytes_growing(2_000))
        .expect("an accumulation this size fits the stated budget");

    let refusal = typecheck_within(BUDGET, &bytes_growing(200_000))
        .expect_err("a hundred times the iterations does not");
    assert!(
        refusal.contains("budget spent"),
        "expected a spent-budget refusal, got: {refusal}"
    );
}

/// A `Bytes` fold over `width` bytes returning a pair, and its single-value twin. `let (a, b) = go(…)` is projection sugar, so the pair form demands the *same* recursive call once per component; the twin demands it once. Everything else about the two is identical, so the gap between their curves is the cost of that second demand and nothing else.
///
/// Each program claims the fold's length, an equation the claim's check decides by running the fold. A call that only handed the fold's result on would not be run by a type at all: a divisor's bound over it is decided by what the sum is, whatever its operand comes to.
fn paired_fold(width: usize, paired: bool) -> String {
    let ones = vec!["1"; 32].join(", ");
    match paired {
        true => fill!(
            r#"
            use /std/{Nat, Byte, Bytes, Eq};
            let go(x: Bytes, c: Byte) -> {Bytes, Byte} =
                match x
                | 0x[] => (0x[], c)
                | 0x[h, ..t] =>
                    let (rest, out) = go(t, Byte/xor(h, c));
                    (0x[Byte/xor(h, c), ..rest], out)
                end;
            let (bytes, _) = go(Bytes/slice(0x[${ones}], 0, ${width}), 0);
            let _counted: Eq()(Bytes/length(bytes), ${width}) = Eq/refl();
            /std/print("")
            "#,
            ones,
            width
        ),
        false => fill!(
            r#"
            use /std/{Nat, Byte, Bytes, Eq};
            let go(x: Bytes, c: Byte) -> Bytes =
                match x
                | 0x[] => 0x[]
                | 0x[h, ..t] => 0x[Byte/xor(h, c), ..go(t, Byte/xor(h, c))]
                end;
            let _counted: Eq()(Bytes/length(go(Bytes/slice(0x[${ones}], 0, ${width}), 0)), ${width}) = Eq/refl();
            /std/print("")
            "#,
            ones,
            width
        ),
    }
}

/// **A recursive call whose result is read at two positions is evaluated once, not twice.**
///
/// The machine records a forced application's value under the application itself, the path whose head is a recursive member included. That *a member selection's calls never repeat within a run because the fold argument strictly shrinks* is false for every tuple-returning recursion: `let (a, b) = go(…)` lowers to two projections of one call, so without the record each level would demand the same call twice and the fold would cost `2^n`.
///
/// What this asserts is the *shape* rather than a figure: the pair form's increment per byte must not grow. The single-value twin is the control — it makes one demand per level, so it grows by the same amount every byte, and a regression that slowed both equally would not read as this defect. What is read is the heaviest single judgment, which here is the elaboration of the declaration that claims the fold's length. The widths stay within the eight bytes one unit of a packed payload is charged for, so no run the fold builds adds a term in its length to either curve.
///
/// **Run against the defect and observed to fail**, which is what makes it a detector rather than a description: with the records removed the pair form's increments double with every byte, and the assertion names them. The match a member's body opens to is recorded too, and serves the second demand where the application's record is gone, so reproduce by deleting both `Frame::Memo` pushes in `curios-core`'s machine, at the head frame's entry and at a match's; deleting the head frame's alone leaves the fold linear.
#[test]
fn a_recursive_call_read_twice_is_evaluated_once() {
    let units = |width: usize, paired: bool| {
        let source = paired_fold(width, paired);
        let entrypoint = source.parse::<Entrypoint>().expect("the program parses");
        typecheck_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none())
            .expect("the fold elaborates within the default budget")
            .consumption
            .units()
    };

    let paired = [5usize, 6, 7, 8].map(|width| units(width, true));
    let single = [5usize, 6, 7, 8].map(|width| units(width, false));

    // The control grows by one demand a level and no more, so what the pair form adds to it is the second demand rather than the fold.
    assert_eq!(
        single[1] - single[0],
        single[3] - single[2],
        "the single-value twin should grow by the same amount every byte: {single:?}"
    );

    // Linear growth: each further byte costs about what the previous one did. Doubling per byte makes the later increments explode, which is exactly what this refuses.
    let increments = [
        paired[1] - paired[0],
        paired[2] - paired[1],
        paired[3] - paired[2],
    ];
    assert!(
        increments[2] <= increments[0] * 2,
        "the pair form's cost is not linear in width — a recursive call read twice is being evaluated twice: {paired:?} (increments {increments:?})"
    );
}

/// A packed fold costs the elaborator linearly in the length folded. Its per-step bound is the decided `i < i + (kp + 1)`, settled by cancellation; an inductive proof by recursion on the remaining length would make every step pay the steps left, and a fold over a 2 KB literal at the type level would run out of budget where an indexed walk finishes.
#[test]
fn a_packed_fold_costs_linearly_in_its_length() {
    let program = |bytes: usize| {
        let literal = "0123456789".repeat(bytes / 10);
        fill!(
            "use /std/{Str, Bytes, Nat, Eq};\n\nlet counted: Eq()(Bytes/fold(Str/to_bytes(\"${literal}\"), 0, (_, n) => n + 1), ${bytes}) = Eq/refl();\n\n/std/print(\"ok\")\n",
            literal,
            bytes
        )
    };

    let (short, ..) = declaration_cost(&program(300));
    let (long, ..) = declaration_cost(&program(600));

    assert!(
        long.units() < short.units() * 3,
        "300 bytes cost {} units and 600 cost {}; a linear fold doubles, a quadratic one quadruples",
        short.units(),
        long.units()
    );
}

/// A bytes walk is run once, however many walks follow it. The grammar reads a run and the comma that closes it, once per token, and the claim reduces it in a type, so the units count the walks: `Parse/bytes/take_while` counts the bytes it reads into a number, stops at the first one refused, and hands on a position at that number. An induction on the remaining length, whose count is its hypothesis read back, multiplies its cost with each token a grammar reads.
#[test]
fn a_bytes_walk_is_run_once_however_many_follow_it() {
    let program = |tokens: usize| {
        let literal = "ab,".repeat(tokens);
        let total = 2 * tokens;
        fill!(
            "use /std/{Parse, Result, Eq, Byte, Bytes, Str, Nat};\n\nlet token() -> Parse(Bytes, Nat) =\n    let run = Parse/bytes/take_while((b) => Byte/to_nat(b) != ',')!;\n    let _ = Parse/bytes/expect(',')!;\n    Parse/pure(Bytes/length(run));\n\nlet tokens(count: Nat, acc: Nat) -> Parse(Bytes, Nat) =\n    match count\n    | 0 => Parse/pure(acc)\n    | k + 1 =>\n        let n = token()!;\n        tokens(k, acc + n)\n    end;\n\nlet walked: Eq()(Parse/run(tokens(${tokens}, 0), Str/to_bytes(\"${literal}\")), Result/success(${total})) = Eq/refl();\n\n/std/print(\"ok\")\n",
            tokens,
            literal,
            total
        )
    };

    let (short, ..) = declaration_cost(&program(4));
    let (long, ..) = declaration_cost(&program(8));

    assert!(
        long.units() < short.units() * 8,
        "4 tokens cost {} units and 8 cost {}; a walk run once grows with its offsets, one run again by each walk after it multiplies with every token",
        short.units(),
        long.units()
    );
}

/// A program stating a claim over one parse: a literal of `letters` letters read by `/std/Parse/str/literal` of the same text, which `claim` is handed as written and answers the declarations for.
///
/// `/std/Parse/run` takes its result's type as a hidden member, which unification solves and the term spells as a metavariable until its declaration is zonked: a claim holding the parse holds that metavariable, and a claim over a declaration bound to the parse holds none.
fn parse_claim(letters: usize, claim: impl FnOnce(&str) -> String) -> String {
    let literal = "a".repeat(letters);
    let claim = claim(&format!(
        "Parse/run(Parse/str/literal(\"{literal}\"), \"{literal}\")"
    ));

    fill!(
        "use /std/{Str, Parse, Result, Bool, Eq};\nuse /std/Bool/{Holds, True};\n\nlet ok(r: Result(Parse/Error, {})) -> Bool =\n    match r\n    | success(_) => true\n    | failure(_) => false\n    end;\n\n${claim}\n\n/std/print(\"ok\")\n",
        claim
    )
}

/// A claim over a declaration bound to the parse: `prove` is the claim's own declaration, stated over `read`.
fn over_read(prove: &str) -> impl FnOnce(&str) -> String {
    move |parse| {
        fill!(
            "let read: Result(Parse/Error, {}) = ${parse};\n\n${prove}",
            parse,
            prove
        )
    }
}

/// A closed claim costs the elaborator what it costs with nothing left to inference. Reduced as its solutions spell it, the claim holding a solved metavariable is one run of the closed machine, as the claim over `read` is; declined at the gate for the metavariable, it is taken apart by the recursive strategy, each closed piece a run of its own, at several times the cost and growing faster than the text.
///
/// So the claim holding the metavariable grows with the text as the claim over `read` does, and costs under twice it at every length. What it pays beyond the run is the metavariable's and the same at every length, which a parse that compares its piece's bytes at once leaves as a large share of a short claim.
#[test]
fn a_claim_over_a_solved_metavariable_costs_what_the_claim_written_out_costs() {
    let cost = |letters: usize| {
        let (inline, ..) = declaration_cost(&parse_claim(letters, |parse| {
            format!("let _claim: Holds(ok({parse})) = True/qed();")
        }));
        let (named, ..) = declaration_cost(&parse_claim(
            letters,
            over_read("let _claim: Holds(ok(read)) = True/qed();"),
        ));

        (letters, inline.units(), named.units())
    };
    let [short, long] = [16, 32].map(cost);

    for (letters, inline, named) in [short, long] {
        assert!(
            inline < named * 2,
            "at {letters} letters the claim costs {inline} units with the parse inline and {named} with it named",
        );
    }
    let ((_, inline_short, named_short), (_, inline_long, named_long)) = (short, long);
    let inline = inline_long.saturating_sub(inline_short);
    let named = named_long.saturating_sub(named_short);
    assert!(
        inline * 4 <= named * 5,
        "from 16 to 32 letters the claim grows by {inline} units with the parse inline and by {named} with it named",
    );
}

/// An equation costs each checker what the decision of the same fact costs. A claim `Eq()(lhs, rhs)` is typed by a run that evaluates both sides, a family's operands being evaluated as any substituend is, and conversion then asks for each side by itself: what the first run answered is kept by the closed term (`curios_core::ClosedMemo`), so the second asking is a probe. Asked twice, the equation cost twice the decision.
#[test]
fn an_equation_costs_what_its_decision_costs() {
    for letters in [16, 32] {
        let decided = declaration_cost(&parse_claim(
            letters,
            over_read("let _claim: Holds(ok(read)) = True/qed();"),
        ));
        let equated = declaration_cost(&parse_claim(
            letters,
            over_read("let _claim: Eq()(ok(read), true) = Eq/refl();"),
        ));

        for (checker, decided, equated) in [
            ("elaborator", decided.0, equated.0),
            ("certifier", decided.1, equated.1),
        ] {
            assert!(
                equated.units() * 4 <= decided.units() * 5,
                "at {letters} letters the {checker} spends {} units on the equation and {} on the decision",
                equated.units(),
                decided.units()
            );
        }
    }
}

/// A published item's binders state whether they bind a proof, each asked at its declared type under the binders before it: a function's proof does, and its type parameter, its number and a value at the type parameter do not; of two `let`s, the equation does and the sum does not.
#[test]
fn a_published_binder_states_whether_it_binds_a_proof() {
    let source = "use /std/{Nat, Eq};\nuse /std/Bool/{Holds};\n\nlet through(A: Type, n: Nat, h: Holds(0 <= n), x: A) -> Nat =\n    let twice = n + n;\n    let same: Eq()(n, n) = Eq/refl();\n    twice;\n\n/std/print(\"ok\")\n";
    let entrypoint = source.parse::<Entrypoint>().expect("the program parses");
    let program = typecheck_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none())
        .expect("the program elaborates within the default budget")
        .program;
    let through = program
        .definitions
        .items
        .iter()
        .find_map(|item| match item {
            curios_core::Item::Let(definition)
                if definition.name.faithful().to_string() == "/through" =>
            {
                Some(definition)
            }
            _ => None,
        })
        .expect("the program defines `through`");

    let curios_core::Subterm::Func(function) = &*through.body else {
        panic!("a function literal: {:?}", through.body);
    };
    assert_eq!(function.telescope.proofs(), [false, false, true, false]);

    let curios_core::Subterm::Let(block) = &**function.telescope.terminal() else {
        panic!("a `let`: {:?}", function.telescope.terminal());
    };
    assert_eq!(
        [block.tail().binds_proof(0), block.tail().binds_proof(1)],
        [false, true]
    );
}

/// A claim over a call with a proof argument: `through` hands its number on and never reads its proof, and `proof` is the argument written at it.
///
/// `checked` is no recursive member's call: it eliminates the lemma, so taking it to a weak-head value runs the lemma's induction, `steps` deep, where a recursive call alone would stay folded.
fn proof_argument_claim(steps: usize, proof: &str) -> String {
    fill!(
        "use /std/{Nat, Eq};\n\nlet lemma(n: Nat) -> Eq()(n, n) =\n    match n\n    | 0 => Eq/refl()\n    | k + 1 => match lemma(k) | refl() => Eq/refl() end\n    end;\n\nlet checked(n: Nat) -> Eq()(n, n) =\n    match lemma(n) | refl() => Eq/refl() end;\n\nlet through(n: Nat, h: Eq()(n, n)) -> Nat = n;\n\nlet _claim: Eq()(through(${steps}, ${proof}), ${steps}) = Eq/refl();\n\n/std/print(\"ok\")\n",
        steps,
        proof
    )
}

/// A type does not run a proof it only binds. The claim's call hands `through` a proof that costs an induction a thousand deep, at a binder that states it binds one, so both checkers bind it as written and the claim costs what it costs with `Eq/refl()` there. Evaluated before it is bound, the proof is the whole of the claim's cost, twenty times the rest.
#[test]
fn a_type_does_not_run_a_proof_it_only_binds() {
    let (proved_elaborated, proved_checked) =
        declaration_cost(&proof_argument_claim(1000, "checked(1000)"));
    let (plain_elaborated, plain_checked) =
        declaration_cost(&proof_argument_claim(1000, "Eq/refl()"));

    assert!(
        proved_elaborated.units() * 4 <= plain_elaborated.units() * 5
            && proved_checked.units() * 4 <= plain_checked.units() * 5,
        "the elaborator spends {} units with the proof computed and {} with it trivial, and the certifier {} and {}",
        proved_elaborated.units(),
        plain_elaborated.units(),
        proved_checked.units(),
        plain_checked.units()
    );
}

/// A program whose one claim is that `/std/http/Url/lit` reads a URL of `characters` characters: `lit` discharges `Valid(s)` by running `/std/http/Url/of_str` in a type, so what the declaration costs is what the reader costs a type.
fn url_literal(characters: usize) -> String {
    let literal = match characters {
        32 => "https://example.com:8443/a/b?x=1",
        64 => "https://example.com:8443/api/v1/items/42/parts?sort=name&page=12",
        128 => {
            "https://example.com:8443/api/v1/items/42/parts/7/revisions/2024/attachments/photo.png?sort=name&page=12&size=50&filter=open&x=12"
        }
        other => panic!("no literal of {other} characters"),
    };
    assert_eq!(literal.len(), characters);

    fill!(
        "use /std/http/{Url};\n\nlet home: Url =\n    Url/lit(\"${literal}\");\n\n/std/print(Url/to_str(home))\n",
        literal
    )
}

/// What reading the 32-character literal cost the heavier checker while `/std/http/Url/of_str` read the bytes of its text, counted at `eef14bad1`: the least budget under which the program is accepted.
const URL_OVER_BYTES_AT_32: u64 = 69_816;

/// **A URL literal costs a type what its length does, read as text.** `/std/http/Url/of_str` reads `Parse(Str, Url)`, each piece a cut of the input between two positions, and `lit` runs it in a type. A position's bound is a proof and a walk's step carries one, and neither checker runs a proof it only binds, so the reader over text costs under twice what the reader over bytes did, and a literal twice as long costs no more than twice as much.
///
/// With the proofs run, a doubling costs more than a doubling, since each step's bound is a lemma by induction on the offset it stands at.
#[test]
fn a_url_literal_read_as_text_costs_a_type_what_its_length_does() {
    let heavier = |characters: usize| {
        let (elaborator, certifier) = declaration_cost(&url_literal(characters));

        elaborator.units().max(certifier.units())
    };
    let [short, middle, long] = [32, 64, 128].map(heavier);

    assert!(
        short < 2 * URL_OVER_BYTES_AT_32,
        "the 32-character literal costs {short} units, where the reader over bytes cost {URL_OVER_BYTES_AT_32}"
    );
    assert!(
        middle <= 2 * short && long <= 2 * middle,
        "a doubling costs more than a doubling: {short}, {middle} and {long} units at 32, 64 and 128 characters"
    );
}

/// A closed recursion through an intrinsic's operand runs at constant native depth on both checkers: `sum(k) + 1` reads `sum(k)` forced, and the machine forces it on its own stack rather than starting a reduction of the host's per level, which overflowed the native stack.
#[test]
fn a_recursion_through_an_intrinsics_operand_runs_at_constant_native_depth() {
    assert_eq!(
        run(r#"
            use /std/{Nat};
            use /std/Bool/{True, Holds};

            pub let sum(n: Nat) -> Nat = match n | 0 => 0 | k + 1 => sum(k) + 1 end;
            pub let x: Holds(sum(20000) == 20000) = True/qed();

            /std/print("ok")
            "#),
        b"ok"
    );
}
