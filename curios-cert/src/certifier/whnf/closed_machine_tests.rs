//! The closed machine, checked against the strategy it accelerates.

use {
    super::test_support::*,
    crate::{test_support::*, *},
    curios_common::{Qualifier, fill},
    curios_core::{Global, Reducer, Term, UniverseContext},
};

/// `g`, which adds one, at `${universes}`, and `twice`, which applies a function twice.
const TWICE: &str = r#"
${universes}let /g: (x: #NatType) -> #NatType = (x: #NatType) => #NatAdd(x, 1);
let /twice: (f: (x: #NatType) -> #NatType) -> (x: #NatType) -> #NatType =
    (f: (x: #NatType) -> #NatType) => (x: #NatType) => f(f(x));
"#;

/// The name [`TWICE`] declares `g` under.
fn g() -> Global {
    Global::Authored(Qualifier::from(["g"]))
}

/// A global name handed to a closed function stays a name in what the machine hands back, exactly as it does under the strategy: `twice(g)` at a plain demand is `(x) => g(g(x))` with `g` *named*, not `g`'s body substituted twice. A machine that evaluates every beta argument and substitutes its value would inline a function-valued global's definition once per occurrence — and a web of definitions each naming the one before it twice would come back as a graph whose tree is `2^n`. The strategy substitutes the argument as written, so the two reducts would differ here, and this fixture is the one that sees it.
#[test]
fn the_closed_machine_keeps_a_global_argument_as_a_name() {
    let definitions = fill!(TWICE, universes = "");
    let passed = term("/twice(/g)");

    let mut machined = certifier();
    declared(&mut machined, &definitions);
    let mut strategy = strategy_certifier();
    declared(&mut strategy, &definitions);

    let reduct = machined
        .reduce(passed.clone())
        .expect("the machine reduces");
    assert_eq!(
        Some(reduct.clone()),
        strategy.reduce(passed).ok(),
        "the machine and the strategy agree on the function handed back"
    );
    assert!(
        reduct.mentions_declaration(&g()),
        "the argument survives as the name it was passed as:\n{reduct:?}"
    );
}

/// A witness named at a universe instance stays that name where it is passed as one: `twice(use g.{0})` hands back `g` at its instance, as the strategy does, where a machine that evaluated it put the witness's dictionary wherever an alias over the witness unfolded, in a reported type included. Passed as an ordinary argument the same instance is evaluated, which is what tells two instances of a family apart in a level from one: the function handed back holds `g`'s body and no name.
#[test]
fn the_closed_machine_keeps_a_witness_named_at_an_instance_as_written() {
    let definitions = fill!(
        TWICE,
        universes = "#{universe_context = {parameter_count = 1}}\n"
    );
    let passed = |mark: &str| {
        term(&fill!(
            "/twice(${mark}#{universes = {levels = [0]}} /g)",
            mark
        ))
    };

    let mut machined = certifier();
    declared(&mut machined, &definitions);
    let mut strategy = strategy_certifier();
    declared(&mut strategy, &definitions);

    let witness = machined
        .reduce(passed("use "))
        .expect("the machine reduces");
    assert_eq!(
        Some(witness.clone()),
        strategy.reduce(passed("use ")).ok(),
        "the machine and the strategy agree on the function handed back"
    );
    assert!(
        witness.mentions_declaration(&g()),
        "the witness survives as the instance it was passed as:\n{witness:?}"
    );

    let ordinary = machined.reduce(passed("")).expect("the machine reduces");
    assert!(
        !ordinary.mentions_declaration(&g()),
        "an instance passed as an ordinary argument is evaluated:\n{ordinary:?}"
    );
}

/// The bytes `data` as a literal's text, `x[0x01, …]`.
fn bytes(data: &[u8]) -> String {
    let written: Vec<String> = data.iter().map(|byte| format!("0x{byte:02X}")).collect();

    format!("x[{}]", written.join(", "))
}

/// `go`, a member walking a `Bytes` to its end, as the projection out of its group a call names it by.
const WALK: &str = "#{node = Rec} (let go: (b: #BytesType) -> #NatType = (b: #BytesType) => match b: (m) => #NatType | x[] => 0 | x[h, ..t]; ih => go(t) end; go)";

/// `member`, which descends from `n` to its zero case, answering `${zero}` at `${result}` there and `${step}` at each successor over `pred` and the hypothesis `ih`, as the projection out of its group a call names it by.
const DESCENT: &str = "#{node = Rec} (let member: (n: #NatType) -> ${result} = (n: #NatType) => match n: (m) => ${result} | 0 => ${zero} | pred + 1; ih => ${step} end; member)";

/// **The differential fixture that holds the machine to the strategy.** The same closed terms are put to a certifier with the closed machine and to one without it — the recursive strategy — and the reducts must be identical, term for term, **at both demands**. The battery covers each rule the machine implements on its own: beta over eagerly-evaluated arguments, zeta's left-to-right release, all four match families, projection, recursive unfolding to a value, and the two fold recursion encodings over a packed carrier. Both evaluators determine these completely — a first-order value at the forced demand, and at the plain one either that or the folded spelling the demand stops at — so equality here is syntactic rather than up-to-anything.
///
/// Both demands are asked because the plain one is where the machine and the strategy can disagree, on the forced-then-plain terms below: a `rec` block both evaluators leave unopened at a plain demand is compared there but reaches nothing.
#[test]
fn the_closed_machine_agrees_with_the_strategy() {
    let ih_fold = fill!(
        "match ${bytes}: (m) => #NatType | x[] => 0 | x[h, ..t]; ih => #NatAdd(ih, 1) end",
        bytes = bytes(&[7; 40])
    );

    let tail_fold = fill!(
        "#{node = Rec} (let go: (acc: #NatType, b: #BytesType) -> #NatType = (acc: #NatType, b: #BytesType) => match b: (m) => #NatType | x[] => acc | x[h, ..t]; ih => go(#NatAdd(acc, #ByteToNat(h)), t) end; go(0, ${bytes}))",
        bytes = bytes(&[3; 40])
    );

    let countdown = "#{node = Rec} (let member: (n: #NatType) -> #NatType = (n: #NatType) => match n: (m) => #NatType | 0 => 0 | pred + 1; ih => member(pred) end; member(9))";

    let beta_zeta = "((x: #NatType, y: #NatType) => let y: #NatType = #NatAdd(x, 3); y)(2, 0)";

    let switch = "match 2: (m) => #NatType | 1 => 10 | 2 => 20 | _ => 99 end";

    let projection = "(10, 20, 30).2";

    let induct = "match #{node = Variant} /E/some(#NatAdd(40, 2)): (m) => #NatType | none() => 0 | some(a) => a end";

    // A run that forces a *bare* member selection and then asks a plain demand for a call on the same member. Both demands are exercised in one term, so that one run records at the first and is probed at the second: the `let` value is an intrinsic operand, which is forced, and its tail is an ordinary application, which must come back folded. Keyed on the term alone, the memo would answer a projection recorded at the forced demand to the plain probe, and the machine would run the whole fold where the strategy stops at the folded spelling.
    let call = fill!("(${go})(${bytes})", go = WALK, bytes = bytes(&[1, 2, 3]));
    let forced_then_plain = fill!(
        "(let x: #NatType = #NatAdd(${go}, 1); ${call})",
        go = WALK,
        call
    );

    // The same order through a call and through a match rather than a bare selection: the `let` value's operand is forced, so the call — or the match whose arm is the call — is remembered with the value the fold computes, and the tail asks the same term plainly, where the folded call is the answer. Keyed on the term alone, the memo would serve the forced value; mutation-checked by reading the forced table at a plain demand.
    let forced_then_plain_through = |subject: &str| {
        fill!(
            "(let x: #NatType = #NatAdd(${subject}, 1); ${subject})",
            subject
        )
    };
    let matched_call = fill!(
        "match true: (m) => #NatType | false => 0 | true => ${call} end",
        call
    );

    // A group whose one member never mentions itself: opening its tail reduces past the projection to the member's own value, so what the head exposes is a `Func` and the application is ordinary beta. The machine takes it from the apply arm, the strategy through `expose_rec_tail`.
    let dissolved_group = "(#{node = Rec} (let unused: (n: #NatType) -> #NatType = (n: #NatType) => n; (n: #NatType) => n))(2)";

    // A member whose result is a function, called past its own parameters: `f(2)(3)` is the call `f(2)` applied to `3`. The plain demand stops at the folded spine, and the forced one reaches through it to the call — which a head frame evaluating its head at the plain demand would hand back unforced while the strategy unfolds it.
    let past_parameters = fill!(
        "(${member})(2)(3)",
        member = fill!(
            DESCENT,
            result = "(x: #NatType) -> #NatType",
            zero = "(x: #NatType) => x",
            step = "(x: #NatType) => member(pred)(x)"
        )
    );

    // The same call demanded plainly and then forced, in one run: the `let` value is released at the plain demand, where the folded spine is the answer, and the tail's operand is forced. A forced probe reads the plain table as well, so the memo's guard must recognize the spine as folded and decline to record it — read one application deep, it would record the spine and serve it to the forced probe, and the sum would stick on it.
    let plain_then_forced = fill!(
        "(let released: #NatType = ${call}; #NatAdd(${call}, 0))",
        call = past_parameters
    );

    // Two closed forwardings under a hidden mark, which the machine contracts through `eta_head` and the strategy through its own statement of the rule: one whose argument stands under its binder's mark contracts, `(@n) => ((@m) => m)(@n)` to `(@m) => m`, and one whose argument does not stays as written.
    let forwarded_under_its_mark = "(@n: #NatType) => ((@m: #NatType) => m)(@n)";
    let forwarded_across_a_mark = "(@n: #NatType) => ((x: #NatType) => x)(n)";

    for term in [
        chain(64),
        term(dissolved_group),
        term(forwarded_under_its_mark),
        term(forwarded_across_a_mark),
        term(&past_parameters),
        term(&plain_then_forced),
        term(&ih_fold),
        term(&tail_fold),
        term(countdown),
        term(beta_zeta),
        term(switch),
        term(projection),
        term(induct),
        term(&forced_then_plain),
        term(&forced_then_plain_through(&call)),
        term(&forced_then_plain_through(&matched_call)),
    ] {
        let mut machined = certifier();
        let mut strategy = strategy_certifier();

        assert_eq!(
            machined.reduce_forced(term.clone()),
            strategy.reduce_forced(term.clone()),
            "the machine and the strategy disagreed on {term:?}",
        );

        // The plain demand is a separate contract, not a weaker reading of the one above: it is where a folded recursive spelling is the answer rather than a step on the way to one, so a machine that unfolds here computes a value the strategy never offers. Asking only the forced demand would leave that whole half of the machine uncompared.
        let mut machined = certifier();
        let mut strategy = strategy_certifier();

        assert_eq!(
            machined.reduce(term.clone()),
            strategy.reduce(term.clone()),
            "the machine and the strategy disagreed at a plain demand on {term:?}",
        );
    }
}

/// **Runs past the fusion cap are the bytes they spell under the machine as under the strategy.** A concatenation of runs too long to fuse stays a node, and both evaluators fold a pointwise operation and a structural fold over it through it: `xor` of two such values split apart at different seams, one of them nested, is the elementwise run, and the fold over that run sums its bytes. A machine that read the node otherwise than the strategy would answer a closed computation in a type with a value reduction of the same term would not.
#[test]
fn the_closed_machine_reads_runs_past_the_fusion_cap_as_the_strategy_does() {
    let run = |byte: u8, count: usize| bytes(&vec![byte; count]);
    // A stand-in for the discharged bound, closed so the machine takes the term; no fold reads it.
    let xor = fill!(
        "#BytesXor(#BytesConcat([${a}, ${b}]), #BytesConcat([${c}, #BytesConcat([${d}, ${e}])]), 0)",
        a = run(0x0F, 65),
        b = run(0x3C, 66),
        c = run(0xF0, 64),
        d = run(0x55, 1),
        e = run(0xAA, 66)
    );

    let mut combined = vec![0xFF; 64];
    combined.push(0x0F ^ 0x55);
    combined.extend([0x3C ^ 0xAA; 66]);
    let total = combined
        .iter()
        .map(|&byte| usize::from(byte))
        .sum::<usize>();

    let summed = fill!(
        "match ${xor}: (m) => #NatType | x[] => 0 | x[h, ..t]; ih => #NatAdd(#ByteToNat(h), ih) end",
        xor
    );

    for (written, value) in [(xor, bytes(&combined)), (summed, total.to_string())] {
        let candidate = term(&written);
        assert_eq!(
            certifier().reduce_forced(candidate.clone()),
            Ok(term(&value)),
            "the machine answers the bytes the runs spell for {candidate:?}",
        );
        assert_eq!(
            certifier().reduce_forced(candidate.clone()),
            strategy_certifier().reduce_forced(candidate.clone()),
            "the machine and the strategy agree on {candidate:?}",
        );
    }
}

/// A closed one-arm match on a proof of a proposition whose index pins its payload fires on the *stated* instance with the proof unevaluated — `ClosedHost::stated_payload` — and the machine and the strategy bind the same payload and reach the same value. The arm reads its payload, so the rule's second clause runs rather than the `unread` one: `match mk(5) at P(5) : (i, q) => Nat | mk(x) => x end` is `5` under both. A machine that read the payload off the scrutinee while the strategy read it off the stated instance (or the reverse) would answer a closed computation in a type with a value reduction of the same term would not.
#[test]
fn the_closed_machine_reads_a_stated_proof_payload_as_the_strategy_does() {
    let family = "induct /P: (i: #NatType) -> pub Prop\n| mk(x: #NatType): (x)\nend";
    let matched = term(
        "#{cases = {stated = #{node = InductType} /P()(5)}} match #{node = Variant} /P/mk(5): (_, _) => #NatType | mk(x) => x end",
    );

    let mut machined = certifier();
    declared(&mut machined, family);
    let mut strategy = strategy_certifier();
    declared(&mut strategy, family);

    assert_eq!(
        machined.reduce_forced(matched.clone()),
        Ok(term("5")),
        "the stated payload is read off the pinned index",
    );
    assert_eq!(
        machined.reduce_forced(matched.clone()),
        strategy.reduce_forced(matched.clone()),
        "the machine and the strategy read one payload for {matched:?}",
    );
}

/// Declare in `certifier` the opaque `g`, a name with a type and no body: no text writes a definition without one, so the entry is declared as a judgment declares one before its body.
fn declare_g(certifier: &mut Certifier) {
    certifier
        .declare(
            &g(),
            &term("(x: #NatType) -> #NatType"),
            &UniverseContext::default(),
        )
        .expect("declared once");
}

/// [`DESCENT`] at `Nat` from `3`, answering `0` at zero and `step` at each successor.
fn descending(step: &str) -> Term {
    term(&fill!(
        "(${member})(3)",
        member = fill!(DESCENT, result = "#NatType", zero = "0", step)
    ))
}

/// The differential's case of a recursive step that applies a head outside the group to the folded call: `member(3)` with `member(n) = match n | 0 => 0 | pred => g(member(pred))` and `g` opaque. The strategy's `force` keeps the folded spelling wherever the reduct still names the member, and the machine's guard reads the stuck application as neutral the same way, so both hand back `member(3)` rather than one of them `g(member(2))`. Asked apart from the loop above because `g` needs a declaration in both certifiers.
#[test]
fn the_closed_machine_folds_a_call_under_an_opaque_head_as_the_strategy_does() {
    let call = descending("/g(member(pred))");

    let mut machined = certifier();
    declare_g(&mut machined);
    let mut strategy = strategy_certifier();
    declare_g(&mut strategy);

    assert_eq!(
        machined.reduce_forced(call.clone()),
        strategy.reduce_forced(call.clone()),
        "the machine and the strategy disagreed on {call:?}",
    );
}

/// The restuck call under heads the opaque one is reached through: a transparent `wrap(x) = g(x)` the machine opens to the opaque `g`, an intrinsic over the stuck call, `g(member(pred)) + 1`, and the stuck call as an argument of a second, `g(g(member(pred)))`. Each is one value however it is spelled, so the machine and the strategy hand back one forced form.
#[test]
fn the_closed_machine_folds_a_call_restuck_under_a_reached_head_as_the_strategy_does() {
    let declare = |certifier: &mut Certifier| {
        declare_g(certifier);
        declared(
            certifier,
            "let /wrap: (x: #NatType) -> #NatType = (x: #NatType) => /g(x);",
        );
    };

    for (label, step) in [
        ("wrap", "/wrap(member(pred))"),
        ("plus one", "#NatAdd(/g(member(pred)), 1)"),
        ("twice", "/g(/g(member(pred)))"),
    ] {
        let call = descending(step);

        let mut machined = certifier();
        declare(&mut machined);
        let mut strategy = strategy_certifier();
        declare(&mut strategy);

        assert_eq!(
            machined.reduce_forced(call.clone()),
            strategy.reduce_forced(call.clone()),
            "{label}: the machine and the strategy disagreed on {call:?}",
        );
    }
}

/// **A host call's operands stand as the strategy leaves them.**
///
/// `whnf` leaves a `Foreign` node whole — it is one of the shapes its loop stops on without entering — and conversion reads it that way: "A host call is its declaration and its operands, and reduction leaves both as they were posed". The machine evaluates every operand before rebuilding the node, which the module documentation defends for *substituted* positions by confluence; a node reduction stands on is not one, and the two evaluators then spell one closed term two ways.
#[test]
fn a_host_calls_operands_are_left_as_the_strategy_leaves_them() {
    let call = closing(
        "foreign /probe(Nat) -> Nat;\nlet /probe: (n: #NatType) -> #IoType(#NatType) = (n: #NatType) => #{node = Foreign} /probe(n);\n#{node = Foreign} /probe(#NatType, #NatAdd(1, 2))",
    );

    let mut machined = certifier();
    let mut strategy = strategy_certifier();

    assert_eq!(
        machined.reduce(call.clone()),
        strategy.reduce(call.clone()),
        "the machine and the strategy disagreed at a plain demand on {call:?}",
    );
}

/// A fold over the list `${items}`, at `${result}`, from the call `member(3)` of a [`DESCENT`] at `${result}` that answers `${zero}` at zero and its hypothesis at each successor, by a step that hands its accumulator back: `member(n)` is `${zero}` for every `n`.
const FOLDED: &str = "#ListFold(#NatType, ${result}, #List(#NatType, [${items}]), (${member})(3), (item: #NatType, acc: ${result}) => acc)";

/// [`FOLDED`] over `items`, at `result` with `zero` at zero.
fn folded(items: &str, result: &str, zero: &str) -> String {
    fill!(
        FOLDED,
        items,
        result,
        member = fill!(DESCENT, result, zero, step = "ih")
    )
}

/// **An operand a fold reads plainly is read plainly under the machine as under the strategy.**
///
/// A fold over the empty list is its initial value, which `reduce_intrinsic` reads at the *plain* demand (`reducer.reduce(init)`): a folded recursive call is its own normal form there, so the strategy's answer is the folded call. The machine evaluates every computed operand at [`Demand::Forced`] before the fold runs and serves those values through its `Tap`, which answers `reduce` and `reduce_forced` out of one table — so the fold reads the operand's forced value where it asked for its plain one.
///
/// The term is a closed one, so which evaluator takes it is not the caller's choice: the machine's gate decides it, and the machine's own documentation is what this holds — a closed term has one value at a demand, "evaluated by the machine inside an arm and by the recursive strategy outside it, it would reach the same value spelled two ways, and a remembered reduct, or a solution spelled from one, would follow which of the two was asked first".
#[test]
fn an_operand_a_fold_reads_plainly_is_not_served_its_forced_value() {
    let fold = term(&folded("", "#NatType", "0"));

    let mut machined = certifier();
    let mut strategy = strategy_certifier();

    assert_eq!(
        machined.reduce(fold.clone()),
        strategy.reduce(fold.clone()),
        "the machine and the strategy disagreed at a plain demand on {fold:?}",
    );
}

/// **A match or a projection that forces an intrinsic reads the forced form of what the fold answers.**
///
/// A fold over `[5]` whose step hands its accumulator back answers its initial value, here the folded call `member(3)`: the plain form, which a match on it or a projection out of it has to force, as the strategy forces what `whnf` answered before it dispatches. Handed the folded call, the machine's match stood stuck, and a recursion stepping through one never computed.
///
/// Mutation-checked at the machine's `Finish::Intrinsic`: with the answer handed back at whatever demand the intrinsic was evaluated at, the match stands stuck and the projection with it.
#[test]
fn a_match_or_a_projection_forces_what_an_intrinsic_answers() {
    let pair = "{first: #NatType, second: #NatType}";
    let matched = term(&fill!(
        "match ${folded}: (m) => #NatType | 0 => 7 | pred + 1; ih => 9 end",
        folded = folded("5", "#NatType", "0")
    ));
    let projected = term(&fill!("${folded}.0", folded = folded("5", pair, "(0, 1)")));

    for (label, candidate) in [("the match", &matched), ("the projection", &projected)] {
        let mut machined = certifier();
        let mut strategy = strategy_certifier();
        assert_eq!(
            machined.reduce(candidate.clone()),
            strategy.reduce(candidate.clone()),
            "{label}: the machine and the strategy disagreed at a plain demand on {candidate:?}",
        );
        assert_eq!(
            machined.reduce_forced(candidate.clone()),
            strategy.reduce_forced(candidate.clone()),
            "{label}: the machine and the strategy disagreed at the forced demand on {candidate:?}",
        );
    }
    let mut machined = certifier();
    assert_eq!(
        machined.reduce(matched),
        Ok(term("7")),
        "the match took no arm"
    );
}
