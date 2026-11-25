use super::*;

/// Pins the census machinery to the shapes a string walk does not carry: the accumulator travels as fields through continuation scalar replacement, and the suffix view as window fields through the window split, so neither survives as a region — and the census still surveys what remains.
#[test]
fn surveys_the_fold_accumulator_region() {
    let source = r#"
        use /std/{Nat, Str, Char, List, Proc};
        let entry() -> /std/Io({}) =
            let taint = List/length(Proc/args()!);
            let n : Nat = taint;
            let text : Str = Nat/to_str(n);
            /std/print(Nat/to_str(Str/fold(text, 0, (codepoint, acc) => acc + Char/to_nat(codepoint))));
        entry()
        "#;
    let module = cps_optm_module(source);
    let census = Census::of(&module);
    let regions = census.regions();

    let accumulator = regions.iter().find(|region| {
        region.tuple_sites.len() >= 2
            && region.arities == BTreeSet::from([2])
            && !region.params.is_empty()
            && region.classes.contains("continuation-transfer")
            && region.classes.contains("projection")
    });
    assert!(
        accumulator.is_none(),
        "the accumulator region dissolved into fields before the survey: {accumulator:#?}",
    );

    let rope = regions.iter().find(|region| {
        region.slice_sites.len() >= 2
            && !region.params.is_empty()
            && region.classes.contains("continuation-transfer")
    });
    assert!(
        rope.is_none(),
        "the suffix-view region virtualized into window fields before the survey: {rope:#?}",
    );

    // What survives in this all-ASCII fixture is the Io plumbing no mechanism claims — enough to pin that the machinery still surveys; the variant-width scan flow needs multi-byte text and is the corpus survey's to report.
    assert!(!regions.is_empty(), "the census still surveys the walk");
}

/// The classifier pinned on the canonical pair, so the census above stays a measurement: a cons rebuilt from a predecessor dying in the same function pairs at its row, and a program whose constructions never see a dying same-layout value reports none.
#[test]
fn death_birth_classifier_pins_the_canonical_pair() {
    let paired = r#"
        use /std/{Str, Nat, Option, Io};
        induct Chain: Type
        | nil()
        | cons(Nat, Chain)
        end
        let step(rest: Chain, acc: Chain) -> Chain =
            match rest
            | nil() => acc
            | cons(v, tail) => step(tail, Chain/cons(v + 1, acc))
            end;
        let build(n: Nat, acc: Chain) -> Chain =
            match n
            | 0 => acc
            | m + 1 => build(m, Chain/cons(n, acc))
            end;
        let total(c: Chain, acc: Nat) -> Nat =
            match c
            | nil() => acc
            | cons(v, tail) => total(tail, (acc + v) % 1000003)
            end;
        let entry() -> /std/Io({}) =
            let arguments = /std/Proc/args()!;
            let input = /std/List/try_get(arguments, 1);
            match input
            | some(bytes) =>
                match Str/of_bytes(bytes)
                | success(s) =>
                    match Nat/of_str(Str/trim(s))
                    | some(n) => /std/print(Nat/to_str(total(step(build(n, Chain/nil()), Chain/nil()), 0)))
                    | none() => /std/print("bad input")
                    end
                | failure(_) => /std/print("invalid utf-8")
                end
            | none() => /std/print("no input")
            end;
        entry()
    "#;
    let module = cps_optm_module(paired);
    let census = Census::of(&module);
    let tally = rebirth(&census);
    assert!(
        tally.row_pairs >= 1,
        "the rebuilt cons beside its dying predecessor pairs: {tally:?}",
    );
}
