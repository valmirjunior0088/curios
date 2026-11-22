//! A name already declared: a module is refused for it before anything is judged, whether the name is its environment's or one of its own.

use {
    super::test_support::*,
    crate::*,
    curios_analysis::test_support::*,
    curios_common::fill,
    curios_core::{Definitions, Global, Places},
};

/// Whether `verdicts` is the one refusal a module redeclaring `name` meets: a redeclaration is refused before anything is judged, so no other verdict stands beside it.
fn redeclares(verdicts: &[Refusal], name: &Global) -> bool {
    matches!(
        verdicts,
        [Refusal { name: Some(owner), error: Error::Redeclared(redeclared) }]
            if owner == name && redeclared == name
    )
}

/// What the walk says of `text` over [`JUDGED_ENVIRONMENT`], in scope as a unit no walk filed a record for.
fn over_the_environment(text: &str) -> Rechecked<Definitions> {
    text_certified_over(text, &already_judged(&judged_environment()).globals())
}

/// `shadowed : Bool = true`, and `reader : Nat = shadowed`.
const SHADOWING: &str = r#"
let /shadowed: #BoolType =
    true;

let /reader: #NatType =
    /shadowed;
"#;

/// A definition under a name the environment holds, differing from the environment's, is refused for the name.
///
/// `shadowed` is `Nat` in the environment and `Bool` in the module, and `reader : Nat = shadowed` reads it. Standing alone the module is refused, `reader` inferring `Bool` where `Nat` was declared. Passed over by name it would be certified, `reader` typed against the environment's `shadowed` while every stage below compiles the body the module holds; so the module is refused for `shadowed`, and `reader` is not judged at all.
///
/// Mutation-checked against the environment's question: with an item's held name never asked after, the module is judged, and `shadowed` is refused where the walk writes it and no sooner.
#[test]
fn a_definition_differing_from_the_environments_is_refused() {
    let alone = text_refusals(SHADOWING);
    assert!(
        refuses(&alone, Some("reader"), |error| matches!(
            error,
            Error::Mismatch { .. }
        )),
        "the control stopped refusing: `reader : Nat` must not accept a `Bool`-typed `shadowed`: {alone:?}",
    );

    let verdicts = over_the_environment(SHADOWING).into_refusals();
    assert!(
        redeclares(&verdicts, &global("shadowed")),
        "a definition differing from the environment's under its name was not refused for the name: {verdicts:?}",
    );
}

/// `Shadowed : Type 0 | mk(a : ${domain})`.
const SHADOWING_FAMILY: &str = r#"
induct /Shadowed: pub Type
| mk(a: ${domain})
end
"#;

/// A registry entry under a name the environment holds, differing from the environment's, is refused for the name.
///
/// Declared over the environment's it would be the entry every rule reads, with the clauses that check a declaration passed over: a payload at `Type 5` under a family declared at `Type 0` is refused as `Oversized` standing alone, and by name alone it would be certified against an environment holding a benign `Shadowed`.
///
/// Mutation-checked against the environment's question: with a held name never asked after, the entry is refused where the walk writes it and judged all the same, `Oversized` beside the name's refusal.
#[test]
fn a_registry_entry_differing_from_the_environments_is_refused() {
    let oversized = fill!(SHADOWING_FAMILY, domain = "#{level = 5} Type");

    let alone = text_refusals(&oversized);
    assert!(
        refuses(&alone, Some("Shadowed"), |error| matches!(
            error,
            Error::Oversized { .. }
        )),
        "the control stopped refusing: a `Type 5` payload exceeds a family declared at `Type 0`: {alone:?}",
    );

    let verdicts = over_the_environment(&oversized).into_refusals();
    assert!(
        redeclares(&verdicts, &global("Shadowed")),
        "a registry entry differing from the environment's under its name was not refused for the name: {verdicts:?}",
    );
}

/// `Wrapper(A) : Type 0 | wrap(held : ${held})`, its entry stamped `Strict` in `A` whatever `held` makes of it: elaboration's vector where `held` holds `A` strictly, and a lie where it does not.
const WRAPPER: &str = r#"
#{polarities = [Strict]}
induct /Wrapper(A: Type): pub Type
| wrap(held: ${held})
end
"#;

/// `Outer : Type 0 | c(held : Wrapper(Outer))`, over a unit declaring `Wrapper`.
const STORED_IN_THE_WRAPPER: &str = r#"
induct /Outer: pub Type
| c(held: #{node = InductType} /Wrapper(#{node = InductType} /Outer()))
end
"#;

/// What the walk says of [`STORED_IN_THE_WRAPPER`] over [`WRAPPER`] at `held`, a unit well formed on its own, in scope as the walk over it filed it.
fn stored_refusals(held: &str) -> Vec<Refusal> {
    let unit = core_text_module(&fill!(WRAPPER, held));

    text_refusals_over(STORED_IN_THE_WRAPPER, &already_judged(&unit).globals())
}

/// Whether `Outer` was refused as negative in itself.
fn outer_not_positive(verdicts: &[Refusal]) -> bool {
    refuses(verdicts, Some("Outer"), |error| {
        matches!(error, Error::NotPositive { .. })
    })
}

/// The polarity vector a walk composes through is the one the certifier filed for the family, never the one its entry carries.
///
/// `Wrapper(A)` holds a function out of `A`, and its entry is stamped `Strict` in `A`: elaboration's vector, and here a lie. `Outer` stores a `Wrapper(Outer)`, so it reaches itself to the left of an arrow and is refused. The walk over `Outer` is handed `Wrapper`'s unit as the walk over it filed it, whose record files the vector that walk computed. Believing the stamp accepts `Outer`, a family negative in itself.
///
/// The control is the same with `Wrapper` holding an `A`: the stamp is then the truth and `Outer` is accepted, so the refusal is the vector's and not that of every family storing itself in a mounted one.
///
/// Mutation-checked against the environment's construction: keeping the vector each entry carries accepts `Outer`.
#[test]
fn a_mounted_familys_vector_is_the_certifiers_and_not_its_entrys() {
    let verdicts = stored_refusals("(taken: A) -> #NatType");
    assert!(
        outer_not_positive(&verdicts),
        "`Outer` was accepted on a `Strict` its entry's stamp claims: {verdicts:?}",
    );

    assert_eq!(
        stored_refusals("A"),
        Vec::new(),
        "the control stopped holding: `Outer` stores itself strictly",
    );
}

/// The same lie told of a carrier: `Wrapper(A)` holds its parameter in a cell or a box, which are read and written and so invariant, or a function out of it in a list or a description, which are covariant and keep the arrow's left where it stands, and its entry is stamped `Strict` in `A` all the same. The walk reads an intrinsic carrier's parameter by its own rule and files that vector, so `Outer`, storing a `Wrapper(Outer)`, is refused.
///
/// Mutation-checked at `curios-analysis`'s positivity walk: reading a cell's element at the polarity it stands at accepts `Outer`.
#[test]
fn a_mounted_familys_carrier_is_read_by_the_certifier_and_not_by_its_entry() {
    let carriers = [
        ("a cell", "#CellType(A)"),
        ("a box", "#BoxType(A)"),
        (
            "a list of functions out of it",
            "#ListType((taken: A) -> #NatType)",
        ),
        (
            "a description of a function out of it",
            "#IoType((taken: A) -> #NatType)",
        ),
    ];

    for (carrier, held) in carriers {
        let verdicts = stored_refusals(held);
        assert!(
            outer_not_positive(&verdicts),
            "`Outer` was accepted through a `Wrapper` holding {carrier} on the `Strict` its stamp claims: {verdicts:?}",
        );
    }
}

/// `reader : Nat = shadowed`.
const READER: &str = r#"
let /reader: #NatType =
    /shadowed;
"#;

/// A declaration under a name the environment holds is refused for the name though it is the environment's, term for term: what an environment holds is read where it stands, and no comparison decides that two declarations under one name are one.
///
/// The module carries the environment's `shadowed` and `Shadowed`, read again so that neither is the environment's allocation, beside `reader : Nat = shadowed`. Each held name is refused once and nothing is judged, `reader` with the rest: the refusals are all the walk answers.
///
/// Mutation-checked against the environment's question: with a held name never asked after, the module is judged, and each name is refused where the walk writes it, once for each write.
#[test]
fn a_declaration_equal_to_the_environments_is_refused_for_its_name() {
    let rechecked = over_the_environment(&fill!(
        "${JUDGED_ENVIRONMENT}\n${READER}",
        JUDGED_ENVIRONMENT,
        READER
    ));

    let refused = rechecked
        .refusals()
        .iter()
        .map(|verdict| match &verdict.error {
            Error::Redeclared(redeclared) => (verdict.name, Some(*redeclared)),
            _ => (verdict.name, None),
        })
        .collect::<Vec<_>>();
    assert_eq!(
        refused,
        ["Shadowed", "shadowed"].map(|symbol| (Some(global(symbol)), Some(global(symbol)))),
        "a module carrying its environment's own declarations was not refused once for each name",
    );
}

/// `Shadowed : Nat = 0`, a definition under the name [`JUDGED_ENVIRONMENT`] declares a family by.
const DEFINITION_UNDER_THE_FAMILY: &str = r#"
let /Shadowed: #NatType =
    0;
"#;

/// A name is one declaration whatever it declares: a definition under a name the environment declares a family by is refused for it.
///
/// The definition is well typed, and standing alone it is certified, so the refusal is the name's.
///
/// Mutation-checked against the environment's question: with a held name never asked after, the definition is judged and refused where the walk writes it.
#[test]
fn a_definition_under_a_name_the_environment_declares_a_family_by_is_refused() {
    assert_eq!(
        text_refusals(DEFINITION_UNDER_THE_FAMILY),
        Vec::new(),
        "the control stopped certifying: the definition is well typed standing alone",
    );

    let verdicts = over_the_environment(DEFINITION_UNDER_THE_FAMILY).into_refusals();
    assert!(
        redeclares(&verdicts, &global("Shadowed")),
        "a definition under a family's name was not refused for the name: {verdicts:?}",
    );
}

/// One group of two constants, `shadowed` and `fresh`: the first under a name [`JUDGED_ENVIRONMENT`] holds, the second under none.
const GROUP_HELD_IN_PART: &str = r#"
let /shadowed: #NatType =
    0
and /fresh: #NatType =
    0;
"#;

/// A group one of whose names the environment holds is refused, named by its first member as a group is.
///
/// Mutation-checked against the environment's question: with an item's held name never asked after, the group is judged and refused where the walk writes its members, and no sooner.
#[test]
fn a_group_the_environment_holds_in_part_is_refused() {
    assert_eq!(
        text_refusals(GROUP_HELD_IN_PART),
        Vec::new(),
        "the control stopped certifying: the group is well formed standing alone",
    );

    let verdicts = over_the_environment(GROUP_HELD_IN_PART).into_refusals();
    assert!(
        redeclares(&verdicts, &global("shadowed")),
        "a group held in part was not refused for the name the environment holds: {verdicts:?}",
    );
}

/// `p : Prop = Holds` and `thm : p = Holds/qed()`, then `${again}`, closed with `thm` stated at `Absurd`.
const A_STATEMENT: &str = r#"
induct /Absurd: pub Prop
end

induct /Holds: pub Prop
| qed()
end

let /p: Prop =
    #{node = InductType} /Holds();

let /thm: /p =
    #{node = Variant} /Holds/qed();
${again}
/thm
: #{node = InductType} /Absurd()
"#;

/// A second `p : Prop = ((x : Holds) => Absurd)(thm)`.
const THE_STATEMENT_AGAIN: &str = r#"
let /p: Prop =
    ((x: #{node = InductType} /Holds()) => #{node = InductType} /Absurd())(/thm);
"#;

/// One group of two constants, both named `shadowed`.
const A_GROUP_NAMING_A_MEMBER_TWICE: &str = r#"
let /shadowed: #NatType =
    0
and /shadowed: #NatType =
    0;
"#;

/// A name is declared by one item of a module: a second item under it is refused, and so is a group naming one member twice.
///
/// The walk takes items as they are stored, and a name declared twice is written twice: `thm : p` is typed while `p` is `Holds`, the second `p` names `thm`, and its definition would stand where the first did — the closing `thm` then read at a `p` that reduces to `Absurd`. With `p` declared once the closing term is the `Mismatch` it should be.
///
/// Mutation-checked against the set of names already declared: with it never consulted the module is judged, and `p` is refused where the walk writes it again and no sooner.
#[test]
fn a_name_declared_by_two_items_is_refused() {
    let once = text_refusals(&fill!(A_STATEMENT, again = ""));
    assert!(
        refuses(&once, None, |error| matches!(error, Error::Mismatch { .. })),
        "the control stopped refusing: `thm : Holds` does not close a program at `Absurd`: {once:?}",
    );

    let twice = text_refusals(&fill!(A_STATEMENT, again = THE_STATEMENT_AGAIN));
    assert!(
        redeclares(&twice, &global("p")),
        "a name declared by two items was not refused: {twice:?}",
    );

    let repeated = text_refusals(A_GROUP_NAMING_A_MEMBER_TWICE);
    assert!(
        redeclares(&repeated, &global("shadowed")),
        "a group naming one member twice was not refused: {repeated:?}",
    );
}

/// `struct Bad { f : (Bad) -> Absurd }`, after `${family}`, closed with `w(Bad { w })` for `w = (b) => b.0(b)` stated at `Absurd`.
const A_RECORD_NEGATIVE_IN_ITSELF: &str = r#"
induct /Absurd: pub Prop
end
${family}
struct /Bad: pub Type {
    f: (devoured: #{node = StructType} /Bad()) -> #{node = InductType} /Absurd(),
}

((b: #{node = StructType} /Bad()) => (#{field = Struct(/Bad, 0)} b.0)(b))(
    /Bad { (b: #{node = StructType} /Bad()) => (#{field = Struct(/Bad, 0)} b.0)(b) })
: #{node = InductType} /Absurd()
"#;

/// A name declares one nominal: a `struct` and an `induct` under one name are refused.
///
/// Every analysis over the declaration set files a declaration under its name, so of two under one name one is never analysed: `struct Bad { f : (Bad) -> Absurd }` beside an empty `induct Bad` read as the family twice lets the record's negative occurrence close Curry's paradox. With the record alone it is the `NotPositive` it should be. An entry is held by its former, so the two are two items under the name, refused as `a_name_declared_by_two_items_is_refused`'s are.
///
/// Mutation-checked against the set of names already declared: with it never consulted the module is judged, `Bad` is refused where the walk writes it again, and the record is analysed with the item that declares it, `NotPositive` beside the name's refusal.
#[test]
fn a_name_under_a_family_and_a_record_is_refused() {
    let alone = text_refusals(&fill!(A_RECORD_NEGATIVE_IN_ITSELF, family = ""));
    assert!(
        refuses(&alone, Some("Bad"), |error| matches!(
            error,
            Error::NotPositive { .. }
        )),
        "the control stopped refusing: the record names itself to the left of an arrow: {alone:?}",
    );

    let both = text_refusals(&fill!(
        A_RECORD_NEGATIVE_IN_ITSELF,
        family = "\ninduct /Bad: pub Type\nend\n"
    ));
    assert!(
        redeclares(&both, &global("Bad")),
        "a name under a family and a record was not refused: {both:?}",
    );
}

/// A driver mounts a unit once. Mounting one twice is a construction bug, a family's unit as much as a definition's.
///
/// Mutation-checked against the assertion: without it the second mount returns.
#[test]
#[should_panic(expected = "a mounted unit redeclared a name already in scope")]
fn a_registry_entry_mounted_twice_is_a_construction_bug() {
    let unit = core_text_module(&fill!(SHADOWING_FAMILY, domain = "#NatType"));
    let judged = already_judged(&unit);
    let walked = walked(&unit);
    let places = Places::of(walked.definitions());
    let mut globals = judged.globals();

    globals.mount(Filed {
        definitions: walked.definitions(),
        places: &places,
        certification: walked.certification(),
    });
}

/// `alias : Type = Nat`, as a unit in scope holds it, with `${decorator}` stating its mark and the sort its value inhabits.
const ALIAS: &str = r#"
${decorator}
let /alias: Type =
    #NatType;
"#;

/// What the walk says of `text` over [`ALIAS`] at `decorator`, mounted beside the record a walk over the undecorated unit files: what `decorator` states of the alias is bytes no walk wrote, mounted as a forger would, so what the mount reads of a mark is seen.
fn over_the_alias(text: &str, decorator: &str) -> Vec<Refusal> {
    let unit = core_text_module(&fill!(ALIAS, decorator));
    let record = walked(&core_text_module(&fill!(ALIAS, decorator = "")))
        .certification()
        .clone();

    text_refusals_over(text, &AlreadyJudged::with(&unit, &record).globals())
}

/// Whether `verdicts` is `symbol`'s refusal as a mismatch, and nothing else.
fn only_mismatched(verdicts: &[Refusal], symbol: &str) -> bool {
    matches!(
        verdicts,
        [Refusal { name: Some(refused), error: Error::Mismatch { .. } }] if *refused == global(symbol)
    )
}

/// `three : alias = 3`.
const THREE: &str = r#"
let /three: /alias =
    3;
"#;

/// A definition's mark is read where its unit is mounted, as it is where a walk brings the definition into scope. `alias : Type = Nat` is no proof, and a walk over its unit refuses the mark; mounted by hand with the mark on, it is a constant, and `three : alias = 3` is refused where the unmarked unit has it accepted. The lie is what makes the reading visible: of a real proof no judgment asks anything its body would answer.
#[test]
fn a_mounted_definition_marked_a_proof_unfolds_to_nothing() {
    assert_eq!(over_the_alias(THREE, ""), Vec::new());

    let verdicts = over_the_alias(THREE, "#{proof = true}");
    assert!(
        only_mismatched(&verdicts, "three"),
        "a mounted definition marked a proof was unfolded: {verdicts:?}",
    );
}

/// `Idx : (i : alias) -> Type 0` with no constructor, since the coercion needs the comparison of two indices and no inhabitant, and `coerce : (x : alias, y : alias, v : Idx(y)) -> Idx(x) = (x, y, v) => v`.
const COERCE: &str = r#"
induct /Idx: (i: /alias) -> pub Type
end

let /coerce: (x: /alias, y: /alias, v: #{node = InductType} /Idx()(y)) ->
    #{node = InductType}
    /Idx()(x) =
    (x: /alias, y: /alias, v: #{node = InductType} /Idx()(y)) => v;
"#;

/// A definition's mark withholds the sort its value inhabits with its body, as `define_as_stated` writes the two together: a reader that took one and not the other would hold a definition in scope otherwise than the walk that judged it.
///
/// `alias : Type = Nat` carries `Prop 0` as the sort its value inhabits, which no walk over its unit states, the walk stating the sort its own reading of the value finds — the forgery that makes the reading visible, since of an honest statement the published sort is the one reduction reaches anyway. Published, it makes `alias` a proposition to the irrelevance gate (`declared_into_prop`), and `coerce` is admitted, two inhabitants of `Nat` being one. Marked a proof, the definition publishes no sort, so `alias` reads at `Type 0` and the coercion is the `Mismatch` it should be — as it is where the unit states no sort at all.
///
/// Mutation-checked: with `held` publishing a marked definition's `value_sort` all the same, the marked arm is admitted and this test fails.
#[test]
fn a_mounted_definition_marked_a_proof_publishes_no_value_sort() {
    assert_eq!(over_the_alias(COERCE, "#{value_sort = Prop}"), Vec::new());

    for withheld in ["#{proof = true, value_sort = Prop}", ""] {
        let verdicts = over_the_alias(COERCE, withheld);
        assert!(
            only_mismatched(&verdicts, "coerce"),
            "a mounted definition published a value sort no reader may have: {verdicts:?}",
        );
    }
}

/// The reflection every intrinsic bound is stated in, declared again under the name the registry knows it by and true of every decision, and the reflection again under a name of the module's own, `/G`, by which every byte is below 256 with nothing written: were the name the module's to state, a byte narrowed from 300 would carry its bound by `qed`, and `/G` at it is `/G(300 < 256)`, which is `False`.
const HOLDS_RESTATED_TO_GAUGE_A_BYTE: &str = r#"
let /std/Bool/Holds : (b: #BoolType) -> Prop =
    (b: #BoolType) => /std/Bool/True;
let /G : (b2: #BoolType) -> Prop =
    (b3: #BoolType) =>
        #{goal = Prop}
        match b3
        | false => /std/Bool/False
        | true => /std/Bool/True
        end;
((
    below: (b4: #ByteType) -> /G(#NatLt(#ByteToNat(b4), 256))
) =>
    below(
        #NatToByte(
            300,
            #{node = Variant}
            /std/Bool/True/qed()
        )
    ))(
    (b5: #ByteType) =>
        #{node = Variant}
        /std/Bool/True/qed()
)
"#;

/// A module cannot make a name the certifier's registry knows stand for something else: every bound `Intrinsic::signature` states is built from the registry's `Holds`, so the one declared over an environment holding it is refused for the name before anything is judged, the closing term among it.
///
/// Mutation-checked against the environment's question in `redeclarations`: with a held name never asked after, the walk goes on, the module's `Holds` is refused where the walk writes it and the closing term beside it, the bound being the environment's. Put with nothing in scope, `HOLDS_RESTATED_TO_GAUGE_A_BYTE` is admitted, its closing term a proof of `False`: the bound's whole force is what the registry's name means.
#[test]
fn the_registrys_reflection_is_not_declared_again() {
    let environment = core_text_module(PROPOSITIONS);
    let verdicts = core_text_refusals_over(
        HOLDS_RESTATED_TO_GAUGE_A_BYTE,
        &already_judged(&environment).globals(),
    );
    let holds = Global::Authored(SYNTAX.proof.holds.qualifier());

    assert!(redeclares(&verdicts, &holds), "{verdicts:?}");
}
