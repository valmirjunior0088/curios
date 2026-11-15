//! Variance of a family's universe levels: which instances the walk compares at a level and which it treats as one, what the walk files of each level, and a carried vector held to what the walk would file.

use {
    super::test_support::*, crate::*, curios_analysis::test_support::*, curios_common::fill,
    curios_core::Global,
};

/// `, variances = [...]` where a declaration carries `variances`, and nothing where it carries none.
fn carried(variances: &[&str]) -> String {
    match variances {
        [] => String::new(),
        _ => format!(", variances = [{}]", variances.join(", ")),
    }
}

/// `Wrap.{u}(A : Type u) : Type u | wrap(a : A)`, carrying `variances`: the level types the parameter and nothing the family holds.
fn wrap(variances: &[&str]) -> String {
    let carried = carried(variances);

    fill!(
        "#{universe_context = {parameter_count = 1}${carried}}\ninduct /Wrap(A: #{level = u} Type): pub #{level = u} Type\n| wrap(a: A)\nend\n",
        carried
    )
}

/// `Box.{u} : Type (u + 1) | box(T : ${payload})`, carrying `variances`: the level is held by what the payload holds.
fn boxed(variances: &[&str], payload: &str) -> String {
    let carried = carried(variances);

    fill!(
        "#{universe_context = {parameter_count = 1}${carried}}\ninduct /Box: pub #{level = u + 1} Type\n| box(T: ${payload})\nend\n",
        carried,
        payload
    )
}

/// [`boxed`] at `Type u`, the payload that holds its level outright.
fn holding(variances: &[&str]) -> String {
    boxed(variances, "#{level = u} Type")
}

/// `coerce : (x : family.{from}${arguments}) -> family.{to}${arguments} = (x) => x`, an occurrence of `family`, a node of `node`, read back at another instance.
fn coercion(node: &str, family: &str, arguments: &str, from: &str, to: &str) -> String {
    fill!(
        r#"
let /coerce: (x: #{node = ${node}, universes = {levels = [${from}]}} /${family}${arguments}) ->
    #{node = ${node}, universes = {levels = [${to}]}}
    /${family}${arguments} =
    (x: #{node = ${node}, universes = {levels = [${from}]}} /${family}${arguments}) => x;
"#,
        node,
        family,
        arguments,
        from,
        to
    )
}

/// Russell's carrier, `Fam.{u, v | u + 1 ≤ v} : Type v | mk(A : Type u, f : (A) -> Fam.{u, v})`, carrying `variances`.
fn russell(variances: &[&str]) -> String {
    let carried = carried(variances);

    fill!(
        r#"
#{universe_context = {parameter_count = 2, constraints = [u + 1 <= v]}${carried}}
induct /Fam: pub #{level = v} Type
| mk(A: #{level = u} Type, f: (x: A) -> #{node = InductType, universes = {levels = [u, v]}} /Fam())
end
"#,
        carried
    )
}

fn refusals(text: &str) -> Vec<Verdict> {
    text_verdicts(text)
}

/// Whether the identity put at two instances was itself refused: the walk's own verdict on the pair, apart from anything the reconciliation says of the declaration.
fn coercion_refused(verdicts: &[Verdict]) -> bool {
    verdicts.iter().any(|verdict| {
        verdict.name == Some(global("coerce")) && matches!(verdict.error, Error::Mismatch { .. })
    })
}

fn denied(verdicts: &[Verdict], family: Global, level: usize) -> bool {
    verdicts.iter().any(|verdict| {
        verdict.error
            == Error::VarianceDenied {
                name: family,
                level,
            }
    })
}

// The level types `Wrap`'s parameter and nothing it holds, so the two instances are one type.
#[test]
fn two_instances_apart_in_an_irrelevant_level_convert() {
    let module = wrap(&["Irrelevant"]) + &coercion("InductType", "Wrap", "(#NatType)", "0", "1");

    assert_eq!(refusals(&module), Vec::new());
}

// `Box` holds a `Type u`, carried honestly and carried with no vector at all.
#[test]
fn an_invariant_level_is_compared() {
    for variances in [&["Invariant"][..], &[]] {
        let module = holding(variances) + &coercion("InductType", "Box", "()", "1", "0");

        assert!(coercion_refused(&refusals(&module)));
    }
}

// The lie that would read `box(Type 0) : Box.{1}` back at `Box.{0}`: the walk compares two instances by the vector it computed, so the coercion is refused, and the entry carrying the lie is denied where it is declared, since the elaborator of a later unit reads what an entry carries. An entry past the declaration's count is denied the same way.
#[test]
fn a_carried_irrelevance_the_walk_denies_is_refused_and_read_by_no_comparison() {
    let family = global("Box");
    let believed =
        refusals(&(holding(&["Irrelevant"]) + &coercion("InductType", "Box", "()", "1", "0")));
    assert!(coercion_refused(&believed));
    assert!(denied(&believed, family, 0));

    let overlong = refusals(
        &(holding(&["Invariant", "Irrelevant"]) + &coercion("InductType", "Box", "()", "1", "1")),
    );
    assert!(denied(&overlong, family, 1));
}

// `Wrap` carried as invariant in a level its declaration never mentions, which is what a family read before its payload was known carries: the walk compares two instances by the vector it computed, in which the level is irrelevant, so the coercion between them stands, and a carried vector that claims less is denied nothing.
#[test]
fn a_carried_invariance_the_walk_would_grant_is_read_by_no_comparison() {
    let carried = |to: &str| {
        refusals(&(wrap(&["Invariant"]) + &coercion("InductType", "Wrap", "(#NatType)", "0", to)))
    };

    assert_eq!(carried("0"), Vec::new());
    assert_eq!(carried("1"), Vec::new());
}

// The certifier sets no level of a definition apart: `Families` is implemented in `curios-elab` alone and `convert`'s level comparison passes `&()`, so `held.{1}` read back at `held.{0}` is refused. What the elaborator concluded of a definition's vector is its own (`curios_elab::Vectors`), which Core holds nothing of, so a later unit's elaborator is wrong by it alone, and this is the second opinion that catches it.
#[test]
fn a_definitions_instances_are_compared_at_every_level() {
    let refused = refusals(
        r#"
#{universe_context = {parameter_count = 1}}
let /held: #{level = u + 1} Type =
    #{level = u} Type;

let /coerce: (x: #{universes = {levels = [1]}} /held) -> #{universes = {levels = [0]}} /held =
    (x: #{universes = {levels = [1]}} /held) => x;
"#,
    );

    assert!(
        coercion_refused(&refused),
        "the higher instance is no type at the lower instance's sort: {refused:?}",
    );
}

// `Box` holds its level only as the element of an intrinsic carrier: a list, a cell, a channel or a description of `Type u`. A carrier is compared by its element, so two instances apart in the level hold carriers of different universes, and granted irrelevant, `box([Type 0]) : Box.{1}` read back at `Box.{0}` would list `Type 0` among the types of `Type 0`. Carried honestly the coercion is refused, and nothing else is; carried as irrelevant the declaration is denied.
//
// Mutation-checked at `curios-analysis`'s variance walk: reading no carrier's element admits the honest coercion.
#[test]
fn a_level_only_a_carriers_element_holds_is_invariant() {
    let family = global("Box");
    for former in ["#ListType", "#CellType", "#ChannelType", "#IoType"] {
        let payload = fill!("${former}(#{level = u} Type)", former);
        let coerced = coercion("InductType", "Box", "()", "1", "0");

        let honest = refusals(&(boxed(&["Invariant"], &payload) + &coerced));
        assert!(only_the_coercion_refused(&honest), "{honest:?}");

        let lying = refusals(&(boxed(&["Irrelevant"], &payload) + &coerced));
        assert!(denied(&lying, family, 0), "{lying:?}");
    }
}

// `Fam`'s first level reaches no payload and no index type, and its constructor's target puts it at `Box`'s invariant position: two instances apart in it have different inhabitants at one index. Carried honestly the coercion is refused; carried as irrelevant the declaration is.
#[test]
fn a_level_only_an_index_target_mentions_is_invariant() {
    let module = |variances: &[&str]| {
        let carried = carried(variances);
        holding(&["Invariant"])
            + &fill!(
                r#"
#{universe_context = {parameter_count = 2, constraints = [u + 1 <= v]}${carried}}
induct /Fam: (D: #{level = v} Type) -> pub Type
| mk(): (#{node = InductType, universes = {levels = [u]}} /Box())
end
"#,
                carried
            )
            + &coercion(
                "InductType",
                "Fam",
                "()(#{node = InductType, universes = {levels = [0]}} /Box())",
                "0, 2",
                "1, 2",
            )
    };

    assert!(coercion_refused(&refusals(&module(&[
        "Invariant",
        "Invariant"
    ]))));
    assert!(denied(
        &refusals(&module(&["Irrelevant", "Invariant"])),
        global("Fam"),
        0
    ));
}

// A concept-shaped struct: the level its method binds at is held by a field, and the level of its carrier's codomain by the parameter's type alone.
#[test]
fn a_concepts_method_level_is_compared() {
    let module = |variances: &[&str], from: &str, to: &str, carrier: &str| {
        let carried = carried(variances);
        fill!(
            r#"
#{universe_context = {parameter_count = 2}${carried}}
struct /Carrier(M: (T: #{level = u} Type) -> #{level = v} Type): pub #{level = max(u + 1, v)} Type {
    pure: (A: #{level = u} Type, x: A) -> M(A),
}

let /coerce: (
    x: #{node = StructType, universes = {levels = [${from}]}} /Carrier((T: Type) => #NatType)) ->
    #{node = StructType, universes = {levels = [${to}]}}
    /Carrier(${carrier}) =
    (x: #{node = StructType, universes = {levels = [${from}]}} /Carrier((T: Type) => #NatType)) => x;
"#,
            carried,
            from,
            to,
            carrier
        )
    };
    let honest = ["Invariant", "Irrelevant"];
    let (low, high) = (
        "(T: Type) => #NatType",
        "(T2: #{level = 1} Type) => #NatType",
    );

    assert_eq!(refusals(&module(&honest, "0, 0", "0, 1", low)), Vec::new());
    assert!(coercion_refused(&refusals(&module(
        &honest, "0, 1", "1, 1", high
    ))));
    assert!(denied(
        &refusals(&module(&["Irrelevant", "Irrelevant"], "0, 1", "1, 1", high)),
        global("Carrier"),
        0
    ));
}

// `Held` reaches a family a unit in scope declares, which answers from the vector the certifier filed for it: its unit's record's, or the one this walk computes before it judges anything where the record files none, as here. So an irrelevance `Held` carries is granted over a scope's irrelevant `Wrap` and denied over its invariant `Box`, and over a scope whose `Box` carries a lie it is denied as well, what an entry carries being read by no judgment.
#[test]
fn a_family_of_a_unit_in_scope_answers_from_what_the_certifier_filed() {
    let held = |result_sort: &str, payload: &str| {
        fill!(
            r#"
#{universe_context = {parameter_count = 1}, variances = [Irrelevant]}
induct /Held: pub ${result_sort}
| c(x: ${payload})
end
"#,
            result_sort,
            payload
        )
    };
    let judged = |text: &str, scope: &str| {
        let scope = already_judged(&core_text_module(scope));
        fixture_verdicts(&core_text_module(text), BUDGET, &scope.globals(), SYNTAX)
    };
    let over_wrap = held(
        "#{level = u} Type",
        "#{node = InductType, universes = {levels = [u]}} /Wrap(#NatType)",
    );
    let over_box = held(
        "#{level = u + 1} Type",
        "#{node = InductType, universes = {levels = [u]}} /Box()",
    );

    assert_eq!(judged(&over_wrap, &wrap(&["Irrelevant"])), Vec::new());
    assert!(denied(
        &judged(&over_box, &holding(&["Invariant"])),
        global("Held"),
        0
    ));
    assert!(denied(
        &judged(&over_box, &holding(&["Irrelevant"])),
        global("Held"),
        0
    ));
}

// `Uses` mentions its level only on `Alias.{u}(A)`, which unfolds to `A`: the recomputation reads the reduct and grants the irrelevance the entry carries.
#[test]
fn a_level_a_reduction_removes_is_carried_as_irrelevant() {
    let module = r#"
#{universe_context = {parameter_count = 1}}
let /Alias: (A: #{level = u} Type) -> #{level = u} Type =
    (A: #{level = u} Type) => A;

#{universe_context = {parameter_count = 1}, variances = [Irrelevant]}
induct /Uses(A: #{level = u} Type): pub #{level = u} Type
| c(x: (#{universes = {levels = [u]}} /Alias)(A))
end
"#
    .to_string()
        + &coercion("InductType", "Uses", "(#NatType)", "0", "1");

    assert_eq!(refusals(&module), Vec::new());
}

/// Whether every refusal is the coercion's: the declarations the fixture forges were accepted, so a refused coercion is the walk's verdict on the instances and not the echo of a family it refused.
fn only_the_coercion_refused(verdicts: &[Verdict]) -> bool {
    coercion_refused(verdicts)
        && verdicts
            .iter()
            .all(|verdict| verdict.name == Some(global("coerce")))
}

// `Arm`'s first level is stated by one arm of a match stuck on its parameter and by no other part the walk reads: a stuck form is read through its children, so the arm's `Type u` holds the level, and two instances apart in it are two types. Granted irrelevant, `c(Type 0): Arm.{1, 2}(true)` read at `Arm.{0, 2}(true)` would hold `Type 0` at `Type 0`. Carried honestly the coercion is refused, and nothing else is; carried as irrelevant the declaration is denied.
#[test]
fn a_level_one_arm_of_a_stuck_match_states_is_invariant() {
    let module = |variances: &[&str]| {
        let carried = carried(variances);
        fill!(
            r#"
#{universe_context = {parameter_count = 2, constraints = [u + 1 <= v]}${carried}}
induct /Arm(b: #BoolType): pub #{level = v} Type
| c(T: #{goal = #{level = v} Type} match b | false => #NatType | true => #{level = u} Type end)
end
"#,
            carried
        ) + &coercion("InductType", "Arm", "(true)", "0, 2", "1, 2")
    };

    assert!(only_the_coercion_refused(&refusals(&module(&[
        "Invariant",
        "Invariant"
    ]))));
    assert!(denied(
        &refusals(&module(&["Irrelevant", "Invariant"])),
        global("Arm"),
        0
    ));
}

// `Held` reaches `Box` only through `Pick`, a definition whose value is `Box.{l}` and names its second level nowhere: the walk reads the payload's reduct, so the first level stands at `Box`'s invariant position and the unfolding drops the second. Two instances apart in the first are refused at the coercion and nowhere else, two apart in the second convert, and carried as irrelevant in the first the declaration is denied.
#[test]
fn a_level_reached_through_a_definition_the_walk_unfolds_is_invariant() {
    let module = |variances: &[&str], from: &str, to: &str| {
        let carried = carried(variances);
        holding(&["Invariant"])
            + &fill!(
                r#"
#{universe_context = {parameter_count = 2}}
let /Pick: #{level = u + 1} Type =
    #{node = InductType, universes = {levels = [u]}} /Box();

#{universe_context = {parameter_count = 2}${carried}}
induct /Held: pub #{level = u + 1} Type
| c(x: #{universes = {levels = [u, v]}} /Pick)
end
"#,
                carried
            )
            + &coercion("InductType", "Held", "()", from, to)
    };
    let honest = ["Invariant", "Irrelevant"];

    assert!(only_the_coercion_refused(&refusals(&module(
        &honest, "0, 0", "1, 0"
    ))));
    assert_eq!(refusals(&module(&honest, "0, 0", "0, 1")), Vec::new());
    assert!(denied(
        &refusals(&module(&["Irrelevant", "Irrelevant"], "0, 0", "1, 0")),
        global("Held"),
        0
    ));
}

/// Whether `name` was refused for a conversion that did not hold.
fn mismatched(verdicts: &[Verdict], name: Global) -> bool {
    verdicts.iter().any(|verdict| {
        verdict.name == Some(name) && matches!(verdict.error, Error::Mismatch { .. })
    })
}

// Russell's carrier, `Fam.{i, o | i + 1 ≤ o} : Type o | mk(A : Type i, f : (A) -> Fam.{i, o})`, reaches its second level in its result sort and at its own position in the forward field's codomain, and the fixpoint grants that level irrelevant: the two instances' telescopes convert entry by entry, so `Fam.{0, 1}` and `Fam.{0, 2}` are one type and the identity across them stands. Carried as irrelevant in the payload's level as well the declaration is denied, which is the level the payload's `Type` holds.
#[test]
fn the_level_a_carrier_stands_at_is_irrelevant_where_its_payloads_level_is_not() {
    let honest = ["Invariant", "Irrelevant"];

    assert_eq!(
        refusals(&(russell(&honest) + &coercion("InductType", "Fam", "()", "0, 1", "0, 2"))),
        Vec::new()
    );
    assert_eq!(
        refusals(&(russell(&honest) + &coercion("InductType", "Fam", "()", "0, 2", "0, 1"))),
        Vec::new()
    );
    let lying = refusals(
        &(russell(&["Irrelevant", "Irrelevant"])
            + &coercion("InductType", "Fam", "()", "0, 1", "0, 2")),
    );
    assert!(denied(&lying, global("Fam"), 0));
}

// Russell's self-application over that carrier: `Fam.{0, 1}/mk(Fam.{0, 1}, (x) => x)` hands the family's own type, a `Type 1`, to the payload the declaration stands at `Type 0`. The irrelevance makes `Fam.{0, 1}` one type with `Fam.{0, o}` for every `o`, so the paradox needs no instance spelled at the payload's sort; what refuses it is the sort the declaration states at the instance written, which conversion comparing nothing at the level does not move, and the declaration itself stands.
#[test]
fn russells_self_application_is_refused_at_the_sort_its_instance_states() {
    let verdicts = refusals(
        &(russell(&["Invariant", "Irrelevant"])
            + r#"
let /russell: #{node = InductType, universes = {levels = [0, 1]}} /Fam() =
    #{node = Variant, universes = {levels = [0, 1]}}
    /Fam/mk(
        #{node = InductType, universes = {levels = [0, 1]}} /Fam(),
        (x: #{node = InductType, universes = {levels = [0, 1]}} /Fam()) => x);
"#),
    );

    assert!(mismatched(&verdicts, global("russell")), "{verdicts:?}");
    assert!(!denied(&verdicts, global("Fam"), 1), "{verdicts:?}");
}

// The two instances convert, and their sorts do not: `Fam.{0, 2}` is no inhabitant of `Type 1`, though `Fam.{0, 1}` is and the two are one type. A level conversion reads at nothing is still read where the declaration states the sort, so the irrelevance transports terms between the two types and never the types between the two sorts.
#[test]
fn an_irrelevant_levels_higher_instance_is_no_type_at_the_lower_instances_sort() {
    let verdicts = refusals(
        &(russell(&["Invariant", "Irrelevant"])
            + r#"
let /lowered: #{level = 1} Type =
    #{node = InductType, universes = {levels = [0, 2]}} /Fam();
"#),
    );

    assert!(mismatched(&verdicts, global("lowered")), "{verdicts:?}");
}

/// `Held@{u}(n)`'s payload is a group's member applied to the family's parameter, a recursion stuck on `n` whose base case is `Type u`, so the level reaches the payload only through a group no reduction opens; `Lied` is the same family carrying that level as irrelevant; `Dropped@{u}(n)`'s group states `u` in its member's type and its body is `#NatType`. `lowered` claims a `Held` at level 1 one at level 0, and `moved` a `Dropped`.
const LEVELS_A_GROUP_HOLDS: &str = r#"
#{universe_context = {parameter_count = 1}}
induct /Held(n: #NatType): pub #{level = u + 2} Type
| held(
    x: (#{node = Rec}
        (let m: (k: #NatType) -> #{level = u + 1} Type =
            (k: #NatType) =>
                #{goal = #{level = u + 1} Type}
                match k
                | 0 =>
                    #{level = u}
                    Type
                | j + 1 =>
                    m(j)
                end;
        m))(n))
end
#{universe_context = {parameter_count = 1}, variances = [Irrelevant]}
induct /Lied(n: #NatType): pub #{level = u + 2} Type
| lied(
    x: (#{node = Rec}
        (let m: (k: #NatType) -> #{level = u + 1} Type =
            (k: #NatType) =>
                #{goal = #{level = u + 1} Type}
                match k
                | 0 =>
                    #{level = u}
                    Type
                | j + 1 =>
                    m(j)
                end;
        m))(n))
end
#{universe_context = {parameter_count = 1}}
induct /Dropped(n: #NatType): pub #{level = u + 2} Type
| dropped(
    x: (#{node = Rec}
        (let m: (k: #NatType) -> #{level = u + 1} Type =
            (k: #NatType) =>
                #NatType;
        m))(n))
end
let /lowered : (n: #NatType, h: #{node = InductType, universes = {levels = [1]}} /Held(n)) -> #{node = InductType, universes = {levels = [0]}} /Held(n) =
    (n: #NatType, h: #{node = InductType, universes = {levels = [1]}} /Held(n)) =>
        h;
let /moved : (n: #NatType, h: #{node = InductType, universes = {levels = [1]}} /Dropped(n)) -> #{node = InductType, universes = {levels = [0]}} /Dropped(n) =
    (n: #NatType, h: #{node = InductType, universes = {levels = [1]}} /Dropped(n)) =>
        h;
/std/Bool/False
"#;

// A level a group in a payload holds is compared, and one a reduction of the group removes is not: granted irrelevant, `held(0, Type 0) : Held@{1}(0)` read at `Held@{0}(0)` would hold `Type 0` at `Type 0`. So `lowered` is refused, `Lied` is denied the irrelevance it carries, and `moved` stands.
#[test]
fn a_level_a_group_in_a_payload_holds_is_invariant() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${LEVELS_A_GROUP_HOLDS}",
        PROPOSITIONS,
        LEVELS_A_GROUP_HOLDS
    ));

    assert!(mismatched(&verdicts, global("lowered")), "{verdicts:?}");
    assert!(denied(&verdicts, global("Lied"), 0), "{verdicts:?}");
    for symbol in ["Held", "Dropped", "moved"] {
        assert!(
            verdicts
                .iter()
                .all(|verdict| verdict.name != Some(global(symbol))),
            "{symbol}: {verdicts:?}"
        );
    }
}
