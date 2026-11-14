//! Which of a baseline's verdicts a walk adopts, and which declarations it judges again.

use {
    super::test_support::*,
    crate::*,
    curios_analysis::test_support::*,
    curios_common::fill,
    curios_core::{
        Applications, Certification, Certified, Definitions, Func, Item, Places, Reads, Subterm,
        Totality, Variance,
    },
};

/// The fixture the trusted base is held by: a dependency's body changes, its dependent is presented as it was, and the walk judges the dependent again because its own record says it unfolded that body. `three : alias = 3` stood while `alias` was `Nat` and is refused once `alias` is `Bool`, whatever the caller took to be unchanged.
#[test]
fn a_declaration_presented_unchanged_is_judged_again_where_what_it_read_has_changed() {
    let baseline = written();
    let certification = walked(&baseline).certification;
    let edited = reading_definitions("#BoolType", "x");

    let rechecked = walked_over(&edited, &baseline, &certification);

    assert_eq!(
        rechecked
            .verdicts
            .iter()
            .map(|verdict| verdict.name)
            .collect::<Vec<_>>(),
        vec![Some(global("three"))],
    );
}

/// Over a baseline the walked module is a version of, the walk compares the two by what they do not share, judges the item that changed and asks what read it, and files the record a walk over every declaration of the module files: `identity` returns `7`.
#[test]
fn a_walk_over_a_version_files_what_a_walk_over_every_declaration_files() {
    let baseline = versioned(written());
    let certification = walked(&baseline).certification;
    let identity = declared_item(
        "let /identity: (x: #NatType) -> #NatType = (x: #NatType) => 7;",
        "identity",
    );

    let rechecked = walked_over(
        &replaced(&baseline, "identity", identity),
        &baseline,
        &certification,
    );

    assert_eq!(rechecked.verdicts, Vec::new());
    assert_eq!(
        rechecked.certification,
        walked(&reading_definitions("#NatType", "7")).certification
    );
}

/// A declaration a recompile writes under a name a unit in scope declares is refused for the name over a baseline, as over every declaration, though the unit in scope is the one the baseline's walk read: `three` respelled as the environment's `shadowed`, the baseline's other declarations shared.
///
/// Mutation-checked: with a changed item never asked whether a unit in scope declares its name, the module is judged and `shadowed` refused where the walk writes it, twice.
#[test]
fn a_name_a_unit_in_scope_declares_written_over_a_baseline_is_refused_for_it() {
    let environment = already_judged(&judged_environment());
    let globals = environment.globals();
    let baseline = versioned(written());
    let certification = fixture_certified(&baseline, BUDGET, &globals, SYNTAX).certification;
    let edited = replaced(
        &baseline,
        "three",
        declared_item("let /shadowed: #NatType = 3;", "shadowed"),
    );
    let (places, now) = (Places::of(&baseline), Places::of(&edited));

    let rechecked = super::verdicts_from(
        &mut Certifier::new(BUDGET, SYNTAX),
        &edited,
        None,
        &globals,
        Some(super::Over {
            baseline: Baseline {
                definitions: &baseline,
                places: &places,
                certification: &certification,
            },
            places: &now,
            candidates: None,
        }),
    );

    assert!(
        matches!(
            rechecked.verdicts.as_slice(),
            [Verdict { name: Some(name), error: Error::Redeclared(_) }] if *name == global("shadowed")
        ),
        "a name the environment declares was written over a baseline: {:?}",
        rechecked.verdicts
    );
}

/// `A.{u, v | u + ${offset} <= v} : Type v = Type u`: its body inhabits `Type (u + 1)`, so it fits its declared type only where its declaration assumes `u + 1 <= v`.
const SCHEME: &str = r#"
#{universe_context = {parameter_count = 2, constraints = [u + ${offset} <= v]}}
let /A: #{level = v} Type =
    #{level = u} Type;
"#;

/// `user.{u, v | u + 1 <= v} : Type v = A.{u, v}`, whose instance discharges `u + 1 <= v`.
const USER: &str = r#"
#{universe_context = {parameter_count = 2, constraints = [u + 1 <= v]}}
let /user: #{level = v} Type =
    #{universes = {levels = [u, v]}} /A;
"#;

/// A scheme's constraint strengthened under the same type and body reaches the declaration that instantiated it, as a walk over every declaration does: `A<u, v | u + 2 <= v>` in place of `u + 1 <= v`, beside `user<u, v | u + 1 <= v> : Type v = A<u, v>`, whose instance discharged the constraint `A` stated and not the one it states now.
///
/// Mutation-checked: with a signature read digested without its scheme's constraints, `user` is adopted and the walk refuses nothing.
#[test]
fn a_schemes_constraint_strengthened_reaches_the_declaration_that_instantiated_it() {
    let scheme = |offset: usize| fill!(SCHEME, offset);
    let baseline = versioned(core_text_module(&fill!(
        "${scheme}${USER}",
        scheme = scheme(1),
        USER
    )));
    let certification = walked(&baseline).certification;
    let edited = replaced(&baseline, "A", declared_item(&scheme(2), "A"));

    let whole = walked_whole(&edited);
    let over = walked_over(&edited, &baseline, &certification);

    assert!(
        whole
            .verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("user"))
                && matches!(verdict.error, Error::UniverseInstance { .. })),
        "the control stopped refusing: `user`'s instance does not discharge `u + 2 <= v`: {:?}",
        whole.verdicts
    );
    assert_eq!(over.verdicts, whole.verdicts);
}

/// A declaration the walk does not visit is read once the walk has passed its place: `alias` respelled to name `five`, stored after it, finds it unbound, as a walk over every declaration does, though nothing `five` read moved and the walk adopts `five` without visiting it.
///
/// Mutation-checked: with a declaration the walk does not visit read wherever it stands, `alias` finds `five` and is refused for its type instead.
#[test]
fn a_declaration_the_walk_does_not_visit_is_unbound_before_its_place() {
    let baseline = versioned(written());
    let certification = walked(&baseline).certification;
    let edited = replaced(
        &baseline,
        "alias",
        declared_item("let /alias: Type = /five;", "alias"),
    );

    let whole = walked_whole(&edited);
    let over = walked_over(&edited, &baseline, &certification);

    assert!(
        whole
            .verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::Undeclared(_))),
        "a walk over every declaration finds `five` undeclared at `alias`: {:?}",
        whole.verdicts
    );
    assert_eq!(over.verdicts, whole.verdicts);
}

/// A module that is the baseline's is adopted whole: every entry comes back as the baseline's record holds it, the mark with it.
#[test]
fn a_declaration_whose_terms_and_reads_stand_takes_the_baselines_verdict() {
    let baseline = written();
    let certification = marked();

    let rechecked = walked_over(&baseline, &baseline, &certification);

    assert_eq!(rechecked.verdicts, Vec::new());
    assert_eq!(rechecked.certification, certification);
}

/// `base : (Nat) -> Nat`, total as `(n) => n` or partial as a group that calls itself without descending, then `middle : Nat = base(1)`, `top : Nat = middle` and `apart : Nat = 7`: `top` reads `middle` and nothing of `base`.
const CHAINED: &str = r#"
let /base: (n: #NatType) -> #NatType =
    ${base};

let /middle: #NatType =
    /base(1);

let /top: #NatType =
    /middle;

let /apart: #NatType =
    7;
"#;

/// [`CHAINED`]'s module, `base` partial where `partial`.
fn chained_definitions(partial: bool) -> Definitions {
    let base = match partial {
        true => "#{node = Rec} (let x: (n: #NatType) -> #NatType = (n2: #NatType) => x(n2); x)",
        false => "(n: #NatType) => n",
    };

    core_text_module(&fill!(CHAINED, base))
}

/// A verdict that moves reaches what read it through a declaration whose terms stand: `base` made partial is judged, `middle`, which read its signature, is judged and comes out partial, and so does `top`, which read `middle` alone and whose terms are the baseline's. `apart` reads nothing that moved and keeps the baseline's verdict.
///
/// Mutation-checked: with the declarations that read a moved one marked and theirs not, `top` keeps the baseline's `Total`.
#[test]
fn a_verdict_that_moves_reaches_what_read_it_through_a_declaration_whose_terms_stand() {
    let baseline = chained_definitions(false);
    let certification = walked(&baseline).certification;
    let edited = chained_definitions(true);

    let rechecked = walked_over(&edited, &baseline, &certification);

    assert_eq!(rechecked.verdicts, Vec::new());
    for name in ["base", "middle", "top"] {
        assert_eq!(
            rechecked.certification.totality(&global(name)),
            Some(Totality::Partial),
            "{name}",
        );
    }
    assert_eq!(
        rechecked.certification.totality(&global("apart")),
        Some(Totality::Total),
    );
}

/// An adopted verdict is the baseline's, partiality included: a record that calls `five` partial files it partial, and refuses nothing for it.
#[test]
fn an_adopted_verdict_is_the_baselines_partiality_included() {
    let baseline = written();
    let certification = doctored(&walked(&baseline).certification, "five", |certified| {
        Certified {
            totality: Totality::Partial,
            ..certified
        }
    });

    let rechecked = walked_over(&baseline, &baseline, &certification);

    assert_eq!(rechecked.verdicts, Vec::new());
    assert_eq!(
        rechecked.certification.totality(&global("five")),
        Some(Totality::Partial),
    );
}

/// `induct Shadowed : Type end` and `struct Kept { held : ${held} }`, each under `${family}`'s and `${record}`'s decorators and with its representation `${public}`: the two registry entries an environment holds, and what elaboration stamps on each.
const TWO_ENTRIES: &str = r#"
${family}induct /Shadowed: ${public}Type
end

${record}struct /Kept: ${public}Type {
    held: ${held},
}
"#;

/// What [`TWO_ENTRIES`] varies of the two entries it declares.
enum Varied {
    /// Nothing: the entries as an environment holds them.
    Nothing,
    /// Elaboration's stamps on both: a polarity vector, the owning module and the representation's privacy.
    Stamps,
    /// The family's variance vector.
    Variance,
    /// The type of the record's field.
    Field,
}

/// [`TWO_ENTRIES`]'s module, differing from the unvaried pair in what `varied` names.
fn two_registry_entries(varied: Varied) -> Definitions {
    let stamps = "#{module = /elsewhere, polarities = [Mixed]}\n";
    let (family, record, public) = match varied {
        Varied::Stamps => (stamps, stamps, ""),
        Varied::Variance => ("#{variances = [Irrelevant]}\n", "", "pub "),
        Varied::Nothing | Varied::Field => ("", "", "pub "),
    };
    let held = match varied {
        Varied::Field => "#BoolType",
        _ => "#NatType",
    };

    core_text_module(&fill!(TWO_ENTRIES, family, record, public, held))
}

/// A registry entry is the baseline's by what the certifier reads of it: its terms, and the variance vector it carries, which `accept` holds to the vector the walk computes. Elaboration's other stamps take no part — the polarity vector, in whose place a walk holds the one the certifier computed, the owning module and the representation's privacy — so an entry differing from the baseline's in those alone is the baseline's, and one differing in its variance vector or in a field's type is not.
///
/// Mutation-checked against each comparison: taken over the whole entry, the stamped pair is another declaration; with the variance vector left out, the family carrying another is the baseline's; and with the arity left out the record is the baseline's.
#[test]
fn a_registry_entry_is_the_baselines_by_what_the_certifier_reads_of_it() {
    let held = two_registry_entries(Varied::Nothing);
    let family =
        |definitions: &Definitions| (*definitions.induct_decls()[&global("Shadowed")]).clone();
    let record = |definitions: &Definitions| (*definitions.struct_decls()[&global("Kept")]).clone();

    let stamped = two_registry_entries(Varied::Stamps);
    assert!(
        induct_alike(&family(&stamped), &family(&held))
            && struct_alike(&record(&stamped), &record(&held)),
        "an entry differing from the baseline's in elaboration's stamps alone is another declaration",
    );
    assert!(
        !induct_alike(
            &family(&two_registry_entries(Varied::Variance)),
            &family(&held)
        ),
        "a family differing from the baseline's in the variance vector it carries alone is the baseline's",
    );
    assert!(
        !struct_alike(
            &record(&two_registry_entries(Varied::Field)),
            &record(&held)
        ),
        "a record differing from the baseline's in a field's type is the baseline's",
    );
}

/// `Absurd`, a proposition with no case, and `loop : Absurd = loop`, a proof that calls itself without descending: the unit a forged record files.
const LOOPING: &str = r#"
induct /Absurd: pub Prop
end

#{proof = true}
let /loop: #{node = InductType} /Absurd() =
    /loop;
"#;

/// A forged `Certification` filing a genuinely partial proof as `Total`, mounted as a unit in scope, admits a looping proof of an empty proposition: the mounted record is believed without the walk that would refuse it.
///
/// `loop : Absurd = loop` recurses without descending, so a walk refuses it as a proof that may not terminate ([`totality-of-the-erased-program`]). Filed `Total` and mounted, nothing re-judges it: `Globals::mount` stands a scope entry at its type with its body, unjudged, and the reader's totality obligation reads the forged verdict (`Globals::partial`). The control is the same record filing it `Partial`, which refuses.
///
/// The input is forged — no walk of this crate produces this record — so this names what the boundary's ground, that a scope entry was judged by the walk that built it, does not itself check: the mounted `Certification`'s totality is believed, and nothing in the record (no digest, no checksum) is consulted to tell a forged verdict from one a walk filed. A stored unit's slot is guarded by the compiler digest and the verified read-set, which bind the record to this compiler over these inputs, not to a walk having concluded it.
#[test]
fn a_forged_total_verdict_mounted_in_scope_admits_a_looping_proof() {
    let mounted = core_text_module(LOOPING);
    let reader = "/loop\n: #{node = InductType} /Absurd()";
    let certified = |totality: Totality| {
        Certification::of(
            mounted
                .items
                .iter()
                .flat_map(Item::declared_names)
                .map(|name| {
                    (
                        name,
                        Certified {
                            totality,
                            applications: Applications::default(),
                            reads: Reads::default(),
                        },
                    )
                }),
            [],
            [],
            [],
        )
    };

    let forged = certified(Totality::Total);
    let scope = AlreadyJudged::with(&mounted, &forged);
    let admitted = text_verdicts_over(reader, &scope.globals());
    assert_eq!(
        admitted,
        Vec::new(),
        "a forged Total verdict admitted a looping proof of Absurd: {admitted:?}",
    );

    // The control: the same record, honest, refuses — so the admission rode on the lie.
    let honest = certified(Totality::Partial);
    let honest_scope = AlreadyJudged::with(&mounted, &honest);
    let refused = text_verdicts_over(reader, &honest_scope.globals());
    assert!(
        refused
            .iter()
            .any(|verdict| matches!(verdict.error, Error::NotTotal { .. })),
        "the control stopped refusing: an honest Partial must refuse the looping proof: {refused:?}",
    );
}

/// [`walked`] without its assertion, for a module a whole walk refuses.
fn walked_whole(definitions: &Definitions) -> Rechecked {
    put().module(definitions, None)
}

/// `Big : (i: Type 5) -> Type 6` with one case aiming at `${target}`, and `f(X: ${assumed}, b: Big(X)) -> X = match b at X | big() => ${body} end`.
///
/// The arm's index inversion would refine `X`, an outer binder assumed at `${assumed}`, to the target, and keeps the refinement only where the judge holds the target's sort below the variable's (`curios_analysis::invert`'s `fits_its_variable`). Kept, the arm's goal is the target, which `Type 3` inhabits where the target is `L` or `Type 4`; dropped, the goal stays the abstract `X` and nothing does.
const REFINING: &str = r#"
induct /Big: (i: #{level = 5} Type) -> pub #{level = 6} Type
| big(): (${target})
end

let /f: (X: ${assumed}, b: #{node = InductType} /Big()(X)) -> X =
    (X: ${assumed}, b: #{node = InductType} /Big()(X)) =>
        #{result = Ambient(X)} match b | big() => ${body} end;
"#;

/// [`REFINING`] at `target`, `assumed` and `body`.
fn refining(target: &str, assumed: &str, body: &str) -> String {
    fill!(REFINING, target, assumed, body)
}

/// The one gate that reads a published level and could admit a term — an index refinement the published sort makes `fits_its_variable` keep — reaches no closed term: the arm is behind a hypothesis the sort the variable is assumed at leaves uninhabited, and the arm's goal, which the publication cannot move, is the case's own target.
#[test]
fn a_kept_index_refinement_above_the_variables_sort_reaches_no_closed_term() {
    let honest = chained_sort_definitions(
        "#{level = 4} Type",
        "#{level = 5} Type",
        "#{level = 5} Type",
        "#{level = 5} Type",
    );
    let record = walked(&honest).certification;
    let scope = AlreadyJudged::with(&honest, &record);
    let verdicts = |text: &str| text_verdicts_over(text, &scope.globals());

    // The arm's body is typed, so what the goal is decides: a body the refined goal does not admit is refused.
    let bogus = verdicts(&refining("/L", "Type", "0"));
    assert!(!bogus.is_empty(), "the arm's body is typed: {bogus:?}");

    // The refinement is kept: `Type 3` inhabits the refined goal.
    assert_eq!(
        verdicts(&refining("/L", "Type", "#{level = 3} Type")),
        Vec::new(),
        "the arm is admitted",
    );

    // What the publication cannot move: the arm's goal is the case's target at every sort the variable is assumed at, since `MatchResult::at` opens the goal at the targets and reads no sort. `Type 3` inhabits `Type 4` and `0` does not, whether the rule holds the target's sort below the variable's (`Type 5`) or not (`Type 0`, `Type 1`).
    for assumed in ["Type", "#{level = 1} Type", "#{level = 5} Type"] {
        assert_eq!(
            text_verdicts(&refining("#{level = 4} Type", assumed, "#{level = 3} Type")),
            Vec::new(),
            "the goal is the target with the variable assumed at {assumed}",
        );
        assert!(
            !text_verdicts(&refining("#{level = 4} Type", assumed, "0")).is_empty(),
            "and a body the target does not admit is refused at {assumed}",
        );
    }

    // The refusal that stops the route: the arm is reachable only through a value of the family at the target, and the target does not inhabit the sort the variable is assumed at, which `subsumes` decides off the declared type and not off any publication.
    let applied = text_verdicts(&fill!(
        "${refined}${APPLYING}",
        refined = refining("#{level = 4} Type", "Type", "#{level = 3} Type"),
        APPLYING
    ));
    assert!(
        applied
            .iter()
            .any(|verdict| verdict.name == Some(global("g"))
                && matches!(verdict.error, Error::Mismatch { .. })),
        "nothing closed reaches the arm: {applied:?}",
    );
}

/// `g : Type 4 = f(Type 4, Big/big())`, beside [`REFINING`]'s family and function: the one call the arm is reachable through.
const APPLYING: &str = r#"
let /g: #{level = 4} Type =
    /f(#{level = 4} Type, #{node = Variant} /Big/big());
"#;

/// `induct Levelled.{u} : Type.{u + 1} | mk(x: Type.{u}) end`, whose entry carries `${variance}`: a payload's type mentions the level, so the walk computes it invariant.
const LEVELLED: &str = r#"
#{universe_context = {parameter_count = 1}, variances = [${variance}]}
induct /Levelled: pub #{level = u + 1} Type
| mk(x: #{level = u} Type)
end
"#;

/// `accept` refuses a carried variance vector claiming an irrelevance the walk denies, and `induct_alike` reads the carried vector, so an item whose terms are the baseline's and whose vector is not is judged again, and refused, as a walk over every declaration refuses it: whole and baseline walks agree, and no unit is stored carrying a vector its walk denies, which a later unit's elaborator would compare instances by.
#[test]
fn a_carried_variance_the_walk_denies_is_refused_over_its_baseline_as_over_none() {
    let truthful = versioned(core_text_module(&fill!(LEVELLED, variance = "Invariant")));
    let certification = walked(&truthful).certification;
    let claimed = versioned(core_text_module(&fill!(LEVELLED, variance = "Irrelevant")));

    let whole = walked_whole(&claimed);
    assert!(
        whole
            .verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::VarianceDenied { .. })),
        "a whole walk refuses the carried irrelevance: {:?}",
        whole.verdicts,
    );

    let over = walked_over(&claimed, &truthful, &certification);
    assert!(
        over.verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::VarianceDenied { .. })),
        "and the walk over the baseline adopts the carried irrelevance: {:?}",
        over.verdicts,
    );
    assert_eq!(
        over.certification.variances(&global("Levelled")),
        Some([Variance::Invariant].as_slice()),
        "while the vector it files, which every judgment reads, is the one the baseline's walk computed",
    );
}

/// `A.{u, v} : Type v = Type u` as a one-member group, its context stating `${constraints}`, the scheme [`SCHEME`] declares as a `let`: its body inhabits `Type (u + 1)`, so it fits the declared type only where the declaration assumes `u + 1 <= v`.
const GROUPED: &str = r#"
#{node = Rec, universe_context = {parameter_count = 2${constraints}}}
let /A: #{level = v} Type =
    #{level = u} Type;
"#;

/// A group's universe context dropped: the terms are the baseline's, the context is not, and `Baseline::holds` reads a group by its members' terms, names and marks alone.
#[test]
fn a_groups_universe_context_dropped_is_judged_again_as_a_whole_compile_judges_it() {
    let grouped = |constraints: &str| fill!(GROUPED, constraints);
    let baseline = versioned(core_text_module(&grouped(", constraints = [u + 1 <= v]")));
    let certification = walked(&baseline).certification;
    let edited = replaced(&baseline, "A", declared_item(&grouped(""), "A"));

    let whole = walked_whole(&edited);
    assert!(
        !whole.verdicts.is_empty(),
        "a whole walk refuses the group whose declaration assumes nothing: {:?}",
        whole.verdicts,
    );

    let over = walked_over(&edited, &baseline, &certification);
    assert_eq!(over.verdicts, whole.verdicts);
}

/// `carrier.{u, v, w, x} : <the record a one-member group computes> = <the four products>`, as a one-member top-level group whose context states `${constraints}`: Hurkens' paradox's `bottom`, `small_universe`, `over` and `pi`, as `xboard`'s formation ticket for a group read under its reader's constraints spells them.
///
/// The declaration is that ticket's `carrier`, a `let` there and a group here: a group's context is what [`Baseline::holds`] does not read, and a `let`'s is. `u + 1 <= w`, `v <= w`, `v + 1 <= x` and `w + 1 <= x` are the cumulativity the four products need.
const CARRIER: &str = r#"
#{node = Rec, universe_context = {parameter_count = 4${constraints}}}
let /carrier:
    #{node = Rec}
    (let x2: #{level = x} Type =
        {
            bottom: #{level = w} Type,
            universe: #{level = w} Type,
            over: (
                S: #{level = v} Type,
                F: (p: (s: S) -> #{level = u} Type) -> #{level = u} Type) -> #{level = w} Type,
            pi: (S2: #{level = v} Type, F2: (x: S2) -> #{level = u} Type) -> #{level = w} Type};
    x2) =
    (
        (A: #{level = u} Type) -> A,
        (
            X: #{level = u} Type,
            f: (
                t:
                    #{share = 0}
                    ((s: (s2: X) -> #{level = u} Type) -> #{level = u} Type)) -> X) ->
            #{shared = 0}
            _,
        (
            S: #{level = v} Type,
            F: (p: #{share = 1} ((s3: S) -> #{level = u} Type)) -> #{level = u} Type) =>
            (p2: #{shared = 1} _) -> F(p2),
        (S2: #{level = v} Type, F2: (x: S2) -> #{level = u} Type) => (x2: S2) -> F2(x2));
"#;

/// `paradox : False = <Hurkens' paradox over the carrier read at `{0, 1, 0, 2}`, boxed>`, closing on it: the unit a reader of the filed group compiles.
///
/// The instance sets `u` and `w` to zero, where the declaration's own constraints say `u + 1 <= w`: the four products then quantify over `Type 0` and stand at `Type 0`, which are the impredicative products the paradox needs.
const PARADOX: &str = r#"
induct /False: pub Prop
end

struct /Boxed: pub Type {
    proof: #{node = InductType} /False(),
}

let /paradox: #{node = InductType} /False() =
    #{field = Struct(/Boxed, 0)}
    (
        #{share = 0}
        ((
            p:
                #{share = 1}
                ((
                    s:
                        #{share = 2}
                        ((X: Type, f: (t: #{share = 3} ((s2: (s3: X) -> Type) -> Type)) -> X) ->
                            #{shared = 3}
                            _)) -> Type),
            h1: (x: #{shared = 2} _) -> (
                h: (
                    #{share = 4}
                    ((s4: #{shared = 2} _) =>
                        s4(
                            (#{universes = {levels = [0, 1, 0, 2]}} /carrier).1,
                            (t2: #{share = 5} ((s5: #{shared = 1} _) -> Type)) =>
                                (
                                    #{share = 6}
                                    ((t3: #{shared = 5} _) =>
                                        (
                                            X2: Type,
                                            f2: (
                                                t4: (
                                                    s6:
                                                        #{share = 7}
                                                        ((s7: X2) -> Type)) -> Type) -> X2) =>
                                            (p2: #{shared = 7} _) =>
                                                t3((x2: #{shared = 2} _) => p2(f2(x2(X2, f2))))))(
                                    t2))))(x)(p)) -> p(x)) =>
            h1(
                #{share = 8}
                (#{shared = 6} _)((p3: #{shared = 1} _) =>
                    (#{universes = {levels = [0, 1, 0, 2]}} /carrier).3(
                        #{shared = 2} _,
                        (x3: #{shared = 2} _) => (h2: (#{shared = 4} _)(x3)(p3)) -> p3(x3))))(
                (x4: #{shared = 2} _) =>
                    (
                        h22: (#{share = 9} (#{shared = 4} _)(x4))((y: #{shared = 2} _) =>
                            p((#{shared = 6} _)((#{shared = 4} _)(y))))) =>
                        h1((#{shared = 6} _)(#{shared = 9} _))(h22))))(
        #{share = 10}
        ((y2: #{shared = 2} _) =>
            (
                h3: (#{share = 11} (#{universes = {levels = [0, 1, 0, 2]}} /carrier).2)(
                    #{shared = 2} _,
                    (p4: #{shared = 1} _) =>
                        (h4: (#{share = 12} (#{shared = 4} _)(y2))(p4)) -> p4(
                            (#{shared = 6} _)(#{shared = 12} _)))) -> (
                #{universes = {levels = [0, 1, 0, 2]}}
                /carrier).0),
        (x5: #{shared = 2} _) =>
            (h23: (#{share = 13} (#{shared = 4} _)(x5))(#{shared = 10} _)) =>
                (
                    h32: (#{shared = 11} _)(
                        #{shared = 2} _,
                        (p5: #{shared = 1} _) =>
                            (h5: (#{shared = 13} _)(p5)) -> p5(
                                #{share = 14} (#{shared = 6} _)(#{shared = 13} _)))) =>
                    h32(#{shared = 10} _)(
                        h23)(
                        (p6: #{shared = 1} _) =>
                            (h42: (#{shared = 4} _)(#{shared = 14} _)(p6)) =>
                                h32((y3: #{shared = 2} _) =>
                                    p6((#{shared = 6} _)((#{shared = 4} _)(y3))))(
                                    h42)))((p7: #{shared = 1} _) =>
        (h6: (#{shared = 4} _)(#{shared = 8} _)(p7)) =>
            (#{shared = 0} _)(
                (y4: #{shared = 2} _) => p7((#{shared = 6} _)((#{shared = 4} _)(y4))),
                h6))(#{node = StructType} /Boxed()).0;

/paradox
: #{node = InductType} /False()
"#;

/// Hurkens' paradox through a group whose cumulativity a recompile dropped, the route `Baseline::holds` once opened: the walk over the baseline refuses the group as a walk over every declaration refuses it, so no unit is filed carrying a context no walk assumed, and the paradox read through the carrier the baseline's walk judged is refused by the instance that cumulativity does not discharge.
#[test]
fn hurkens_paradox_through_a_group_offered_without_its_context_is_refused() {
    let carrier = |constraints: &str| fill!(CARRIER, constraints);
    let baseline = versioned(core_text_module(&carrier(
        ", constraints = [u + 1 <= w, v <= w, v + 1 <= x, w + 1 <= x]",
    )));
    let certification = walked(&baseline).certification;
    let offered = replaced(&baseline, "carrier", declared_item(&carrier(""), "carrier"));

    let over = walked_over(&offered, &baseline, &certification);
    assert!(
        !over.verdicts.is_empty(),
        "the walk adopted a group its context no longer licenses",
    );
    assert_eq!(over.verdicts, walked_whole(&offered).verdicts);

    let judged = AlreadyJudged::with(&baseline, &certification);
    let refused = text_verdicts_over(PARADOX, &judged.globals());
    assert!(
        refused
            .iter()
            .any(|verdict| matches!(verdict.error, Error::UniverseInstance { .. })),
        "the paradox's instance does not discharge the carrier's constraints, and was not refused for it: {refused:?}",
    );
}

/// `lemma : (P: Prop, h: P) -> P = (P, h) => h`, the item the baseline's walk states the marks of.
const LEMMA: &str = "let /lemma: (P: Prop, h: P) -> P = (P: Prop, h: P) => h;";

/// `datum : (A: Type, a: A) -> A = (A, a) => a`, [`LEMMA`]'s twin at `Type`.
const DATUM: &str = "let /datum: (A: Type, a: A) -> A = (A: Type, a: A) => a;";

/// An item the elaborator made again, its terms the baseline's but for its binders' names and marks, is handed back with what the baseline's walk stated of it under the names it was written with: `lemma`'s second binder binds a proof, and its binders are `Q` and `k` though the baseline's are `P` and `h`.
#[test]
fn an_item_made_again_is_handed_back_with_the_baselines_statements_under_its_own_names() {
    let stated = walked(&core_text_module(LEMMA));
    let baseline = versioned(stated.definitions);
    let edited = replaced(
        &baseline,
        "lemma",
        declared_item(
            "let /lemma: (Q: Prop, k: Q) -> Q = (#{proof = true} Q: Prop, k: Q) => k;",
            "lemma",
        ),
    );

    let rechecked = walked_over(&edited, &baseline, &stated.certification);

    assert_eq!(rechecked.verdicts, Vec::new());
    let handed = rechecked
        .definitions
        .items
        .iter()
        .find_map(|item| match item {
            Item::Let(definition) if definition.name == global("lemma") => {
                Some(definition.body.clone())
            }
            _ => None,
        })
        .expect("the module holds `lemma`");
    let Subterm::Func(Func { telescope }) = &*handed else {
        panic!("a function literal");
    };
    assert_eq!(telescope.proofs(), [false, true]);
    assert_eq!(telescope.labels(), ["Q", "k"]);
}

/// A module that declares a name twice reaches no walk over a baseline: `three` respelled as a second `identity`, a name the baseline holds at a place the edit leaves alone and no unit in scope declares, so the clause that would answer is [`declares_a_held_name`]'s reading of the baseline's places.
///
/// It never gets there. A walk over a baseline is handed where the module holds each name, and `Places::of` asserts a name is declared once, so the duplicate refuses the compilation ahead of the walk's own `Error::Redeclared` — a panic where it refuses, as the soundness board's fourth boundary allows. The whole-module walk, handed no places, is the one `redeclarations` guards, and `xboard`'s admission ticket is its witness.
#[test]
#[should_panic(expected = "a module declares one name twice")]
fn a_module_that_declares_a_name_twice_reaches_no_walk_over_a_baseline() {
    let baseline = versioned(written());
    let certification = walked(&baseline).certification;
    let edited = replaced(
        &baseline,
        "three",
        declared_item(
            "let /identity: (x: #NatType) -> #NatType = (x: #NatType) => x;",
            "identity",
        ),
    );

    walked_over(&edited, &baseline, &certification);
}

/// Two items the elaborator made again, each standing where the other stood, are handed back with what the baseline's walk stated of the item of their own name, under their own binders' names: `lemma`'s second binder binds a proof and `datum`'s does not, though the module states each the other way round and holds each at the other's place.
///
/// Mutation-check to make: with the item whose statements are carried taken where the baseline holds the place the module holds the item at, rather than where it holds the item's first name (`held_item`), each takes the other's.
#[test]
fn items_made_again_in_each_others_places_take_the_statements_of_their_own_names() {
    let stated = walked(&core_text_module(&fill!(
        "${LEMMA}\n${DATUM}",
        LEMMA,
        DATUM
    )));
    let baseline = versioned(stated.definitions);
    let places = Places::of(&baseline);
    let at = |name: &str| {
        places
            .place(&global(name))
            .expect("the baseline declares the name")
            .item()
    };
    let mut items = baseline.items.clone();
    items.replace(
        at("lemma"),
        declared_item(
            "let /datum: (B: Type, b: B) -> B = (B: Type, #{proof = true} b: B) => b;",
            "datum",
        ),
    );
    items.replace(
        at("datum"),
        declared_item(
            "let /lemma: (Q: Prop, k: Q) -> Q = (Q: Prop, k: Q) => k;",
            "lemma",
        ),
    );

    let rechecked = walked_over(&Definitions { items }, &baseline, &stated.certification);

    assert_eq!(rechecked.verdicts, Vec::new());
    let statements = |name: &str| {
        let body = rechecked
            .definitions
            .items
            .iter()
            .find_map(|item| match item {
                Item::Let(definition) if definition.name == global(name) => {
                    Some(definition.body.clone())
                }
                _ => None,
            })
            .expect("the module holds the name");
        let Subterm::Func(Func { telescope }) = &*body else {
            panic!("a function literal");
        };

        (
            telescope.proofs(),
            telescope
                .labels()
                .into_iter()
                .map(str::to_string)
                .collect::<Vec<_>>(),
        )
    };
    assert_eq!(
        statements("lemma"),
        (vec![false, true], vec!["Q".to_string(), "k".to_string()])
    );
    assert_eq!(
        statements("datum"),
        (vec![false, false], vec!["B".to_string(), "b".to_string()])
    );
}
