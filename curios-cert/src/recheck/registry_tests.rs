//! Index targets the walk checks rather than believes, and the scheme a registry may declare apart from its type former.

use {super::test_support::*, crate::*, curios_common::fill, curios_surface::Spelling};

/// `Indexed(i : Nat) : Type`, whose one case, not defined, targets `${target}`.
const INDEXED: &str = r#"
induct /Indexed: (i: #NatType) -> pub Type
| mk(): (${target})
end
"#;

/// `Held`, a proposition, and `Indexed(i : Held) : Type`, whose one case targets the proof `${target}`; neither family's cases are defined.
const INDEXED_BY_PROOF: &str = r#"
induct /Held: pub Prop
| qed()
end

induct /Indexed: (i: #{node = InductType} /Held()) -> pub Type
| mk(): (${target})
end
"#;

/// A constructor's *index target* is registry data, which the walk checks rather than believes.
///
/// The sizing check walks a constructor telescope's **domains** — each must sit at or below the family's declared level — and stops there, so the terminal the telescope ends in, `Family(params, indices)`, is typed by clause 9 of [`check_induct_decl`](crate::check_induct_decl). In a module the elaborator built, a constructor wrapper's declared type ends in that terminal too, so checking the wrapper's body types the targets again — but a module need not carry the wrappers.
///
/// The module below carries the registry entry and no items at all, and its index target is an unsolved metavariable — precisely what the elaborator's strict zonk promises has been eliminated. Relying on the lowering to type it would make the guarantee the elaborator's word rather than the certifier's, which is the dependency the two-checker split exists to remove.
///
/// It is reachable from no surface program — the elaborator builds registry and bindings from one declaration — which is why it belongs here rather than in `curios/src/tests`. The diagnostic is asserted rather than bare failure, since a module this small could fail for unrelated reasons and still look guarded.
///
/// The control is [`a_registry_index_target_of_a_real_term_is_accepted`], which is the same module with the metavariable replaced by a literal: the pass must refuse an elaboration-only node, not every registry entry.
#[test]
fn a_registry_index_target_is_checked_rather_than_believed() {
    let target = "#{id = 7, origin = Hole} ?";
    let verdicts = text_verdicts(&fill!(INDEXED, target));

    assert!(
        refuses(&verdicts, Some("Indexed"), |error| matches!(
            error,
            Error::NotCore(_)
        )),
        "the certifier accepted a module carrying an unsolved metavariable: {verdicts:?}",
    );
}

/// The control for the fixture above: a registry entry whose index target is a real term stays accepted.
#[test]
fn a_registry_index_target_of_a_real_term_is_accepted() {
    let target = "0";

    assert_eq!(
        text_verdicts(&fill!(INDEXED, target)),
        Vec::new(),
        "the boundary pass refused a registry entry that carries nothing elaboration-only",
    );
}

/// A constructor's index target may be a *proof*, and no judgment in the walk types it.
///
/// Definitional proof irrelevance accepts without inspecting either term, and what makes that correct is that every inhabitant of a proposition is total, which is (V)'s job (see `documentation/design/soundness/totality-of-the-erased-program.md`). The premise that argument needs is that **(V) inspects every `Prop`-typed term in the accepted module** — and for the certifier's own (V), seeded from its own typing rather than from the elaborator's hook, that reduces to whether the walk types every such term.
///
/// A constructor's index target is where that needs a clause. `Totals::classify` reads the items the walk judges and nothing else, and the module below has none; `free_locals_of` is the only other pass that reads a registry entry, and it collects free locals rather than partiality; and the sizing check walks a constructor telescope's domains and stops at the terminal. So only typing the target puts `check_group`'s local gate — which refuses a proof-typed member whose group does not descend — in front of it.
///
/// The module below states its constructor's index at `rec p : Held = p`, a closed non-descending inhabitant of a proposition; irrelevance would identify that target with any other proof of `Held`, which is the identification (V) exists to prevent. Reachable from no surface program — the elaborator builds registry and bindings from one declaration and types the targets through the constructor wrappers it lowers — which is why it is built here.
///
/// Clause 9 of `check_induct_decl`, in `check_constructed`, is what refuses it: every index target must inhabit the index telescope at the constructor's own parameters, which *checks* the target instead of scanning past it — partiality is not syntactic, so establishing it takes typing the registry rather than reading it — and typing `rec p : Held = p` against `Held` is what puts `check_group`'s gate in front of it.
///
/// The control is [`a_real_proof_in_a_registry_index_target_is_accepted`], the same declaration aimed at a genuine `qed()`: what establishes the target must refuse a diverging proof without refusing an ordinary one. The assertion pins the refusing item as well as the diagnostic, because this module declares two families and "does not descend" reaching the verdict list from either would otherwise read as a pass.
#[test]
fn a_partial_proof_in_a_registry_index_target_is_refused() {
    let target = "#{node = Rec}\n    (let p: #{node = InductType} /Held() = p;\n    p)";
    let verdicts = text_verdicts(&fill!(INDEXED_BY_PROOF, target));

    assert!(
        refuses(&verdicts, Some("Indexed"), |error| {
            error
                .rendered(&Spelling::default())
                .contains("does not descend")
        }),
        "the certifier accepted a non-descending proof standing as a constructor's index target: {verdicts:?}",
    );
}

/// The control for the fixture above: an ordinary proof in the same position stays accepted.
#[test]
fn a_real_proof_in_a_registry_index_target_is_accepted() {
    let target = "#{node = Variant} /Held/qed()";

    assert_eq!(
        text_verdicts(&fill!(INDEXED_BY_PROOF, target)),
        Vec::new(),
        "a constructor aimed at a genuine proof was refused",
    );
}

/// An inductive's registry entry and its type-former definition may declare different universe schemes here, and that is not a hole.
///
/// `curios-elab`'s `validate_universes` refuses the disagreement outright — "inductive Foo and its type-former definition have different universe contexts" — and it is one of three such agreement clauses, the others pairing a `struct` with its type-former and a `concept` with its structure entry. All three are elaborator-only, and one concept stored twice with the agreement checked in one place is a shape worth constructing rather than assuming.
///
/// The certifier accepts it, in both directions of the mismatch, and the reason it may is that it never reads the agreement: it reads each scheme where that scheme is *authoritative*. An `InductType` occurrence is checked and instantiated against the registry entry's context; the definition is checked under its own. Neither is ever consulted for the other's question, so there is nothing for a disagreement to corrupt.
///
/// The second fixture is what makes that a demonstration rather than a hope. It claims `Foo` at instance `[u, u]` inhabits the sort `Foo`'s registry gives instance `[u, v]`, and the certifier refuses with a `Mismatch` of `Type u` against `Type v` — the registry's `result_sort` applied faithfully to the levels the *occurrence* supplied. A certifier that read the definition's scheme here, or conflated the two, would accept it.
///
/// The verdict: this is the elaborator being strict, not the certifier being permissive. The distinction is not that the elaborator is merely cautious — the agreement is load-bearing *for the elaborator*, which reads a scheme from whichever of its two copies is in hand. It is not load-bearing for a checker that reads each in its own place. No rule was added: making the certifier enforce an elaborator construction invariant it does not consult would be the certifier believing elaborator output, which is the thing this crate exists not to do.
///
/// Both routes by which a disagreement *could* have mattered are closed already, and by this same file: an ill-scoped level (`a_level_naming_an_undeclared_universe_parameter_is_refused`) and an instance of the wrong width (`a_universe_instance_narrower_than_its_scheme_is_refused`).
#[test]
fn a_registry_and_its_type_former_may_declare_different_schemes() {
    for (label, text) in [
        (
            "registry narrower",
            r#"
#{
    node = Let,
    universe_context = {parameter_count = 2},
    entry = {universe_context = {parameter_count = 1}}}
induct /Foo: pub #{level = u} Type
end
"#,
        ),
        (
            "definition narrower",
            r#"
#{
    node = Let,
    universe_context = {parameter_count = 1},
    body = #{node = InductType, universes = {levels = [u, u]}} /Foo(),
    entry = {universe_context = {parameter_count = 2}}}
induct /Foo: pub #{level = u} Type
end
"#,
        ),
    ] {
        assert_eq!(
            text_verdicts(text),
            Vec::new(),
            "{label}: the certifier refused a disagreement it never consults",
        );
    }
}

/// The demonstration that each scheme is nonetheless applied where it is authoritative: a claim about the family's sort that the *registry's* `result_sort` refutes.
#[test]
fn a_family_takes_the_sort_its_registry_gives_the_levels_supplied() {
    // The sort is the *second* parameter, so an occurrence's second level decides it: `Foo.{u, u}` claimed at `Type v`, whose sort is `Type u`, and nothing may conflate the two.
    let verdicts = text_verdicts(
        r#"
#{universe_context = {parameter_count = 2}}
induct /Foo: pub #{level = v} Type
end

#{universe_context = {parameter_count = 2}}
let /held: #{level = v} Type =
    #{node = InductType, universes = {levels = [u, u]}} /Foo();
"#,
    );

    assert!(
        refuses(&verdicts, Some("held"), |error| matches!(
            error,
            Error::Mismatch { .. }
        )),
        "the registry's result sort was not applied to the levels the occurrence supplied: {verdicts:?}",
    );
}

/// What typing a registry entry records is checked under the item that declares it. A constructor's payload type is a type position, and `Carrier`'s, `((g : (Nat) -> Nat) => Nat)(reaches)`, is `Nat` once reduced and names the partial `reaches` as written: refused under `Carrier`, whether a definition follows it or it is the module's last.
///
/// Mutation-checked: with an item's positions drained before its entries are accepted, the refusal names the definition after `Carrier`, and no name where `Carrier` is last.
#[test]
fn an_entrys_obligation_is_reported_under_the_item_that_declares_it() {
    for (followed, after) in [(true, "\nlet /after: #NatType = 0;"), (false, "")] {
        let verdicts = text_verdicts(&fill!(
            r#"
let /sink: (n: #NatType) -> #NatType =
    #{node = Rec}
    (let x: (n: #NatType) -> #NatType = (n2: #NatType) => x(n2);
    x);

let /reaches: (m: #NatType) -> #NatType =
    (m: #NatType) => /sink(m);

induct /Carrier: pub Type
| mk(x: ((g: (n: #NatType) -> #NatType) => #NatType)(/reaches))
end
${after}"#,
            after
        ));

        assert!(
            refuses(&verdicts, Some("Carrier"), |error| matches!(
                error,
                Error::NotTotal { .. }
            )),
            "followed by a definition: {followed}; {verdicts:?}",
        );
    }
}
