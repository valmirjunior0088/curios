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
/// The sizing check walks a constructor telescope's **domains** — each must sit at or below the family's declared level — and stops there, so the terminal the telescope ends in, `Family(params, indices)`, is typed by clause 8 of [`check_induct_decl`](crate::check_induct_decl). A constructor's declared type ends in that terminal too, but a constructor is made of the entry where it is read and judged by nothing apart from it, so no other judgment types the targets.
///
/// The module below carries the registry entry and no items at all, and its index target is an unsolved metavariable — precisely what the elaborator's strict zonk promises has been eliminated. Relying on the lowering to type it would make the guarantee the elaborator's word rather than the certifier's, which is the dependency the two-checker split exists to remove.
///
/// It is reachable from no surface program — the elaborator builds registry and bindings from one declaration — which is why it belongs here rather than in `curios/src/tests`. The diagnostic is asserted rather than bare failure, since a module this small could fail for unrelated reasons and still look guarded.
///
/// The control is [`a_registry_index_target_of_a_real_term_is_accepted`], which is the same module with the metavariable replaced by a literal: the pass must refuse an elaboration-only node, not every registry entry.
#[test]
fn a_registry_index_target_is_checked_rather_than_believed() {
    let target = "#{id = 7, origin = Hole} ?";
    let verdicts = text_refusals(&fill!(INDEXED, target));

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
        text_refusals(&fill!(INDEXED, target)),
        Vec::new(),
        "the boundary pass refused a registry entry that carries nothing elaboration-only",
    );
}

/// A constructor's index target may be a *proof*, and no judgment in the walk types it.
///
/// Definitional proof irrelevance accepts without inspecting either term, and what makes that correct is that every inhabitant of a proposition is total, which is proof totality's job (see `documentation/design/soundness/totality-of-the-erased-program.md`). The premise that argument needs is that **proof totality inspects every `Prop`-typed term in the accepted module** — and for the certifier's own proof totality, seeded from its own typing rather than from the elaborator's hook, that reduces to whether the walk types every such term.
///
/// A constructor's index target is where that needs a clause. `Totals::classify` reads the items the walk judges and nothing else, and the module below has none; `free_locals_of` is the only other pass that reads a registry entry, and it collects free locals rather than partiality; and the sizing check walks a constructor telescope's domains and stops at the terminal. So only typing the target puts `check_group`'s local gate — which refuses a proof-typed member whose group does not descend — in front of it.
///
/// The module below states its constructor's index at `rec p : Held = p`, a closed non-descending inhabitant of a proposition; irrelevance would identify that target with any other proof of `Held`, which is the identification proof totality exists to prevent. Reachable from no surface program — the elaborator builds registry and bindings from one declaration and types the targets through the constructor wrappers it lowers — which is why it is built here.
///
/// Clause 8 of `check_induct_decl`, in `check_constructed`, is what refuses it: every index target must inhabit the index telescope at the family's parameters, which *checks* the target instead of scanning past it — partiality is not syntactic, so establishing it takes typing the registry rather than reading it — and typing `rec p : Held = p` against `Held` is what puts `check_group`'s gate in front of it.
///
/// The control is [`a_real_proof_in_a_registry_index_target_is_accepted`], the same declaration aimed at a genuine `qed()`: what establishes the target must refuse a diverging proof without refusing an ordinary one. The assertion pins the refusing item as well as the diagnostic, because this module declares two families and "does not descend" reaching the verdict list from either would otherwise read as a pass.
#[test]
fn a_partial_proof_in_a_registry_index_target_is_refused() {
    let target = "#{node = Rec}\n    (let p: #{node = InductType} /Held() = p;\n    p)";
    let verdicts = text_refusals(&fill!(INDEXED_BY_PROOF, target));

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
        text_refusals(&fill!(INDEXED_BY_PROOF, target)),
        Vec::new(),
        "a constructor aimed at a genuine proof was refused",
    );
}

/// A family takes its sort from its entry at the levels an occurrence supplies: `Foo` at instance `[u, u]` claimed to inhabit the sort its entry gives instance `[u, v]` is refused with a `Mismatch` of `Type u` against `Type v`.
#[test]
fn a_family_takes_the_sort_its_registry_gives_the_levels_supplied() {
    // The sort is the *second* parameter, so an occurrence's second level decides it: `Foo.{u, u}` claimed at `Type v`, whose sort is `Type u`, and nothing may conflate the two.
    let verdicts = text_refusals(
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
        let verdicts = text_refusals(&fill!(
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

/// A family's index types are type positions its entry's typing records, checked under the item that declares the family: `Indexed`'s one index is typed `((g : (Nat) -> Nat) => Nat)(reaches)`, which names the partial `reaches` as written, and is refused under `Indexed`. A former is made of its entry and checked as no definition, so the entry's typing is the one place an index type is read.
#[test]
fn a_partial_index_type_is_refused_under_its_family() {
    let verdicts = text_refusals(
        r#"
let /sink: (n: #NatType) -> #NatType =
    #{node = Rec}
    (let x: (n: #NatType) -> #NatType = (n2: #NatType) => x(n2);
    x);

let /reaches: (m: #NatType) -> #NatType =
    (m: #NatType) => /sink(m);

induct /Indexed: (i: ((g: (n: #NatType) -> #NatType) => #NatType)(/reaches)) -> pub Type
end
"#,
    );

    assert!(
        refuses(&verdicts, Some("Indexed"), |error| matches!(
            error,
            Error::NotTotal { .. }
        )),
        "{verdicts:?}",
    );
}
