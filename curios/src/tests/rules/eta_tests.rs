//! Eta at a function and a record: an expansion that drops a binder or swaps components is none, a literal's eta where no type directs it, and a struct's function fields.

use {super::test_support::*, crate::tests::run};

// Eta and untyped child positions. Conversion is type-directed, so eta is what converts `f` with `(x) => f(x)` and `p` with `(p.0, p.1)` without either side having to be written in that shape. Both rules are *accepting*, so each widens what counts as equal, and the two refusals beside them are what keep the acceptance from reading as "any two functions convert" and "any two records convert": drop the binder from the expansion and the equation dies, swap the components and it dies. Without them a `compare` that answered `true` at every Π and every Σ would satisfy the accepting rung and nothing here would notice.
#[test]
fn converts_a_function_and_a_record_with_their_expansions() {
    assert_eq!(
        run(ETA_CONVERTS_A_FUNCTION_AND_A_RECORD_WITH_THEIR_EXPANSIONS),
        b"1"
    );
}

#[test]
fn an_expansion_that_drops_its_binder_is_not_eta() {
    rejected_by(
        AN_EXPANSION_THAT_DROPS_ITS_BINDER_IS_NOT_ETA,
        "type mismatch",
    );
}

#[test]
fn an_expansion_that_swaps_its_components_is_not_eta() {
    rejected_by(
        AN_EXPANSION_THAT_SWAPS_ITS_COMPONENTS_IS_NOT_ETA,
        "type mismatch",
    );
}

// **Eta by a literal needs no type, so it holds where none directs it.** The elaborator fires eta by a side's shape at every goal, and the kernel by the goal's type, which a child it compares at `Type` does not have: a stuck elimination's arm, a projection's head, a lambda's body inside one, and — with no `match` written — the arms a definition unfolds to once a record's eta has projected its two calls. An expansion equal to its neutral at the goal was refused there, by the kernel alone, so conversion was no congruence in the trusted checker and the refusal reached the author as the kernel's. A lambda and a tuple literal against a neutral fire the rule by their own shape (`function_eta`, `tuple_eta`), as a struct literal always has.
//
// The two refusals beside it are the near misses of the first fixtures, in the same position: the rule compares what the literal holds, and does not give up on functions and records there.
#[test]
fn a_literals_eta_holds_where_no_type_directs_it() {
    assert_eq!(run(A_LITERALS_ETA_HOLDS_WHERE_NO_TYPE_DIRECTS_IT), b"1");
}

#[test]
fn an_expansion_that_drops_its_binder_is_not_eta_in_an_arm() {
    rejected_by(
        AN_EXPANSION_THAT_DROPS_ITS_BINDER_IS_NOT_ETA_IN_AN_ARM,
        "type mismatch",
    );
}

#[test]
fn an_expansion_that_swaps_its_components_is_not_eta_in_an_arm() {
    rejected_by(
        AN_EXPANSION_THAT_SWAPS_ITS_COMPONENTS_IS_NOT_ETA_IN_AN_ARM,
        "type mismatch",
    );
}

// **A literal's eta is taken against every neutral, one set in both checkers.** The kernel took it against a variable, a projection and a stuck application, and the elaborator against any term, so a literal against a stuck elimination elaborated and was refused by the kernel alone: a tuple literal and a lambda in a stuck elimination's arm, where no type directs the rule, a tuple literal there against a definition that unfolds to one, and a struct literal at the struct's own type. A neutral is every term whose head does not show its type, which is what a shape can tell where the goal states no type.
//
// The two refusals beside it are the near misses: the rule compares what the literal holds.
#[test]
fn a_literals_eta_is_taken_against_a_stuck_elimination() {
    assert_eq!(
        run(A_LITERALS_ETA_IS_TAKEN_AGAINST_A_STUCK_ELIMINATION),
        b"1"
    );
}

#[test]
fn a_tuple_that_swaps_a_stuck_eliminations_components_is_not_its_eta() {
    rejected_by(
        A_TUPLE_THAT_SWAPS_A_STUCK_ELIMINATIONS_COMPONENTS_IS_NOT_ITS_ETA,
        "type mismatch",
    );
}

#[test]
fn a_struct_that_swaps_a_stuck_eliminations_fields_is_not_its_eta() {
    rejected_by(
        A_STRUCT_THAT_SWAPS_A_STUCK_ELIMINATIONS_FIELDS_IS_NOT_ITS_ETA,
        "type mismatch",
    );
}

// **What this pins is the vacuous walk, and the invariant that licenses it.** Every field of `Sealed` is a proposition, so `struct_eta`'s walk compares *nothing at all* and answers `true` on the strength of the neutral restriction alone. That is sound because `other` inhabits `Sealed` — conversion is only ever asked about two terms of one type — and because eta for a single-constructor record equates any inhabitant with the literal of its projections.
//
// The restriction is a proxy for that invariant rather than a second guarantee: a `Var` is as arbitrary a term as any other, and the invariant is a property of the *callers* rather than of this function. `struct_eta` says so in its own documentation.
//
// Its comparison is `Sealed { one = p, two = q }` against `b` as arguments of `f`, and a spine's arguments take the telescope their head carries, so the position is typed: the fixture reaches `struct_eta` from a typed goal, and what it holds is the vacuous walk itself.
#[test]
fn a_nominal_structs_eta_is_not_forfeited_there() {
    assert_eq!(run(A_NOMINAL_STRUCTS_ETA_IS_NOT_FORFEITED_THERE), b"1");
}

// **`/std`'s own laws need the rest of it.** `State`'s left identity sets `State/bind`'s literal against the neutral `f(a)`, and two things make it converge. The field compares at the function type the declaration gives it, where eta opens both sides at a fresh state, rather than at `Type`, where a lambda never meets a projection. And a stuck *application* counts as the neutral inhabitant it is, where a `struct_eta` taking only a variable or a projection would refuse the law not on its content but on the shape of the side it is stated against.
//
// A refusal there falls through to `unfolded_retry` — an application may have an unfolding left where a variable and a projection have none, so the eta attempt is tried first and a failure hands the pair on rather than deciding it.
#[test]
fn a_structs_function_field_meets_a_neutral_application() {
    assert_eq!(
        run(A_STRUCTS_FUNCTION_FIELD_MEETS_A_NEUTRAL_APPLICATION),
        b"1"
    );
}

// The right identity is the same law with a variable on the neutral side, which `struct_eta` admits as a variable — so this one needs the typed field alone, and it is what separates the two halves.
#[test]
fn a_structs_function_field_meets_a_neutral_variable() {
    assert_eq!(run(A_STRUCTS_FUNCTION_FIELD_MEETS_A_NEUTRAL_VARIABLE), b"1");
}

// The control for the pair above. Associativity sets two literals against each other, so both checkers compare the field at the declaration's telescope and the law certifies: what the two refusals lack is a typed position, not a rule about `State`.
#[test]
fn two_struct_literals_compare_their_function_fields() {
    assert_eq!(run(TWO_STRUCT_LITERALS_COMPARE_THEIR_FUNCTION_FIELDS), b"1");
}

// Two recursions of one shape that part in their base case. Unfolded once, each poses the same two calls again, and a conversion that took a pair it met again as held would equate two definitions that differ.
#[test]
fn two_distinct_recursions_do_not_convert() {
    rejected_by(DISTINCT_RECURSIONS_ARE_NOT_EQUAL, "type mismatch");
}
