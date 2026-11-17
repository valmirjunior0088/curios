//! The program tests of the rules that can admit a term: what fails when a rule stops holding, a file for each rule.
//!
//! What the soundness claim covers is `documentation/design/soundness/the-soundness-board.md`'s. The soundness board, a directory per ticket under `xboard/tickets/<part>/`, holds a ticket only for a proof of `False` that was seen admitted, so a rule nothing has broken has no witness there: these tests are what fails when such a rule stops holding. Each program is compiled as a user's is, through the prelude-replay path where every analysis sees only the user suffix and reads the prelude's verdicts back from the archive; the lattices and checkers themselves are unit-tested in their own crates.
//!
//! Strict positivity is what makes `induct` and `struct` sound: an inductive declaration claims its functor has an initial algebra, and without the gate `induct Bad | c(f : (Bad) -> False) end` inhabits `False`. The totality obligations are erasure's: it deletes types and `Prop`-sorted proofs, and both must be total, since a divergent type breaks type formation and a divergent proof proves anything, while what erasure keeps may diverge freely — which is why every totality rejection is of a position and never of a definition. Coherence is one witness per key.
//!
//! Each refusal is derived from its control ([`test_support::Control::refused`]) and is a pattern over the checker's error, by its variant and where it stands, as [the soundness board](../../../documentation/design/soundness/the-soundness-board.md) places a held attack: a rule test that accepts any error, or any error whose message holds a fragment, is worse than none, since an invalid fixture passes it while the rule it names goes unchecked, as a probe refused with `unbound variable` passes having never reached the check at all.
//!
//! A program a rule admits is a test of its own only where it states a claim of its own: a control broader than one refusal's, or the twin of a certifier test under its name. One that controls a single refusal is that refusal's control.

mod arm;
mod chain;
mod coherence;
mod coverage;
mod effect;
mod eta;
mod group;
mod index;
mod inversion;
mod irrelevance;
mod large_elimination;
mod metavariable;
mod mutation;
mod one_inhabitant;
mod plicity;
mod positivity_composition;
mod positivity_computed_type;
mod positivity_datum;
mod positivity_index;
mod positivity_occurrence;
mod proposition;
mod proposition_elimination;
mod sequence;
mod spine;
mod subsumption;
mod test_support;
mod totality;
mod totality_callback;
mod totality_callback_binder;
mod totality_callback_part;
mod totality_descent;
mod totality_partiality;
mod totality_projection;
mod totality_proof_release;
mod totality_record;
mod totality_standing;
