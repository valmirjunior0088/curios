# `List` and `Str` state the laws a proof about them reaches for

**Not refined yet.** This specification reserves the theorems `/std/List` and `/std/Str` state of their own operations, for the proofs a program writes about the lists and texts it holds. It is not an implementation plan; its gate is a program whose proof reaches for a law no lemma states.

## What is missing

- **`List` and `Str` state almost none of their laws.** `/std/List`'s functions (`reverse`, `filter`, `take`, `drop`, `zip`, `map`, `fold`, `range`, `replicate`) carry no theorem, and `/std/Str` states two (`eq_of_bytes` and `Str/length/concat`), where `Map` proves what lookup answers after each of its operations, and `Set/has` and `List/sort_by` prove theirs. `/std` derives the few list facts it needs where it needs them (`List/set`'s `length` and `at`, `Vec`'s replicate, `Tui/Frame`'s padding), so no proof of the library carries the weight; a program's would.

## What is known

- **Peers hold hundreds.** Lean's core and Batteries, Agda's `Data.List.Properties`, Rocq's `Lists`, Dafny's `Std.Collections` and Verus's `seq_lib` each state the length, membership and composition laws a proof about a program's lists reaches for.
- **Conversion decides concatenation's laws**, so those need no lemma; a law is one conversion does not decide.
- **A law takes the form its kind calls for** ([Equality is decided where a decider exists and is `Eq` everywhere, joined by one law](../../design/theory/equality-is-decided-where-a-decider-exists-and-is-eq-everywhere-joined-by-one-law.md)): a length law a decided bound the elaborator discharges once stated, a membership law over elements with no decider an `Eq`.

## Open

- **Which laws first**: those the first programs that write such proofs reach for.
