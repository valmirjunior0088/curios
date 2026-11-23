//! The respelling grid: whether a verdict follows how a term is spelled, stated so that both answers are checked.
//!
//! A row is two spellings conversion equates at the top of an item, and a column is a position one of them is put in: the arm of a guard written as the other, an index inverted in that arm, a fold's operand, the arm of a second match on a variable the guard names or on an expression it meets, two such arms one inside the other. Where conversion is a congruence, stable under an arm's equation and under the substitution an arm performs, every cell holds, since a term stands wherever a term it converts with stands. A cell that does not is stated here as refused, or as the certifier's refusal of what the elaborator accepted, so the refused set is a record, and deciding one is a cell moving rather than a test appearing.
//!
//! **Each row's control is its first column.** A pair conversion does not equate outside every arm is no respelling, and a cell over it would be refused for that and say nothing of the position; so each row states the equation at the top of an item, and it is held.

use {
    super::laws::{Binder, orders},
    super::test_support::*,
    curios_common::fill,
};

const IMPORTS: &str =
    "use /std/{Nat, Int, Bool, List, Eq, Io, proved}; use /std/Bool/{Holds, True};";

/// Three recursive definitions over a list of any element type. Every written call of one takes a universe level of its own, so one call written twice is two spellings apart in an instance, which no unfolding settles. A predicate, a wrapper of it and an alias of `Nat` give the respellings of an argument no implicit takes part in; a family indexed by a truth value is the index a respelling is put at, and a pair of naturals the projection one is spelled through; a proof stuck on a binder, a struct and a family each bounding a field, and a division's bound give the proofs one is spelled through; a structure over a natural, and a structure, a family and an indexed family over a proof, give the types one is spelled in; a match on a call's result, that call behind a definition and a match in another's arm give the guards that meet a second match's scrutinee only where one of the two is unfolded; and a guard whose spelling is a case of another's gives two equations an arm's solution restates together.
const DEFINITIONS: &str =
    "let count(@T: Type, l: List(T)) -> Nat = match l | [] => 0 | [_, ..t] => count(t) + 1 end;
let whole(@T: Type, l: List(T)) -> Bool = match l | [] => true | [_, ..t] => whole(t) end;
let all(@T: Type, l: List(T), p: (T) -> Bool) -> Bool = match l | [] => true | [h, ..t] => match p(h) | true => all(t, p) | false => false end end;
let small(n: Nat) -> Bool = n < 10;
let tiny(n: Nat) -> Bool = small(n);
let Naturals: Type = Nat;
induct Tag: (Bool) -> Type | yes(): (true) end
struct Pair: Type { left: Nat, right: Nat }
let lemma(a: Nat, c: Nat, p: Holds(a < 10)) -> Holds(a < 10) = match c | 0 => p | _ => p end;
let nonzero(b: Nat, c: Nat, p: Nat/NonZero(b)) -> Nat/NonZero(b) = match c | 0 => p | _ => p end;
struct Bounded: Type { n: Nat, at: Holds(n < 10) }
induct Checked: Type | checked(n: Nat, at: Holds(n < 10)) end
struct Box(n: Nat): Type { at: Nat }
struct Within(n: Nat, at: Holds(n < 10)): Type { k: Nat }
induct Pinned(n: Nat, at: Holds(n < 10)): Type | pinned() end
induct Proven: (n: Nat, at: Holds(n < 10)) -> Type end
let pick(e: (Nat) -> Bool, k: (Nat) -> Bool, c: Nat) -> Bool = match e(c) | true => k(0) | false => k(1) end;
let test(e: (Nat) -> Bool, c: Nat) -> Bool = e(c);
let chain(e: (Nat) -> Bool, r: (Nat) -> Bool, k: (Nat) -> Bool, c: Nat) -> Bool = match e(c) | true => match r(c) | true => k(0) | false => k(1) end | false => k(2) end;
let first(e: (Nat) -> Bool, x: Nat, c: Nat) -> Bool = e(x + c);
let second(e: (Nat) -> Bool, r: (Nat) -> Bool, x: Nat, c: Nat) -> Bool = match first(e, x, c) | true => r(c) | false => false end;";

/// The binders every cell is stated under. Two proofs of one bound and a function proving it, two functions and a higher-order one give the atoms that convert without being identical; functions of a type, of a proposition and of a structure, a type family, and a structure's value give the types and the literals a form is spelled with.
const BINDERS: &[Binder] = &[
    Binder {
        name: "a",
        type_: "Nat",
        needs: &[],
    },
    Binder {
        name: "b",
        type_: "Nat",
        needs: &[],
    },
    Binder {
        name: "c",
        type_: "Nat",
        needs: &[],
    },
    Binder {
        name: "f",
        type_: "(Nat) -> Nat",
        needs: &[],
    },
    Binder {
        name: "w",
        type_: "(n: Nat, at: Holds(n < 10)) -> Nat",
        needs: &[],
    },
    Binder {
        name: "p1",
        type_: "Holds(a < 10)",
        needs: &["a"],
    },
    Binder {
        name: "p2",
        type_: "Holds(a < 10)",
        needs: &["a"],
    },
    Binder {
        name: "h",
        type_: "(Nat) -> Holds(a < 10)",
        needs: &["a"],
    },
    Binder {
        name: "q",
        type_: "Nat/NonZero(b)",
        needs: &["b"],
    },
    Binder {
        name: "s",
        type_: "(Bounded) -> Nat",
        needs: &[],
    },
    Binder {
        name: "t",
        type_: "(Checked) -> Nat",
        needs: &[],
    },
    Binder {
        name: "u",
        type_: "((Nat) -> Nat) -> Nat",
        needs: &[],
    },
    Binder {
        name: "i",
        type_: "Int",
        needs: &[],
    },
    Binder {
        name: "j",
        type_: "Int",
        needs: &[],
    },
    Binder {
        name: "v",
        type_: "(Int) -> Int",
        needs: &[],
    },
    Binder {
        name: "xs",
        type_: "List(Nat)",
        needs: &[],
    },
    Binder {
        name: "flag",
        type_: "Bool",
        needs: &[],
    },
    Binder {
        name: "g",
        type_: "(Bool) -> Nat",
        needs: &[],
    },
    Binder {
        name: "e",
        type_: "(Nat) -> Bool",
        needs: &[],
    },
    Binder {
        name: "k",
        type_: "(Nat) -> Bool",
        needs: &[],
    },
    Binder {
        name: "ft",
        type_: "(Type) -> Nat",
        needs: &[],
    },
    Binder {
        name: "F",
        type_: "(Nat) -> Type",
        needs: &[],
    },
    Binder {
        name: "fp",
        type_: "(Prop) -> Nat",
        needs: &[],
    },
    Binder {
        name: "bx",
        type_: "Box(c)",
        needs: &["c"],
    },
    Binder {
        name: "gb",
        type_: "(Box(c)) -> Nat",
        needs: &["c"],
    },
    Binder {
        name: "pv",
        type_: "(Pinned(a, p1)) -> Nat",
        needs: &["a", "p1"],
    },
    Binder {
        name: "r",
        type_: "(Nat) -> Bool",
        needs: &[],
    },
    Binder {
        name: "m",
        type_: "(Bool) -> Bool",
        needs: &[],
    },
];

/// How many orders of [`BINDERS`] the held cells are stated at. A refused cell is stated at the first two, the declared order and its reverse.
const ORDERS: usize = 8;

/// What the compiler answers of one cell.
#[derive(Clone, Copy, Debug, PartialEq)]
enum Answer {
    Held,
    /// Refused by the elaborator, so the certifier is not asked.
    Refused,
    /// Accepted by the elaborator and refused by the certifier.
    Certifier,
}

/// One cell: the row and column it stands at, the item's result and body, and the side it is stated on.
struct Cell {
    at: String,
    result: String,
    body: String,
    stated: Answer,
}

/// A guard and a respelling of it, with the respelling's dual where the guard is an ordering.
struct Guard {
    guard: &'static str,
    respelled: &'static str,
    dual: Option<&'static str>,
    refused: &'static [Arm],
}

/// The positions a guard's respelling is put in.
#[derive(Clone, Copy, Debug, PartialEq)]
enum Arm {
    /// The control: the two spellings convert at the top of an item.
    Top,
    /// `Holds` of the respelling, in the guard's `true` arm, by `True/qed()`.
    Qed,
    /// The same, by `proved()`.
    Proved,
    /// A `match` on the respelling in a term, in the guard's `true` arm.
    Term,
    /// A `match` on the respelling in a type, in the guard's `true` arm.
    Type,
    /// `Holds` of the respelling's dual, in the guard's `false` arm.
    Dual,
    /// A value of a family indexed by the respelling, taken apart by a match with no arm in the guard's `false` arm: inversion reads the index as the guard's case, which rules out the family's one case at `true`.
    Index,
}

const ARMS: &[Arm] = &[
    Arm::Top,
    Arm::Qed,
    Arm::Proved,
    Arm::Term,
    Arm::Type,
    Arm::Dual,
    Arm::Index,
];

const GUARDS: &[Guard] = &[
    Guard {
        guard: "a + b < 10",
        respelled: "b + a < 10",
        dual: Some("10 <= b + a"),
        refused: &[],
    },
    Guard {
        guard: "a + b + c < 10",
        respelled: "c + a + b < 10",
        dual: Some("10 <= c + a + b"),
        refused: &[],
    },
    Guard {
        guard: "a + b < c + 10",
        respelled: "b + a < 10 + c",
        dual: Some("10 + c <= b + a"),
        refused: &[],
    },
    // A bare truncated difference as a whole side, read across by the adjunction: `a - b <= 3` is `a <= b + 3`, and its dual `3 < a - b` is `b + 3 < a`.
    Guard {
        guard: "a - b <= 3",
        respelled: "a <= b + 3",
        dual: Some("b + 3 < a"),
        refused: &[],
    },
    Guard {
        guard: "a == b",
        respelled: "b == a",
        dual: None,
        refused: &[],
    },
    Guard {
        guard: "f(a + b) < 10",
        respelled: "f(b + a) < 10",
        dual: Some("10 <= f(b + a)"),
        refused: &[],
    },
    Guard {
        guard: "w(a, p1) < 5",
        respelled: "w(a, p2) < 5",
        dual: Some("5 <= w(a, p2)"),
        refused: &[],
    },
    Guard {
        guard: "u((x) => x + a) < 5",
        respelled: "u((x) => a + x) < 5",
        dual: Some("5 <= u((x) => a + x)"),
        refused: &[],
    },
    Guard {
        guard: "Nat/in_range(a + b, 1, 5)",
        respelled: "Nat/in_range(b + a, 1, 5)",
        dual: None,
        refused: &[],
    },
    // One call written on each side, apart in a universe instance: a `Bool`'s readers hold the two one atom, as a number's do.
    Guard {
        guard: "whole(xs) == flag",
        respelled: "flag == whole(xs)",
        dual: None,
        refused: &[],
    },
    // An operation one reader reads, respelled under the comparison another reads: the guard's atom is the bitwise operation, which its own reader holds one with its operands swapped.
    Guard {
        guard: "Nat/and(a, b) < 10",
        respelled: "Nat/and(b, a) < 10",
        dual: Some("10 <= Nat/and(b, a)"),
        refused: &[],
    },
    Guard {
        guard: "Nat/and(a + b, c) == f(a)",
        respelled: "f(a) == Nat/and(c, b + a)",
        dual: None,
        refused: &[],
    },
    // A stuck call of a recursive definition, its implicit written on one side and left out on the other: the two calls are apart in their instance, and are asked about by their arguments.
    Guard {
        guard: "whole(@Nat, xs)",
        respelled: "whole(xs)",
        dual: None,
        refused: &[],
    },
    Guard {
        guard: "whole(xs)",
        respelled: "whole(@Nat, xs)",
        dual: None,
        refused: &[],
    },
    // Arguments respelled with no implicit taking part: through an alias, an eta expansion and a wrapper.
    Guard {
        guard: "whole(@Naturals, xs)",
        respelled: "whole(xs)",
        dual: None,
        refused: &[],
    },
    Guard {
        guard: "all(xs, (n) => small(n))",
        respelled: "all(xs, small)",
        dual: None,
        refused: &[],
    },
    Guard {
        guard: "all(xs, tiny)",
        respelled: "all(xs, small)",
        dual: None,
        refused: &[],
    },
    // A binder the guard does not name, where conversion does not read it: `c` stands where reducing an argument erases it, in an operand and in a projection, and inside a proof, an application of the proof binder `h`. The filter in front of an equation reads past each as conversion does.
    Guard {
        guard: "f(b) < 10",
        respelled: "f(b + 0 * c) < 10",
        dual: Some("10 <= f(b + 0 * c)"),
        refused: &[],
    },
    Guard {
        guard: "f(b) < 10",
        respelled: "f(Pair { left = b, right = c }.left) < 10",
        dual: Some("10 <= f(Pair { left = b, right = c }.left)"),
        refused: &[],
    },
    Guard {
        guard: "w(a, p1) < 5",
        respelled: "w(a, h(c)) < 5",
        dual: Some("5 <= w(a, h(c))"),
        refused: &[],
    },
    // A binder the guard does not name inside a proof stuck on it, standing where conversion compares a proposition and reads nowhere: a function's bounded argument, a struct literal's bounded field, a constructor's bounded payload and a division's bound.
    Guard {
        guard: "w(a, p1) < 5",
        respelled: "w(a, lemma(a, c, p1)) < 5",
        dual: Some("5 <= w(a, lemma(a, c, p1))"),
        refused: &[],
    },
    Guard {
        guard: "s(Bounded { n = a, at = p1 }) < 5",
        respelled: "s(Bounded { n = a, at = lemma(a, c, p1) }) < 5",
        dual: Some("5 <= s(Bounded { n = a, at = lemma(a, c, p1) })"),
        refused: &[],
    },
    Guard {
        guard: "t(Checked/checked(a, p1)) < 5",
        respelled: "t(Checked/checked(a, lemma(a, c, p1))) < 5",
        dual: Some("5 <= t(Checked/checked(a, lemma(a, c, p1)))"),
        refused: &[],
    },
    Guard {
        guard: "Nat/div(a, b, @q) < 5",
        respelled: "Nat/div(a, b, @nonzero(b, c, q)) < 5",
        dual: Some("5 <= Nat/div(a, b, @nonzero(b, c, q))"),
        refused: &[],
    },
    // A binder the guard does not name beneath a binder of the form's or in a type: a lambda's body, a proof beneath a lambda, a function type's domain, an inductive type's index, a structure type's parameter, and a struct literal's parameter, which eta reads nowhere. The filter in front of an equation passes what conversion compares only in step and reduces the rest, as conversion does.
    Guard {
        guard: "u((x) => x) < 10",
        respelled: "u((x) => x + 0 * c) < 10",
        dual: Some("10 <= u((x) => x + 0 * c)"),
        refused: &[],
    },
    Guard {
        guard: "u((x) => Nat/div(x, b, @q)) < 5",
        respelled: "u((x) => Nat/div(x, b, @nonzero(b, c, q))) < 5",
        dual: Some("5 <= u((x) => Nat/div(x, b, @nonzero(b, c, q)))"),
        refused: &[],
    },
    Guard {
        guard: "ft((y: F(b)) -> Nat) < 5",
        respelled: "ft((y: F(b + 0 * c)) -> Nat) < 5",
        dual: Some("5 <= ft((y: F(b + 0 * c)) -> Nat)"),
        refused: &[],
    },
    Guard {
        guard: "fp(Eq()(b, b)) < 5",
        respelled: "fp(Eq()(b + 0 * c, b)) < 5",
        dual: Some("5 <= fp(Eq()(b + 0 * c, b))"),
        refused: &[],
    },
    Guard {
        guard: "ft(Box(b)) < 5",
        respelled: "ft(Box(b + 0 * c)) < 5",
        dual: Some("5 <= ft(Box(b + 0 * c))"),
        refused: &[],
    },
    Guard {
        guard: "gb(bx) < 5",
        respelled: "gb(Box { at = bx.at }) < 5",
        dual: Some("5 <= gb(Box { at = bx.at })"),
        refused: &[],
    },
    // A function type's codomain beneath its own binder, which conversion compares in step, the two binders opened as one.
    Guard {
        guard: "ft((y: Nat) -> F(y + b)) < 5",
        respelled: "ft((y: Nat) -> F(y + b + 0 * c)) < 5",
        dual: Some("5 <= ft((y: Nat) -> F(y + b + 0 * c))"),
        refused: &[],
    },
    // A binder the guard does not name inside a proof at a type's parameter or index, or at a constructor's parameter, which conversion compares at a proposition and reads nowhere.
    Guard {
        guard: "ft(Within(a, p1)) < 5",
        respelled: "ft(Within(a, lemma(a, c, p1))) < 5",
        dual: Some("5 <= ft(Within(a, lemma(a, c, p1)))"),
        refused: &[],
    },
    Guard {
        guard: "ft(Pinned(a, p1)) < 5",
        respelled: "ft(Pinned(a, lemma(a, c, p1))) < 5",
        dual: Some("5 <= ft(Pinned(a, lemma(a, c, p1)))"),
        refused: &[],
    },
    Guard {
        guard: "ft(Proven(a, p1)) < 5",
        respelled: "ft(Proven(a, lemma(a, c, p1))) < 5",
        dual: Some("5 <= ft(Proven(a, lemma(a, c, p1)))"),
        refused: &[],
    },
    Guard {
        guard: "pv(Pinned/pinned(@a, @p1)) < 5",
        respelled: "pv(Pinned/pinned(@a, @lemma(a, c, p1))) < 5",
        dual: Some("5 <= pv(Pinned/pinned(@a, @lemma(a, c, p1)))"),
        refused: &[],
    },
];

/// A guard and a form apart from it past every reduction, with the form's dual: no conversion holds the two equal, so no arm of the guard answers the form, however the filter in front of an equation reads it.
struct Apart {
    guard: &'static str,
    apart: &'static str,
    dual: &'static str,
}

/// Near misses of the rows above whose binder stands beneath a binder of the form's or in a type: a lambda's body, a proof's neighbour beneath a lambda, a function type's domain, an inductive type's index, a structure type's parameter and a function type's codomain beneath its binder, each apart from the guard.
const APART: &[Apart] = &[
    Apart {
        guard: "u((x) => x) < 10",
        apart: "u((x) => x + c) < 10",
        dual: "10 <= u((x) => x + c)",
    },
    Apart {
        guard: "u((x) => Nat/div(x, b, @q)) < 5",
        apart: "u((x) => Nat/div(x + c, b, @q)) < 5",
        dual: "5 <= u((x) => Nat/div(x + c, b, @q))",
    },
    Apart {
        guard: "ft((y: F(b)) -> Nat) < 5",
        apart: "ft((y: F(b + c)) -> Nat) < 5",
        dual: "5 <= ft((y: F(b + c)) -> Nat)",
    },
    Apart {
        guard: "fp(Eq()(b, b)) < 5",
        apart: "fp(Eq()(b + c, b)) < 5",
        dual: "5 <= fp(Eq()(b + c, b))",
    },
    Apart {
        guard: "ft(Box(b)) < 5",
        apart: "ft(Box(b + c)) < 5",
        dual: "5 <= ft(Box(b + c))",
    },
    Apart {
        guard: "ft((y: Nat) -> F(y + b)) < 5",
        apart: "ft((y: Nat) -> F(y + b + c)) < 5",
        dual: "5 <= ft((y: Nat) -> F(y + b + c))",
    },
];

impl Apart {
    /// The form stated true in the guard's `true` arm, and its dual in the `false` arm: each refused.
    fn cells(&self) -> Vec<Cell> {
        let Apart { guard, apart, dual } = self;
        [
            format!(
                "match {guard} | true => let _: Holds({apart}) = True/qed(); () | false => () end"
            ),
            format!(
                "match {guard} | true => () | false => let _: Holds({dual}) = True/qed(); () end"
            ),
        ]
        .into_iter()
        .map(|body| Cell {
            at: format!("`{guard}` beside `{apart}`"),
            result: "{}".to_owned(),
            body,
            stated: Answer::Refused,
        })
        .collect()
    }
}

impl Guard {
    fn cells(&self) -> Vec<Cell> {
        let Guard {
            guard, respelled, ..
        } = self;
        ARMS.iter()
            .filter_map(|arm| {
                let in_true = |fact: String| {
                    (
                        "{}".to_owned(),
                        format!("match {guard} | true => {fact} () | false => () end"),
                    )
                };
                let (result, body) = match arm {
                    Arm::Top => (
                        format!("Eq()({guard}, {respelled})"),
                        "Eq/refl()".to_owned(),
                    ),
                    Arm::Qed => in_true(format!("let _: Holds({respelled}) = True/qed();")),
                    Arm::Proved => in_true(format!("let _: Holds({respelled}) = proved();")),
                    Arm::Term => in_true(format!(
                        "let _: Eq()(match {respelled} | true => 0 | false => 1 end, 0) = Eq/refl();"
                    )),
                    Arm::Type => in_true(fill!("let _: (match ${respelled} | true => {} | false => Nat end) = ();", respelled)),
                    Arm::Dual => {
                        let dual = self.dual?;
                        (
                            "{}".to_owned(),
                            format!(
                                "match {guard} | true => () | false => let _: Holds({dual}) = True/qed(); () end"
                            ),
                        )
                    }
                    Arm::Index => (
                        "{}".to_owned(),
                        fill!(
                            "match ${guard} | true => () | false => let _: (Tag(${respelled})) -> {} = (x) => match x end; () end",
                            guard,
                            respelled,
                        ),
                    ),
                };
                Some(Cell {
                    at: format!("`{guard}` respelled `{respelled}`, {arm:?}"),
                    result,
                    body,
                    stated: match self.refused.contains(arm) {
                        true => Answer::Refused,
                        false => Answer::Held,
                    },
                })
            })
            .collect()
    }
}

/// Two terms of one carrier that convert without being identical.
struct Atoms {
    left: &'static str,
    right: &'static str,
    /// The carrier's zero, and whether the carrier is `Nat`, whose `match` takes a literal arm.
    zero: &'static str,
    nat: bool,
    refused: &'static [Fold],
}

/// The positions two atoms that convert are put in.
#[derive(Clone, Copy, Debug, PartialEq)]
enum Fold {
    /// The control: the two atoms convert at the top of an item.
    Top,
    /// Their `==` against `true`.
    Equal,
    /// Their `<=` against `true`.
    AtMost,
    /// Their difference against zero.
    Difference,
    /// A `match` on their `==` against its `true` arm.
    Match,
    /// The `0` arm of a `match` on one, holding the other equal to `0`.
    Switch,
}

const FOLDS: &[Fold] = &[
    Fold::Top,
    Fold::Equal,
    Fold::AtMost,
    Fold::Difference,
    Fold::Match,
    Fold::Switch,
];

const ATOMS: &[Atoms] = &[
    Atoms {
        left: "f(a + b)",
        right: "f(b + a)",
        zero: "0",
        nat: true,
        refused: &[],
    },
    Atoms {
        left: "w(a, p1)",
        right: "w(a, p2)",
        zero: "0",
        nat: true,
        refused: &[],
    },
    Atoms {
        left: "u((x) => x + a)",
        right: "u((x) => a + x)",
        zero: "0",
        nat: true,
        refused: &[],
    },
    Atoms {
        left: "v(i + j)",
        right: "v(j + i)",
        zero: "+0",
        nat: false,
        refused: &[],
    },
    // Two spellings of one operation, which its own reader holds one and no checker is asked about.
    Atoms {
        left: "Nat/and(a, b)",
        right: "Nat/and(b, a)",
        zero: "0",
        nat: true,
        refused: &[],
    },
    Atoms {
        left: "Nat/and(a + b, c)",
        right: "Nat/and(c, b + a)",
        zero: "0",
        nat: true,
        refused: &[],
    },
    Atoms {
        left: "Int/and(i, j)",
        right: "Int/and(j, i)",
        zero: "+0",
        nat: false,
        refused: &[],
    },
    // A magnitude over two spellings of one call, which hash apart: its fold leads its operand by the order of its atoms, and is taken again over the classes.
    Atoms {
        left: "Int/abs(Nat/to_int(f(a + b)) - Nat/to_int(c))",
        right: "Int/abs(Nat/to_int(c) - Nat/to_int(f(b + a)))",
        zero: "0",
        nat: true,
        refused: &[],
    },
    Atoms {
        left: "Int/sign(v(i + j) - i)",
        right: "Int/sign(v(j + i) - i)",
        zero: "+0",
        nat: false,
        refused: &[],
    },
    // One call written twice: two instances of a universe-polymorphic definition, which a number's readers hold one atom.
    Atoms {
        left: "count(xs)",
        right: "count(xs)",
        zero: "0",
        nat: true,
        refused: &[],
    },
    // The same call with its implicit written on one side: a match on it is a stuck call met whole by its equation.
    Atoms {
        left: "count(@Nat, xs)",
        right: "count(xs)",
        zero: "0",
        nat: true,
        refused: &[],
    },
];

impl Atoms {
    fn cells(&self) -> Vec<Cell> {
        let Atoms {
            left, right, zero, ..
        } = self;
        FOLDS
            .iter()
            .filter_map(|fold| {
                let claim = |claim: String| (claim, "Eq/refl()".to_owned());
                let (result, body) = match fold {
                    Fold::Top => claim(format!("Eq()({left}, {right})")),
                    Fold::Equal => claim(format!("Eq()({left} == {right}, true)")),
                    Fold::AtMost => claim(format!("Eq()({left} <= {right}, true)")),
                    Fold::Difference => claim(format!("Eq()({left} - {right}, {zero})")),
                    Fold::Match => claim(format!(
                        "Eq()(match {left} == {right} | true => 0 | false => 1 end, 0)"
                    )),
                    Fold::Switch => {
                        if !self.nat {
                            return None;
                        }
                        (
                            "{}".to_owned(),
                            format!(
                                "match {left} | 0 => let _: Eq()({right}, 0) = Eq/refl(); () | _ => () end"
                            ),
                        )
                    }
                };
                Some(Cell {
                    at: format!("`{left}` and `{right}`, {fold:?}"),
                    result,
                    body,
                    stated: match self.refused.contains(fold) {
                        true => Answer::Refused,
                        false => Answer::Held,
                    },
                })
            })
            .collect()
    }
}

/// A guard and a second match, inside the guard's `true` arm, on a variable the guard names or on an expression it reads.
struct Substitution {
    guard: &'static str,
    variable: &'static str,
    /// The arm of the second match the fact is stated in, and the arm beside it.
    case: &'static str,
    other: &'static str,
    /// The guard with the case's value written for the variable.
    at_case: &'static str,
    refused: &'static [Under],
    certifier: &'static [Under],
}

/// The positions a guard's fact is put in under a second match.
#[derive(Clone, Copy, Debug, PartialEq)]
enum Under {
    /// The control: the two matches nested the other way, the fact as written.
    Outside,
    /// The fact as written, by `True/qed()`.
    Qed,
    /// The fact as written, by `proved()`.
    Proved,
    /// The fact with the case's value written, by `True/qed()`.
    AtCase,
    /// The control of the cell before it: the two matches nested the other way, the fact with the case's value written.
    Reversed,
}

const UNDERS: &[Under] = &[
    Under::Outside,
    Under::Qed,
    Under::Proved,
    Under::AtCase,
    Under::Reversed,
];

const SUBSTITUTIONS: &[Substitution] = &[
    Substitution {
        guard: "a < List/length(xs)",
        variable: "a",
        case: "0",
        other: "_",
        at_case: "0 < List/length(xs)",
        refused: &[],
        certifier: &[],
    },
    Substitution {
        guard: "a + b < 10",
        variable: "b",
        case: "0",
        other: "_",
        at_case: "a + 0 < 10",
        refused: &[],
        certifier: &[],
    },
    Substitution {
        guard: "g(flag) < 5",
        variable: "flag",
        case: "true",
        other: "false",
        at_case: "g(true) < 5",
        refused: &[],
        certifier: &[],
    },
    // A second match on an expression the guard reads: the guard's spelling, settled outside it, is the stuck match on `e(c)`, settled again under the arm on `e(c)` it is the case's, and its fact at the case is `k(0)`.
    Substitution {
        guard: "(match e(c) | true => k(0) | false => k(1) end)",
        variable: "e(c)",
        case: "true",
        other: "false",
        at_case: "k(0)",
        refused: &[],
        certifier: &[],
    },
    // The same guard behind a definition, at either case: its spelling as written names no `e(c)`, and settled again under the arm it is the case's all the same.
    Substitution {
        guard: "pick(e, k, c)",
        variable: "e(c)",
        case: "true",
        other: "false",
        at_case: "k(0)",
        refused: &[],
        certifier: &[],
    },
    Substitution {
        guard: "pick(e, k, c)",
        variable: "e(c)",
        case: "false",
        other: "true",
        at_case: "k(1)",
        refused: &[],
        certifier: &[],
    },
    // The second match's scrutinee behind a definition: the guard's spelling holds `e(c)` and no `test(e, c)`, and the arm's equation answers that `e(c)` once `test` is unfolded. The fact as written holds by the same settlement: what reduction leaves of it is `k(0)`, the guard's spelling under the arm.
    Substitution {
        guard: "(match e(c) | true => k(0) | false => k(1) end)",
        variable: "test(e, c)",
        case: "true",
        other: "false",
        at_case: "k(0)",
        refused: &[],
        certifier: &[],
    },
    // The guard's call in an argument, which conversion compares whole: the fact is no operation of the algebra, whose atoms are classed with every equation in force.
    Substitution {
        guard: "m(pick(e, k, c))",
        variable: "e(c)",
        case: "true",
        other: "false",
        at_case: "m(k(0))",
        refused: &[],
        certifier: &[],
    },
    // The second match's scrutinee respelled, with nothing unfolded: the guard's spelling holds `a + b < 10` and no `b + a < 10`, which the arm's equation answers as the readers hold the two one.
    Substitution {
        guard: "(match a + b < 10 | true => k(0) | false => k(1) end)",
        variable: "b + a < 10",
        case: "true",
        other: "false",
        at_case: "k(0)",
        refused: &[],
        certifier: &[],
    },
];

/// A cell no row's columns state: a guard under two matches, one inside the other, and a fact no arm's equation reaches.
struct Nest {
    at: &'static str,
    body: &'static str,
    stated: Answer,
}

const NESTS: &[Nest] = &[
    // A guard that meets one arm's scrutinee and, in that case, another's: settled again under both, with each arm's equation in view, it is the innermost case, in either order; the guard innermost is the control.
    Nest {
        at: "`chain(e, r, k, c)` under a match on `r(c)` and one on `e(c)` inside it",
        body: "match chain(e, r, k, c) | true => (match r(c) | true => (match e(c) | true => let _: Holds(k(0)) = True/qed(); () | false => () end) | false => () end) | false => () end",
        stated: Answer::Held,
    },
    Nest {
        at: "`chain(e, r, k, c)` under a match on `e(c)` and one on `r(c)` inside it",
        body: "match chain(e, r, k, c) | true => (match e(c) | true => (match r(c) | true => let _: Holds(k(0)) = True/qed(); () | false => () end) | false => () end) | false => () end",
        stated: Answer::Held,
    },
    Nest {
        at: "`chain(e, r, k, c)` inside a match on `r(c)` and one on `e(c)`",
        body: "match r(c) | true => (match e(c) | true => (match chain(e, r, k, c) | true => let _: Holds(k(0)) = True/qed(); () | false => () end) | false => () end) | false => () end",
        stated: Answer::Held,
    },
    // A guard that meets one arm's scrutinee, under that arm and one that says nothing of it, in either order: settled again at the inner arm's height, its spelling still has the outer arm's equation in view.
    Nest {
        at: "`pick(e, k, c)` under a match on `r(c)` and one on `e(c)` inside it",
        body: "match pick(e, k, c) | true => (match r(c) | true => (match e(c) | true => let _: Holds(k(0)) = True/qed(); () | false => () end) | false => () end) | false => () end",
        stated: Answer::Held,
    },
    Nest {
        at: "`pick(e, k, c)` under a match on `e(c)` and one on `r(c)` inside it",
        body: "match pick(e, k, c) | true => (match e(c) | true => (match r(c) | true => let _: Holds(k(0)) = True/qed(); () | false => () end) | false => () end) | false => () end",
        stated: Answer::Held,
    },
    // The near misses, which no reading of the two equations holds: the other case's fact, an arm on another expression, and the guard's other arm.
    Nest {
        at: "`pick(e, k, c)` under a match on `e(c)`, the fact of the case beside",
        body: "match pick(e, k, c) | true => (match e(c) | true => () | false => let _: Holds(k(0)) = True/qed(); () end) | false => () end",
        stated: Answer::Refused,
    },
    Nest {
        at: "`pick(e, k, c)` under a match on `e(a)`",
        body: "match pick(e, k, c) | true => (match e(a) | true => let _: Holds(k(0)) = True/qed(); () | false => () end) | false => () end",
        stated: Answer::Refused,
    },
    Nest {
        at: "`pick(e, k, c)` at `false` under a match on `e(c)`",
        body: "match pick(e, k, c) | true => () | false => (match e(c) | true => let _: Holds(k(0)) = True/qed(); () | false => () end) end",
        stated: Answer::Refused,
    },
    // Two guards over `a`, the second's spelling a case of the first's, which an arm on `a` restates together at `0`, one equation each in the arm's frame. Under an arm on an expression inside, the second is settled again with the first's instance in view, its frame's other equation: the view withholds the one equation being settled, not its frame.
    Nest {
        at: "`second(e, r, a, c)` under `first(e, a, c)`, both restated at `a` = 0, under a match on `k(5)`",
        body: "match first(e, a, c) | true => (match second(e, r, a, c) | true => (match a | 0 => (match k(5) | true => let _: Holds(r(c)) = True/qed(); () | false => () end) | _ => () end) | false => () end) | false => () end",
        stated: Answer::Held,
    },
    // The same with no arm on an expression inside, in either nesting: whichever instance the arm registers first is read past its own height, settled again with the other in view, and the one registered last is settled with the one before it in view, as the certifier settles an equation with those recorded before it.
    Nest {
        at: "`second(e, r, a, c)` under `first(e, a, c)`, both restated at `a` = 0",
        body: "match first(e, a, c) | true => (match second(e, r, a, c) | true => (match a | 0 => let _: Holds(r(c)) = True/qed(); () | _ => () end) | false => () end) | false => () end",
        stated: Answer::Held,
    },
    Nest {
        at: "`first(e, a, c)` under `second(e, r, a, c)`, both restated at `a` = 0",
        body: "match second(e, r, a, c) | true => (match first(e, a, c) | true => (match a | 0 => let _: Holds(r(c)) = True/qed(); () | _ => () end) | false => () end) | false => () end",
        stated: Answer::Held,
    },
    // A second match on the guard's own scrutinee, in the arm no value reaches: the arm's own equation answers before the guard's, so the arm holds its case. The proofs the elaborator writes for a bound rest on it, splitting a conjunction by a match on a comparison an arm around it decided.
    Nest {
        at: "`+0 <= i` at `false` under a second match on `+0 <= i`, its `true` arm",
        body: "match +0 <= i | false => (match +0 <= i | true => let _: Holds(+0 <= i) = True/qed(); () | false => () end) | true => () end",
        stated: Answer::Held,
    },
];

impl Nest {
    fn cell(&self) -> Cell {
        Cell {
            at: self.at.to_owned(),
            result: "{}".to_owned(),
            body: self.body.to_owned(),
            stated: self.stated,
        }
    }
}

impl Substitution {
    fn cells(&self) -> Vec<Cell> {
        let Substitution {
            guard,
            variable,
            case,
            other,
            at_case,
            ..
        } = self;
        UNDERS
            .iter()
            .map(|under| {
                let inside = |fact: &str, proof: &str| {
                    format!(
                        "match {guard} | true => (match {variable} | {case} => let _: Holds({fact}) = {proof}; () | {other} => () end) | false => () end"
                    )
                };
                let body = match under {
                    Under::Outside => format!(
                        "match {variable} | {case} => (match {guard} | true => let _: Holds({guard}) = True/qed(); () | false => () end) | {other} => () end"
                    ),
                    Under::Qed => inside(guard, "True/qed()"),
                    Under::Proved => inside(guard, "proved()"),
                    Under::AtCase => inside(at_case, "True/qed()"),
                    Under::Reversed => format!(
                        "match {variable} | {case} => (match {guard} | true => let _: Holds({at_case}) = True/qed(); () | false => () end) | {other} => () end"
                    ),
                };
                Cell {
                    at: format!("`{guard}` under a match on `{variable}`, {under:?}"),
                    result: "{}".to_owned(),
                    body,
                    stated: match (self.certifier.contains(under), self.refused.contains(under)) {
                        (true, _) => Answer::Certifier,
                        (false, true) => Answer::Refused,
                        (false, false) => Answer::Held,
                    },
                }
            })
            .collect()
    }
}

/// One program stating each cell as an item under `binders`.
fn program(binders: &str, cells: &[&Cell]) -> String {
    let items = cells
        .iter()
        .enumerate()
        .map(|(index, cell)| {
            format!(
                "let cell{index}({binders}) -> {} = {};",
                cell.result, cell.body
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    format!("{IMPORTS}\n{DEFINITIONS}\n{items}\nIo/pure(())")
}

/// What the compiler answers of `cell` alone under `binders`.
fn answer(binders: &str, cell: &Cell) -> Answer {
    match typecheck(&program(binders, &[cell])) {
        Ok(()) => Answer::Held,
        Err(error) if error.contains("the certifier refused") => Answer::Certifier,
        Err(_) => Answer::Refused,
    }
}

/// The cells the compiler puts on another side than stated, each asked alone at the declared order of the binders and at its reverse.
fn misplaced(cells: &[Cell]) -> Vec<String> {
    let mut misplaced = Vec::new();
    for binders in orders(BINDERS, 2) {
        for cell in cells {
            let answered = answer(&binders, cell);
            if answered != cell.stated {
                misplaced.push(format!(
                    "{}: stated {:?}, answered {answered:?}",
                    cell.at, cell.stated
                ));
            }
        }
    }
    misplaced
}

#[test]
fn every_respelled_guard_is_on_the_side_stated() {
    let cells = GUARDS.iter().flat_map(Guard::cells).collect::<Vec<_>>();
    let misplaced = misplaced(&cells);
    assert!(misplaced.is_empty(), "{}", misplaced.join("\n"));
}

#[test]
fn a_form_apart_from_its_guard_past_every_reduction_is_answered_by_no_arm() {
    let cells = APART.iter().flat_map(Apart::cells).collect::<Vec<_>>();
    let misplaced = misplaced(&cells);
    assert!(misplaced.is_empty(), "{}", misplaced.join("\n"));
}

#[test]
fn every_fold_over_two_atoms_that_convert_is_on_the_side_stated() {
    let cells = ATOMS.iter().flat_map(Atoms::cells).collect::<Vec<_>>();
    let misplaced = misplaced(&cells);
    assert!(misplaced.is_empty(), "{}", misplaced.join("\n"));
}

#[test]
fn every_guard_under_a_match_on_its_variable_is_on_the_side_stated() {
    let cells = SUBSTITUTIONS
        .iter()
        .flat_map(Substitution::cells)
        .collect::<Vec<_>>();
    let misplaced = misplaced(&cells);
    assert!(misplaced.is_empty(), "{}", misplaced.join("\n"));
}

#[test]
fn every_guard_under_matches_nested_in_its_arm_is_on_the_side_stated() {
    let cells = NESTS.iter().map(Nest::cell).collect::<Vec<_>>();
    let misplaced = misplaced(&cells);
    assert!(misplaced.is_empty(), "{}", misplaced.join("\n"));
}

/// A comparison guard's case equation answers the order-dual of its scrutinee only where the carrier is totally ordered. In the `false` arm of `match a + b < c`, the equation `(a + b < c) = false` answers `c <= b + a` as `true` — the reader that takes a guard's dual (`curios_analysis::answers`, through the negation `!(x < y)`) reading it off the carrier. At `Nat` that dual holds, so the arm proves `Holds(c <= b + a)`; at `Flt` it must not, since `+nan.0 < 10.0` and `10.0 <= +nan.0` are both `false`, so reading the dual there would admit `Holds(10.0 <= e + d)` from a guard that says nothing of it, and `True/qed()` at `Holds(false)` is a step from `False`.
#[test]
fn a_comparison_guards_dual_is_answered_only_at_a_total_order() {
    let guarded = |imports: &str, binders: &str, dual: &str, less: &str| {
        fill!(
            r#"
            use /std/{${imports}, Io};
            use /std/Bool/{True, Holds};
            let cell(${binders}) -> {} =
                match ${less} | true => () | false => let _: Holds(${dual}) = True/qed(); () end;
            Io/pure(())
            "#,
            imports,
            binders,
            less,
            dual,
        )
    };

    // Nat is totally ordered: the dual of `<` is `<=` reversed, so the equation answers it and the arm holds.
    let nat = guarded("Nat", "a: Nat, b: Nat, c: Nat", "c <= b + a", "a + b < c");
    assert!(
        typecheck(&nat).is_ok(),
        "the dual of a `Nat` comparison guard is answered in its false arm",
    );

    // Flt is not: `!(x < y)` is not `y <= x` at a NaN, so the equation answers nothing of the dual and the arm is refused.
    let flt = guarded(
        "Flt",
        "d: Flt, e: Flt",
        "Flt/le(10.0, e + d)",
        "Flt/lt(d + e, 10.0)",
    );
    let refusal =
        typecheck(&flt).expect_err("the dual of a `Flt` comparison guard is not answered");
    assert!(
        refusal.contains("type mismatch") && !refusal.contains("the certifier refused"),
        "the elaborator refuses the Flt dual as a type mismatch, not the certifier: {refusal}",
    );
}

/// A guard that reads an expression only through the types of the binders it names is settled again under an arm on that expression: under `match e(c)` the binders `p` and `q` of `F(e(c))` are units, so the guard `h(p) == h(q)` is `true` and the guard's scrutinee `k(0)`, which the guard's spelling, settled outside the arm with `p` and `q` apart, is not. The control is an arm on an expression the guard does not read, `e(d)`, under which `k(0)` is no case the guard's equation reaches and the fact is refused where it is written.
#[test]
fn a_guard_reading_an_expression_through_its_binders_types_is_settled_again_under_an_arm_on_it() {
    let program = |inner: &str| {
        fill!(
            "use /std/{Nat, Bool, Io};
use /std/Bool/{Holds, True};
let F(b: Bool) -> Type = match b | true => {} | false => Nat end;
let cell(c: Nat, d: Nat, e: (Nat) -> Bool, k: (Nat) -> Bool, h: (F(e(c))) -> Nat, p: F(e(c)), q: F(e(c))) -> {} =
    match (match h(p) == h(q) | true => k(0) | false => k(1) end)
    | true => match ${inner} | true => let _: Holds(k(0)) = True/qed(); () | false => () end
    | false => ()
    end;
Io/pure(())",
            inner,
        )
    };

    assert_eq!(typecheck(&program("e(c)")), Ok(()));
    let refusal = typecheck(&program("e(d)")).expect_err("`k(0)` is no case of the guard there");
    assert!(
        refusal.contains("type mismatch") && !refusal.contains("the certifier refused"),
        "the elaborator refuses the fact where it is written: {refusal}",
    );
}

/// The definitions the index tests read: a predicate written two ways and a family indexed below a constructor.
const INDEXED: &str = r#"
use /std/{Nat, Bool, List, Option, Io};
let all(@T: Type, l: List(T), p: (T) -> Bool) -> Bool = match l | [] => true | [h, ..t] => match p(h) | true => all(t, p) | false => false end end;
let small(n: Nat) -> Bool = n < 10;
let tiny(n: Nat) -> Bool = small(n);
induct Nested: (Option(Bool)) -> Type | yes(): (Option/some(true)) end
"#;

/// An index is read as the guard's case below a constructor too: `some(all(xs, tiny))` against the one case's `some(true)` is taken apart by injectivity and its part read by the judgment's reduction, which the guard `all(xs, small)` at `false` answers, so the case is ruled out in both checkers.
#[test]
fn an_index_below_a_constructor_is_read_as_the_guards_case() {
    let program = fill!(
        "${definitions}let cell(xs: List(Nat), x: Nested(Option/some(all(xs, tiny)))) -> {} =
    match all(xs, small) | true => () | false => match x end end;
Io/pure(())",
        definitions = INDEXED,
    );
    assert_eq!(typecheck(&program), Ok(()));
}

/// An index the guard does not equal leaves its case possible: under `all(xs, small)` at `false`, `all(ys, small)` is no term the equation answers, so a match with no arm is refused where it is written.
#[test]
fn an_index_the_guard_does_not_equal_leaves_its_case_possible() {
    let program = fill!(
        "${definitions}let cell(xs: List(Nat), ys: List(Nat), x: Nested(Option/some(all(ys, small)))) -> {} =
    match all(xs, small) | true => () | false => match x end end;
Io/pure(())",
        definitions = INDEXED,
    );
    let refusal = typecheck(&program).expect_err("the case at `true` stays possible");
    assert!(
        refusal.contains("no arm matches") && !refusal.contains("the certifier refused"),
        "the elaborator refuses the match for its missing arm: {refusal}",
    );
}

#[test]
fn every_held_cell_holds_at_every_order_of_its_binders() {
    // The admitting direction at the whole sweep: one program per order holding every cell stated held, and on its failure one per cell, so the report names the cells an order moved.
    let cells = GUARDS
        .iter()
        .flat_map(Guard::cells)
        .chain(ATOMS.iter().flat_map(Atoms::cells))
        .chain(SUBSTITUTIONS.iter().flat_map(Substitution::cells))
        .chain(NESTS.iter().map(Nest::cell))
        .filter(|cell| cell.stated == Answer::Held)
        .collect::<Vec<_>>();
    let held = cells.iter().collect::<Vec<_>>();
    for (order, binders) in orders(BINDERS, ORDERS).into_iter().enumerate() {
        if typecheck(&program(&binders, &held)).is_ok() {
            continue;
        }
        let moved = cells
            .iter()
            .filter(|cell| answer(&binders, cell) != Answer::Held)
            .map(|cell| cell.at.clone())
            .collect::<Vec<_>>();
        panic!(
            "at order {order} of the binders these cells do not hold:\n{}",
            moved.join("\n")
        );
    }
}
