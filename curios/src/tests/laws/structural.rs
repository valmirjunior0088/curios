//! The seeds: one equation for each rule of conversion no law table states — beta, delta, zeta, iota at each elimination, eta at a function, a record, a struct and a unit, and irrelevance — and the near miss beside it.
//!
//! A seed is two sides at one type, under one list of binders. A held seed's right side is a neutral of that type, a binder or a call of one, so two held seeds at one type meet at it and chain, and a seed placed under a context keeps a side the context cannot reduce. A near miss differs from the held seed beside it in the one thing its rule must not let pass: a binder dropped, two components swapped, the other arm taken.
//!
//! No seed states a verdict. Which side a checker puts it on is read from each checker asked by itself, and a seed a checker puts on the other side is listed in the table of parted rows with the finding that holds why.

use super::{Answers, Audit, asked_alone, closes, hold_to_the_table, parted};

/// What a seed's program states ahead of its rows: its imports, the declarations the seeds are stated over, and the ones a context places a seed under — a struct over any type, and a definition and a recursive function that each hand an argument back from a match stuck on a count. `through`, `into` and `pair` eliminate a proof of `Zero`, so unfolding one leaves its proof a stuck elimination's scrutinee: what comparing two calls by their spines first is for.
pub(super) const STRUCTURAL: &str =
    "use /std/{Eq, Nat, Bool, List, Option, Io}; use /std/Bool/{Holds};
struct Record: pub Type { a: Nat, b: Nat }
struct Empty: pub Type {}
struct Unital: pub Type { unit: {}, proof: Holds(0 < 10) }
struct Bounded: pub Type { n: Nat, ok: Holds(n < 10) }
struct Box(A: Type): pub Type { held: A }
struct Holder: pub Type { member: Member, unit: {} }
and Member: pub Type { unit: {} }
induct Zero: (Nat) -> pub Prop
| zero(): (0)
end
induct Both: pub Type
| both(first: Holds(0 < 10), second: Holds(0 < 10))
end
let through(n: Nat, e: Zero(n)) -> Nat = match e | zero() => 1 end;
let into(n: Nat, e: Zero(n)) -> (Nat) -> Nat = match e: (_, _) => (Nat) -> Nat | zero() => (x: Nat) => x end;
let pair(n: Nat, e: Zero(n)) -> {Nat, Nat} = match e: (_, _) => {Nat, Nat} | zero() => (1, 2) end;
let forward(h: (Nat) -> Nat) -> (Nat) -> Nat = h;
let shifted(h: (Nat) -> Nat) -> (Nat) -> Nat = (x: Nat) => h(x + 1);
let hold(@A: Type, a: A, count: Nat) -> A = match count: (_) => A | 0 => a | pred + 1 => a end;
let carry(@A: Type, count: Nat, a: A) -> A = match count: (_) => A | 0 => a | pred + 1 => carry(pred, a) end;";

/// The seeds at one type, under the binders they share.
pub(super) struct Seeds {
    pub(super) type_: &'static str,
    pub(super) binders: &'static str,
    /// What the audit puts in place of these seeds' neutral to state them closed under substitution, where the neutral is a binder no other binder's type names.
    pub(super) compound: Option<Compound>,
    pub(super) seeds: &'static [Seed],
}

/// A compound term for a neutral: the binder it replaces, as declared, the term, and the binders the term is over.
pub(super) struct Compound {
    pub(super) binder: &'static str,
    pub(super) term: &'static str,
    pub(super) binders: &'static str,
}

/// One equation: the rule it is a seed of, its two sides, and whether the rule holds it.
pub(super) struct Seed {
    pub(super) rule: &'static str,
    pub(super) left: &'static str,
    pub(super) right: &'static str,
    pub(super) holds: bool,
}

const fn held(rule: &'static str, left: &'static str, right: &'static str) -> Seed {
    Seed {
        rule,
        left,
        right,
        holds: true,
    }
}

const fn miss(rule: &'static str, left: &'static str, right: &'static str) -> Seed {
    Seed {
        rule,
        left,
        right,
        holds: false,
    }
}

pub(super) const SEEDS: &[Seeds] = &[
    Seeds {
        type_: "(Nat) -> Nat",
        binders: "g: (Nat) -> Nat, k: (Nat) -> Nat",
        compound: Some(Compound {
            binder: "g: (Nat) -> Nat",
            term: "(z: Nat) => k(z + 1)",
            binders: "",
        }),
        seeds: &[
            held("function eta", "(x: Nat) => g(x)", "g"),
            held("function eta past a fold", "(x: Nat) => g(x + 0)", "g"),
            miss("function eta, its binder dropped", "(x: Nat) => g(0)", "g"),
            held("beta", "((h: (Nat) -> Nat) => h)(g)", "g"),
            miss(
                "beta, its argument dropped",
                "((h: (Nat) -> Nat) => k)(g)",
                "g",
            ),
            held("delta", "forward(g)", "g"),
            miss("delta, another definition", "shifted(g)", "g"),
            held("zeta", "(let h = g; h)", "g"),
            miss("zeta, another value", "(let h = k; h)", "g"),
            held(
                "iota at Bool",
                "match true: (_) => (Nat) -> Nat | true => g | false => k end",
                "g",
            ),
            miss(
                "iota at Bool, the other arm",
                "match false: (_) => (Nat) -> Nat | true => g | false => k end",
                "g",
            ),
            held(
                "iota at Nat",
                "match 0: (_) => (Nat) -> Nat | 0 => g | m + 1 => k end",
                "g",
            ),
            miss(
                "iota at Nat, the other arm",
                "match 1: (_) => (Nat) -> Nat | 0 => g | m + 1 => k end",
                "g",
            ),
            held(
                "iota at a literal",
                "match 3: (_) => (Nat) -> Nat | 3 => g | _ => k end",
                "g",
            ),
            miss(
                "iota at a literal, the default",
                "match 4: (_) => (Nat) -> Nat | 3 => g | _ => k end",
                "g",
            ),
            held(
                "iota at List",
                "match [1]: (_) => (Nat) -> Nat | [] => k | [h, ..t] => g end",
                "g",
            ),
            miss(
                "iota at List, the other arm",
                "match [1]: (_) => (Nat) -> Nat | [] => g | [h, ..t] => k end",
                "g",
            ),
            held(
                "iota at a family",
                "match Option/some(0): (_) => (Nat) -> Nat | some(v) => g | none() => k end",
                "g",
            ),
            miss(
                "iota at a family, the other arm",
                "match Option/none(@Nat): (_) => (Nat) -> Nat | some(v) => g | none() => k end",
                "g",
            ),
        ],
    },
    Seeds {
        type_: "{Nat, Nat}",
        binders: "p: {Nat, Nat}",
        compound: Some(Compound {
            binder: "p: {Nat, Nat}",
            term: "(a, b)",
            binders: "a: Nat, b: Nat",
        }),
        seeds: &[
            held("record eta", "(p.0, p.1)", "p"),
            miss("record eta, its components swapped", "(p.1, p.0)", "p"),
        ],
    },
    Seeds {
        type_: "Record",
        binders: "s: Record",
        compound: Some(Compound {
            binder: "s: Record",
            term: "Record { a = m, b = n }",
            binders: "m: Nat, n: Nat",
        }),
        seeds: &[
            held("struct eta", "Record { a = s.a, b = s.b }", "s"),
            miss(
                "struct eta, its fields swapped",
                "Record { a = s.b, b = s.a }",
                "s",
            ),
        ],
    },
    // A literal's eta against a neutral that is a stuck elimination, which says no more of its type than a variable does.
    Seeds {
        type_: "{Nat, Nat}",
        binders: "which: Bool, p: {Nat, Nat}, q: {Nat, Nat}",
        compound: None,
        seeds: &[
            held(
                "record eta against a stuck elimination",
                "((match which: (_) => {Nat, Nat} | true => p | false => q end).0, (match which: (_) => {Nat, Nat} | true => p | false => q end).1)",
                "match which: (_) => {Nat, Nat} | true => p | false => q end",
            ),
            miss(
                "record eta against a stuck elimination, its components swapped",
                "((match which: (_) => {Nat, Nat} | true => p | false => q end).1, (match which: (_) => {Nat, Nat} | true => p | false => q end).0)",
                "match which: (_) => {Nat, Nat} | true => p | false => q end",
            ),
        ],
    },
    Seeds {
        type_: "(Nat) -> Nat",
        binders: "which: Bool, g: (Nat) -> Nat, k: (Nat) -> Nat",
        compound: None,
        seeds: &[
            held(
                "function eta against a stuck elimination",
                "(x: Nat) => (match which: (_) => (Nat) -> Nat | true => g | false => k end)(x)",
                "match which: (_) => (Nat) -> Nat | true => g | false => k end",
            ),
            miss(
                "function eta against a stuck elimination, its binder dropped",
                "(x: Nat) => (match which: (_) => (Nat) -> Nat | true => g | false => k end)(0)",
                "match which: (_) => (Nat) -> Nat | true => g | false => k end",
            ),
        ],
    },
    Seeds {
        type_: "Record",
        binders: "which: Bool, s: Record, r: Record",
        compound: None,
        seeds: &[
            held(
                "struct eta against a stuck elimination",
                "Record { a = (match which: (_) => Record | true => s | false => r end).a, b = (match which: (_) => Record | true => s | false => r end).b }",
                "match which: (_) => Record | true => s | false => r end",
            ),
            miss(
                "struct eta against a stuck elimination, its fields swapped",
                "Record { a = (match which: (_) => Record | true => s | false => r end).b, b = (match which: (_) => Record | true => s | false => r end).a }",
                "match which: (_) => Record | true => s | false => r end",
            ),
        ],
    },
    Seeds {
        type_: "{}",
        binders: "u: {}, v: {}, f: (Nat) -> {}, e: (Nat) -> {}, n: Nat",
        compound: Some(Compound {
            binder: "u: {}",
            term: "()",
            binders: "",
        }),
        seeds: &[
            held("unit eta, the literal", "()", "u"),
            held("unit eta, two neutrals", "v", "u"),
            // Two sides of one shape, which a structural rule compares head against head: the type decides them, and no shape does.
            held("unit eta, two applications", "f(n)", "e(n)"),
        ],
    },
    // A function into a unit has one inhabitant too, and no rule says so but two composed: eta at the function carries the goal to the unit, whose type decides it.
    Seeds {
        type_: "(Nat) -> {}",
        binders: "f: (Nat) -> {}, e: (Nat) -> {}",
        compound: Some(Compound {
            binder: "f: (Nat) -> {}",
            term: "(x: Nat) => ()",
            binders: "",
        }),
        seeds: &[
            held(
                "unit eta at a function into a unit, the literal",
                "(x: Nat) => ()",
                "f",
            ),
            held("unit eta at a function into a unit, two neutrals", "e", "f"),
        ],
    },
    // A record whose fields each have one inhabitant has one too. The literal meets a neutral by its shape and two neutrals meet by nothing but the type, so a checker that takes the first and not the second holds `r` and `s` each equal to the literal and not to each other.
    Seeds {
        type_: "{{}, {}}",
        binders: "r: {{}, {}}, s: {{}, {}}",
        compound: Some(Compound {
            binder: "r: {{}, {}}",
            term: "((), ())",
            binders: "",
        }),
        seeds: &[
            held(
                "unit eta at a record of units, the literal",
                "((), ())",
                "r",
            ),
            held("unit eta at a record of units, two neutrals", "s", "r"),
        ],
    },
    Seeds {
        type_: "Empty",
        binders: "u: Empty, v: Empty",
        compound: Some(Compound {
            binder: "u: Empty",
            term: "Empty {}",
            binders: "",
        }),
        seeds: &[
            held("unit eta at a struct, the literal", "Empty {}", "u"),
            held("unit eta at a struct, two neutrals", "v", "u"),
        ],
    },
    // A struct whose fields each have one inhabitant has one too, and a nominal struct has no eta by its type: the literal meets a neutral by its shape, and two neutrals by nothing but the shape of the type.
    Seeds {
        type_: "Unital",
        binders: "u: Unital, v: Unital, f: (Nat) -> Unital, e: (Nat) -> Unital, n: Nat, p: Holds(0 < 10)",
        compound: None,
        seeds: &[
            held(
                "one inhabitant at a struct, the literal",
                "Unital { unit = (), proof = p }",
                "u",
            ),
            held("one inhabitant at a struct, two neutrals", "v", "u"),
            held(
                "one inhabitant at a struct, two applications",
                "f(n)",
                "e(n)",
            ),
        ],
    },
    // A struct nested in its own parameter is met again while its inhabitants are read, and is judged again: its declaration names no struct.
    Seeds {
        type_: "Box(Box(Empty))",
        binders: "u: Box(Box(Empty)), v: Box(Box(Empty)), f: (Nat) -> Box(Box(Empty)), e: (Nat) -> Box(Box(Empty)), n: Nat",
        compound: None,
        seeds: &[
            held(
                "one inhabitant at a struct nested in its own parameter, the literal",
                "Box { held = Box { held = Empty {} } }",
                "u",
            ),
            held(
                "one inhabitant at a struct nested in its own parameter, two neutrals",
                "v",
                "u",
            ),
            held(
                "one inhabitant at a struct nested in its own parameter, two applications",
                "f(n)",
                "e(n)",
            ),
        ],
    },
    // A struct declared in a group is referred to through the group, which weak-head reduction leaves folded: each checker reads a goal's type forced, so its inhabitants are read there as anywhere.
    Seeds {
        type_: "Holder",
        binders: "u: Holder, v: Holder, f: (Nat) -> Holder, e: (Nat) -> Holder, n: Nat",
        compound: None,
        seeds: &[
            held(
                "one inhabitant at a struct declared in a group, the literal",
                "Holder { member = Member { unit = () }, unit = () }",
                "u",
            ),
            held(
                "one inhabitant at a struct declared in a group, two neutrals",
                "v",
                "u",
            ),
            held(
                "one inhabitant at a struct declared in a group, two applications",
                "f(n)",
                "e(n)",
            ),
        ],
    },
    Seeds {
        type_: "Nat",
        binders: "a: Nat, b: Nat, f: (Holds(a < 10)) -> Nat, c: (Nat) -> Nat, p1: Holds(a < 10), p2: Holds(a < 10)",
        compound: None,
        seeds: &[
            held("irrelevance through a variable head", "f(p1)", "f(p2)"),
            miss("relevance through a variable head", "c(a)", "c(b)"),
        ],
    },
    // A spine's arguments are typed under whatever head a lookup types, and two calls of one definition are compared by their spines before either unfolds, at whatever type and through a curried spine. Each right side is the same spine at another proof, so no seed here chains with another.
    Seeds {
        type_: "Nat",
        binders: "n: Nat, m: Nat, p: Zero(n), q: Zero(n), f: (Nat) -> (Zero(n)) -> Nat, r: {(Zero(n)) -> Nat, Nat}, c: (Nat) -> (Nat) -> Nat",
        compound: None,
        seeds: &[
            held("irrelevance past a curried head", "f(n)(p)", "f(n)(q)"),
            held("irrelevance past a projected head", "r.0(p)", "r.0(q)"),
            miss("relevance past a curried head", "c(n)(n)", "c(n)(m)"),
            held(
                "irrelevance through a definition",
                "through(n, p)",
                "through(n, q)",
            ),
            held(
                "irrelevance through a definition's curried call",
                "into(n, p)(3)",
                "into(n, q)(3)",
            ),
        ],
    },
    Seeds {
        type_: "(Nat) -> Nat",
        binders: "n: Nat, p: Zero(n), q: Zero(n)",
        compound: None,
        seeds: &[held(
            "irrelevance through a definition, at a function type",
            "into(n, p)",
            "into(n, q)",
        )],
    },
    Seeds {
        type_: "{Nat, Nat}",
        binders: "n: Nat, p: Zero(n), q: Zero(n)",
        compound: None,
        seeds: &[held(
            "irrelevance through a definition, at a record type",
            "pair(n, p)",
            "pair(n, q)",
        )],
    },
    // A tuple literal's component in a stuck elimination's arm is a child no type reaches, and what a type directs there is read off the type a lookup gives both sides: a stuck elimination's by its result, a constructor's value's by its declaration, and a binder the arm opened by its constructor's field. Each seed sets its pair there, and projects the two eliminations at the number beside it.
    Seeds {
        type_: "Nat",
        binders: "c: Bool, d: Bool, t: Both, p: Holds(0 < 10), q: Holds(0 < 10), e: Eq()(0, 0), m: Nat, n: Nat",
        compound: None,
        seeds: &[
            held(
                "irrelevance where a lookup types a stuck elimination",
                "(match c: (_) => {Holds(0 < 10), Nat} | true => (match d: (_) => Holds(0 < 10) | true => p | false => q end, 1) | false => (p, 2) end).1",
                "(match c: (_) => {Holds(0 < 10), Nat} | true => (p, 1) | false => (p, 2) end).1",
            ),
            held(
                "irrelevance where a lookup types a constructor's value",
                "(match c: (_) => {Eq()(0, 0), Nat} | true => (Eq/refl(), 1) | false => (e, 2) end).1",
                "(match c: (_) => {Eq()(0, 0), Nat} | true => (e, 1) | false => (e, 2) end).1",
            ),
            held(
                "irrelevance between two proofs an arm binds",
                "(match t: (_) => {Holds(0 < 10), Nat} | both(x, y) => (x, 1) end).1",
                "(match t: (_) => {Holds(0 < 10), Nat} | both(x, y) => (y, 1) end).1",
            ),
            miss(
                "relevance at an arm's tuple component",
                "(match c: (_) => {Nat, Nat} | true => (m, 1) | false => (m, 2) end).1",
                "(match c: (_) => {Nat, Nat} | true => (n, 1) | false => (m, 2) end).1",
            ),
        ],
    },
    Seeds {
        type_: "Bounded",
        binders: "a: Nat, b: Nat, p1: Holds(a < 10), p2: Holds(a < 10), q: Holds(b < 10)",
        compound: None,
        seeds: &[
            held(
                "irrelevance in a proof field",
                "Bounded { n = a, ok = p1 }",
                "Bounded { n = a, ok = p2 }",
            ),
            miss(
                "relevance beside a proof field",
                "Bounded { n = a, ok = p1 }",
                "Bounded { n = b, ok = q }",
            ),
        ],
    },
];

impl Seeds {
    /// One seed as a row, under the binders these seeds share.
    pub(super) fn row(&self, seed: &Seed) -> (String, String) {
        (
            self.binders.to_owned(),
            claim(self.type_, seed.left, seed.right),
        )
    }

    /// These seeds as rows.
    pub(super) fn rows(&self) -> Vec<(String, String)> {
        self.seeds.iter().map(|seed| self.row(seed)).collect()
    }
}

/// The claim that `left` and `right` are equal at `type_`. It states the type, where a carrier's row leaves it to inference: a row that left the type out would ask the elaborator for a solution as well, `Eq/refl()`'s implicit against a lambda whose type the item's drain settles, and what a seed asks for is a comparison.
pub(super) fn claim(type_: &str, left: &str, right: &str) -> String {
    format!("Eq(@({type_}))({left}, {right})")
}

#[test]
fn every_seed_is_on_the_side_each_checker_puts_it() {
    // Mutation-checked both ways: a near miss stated as held is named as refused by both and unlisted, and a row taken off the table is named as parting and unlisted.
    let found = SEEDS
        .iter()
        .flat_map(|seeds| {
            let answers = asked_alone(STRUCTURAL, seeds.type_, &seeds.rows());
            seeds
                .seeds
                .iter()
                .zip(answers)
                .filter(|(seed, answers)| *answers != Answers::both(seed.holds))
                .map(|(seed, answers)| (seed.rule.to_owned(), answers))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    hold_to_the_table(Audit::Seeds, &found);
}

#[test]
fn every_held_seed_the_table_does_not_list_closes_by_refl() {
    // The admitting direction as an author meets it: a compilation, the elaborator and then the kernel over the proof it built.
    let listed = parted();
    for seeds in SEEDS {
        let rows = seeds
            .seeds
            .iter()
            .filter(|seed| seed.holds)
            .filter(|seed| {
                !listed
                    .iter()
                    .any(|(audit, row, ..)| *audit == Audit::Seeds && row == seed.rule)
            })
            .map(|seed| seeds.row(seed))
            .collect::<Vec<_>>();
        if let Err(failures) = closes(STRUCTURAL, &rows) {
            panic!(
                "a held seed at `{}` no longer closes:\n{}",
                seeds.type_,
                failures.join("\n")
            );
        }
    }
}
