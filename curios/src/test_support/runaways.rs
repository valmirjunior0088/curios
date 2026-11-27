//! A runaway: a short program whose one claim makes a checker do more the longer it is let run, which the runaway suites refuse inside the budgets they state and the bench's measurement of runaways reads.

use curios_common::fill;

/// What a runaway's claim makes a checker do.
#[derive(Clone, Copy)]
pub enum Runaway {
    /// `spin(k)` reduced at a literal no budget reaches: a step a unit, and nothing built.
    Spin,
    /// `f(c + k, k)` against itself at another first argument: unfolded, the two calls meet again under a binder, over a sum one summand longer.
    Sums,
    /// `f(c, a + k, k)` against itself at another first argument: the sum that grows is built apart from the argument the two calls differ in.
    SumsApart,
    /// `g(h, s, c + h(d), s(d), n / 2)` against itself at another third argument: unfolded, the two calls meet again under no binder, over a sum that has gained a call.
    SumsOfCalls,
    /// `g(c, n / 2)` against itself at another first argument: unfolded, the two calls meet again with nothing built.
    Halvings,
    /// `f(k) + 2` against `2 + h(k)`: two recursions of one shape, each of whose unfoldings is the other's one step on.
    Rounds,
    /// `acc(h, k, c + h(k))` reduced at so many steps: a sum that gains a call a step, whose atoms each fold classes again.
    Classes(u32),
    /// `pw(k, c * y, y)` reduced at so many steps: a product that gains a factor a step.
    Powers(u32),
    /// A power of so many factors widened and compared at each of so many steps: an image written, and read back, a step.
    Images(u32, u32),
    /// `nest(k, T)` at so many steps, over two types: two types nested alike and apart at their core.
    Nests(u32),
    /// `g(tower(g, k))` against `stack(g, k, g(top))` at so many steps: one term built by two recursions, the second of which a checker opens alone a step at a time, a comparison nested in the last at each.
    Stacks(u32),
    /// `walk(g, k, top)` at so many steps, against `g(top)`: a recursion whose last step applies a variable.
    Walks(u32),
    /// `b || (b && (a0 && … ))` over so many atoms, against `b`: absorption, which a decision diagram decides in the square of its atoms.
    Absorption(u32),
    /// `(x0 && … && c) || P` against `P || (c && x0 && … )`, `P` the disjunction of so many pairs `xi && yi`: one value, read from a side that meets every `xi` before any `yi`, an order whose diagrams double with every pair.
    Pairs(u32),
    /// [`Runaway::Pairs`] with its sides swapped, read from the side that meets each pair's two atoms together, an order whose diagrams grow by a few nodes a pair.
    PairsTurned(u32),
}

/// The claim [`Runaway::Pairs`] and [`Runaway::PairsTurned`] state over so many pairs, the side that meets every `xi` first on the left or the right.
fn pairs(count: u32, turned: bool) -> String {
    let xs = (0..count)
        .map(|index| format!("x{index}"))
        .collect::<Vec<_>>();
    let binders = (0..count)
        .flat_map(|index| [format!("x{index}: Bool"), format!("y{index}: Bool")])
        .chain(["c: Bool".to_string()])
        .collect::<Vec<_>>()
        .join(", ");
    let pairs = (0..count)
        .map(|index| format!("(x{index} && y{index})"))
        .collect::<Vec<_>>()
        .join(" || ");
    let first = format!("({} && c) || ({pairs})", xs.join(" && "));
    let second = format!("({pairs}) || (c && {})", xs.join(" && "));
    let (left, right) = match turned {
        false => (first, second),
        true => (second, first),
    };
    format!("let claim({binders}) -> Eq()({left}, {right}) =>")
}

const POWER: &str =
    "let pw(n: Nat, c: Nat, y: Nat) -> Nat => match n | 0 => c | k + 1 => pw(k, c * y, y) end;";

/// A runaway as a whole program: its definitions, its one claim closed by `Eq/refl()`, and an entrypoint that reads none of them. A claim is written with its separator, `=` after a value's type and `=>` after a function's result.
pub fn runaway(shape: Runaway) -> String {
    // What reads the claim after it, where a row measures what the certifier made of it: the certifier files what a judgment cost it where it begins the next.
    let mut reader = String::new();
    let (imports, definitions, claim) = match shape {
        Runaway::Spin => (
            "Nat, Eq, Io",
            "let spin(n: Nat) -> Nat => match n | 0 => 0 | k + 1 => spin(k) end;".to_string(),
            "let claim: Eq()(spin(100000000), 0) =".to_string(),
        ),
        Runaway::Sums => (
            "Nat, Eq, Io",
            "let f(c: Nat, n: Nat) -> Nat => match n | 0 => 0 | k + 1 => f(c + k, k) end;"
                .to_string(),
            "let claim(x: Nat, y: Nat) -> Eq()(f(x, y), f(2, y)) =>".to_string(),
        ),
        Runaway::SumsApart => (
            "Nat, Eq, Io",
            "let f(c: Nat, a: Nat, n: Nat) -> Nat => match n | 0 => 0 | k + 1 => f(c, a + k, k) end;"
                .to_string(),
            "let claim(x: Nat, y: Nat) -> Eq()(f(x, 0, y), f(2, 0, y)) =>".to_string(),
        ),
        Runaway::SumsOfCalls => (
            "Nat, Eq, Io",
            "let g(h: (Nat) -> Nat, s: (Nat) -> Nat, c: Nat, d: Nat, n: Nat) -> Nat =>\n    match n == 0 | true => 0 | false => g(h, s, c + h(d), s(d), n / 2) end;"
                .to_string(),
            "let claim(h: (Nat) -> Nat, s: (Nat) -> Nat, x: Nat, d: Nat, y: Nat)\n    -> Eq()(g(h, s, x, d, y), g(h, s, 2, d, y)) =>"
                .to_string(),
        ),
        Runaway::Halvings => (
            "Nat, Eq, Io",
            "let g(c: Nat, n: Nat) -> Nat => match n == 0 | true => 0 | false => g(c, n / 2) end;"
                .to_string(),
            "let claim(x: Nat, y: Nat) -> Eq()(g(x, y), g(2, y)) =>".to_string(),
        ),
        Runaway::Rounds => {
            reader.push_str("let read(n: Nat) -> Nat => match claim(n) | refl => 0 end;\n\n");
            (
                "Nat, Eq, Io",
                "let f(n: Nat) -> Nat => match n | 0 => 0 | k + 1 => f(k) + 2 end;\n\nlet h(n: Nat) -> Nat => match n | 0 => 0 | k + 1 => 2 + h(k) end;"
                    .to_string(),
                "let claim(n: Nat) -> Eq()(f(n), h(n)) =>".to_string(),
            )
        }
        Runaway::Classes(steps) => (
            "Nat, Eq, Io",
            "let acc(h: (Nat) -> Nat, n: Nat, c: Nat) -> Nat => match n | 0 => c | k + 1 => acc(h, k, c + h(k)) end;"
                .to_string(),
            format!("let claim(h: (Nat) -> Nat, x: Nat) -> Eq()(acc(h, {steps}, x), x) =>"),
        ),
        Runaway::Powers(steps) => (
            "Nat, Eq, Io",
            POWER.to_string(),
            format!("let claim(a: Nat, b: Nat, y: Nat) -> Eq()(pw({steps}, a + b, y), a) =>"),
        ),
        Runaway::Images(factors, steps) => (
            "Nat, Eq, Io",
            format!(
                "{POWER}\n\nlet widen(n: Nat, c: Nat) -> Nat =>\n    match n | 0 => 0 | k + 1 => match Nat/to_int(c + k) < +0 | true => 1 | false => widen(k, c) end end;"
            ),
            format!("let claim(y: Nat) -> Eq()(widen({steps}, pw({factors}, y, y)), 1) =>"),
        ),
        Runaway::Nests(steps) => (
            "Nat, Int, Option, Eq, Io",
            "let nest(n: Nat, T: Type) -> Type => match n | 0 => T | k + 1 => Option(nest(k, T)) end;"
                .to_string(),
            format!("let claim: Eq()(nest({steps}, Nat), nest({steps}, Int)) ="),
        ),
        Runaway::Stacks(steps) => {
            reader.push_str(
                "let read(g: (Nat) -> Nat) -> Nat => match claim(g) | refl => 0 end;\n\n",
            );
            (
                "Nat, Eq, Io",
                "let tower(g: (Nat) -> Nat, n: Nat) -> Nat => match n | 0 => 0 | k + 1 => g(tower(g, k)) end;\n\nlet stack(g: (Nat) -> Nat, n: Nat, top: Nat) -> Nat => match n | 0 => top | k + 1 => stack(g, k, g(top)) end;"
                    .to_string(),
                format!("let claim(g: (Nat) -> Nat) -> Eq()(tower(g, {steps}), stack(g, {steps}, 0)) =>"),
            )
        }
        Runaway::Walks(steps) => {
            reader.push_str(
                "let read(g: (Nat) -> Nat, x: Nat) -> Nat => match claim(g, x) | refl => 0 end;\n\n",
            );
            (
                "Nat, Eq, Io",
                "let walk(g: (Nat) -> Nat, n: Nat, top: Nat) -> Nat => match n | 0 => g(top) | k + 1 => walk(g, k, top) end;"
                    .to_string(),
                format!("let claim(g: (Nat) -> Nat, x: Nat) -> Eq()(walk(g, {steps}, x), g(x)) =>"),
            )
        }
        Runaway::Absorption(atoms) => {
            let names = (0..atoms).map(|index| format!("a{index}")).collect::<Vec<_>>();
            let binders = names
                .iter()
                .map(|name| format!("{name}: Bool"))
                .collect::<Vec<_>>()
                .join(", ");
            (
                "Bool, Eq, Io",
                String::new(),
                format!(
                    "let claim(b: Bool, {binders}) -> Eq()(b || (b && ({})), b) =>",
                    names.join(" && ")
                ),
            )
        }
        Runaway::Pairs(count) => ("Bool, Eq, Io", String::new(), pairs(count, false)),
        Runaway::PairsTurned(count) => ("Bool, Eq, Io", String::new(), pairs(count, true)),
    };

    fill!(
        "use /std/{${imports}};\n\n${definitions}\n\n${claim} Eq/refl();\n\n${reader}Io/pure(())\n",
        imports,
        definitions,
        claim,
        reader
    )
}
