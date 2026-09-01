//! Property-based testing across the stages: the pure splittable `Seed`, the `Draw` roster and the tuple `Spell` witnesses a counterexample renders, `Property` and `Test/property`, and the parameterized `test` declaration the synthesized tail closes through `Test/property`.

use super::run;

#[test]
fn a_drawn_sequence_is_a_function_of_its_seed() {
    // Pinned from the generator's first run: the same seed yields the same draws run after run, the size bounds a drawn number, and an `Int` spells with its sign as every `Spell` does.
    assert_eq!(
        run(r#"
        use /std/{Nat, Int, Str, List, Spell, Io};
        use /std/Test/{Seed, Draw};
        let draws(@A: Type, use Draw(A), use Spell(A), seed: Seed, size: Nat, count: Nat) -> List(Str) =
            match count
            | 0 => []
            | more + 1 =>
                let (value, rest) = Draw/draw(@A, seed, size);
                [Spell/spell(value), ..draws(@A, rest, size, more)]
            end;
        let seed: Seed = Seed/make(42);
        /std/print(
            Str/join(
                "\n",
                [
                    Str/join(", ", draws(@Nat, seed, 10, 8)),
                    Str/join(", ", draws(@Int, seed, 10, 8)),
                    Str/join(", ", draws(@Str, seed, 6, 4)),
                    ""]))
        "#),
        b"9, 7, 7, 5, 8, 7, 10, 8\n-9, -7, +8, +10, +3, -4, +10, +5\n\"r 61]\", \"\", \"T:v#v\", \"o!1\"\n"
    );
}

#[test]
fn tuple_shapes_draw_and_spell_through_their_premises() {
    // A positional shape composes its fields' witnesses, and `Spell` renders the literal the shape is written as — the one-field literal with the trailing comma that separates it from a parenthesized term.
    assert_eq!(
        run(r#"
        use /std/{Nat, Bool, Char, Str, List, Option, Spell, Io};
        use /std/Test/{Seed, Draw};
        let draws(@A: Type, use Draw(A), use Spell(A), seed: Seed, size: Nat, count: Nat) -> List(Str) =
            match count
            | 0 => []
            | more + 1 =>
                let (value, rest) = Draw/draw(@A, seed, size);
                [Spell/spell(value), ..draws(@A, rest, size, more)]
            end;
        let seed: Seed = Seed/make(42);
        /std/print(
            Str/join(
                "\n",
                [
                    Str/join(", ", draws(@{}, seed, 4, 2)),
                    Str/join(", ", draws(@{Nat}, seed, 4, 3)),
                    Str/join(", ", draws(@{Nat, Bool}, seed, 4, 4)),
                    Str/join(", ", draws(@{Nat, Char, Option(Bool)}, seed, 4, 3)),
                    Spell/spell((1, "a", ('x', true))),
                    ""]))
        "#),
        b"(), ()\n(0,), (2,), (0,)\n(0, true), (0, true), (2, false), (4, false)\n(0, 'r', /std/Option/none()), (2, '1', /std/Option/none()), (4, ':', /std/Option/some(false))\n(1, \"a\", ('x', true))\n"
    );
}

#[test]
fn the_generator_never_leaves_the_envelope() {
    // `Nat` rides an i31 and a result past it traps, so a generator whose intermediates could exceed 2^30 would trap somewhere in a long enough walk; this one masks before every left shift, and a walk of thousands of steps — `next` and both `split` children alike — finishes. The three first draws after a split are also pairwise distinct, which is what makes the children independent cases rather than a replay.
    assert_eq!(
        run(r#"
        use /std/{Nat, Bool, Str, Io};
        use /std/Test/{Seed};
        let walk(seed: Seed, steps: Nat) -> Seed =
            match steps
            | 0 => seed
            | more + 1 =>
                let (_, stepped) = Seed/next(seed);
                let (left, right) = Seed/split(stepped);
                let (_, again) = Seed/next(left);
                walk(match Nat/and(more, 1) == 0 | true => again | false => right end, more)
            end;
        let seed: Seed = walk(Seed/make(7), 5000);
        let (a, _) = Seed/next(seed);
        let (left, right) = Seed/split(seed);
        let (b, _) = Seed/next(left);
        let (c, _) = Seed/next(right);
        /std/print(Str/concat(Bool/to_str(a != b && b != c && a != c), "\n"))
        "#),
        b"true\n"
    );
}
