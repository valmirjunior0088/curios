//! A `let` block read where a value is: what leaves the block names none of its locals.
//!
//! A block is typed by its tail, and the tail's type may name what the block binds. Nothing binds those names once the block is left, so the type the block answers reads each local as its value and a recursive member as its projection out of its group, and a candidate that names a local, for a metavariable born outside the block, is that value's. A metavariable born in the block holds each local as an argument: one the block leaves unsolved is restricted to the binders that remain, so that whatever solves it later names no local where the block's type is read.

use crate::tests::run;

/// A block's type reads the block's local by value: `tag(n)` is typed `Eq()(n, n)` inside the block and `Eq()(2, 2)` where the block is left, which is how the kernel types a `let`. Left naming `n`, the binding after the block is refused for a type that reads `Eq()(n, n)`, and with nothing reading the type the kernel refuses the declaration for an unbound name.
#[test]
fn a_blocks_type_reads_its_local_by_value_where_the_block_is_left() {
    let output = run(r#"
        use /std/{Eq, Nat, print};

        let tag(z: Nat) -> Eq()(z, z) = Eq/refl();

        let read() -> Nat =
            let x = (let n = 2; tag(n));
            let _y: Eq()(2, 2) = x;
            0;

        let unread() -> Nat =
            let _x = (let n = 2; tag(n));
            0;

        print(Nat/to_str(read() + unread()))
        "#);

    assert_eq!(output, b"0");
}

/// A recursive block's type reads a member as its projection out of the group, which computes wherever the type is read: `tag(g(1))` is typed `Eq()(0, 0)` where the block is left.
#[test]
fn a_recursive_blocks_type_reads_its_member_as_the_groups() {
    let output = run(r#"
        use /std/{Eq, Nat, print};

        let tag(z: Nat) -> Eq()(z, z) = Eq/refl();

        let read() -> Nat =
            let x = (let g(k: Nat) -> Nat = match k | 0 => 0 | j + 1 => g(j) end; tag(g(1)));
            let _y: Eq()(0, 0) = x;
            0;

        let unread() -> Nat =
            let _x = (let g(k: Nat) -> Nat = match k | 0 => 0 | j + 1 => g(j) end; tag(g(1)));
            0;

        print(Nat/to_str(read() + unread()))
        "#);

    assert_eq!(output, b"0");
}

/// A block gives an implicit its type whichever argument is read first. Read first, the block offers `Le(n)` for `both`'s `A`, which is born outside the block; `n` stands for `2`, so the candidate is `Le(2)`. Refused for naming `n`, the call was admitted with the block second and refused with it first.
#[test]
fn a_block_gives_an_implicit_its_type_whichever_argument_is_read_first() {
    let output = run(r#"
        use /std/{Eq, Nat, print};
        use /std/Bool/{Holds};

        struct Le(z: Nat): Type {
            low: Nat,
            @ok: Holds(low <= z),
        }

        let mk(@z: Nat, @w: Le(z)) -> Eq(@Le(z))(w, w) = Eq/refl();
        let both(@A: Type, @v: A, a: Eq(@A)(v, v), b: Eq(@A)(v, v)) -> Nat = 0;

        let first(d: Le(2)) -> Nat =
            let s: Eq(@Le(2))(d, d) = Eq/refl();
            both((let n = 2; mk(@n)), s);

        let second(d: Le(2)) -> Nat =
            let s: Eq(@Le(2))(d, d) = Eq/refl();
            both(s, (let n = 2; mk(@n)));

        print(Nat/to_str(first(Le { low = 1 }) + second(Le { low = 1 })))
        "#);

    assert_eq!(output, b"0");
}

/// A solution that names a block's local reads it by value where the block is left. `wrap`'s `A` is solved inside the block to `Le(x)`, so the block is typed `Eq(@Le(x))(q, q)` there and `Eq(@Le(2))(p, p)` where it is left. Read with only the type's own names replaced, the solution kept `x`, and with nothing reading the type the kernel refused the declaration for an unbound name.
#[test]
fn a_solution_naming_a_blocks_local_reads_it_by_value_where_the_block_is_left() {
    let output = run(r#"
        use /std/{Eq, Nat, print};
        use /std/Bool/{Holds};

        struct Le(z: Nat): Type {
            low: Nat,
            @ok: Holds(low <= z),
        }

        let wrap(@A: Type, a: A) -> Eq(@A)(a, a) = Eq/refl();

        let read(p: Le(2)) -> Nat =
            let r = (let x = 2; let q: Le(x) = p; wrap(q));
            let _y: Eq(@Le(2))(p, p) = r;
            0;

        let unread(p: Le(2)) -> Nat =
            let _r = (let x = 2; let q: Le(x) = p; wrap(q));
            0;

        print(Nat/to_str(read(Le { low = 0 }) + unread(Le { low = 0 })))
        "#);

    assert_eq!(output, b"0");
}

/// A problem parked in a block solves an implicit the block left unsolved, and what it gives names no local where the block's type is read. `hold`'s `V` waits on `b`: the block ends with `Le(x) = sel(b, V)` parked and `V` unsolved, `settle` then gives `b`, and the parked problem, retried where `x` stands, offers `Le(x)`. `V` was restricted to the binders before the block's where the block ended, so the candidate is `Le(2)` and `r.1` is a `Tag(true, Le(2))`. Unrestricted, `V` took `Le(x)` through its spine and the kernel refused the declaration for an unbound name.
#[test]
fn a_problem_parked_in_a_block_solves_an_implicit_the_block_left_unsolved() {
    let output = run(r#"
        use /std/{Eq, Bool, Nat, print};
        use /std/Bool/{Holds};

        struct Le(z: Nat): Type {
            low: Nat,
            @ok: Holds(low <= z),
        }

        struct Flag(b: Bool): Type {}
        struct Tag(b: Bool, V: Type): Type {}

        let sel(b: Bool, V: Type) -> Type = match b | true => V | false => Nat end;
        let hold(@b: Bool, @V: Type, w: sel(b, V)) -> {Flag(b), Tag(b, V)} = (Flag {}, Tag {});
        let settle(@b: Bool, f: Flag(b), e: Eq(@Bool)(b, true)) -> Nat = 0;
        let take(t: Tag(true, Le(2))) -> Nat = 0;

        let read(p: Le(2)) -> Nat =
            let r = (let x = 2; let q: Le(x) = p; hold(q));
            let n = settle(r.0, Eq/refl());
            n + take(r.1);

        let unread(p: Le(2)) -> Nat =
            let r = (let x = 2; let q: Le(x) = p; hold(q));
            settle(r.0, Eq/refl());

        print(Nat/to_str(read(Le { low = 0 }) + unread(Le { low = 0 })))
        "#);

    assert_eq!(output, b"0");
}

/// An implicit a block left unsolved takes a name the block's local stands for. `pick`'s `v` is born over `d` and `x`, where `x` is `d`: the block ends with it unsolved, and `both` then gives it `d`. Its spine is the renaming a solution is inverted through, and `d` written into it for `x` would name `d` twice, which no inversion reads.
#[test]
fn an_implicit_a_block_left_unsolved_takes_a_name_the_blocks_local_stands_for() {
    let output = run(r#"
        use /std/{Eq, Nat, print};

        let pick(@A: Type, @v: A, k: Nat) -> Eq(@A)(v, v) = Eq/refl();
        let both(@A: Type, @v: A, a: Eq(@A)(v, v), b: Eq(@A)(v, v)) -> Nat = 0;

        let f(d: Nat) -> Nat =
            let s: Eq(@Nat)(d, d) = Eq/refl();
            let r = (let x = d; pick(x));
            both(r, s);

        print(Nat/to_str(f(1)))
        "#);

    assert_eq!(output, b"0");
}

/// An implicit born in a block is solved once the block is left, at its type as the block's local reads there. `mk`'s `w` is born at `Le(n)` and the block ends with it unsolved; `both` then gives it `d`, a `Le(2)`. Restricted where the block ended, it stands at `Le(2)`; left as it was born, its candidate was checked at `Le(n)` with nothing bound to `n`, and the call refused.
#[test]
fn an_implicit_born_in_a_block_is_solved_once_the_block_is_left() {
    let output = run(r#"
        use /std/{Eq, Nat, print};
        use /std/Bool/{Holds};

        struct Le(z: Nat): Type {
            low: Nat,
            @ok: Holds(low <= z),
        }

        let mk(@z: Nat, @w: Le(z)) -> Eq(@Le(z))(w, w) = Eq/refl();
        let both(@A: Type, @v: A, a: Eq(@A)(v, v), b: Eq(@A)(v, v)) -> Nat = 0;

        let f(d: Le(2)) -> Nat =
            let s: Eq(@Le(2))(d, d) = Eq/refl();
            let x = (let n = 2; mk(@n));
            both(x, s);

        print(Nat/to_str(f(Le { low = 1 })))
        "#);

    assert_eq!(output, b"0");
}
