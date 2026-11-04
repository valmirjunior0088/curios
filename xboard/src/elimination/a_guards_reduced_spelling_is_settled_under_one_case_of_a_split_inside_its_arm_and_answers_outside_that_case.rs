use crate::{Date, Proof, Status, Ticket, Witness, refused};

pub(super) const TICKET: Ticket = Ticket {
    title: "A guard's reduced spelling is settled under one case of a split inside its arm, and answers outside that case",
    found_on: Date::new(2026, 10, 9),
    status: Status::Fixed,
    witnesses: &[
        Witness {
            what: "a guard over a match on a proof, read at zero in one arm of a split and answered at a successor in the next",
            proof: Proof::Program(A_GUARD_SETTLED_AT_ZERO_ANSWERS_AT_A_SUCCESSOR),
            expect: refused![elaborator: curios_elab::Error::TypeMismatch { .. } in "/f"],
        },
        Witness {
            what: "the same guard settled in the false arm of a split and answered in the true one, as a term",
            proof: Proof::Module(A_GUARD_SETTLED_UNDER_ONE_CASE_OF_A_SPLIT),
            expect: refused![kernel: curios_cert::Error::Mismatch { .. } in entry],
        },
        Witness {
            what: "a guard decided by proof irrelevance under one case of a split, and answered after the split",
            proof: Proof::Program(A_GUARD_SETTLED_WHERE_ITS_OPERANDS_ARE_PROOFS),
            expect: refused![elaborator: curios_elab::Error::TypeMismatch { .. } in "/F"],
        },
        Witness {
            what: "the same guard settled where its operands are proofs and answered where they are not, as a term",
            proof: Proof::Module(A_GUARD_SETTLED_WHERE_ITS_OPERANDS_ARE_PROOFS_AS_A_TERM),
            expect: refused![kernel: curios_cert::Error::Mismatch { .. } in entry],
        },
        Witness {
            what: "a guard decided by the unit type's one inhabitant under one case of a split, and answered after the split",
            proof: Proof::Program(A_GUARD_SETTLED_WHERE_ITS_OPERANDS_ARE_UNITS),
            expect: refused![elaborator: curios_elab::Error::TypeMismatch { .. } in "/f"],
        },
        Witness {
            what: "the same guard settled where its operands are units and answered where they are not, as a term",
            proof: Proof::Module(A_GUARD_SETTLED_WHERE_ITS_OPERANDS_ARE_UNITS_AS_A_TERM),
            expect: refused![kernel: curios_cert::Error::Mismatch { .. } in entry],
        },
    ],
};

/// The guard is the match on `p`, which stands while `n` is open: its proof's two indices are one only at a literal, so nothing states `z` before then. Its `true` arm assumes it, and the equation stands for the whole arm. Inside the `0` arm of the split `p` proves `Eq()(0, 0)`, and the match fired on the type `p` stood at there, so the equation's reduced spelling was settled as `h(0)`, where it is true. The spelling is kept with the equation, which outlives that arm: under `k + 1`, where the guard says `h(1)`, `h(0)` was answered `true` still. The match now states the instance it eliminates, `Eq()(g(n), g(g(n)))`, and fires on that: the `0` arm reaches it as a substitution into the term, the equation's own spelling is settled with `n` open, and `True/qed()` under `k + 1` is refused at `Holds(h(0))`.
const A_GUARD_SETTLED_AT_ZERO_ANSWERS_AT_A_SUCCESSOR: &str = r#"
    use /std/{Nat, Eq, Bool};
    use /std/Bool/{True, False, Holds};
    let g(n: Nat) -> Nat = match n | 0 => 0 | j + 1 => 1 end;
    let f(
        h: (Nat) -> Bool,
        n: Nat,
        p: Eq()(g(n), g(g(n))),
        guard: Holds(match p: (i, j, q) => Bool | refl(@z) => h(z) end),
        nonzero: Holds(n != 0)) -> Holds(h(0)) =
        match (match p: (i, j, q) => Bool | refl(@z) => h(z) end)
        | false => match guard end
        | true =>
            match n
            | 0 =>
                let settled: Holds(h(0)) = True/qed();
                match nonzero end
            | k + 1 => True/qed()
            end
        end;
    f((m: Nat) => m != 0, 1, Eq/refl(), True/qed(), True/qed())
    "#;

/// `F(c)` was a proposition at `true` and `Nat` at `false`, so `p` and `q` were proofs only under the `true` case of the split, where conversion held `h(p)` and `h(q)` one term without reading either. The guard is the match on `h(p) == h(q)`, which stands while `c` is open, and its reduced spelling was settled inside that arm, where the comparison was `true` and the match `k(0)`. The spelling is kept with the guard's equation, which outlives the split: after it, with `c` open again, `k(0)` was answered `true`, and at `false`, where the guard says `k(1)`, that is `Holds(false)`. What the settlement read of the split was whether two locals are proofs. No proposition stands where a type is wanted now, so `F` is refused where it is declared: a type is a proposition under every case of `c` or under none.
const A_GUARD_SETTLED_WHERE_ITS_OPERANDS_ARE_PROOFS: &str = r#"
    use /std/{Nat, Bool};
    use /std/Bool/{True, False, Holds};
    induct Two: Prop | a() | b() end
    let F(c: Bool) -> Type = match c | true => Two | false => Nat end;
    let f(
        c: Bool,
        h: (F(c)) -> Nat,
        k: (Nat) -> Bool,
        p: F(c),
        q: F(c),
        guard: Holds(match h(p) == h(q) | true => k(0) | false => k(1) end)) -> Holds(k(0)) =
        match (match h(p) == h(q) | true => k(0) | false => k(1) end)
        | false => match guard end
        | true =>
            let split: {} = match c
                | false => ()
                | true => (let settled: Holds(k(0)) = True/qed(); ())
                end;
            True/qed()
        end;
    f(false, (n: Nat) => n, (n: Nat) => n == 1, 0, 1, True/qed())
    "#;

/// [`A_GUARD_SETTLED_WHERE_ITS_OPERANDS_ARE_PROOFS`] with no proposition in it: `F(c)` is the unit type at `true`, where any two inhabitants are one by the type's shape, so the comparison is decided inside that arm and the guard's spelling was settled the same way. What the settlement read of the split was the type the arm re-typed `p` and `q` at, which the guard names and the split solves. The guard is now restated in that arm, and its spelling settled there goes with it: the `True/qed()` inside the arm holds, and the one after the split, where the guard is settled with `c` open and `h(p)` and `h(q)` are apart, is refused at `Holds(k(0))`.
const A_GUARD_SETTLED_WHERE_ITS_OPERANDS_ARE_UNITS: &str = r#"
    use /std/{Nat, Bool};
    use /std/Bool/{True, False, Holds};
    let F(c: Bool) -> Type = match c | true => {} | false => Nat end;
    let f(
        c: Bool,
        h: (F(c)) -> Nat,
        k: (Nat) -> Bool,
        p: F(c),
        q: F(c),
        guard: Holds(match h(p) == h(q) | true => k(0) | false => k(1) end)) -> Holds(k(0)) =
        match (match h(p) == h(q) | true => k(0) | false => k(1) end)
        | false => match guard end
        | true =>
            let split: {} = match c
                | false => ()
                | true => (let settled: Holds(k(0)) = True/qed(); ())
                end;
            True/qed()
        end;
    f(false, (n: Nat) => n, (n: Nat) => n == 1, 0, 1, True/qed())
    "#;

/// `read` is the match on `p` that hands its payload to `g`, and the guard is the match on `read`.
///
/// The `true` arm of the match on `read` assumes `read` is `true`, an equation recorded under `read`. The match fired on the type `p` was assumed at, so `read` named `p` and `g` and not `b`: the `false` arm of the split re-types `p` at `Read(false)`, where `read` was `g(false)`, and a stuck form met while that arm is typed settled the equation's reduced spelling there, as `g(false)`, which the equation rightly answers `true` under that case. The settled spelling stays with the equation when the arm is left, so in the `true` arm, where `read` is `g(true)`, `g(false)` was answered `true` as well, and at the identity what the `qed` proved is `Holds(false)`. The match now fires on the instance it states, `Read(b)`, so `read` is `g(b)` wherever it stands and names `b`: the spelling is settled as `g(b)` under either case, and the `qed` in the `true` arm is refused at `Holds(g(false))`.
const A_GUARD_SETTLED_UNDER_ONE_CASE_OF_A_SPLIT: &str = r#"
    #{absent = [/Read/mk]} induct /Read: (i: %BoolType) -> pub Prop
    | mk(x: %BoolType): (x)
    end
    ((g: (c: %BoolType) -> %BoolType, b: %BoolType, p: #{node = InductType} /Read()(b), guard: /sys/Bool/Holds(#{cases = {stated = #{node = InductType} /Read()(b)}} match p: (x3, x4) => %BoolType
            | mk(x) =>
                g(x)
            end), chosen: /sys/Bool/Holds(b)) =>
        match #{cases = {stated = #{node = InductType} /Read()(b)}} match p: (x9, x10) => %BoolType
        | mk(x2) =>
            g(x2)
        end: (x5) => /sys/Bool/Holds(g(false))
        | false =>
            match guard: (x6) => /sys/Bool/Holds(g(false))
            end
        | true =>
            match b: (x7) => /sys/Bool/Holds(g(false))
            | false =>
                match chosen: (x8) => /sys/Bool/Holds(g(false))
                end
            | true =>
                #{node = Variant} /sys/Bool/True/qed()
            end
        end)((c2: %BoolType) => c2, true, #{node = Variant} /Read/mk(true), #{node = Variant} /sys/Bool/True/qed(), #{node = Variant} /sys/Bool/True/qed())
    "#;

/// `F(c)` is the match on `c` into `Type`, a proposition `Two` at `true` and `Nat` at `false`, and `read` is the match on `h(p) == h(q)`, `k(0)` at `true` and `k(1)` at `false`.
///
/// The `true` arm of the match on `read` assumes `read` is `true`, an equation recorded under `read`, which names `h`, `p`, `q` and `k` and not `c`. The `true` arm of the split re-types `h`, `p` and `q` at `Two`, where conversion holds `h(p)` and `h(q)` one term, the comparison folds to `true` and `read` is `k(0)`: the `qed` that arm binds at `Holds(k(0))` settles the equation's reduced spelling there, as `k(0)`. The settled spelling stays with the equation when the split is left, so the `qed` after it, typed with `c` open, was checked at a `Holds(k(0))` that still answered `True`, and at `false`, where `read` is `k(1)`, what it proved is `Holds(false)`. The kernel now refuses `F(c)` where its `true` arm is typed, a proposition where the motive wants a type.
const A_GUARD_SETTLED_WHERE_ITS_OPERANDS_ARE_PROOFS_AS_A_TERM: &str = r#"
    #{absent = [/Two/a, /Two/b]} induct /Two: pub Prop
    | a()
    | b()
    end
    ((c: %BoolType, h: (x: match c: (x2) => Type
            | false =>
                %NatType
            | true =>
                #{node = InductType} /Two()
            end) -> %NatType, k: (n: %NatType) -> %BoolType, p: match c: (x3) => Type
        | false =>
            %NatType
        | true =>
            #{node = InductType} /Two()
        end, q: match c: (x4) => Type
        | false =>
            %NatType
        | true =>
            #{node = InductType} /Two()
        end, guard: /sys/Bool/Holds(match %NatEql(h(p), h(q)): (x5) => %BoolType
            | false =>
                k(1)
            | true =>
                k(0)
            end)) =>
        match match %NatEql(h(p), h(q)): (x9) => %BoolType
        | false =>
            k(1)
        | true =>
            k(0)
        end: (x6) => /sys/Bool/Holds(k(0))
        | false =>
            match guard: (x7) => /sys/Bool/Holds(k(0))
            end
        | true =>
            let w: #{node = InductType} /sys/Bool/True() =
                match c: (x8) => #{node = InductType} /sys/Bool/True()
                | false =>
                    #{node = Variant} /sys/Bool/True/qed()
                | true =>
                    let u: /sys/Bool/Holds(k(0)) =
                        #{node = Variant} /sys/Bool/True/qed();
                    #{node = Variant} /sys/Bool/True/qed()
                end;
            #{node = Variant} /sys/Bool/True/qed()
        end)(false, (n2: %NatType) => n2, (n3: %NatType) => %NatEql(n3, 1), 0, 1, #{node = Variant} /sys/Bool/True/qed())
    "#;

/// [`A_GUARD_SETTLED_WHERE_ITS_OPERANDS_ARE_PROOFS_AS_A_TERM`] with the unit type `{}` where `Two` stood, and no declaration: at `true` the operands' type has one inhabitant by its shape, and conversion holds `h(p)` and `h(q)` one term there as it does at a proposition. The guard names `p` and `q`, whose type names `c`, so the split's arm restates it and settles it there, and the `qed` after the split is refused at `Holds(k(0))`.
const A_GUARD_SETTLED_WHERE_ITS_OPERANDS_ARE_UNITS_AS_A_TERM: &str = r#"
    ((c: %BoolType, h: (x: match c: (x2) => Type
            | false =>
                %NatType
            | true =>
                {}
            end) -> %NatType, k: (n: %NatType) -> %BoolType, p: match c: (x3) => Type
        | false =>
            %NatType
        | true =>
            {}
        end, q: match c: (x4) => Type
        | false =>
            %NatType
        | true =>
            {}
        end, guard: /sys/Bool/Holds(match %NatEql(h(p), h(q)): (x5) => %BoolType
            | false =>
                k(1)
            | true =>
                k(0)
            end)) =>
        match match %NatEql(h(p), h(q)): (x9) => %BoolType
        | false =>
            k(1)
        | true =>
            k(0)
        end: (x6) => /sys/Bool/Holds(k(0))
        | false =>
            match guard: (x7) => /sys/Bool/Holds(k(0))
            end
        | true =>
            let w: #{node = InductType} /sys/Bool/True() =
                match c: (x8) => #{node = InductType} /sys/Bool/True()
                | false =>
                    #{node = Variant} /sys/Bool/True/qed()
                | true =>
                    let u: /sys/Bool/Holds(k(0)) =
                        #{node = Variant} /sys/Bool/True/qed();
                    #{node = Variant} /sys/Bool/True/qed()
                end;
            #{node = Variant} /sys/Bool/True/qed()
        end)(false, (n2: %NatType) => n2, (n3: %NatType) => %NatEql(n3, 1), 0, 1, #{node = Variant} /sys/Bool/True/qed())
    "#;
