//! Runtime-dependent consumers of the optimizer's five snapshot sweeps.

pub(super) const JUMPS: &str = r#"
use /std/{Nat, Bool, Option, List, Process};
let walk(n: Nat, acc: Nat) -> Nat =
    match n
    | 0 => acc
    | k + 1 =>
        let a = match k % 2 == 0 | true => Option/some(acc + 1) | false => Option/none() end;
        let b = match k % 3 == 0 | true => Option/some(acc + 2) | false => Option/none() end;
        let x = match a | some(x) => x | none() => acc end;
        let y = match b | some(y) => y | none() => acc end;
        walk(k, (x + y) % 65537)
    end;
let entry() -> /std/Io({}) =
    let n = List/len(Process/args()!);
    /std/print(Nat/to_str(walk(n, 0)));
entry()
"#;

pub(super) const WORKERS: &str = r#"
use /std/{Nat, List, Process};
let left(n: Nat, pair: {Nat, Nat}) -> Nat =
    match n | 0 => pair.0 + pair.1 | k + 1 => left(k, (pair.1, (pair.0 + pair.1) % 65537)) + 1 end;
let right(n: Nat, pair: {Nat, Nat}) -> Nat =
    match n | 0 => pair.0 + pair.1 | k + 1 => right(k, ((pair.0 + pair.1) % 65537, pair.0)) + 2 end;
let entry() -> /std/Io({}) =
    let n = List/len(Process/args()!);
    /std/print(Nat/to_str(left(n, (n, 1)) + right(n, (n, 2))));
entry()
"#;

pub(super) const UNCURRY: &str = r#"
use /std/{Nat, List, Process};
let left(n: Nat) -> (Nat) -> Nat =
    match n
    | 0 => (s) => (s * 7 + 13) % 30011
    | k + 1 => (s) =>
        let a = (s * 5 + 3) % 30011;
        let b = (a * 11 + 17) % 30011;
        let c = (b * 3 + 19) % 30011;
        left(k)(c)
    end;
let right(n: Nat) -> (Nat) -> Nat =
    match n
    | 0 => (s) => (s * 3 + 11) % 30013
    | k + 1 => (s) =>
        let a = (s * 7 + 5) % 30013;
        let b = (a * 13 + 19) % 30013;
        let c = (b * 5 + 23) % 30013;
        right(k)(c)
    end;
let entry() -> /std/Io({}) =
    let n = List/len(Process/args()!);
    /std/print(Nat/to_str(left(n)(1) + right(n)(2)));
entry()
"#;

pub(super) const CALLS: &str = r#"
use /std/{Nat, List, Process};
induct Side: Type | left(Nat) | right(Nat) end
let left(side: Side) -> Nat =
    let a = match side | left(x) => x | right(x) => x + 1 end;
    let b = a * 3 % 65537;
    let c = b * 5 % 65537;
    let d = c * 7 % 65537;
    let e = d * 11 % 65537;
    let f = e * 13 % 65537;
    f * 17 % 65537;
let right(side: Side) -> Nat =
    let a = match side | left(x) => x + 2 | right(x) => x end;
    let b = a * 5 % 65539;
    let c = b * 7 % 65539;
    let d = c * 11 % 65539;
    let e = d * 13 % 65539;
    let f = e * 17 % 65539;
    f * 19 % 65539;
let entry() -> /std/Io({}) =
    let n = List/len(Process/args()!);
    let a = left(Side/left(n));
    let b = left(Side/right(n));
    let c = right(Side/left(n));
    let d = right(Side/right(n));
    let f = match n % 2 | 0 => left | _ => right end;
    let extra = f(Side/left(n));
    /std/print(Nat/to_str(a + b + c + d + extra));
entry()
"#;

pub(super) const SCCS: &str = r#"
use /std/{Nat, List, Process};
let walk(op: (Nat) -> Nat, n: Nat, acc: Nat) -> Nat =
    match n | 0 => acc | k + 1 => op(walk(op, k, (acc + n) % 65537)) end;
let up(n: Nat) -> Nat = (n + 1) % 65537;
let down(n: Nat) -> Nat = (n * 3 + 2) % 65537;
let entry() -> /std/Io({}) =
    let n = List/len(Process/args()!);
    /std/print(Nat/to_str(walk(up, n, 0) + walk(down, n, 1)));
entry()
"#;
