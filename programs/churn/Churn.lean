-- churn: thread a six-field record through N LCG-fed steps, two fields updated
-- per step via structure update.
--
-- This is the column the workload exists for: `{ r with … }` is the shape
-- Perceus's reset-and-reuse is named for — `r` is uniquely owned through the
-- tail-recursive walk, so the update writes two fields of the existing object
-- in place and the loop allocates nothing after the first step.
structure Churn where
  a : UInt64
  b : UInt64
  c : UInt64
  d : UInt64
  e : UInt64
  f : UInt64

partial def walk (i : Nat) (p : Nat) (x : UInt64) (r : Churn) : Churn :=
  if i == 0 then r
  else
    let y := 75 * x % 65537
    match p with
    | 0 => walk (i - 1) 1 y { r with a := (r.c + r.e + y) % 1000003, b := (r.d + r.f + y) % 1000003 }
    | 1 => walk (i - 1) 2 y { r with c := (r.e + r.a + y) % 1000003, d := (r.f + r.b + y) % 1000003 }
    | _ => walk (i - 1) 0 y { r with e := (r.a + r.c + y) % 1000003, f := (r.b + r.d + y) % 1000003 }

def main (args : List String) : IO Unit := do
  let n := args.head!.toNat!
  let seed : UInt64 := (UInt64.ofNat n + 1) % 65537
  let r := walk n 0 seed { a := 1, b := 2, c := 3, d := 4, e := 5, f := 6 }
  IO.println r.a
