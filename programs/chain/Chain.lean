-- chain: build a cons list of 10 000 cells once, then transform it K times, every
-- round rebuilding each cell from a predecessor that dies with it.
--
-- This is the column the workload exists for. Lean is compiled under Perceus
-- reference counting with reset and reuse, and `step` is the shape that reuse is
-- named for: the cell destructured out of `rest` is dead at the moment the cell
-- prepended to `acc` is built, so a uniquely-owned chain is rewritten in place and
-- the whole round allocates nothing after the first.
--
-- Every walk is tail-recursive, so no contestant's stack depends on the 10 000.
def cells : Nat := 10000

partial def build (n : Nat) (x : UInt64) (acc : List UInt64) : List UInt64 :=
  if n == 0 then acc else build (n - 1) (75 * x % 65537) (x :: acc)

partial def step (rest : List UInt64) (acc : List UInt64) : List UInt64 :=
  match rest with
  | [] => acc
  | v :: tail => step tail ((75 * v + 13) % 65537 :: acc)

partial def rounds (k : Nat) (c : List UInt64) : List UInt64 :=
  if k == 0 then c else rounds (k - 1) (step c [])

partial def total (c : List UInt64) (acc : UInt64) : UInt64 :=
  match c with
  | [] => acc
  | v :: tail => total tail ((acc + v) % 1000003)

def main (args : List String) : IO Unit := do
  let k := args.head!.toNat!
  let seed : UInt64 := (UInt64.ofNat k + 1) % 65537
  IO.println (total (rounds k (build cells seed [])) 0)
