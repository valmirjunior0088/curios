-- spines: N LCG-keyed inserts into a map, then fold the values.
--
-- This is the reuse-on-spines column the workload exists for: Std.TreeMap is a
-- persistent balanced tree, `insert` rebuilds the root-to-key path — and with
-- the map uniquely owned through the tail-recursive walk, Perceus's
-- reset-and-reuse rewrites the dying spine's nodes in place instead of
-- allocating fresh ones.
--
-- VERIFY: Std.TreeMap's `insert`/`foldl` names and argument order against the
-- toolchain's Std; keys are UInt64 under the default `compare`.
partial def walk (n : Nat) (i : Nat) (x : UInt64) (m : Std.TreeMap UInt64 UInt64) : Std.TreeMap UInt64 UInt64 :=
  if i == n then m
  else
    let y := 75 * x % 65537
    walk n (i + 1) y (m.insert y (UInt64.ofNat i % 1000003))

def main (args : List String) : IO Unit := do
  let n := args.head!.toNat!
  let seed : UInt64 := (UInt64.ofNat n + 1) % 65537
  let m := walk n 0 seed Std.TreeMap.empty
  IO.println (m.foldl (fun acc _ v => (acc + v) % 1000003) (0 : UInt64))
