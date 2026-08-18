-- lcg: iterate a small LCG `x = (75 * x) mod 65537` N times.
-- Small constants keep every intermediate within Curios's i31 runtime ints,
-- so all languages compute identical values (see README).
def main (args : List String) : IO Unit := do
  let n := args.head!.toNat!
  let mut x : UInt64 := 1
  for _ in [0:n] do
    x := (75 * x) % 65537
  IO.println x
