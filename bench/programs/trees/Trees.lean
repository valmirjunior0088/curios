-- trees: build a perfect tree of depth D with unique per-node payloads, then sum
-- them (mod 1000003). Lean uses Perceus reference counting.
-- `deriving Inhabited` gives the `partial def`s below a Nonempty return type
-- (their termination isn't proved structurally, so Lean asks for it).
inductive Tree where
  | leaf : UInt64 → Tree
  | node : UInt64 → Tree → Tree → Tree
  deriving Inhabited

partial def build (d : Nat) (v : UInt64) : Tree :=
  if d == 0 then .leaf v
  else .node v (build (d - 1) (v * 2)) (build (d - 1) (v * 2 + 1))

partial def sumT : Tree → UInt64
  | .leaf v => v % 1000003
  | .node v l r => (v + sumT l + sumT r) % 1000003

def main (args : List String) : IO Unit := do
  let d := args.head!.toNat!
  IO.println (sumT (build d 1))
