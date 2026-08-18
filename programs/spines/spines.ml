(* spines: N LCG-keyed inserts into a map, then fold the values. OCaml's Map is
   the persistent balanced tree, and `add` rebuilds the root-to-key path the way
   any persistent map does — the same spine churn Curios's crit-bit trie pays,
   priced under a generational minor heap, keyed by the integer itself. *)
module IntMap = Map.Make (Int)

let p_mod = 1000003

let () =
  let n = int_of_string Sys.argv.(1) in
  let rec go i x m =
    if i = n then m
    else
      let y = 75 * x mod 65537 in
      go (i + 1) y (IntMap.add y (i mod p_mod) m)
  in
  let m = go 0 ((n + 1) mod 65537) IntMap.empty in
  Printf.printf "%d\n" (IntMap.fold (fun _ v acc -> (acc + v) mod p_mod) m 0)
