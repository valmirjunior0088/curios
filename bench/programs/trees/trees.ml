(* binary-trees: build a perfect tree of depth D with unique per-node payloads,
   then sum them. OCaml's 63-bit int holds the sums for D up to ~24. *)
type tree =
  | Leaf of int
  | Node of int * tree * tree

let rec build d v =
  if d = 0 then Leaf v
  else Node (v, build (d - 1) (v * 2), build (d - 1) (v * 2 + 1))

let rec sum = function
  | Leaf v -> v mod 1000003
  | Node (v, l, r) -> (v + sum l + sum r) mod 1000003

let () =
  let d = int_of_string Sys.argv.(1) in
  Printf.printf "%d\n" (sum (build d 1))
