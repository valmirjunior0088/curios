(* chain: build a cons list of 10 000 cells once, then transform it K times, every
   round rebuilding each cell from a predecessor that dies with it. OCaml's list
   is the cons list itself, and its generational minor heap is the point of
   comparison: young cells die where they are born, but nothing reuses them. *)
let cells = 10000

let build n seed =
  let rec go i x acc = if i = 0 then acc else go (i - 1) (75 * x mod 65537) (x :: acc) in
  go n seed []

let step rest =
  let rec go rest acc =
    match rest with
    | [] -> acc
    | v :: tail -> go tail ((75 * v + 13) mod 65537 :: acc)
  in
  go rest []

let rec rounds k c = if k = 0 then c else rounds (k - 1) (step c)

let total c = List.fold_left (fun acc v -> (acc + v) mod 1000003) 0 c

let () =
  let k = int_of_string Sys.argv.(1) in
  Printf.printf "%d\n" (total (rounds k (build cells ((k + 1) mod 65537))))
