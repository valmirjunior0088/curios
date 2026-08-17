(* churn: thread a six-field record through N LCG-fed steps, two fields updated
   per step. OCaml's records are immutable and `{ r with … }` is the obvious
   functional spelling, so this column allocates a fresh 7-word block per step
   on the minor heap — the nursery priced on record churn, against the
   contestants that mutate and the one that reuses. *)
type churn = { a : int; b : int; c : int; d : int; e : int; f : int }

let p_mod = 1000003

let rec walk i p x r =
  if i = 0 then r
  else
    let y = 75 * x mod 65537 in
    match p with
    | 0 -> walk (i - 1) 1 y { r with a = (r.c + r.e + y) mod p_mod; b = (r.d + r.f + y) mod p_mod }
    | 1 -> walk (i - 1) 2 y { r with c = (r.e + r.a + y) mod p_mod; d = (r.f + r.b + y) mod p_mod }
    | _ -> walk (i - 1) 0 y { r with e = (r.a + r.c + y) mod p_mod; f = (r.b + r.d + y) mod p_mod }

let () =
  let n = int_of_string Sys.argv.(1) in
  let r = walk n 0 ((n + 1) mod 65537) { a = 1; b = 2; c = 3; d = 4; e = 5; f = 6 } in
  Printf.printf "%d\n" r.a
