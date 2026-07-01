(* lcg: iterate a small LCG `x = (75 * x) mod 65537` N times.
   Small constants keep every intermediate within Curios's i31 runtime ints,
   so all languages compute identical values (see README). *)
let () =
  let n = int_of_string Sys.argv.(1) in
  let x = ref 1 in
  for _ = 1 to n do
    x := (75 * !x) mod 65537
  done;
  Printf.printf "%d\n" !x
