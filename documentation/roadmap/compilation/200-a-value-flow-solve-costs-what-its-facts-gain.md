# A value flow solve costs what its facts gain

**Not refined yet.** A compilation solves `curios-cps`'s value flow (`ValueFlow::of`) whole each time something reads it, twelve times for the program that decodes TOML: once a round in which a row test stands (`answer_row_tests`), once for the rebuild in place (`rebuild_in_place`), once for the frame bound (`bound_frames`), and once at the machine boundary, which `curios-emit`'s `lower` shares between the verifier's rebuilds (`Module::verify_reading`) and the entry's capability (`Module::entry_suspension_reading`). A solve reruns a rule for each move of a fact it read, and each run copies the facts it reads, so it costs its runs times the sets those facts hold. On the program that decodes TOML this is nearly all of a compile.

## What is known

- **Where the compile goes.** Mixed, at `6d7c23f7e`, in a profiling build by the protocol below, one reading: 13 solves took 65.9 s of the 69.4 s from ANF to Wasm. The nine of `answer_row_tests` took 0.1 s to 6.2 s each as the module grew, the rebuild's and the frame bound's 2.7 s each, and the two at the machine boundary 17.5 s each, over the module the frame bound left: 23,217 nodes, 395 functions and 1.7 million set elements, the largest set 2,099, against 16,196, 213, 512 thousand and 862 before it.
- **What sharing the boundary's solve and joining in place bought.** Timed in a release build at `4cd33f1a3`, two readings each, interleaved with `6d7c23f7e`'s: build 31.7 and 32.0 s against 29.0 and 28.9 s, emit 35.7 and 35.8 s against 17.3 and 16.9 s, with the optimized CPS and the Wasm unchanged byte for byte. The join in place bought about a tenth of build, so what a solve still costs is its rules reading each fact whole, every run copying the sets it reads.
- **The protocol.** A scratch copy of the tree (`git archive HEAD` into `target/scratch/<name>`) built with `--features profile`, a temporary `profile!` span around `ValueFlow::of`, `Module::entry_suspension` and `bound_frames`, and temporary `sample!`s after each solve of its facts' count, the total and the largest of their sets, the module's live nodes, functions and escaping functions, and, in `dataflow::Solver::solve` while a value flow solves, its rules, the runs it made, its joins and the joins that moved a fact. The program is `use /std/{Toml, print};` closing with `match Toml/decode("a = 1") | success(_) => print("decoded\n") | failure(_) => print("refused\n") end`, put to `curios --profile <stream> run` as a file, and the stream folded by `curios profile`; the counts are read off the stream's `V` rows in order.
- **What a solve's cost follows.** The facts are sets of node and function identities, the largest past two thousand after the frame bound copies a recursion's members; the solve's later rounds run a rule about three times and join about a quarter of a million facts.

## Stages

1. **Difference propagation.** A rule hands on what a fact gained since the rule last read it, as points-to solvers do (Pearce, Kelly and Hankin, *Online Cycle Detection and Difference Propagation for Pointer Analysis*, SCAM 2003), so a solve costs the sets its facts end at rather than its runs times their size. Check: the value flow's tests, the corpus's optimized CPS and Wasm unchanged byte for byte over `/std`'s test program and `programs/`, and the protocol above, counted.
2. **One solve a module state.** Whether a round whose passes moved nothing a flow reads can keep the last round's solve, and whether the frame bound's own solve can stand for the boundary's, are questions for the counts stage 1 leaves.

## Retirement

Once a solve's cost follows what its facts gain and the protocol's counts say no stage is left, this specification is deleted.
