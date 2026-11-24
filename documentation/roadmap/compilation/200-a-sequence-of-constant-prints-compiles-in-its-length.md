# A sequence of constant prints compiles in its length

**Not refined yet.** The closed evaluator declines a description-bearing replacement past a count of nodes (`DESCRIPTION_COPY_NODE_LIMIT`, `curios-anf/src/optimize/evaluate/budget.rs`), a cap that exists to keep a sequencing chain's continuation from being copied at every link, and at the value the library's write loop needs a chain of constant prints is copied so: its optimized code doubles a print. This specification reserves the criterion that tells a chain's suffix from a performance by what each is, where a count tells them by weight. It waits on that criterion, which no one has drawn; its next step is a bounded investigation.

## What is wrong today

- **The optimized ANF of `n` constant prints doubles a print.** A program of `n - 1` lines `let () = print("line k\n")!;` closed by one more `print` holds 4 calls of `handle_write` in its ANF at every `n`, and 10, 22, 46, 94, 190, 382, 766, 1,534, 3,064 and 6,055 in its optimized ANF at `n` from 1 to 10, `12 * 2^(n-1) - 2`, until the pass's shared pool ends the copying near 12,500 at 16 (counted at `5763feb48`, the lines of `curios wonder --stage anf -` and `--stage anf-optm -` that hold `handle_write(`).
- **A user pays it in compile time and in the module.** `curios run -` over the same program, the debug compiler at `5763feb48`, timed, the median of five interleaved rounds with their span: 8 prints 1.34 s [1.34, 1.36], 10 prints 4.01 s [3.99, 4.03], 16 prints 5.38 s [5.33, 5.41], 24 prints 5.64 s [5.58, 5.86]. With the cap at 64, in a copy of the tree, one reading each with the two caps interleaved: 16 prints take 0.36 s where they take 5.70 s, 128 take 0.80 s where they take 9.09 s, and the text of the module at 128 is 1,034 KB where it is 7,434 KB.
- **The cap's value is the write loop's weight.** Its documentation sets 256 to clear the write through `/std/internal/Handle/retried`, the heaviest description a constant `print` copies, and `curios-prelude-build/std/internal/Handle.crs` writes its loops "so the description a constant `print` copies stays under the evaluator's cap". At 64 the chain above holds 4 calls at every `n`, and no constant `print` folds.
- **The tests pin the arrangement, not what it protects.** `curios/src/tests/fmt.rs`'s `a_constant_prints_descriptions_are_copied_within_the_evaluators_cap` holds the loop's count under the cap, `print_constant_args_collapses_at_anf` asserts one exact line of printed ANF naming `/std/internal/Handle/handle_write(io:1, …` and no `[lambda]` in the optimized CPS, where what it protects is the formatted bytes reaching the host write as one constant, and `a_format_past_the_evaluators_budget_still_prints` never checks that its format is past the budget. Nothing pins the compounding.

## What is known

- **A loop's description is told by its structure already.** `ReifyScope::reaches_candidate` (`curios-anf/src/optimize/evaluate/reify.rs`) tells a loop's description that leads back to its candidate.
- **The write loop keeps a loop of its own under today's cap.** Folded into `/std/internal/until`, the loop the waits on the program's own state go through, it is past the cap, and no constant `print` folds.

## Next step

A bounded investigation. Its question: in the chain above, which replacements copy a link's continuation, the prints after it, and which copy a performance, and what in the term tells the two apart before either is copied? It ends at a criterion under which the chain's optimized ANF holds a number of `handle_write` calls linear in `n` and a constant `print` still reaches the host write as one constant; or at the first program of `programs/` whose formatted print stops folding under every criterion tried, which names what the criterion must keep.

## Verification and retirement

The chain's growth joins `curios`'s `tests::growth`; the three tests above are restated at the grain they protect; `curios-prelude`'s corpus tests pass, and the module sizes over `programs/` are read before and after. Once a criterion lands with them, this file is deleted.
