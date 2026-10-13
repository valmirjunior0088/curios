# A refusal names the form it met

Working specification for the refusals a newcomer meets first: a form another language writes, put where Curios writes another. Where a name does not resolve or a type does not fit, a report says what was written and what may be written instead ([A diagnostic spells what its reader can write](../../design/tools/a-diagnostic-spells-what-its-reader-can-write.md)). Where the grammar meets a form it does not have, the report is the token the parser wanted next, and it names neither the form nor the one Curios has. And in three places a refusal stands away from the mistake it is about, or takes the file's other reports with it, because the parser or the lowering has not committed where its own contract says it has.

It is independent of every other spec.

## What this builds on

- **A parse failure is a message at an offset** (`ParserError`, `curios-parse/src/error.rs`), and where nothing rewords it the message is the literal's own: `Expected '{expected}', obtained …`.
- **The parser already rewords where it knows the mistake.** `refuse_asymmetric_operator` (`curios-text/src/parse/expr.rs`) probes what follows a finished operand without consuming and refuses a glued operator as "an infix operator takes whitespace on both sides"; a comment without its space, a string that meets its line's end, a reserved word that begins a declaration where a term is read (`refuse_declaration_head`) and a head that names no item after `pub` (`NOT_A_TOP_LEVEL_ITEM`) each have a sentence of their own. `WIRE_RESULT_IS_A_TYPE` (`curios-text/src/parse/top_level.rs`) is the closest to what this spec asks: "`()` is the unit value, and a result is a type: a row that answers nothing is written `-> {}`".
- **A program is items and then one term** (`Entrypoint::parse_reading`, `curios-text/src/module.rs`): the item loop, `parse_term`, and `parse_program_end` (`curios-text/src/parse.rs`), which is `take_eof` beside one refusal for a documentation block. Whatever stands past the final term is reported by `take_eof` as `Expected 'end-of-file'`.
- **An unrecognized head falls through to the tail.** `parse_top_item` commits on `mod`, `use`, `induct`, `struct` and `foreign` and lets every other head fail recoverably, so a word that names no item is read as the program's final term and what follows it meets `parse_program_end`.
- **A contextual head falls through only so far.** `parse_top_item`'s contract has a `let` fall through "only as far as the `=`", since past it "the local reading is the same text failing the same way", ends a `test`'s fall-through at its label because "`test name` is two names in a row and so no term at all", and ends a `satisfy`'s at the concept's name; a `pub` or a documentation block in front commits every arm. Past that point a failure is the item's own diagnosis, and `Module::parse` (`curios-text/src/module.rs`) states that "one mistake costs one item's diagnosis rather than the file's".
- **The match lowering raises its refusals bare.** `Error::MatchNamedCatchAll`, `Error::BindArmIrrefutable` and `Error::MatrixMotiveRequiresCtorHead` are raised where the lowering holds the arm's pattern or the motive (`curios-text/src/into_core/match_compile.rs`). A binder is a label and a motive a term, so each is written at a place, and [A diagnostic spells what its reader can write](../../design/tools/a-diagnostic-spells-what-its-reader-can-write.md) places a refusal at "the narrowest written term it is about".
- **Curios has no juxtaposition.** A call fills one parenthesized list, a path is whitespace-free and an infix operator takes whitespace on both sides ([`documentation/syntax.md`](../../syntax.md)), so two operands with nothing between them are no form, wherever they stand.
- **The rule for where a refusal belongs is stated.** "A parser alternative commits once it has read the prefix that discriminates it, and asks for commitment explicitly" (`.claude/rules/surface.md`).

## The gap

Probed at `d5046c5cc`, each on standard input.

| Written | Reported |
| --- | --- |
| two `print(…)` lines closing a program | `Expected 'end-of-file'`, at the second |
| `print("one\n");` or `print("one\n")!;` before the last line | `Expected 'end-of-file'`, at the `;` |
| `fn double(n: Nat) -> Nat { … }`, `def double(…) = …;`, `type Row = List(Nat);` | `Expected 'end-of-file'`, after the first word |
| `if n > 10 then "big" else "small"` | `Expected ';', obtained 'n'` |
| `return n + n` | `Expected ';', obtained 'n'` |
| `add 1 2` | `Expected ';', obtained '1'`, or `Expected ')'` in an argument |
| `struct Point { x: Nat, y: Nat }` | `Expected ':', obtained '{'` |
| `let double(n: Nat) = n + n;` at the top level | `Expected '->', obtained '='` |
| `let x: Nat := 3;` | `Expected '=', obtained ':'` |
| `List/map(xs, x => x + 1)` | `Expected ')', obtained '='` |
| `match n { 0 => "zero", … }` | `Expected '}', obtained '='` |
| `xs.len()` | `projected from a non-tuple`, `head has type: List(Nat)` |
| `Io(())` | `introduced a tuple where the expected type is not a tuple type`, `expected: Sort` |
| `Fmt/print("% + % = %\n")(1, 2)` | `wrong number of arguments: expected 1, got 2` |
| `\| none => 0` over an `Option` | `a named final arm cannot be a catch-all; write \`\| _ =>\` for a default, or name the constructor` |

The first six rows are three positions: past a program's final term, two terms side by side, and — for the word read as the tail — both. The next five are a declaration or a form missing a part its grammar requires. The last four are decided where types are known, and each is exact there: a label that is a function of the head's own module, a unit value where a type stands, a result that is itself a function, a binder spelled as a constructor of the scrutinee's type.

Three shortfalls are of where a refusal stands and of what it costs, whatever it says, probed at `653af23d2`:

- **A private `let` that fails at its `;`, and a private `concept` that fails outside a field's type, take the file's diagnosis with them.** `parse_top_let` (`curios-text/src/parse/top_level.rs`) reads the `;` that closes a group with nothing committed, where the body before it commits, and `parse_concept_member` commits nothing but a field's type, past its `:` or `->`. So `let b(n: Nat) -> Nat = n +;`, a `let` or a group whose `;` is left out, and a `concept` with no sort, no closing brace or no comma between two fields each fail recoverably: the item loop ends there and the file is refused with that one report, a program by `run -` and a module by `wonder diagnostics -` alike. With `let a: Nat = "one";` before the item and `let c: Str = 3;` after it, neither mismatch is reported, where the same file reports all three once the item is `pub`, where the `let`'s body is what fails, `(n + 1;`, and where a `satisfy` is left unclosed. The `concept`'s one report is the tail's besides, `Expected 'end-of-file'` with its caret under the concept's name, where the `pub` one reports `Expected '}'` at the token that stands where the brace is missing. `concept Name` is two names in a row, as `test name` is.
- **A constructor written with no payload list is reported at its bar, as a missing `end`.** `induct Color: Type | red | green end` reports `Expected keyword 'end', obtained '|'` with its caret under the bar before `red`, and `induct Shade: Type | dark() | light end` under the bar before `light`. `parse_top_induct_case` (`curios-text/src/parse/top_level.rs`) reads the bar, the label and then `(` with nothing committed, so a missing `(` backtracks past the bar, the case list ends before that case, and the `end` that follows the list complains at the bar. A bar in a case list begins a case and nothing else; a `match` arm and a `choose` arm that lack a pattern, a guard or an `=>` are each reported where the part is missing.
- **The match lowering's three refusals stand at the head of the match, not at the arm or the motive they are about.** `match o | some(n) => n | none => 0 end` reports its named final arm with the caret under `match o`; a `choose` whose second arm is `| n = o => 0` reports that a bind arm needs a refutable pattern under `choose`, two lines above the arm; and a motive written over a tuple-headed match is refused under the match's first line.

## Prior art

- **A few messages are most of what a novice meets.** Over a large body of novice programs, Python's most frequent report is `SyntaxError: invalid syntax` and Java's second is `';' expected`, and the counts fall off as a power law, so wording a handful of reports reaches most encounters (Pritchard, [Frequency distribution of error messages](https://doi.org/10.1145/2846680.2846681), PLATEAU 2015; summarized at [It Will Never Work in Theory](https://neverworkintheory.org/2016/06/12/error-message-frequency.html)).
- **Elm rewrote its syntax reports to teach the syntax.** In 0.19.1 an import written as JavaScript writes it is reported as an import that expected a module name, with the forms Elm has beside it, the stated aim being a compiler "actually *helping* them learn Elm syntax" ([InfoQ, 2020](https://www.infoq.com/news/2020/01/elm-learn-syntax-error-message)).
- **Python names the form it thinks was meant.** `import x from y` is reported as `Did you mean to use 'from ... import ...' instead?` since 3.12 ([What's new in Python 3.12](https://docs.python.org/3/whatsnew/3.12.html)).
- **Complex syntax and unclear error messages are the usability problems reported of proof assistants in teaching**, from a user study of students in Coq and a heuristic evaluation of Coq and Lean (Mahinpei, Horta Ribeiro and Milano, [Interactive Theorem Provers for Proof Education](https://2025.splashcon.org/details/splash-2025-SPLASH-E/3/Interactive-Theorem-Provers-for-Proof-Education), SPLASH-E 2025).

## Decisions

None is settled. What the tree's rules already fix:

- **Nothing here adds a form.** Syntax forms are closed, and each row is refused after this spec as before; only what the refusal says and where it stands move.
- **A refusal is worded where the parser has read what discriminates the mistake**, by a commitment it asks for, never by a guess after the failure.

## Open questions

- **Whether a refusal may read the word that was written.** Two terms side by side can be refused as that, naming both, with the call's spelling beside it; it cannot say that `if` is no form of Curios without knowing `if`. A table of words other languages reserve — `if`, `else`, `then`, `return`, `fn`, `def`, `fun`, `function`, `var`, `const`, `type`, `enum`, `data`, `class`, `import`, `for`, `while` — each with the form Curios writes, is what Elm and Python do. The tree states that nothing branches on a name's spelling; a note added to a refusal that already stands changes no verdict and no place. Recommended, and not settled: the positions first, worded by what the grammar read; the table as the last stage, as a note line alone, decided once the stages before it show what the positions already say.
- **What the refusal past a final term says of the term before it.** `print("one\n")` followed by a second term is most often a sequence, and the form is `let _ = print("one\n")!;`. Saying so presumes the first is an effect, which the parser does not know. Recommended: the refusal states the rule — a program closes with one term, the description it performs — names the second term as one too many, and gives the sequencing form as what is written where the first is an action.
- **Where the binder spelled as a constructor is worded.** The refusal is the lowering's (`Error::MatchNamedCatchAll`, `curios-text/src/into_core/match_compile.rs`), which holds no type. Recommended: it stays there, standing at the binder once the first stage lands, and its sentence gains what holds without a type, that a constructor is written with its payload list, `none()`.
- **`(x) -> x + 1` where a lambda was meant** parses as a function type and is reported as `` `x` is not in scope ``. Nothing short of the expected type tells it from a function type written on purpose. Left out unless the fifth stage finds it exact.

## Stages

1. **A refusal stands where the mistake is, and costs one item.** A top-level `let`'s `;` is read committed once its first member's body is; a `concept`'s fall-through ends at its label, as a `test`'s does; an `induct` case commits past its bar, where no documentation block has decided otherwise; and the match lowering's first two refusals stand at the binder and its third at the motive. No sentence is reworded. Check: each file of the gap's first shortfall reports its two mismatches beside the parse failure, as tests in `curios-text/src/parse/recovery_tests.rs`, with `an_uncommitted_failure_still_ends_the_loop` passing untouched and a program whose tail calls a function named `concept` compiling as it did; the two `induct` programs report `Expected '('` after the label, as tests in `curios-text/src/parse/top_level_tests.rs`; and the three lowering reports stand where they are about, as fixtures.
2. **Past a program's final term.** `parse_program_end` probes what stands there: a term, a `;`, or the end. Check: fixtures for the first three rows, each standing where it stands today and saying what a program closes with; a documentation block there still reports as it does.
3. **Two terms side by side.** A probe beside `refuse_asymmetric_operator`, once an operand has ended and what follows begins one. Check: `if n > 10 then …`, `return n + n` and `add 1 2` in a `let`'s body, in an argument and in an arm's body; and every file of `curios-std/src/`, `programs/` and `editors/grammar/test/corpus/` parses to what it parsed to, which is what shows the probe refuses nothing the grammar has.
4. **A head or a form missing its part.** A structure's or a concept's sort, a top-level `let`'s result, `:=`, a lambda's parentheses, a constructor's payload list. Each is a sentence at a commitment the alternative owes, which the first stage has made where it was missing. Check: a fixture a row, and the parser's own tests unchanged elsewhere.
5. **The forms decided where types are known, and the named final arm.** A label that the head's own module declares as a function, where the head is no record; `()` where a type is expected; arguments past a call's list where the result is itself a function. Each names what is written instead: `List/len(xs)`, `{}`, `f(a)(b)`. The named final arm's sentence is the lowering's, as the third open question settles it. Check: a fixture each, and one for each where the condition does not hold and the report is the one it was.
6. **The note for a reserved word of another language**, if the first open question settles for it. Check: a fixture a word, and that a program which declares a function under one of the words and calls it compiles as it did.

## Verification

`cargo xtask workspace lint` and `cargo xtask workspace format` after each stage, with the parser's tests and the stage's fixtures by name; a stage that rewords or moves a report `curios-pipeline` or `curios`'s tests pin moves that pin in the same commit. No stage changes what parses, so `editors/grammar` and `documentation/syntax.md` are not reached; the third stage's sweep is what holds that.

## Rejected

- **Recovering the form and compiling it** — reading `if c then a else b` as the match it means. A second spelling of what already has one is what [Syntax forms are closed](../../design/surface/syntax-forms-are-closed-semantics-extend-by-witness.md) rules out, and a program that compiles under a form the reference does not have teaches that form.
- **A classifier that reads the source again once the parse has failed.** It guesses from text what the parser knew where it failed, and stands wherever the generic failure stood.

## Completion and retirement

Each sentence lives at the refusal that says it, held by its fixture; `parse_top_item`'s contract states where a `concept`'s fall-through ends, as it states a `test`'s; [A diagnostic spells what its reader can write](../../design/tools/a-diagnostic-spells-what-its-reader-can-write.md) takes one paragraph stating that a form the grammar does not have is refused by name with the form it has. The line in `documentation/roadmap.md` becomes part of the checked line that cites the decision, and this file is deleted.
