# A loose text is admitted as the unit it is

[An argument names one subject, and each command states what it accepts](../../design/tools/an-argument-names-one-subject-and-each-command-states-what-it-accepts.md) has every command resolve its argument "by one rule into one subject", a loose file "checked as a module when it has no final term". The one rule (`Selection::of`, `curios-package/src/selection.rs`) hands every loose text on as `Selection::Program`, so no admission knows what a loose text is, and each reader past it reads the form again and answers by its own reading. Fed at `0c26dff30` a loose module, `double.crs` and the same text on standard input:

- `wonder`, `wonder --tests`, `lint`, `test`, `--classes`, `--at`, `--name /double/double` and `--name /double/double --stage core` answer it as the module it is, under its stem, `/stdin` on standard input;
- `wonder --stage text`, `core` and `core-elab` refuse it as `not a program`, `this file is written as a module`, where `STAGE_OF_A_UNIT` (`curios/src/contract.rs`) reprints a library's file at those three rungs; a loose module of Core text, `dmod.crsc`, is refused at `core-elab` too, its first rung, since `enter_core_text` (`curios-pipeline/src/read.rs`) emits it on a program's branch alone;
- `document` refuses it as `not a library`, `a program is not one`, of the text `run` calls a module;
- `run` and `compile` admit it, as `curios/src/contract/tests.rs` pins `run stray.crs` admitted as `loose stray.crs` for an empty file, and refuse it only once the program grammar fails at its end;
- under a stem that is no name, `my-file.crs`, the check answers `unnameable module` and exits 0, `test` refuses it as `not a program` and `--name` as a `syntax error`, `expected: a term`: three readings of one text.

`Form::of` (`curios-surface/src/module.rs`) is asked by `RootSource::loose_module` and `supplied_module` (`curios-module/src/root_source.rs`), `opened` and `supplied` (`curios/src/testing.rs`), and `Subject::unnameable` (`curios-wonder/src/diagnostics.rs`), which reads Core text's form beside it, and `Subject::formed` reaches it through the first two. Standard input cannot be read at admission as it stands: it is drained past it, by `drained` (`curios/src/pipeline.rs`) for `run`, `compile`, `test` and `format` and by `read_stdin` (`curios-wonder/src/ask.rs`), a second spelling of the same read, for `wonder` and `lint`, and `--core` is applied to what was admitted by `read_stdin_as` at eight sites of `curios/src/main.rs`.

Beside it, a declared executable whose entry is written as a module, `exe.crs`, is refused by `run` and `test` as `not a program`, through `opened`'s second reading, and by `wonder`, `--stage` and `--name` as a `syntax error`, `expected: a term`, `obtained: the end of the file`: the entry grammar's own failure, `expected: a program's final term` (`Entrypoint::parse_reading`), loses to the term parser's at the end of the file.

The usage reference's Standard input lists `wonder --stage` among the commands that need a program, against what it says of `wonder`.

The next step is delivery, stage 1.

## Decided

Settled with the user on 2026-11-20 and 2026-11-21.

- **A subject is a unit, and a program is a unit with a closing term.** Which one a loose text is is read once, where it is admitted, and every reader past admission is handed it.
- **The selection holds the form.** `Selection::of` reads a loose text's form as it places it, in either dialect, and selects a loose module as a unit of its own beside a library; the server, which admits through it, is handed the same.
- **Standard input is read once, at admission.** What it holds is drained where its subject is selected, and travels with what is admitted; `--core` is told to admission, as a file's name tells it.
- **A command answers a loose module wherever it answers a library's file.** `--stage` reprints it at the rungs a unit has, a module of Core text at `core-elab`, and refuses the rungs past them as `not a program`; `--name` and its views take it as a scope; `run`, `compile`, `--false` and `--footprint` refuse it at admission; `document`, which takes a library, refuses it by what it is.
- **A loose module answers under its name.** Its declarations answer under its stem, and under `/stdin` on standard input, as a library's answer under its package's name and a module a `mod` line names under its own: the stem is the name the file takes the day a `mod` line reaches it. A program is the root of its unit, since nothing imports one, so a file gains the prefix when its closing term goes. A stem that is no name is refused at admission, by every command alike, as a subject no question can be asked of.
- **A program's text names its missing term.** A declared entry is a program by its declaration, so a text of one closing with no term is refused by the program grammar, in the grammar's own words, by every reader.

## Stages

1. **The grammar names a program's final term.** A text the entry grammar reads to its end with no closing term is refused as missing a program's final term, by every reader of a declared entry.
2. **Standard input is read at admission.** The selection takes standard input's text and what it is written in, the admitted program holds it, and `drained`, `read_stdin` and every `read_stdin_as` go. No answer moves.
3. **The form is read once.** `Selection::of` selects a loose module as a unit; `test_subjects`, `Asked::every` and the admissions are handed it, `opened`, `supplied`, `Subject::formed` and `Subject::unnameable` read no form, and `Form::of` is asked nowhere else. `run`, `compile`, `--false` and `--footprint` refuse a loose module at admission, and a stem that is no name is refused there by every command.
4. **A loose module at a unit's rungs.** `admit_staged` and `admit_scope` take a loose module as they take a library's file: `--stage text`, `core` and `core-elab` reprint it, from its file or from standard input's text, a loose module of Core text reprints its unit at `core-elab`, and a rung past them is refused as `not a program`.
5. **A refusal by what it is.** `admit_library` words a loose module's refusal for a module; the decision this file opens with and the usage reference's Subjects, Standard input and `wonder` sections state one rule, and where a loose module answers.

## Verification

- Stage 1: a test of `wonder` and of `run` over a declared entry written as a module, each refused as missing a program's final term, mutation-checked.
- Stage 2: the contract table pins `-` for every command over a text it is handed, and every answer of the suite over standard input stands.
- Stage 3: `Form::of` has one caller outside `curios-surface`; the contract table pins a loose module, on standard input and as a file, admitted as a unit by every command that takes one and refused by `run` and `compile`; a test of a stem that is no name, refused alike by `wonder`, `test` and `--name`.
- Stage 4: `curios`'s tests of `--stage text`, `core` and `core-elab` over a loose module, on standard input and as a file, of `core-elab` over a loose module of Core text, and of `--stage anf` refused as `not a program`, each mutation-checked.
- Stage 5: a test of `document` over a loose module, refused as a module.

## Retirement

When stage 5 lands, its rule lives in the decision this file opens with and in the usage reference, and this file is deleted.
