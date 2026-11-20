# A loose text is admitted as the unit it is

[An argument names one subject, and each command states what it accepts](../../design/tools/an-argument-names-one-subject-and-each-command-states-what-it-accepts.md) has every command resolve its argument "by one rule into one subject", a loose file "checked as a module when it has no final term", and [the usage reference](../../usage.md#wonder) gives `wonder --stage` a program only "past its three rungs a library has". A loose text written as a module, a file no manifest claims or standard input holding items and no closing term, is no subject the one rule knows: fed at `3459b5a2b` on standard input and as a loose file `double.crs`,

- `wonder`, `wonder --tests`, `lint`, `test`, `--classes`, `--at`, `--name /double/double` and `--name /double/double --stage core` answer it as the module it is, mounted at its stem, `/stdin` on standard input;
- `wonder --stage text`, `core` and `core-elab` refuse it as `not a program`, `this file is written as a module`, where `STAGE_OF_A_UNIT` (`curios/src/contract.rs`) reprints a library's file at those three rungs;
- `document` refuses it as `not a library`, `a program is not one`, of the text `run` calls a module.

`Selection::of` (`curios-package/src/selection.rs`) hands every loose text on as `Selection::Program`, and nothing admitted says which form the text is written in. `Form::of` (`curios-surface/src/module.rs`), which parses a text to tell a module from a program, is asked again by each reader: `RootSource::loose_module` and `supplied_module` (`curios-module/src/root_source.rs`), `opened` and `supplied` (`curios/src/testing.rs`), and `Subject::formed`'s origin (`curios-wonder/src/diagnostics.rs`). So `admit_staged` takes a loose module for a program and `stage` refuses it whole (`Refusal::Unit`, `curios-wonder/src/stage.rs`), `admit_library` words its refusal for a program, and the usage reference's Standard input lists `wonder --stage` among the commands that need a program, against what it says of `wonder`. `--name /double` and `--name /double --exports`, of the module's own path, are refused as [the findings](000-findings.md)' entry on a path that names a module states.

The next step is a decision, *Open*'s; stage 1 follows it.

## Decided

Settled with the user on 2026-11-20.

- **A subject is a unit, and a program is a unit with a closing term.** Which one a loose text is is read once, where it is admitted, and every reader past admission is handed it.
- **A command answers a loose module wherever it answers a library's file.** `--stage` reprints it at the rungs a unit has and refuses the rungs past them as `not a program`; `--name` and its views take it as a scope; `document`, which takes a library, refuses it by what it is.
- **A loose module answers under its name.** Its declarations answer under its stem, and under `/stdin` on standard input, as a library's answer under its package's name and a module a `mod` line names under its own: the stem is the name the file takes the day a `mod` line reaches it. A program is the root of its unit, since nothing imports one, so a file gains the prefix when its closing term goes.

## Open

- **Who holds the form.** `Selection` gains a loose unit, reading the form where it selects, `curios-package` depending on `curios-surface` already; or it stays as it is while the contract's own admissions (`Staged`, `Scope`) carry the form.

## Stages

1. **The form is read once.** A loose text's form is read where the contract selects its subject and travels with what it admits; `opened`, `supplied`, `Subject::formed` and the two `RootSource` constructors read it there, and `Form::of` is asked nowhere else. No answer moves.
2. **A loose module at a unit's rungs.** `admit_staged` and `admit_scope` take a loose module as they take a library's file: `--stage text`, `core` and `core-elab` reprint it, and a rung past them is refused as `not a program`.
3. **A refusal by what it is.** `admit_library` and `admit_program` word a loose module's refusal for a module; the usage reference's Standard input and `wonder` sections state one rule, and where a loose module answers.

## Verification

- Stage 1: `Form::of` has one caller outside `curios-surface`, and every answer of the suite over a loose text stands.
- Stage 2: `curios`'s tests of `--stage text`, `core` and `core-elab` over a loose module, on standard input and as a file, and of `--stage anf` refused as `not a program`, each mutation-checked.
- Stage 3: a test of `document` over a loose module, refused as a module.

## Retirement

When stage 3 lands, its rule lives in the decision this file opens with and in the usage reference, and this file is deleted.
