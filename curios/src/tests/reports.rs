//! What a refusal's report says and where it points, for refusals `tests::rules` holds as patterns over the checker's error: a report is read by a person, so its words and its caret are claims of their own, which no pattern over an error states.

use {crate::test_support::*, curios_common::fill};

// The refusal names the declaration by the keyword it is written with and stands at the field's type, spelling the field as it is written: a superclass edge under `use`, a labeled field by its label, and a method or field written as a signature over the whole of it.
#[test]
fn an_informative_field_is_reported_as_written_at_its_type() {
    for (source, words, place) in [
        (
            "use /std/{Show};\nconcept Carry(A: Type): Prop { use Show(A), }\n/std/print(\"no\")\n",
            "the concept `Carry` is declared at `Prop`\n│  its field `use Show(A)` is no proposition\n│  every field of a `Prop` concept is a proposition\n",
            "│  2 │ concept Carry(A: Type): Prop { use Show(A), }\n│    │                                    ^^^^^^^",
        ),
        (
            "use /std/{Nat};\nuse /std/Bool/{True};\npub struct Bad: Prop { first: True, value: Nat, }\n/std/print(\"no\")\n",
            "the struct `Bad` is declared at `Prop`\n│  its field `value: Nat` is no proposition\n│  every field of a `Prop` struct is a proposition\n",
            "│  3 │ pub struct Bad: Prop { first: True, value: Nat, }\n│    │                                            ^^^",
        ),
        (
            "use /std/{Nat};\nconcept Bad(A: Type): Prop { get(A) -> Nat, }\n/std/print(\"no\")\n",
            "the concept `Bad` is declared at `Prop`\n│  its field `get: (A) -> Nat` is no proposition\n│  every field of a `Prop` concept is a proposition\n",
            "│  2 │ concept Bad(A: Type): Prop { get(A) -> Nat, }\n│    │                              ^^^^^^^^^^^^^",
        ),
        (
            "use /std/{Nat};\npub struct Bad: Prop { get(Nat) -> Nat, }\n/std/print(\"no\")\n",
            "the struct `Bad` is declared at `Prop`\n│  its field `get: (Nat) -> Nat` is no proposition\n│  every field of a `Prop` struct is a proposition\n",
            "│  2 │ pub struct Bad: Prop { get(Nat) -> Nat, }\n│    │                        ^^^^^^^^^^^^^^^",
        ),
    ] {
        let report = error(source);
        assert!(report.contains(words), "expected the words:\n{report}");
        assert!(report.contains(place), "expected the place:\n{report}");
    }
}

/// A witness refusal is located at the concept application the author wrote, under a telescope as without one.
///
/// A `satisfy` block has no name, so its declared type is the only thing a caret can point at — and that type is synthesized during lowering (`witness_concept_application`), which carries the application's span — without it, every duplicate, orphan, unkeyable and irregular-premise refusal would arrive with no line at all, naming only a module that may hold a dozen witnesses. Under a telescope the declared type is a `FuncType` built around the application, so it carries the span too.
#[test]
fn a_witness_refusal_is_located_at_its_concept_application() {
    let telescoped = error(
        r#"
        use /std/{Str, Show, List};
        satisfy (@A : Type, use Show(A)) => Show(List(A)) {
            show(v) = "l"
        }
        /std/Io/pure(())
        "#,
    );
    assert!(telescoped.contains("orphan witness"), "{telescoped}");
    assert!(telescoped.contains("=> Show(List(A)) {"), "{telescoped}");

    // The nullary form, whose declared type *is* the application.
    let bare = error(
        r#"
        use /std/{Str, Show, Nat};
        satisfy Show(Nat) {
            show(n) = "n"
        }
        /std/Io/pure(())
        "#,
    );
    assert!(bare.contains("orphan witness"), "{bare}");
    assert!(bare.contains("satisfy Show(Nat) {"), "{bare}");
}

// An operation that is another's operand reports its own refusal at itself: its operator's bound, which nothing in a chain discharges, is placed at the operation written for it, on either side of the operator it stands beside, as it is where it stands alone.
#[test]
fn an_operations_bound_is_reported_at_the_operation_inside_a_chain() {
    for (source, place) in [
        (
            "use /std/{Bool, Nat};\nlet f(x: Nat) -> Bool =\n    10 / x > 1;\n/std/print(\"no\")\n",
            "│  3 │     10 / x > 1;\n│    │     ^^^^^^",
        ),
        (
            "use /std/{Bool, Nat};\nlet f(x: Nat) -> Bool =\n    1 < 10 / x;\n/std/print(\"no\")\n",
            "│  3 │     1 < 10 / x;\n│    │         ^^^^^^",
        ),
        (
            "use /std/{Nat};\nlet f(x: Nat) -> Nat =\n    10 / x;\n/std/print(\"no\")\n",
            "│  3 │     10 / x;\n│    │     ^^^^^^",
        ),
    ] {
        let report = error(source);
        assert!(
            report.contains("the bound of the `/` operator was not discharged"),
            "expected the bound's refusal:\n{report}"
        );
        assert!(report.contains(place), "expected the place:\n{report}");
    }
}

// A goal stands where its `?` was written, which its birth records, whatever term it is later read in: the whole argument of a method named by its path, hidden or not, and a structure former's parameter, whose call is made as what it reduces to. Read off the term it was met in, each stood at the definition holding it.
#[test]
fn a_goal_stands_where_its_question_mark_was_written() {
    for (declared, written) in [
        (
            "",
            "let twice(a: Nat) -> Nat = /std/ops/Add/add(@Nat, a, ?);",
        ),
        (
            "",
            "let half(a: Nat) -> Nat = match a > 0 | true => /std/ops/Div/div(@Nat, a, a, @?) | false => 0 end;",
        ),
        (
            "struct Pair(A: Type, B: Type): Type { fst: A, snd: B }",
            "let first(a: Nat) -> Nat = let p: Pair(?, Nat) = Pair { fst = a, snd = a }; p.fst;",
        ),
    ] {
        let report = error(&fill!(
            "use /std/{Nat}; ${declared}\n${written}\n/std/Io/pure(())\n",
            declared,
            written
        ));
        let column = written.find('?').expect("a goal is written") + 1;
        assert!(
            report.contains(&format!("[goal] 2:{column}")),
            "{written}: {report}"
        );
    }
}

// One refusal of each kind a reader meets as they write, raised once, with its words and where it points: the headline names the fault, each fact speaks of what the reader wrote, and the caret stands on it. Each kind's own rule has a test of its own; this one holds the whole of what each says, so a change to any is seen here.
#[test]
fn each_refusal_kind_reads_as_its_reader_wrote_at_its_place() {
    for (source, words, place) in [
        // Repeated field.
        (
            "use /std/{Nat};\nstruct Point: Type { x: Nat, y: Nat }\nlet p: Point = Point { x = 1, x = 2, y = 3 };\n/std/print(\"\")\n",
            "repeated field\n│  `x` is written twice\n│  a literal writes the fields of `Point` in their order, each once\n│  order: `x`, `y`\n",
            "│  3 │ let p: Point = Point { x = 1, x = 2, y = 3 };\n│    │                                   ^",
        ),
        // Field out of order.
        (
            "use /std/{Nat};\nstruct Point: Type { x: Nat, y: Nat }\nlet p: Point = Point { y = 2, x = 1 };\n/std/print(\"\")\n",
            "field out of order\n│  `x` is written after a field declared after it\n│  a literal writes the fields of `Point` in their order, each once\n│  order: `x`, `y`\n",
            "│  3 │ let p: Point = Point { y = 2, x = 1 };\n│    │                                   ^",
        ),
        // Mark mismatch.
        (
            "use /std/{Nat};\nstruct Point: Type { x: Nat, y: Nat }\nlet p: Point = Point { @x = 1, y = 2 };\n/std/print(\"\")\n",
            "mark mismatch\n│  `x` is a plain field of `Point`\n│  a plain field is written with no mark\n│  write it `x = ...`\n",
            "│  3 │ let p: Point = Point { @x = 1, y = 2 };\n│    │                             ^",
        ),
        // Method count mismatch.
        (
            "use /std/{Nat, Show};\nstruct Point: Type { x: Nat }\nsatisfy Show(Point) { }\n/std/print(\"\")\n",
            "method count mismatch\n│  `Show` has 1 method\n│  written: 0\n│  missing: `show`\n",
            "│  3 │ satisfy Show(Point) { }\n│    │         ^^^^^^^^^^^",
        ),
        // Not a function.
        (
            "use /std/{Nat, Fmt};\nFmt/print(\"%\\n\")(1)(2)\n",
            "not a function\n│  applied: a term of type `/std/Io({})`, which takes no argument\n",
            "│  2 │ Fmt/print(\"%\\n\")(1)(2)\n│    │ ^^^^^^^^^^^^^^^^^^^^^^",
        ),
        // No witness for a closing function.
        (
            "use /std/{Nat, Fmt};\nFmt/print(\"% and %\\n\")(1)\n",
            "no witness\n│  wanted: `/std/Program((@A: Type, use /std/Show(A), a: A) -> /std/Io({}))`\n│  a program closes with an action a `Program` witness runs, such as an `Io({})`\n│  this one's type is `(@A: Type, use /std/Show(A), a: A) -> /std/Io({})`\n│  it is a function still to be called with what it takes\n",
            "│  2 │ Fmt/print(\"% and %\\n\")(1)\n│    │ ^^^^^^^^^^^^^^^^^^^^^^^^^",
        ),
        // Type mismatch at a bang.
        (
            "use /std/{Nat, Io};\nlet both() -> Io({}) =\n    let x = 1!;\n    Io/pure(());\nboth()\n",
            "type mismatch\n│  inferred: `Nat`\n│  expected: `Io(?)`\n",
            "│  3 │     let x = 1!;\n│    │             ^",
        ),
        // Undetermined type.
        (
            "use /std/{Io, Str, Bool, Nat};\nconcept Joint(T: Type, R: Type): Type {\n    run(T) -> Io(R),\n}\nsatisfy (@A: Type, @B: Type, @C: Type) => Joint({Io(A), Io(B), Io(C)}, {A, B, C}) {\n    run(t) =\n        let (x, (y, z)) = Io/both(t.0, Io/both(t.1, t.2))!;\n        Io/pure((x, y, z)),\n}\nlet all_of(@T: Type, @R: Type, use Joint(T, R), actions: T) -> Io(R) =\n    Joint/run(actions);\nlet execute() -> Io({}) =\n    let (n, s, b) = all_of((Io/pure(7), Io/pure(\"seven\"), Io/pure(true)))!;\n    Io/pure(());\nexecute()\n",
            "undetermined type\n│  a tuple pattern of 3 fields takes apart a value whose type is not known here\n│  write the type where the value is bound, `let x: {A, B} = ...;`, and take `x` apart\n",
            "│  13 │     let (n, s, b) = all_of((Io/pure(7), Io/pure(\"seven\"), Io/pure(true)))!;\n│     │         ^^^^^^^^^",
        ),
        // Declaration cycle.
        (
            "use /std/{Nat};\nconcept A(T: Type): Type { use B(T), }\nconcept B(T: Type): Type { use A(T), }\n/std/print(\"\")\n",
            "declaration cycle\n│  `A` and `B` need one another\n│  each reads another before it can be elaborated, so none can be first\n│  declare types and concepts that name one another as one group, `struct ... and ...` or `concept ... and ...`\n│  a concept's `use` fields form no cycle, grouped or not\n",
            "│  3 │ concept B(T: Type): Type { use A(T), }\n│    │         ^",
        ),
        // Undischarged bound under guards.
        (
            "use /std/{Nat, List};\nlet pick(xs: List(Nat), i: Nat) -> Nat =\n    match i <= List/length(xs) | true => List/get(xs, i) | false => 0 end;\n/std/print(\"\")\n",
            "undischarged bound\n│  the bound `ok` of `List/get` was not discharged\n│  nothing discharged `/std/Bool/Holds(i < List/length(xs))`\n│  it does not follow from the facts in scope:\n│    a guard, `i <= List/length(xs)`\n│  it fails at `i = 0`, `List/length(xs) = 0`\n│  guard the call with `i < List/length(xs)`\n│  or supply it explicitly, `List/get(@...)`\n",
            "│  3 │     match i <= List/length(xs) | true => List/get(xs, i) | false => 0 end;\n│    │                                          ^^^^^^^^^^^^^^^",
        ),
        // Undischarged bound of an operator.
        (
            "use /std/{Nat};\nlet share(total: Nat, people: Nat) -> Nat = total / people;\n/std/print(\"\")\n",
            "undischarged bound\n│  the bound of the `/` operator was not discharged\n│  nothing discharged `/std/ops/Div/Ok(people)`\n│  it fails at `people = 0`\n│  an operator takes no written arguments\n│  guard the operation with `people != 0`\n│  or call `/std/ops/Div/div(@T, a, b, @proof)`\n",
            "│  2 │ let share(total: Nat, people: Nat) -> Nat = total / people;\n│    │                                             ^^^^^^^^^^^^^^",
        ),
        // Undischarged bound over a disequality, written back as the `!=` that elaborates to its negation.
        (
            "use /std/{Nat};\nuse /std/Bool/{Holds};\nlet needs(b: Nat, @_: Holds(b != 0)) -> Nat = b;\nlet call(b: Nat) -> Nat = needs(b);\n/std/print(\"\")\n",
            "undischarged bound\n│  the bound of `needs` was not discharged\n│  nothing discharged `Holds(b != 0)`\n│  it fails at `b = 0`\n│  guard the call with `b != 0`\n│  or supply it explicitly, `needs(@...)`\n",
            "│  4 │ let call(b: Nat) -> Nat = needs(b);\n│    │                           ^^^^^^^^",
        ),
        // Undischarged bound over `>`, written back as the `<` over the operands swapped that it elaborates to.
        (
            "use /std/{Nat};\nuse /std/Bool/{Holds};\nlet needs(b: Nat, @_: Holds(b > 3)) -> Nat = b;\nlet call(b: Nat) -> Nat = needs(b);\n/std/print(\"\")\n",
            "undischarged bound\n│  the bound of `needs` was not discharged\n│  nothing discharged `Holds(3 < b)`\n│  it fails at `b = 0`\n│  guard the call with `3 < b`\n│  or supply it explicitly, `needs(@...)`\n",
            "│  4 │ let call(b: Nat) -> Nat = needs(b);\n│    │                           ^^^^^^^^",
        ),
        // Undischarged bound over a product, which the search reads as an unknown of its own.
        (
            "use /std/{Nat};\nuse /std/Bool/{Holds};\nlet small(x: Nat, y: Nat, @_: Holds(x * y <= 10)) -> Nat = x;\nlet call(x: Nat, y: Nat) -> Nat = small(x, y);\n/std/print(\"\")\n",
            "undischarged bound\n│  the bound of `small` was not discharged\n│  nothing discharged `Holds(x * y <= 10)`\n│  the search finds no proof, and no values under which it fails\n│  it reads a product of unknowns as an unknown of its own\n│  supply it explicitly, `small(@...)`\n",
            "│  4 │ let call(x: Nat, y: Nat) -> Nat = small(x, y);\n│    │                                   ^^^^^^^^^^^",
        ),
        // Argument count mismatch.
        (
            "use /std/{Nat};\nlet double(n: Nat) -> Nat = n + n;\nlet four: Nat = double(1, 2);\n/std/print(\"\")\n",
            "argument count mismatch\n│  written: 2\n│  `double` takes 1 argument\n",
            "│  3 │ let four: Nat = double(1, 2);\n│    │                 ^^^^^^^^^^^^",
        ),
        // Surplus member.
        (
            "use /std/{Nat};\nlet double(n: Nat) -> Nat = n + n;\nlet two: Nat = double(@1);\n/std/print(\"\")\n",
            "surplus member\n│  `double` takes no `@` argument\n│  write it with no mark\n",
            "│  3 │ let two: Nat = double(@1);\n│    │                        ^",
        ),
        // No witness for a returned action.
        (
            "use /std/{Nat, Io, Option};\nlet wrap() -> Option(Nat) = Io/pure(1);\n/std/print(\"\")\n",
            "no witness\n│  wanted: `/std/Monad/Lift(Io, Option)`\n│  needed to return an `Io` action where this `Option` region's is expected\n",
            "│  2 │ let wrap() -> Option(Nat) = Io/pure(1);\n│    │                             ^^^^^^^^^^",
        ),
        // A word another language reserves.
        (
            "use /std/{Nat};\nlet big(n: Nat) -> Nat = if n > 10 then 1 else 0;\n/std/print(\"\")\n",
            "syntax error\n│  `if` is no keyword of Curios: a choice is a `match` on its condition, `match c | true => a | false => b end`\n",
            "│  2 │ let big(n: Nat) -> Nat = if n > 10 then 1 else 0;\n│    │                          ^",
        ),
        // A run between single quotes.
        (
            "use /std/{Str};\nlet s: Str = 'hello';\n/std/print(s)\n",
            "syntax error\n│  a character literal holds one character\n│  a string is written between double quotes, `\"...\"`\n",
            "│  2 │ let s: Str = 'hello';\n│    │              ^^",
        ),
        // No such field.
        (
            "use /std/{Nat, List};\nlet count(xs: List(Nat)) -> Nat = xs.len();\n/std/print(\"\")\n",
            "no such field\n│  `.len` reads a field, and a `List(Nat)` has none\n",
            "│  2 │ let count(xs: List(Nat)) -> Nat = xs.len();\n│    │                                   ^^^^^^",
        ),
        // Not a concept.
        (
            "use /std/{Nat};\nsatisfy Nat(Nat) { }\n/std/print(\"\")\n",
            "not a concept\n│  `Nat(Nat)` applies a head that is no concept\n│  a `satisfy` names the concept it witnesses, `satisfy Show(Point) { ... }`\n",
            "│  2 │ satisfy Nat(Nat) { }\n│    │         ^^^^^^^^",
        ),
        // Ambiguous witness.
        (
            "use /std/{Str, Show};\nconcept Pretty(T: Type): Type { use Show(T), }\nconcept Loud(T: Type): Type { use Show(T), }\npub let say(@T: Type, use Pretty(T), use Loud(T), x: T) -> Str = Show/show(x);\n/std/print(\"\")\n",
            "ambiguous witness\n│  wanted: `Show(T)`\n│  `use Pretty(T)` and `use Loud(T)` both hold it as a superclass, neither nearer than the other\n",
            "│  4 │ pub let say(@T: Type, use Pretty(T), use Loud(T), x: T) -> Str = Show/show(x);\n│    │                                                                  ^^^^^^^^^^^^",
        ),
        // No witness for an argument.
        (
            "use /std/{Nat, Fmt};\nstruct Point: Type { x: Nat, y: Nat }\nFmt/print(\"%\\n\")(Point { x = 1, y = 2 })\n",
            "no witness\n│  wanted: `/std/Show(Point)`\n│  needed by `Fmt/print` for this argument\n",
            "│  3 │ Fmt/print(\"%\\n\")(Point { x = 1, y = 2 })\n│    │                  ^^^^^^^^^^^^^^^^^^^^^^",
        ),
        // Shape mismatch.
        (
            "use /std/{Nat};\nlet pick(p: {Nat, Nat}) -> Nat =\n    match p | (a, b) => a | (a, b, c) => b end;\n/std/print(\"\")\n",
            "shape mismatch\n│  the arms' patterns disagree on shape in one column\n│  one arm takes apart a tuple of 2 fields, another a tuple of 3 fields\n",
            "│  3 │     match p | (a, b) => a | (a, b, c) => b end;\n│    │     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^",
        ),
    ] {
        let report = error(source);
        assert!(report.contains(words), "expected the words:\n{report}");
        assert!(report.contains(place), "expected the place:\n{report}");
    }
}
