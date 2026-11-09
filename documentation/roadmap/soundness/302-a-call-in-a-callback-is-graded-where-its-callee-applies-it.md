# A call in a callback is graded where its callee applies it

This specification describes what remains of a capability that has landed in part. A recursion through a callback a recursive definition applies is graded where the definition applies it ([Totality of the erased program](../../design/soundness/totality-of-the-erased-program.md)): a tree's `has_zero` recursing through `List/any(children, (c) => has_zero(c))` is total (`curios`'s `a_call_in_a_callback_descends_where_its_callee_applies_it_below`). What is still graded as fresh is listed below, `/std/List/map` and `/std/List/fold` first. Its next step is stage 1, a hunt run on the user's word.

## What is missing

- **A list intrinsic's callback.** `/sys/List/map` and `/sys/List/fold` are definitions of no recursive group whose bodies are the intrinsics `ListMap` and `ListFold`, each applying its function operand to every element of its list, and the walk reads an intrinsic's operand as a use it does not follow. So neither publishes a summary, and every recursion through them is graded as fresh: `/std/Json/encode`'s, through `List/map(xs, encode)` and `List/map(ps, (pair: {Str, Json}) => … encode(pair.1))`, and `/std/Toml/encode`'s `value_str`'s, through `List/fold(items, start, (item, parts) => …)`.
- **A definition that is no recursive group.** `group_applications` summarizes a group's members, so a function that applies its callback without recursing, `/std/Option/map` among them, publishes nothing either.
- **A projection of a bound binder.** `/std/Json/encode`'s `obj` arm calls `encode(pair.1)`, `pair` an element of `ps`. The grader reads a projection's shape off its subject's expansion, and a binder a summary bounds has none, so `pair.1` would grade below nothing once `pair` is bounded.
- **A callback handed on to another function.** A parameter handed to another summarized definition is a use the summary does not follow, so `any_kept`, which hands its `pred` to `List/any`, publishes nothing for it (`a_callback_its_callee_hands_on_is_graded_as_unrelated`). Composing the callee's summary at the call inside the definition being summarized would follow it.
- **A result's elements below an argument.** `value_str` also reaches a table's values through `Map/to_list(m)`, which no summary of how a function applies a parameter relates to `m`, so its group stays `Partial` through `pairs_str` whatever `List/fold` publishes. A fact about what a function returns is another analysis, and out of scope here.

## Stages

1. **A hunt.** A soundness hunt with a charter on what landed — a parameter used in a way the walk misses, an application under a binder a summary misreads, a bound carried past the lambda it was handed — run on the user's word, ahead of every stage that widens what a summary admits.
2. **A definition that is no recursive group, and a list intrinsic's callback.** The walk run over a definition's own body with no group, published beside its totality as a group's member's is, reading `ListMap`'s and `ListFold`'s function operand, where it is a parameter, as applied to an element below the list operand, and `ListFold`'s accumulator as unrelated. Check: a recursion through `/sys/List/map`, `/sys/List/fold` and `/std/Option/map` descends, each put to both checkers (`curios/src/tests/rules/`).
3. **A projection of a bound binder.** The grader reads `b.i` below `b` where `b` is a binder a summary bounds, as a constructed tuple's fields are its children. Check: `/std/Json/encode` is total, as a fixture put to both checkers, with a refusal that projects a field of something not below.
4. **Composition across declarations.** First a count of the definitions in `/std`, `programs/` and `../curios-papers` whose parameter is handed to another summarized definition and would be summarized through it; a count of none drops this stage, and any other is delivered with `any_kept`'s test restated.

## Retirement

Each stage lands with the totality decision restated where it moves the rule. Then this specification is deleted.
