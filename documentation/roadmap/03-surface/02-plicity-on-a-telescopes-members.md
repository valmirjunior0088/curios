# Plicity on a telescope's members

Working specification for the marks no site takes yet. [A hidden member takes no position](../../design/theory/a-hidden-member-takes-no-position.md) states the rule every telescope follows, under which a constructor's payload and a structure's field declare `@` and a concept's field `use`. Three marks are refused, each by its own rule (`curios-text/src/parse/marks.rs`), and each waits for a program that needs it.

It is independent of every other spec.

## The gap

- **`use` on data.** A constructor's payload takes no `use` member (`PAYLOAD_TAKES_NO_USE`), and neither does a structure's field (`STRUCT_FIELD_TAKES_NO_USE`). A value would hold the dictionary its construction resolved, and opening it would put that dictionary in the witness scope. No type of `/std` or `programs/` holds one: a type declared under a premise names its dictionary in its type, `struct Slot(K: Type, use Key(K))`, and a dictionary packed by hand is a plain member passed on with `use value`.
- **`@` on a concept's field** (`CONCEPT_FIELD_TAKES_NO_IMPLICIT`). A concept's hidden members are its superclass edges, which resolution fills; an `@` field would be a member a witness leaves to inference, and no concept has one to leave.
- **A mark on a tuple type's field** (`TUPLE_FIELD_TAKES_NO_MARK`). A tuple type is built all-plain and keyed by its shape, so a hidden field is a structure's.

## Prior art

- **A dictionary in data is the contested case.** GHC stores one at construction and extracts it on a match, and forbids matching such a constructor in a `let` and deriving over it ([Existential quantification](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/existential_quantification.html)); Scala 3 has the pattern write `given` ([Pattern-bound givens](https://docs.scala-lang.org/scala3/reference/contextual/more-givens.html)).

## Decisions

1. **`use` on a payload and on a field are one decision.** A one-constructor family is that structure, so admitting one without the other states no rule.
2. **Each lands with its consumer.** A mark is admitted when a program of `/std` or `programs/` is written shorter or safer by it, and its fixture is that program.

## Completion and retirement

Done when each mark is admitted with a consumer or recorded as rejected in [A hidden member takes no position](../../design/theory/a-hidden-member-takes-no-position.md), which then takes what this spec decided.
