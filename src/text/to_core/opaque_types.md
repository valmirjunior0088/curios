# Opaque Types in the Text Layer

## Motivation

A transparent type alias, written `let Foo : Type = ...`, makes the definition
visible to callers: `Foo` and its underlying type are interchangeable everywhere.
Opaque types exist for when that transparency is undesirable. A `def` introduces
a type whose representation is hidden behind a controlled interface, enforced by
the module system.

## Syntax

```
[pub] def <Name> = <Type> in
    <items>
end
```

Inside the block, two atomic expression forms are available:

- `<Name>.from <term>` — wraps a term of the representation type, yielding `<Name>`
- `<Name>.into <term>` — unwraps a term of `<Name>`, yielding the representation type

The term is part of the operation: these are not first-class values and cannot
be used independently of their operand.

The `pub` modifier on `def` controls whether the type and its exported interface
are visible outside the enclosing module, consistent with how `pub` behaves on
modules. The block may contain anything a module can hold; individual members
are exported with `pub`.

## Scoping

`<Name>.from` and `<Name>.into` are only in scope within the `def` block. This
is enforced by the module system. There is no way to declare them `pub` — they
are not members of the interface, only tools for constructing it.

The `<Name>` prefix in both coercions is not merely a namespace convention: it
identifies which `def` binding provides the witness, which is information the
elaborator needs when lowering to core.

## Type-Theoretic Semantics

Outside the `def` block, `<Name>` and `<Type>` are nominally distinct. A value
of type `<Type>` is not accepted where `<Name>` is expected, and vice versa,
without an explicit coercion. The abstraction boundary is absolute: callers can
only interact with `<Name>` through whatever `pub let` members the block exports.

## Elaboration to Core

The `to_core` pass translates a `def` as follows:

- `def <Name> = <Type>` introduces a `sealed <Name> = <Type>` binding into the
  flat core item sequence, available to all subsequent items.
- `<Name>.from e` elaborates to `Seal <Name> e`.
- `<Name>.into e` elaborates to `Unseal <Name> e`.

`Seal` and `Unseal` are the intro and elim forms for `sealed` bindings in core,
and both require the witness bound by `sealed` — hence the `<Name>` argument.

The `pub let` declarations inside the block elaborate as members nested under
`<Name>`, consistent with the module analogy. The `def` block boundary itself
does not survive elaboration.

By the time core sees the program, the module system has already validated that
`<Name>.from` and `<Name>.into` were only used within the block. Core can
therefore type-check `Seal` and `Unseal` without re-enforcing access control:
opacity has already been validated upstream.

## Error Cases

The following are rejected during elaboration:

- **Coercion outside the block** — using `<Name>.from <term>` or
  `<Name>.into <term>` outside the `def` block is a module system error, caught
  before `to_core` runs.
- **Wrong type for `.from`** — if the term passed to `<Name>.from` does not have
  type `<Type>`, it is a type error.
- **Wrong type for `.into`** — if the term passed to `<Name>.into` does not have
  type `<Name>`, it is a type error.

## Example

```
pub def Str = Bin in
    pub let from : Bin -> Str = bin => Str.from bin
    pub let into : Str -> Bin = str => Str.into str
end
```

Elaborates to (schematically):

```
sealed Str = Bin
pub module Str {
    pub let from : Bin -> Str = bin => Seal Str bin
    pub let into : Str -> Bin = str => Unseal Str str
}
```
