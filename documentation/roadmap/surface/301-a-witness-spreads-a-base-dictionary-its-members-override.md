# A witness spreads a base dictionary its members override

**Not refined yet.** A witness is derived whole or written whole: `satisfy C(T) { .. }` holds the dots alone, so a type that wants its order derived but its `lt` written, or its order read off a projection, writes every member and every law by hand. A spread of a base dictionary — the derived one where `..` is bare, an expression's where one follows — with the written members overriding it, makes both one form of the concept literal a witness already is. It waits on the questions below.

## What is missing today

- `{ .. }` derives a whole body, and entries beside the dots are rejected because "a derived `neq` negates the `eql` it wrote" ([A witness body may be written by the compiler](../../design/surface/a-witness-body-may-be-written-by-the-compiler.md)).
- A witness given by another dictionary, an order read off a projection, has no form: its literal writes every member and every law.

## Goal

`satisfy C(T) { ..base, members }`: the base a dictionary of `C(T)`, the derived one where `..` is bare and the expression's where one follows, each written member overriding the base's, and the laws checked against the members the witness ends with.

## Settled

- A witness's body stays the braces of the concept literal it is. Its premises are its signature's, `satisfy (@A: Type, use Ord(A)) -> Ord(Tree(A)) { .. }`, as the surface findings' entry giving bodies `=>` writes them, a fix of its own that lands before this.
- The spread is an update's: `..` first, then the written members.
- A law is a field, so an override that breaks a law the members left from the base state is refused at that law, where the rejected pairing — a derived `neq` beside a written `eql` — would have read silently.

## Open

- Whether a derived base reads the members written beside it, a derived `lt` over a written `ord`, or only its own: the first makes a partial derivation compose, and is what the rejection weighed; the second keeps a derivation independent of what is written beside it.
- What a computed base states for its laws: `Ord/on`'s projection and the proof it is injective.
- Whether `..expr` stands at a registered key with any dictionary of that key, another witness's included.

## Stages

1. **The decisions.** The three questions answered, with `Ord(Version)` written each way they leave open.
2. **The spread in a witness body.** `..` bare and `..expr`, members overriding, laws checked against the result. Check: `Ord(Version)` derived with a written `lt`; an override that breaks `is_lt` refused at that law; an order read off a projection through `Ord/on`.

## Verification and retirement

Retired once a witness body spreads a base dictionary, with [A witness body may be written by the compiler](../../design/surface/a-witness-body-may-be-written-by-the-compiler.md) restated to the spread and its rejection of entries beside the dots revisited.
