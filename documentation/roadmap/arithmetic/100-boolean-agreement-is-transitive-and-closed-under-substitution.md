# Boolean agreement is transitive and closed under substitution

[The carriers' algebra stays in conversion](../../design/arithmetic/the-carriers-algebra-stays-in-conversion.md) holds conversion to symmetry, transitivity and closure under substitution with the theory in it, and `tests::laws::audit` holds each condition at every law the law table declares. The conversion chain's Boolean rung decides every identity of the connectives, at any number of atoms, by a decision diagram priced in the reduction budget ([A law is decided where it neither respells nor invents](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md)), and the law table declares only some of them, so the audit holds the others to none of its conditions.

Its next step is delivery, in the stage below.

## What is wrong

`curios-algebra`'s `TABLE` declares for `Bool` neither `xor`'s associativity, complement and dual, nor `==`'s associativity and nested cancellation, nor `!=`'s associativity, nested cancellation and unit on the left, nor `&&` distributing over `xor` and over `!=`, nor `||` over `==`. Probed at `0fc2fc1bd` with the release compiler, each closes by `Eq/refl()`, and no generated row, reversal, chain or substitution holds it, where `curios-algebra`'s README has the table declare, for each operation at each carrier, exactly the families conversion decides.

## The design

Every family that holds of a `Bool` connective at every value is declared, and a test holds the table to it, but for what declared families give: a commutative operation's dual with its operands swapped, and an idempotent operation's distribution over itself.

## Not part of it

A `Bool` elimination whose scrutinee the rung decides is a literal stays stuck, so `match b && Bool/not(b) | true => 1 | false => 2 end` meets no arm: [A Boolean elimination reads its scrutinee as conversion does](102-a-boolean-elimination-reads-its-scrutinee-as-conversion-does.md).

## Stage

1. **The families.** A test in `curios-algebra`'s `law` that evaluates every family of a `Bool` connective at every value and holds `TABLE` to declare exactly those that hold, less what declared families give; the declarations it asks for. **Check**: the generated grid and the audit hold their rows, and a row that proposes no solution for a metavariable is stated so in the audit, with its reason; `/std`'s image digest is compared before and after, at one path, and what moved named.

It retires with its stage, once the mention in [arithmetic 311](311-conversion-decides-parity-and-map-fusion.md) and in [arithmetic 307](307-boolean-and-bitwise-terms-meet-in-a-normal-form.md) state what landed.
