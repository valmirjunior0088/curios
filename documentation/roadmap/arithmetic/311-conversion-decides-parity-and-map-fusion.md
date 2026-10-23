# Conversion decides parity and map fusion

**Not refined yet.** This specification reserves two rows `curios`'s `tests::laws::written` holds that no rule decides yet, each a candidate for a family of its own. It is not an implementation plan. It waits for a consumer of either row.

## What is missing

- **Parity read as a clash.** The gcd test decides `x * 2 + 1 == y * 2` false as a fold, and read by the `Nat` peel as `Impossible` it would let `match h end` close `Eq()(x * 2 + 1, y * 2)`, so it needs its own row and its own evidence before inversion may rely on it.
- **Map fusion**, `map(map(xs, f), g) = map(xs, (x) => g(f(x)))`.

## What is known

- **The grid refuses more rows, owned elsewhere.** Boolean agreement past the truth table's cap is [Boolean agreement is transitive and closed under substitution](100-boolean-agreement-is-transitive-and-closed-under-substitution.md)'s. Two more leave an operation with no meaning the logic can read, each with a consumer waiting: [A right shift and a remainder state their bounds](300-a-right-shift-and-a-remainder-state-their-bounds.md).

It retires when `tests::laws::written` holds neither row as refused, parity with the evidence inversion relies on.
