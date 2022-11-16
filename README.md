<p align="center">
  <img width="350" height="183" src="https://github.com/valmirjunior0088/curios/raw/master/logo.png">
</p>

### System dependencies

1) For the interpreter:
  - [`stack`](https://www.haskellstack.org/).

2) For the compiler:
  - [`stack`](https://www.haskellstack.org/);
  - [`clang`](https://clang.llvm.org/);
  - [`lld`](https://lld.llvm.org/).

### Example source

```
Unit : Type.
Unit = {:unit}.

unit : Unit.
unit = :unit.

List : Type -> Type.
List = A =>
  (label: {:null, :cons}) *> match label {
    :null = Unit.
    :cons = A *> List A.
  }.

null : (A: Type) -> List A.
null = A => (:null, unit).

cons : (A: Type) -> A -> List A -> List A.
cons = A => a => rest => (:cons, a, rest).

fold : (A: Type) -> (B: Type) -> (B -> A -> B) -> B -> List A -> B.
fold = A => B => action => initial => list =>
  let (label, list) = list;

  match label {
    :null = initial.

    :cons =
      let (value, rest) = list;
      fold A B action (action initial value) rest.
  }.

Nat : Type.
Nat =
  (label: {:zero, :succ}) *> match label {
    :zero = Unit.
    :succ = Nat.
  }.

zero : Nat.
zero = (:zero, unit).

succ : Nat -> Nat.
succ = value => (:succ, value).

add : Nat -> Nat -> Nat.
add = one => other =>
  let (label, one) = one;

  match label {
    :zero = other.
    :succ = succ (add one other).
  }.

one : Nat.
one = succ zero.

two : Nat.
two = succ one.

three : Nat.
three = succ two.

nat_list : List Nat.
nat_list = cons Nat one (cons Nat two (cons Nat three (null Nat))).

folded_nat_list : Nat.
folded_nat_list = fold Nat Nat add zero nat_list.

to_int32 : Nat -> Int32.
to_int32 = value =>
  let (label, value) = value;

  match label {
    :zero = 0.
    :succ = [int32.add 1 (to_int32 value)].
  }.

start : Int32.
start = to_int32 folded_nat_list.
```