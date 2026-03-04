# `to_cont` Lowering Plan

## Overall Shape

`to_cont` should produce a `cont::Module` with:

- exactly one top-level `cont::Func`: `main`
- zero top-level `consts`

That means:

- literals become local `Value::Pure(...)` bindings inside regions
- `CallTarget::Direct` is not needed for source-level functions in the MVP

## Core Assumption

Treat one `cont::Region` as containing:

- one mutually recursive `values` group
- one mutually recursive `blocks` group

Because of that:

- there is no need to topologically sort recursive bindings
- auxiliary temporaries created while lowering a recursive item can also live in the same `values`
- blocks inside one region may freely jump to each other without ordering constraints

## Lowering State

`to_cont` should keep:

- `module: cont::Module`
- one module-wide fresh generator for `ClsrName`
- an environment `HashMap<String, cont::ValueName>`

Each lambda entry, including `main` and every lowered closure body, gets its own fresh:

- `ValueName` source
- `BlockName` source

When instantiating the lambda-local `BlockName` source, allocate one fresh block name
immediately for the return/resume block. After that, pass forward both:

- the reserved return block name
- the advanced lambda-local block source

as one piece of state for the rest of the lowering.

## Lowering Interface

### `lower_value(term, env, builder) -> ValueName`

This is the value-emission operation. It:

- emits zero or more entries into the current `Region.values`
- returns the `ValueName` holding the result

Expected behavior:

- `Name` returns the mapped `ValueName` directly
- `Prim(Unit|Int|Flt)` emits `Value::Pure(...)`
- `Atom(index)` emits `Value::Pure(ConstValue::Int(index as i32))`
- primitive ops emit `Value::Eval(...)`
- `Pair` emits `Value::Tpl2(...)`
- `Func` emits a new top-level `cont::Clsr`, then a local `Value::Clsr(...)`

Captured variables should stay in the order already present on `ErasedFunc`.

### `lower_tail(term, env, ret, builder)`

This is the tail-lowering operation. It:

- ends the current region or block
- may emit entries into the current `Region.values`
- may emit auxiliary mutually recursive blocks into the current `Region.blocks`

Expected behavior:

- value terms delegate to `lower_value`, then jump to `ret`
- `Let` lowers the RHS, extends the environment, then lowers the tail
- `Apply`, `Match`, and `Split` are tail-only forms
- `Apply` lowers to `Tail::Call(Indirect { ... resume: ret })`
- `Match` and `Split` lower by introducing mutually recursive blocks and ending in an appropriate
  `Tail`

### `lower_to_name(term, env, builder, k)`

This is the bridge between value contexts and tail-only terms. It:

- obtains a `ValueName` for any term
- calls `k` with that `ValueName`

Expected behavior:

- value terms delegate to `lower_value`
- tail-only terms continue by introducing a fresh one-parameter block, lowering through
  `lower_tail`, and using that block parameter as the resulting `ValueName`

### `lower_entry(term, env) -> Region`

This is the entry-region lowering operation for `main` and closure bodies. It:

- lowers the body under the lambda-local sources established for that entry
- builds one `Region` by lowering the body through `lower_tail`
- ensures the final result is transferred to the reserved return/resume block

## `LetRec`

`LetRec` should be lowered directly into the current region's `values`.

Algorithm:

1. Reserve one fresh `ValueName` for every recursive source binder.
2. Extend the environment with all reserved names before lowering any RHS.
3. Lower every RHS into the same current `values` section.
4. Lower the tail under the extended environment.

## Closure Lowering

Each `ErasedFunc` should lower to:

- one new top-level `cont::Clsr`
- one local `Value::Clsr(clsr_name, captured_values)` at the use site

The closure body gets:

- closure fields for captured variables
- one parameter for the function argument

The closure body itself should be lowered with `lower_entry`.

## Main Entry

`main` has:

- `params: vec![]`
- one root region containing the lowered program
