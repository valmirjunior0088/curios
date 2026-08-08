# Full-conformance Wasm data and element sections

Not refined yet. This umbrella placeholder reserves the specification location for the roadmap items below until each is refined into a working implementation specification.

## Covered roadmap items

- Full data section support in `curios-wasm` (active data segments, `memory.init`/`data.drop`, and the complete linear-memory load/store instruction family)
- Full element section support in `curios-wasm` (every element-segment mode with table declarations and table instructions)

## Known constraints

- Today each section is minimum-fitted to its one consumer: passive-only data segments reached through `array.new_data`, and a single declarative element segment making functions `ref.func`-eligible.
- The representation, parser, encoder, and round-trip tests belong to `curios-wasm`; continuation emission in `curios-cont` is the consumer to check.
