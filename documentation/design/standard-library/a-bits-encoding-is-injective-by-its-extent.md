# A `Bits` encoding is injective by its extent

**Decision.** `/std/Hash`'s `Bits` witness encodes one byte per bit, least significant first as the carrier reads, so the encoding is as long as the run and nothing beside the bytes states the length. It is spelled as the structural fold over the run rather than through `Bits/fold`, which threads an accumulator. The eightfold width is a held position, not a settled one: a dense encoding is available the moment a consumer needs one — the length beside the packed bytes, with `/std/Map`'s `Key(Bits)` reproved over the pair rather than over the extent — and until then the cheap encoding would buy nothing and owe a second proof. The width is paid at the encoding and not in what a key holds, since `Hash/of` digests the result to eight bytes.

**Rationale.**

- **Packing is not injective on its own, and that is the whole difficulty.** `b[1]` and `b[1, 0]` are one byte each and the *same* byte, because a run's padding is zeroed. A dense encoding therefore has to carry the length beside the bytes, and `Bits/to_bytes` has to be reached past the alignment it demands. A byte per bit carries the length in its own extent and needs neither.
- **The spelling is what makes the inversion reduce.** `/std/Map`'s `Key(Bits)` inverts this encoding by induction, and the structural fold's `; ih` form is what makes `hash(b[h, ..t])` reduce to `x[<h>, ..hash(t)]` at all.
- **A reading with no consumers should cost the library only what it costs to state.** The width is the honest price of an encoding nothing yet reads densely, and it is one digest away from the eight bytes a key actually holds.

**Rejected.**

- **The packed form**, which is not injective without a length beside it, so it trades the extent for a second field and owes `Key(Bits)` a proof over the pair.
- **The accumulating fold**, `Bits/fold`: the two sides then differ in what they start from, and relating them costs a distribution lemma for nothing.
