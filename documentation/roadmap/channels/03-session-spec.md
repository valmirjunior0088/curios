# A protocol is data, and its handlers are computed from it

## Status

Researched and probed, not designed. The obligations this would have to satisfy were traced against the compiler this checkout builds rather than argued from the documents, and the trace is recorded below with the refusals it produced. Depends on `/std/Async/Channel` for the channel and, where a session runs across a process boundary, on [02](02-remote-spec.md) for the codec.

## Why it exists

A channel carries values of one type in whatever order the two ends happen to agree on, and nothing checks the agreement. Every mature answer to that problem is a session type, and every mature *implementation* of a session type rests on a substructural discipline Curios does not have. This specification exists because the callback-shaped alternative — the one that dispenses with linearity — turns out to be a shape the standard library already ships for a different purpose.

## What the survey settled

- **Linearity is the price of a session type, and it is charged in every implementation.** Priority Sesh needs Linear Haskell; Ferrite needs Rust's affinity; Idris 2 needs quantitative type theory. A channel endpoint that can be duplicated and stepped twice makes the protocol index a lie.
- **The Idris 2 implementation is the closest peer, and its own limitations are the price list.** Allais's *Type-safe Bidirectional Channels in Idris 2* represents a bidirectional channel as a pair of untyped unidirectional ones, manufactures an open union of all sent and all received types, and tracks position in a protocol with de Bruijn indices over a stack of open fixpoints, with offsets proved correct by a cut-down one-hole context. Every primitive is linear in the channel. The stated costs are four: uniqueness has to be emulated by a library-wide invariant because the host gives linearity and not uniqueness; the protocol *stops being erased*, because offsets are computed by induction over it; the surface syntax is "tedious" and "noisy"; and the productivity checker cannot see that a server making steady progress through a largest fixpoint is total.
- **Inversion of control dispenses with linearity entirely.** The *Session Types Without Sophistry* line of work, and the callback-style session programming beside it, obtain the linearity guarantee statically without a linear type system, by never exposing the channel: an input is a callback taking the received message, an output a callback returning what to send. The guarantee becomes structural because the user cannot hold the endpoint twice.
- **Separation logic is a different tool for a different job.** Actris's dependent separation protocols verify a message-passing program in Rocq; they do not type one. Useful as a source of vocabulary — its subprotocol relaxation of duality is the right idea — and not a design a compiler can adopt.
- **A choreography is a consumer of this, not a rival to it.** HasChor and ChoRus get deadlock freedom by endpoint projection with no linearity anywhere, and both still need a channel underneath. It is a later question, not a fork in this road.

## What is certain

Traced against `target/release/curios` built on 2026-09-06, each program on standard input under a memory cap. Every one compiled in about a second.

- **The declaration is admitted, and positivity is not the obstacle.** `induct Session | send(A: Type, rest: Session) | recv(A: Type, rest: Session) | offer(branches: List(Session)) | mu(body: Session) | bound(depth: Nat) | stop() end` elaborates with no diagnostic. The immediate occurrences are strict, and the occurrence through `List(Session)` is accepted by the same path [strict positivity, modulo polarity](../../design/language/strict-positivity-modulo-polarity.md) already carries `/std/Json` and `/std/Toml` along. A recursive protocol is finite *syntax*; only its unfolding is infinite, which is what the de Bruijn pair encodes.
- **`Handlers : (Session) -> Type` is total when the fixpoint arm does not unroll.** With `mu(body) => Handlers(body)` and `bound(_) => {}`, every call descends on a constructor payload, the definition is accepted, and a witness typechecks against `Handlers(example)` for a looping `example`. This is the shape `/std/Fmt`'s `format_type_with` already has.
- **The unrolling arm is refused, and the refusal is legible.** Writing `mu(body) => Handlers(unroll(body, p))`, where `unroll` is an ordinary total substitution, produces `the recursive definition '/Handlers' is a type position but does not terminate on every input` with `everything a type reaches must terminate, or type formation may not` beneath it. So the recursion is not banned — it is relocated, out of the type-level interpreter and into the value that serves the protocol, where general recursion is unrestricted by [totality of the erased program](../../design/language/totality-of-the-erased-program.md) and where `/std/Async` already classifies partial.
- **A branch arm must be written as a walk, not as a fold.** `offer` handled by a mutual `match` over the list is accepted; the same thing through `List/fold`'s lambda is refused with the identical message. This is not conservatism to be relaxed: the recursive call's argument is a lambda binder rather than an arm binder, so no argument is available to grade, and reading one would need the interprocedural summary that decision explicitly declines. `/std/Cli/Lookup` is already written the accepted way.
- **The protocol *value* must be built by total definitions.** `Handlers(spin(0))` for a `spin` that does not terminate is refused with `the type of '/bad' is a type position but reaches '/spin', which is not known to terminate`. This is (T)'s aggressive reading catching a type passed as an argument, and it means a protocol is written or built structurally, never decoded at run time — the same standing constraint `/std/Cli` lives under, where a specification is a written `List(Arg)`.
- **There is no universe obstacle.** The `?` oracle reports `? = Type` for a `Session` carrying `Type` payloads, the same as for one carrying none. So a universe of wire codes is a codec argument, not a forced one.

## The design

One sentence: **a protocol is a first-order value, the record of handlers it demands is computed from that value, and the library owns the endpoint so that linearity never has to be spoken.**

**The shape already exists in `/std/Cli`.** `Values` is an inductive indexed by `List(Arg)`; `Carrier(a: Arg) -> Type` computes what one field holds; `Has(spec, name) -> Prop` refuses a name the declaration lacks, decided by reduction, in the shape [a bound is stated in a decided proposition and discharged by reduction](../../design/language/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md) records. Replace `Arg` with `Session` and the machinery is the same machinery.

**`serve` owns the channel.** `serve` takes the protocol, the handlers computed from it, and a channel, and drives one against the other. The user writes callbacks and never holds an endpoint, so a channel cannot be stepped twice and no linear discipline is needed to say so.

**The index erases.** `serve` walks the protocol *value*; nothing inducts over a proof about it. That is the concrete difference from the Idris 2 implementation, whose protocol survives to run time because offsets are computed from it.

**The loop is a value, not a type.** A server is a largest fixpoint, productive and not descending. Here it is an ordinary partial `Async` definition, which is the licensed case rather than the awkward one — the productivity problem Allais names does not arise, because nothing asks the loop to be total.

## What has to be decided

- **Whether the protocol names types or codes.** `send(A: Type, …)` is expressive and admits types that can be neither sent across a boundary nor compared; a first-order `Wire` with `El : (Wire) -> Type` makes [02](02-remote-spec.md)'s codec a total function and states what may cross as data. Shared decision with 02.
- **Whether duality is written or computed.** Computed is one more total structural function; written is one more thing to get wrong.
- **How much survives a runtime branch.** An `offer` whose taken branch is decided by the peer cannot be checked statically past the choice point; whether the refusal is a decided proposition at the call site, a runtime failure, or both.
- **Whether a session may run over a remote channel in the first cut**, which is what pulls `Codec` in as a premise on every payload.
- **Naming and placement**, and whether this is `/std/Async/Session` or a package.

## Deliberately not specified

Multiparty sessions and any projection from a global type. Subtyping or subprotocols. Any static claim about deadlock freedom, which binary session types do not give on their own and which the priority-based and tree-structured disciplines buy with restrictions this specification has not evaluated. Any timing.
