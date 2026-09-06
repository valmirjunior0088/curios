# A remote channel is a codec and a framing over a stream

## Status

Researched, not designed. The survey is settled and the transport inventory is read off `curios-abi`; the shape below is a proposal, and the questions under *What has to be decided* outnumber the paragraphs that answer any. Depends on `/std/Async/Channel`, which owns the local channel this one bridges.

## Why it exists

Two Curios processes have no way to talk that is not bytes on a handle. `/std/tcp` and `/std/Command/Child` hand a program a stream, and everything above that — where a message begins and ends, what a value looks like on the wire, which end may travel — is written again by every program that needs it. A channel whose two ends sit in different processes is the same vocabulary `/std/Async/Channel` already defines, with the costs of a process boundary stated rather than hidden.

## What the survey settled

- **Cloud Haskell's asymmetry is the design to take.** `SendPort` is serializable and `ReceivePort` is not. One end travels, the other stays. That single constraint is what makes "spawn a peer and hand it a way to answer" expressible without a general closure-serialization story, and it is the shape every later distributed-channel library converged on.
- **A function cannot cross a wire, and a proof should not.** Cloud Haskell needs `Closure b` and symbolic static pointers precisely because a value of type `Process a` cannot be marshalled. In a dependently typed setting the constraint is sharper: a type may mention values, and a proof is evidence about a particular term, so neither travels.
- **Framing is not the transport's problem.** Every mature protocol above a byte stream — and both transports available here — needs an explicit message boundary, and length prefixing is the form that costs nothing to parse and nothing to generate.
- **What is lost at a process boundary is a failure story, not a feature list.** A peer can die between two messages, a half-close ends one direction only, and no scheduler can see across the boundary. The libraries that pretend otherwise are the ones with the worst reputations.

## What the tree already decides

- **There is no new transport to build, and none to add.** `curios-abi`'s row list has no `pipe`, no `socketpair`, no unix-domain socket and no shared memory. What exists is a child's standard streams (`proc/spawn`, `proc/stream`) and TCP with TLS above it, multiplexed by `Handle/poll`. Both already satisfy `/std/stream`'s `Read` and `Write` — over `File`, `tcp/Socket` and `Child/Pipe` — so a channel written against those two concepts reaches every transport this system has without an ABI change.
- **A fallible operation is `Try`.** By [a fallible operation returns `Try`](../../design/language/a-fallible-operation-returns-try.md), a remote receive is `Try(Async, Io/Error, …)` and a local one is not. The two are therefore different types, and unifying them would either infect every in-process program with an impossible error case or hide a peer's death.
- **A concept resolves with global coherence**, which is exactly the property a wire format wants: one encoding per type, decided once for the whole program, not per call site.
- **Only `Spell` and `Equal` are derivable** (`curios-elab/src/derive.rs`), so a codec is hand-written witnesses until and unless a third derivation is added.
- **The host never waits on a peer** ([the host never waits on a peer](../../design/toolchain/the-host-never-waits-on-a-peer.md)), so the bridge parks on `poll` like every other peer-facing read, and a framing that blocks mid-message is not available.

## The design

One sentence: **a remote channel is a local channel with a bridge, and the bridge is a codec, a framing and a fiber at each end.**

**The transport is a premise, not a type.** `Remote(A)` is built over any `S` satisfying `stream/Read(S)` and `stream/Write(S)`, so a child's pipe and a TLS socket are one implementation.

**The codec is a concept.** `Codec(A)` encodes an `A` to `Bytes` and decodes a prefix of `Bytes` back to an `A` and the remainder. A witness exists only for data that is first-order and closed: no functions, no proofs, no `Io`, nothing whose type mentions a value the peer cannot reconstruct.

**The framing is a length and a payload.** A frame carries its own length so a partial read resumes without a parser that can block.

**The bridge is two fibers under a bracket.** One drains the local channel into the transport; one decodes frames from the transport into a local channel. Both are owned by an `Async/using` bracket, so a bridge cannot outlive the transport it was built on.

**Only the sending end travels.** A `Sender` can be handed to a peer; a `Receiver` cannot.

**What is lost is written down.** `Async`'s deadlock detector reports a local cycle because it can see every fiber; it cannot see a peer, and a fiber blocked on a socket is correctly not stalled. A cross-process deadlock therefore hangs, exactly as it would in any other language, and this specification does not claim otherwise.

## What has to be decided

- **Whether several channels share one transport.** One channel per transport is trivial and does not survive contact with a program that wants two conversations with one peer; multiplexing needs a channel identifier in the frame header and a demultiplexing fiber. This is the largest question here and it is invisible until the second channel.
- **Whether rendezvous is offered remotely at all.** It costs a round trip and cannot be free. Declining it, and offering only a bounded remote channel, is the honest first cut.
- **How back-pressure crosses.** The transport's own buffer is not the channel's capacity, so a bounded remote channel needs either a credit scheme or an acknowledgement the sender waits on.
- **Whether the codec is a concept with hand-written witnesses, a third derivation, or a universe of wire codes.** A first-order `Wire` datatype with `El : (Wire) -> Type` would make the codec a total function by recursion on the code and would state exactly what may cross a boundary as data rather than as a rule about witnesses. It is not forced: a `Session` carrying `Type` payloads reports at `Type` with no universe obstacle, measured with the `?` oracle. It is a genuine fork, and it is shared with [03](03-session-spec.md).
- **What a half-close means**, and whether a remote `recv` distinguishes "peer closed" from "peer died".

## Deliberately not specified

Any wire compatibility or versioning story. Encryption or authentication beyond what `/std/tcp`'s TLS already provides. A discovery or naming service. Anything resembling a distributed failure detector or consensus. Any timing.
