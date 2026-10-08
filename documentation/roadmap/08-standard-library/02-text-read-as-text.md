# Text read as text

Working specification for the last reader in `/std` that parses the bytes of a text it was handed, `http/Url/of_str`, and for what keeps it there: a type pays for a text many times what it pays for the text's bytes, in work the erased program never does. Every other format reads its input as `Parse(Str, A)`, by [`curios-std`'s README](../../../curios-std/README.md#std-invents-no-value-where-a-proof-belongs-and-text-is-addressed-by-position).

## What this builds on

- **One parser type, indexed by its carrier.** `Parse(Bytes, A)` reads bytes and `Parse(Str, A)` reads text, whose positions are boundaries of valid text, so a piece a text parser takes is text with no check. The combinators are written once, and each carrier's primitives are a module of their own, `Parse/bytes` and `Parse/text`.
- **A parser a type runs is total.** `http/Url/lit` discharges `Valid(s)` by running `of_str` in a type, so everything `of_str` reaches is total ([Totality of the erased program](../../design/soundness/totality-of-the-erased-program.md)).
- **Erasure says which positions it deletes, and that they are not evaluated.** The same decision: "An expression in an erased value position — an argument to an erased parameter, an erased field, an operand slot instantiated at a proposition or a type, the value a `let` binds at one, locally or at the top level — is not evaluated: its refusals do not occur and it costs nothing." Erasure decides a position by the sort of its binder's declared type, the binders around it opened as opaque variables (`signature_entries`, `curios-elab/src/into_ersd/classify.rs`).
- **A declaration is published as it finishes.** What is decided of it over its final terms is decided once, there, before any other declaration reads it ([A declaration is a function of what it reads](../../design/compilation/a-declaration-is-a-function-of-what-it-reads.md)).

## The gap

**`http/Url/of_str` drops the proof on entry.** It receives a `Str`, parses its bytes as `Parse(Bytes, Url)`, and builds each piece back from a byte slice through `text` (`http/Url.crs`), a check whose refusal, `a URL is text`, cannot fire on input that began as a `Str`. A server reads a request's target the same way round: `Request/head_of` makes the target's bytes a text, and `of_str` takes the text's bytes and makes each piece a text again.

**The closed machine evaluates what erasure deletes.** It is the one evaluator that does not keep the totality decision's sentence, at two sites that are mended together or to no effect: `Machine::beta` and a `let` take a substituend to a weak-head value before they bind it, a proof among them, and the machine's intrinsic arm forces every operand of an operation, the proof of its bound among them, which no fold reads (`curios-core/src/machine.rs`). A step over text carries a bound a lemma proves by induction on the offset, so the reader over text costs `http/Url/lit` more with every character before the one it reads. Counted at `6cfe914e0`:

| literal | over bytes | over text |
| --- | --- | --- |
| 32 characters | 108 547 units | 398 417 |
| 64 characters | 140 002 | 850 057 |
| 128 characters | 190 074 | 2 276 364 |

A doubling costs the bytes reader about 1.3 times and the text reader 2.1 and 2.7. At that rate a literal towards a thousand characters, a signed address or a long query, passes the default budget of 30 000 000 units, where the bytes reader reads it for under a million.

**A walk over positions pays three proofs of accessibility a step.** `Str/At/forward` descends on `WellFounded/narrow` over `WellFounded/on` over `WellFounded/lt`, each an `Accessible/intro` a step that a computation does read, since the recursion eliminates the proof into data.

**`/std` is written around the cost.** `Parse/bytes/take_while` decides the bound of the position it hands on before it argues it, "so a closed input takes the arm that holds no lemma", and the README's decision says a walk's position "is built under the decision of its bound": a comparison at run time, kept for what a type would otherwise pay.

## Decisions

1. **The closed machine does not evaluate what erasure deletes.** An argument at an erased binder, the value a `let` binds at one and a constructor's payload at one are bound as written, as an induction hypothesis is, and an operand no fold reads is not forced. A match that eliminates a proof into data, as `WellFounded/recurse` does, still demands it, and the run's memo keeps the demand to one evaluation. An argument left unreduced changes no result, a closed term having one, so this is the machine's own business and no judgment's. Every proposition's proof is total by obligation (V), so nothing is hidden by not running one.
2. **A binder states what erasure makes of it.** The decision erasure takes at every walk is taken once, where the binder's declaration is published, by the same classifier, and written on the binder: in what its scope remembers of it, beside its hint (`curios-core`'s `Label`). The machine reads it there at an application, a `let` and an arm, which closes over its payload under the marks its constructor's telescope carries. What a scope remembers of a binder is no part of a term's identity, so conversion and every memo are as they were, and the stored node keeps it, a node being stored under its binders' labels. The kernel refuses a mark that its own reading of the binder's type does not bear, where it types the binder: a binder marked erased whose type is no proposition, no sort and no function into one.
3. **A position's accessibility is one proof.** `Str/At/forward` and `backward` descend on `Accessible` as they do, with no bound and no fuel, through a proof written for positions: recursion, inside the proof, on what is left of the text or on the offset. `WellFounded/on` and `narrow` stay for a relation that is not a position's.
4. **`of_str` reads `Parse(Str, Url)`.** The scheme, the host, the path and the query are pieces of the input, `text` goes, and every refusal keeps its message and its byte offset: a part still holds visible ASCII alone, and a port is the digits `Nat/of_str` reads. A request's target is then validated once, where its bytes become text.
5. **What the cost forced is undone.** `Parse/bytes/take_while` builds its position from its bound's proof, and the README's decision loses the clause.
6. **The gate is tests.** In `curios/src/tests/reduction.rs`, counted and asserting, each checker's cost read by `declaration_cost`: `http/Url/lit` on the 32-character literal costs under twice what the reader over bytes cost at the commit before it was replaced; a doubling of the literal, to 64 and to 128, costs no more than a doubling; and a measurement reports each `Parse/text` primitive beside its `Parse/bytes` twin.

**What the decisions are worth**, by a prototype in a copy of the sources at `e506feab0`: the machine leaving as written each substituend at a binder whose type is headed by a proposition's name, at an application and a `let`, and each bound's operand. The literals above cost 159 648, 231 199 and 361 763 units over text against 83 008, 101 782 and 127 433 over bytes: 1.9 times at 32 characters, and 1.4 and 1.6 a doubling. Either site alone moves the 32-character literal by under a fifth. The walk's step then holds 99 `Accessible/intro` in a run over 32 characters, three a character, and no lemma.

## Verification

- `http/Url/lit` discharges `Valid` by reduction and holds its gate at 32, 64 and 128 characters.
- `curios-std/src/tests/http/url.crs` expects what it expected, each refusal where it was.
- `curios-core/src/machine/tests.rs`: an erased argument whose evaluation would exhaust the budget is never run, one a match reads is run once, and a bound's operand is not forced.
- Over the restored prelude, no binder erasure's classifier deletes is unmarked: what holds every pass that rebuilds a scope to keeping what it remembers.
- `/std` elaborates and certifies, and the counted column of the prelude build is read before and after the marking pass.

**To retake the measurements.** A counted figure is the least `N` under which `cargo run --release --package curios -- wonder diagnostics --budget N -` accepts its program on standard input, neither checker reporting an exhaustion, found by bisection; a lint is no refusal. The reader over bytes is measured through `/std`:

```crs
use /std/http/{Url};
let home: Url =
    Url/lit("https://example.com:8443/a/b?x=1");
/std/print(Url/to_str(home))
```

The longer literals are `https://example.com:8443/api/v1/items/42/parts?sort=name&page=12` and `https://example.com:8443/api/v1/items/42/parts/7/revisions/2024/attachments/photo.png?sort=name&page=12&size=50&filter=open&x=12`. The reader over text is measured before it lands as a program of its own: `use /std/{Bytes, Byte, Char, Str, Nat, Bool, List, Option, Result, Parse}; use /std/Bool/{True, False}; use /std/http/{Url}; use /std/http/Url/{Scheme, default_port};`, the reader below with `of_str` private, then `Read`, `Valid` and `lit` as `http/Url.crs` declares them, and the same `home` through that `lit`. A claim about a primitive is stated over a `Bool` in a declaration of its own, `let read = Parse/run(p, input);` and then `let _claim: Holds(ok(read)) = True/qed();`, so that neither what the claim reduces to nor an implicit left to inference is counted with the primitive. The reader, which stands in `http/Url.crs` for everything from `until` to `of_str`:

```crs
let until(stops: List(Char)) -> (Char) -> Bool =
    (c) => Bool/not(List/any(stops, (stop) => c == stop));

-- Whether the wire carries `b` as it is: visible ASCII.
let visible(b: Byte) -> Bool =
    Nat/in_range(Byte/to_nat(b), 33, 126);

-- One part of a URL. A space, a control octet and an octet past ASCII have no place in one: each is written percent-encoded, and one left bare is refused.
let part(s: Str) -> Parse(Str, Str) =
    match Bytes/all(Str/to_bytes(s), visible)
    | true => Parse/pure(s)
    | false =>
        Parse/fail(
            "a URL holds no space, control octet or octet past ASCII, each of which is written percent-encoded")
    end;

let scheme: Parse(Str, Scheme) =
    let name = Parse/text/take_while(until([':']))!;
    let _ = Parse/text/literal("://")!;
    choose
    | Str/eql_ascii_ci(name, "http") => Parse/pure(Scheme/http())
    | Str/eql_ascii_ci(name, "https") => Parse/pure(Scheme/https())
    | _ => Parse/fail(Str/flatten(["unknown scheme '", name, "'"]))
    end;

let is_digit(c: Char) -> Bool =
    Nat/in_range(Char/to_nat(c), '0', '9');

-- Whether the authority ends here: at the path, the query, the fragment, or the end of the text.
let ends_authority(next: Option(Char)) -> Bool =
    match next
    | some(c) =>
        match Char/to_nat(c)
        | '/' => true
        | '?' => true
        | '#' => true
        | _ => false
        end
    | none() => true
    end;

-- The port after a `:`: its digits up to where the authority ends. None is the scheme's, as RFC 3986 §3.2.3 reads an empty port, and a number no port holds is refused here rather than at the connection.
let port_number(s: Scheme) -> Parse(Str, Nat) =
    let _ = Parse/text/expect(':')!;
    let digits = Parse/text/take_while(is_digit)!;
    let next = Parse/text/peek!;
    match ends_authority(next)
    | false => Parse/fail("a port is a number")
    | true =>
        match Str/is_empty(digits)
        | true => Parse/pure(default_port(s))
        | false =>
            match Nat/of_str(digits)
            | some(n) =>
                match n <= 65535
                | true => Parse/pure(n)
                | false => Parse/fail("a port is at most 65535")
                end
            | none() => Parse/fail("a port is a number")
            end
        end
    end;

let port_of(s: Scheme) -> Parse(Str, Nat) =
    let next = Parse/text/peek!;
    match next
    | some(c) => match Char/to_nat(c) | ':' => port_number(s) | _ => Parse/pure(default_port(s)) end
    | none() => Parse/pure(default_port(s))
    end;

let path: Parse(Str, Str) =
    let written = Parse/text/take_while(until(['?', '#']))!;
    let s = part(written)!;
    match Str/is_empty(s) | true => Parse/pure("/") | false => Parse/pure(s) end;

let query_text: Parse(Str, Option(Str)) =
    let _ = Parse/text/expect('?')!;
    let written = Parse/text/take_while(until(['#']))!;
    let s = part(written)!;
    Parse/pure(Option/some(s));

let query: Parse(Str, Option(Str)) =
    let next = Parse/text/peek!;
    match next
    | some(c) => match Char/to_nat(c) | '?' => query_text | _ => Parse/pure(Option/none()) end
    | none() => Parse/pure(Option/none())
    end;

let url: Parse(Str, Url) =
    let s = scheme!;
    let h = Parse/text/take_while(until([':', '/', '?', '#']))!;
    let host = part(h)!;
    let p = port_of(s)!;
    let pa = path!;
    let q = query!;
    let _ = Parse/text/rest!;
    match Str/is_empty(host)
    | true => Parse/fail("a URL names a host")
    | false => Parse/pure(Url { scheme = s, host = host, port = p, path = pa, query = q })
    end;

pub let of_str(s: Str) -> Result(Parse/Error, Url) =
    Parse/run(url, s);
```

## Rejected

- **A walk that descends on a bound.** The fixpoint Lean gives a recursion measured in `Nat`, structural on a fuel the measure seeds ([Lean 4.27.0](https://lean-lang.org/doc/reference/latest/releases/v4.27.0/)), takes more off a step than one proof of accessibility does: in the prototype a walk over it costs 2 279 units a character against 3 631 over the three proofs. It is a count carried through every walk at run time, where the proof erases, and `/std` walks on no fuel.
- **Reading the sort where the machine substitutes**, off the head of the binder's type. A reader of declared names missed, in turn, a relation given as a function literal, a variable an earlier entry of the same telescope declares a predicate, and a family applied in two steps, `Eq()(a, b)` and `Accessible(R)(x)`: each a binder silently evaluated. And the checkers' own `Sort::of` cannot be asked there, a closed type being reduced before it is classified, so `Holds(d)` would be decided to be called a proposition.
- **A mask a declaration publishes for its parameters**, which a local function and a `let` do not have: the prototype held to calls of a global costs the 32-character literal 4% more.
- **The mark beside a binder's plicity.** Plicity is part of a function type's identity and this is not, so it would be a field every comparison skips, a second field on a `let`'s binding and on an arm, and a new key for the stored node, where what a scope remembers of a binder is already none of those and carried by all of them.
- **A certificate written behind the decision of its goal**, so that a closed instance takes the arm holding no lemma. It reaches the certificates the elaborator writes and none a library does, `Str/Valid/concat` among them, and it is the comparison at run time decision 5 removes.
- **Firing a match on an accessibility proof without reducing the proof.** [`Prop` is strict, proof-irrelevant, and definitionally K](../../design/theory/prop-is-strict-proof-irrelevant-and-definitionally-k.md) keeps K-like reduction out of the rules, and the machine holds no type to read a payload's pinned index from.
- **Landing the reader over its gate**, on the argument that a URL literal is short: the cost grows faster than the literal, and it is the cost decision 1 removes.
- **A fold over the address's characters**, as `Fmt/parse` is one. A URL is cut, not read through: its host, path and query are pieces of the input, which a position cuts out in O(1) and a fold builds again a character at a time, and `http/Url` would be the one format not written over `Parse`, its refusals placed by hand.
- **`http/Request` and `http/Response` over text.** An HTTP head is octets, not text, and they read it as octets, by [`curios-std`'s README](../../../curios-std/README.md#an-http-message-is-read-through-one-framing-core-and-made-as-one-a-wire-can-carry).
