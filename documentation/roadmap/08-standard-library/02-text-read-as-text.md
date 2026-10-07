# Text read as text: `http/Url`

Working specification for the one reader in `/std` that still parses the bytes of a text it was handed: `http/Url/of_str`. Every other format reads its input as `Parse(Str, A)`, by [`curios-std`'s README](../../../curios-std/README.md#std-invents-no-value-where-a-proof-belongs-and-text-is-addressed-by-position). The reader is written below and waits on a gate it does not hold: what a type pays for a position, which [the closed machine evaluating a proof no computation reads](../02-theory/00-findings.md) decides.

## What this builds on

- **One parser type, indexed by its carrier.** `Parse(Bytes, A)` reads bytes and `Parse(Str, A)` reads text, whose positions are boundaries of valid text, so a piece a text parser takes is text with no check. The combinators are written once, and each carrier's primitives are a module of their own, `Parse/bytes` and `Parse/text`.
- **A walk stops where its predicate does, and is total.** `Parse/bytes/take_while` and `Parse/text/take_while` cost what they take, and a match or a search keeps nothing waiting on a character it passes.
- **A parser a type runs is total.** `http/Url/lit` discharges `Valid(s)` by running `of_str` in a type, so everything `of_str` reaches is total ([Totality of the erased program](../../design/soundness/totality-of-the-erased-program.md)).

## The gap

**`http/Url/of_str` drops the proof on entry.** It receives a `Str`, parses its bytes as `Parse(Bytes, Url)`, and builds each piece back from a byte slice through `text` (`http/Url.crs:49`), a check whose refusal, `a URL is text`, cannot fire on input that began as a `Str`.

**A position costs its offset in a type.** A step over text reaches `Bytes/drop`, whose bound is a lemma proved by induction on the offset, and the closed machine runs that induction. So the reader over text costs `http/Url/lit` more with every character before the one it reads, where the reader over bytes does not:

| literal | over bytes | over text |
| --- | --- | --- |
| 32 characters | 108 547 units | refused at 375 000, accepted at 400 000 |
| 64 characters | 140 002 | refused at 800 000, accepted at 1 000 000 |
| 128 characters | 190 074 | refused at 2 000 000, accepted at 2 500 000 |

A doubling costs the bytes reader about 1.3 times and the text reader about 2.5. At that rate a literal towards a thousand characters, a signed address or a long query, passes the default budget of 30 000 000 units and is refused, where the bytes reader reads it for under a million.

## Decisions

1. **`of_str` reads `Parse(Str, Url)`.** The scheme, the host, the path and the query are pieces of the input, `text` goes, and every refusal keeps its message and its byte offset: a part still holds visible ASCII alone, and a port is the digits `Nat/of_str` reads.
2. **It lands under a counted gate.** `http/Url/lit` on the 32-character literal stays under twice what it costs over bytes, and a doubling of the literal costs no more than a doubling. It holds neither today, so the reader waits on the theory finding and lands as written once that is fixed.

## Verification

- `http/Url/lit` discharges `Valid` by reduction and holds its gate at 32, 64 and 128 characters.
- `curios-std/src/tests/http/url.crs` expects what it expected, each refusal where it was.
- `/std` elaborates and certifies, and the counted column of the prelude build is read before and after.

**To retake the measurements.** A counted figure here is taken at `3cd8ef597`, and is the least `N` under which `cargo run --release --package curios -- wonder diagnostics --budget N` accepts its program, on standard input (`-`) or at a file of a copy of the library, neither checker reporting an exhaustion, found by bisection; where two budgets are given, the floor lies between them. The reader over bytes is measured on standard input:

```crs
use /std/http/{Url};
let home: Url =
    Url/lit("https://example.com:8443/a/b?x=1");
/std/print(Url/to_str(home))
```

The longer literals are `https://example.com:8443/api/v1/items/42/parts?sort=name&page=12` and `https://example.com:8443/api/v1/items/42/parts/7/revisions/2024/attachments/photo.png?sort=name&page=12&size=50&filter=open&x=12`. The reader over text is measured before it lands, in a copy of `curios-std/src` with the reader laid over `http/Url.crs` and a module holding the same `let` mounted by a `mod` line of the copy's `lib.crs`, the copy's `.curios` removed before each asking. A floor under what the copy's own closure costs does not show there: the refusal names the declaration that ran out, and only one naming the claim is a reading. The reader, which stands in `http/Url.crs` for everything from `until` to `of_str`:

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

- **Landing the reader over its gate**, on the argument that a URL literal is short: the cost grows faster than the literal, and it is the cost the theory finding removes.
- **A fold over the address's characters**, as `Fmt/parse` is one. It is cheap in a type: joining a text's characters back together, `Str/fold(s, "", (c, acc) => Str/concat(acc, Str/of_char(c)))` claimed equal to `s`, costs 187 634, 194 258 and 207 890 units over 16, 32 and 64 characters, some four hundred a character. But a URL is cut, not read through: its host, path and query are pieces of the input, which a position cuts out in O(1) and a fold builds again a character at a time, and `http/Url` would be the one format not written over `Parse`, its refusals placed by hand.
- **`http/Request` and `http/Response` over text.** An HTTP head is octets, not text, and they read it as octets, by [`curios-std`'s README](../../../curios-std/README.md#an-http-message-is-read-through-one-framing-core-and-made-as-one-a-wire-can-carry).
