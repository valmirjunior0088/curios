# Time is one count, and a calendar under proofs

Working specification for rebuilding `/std/time` on one convention that the rest of `/std` and every library above it can rest on. An absolute time is one signed count, a civil one is fields under proofs, and an offset says what RFC 3339 and RFC 9557 say it says. One grammar writes and reads every time, and the host boundary carries every time in one shape.

Two rules are broken today:

- [`/std` invents no value where a proof belongs](../../../curios-std/README.md#std-invents-no-value-where-a-proof-belongs-and-text-is-addressed-by-position): `/std/time/DateTime` publishes seven fields nothing proves, and `Toml` writes an offset of zero as `Z`, which RFC 9557 §2 reads as a different statement than the `+00:00` it read.
- [A narrowing … never hands a consumer a changed value](../../design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md): `os_host` hands a clock reading or a modification time from before 1970 to the guest as `(0, 0)`.

## What this builds on

- **`Nat` and `Int` are unbounded at run time** ([Nat and Int are an i31 until they outgrow it](../../design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md)). A count needs no range chosen for it; a value at or past 2³⁰ is a `$big`; the wire carries an `Int` inside [−2⁶³, 2⁶³) and refuses one outside.
- **The proof machinery `/std` already uses:** [a certified function hands back its result with its proofs](../../../curios-std/README.md#a-certified-function-hands-back-its-result-with-its-proofs), `Int` through its sign view; [a measure descends on a bound](../../../curios-std/README.md#a-measure-descends-on-a-bound); the elaborator proving a bound from the facts in scope.
- **Derived witnesses** over a struct or a constructor carrying proofs: a proof payload contributes nothing to `Eql`, `Ord` or `Hash`, and `Spell` leaves it to the re-parsed text.
- **The wait model** ([A wait is a suspension and only the host waits](../../design/standard-library/a-wait-is-a-suspension-and-only-the-host-waits.md)): a description that waits suspends on handles, and `Io`'s subtree mints the one handle that stands for a time.
- **The calendar types `Toml` held,** under `/std/time` since `3df079adc`.

## The gap

At `c6ae1fcc3`:

- **Three conventions share `/std/time`.** `Duration` and `Timestamp` are sealed `{secs: Nat, nanos: Nat}` pairs whose carry is written four times and proved nowhere; `DateTime` publishes seven unproved fields, so a February 31st, a 25th hour or a weekday that is not its date's is a value and `http` writes it; `Date`, `Time` and `OffsetDateTime` publish theirs under proofs, and `Toml/LocalDateTime` is a fourth civil type.
- **Time is unsigned.** `diff` and `sub` answer zero where the difference is negative; `Timestamp` holds nothing before 1970, so a TOML date from 1900 has no timestamp; `os_host` answers such a moment as `(0, 0)`, and so it answers the modification time of a platform that keeps none and of a dangling link.
- **An offset loses what it said.** `OffsetDateTime` holds whole minutes as an `Int`, so `Z`, `+00:00` and `-00:00` read as one value and write back as `Z`. RFC 9557 §2 makes `Z` mean "UTC is known and the local offset is not" — RFC 3339's `-00:00` — and `+00:00` mean UTC is the preferred reference.
- **The timer is relative while its deadline is absolute.** `Io/Offer/after` reads the clock on every round to ask `clock_timer` for the time left, each host turns that back into an instant of its own, so a round costs two clock reads and fires late by the gap between them, and the row carries its own carry.
- **Text lives in the wrong places.** RFC 3339's grammar is private to `Toml`; IMF-fixdate is in `time/DateTime.crs` though only `http` writes it; `Timestamp`'s `Show` is `secs.nanos`.
- **Deciding a bound is written by hand thirteen times**, `match b: (r) => Option(Holds(r)) | true => … | false => … end`.
- **`Int` has no floored division**: `/` and `%` truncate toward zero, and a pre-1970 date and a negative time's split need the floor.
- **The wire has no signed reply value.** `curios-abi`'s `WireValue` is `Nat(u64)`, `Bytes`, `Handle` and `BytesList`; an `Int` operand crosses and an `Int` row result boxes signed, but a payload field cannot be one.
- **The elaborator leaves a split's own facts out of its products.** An `==` goal is refuted through the two cases of its disequality, and a case's fact — `q < s` — is never multiplied by a fact in scope, where the same fact as a `<=` goal's negation is: `q * d + r == s * d + t` with both remainders below `d` proves `s <= q` and `q <= s` and not `q == s`. (`curios-elab`'s `entailment::products`.)

## Prior art

- **Absolute and civil, kept apart:** [jiff](https://docs.rs/jiff/latest/), [Temporal](https://tc39.es/proposal-temporal/docs/), [java.time](https://docs.oracle.com/javase/8/docs/api/java/time/Instant.html), [Haskell's `time`](https://hackage-content.haskell.org/package/time-1.16.0.1/docs/Data-Time-Calendar.html), [Lean's `Std.Time`](https://github.com/leanprover/lean4/tree/master/src/Std/Time).
- **One count.** Temporal's `Instant` is a BigInt of nanoseconds; jiff's `Timestamp` 96 bits of them; Idris's [`System.Clock`](https://idris-lang.org/Idris2/base/source/System.Clock.html) computes in `Integer` nanoseconds (`toNano`, `fromNano`). java.time stores seconds and a nanosecond below 10⁹, and Lean's `Duration` a second and a nanosecond under a proof that their signs agree — the pair this rejects.
- **A civil date under its proof.** Lean's `PlainDate` carries `valid : year.Valid month day`, and its `ofEpochDay` obtains it by clipping the day Hinnant's algorithm computes (`ofYearMonthDayClip`); Haskell's `fromGregorian` clips and `fromGregorianValid` answers `Nothing`.
- **The calendar.** Hinnant's [algorithms](http://howardhinnant.github.io/date_algorithms.html) through 400-year eras; [Neri and Schneider](https://arxiv.org/abs/2102.06959)'s faster forms, in GCC, the Linux kernel and .NET. Haskell's `Day` is an `Integer` Modified Julian Day and its `Year` an `Integer` with a year 0.
- **The leap second.** Haskell's `TimeOfDay` and chrono admit `:60`; [Ptime](https://erratique.ch/software/ptime/doc/Ptime/index.html) maps it one second later and never answers it; [Temporal](https://tc39.es/proposal-temporal/docs/plaindatetime.html) clamps it to `:59`; jiff and Gleam's [`gleam_time`](https://gleam-time.hexdocs.pm/gleam/time/timestamp.html) ignore leap seconds. The 28th CGPM voted on 15 October 2026 for a continuous UTC from 20 May 2027, with UT1−UTC allowed to reach an hour ([Scientific American](https://www.scientificamerican.com/article/scientists-vote-to-end-leap-seconds-redefining-how-the-world-keeps-time/)): the 27 leap seconds inserted since 1972 stay in written text, and no more come.
- **The unknown local offset.** [RFC 9557 §2](https://www.rfc-editor.org/rfc/rfc9557.html) gives `Z` RFC 3339's `-00:00` meaning and keeps `+00:00` as UTC preferred; Ptime's `of_rfc3339` answers no offset exactly for that convention; Temporal's `PlainDateTime.from` refuses `Z`, "interpreting these strings as a local date and time is usually a bug", and `Instant.toString` writes `Z`.
- **The wire.** WASI 0.3's `system-clock` `instant` is `{seconds: s64, nanoseconds: u32}`, the nanoseconds below 10⁹ and moving forward before the epoch; its `monotonic-clock` offers `wait-until(mark)` beside `wait-for(duration)` ([`clocks.wit`](https://docs.rs/crate/wasmtime-wasi/47.0.3/source/src/p3/wit/deps/clocks.wit)). Swift's [SE-0329](https://github.com/apple/swift-evolution/blob/main/proposals/0329-clock-instant-duration.md) makes `sleep(until:)` the clock's one wait, a deadline being "static throughout" a multi-step timeout.
- **The formats.** RFC 3339 profiles ISO 8601 with four-digit years and whole-minute offsets and admits a lowercase `t` and `z` and a space for the `T`; ISO 8601 writes a year past four digits with a sign; java.time's [`ZoneOffset`](https://docs.oracle.com/javase/8/docs/api/java/time/ZoneOffset.html) writes an offset with seconds as `+hh:mm:ss`, "not ISO-8601"; [RFC 9110 §6.6.1](https://www.rfc-editor.org/rfc/rfc9110#section-6.6.1) forbids a `Date` from a server that cannot approximate the time.

## Decisions

1. **Two layers, and every crossing names an offset.**
   - **Absolute:** `Timestamp`, `Instant` and `Duration`. **Civil:** `Date`, `Time`, `DateTime`, `Month`, `Weekday`, with `Offset` and `OffsetDateTime` where a civil value meets the timeline.
   - A crossing is `OffsetDateTime/of_timestamp(t, o)` and `to_timestamp(odt)`. A civil value never stands for a moment by convention, and no operation takes a `DateTime` "in UTC": what stands for a moment takes a `Timestamp`.
2. **An absolute value is one sealed `Int` count of nanoseconds.**
   - `Timestamp` counts from 1970-01-01T00:00:00Z as POSIX's seconds do; `Instant` from its clock's origin, comparable within one run; `Duration` is signed.
   - `diff` answers the true difference, and a wait of a negative length is over. `Duration` takes `Add` and `Sub`; a timestamp and an instant move by `add(t, d)` and `sub(t, d)`.
   - Constructors name their unit and take an `Int`: `Duration/of_secs`, `of_millis`, `of_micros`, `of_nanos` and `zero`; `Timestamp/of_unix_secs` and `of_unix_nanos`.
   - A `Duration` reads whole units toward zero, as `Int`'s `/` does (`to_secs`, `to_millis`, `to_nanos`); a `Timestamp` reads its Unix seconds floored (`to_unix_secs`) and its nanoseconds exactly.
   - Each takes `Cmp`, and the `before` functions go. `diff(add(t, d), t)` is `d` and `add(add(t, a), b)` is `add(t, a + b)`, each `Eq/refl()`.
3. **A civil value publishes its fields under proofs**, transparent, with `of(…, @ok)` and `try_of(…)`, so a literal outside the calendar is refused where it is written, naming `Valid`.
   - `Date { year: Int, month: Month, day: Nat, @ok: Valid(year, month, day) }`, proleptic Gregorian with a year 0. `Valid` is `Holds(Nat/in_range(day, 1, days_in_month(year, month)))`, February `28 + leap_day(year)` and a year `365 + leap_day(year)`, so every length the proofs sum is linear in one atom.
   - `Time { hour, minute, second, nanosecond, @ok }`, `second <= 60`. `DateTime { date, time }`, which `Toml/LocalDateTime` becomes.
   - `Month` and `Weekday`, Monday first as ISO 8601 numbers it, each a facade of its own, with `to_nat` and `try_of_nat`.
4. **An offset is what RFC 3339's `time-offset` states, read as RFC 9557 reads it.**
   - `Offset` is `unknown()` — UTC is known and the local offset is not, which `Z` writes and `-00:00` reads as — or `known(seconds: Int, @ok: Holds(Int/abs(seconds) <= 86399))`. `Offset/utc` is `known(0)`, written `+00:00`.
   - `Offset/ahead(o)` is how far the stated time is ahead of UTC, zero for `unknown()`, which states its time in UTC.
   - `OffsetDateTime { date_time: DateTime, offset: Offset }` keeps the offset it was written with. Its `Ord` is the instant, then the local date-time, then the offset, `unknown()` first, so two compare equal exactly where they are one value.
5. **One bijection carries the calendar:** `Date/to_days(d) -> Int` and `Date/of_days(n: Int) -> Date`, days since 1970-01-01.
   - `to_days` is Hinnant's closed form over `Int/DivMod`.
   - `of_days` takes the year Hinnant's inverse computes as where to start, not as an answer: the days from that year's first of January are walked forward a year at a time while a year fits, or back while they are below zero, each over `Nat/descend`, and then at most twelve months. The guards are the proof: the first year whose length exceeds the days left, and the first month whose length does, give `Valid`. The estimate is exact, so no walk takes a step, and nothing is clipped or invented where it would not be.
   - `Date/weekday` (`n + 3` modulo 7 from Monday), `add_days`, `days_between`, `is_leap` and `days_in_month` follow from the pair.
6. **The leap second is admitted and placed by POSIX's formula.** `Time` admits `:60` so text that writes one reads back as written; `to_timestamp` counts `:60` as the next minute's `:00`; `of_timestamp` never answers one. No leap-aware timescale is built: UTC is continuous from 20 May 2027.
7. **One grammar, in one place.**
   - `/std/time/iso8601` holds the extended format's pieces over `Parse(Str, _)` and its writers; every civil type, `Timestamp` and `Duration` shows and reads through them (`to_str`, `Show`, `of_str`). A year past four digits is written with its sign, `+10000-01-01`, `-0001-01-01`; an offset with seconds `+05:30:15`, as java.time writes past ISO 8601.
   - `/std/time/rfc3339` is the profile: its evidence, `FourDigits(d: Date)` (`0 <= year <= 9999`) and `WholeMinutes(o: Offset)`, and the readers and writers that take it. Its readers accept §5.6's `t`, `z` and space; its writers write `T`, and `Z` for `unknown()` alone.
   - A `Timestamp` shows as its UTC date-time at `Offset/unknown()`, `…Z`, and reads any date-time with an offset. A `Duration` shows as `PT1.5S` or `-PT1.5S`, hours, minutes and seconds, zero parts left out, `PT0S` for none. Fractions are trimmed of trailing zeros. `Instant` has no text.
   - `Toml` composes `rfc3339`'s pieces, its date-bearing variants carrying `FourDigits` and its offset-bearing one `WholeMinutes`, so `encode` stays total and writes the offset it read.
8. **The same witnesses everywhere.** Civil types derive `Eql`, `Ord`, `Hash` and `Spell`, `Month` in calendar order; `OffsetDateTime` writes `Ord`. Absolute types derive `Eql`, `Ord` and `Hash` and write `Cmp`; `Timestamp` and `Duration` write `Spell` as their constructor's call, and `Instant`, which has no constructor, none.
9. **Every time crosses the wire in one shape:** signed seconds and nanoseconds below 10⁹, as WASI 0.3's `system-clock` does.
   - `curios-abi`'s `Timestamp` is renamed `ClockReading { secs: i64, nanos: u64 }`, answered by `clock_wall` and `clock_mono`; `file_stat`'s `mtime_secs` is signed. `WireValue` gains `Int(i64)`, which `curios-runtime`'s lowering answers beside `Nat`.
   - A host answers what it read and nothing in its place. `clock_wall` refuses a reading past `i64` seconds, as `rand_bytes` refuses what it cannot draw. `file_stat` fails with `EOVERFLOW` where a time cannot cross, as `stat(2)` does, and with the platform's own error where it keeps no modification time, where today it answers the epoch; a dangling link reports what `symlink_metadata` reads of the link itself, where today it reports zero sizes and the epoch.
   - `clock_timer` gives way to `clock_deadline(secs: i64, nanos: u64) -> Handle`, ready once `clock_mono` reads at least that instant. A deadline past is ready at once; one past what a host's clock holds is never ready; the guest saturates one past the wire's range.
   - Whether a suspended machine's time counts is the host's, as Rust's `Instant` documents it.
   - The mock host keeps one timeline: a scripted reading moves it forward, a wait moves it to the soonest deadline it holds out for, and `clock_mono` reads it, so no reading after a wait is before the deadline it woke for.
10. **A deadline is an `Instant`; a timeout is a `Duration` that starts now.**
    - `Io/sleep_until(i: Instant)` is the wait the host offers, public; `Io/sleep(d)` reads the clock once and sleeps until `now + d`. `Io/timeout(d, a)`, `Test/timeout` and `http/Client.timeout` keep their names.
    - `Io/Offer/after(d)` holds its instant and waits on it with no clock read inside its wait.
    - `http/Server.deadline`, a length, becomes `timeout`.
11. **HTTP's date is HTTP's.** IMF-fixdate moves into `http/Field/Value` as `of_timestamp(t: Timestamp, @four_digits: FourDigits(OffsetDateTime/of_timestamp(t, Offset/utc).date_time.date))`, its three-letter names private to it. `Server` decides that evidence for the moment it dates a reply and omits `Date` where it fails (RFC 9110 §6.6.1).
12. **Three foundation pieces the rest consumes.**
    - `Bool/decide(b) -> Option(Holds(b))` replaces the thirteen decisions.
    - `Int/DivMod(n: Int, d: Nat)`, floored division by a positive natural: the quotient, the remainder, `n = quotient · d + remainder`, `remainder < d` and `unique`, built through `Int`'s sign view on `Nat/DivMod`.
    - The elaborator multiplies a split's own facts in the case that opens them, as it multiplies the facts and the negated goal, so `unique`'s quotient is one `True/proved()`.

## Stages

Each stage lands alone, as one commit, with `/std`, the corpus and the suite green.

1. **Foundation**, three commits.
   - A refused bound's report writes the facts it lists through the metavariables elaboration solved, as it writes its counterexample's atoms; a local `let`'s statement is listed today as `?.add(?.mul(b, c), c)`.
   - The case products in `entailment`, with a row in `curios`'s `tests::bounds` and the decision's sentence.
   - `Bool/decide` at its thirteen sites; `Int/DivMod` with `unique`, held against `/` and `%` over both signs in a `curios-std` test.
2. **The absolute layer.** One count each, signed `diff`, `Cmp`, the unit constructors and readers, `Spell`; their readers moved over; `Server.deadline` becomes `timeout`. The wire is untouched: the readings fold into the count, and `Io/timer` hands `clock_timer` the length clamped at zero and split. Until stage 4 retires it, the seven-field `DateTime/of_timestamp` answers an `Option`, none before 1970, and `Server` omits `Date` there; `Timestamp`'s `Show` stays `secs.nanos` over its floored seconds until stage 5.
3. **The wire.** `WireValue::Int`, `ClockReading`, signed `mtime_secs`, `clock_deadline` across `curios-abi`, `os_host`, the harness and the mock host, whose timeline is one; `Io/sleep_until` replaces `Io/timer`, and `Offer/after` waits on its instant; "timer" becomes "deadline" through the hosts, `event::READY`'s doc, `curios-runtime/README.md` and the wait decision.
4. **The civil layer.** `Month`, `Weekday`, `Date`, `Time`, `DateTime`, `Offset` and `OffsetDateTime`; the calendar core and the crossings; `time/rfc3339` with `FourDigits` and `WholeMinutes`, and `Toml` over them, `LocalDateTime` retired; IMF-fixdate in `http` over a `Timestamp`; the seven-field `DateTime` retires.
5. **Text.** `time/iso8601`; `to_str`, `Show` and `of_str` for every civil type, `Timestamp` and `Duration`; `rfc3339`'s readers and writers over its pieces, and `Toml`'s grammar replaced by them, writing the offset it read.
6. **Evidence and retirement.** `of_days` and `to_days` inverse over every day of one era, walked against the successor date; the absolute laws and closed dates — 2000-02-29 a Tuesday, `try_of` of 1900-02-29 `none` — claimed in the corpus as `let _…` items, each `Eq/refl()`, since a fact conversion decides is no lemma of `/std`; `curios-std/README.md` gains the decision, and the specification is deleted.

## Verification

- **Every stage:** `cargo xtask workspace format` and `lint`, the stage's tests by name, and a scratch copy of `curios-std/src` put to `wonder diagnostics` before it lands.
- **Across the stages:** `curios-pipeline`'s paste-back sweeps, the spelling tests, `every_source_is_canonically_formatted`, `every_authored_source_is_lint_clean` and the purity tests: `time` stays `Performs`, `Toml` `Pure`, reaching only `time`'s calendar and text, none of it `foreign`.
- **The full gate** once the last stage lands.
- **Figures the work may move:** `/std`'s elaboration units; the `entailment::derived` sample and `curios`'s `tests::bounds`; `census.rs` over `TOML_DRIVER`; `fixpoint_pass_measurements`; and the run-time cost of a clock read, every `Timestamp` and an `Instant` past a second of uptime being a `$big`.

## Design decisions this overturns or corrects

- **[A wait is a suspension and only the host waits](../../design/standard-library/a-wait-is-a-suspension-and-only-the-host-waits.md)**: "a deadline is a timer's handle" becomes a handle `clock_deadline` mints at an instant on the monotonic clock.
- **`curios-runtime/README.md`'s wait decision**: timers become deadlines, at their instants.
- **[A bound that follows from the facts in scope is proved by the elaborator](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md)**: the products take a split's own facts in its case.
- **`lib.crs`'s `time` paragraph**: a `DateTime` is a civil date and time, and HTTP's form is HTTP's.

## Non-goals

Each waits on a consumer: time zones beyond a fixed offset and RFC 9557's bracketed suffix; calendar spans of months and years; formatting by pattern; calendars other than the Gregorian; ISO week and ordinal dates; `Key` witnesses for the time types; `timeout_at`; TOML 1.1's times without seconds.

## Rejected

- **A `{secs, nanos}` pair**, today's and java.time's, or Lean's under a sign proof: a carry at every operation, proved at none, and associativity no conversion.
- **A fixed-width count**, jiff's and Rust's: `Int` is unbounded, and the wire already refuses past `i64`.
- **One family indexed by its clock**, C++'s `time_point<Clock>` and Idris's `Clock type`: what `Timestamp` and `Instant` share is six one-line operations, what they do not — text, the civil crossing, a constructor — is most of each, and every report would spell `Point(Wall)`.
- **A wall reading carried inside every `Timestamp` beside a monotonic one**, as Go's `Time` does, which a reader has to know when an operation strips.
- **`Date` as a day count**, Haskell's `Day`: a literal outside the calendar would be no refusal.
- **A `Nat` year**: a signed `Timestamp` reaches before year 0.
- **Clamping or refusing the 61st second**, as Temporal and Go do, which drops text TOML reads back.
- **`Z` and `+00:00` as one offset**: RFC 9557 states them apart, and a codec that collapses them writes back a different statement.
- **Clipping a computed date into the calendar**, as Lean's `ofEpochDay` does, and a check with a fallback: the walk's guards are the proof, and the estimate only says where it starts.
- **Walking up to 400 years from the era's start**: the same proof at a walk a call, where the estimate leaves none.
- **A relative timer row**: two clock reads a round, a drift by their gap, and a carry in each host.
- **The four-digit bound on `Date`**: it belongs to a format, and the formats that need it carry it.

## Completion and retirement

Every stage has landed, `curios-std/README.md` states the convention as a decision of its own with the rationale and rejections above, the decisions this corrects say what the code does, and nothing cites this file. Then the specification is deleted, and its number is left free.
