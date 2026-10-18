# Time is one count, and a calendar under proofs

Working specification for rebuilding `/std/time` on one convention that the rest of `/std` and every library above it can rest on. An absolute time is one count, and a civil one is a set of fields under proofs. One grammar writes and reads both, and the host boundary carries every time in one shape.

Two rules are broken today:

- [`/std` invents no value where a proof belongs](../../../curios-std/README.md#std-invents-no-value-where-a-proof-belongs-and-text-is-addressed-by-position): `/std/time/DateTime` publishes seven fields nothing proves.
- [A narrowing … never hands a consumer a changed value](../../design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md): both native hosts hand a clock reading or a modification time from before 1970 to the guest as `(0, 0)`.

## What this builds on

- **`Nat` and `Int` are unbounded at run time** ([Nat and Int are an i31 until they outgrow it](../../design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md)).
  - A count needs no range chosen for it.
  - A value at or past 2³⁰ is a `$big`.
  - The host wire carries an `Int` inside [−2⁶³, 2⁶³) and refuses one outside that range.
- **The proof machinery `/std` already uses:**
  - [A certified function hands back its result with its proofs](../../../curios-std/README.md#a-certified-function-hands-back-its-result-with-its-proofs) (`Nat/DivMod`, `Nat/Gcd`);
  - [a measure descends on a bound](../../../curios-std/README.md#a-measure-descends-on-a-bound) (`Nat/descend`);
  - the elaborator proving a bound from the facts in scope (`documentation/syntax.md`, *Bounds*), with `Bool/and/both` and `Nat/le/to_in_range` to build a conjunction and a range check.
- **Derived witnesses.** `Eql`, `Ord`, `Hash` and `Spell` derive over a struct whose fields carry proofs, as `/std/Tui/Glyph` does. A proof payload contributes nothing to them.
- **The wait model** ([A wait is a suspension and only the host waits](../../design/standard-library/a-wait-is-a-suspension-and-only-the-host-waits.md)).
  - A description that waits suspends on handles.
  - `Io`'s own subtree mints the one kind of handle that stands for a time. Today that is `clock_timer`, private to `/std/Io` behind its private `timer`, and closed by the wait that made it.
- **The calendar types `Toml` held,** moved under `/std/time` at `3df079adc`: `Date`, `Time` and `OffsetDateTime`, each under a decided `Valid`. TOML's four-digit year rides its variants as `Toml/FourDigits`.

## The gap

At `3df079adc`:

- **Three conventions share `/std/time`.**
  - `Duration` and `Timestamp` are sealed `{secs: Nat, nanos: Nat}` pairs, and `Instant` wraps a `Duration`. No field proves `nanos < 10⁹`; the carry is written again in `Duration/of`, `Timestamp/of_unix`, `Duration/add` and `Duration/sub`.
  - `DateTime` publishes `year`, `month`, `day`, `weekday`, `hour`, `minute` and `second` with nothing proved. A program builds a February 31st, a 25th hour or a weekday that is not its date's, and `http/Field/Value/of_date_time` writes whatever it is given.
  - `Date`, `Time` and `OffsetDateTime` publish their fields under proofs.
- **Time is unsigned.**
  - `Timestamp/diff`, `Instant/diff` and `Duration/sub` answer zero where the true difference is negative.
  - `Timestamp` cannot hold a moment before 1970, so a TOML date from 1900 has no timestamp.
  - `os_host`'s `clock_wall` and `file_stat` answer such a moment as `(0, 0)` (`unwrap_or_default`, `unwrap_or((0, 0))`, `curios-runtime/src/os_host.rs`).
- **The timer is relative while its deadline is absolute.**
  - `Io/Offer/after` fixes `due` once. On every round it then reads the clock again and asks `clock_timer` for `diff(due, now)`.
  - Each host turns that back into an instant on its own monotonic clock, so a round costs two clock reads, and the timer fires late by the gap between the guest's read and the host's.
  - The row also states and implements its own carry ("nanoseconds past a second carry into the seconds", `curios-abi/src/host/ops.rs`).
- **Text lives in the wrong places.**
  - RFC 3339's grammar is private to `Toml` (`Toml/date_time.crs`, and `date_str`, `time_str` and `offset_str` in `Toml/encode.crs`), so no other reader or writer of a date shares it.
  - IMF-fixdate is in `time/DateTime.crs`, though only `http` writes it.
  - `Timestamp`'s `Show` is `secs.nanos`.
- **Deciding a bound is written by hand thirteen times:** `match b: (r) => Option(Holds(r)) | true => some(True/qed()) | false => none() end`, in `Flt` (three), `Char/Valid`, `Int`, `Int/le`, `Int/lt`, `Nat/le`, `Nat/lt`, `Str/Valid`, `time/Date`, `time/Time` and `time/OffsetDateTime`.
- **`Int` has no floored division.** `/` and `%` truncate toward zero (`-7 / 2` is `-3`, probed at `b63bccbd5`), and both a pre-1970 calendar date and a negative time's split into seconds and nanoseconds need the floor.

## Prior art

- **Absolute and civil, kept apart.**
  - [jiff](https://docs.rs/jiff/latest/) separates `Timestamp` and `Zoned` from `civil::Date`, `civil::Time` and `civil::DateTime`.
  - [Temporal](https://tc39.es/proposal-temporal/docs/) separates `Instant` from `PlainDate`, `PlainTime` and `PlainDateTime`.
  - [java.time](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html) separates `Instant` from `LocalDate`, `LocalTime` and `LocalDateTime`, with `OffsetDateTime` a local date-time at a `ZoneOffset`.
  - [Haskell's `time`](https://hackage.haskell.org/package/time-1.8.0.4/docs/Data-Time.html) pairs a `Day` with a `TimeOfDay` as `LocalTime` beside `UTCTime`.
  - TOML's four kinds are exactly java.time's four.
- **The calendar.** Hinnant's [chrono-compatible date algorithms](http://howardhinnant.github.io/date_algorithms.html) map a civil date to days since 1970 and back through 400-year eras of 146 097 days. They are tested over every day of ±1,000,000 years ([slides](https://itp.uni-frankfurt.de/~sciarra/C++_User_Group/Talks/2019_05.pdf)) and underlie C++20's calendar ([P0355R7](https://open-std.org/JTC1/SC22/WG21/docs/papers/2018/p0355r7.html)).
- **The leap second.**
  - Haskell's `TimeOfDay` admits a 61st second in any local minute.
  - chrono's `NaiveTime` reads and writes `:60` ([chrono](https://www.rustmax.net/api/chrono/struct.naivetime)).
  - Temporal clamps it to `:59` ([`PlainTime`](https://tc39.es/proposal-temporal/docs/plaintime.html)).
  - Go refuses it ([golang/go#50888](https://golang.org/issue/50888)).
- **The formats.**
  - [RFC 3339](https://www.rfc-editor.org/rfc/rfc3339) profiles ISO 8601's extended format with four-digit years and whole-minute offsets, and admits a lowercase `t` and `z` and a space for the `T` (§5.6).
  - java.time writes a year past that profile in ISO 8601's expanded form: `+10000-01-01`, `-0001-01-01`.
  - [RFC 9110 §6.6.1](https://www.rfc-editor.org/rfc/rfc9110#section-6.6.1) forbids an origin server whose clock cannot approximate the current time from sending `Date`.

## Decisions

1. **Two layers, and every crossing names an offset.**
   - **Absolute** values are a point on a timeline or a length: `Timestamp`, `Instant` and `Duration`.
   - **Civil** values are calendar and clock fields with no zone: `Date`, `Time`, `DateTime`, `Month` and `Weekday`, with `Offset` and `OffsetDateTime` where a civil value meets the timeline.
   - A crossing is `OffsetDateTime/of_timestamp(t, o)` and `OffsetDateTime/to_timestamp(odt)`, with `Offset/utc` the offset HTTP and a UTC log name. No other operation crosses, so a civil value never stands for a moment by convention.
2. **An absolute value is one sealed `Int` count of nanoseconds.**
   - **What each counts.**
     - `Timestamp` counts from 1970-01-01T00:00:00Z without leap seconds, as POSIX's seconds since the epoch do.
     - `Instant` counts from its clock's origin, and two of them compare only within one run.
     - `Duration` is signed.
   - **What the sign gives.** `diff` answers the true difference, and a wait of a negative length is already over. `Duration` takes `Add` and `Sub`; a timestamp moves by `Timestamp/add(t, d)` and `sub(t, d)`, since `/std/ops` is homogeneous.
   - **Constructors** name their unit and take an `Int`: `Duration/of_secs`, `of_millis`, `of_micros` and `of_nanos`, and `Timestamp/of_unix_secs` and `of_unix_nanos`. A numeral takes `Int` from its context, so `Duration/of_secs(30)` reads as it does today.
   - **Readers.**
     - A `Duration` reads whole units toward zero, as `Int`'s `/` does: `to_secs`, `to_millis` and `to_nanos`.
     - A `Timestamp` reads its Unix seconds floored, the second it lies in, as POSIX counts: `to_unix_secs`, and `to_unix_nanos` exactly.
   - **Comparison.** Each type takes `Cmp`, so `a < b` reads directly, and `Duration/before`, `Timestamp/before` and `Instant/before` go.
   - **Laws are conversion's.**
     - Over one count, `diff(add(t, d), t)` is `d` and `add(add(t, a), b)` is `add(t, a + b)`, each `Eq/refl()`.
     - Over a carried `{secs, nanos}`, the sum's associativity is refused: the carry is a branch conversion does not decide.
     - Both were probed at `b63bccbd5`, with a false law refused beside them as a control.
3. **A civil value publishes its fields under proofs.** Each type is transparent and offers `of(…, @ok)` under the evidence and `try_of(…)` deciding it, so a literal outside the calendar is refused at elaboration, naming `Valid`.
   - `Date { year: Int, month: Month, day: Nat, @ok: Valid(year, month, day) }`, in the proleptic Gregorian calendar with a year 0, as ISO 8601 counts. `Valid` is `Holds(Nat/in_range(day, 1, days_in_month(year, month)))`.
   - `Time { hour, minute, second, nanosecond, @ok }`, with `second ≤ 60`.
   - `DateTime { date: Date, time: Time }`.
   - `Offset { seconds: Int, @ok: Holds(Int/abs(seconds) <= 86399) }`, in seconds, the unit a zone database answers in.
   - `OffsetDateTime { date_time: DateTime, offset: Offset }`: RFC 3339's `date-time`, kept apart from `Timestamp` so that its text keeps the offset it was written with.
   - `Month`, January to December, and `Weekday`, Monday to Sunday as ISO 8601 numbers it, each a facade of its own. `Month/to_nat` and `try_of_nat` convert to and from the number.
4. **One bijection carries the calendar:** `Date/to_days(d) -> Int` and `Date/of_days(n: Int) -> Date`, days since 1970-01-01.
   - **The era.** `of_days` takes the 400-year era by `Int/DivMod(n, 146097)`, as Hinnant's algorithms do; any 400 consecutive Gregorian years hold 146 097 days.
   - **The walk.** It then walks at most 400 years over `Nat/descend`, with the days left as the measure, and at most twelve months over the list of months.
   - **The proof.** It stops at the first month whose length exceeds the days left, and that guard is the proof that the day is valid. No branch decides validity at run time, and none invents a date.
   - **Probed at `b63bccbd5`:**
     - the descent's step proof is the arithmetic in scope;
     - the month walk refutes its empty case from the hypothesis;
     - a conjunction of bounds is built with `Bool/and/both`, where `True/proved()` alone does not reach one.
   - **What follows from the pair:**
     - `Date/weekday` is `n + 3` modulo 7, from Monday;
     - `Date/add_days`;
     - `Date/days_between`, signed;
     - `Date/is_leap` and `Date/days_in_month`.
   - `to_days` is Hinnant's closed form over floored division.
5. **The leap second is admitted and placed by POSIX's formula.**
   - `Time` admits a second of 60, so RFC 3339 and TOML text that writes one reads back as written.
   - `to_timestamp` counts the fields as POSIX's seconds-since-the-epoch formula does, so `:60` lands on the next minute's `:00`. No code is written for it.
   - `of_timestamp` never answers a 60th second.
6. **One grammar, in one place.**
   - **The text.** Every civil type, `Timestamp` and `Duration` shows and reads its ISO 8601 extended form: `Show` and `of_str`.
   - **The RFC 3339 profile.** RFC 3339 is that form's profile of four-digit years and whole-minute offsets. `/std/time/rfc3339` holds it:
     - the parser pieces over `Parse(Str, _)`;
     - the writers;
     - the evidence its text needs, `FourDigits(d: Date)` and `WholeMinutes(o: Offset)`. `FourDigits` moves there from `Toml`.

     Its readers accept §5.6's lowercase `t` and `z` and a space for the `T`, and its writers write the uppercase `T`.
   - **Past the profile.** A year past four digits is written in ISO 8601's expanded form, as java.time writes it: `+10000-01-01`, `-0001-01-01`. An offset with seconds is written `+05:30:15`.
   - **Duration and Timestamp.** A `Duration` shows as `PT1.5S` or `-PT1.5S`, in hours, minutes and seconds with zero parts left out and `PT0S` for none. A `Timestamp` shows in UTC with a `Z`.
   - **Fractions** are trimmed of trailing zeros.
   - **`Toml` composes `rfc3339`'s pieces,** and its date-bearing variants carry `FourDigits`, its offset-bearing one `WholeMinutes` too, so `encode` stays total.
7. **The same witnesses everywhere.**
   - **Civil types** derive `Eql`, `Ord`, `Hash` and `Spell`.
     - `Month` is declared in calendar order, so `Date`'s derived order is the calendar's.
     - `OffsetDateTime`'s `Ord` is written: the instant first, then the local date-time, as java.time compares, so two values compare equal exactly where they are structurally equal.
   - **Absolute types** derive `Eql`, `Ord` and `Hash`, and write `Cmp`. A sealed type's literal cannot be written outside its subtree, so each also writes `Spell` as its constructor's call.
   - **`Instant`** has no text: only the difference of two readings means anything.
8. **Every time crosses the wire in one shape:** signed seconds and nanoseconds below 10⁹.
   - **The reading.** `curios-abi`'s `Timestamp`, which also carries the monotonic clock, is renamed `ClockReading { secs: i64, nanos: u64 }`. It is what `clock_wall` and `clock_mono` answer, and `file_stat`'s `mtime_secs` becomes signed. A moment before 1970 crosses as itself.
   - **The deadline row.** `clock_timer(secs, nanos)` gives way to `clock_deadline(secs: i64, nanos: u64) -> Handle`, a handle ready once the monotonic clock `clock_mono` reads is at least that instant.
     - A deadline already past is ready at once, which covers a wait of no time or of a negative one.
     - One past what a host's clock can hold is never ready, as `os_host`'s `Timer(None)` is today.
     - The guest saturates a deadline past the wire's range, which no clock reaches, so the saturation changes no answer.
   - **The mock host** keeps its deadlines and its monotonic clock on one virtual time: a deadline is due once its waits have slept to it, and `clock_mono` reads that time once its script is spent, where it read the epoch.
9. **A deadline is an `Instant`; a timeout is a `Duration` that starts now.**
   - `Io/deadline(i: Instant) -> Io(Handle)` is private to `Io`'s subtree, as `timer` is.
   - `Io/sleep(d)` reads the clock once and waits on `deadline(now + d)`.
   - `Io/timeout(d, a)`, `Test/timeout` and `http/Client.timeout` keep their name, which says the length runs from the call.
   - `Io/Offer/after(d)` holds `due` and waits on `Io/deadline(due)` with no clock read inside its wait.
   - **Renames.** `http/Server.deadline`, a length, becomes `timeout`, the name `Client` gives the same thing, and `http.crs`'s "the deadline" for a client's timeout becomes "the timeout".
10. **HTTP's date is HTTP's.**
    - IMF-fixdate moves into `http/Field/Value` as `of_date_time(d: DateTime, @four_digits: FourDigits(d.date))` over a UTC date-time, with its three-letter names private to it. `year_digits`'s writing of a year past four digits goes.
    - `Server` decides `FourDigits` of the moment it dates a reply and omits `Date` where it fails. A clock reading no IMF date can state is no reasonable approximation of the current time, the case RFC 9110 §6.6.1 forbids a `Date` for.
11. **Two foundation pieces the rest consumes.**
    - `Bool/decide(b) -> Option(Holds(b))` replaces the thirteen hand-written decisions.
    - `Int/DivMod(n: Int, d: Nat)` is floored division by a positive natural, `n = quotient · d + remainder` with `remainder < d`, mirroring `Nat/DivMod` and built through `Int`'s sign view.

## The components

| Component | Where |
| --- | --- |
| `Bool/decide` and its thirteen sites | `curios-std/src/Bool.crs`; `Flt`, `Char/Valid`, `Int`, `Int/le`, `Int/lt`, `Nat/le`, `Nat/lt`, `Str/Valid`, `time/Date`, `time/Time`, `time/OffsetDateTime` |
| `Int/DivMod` | `curios-std/src/Int/DivMod.crs`, declared in `Int.crs` |
| The absolute layer | `curios-std/src/time/{Duration,Timestamp,Instant}.crs` |
| Readers of the absolute layer | `Io.crs`, `Io/Offer.crs`, `Test.crs`, `Tui.crs`, `Tui/Session.crs`, `fs.crs`, `http/Client.crs`, `http/Server.crs`, `http.crs` |
| The wire | `curios-abi/src/host/{types,ops,crossing,tests}.rs`; `curios-runtime/src/os_host.rs`, `mock_host.rs` and its tests; `curios-js/src/harness.js` |
| The civil layer | `curios-std/src/time/{Month,Weekday,Date,Time,DateTime,Offset,OffsetDateTime}.crs`, `time.crs` |
| Text | `curios-std/src/time/rfc3339.crs`; `Toml.crs`, `Toml/date_time.crs`, `Toml/encode.crs` |
| HTTP's date | `curios-std/src/http/Field/Value.crs`, `http/Server.crs` |
| Corpus tests | `curios-std/src/tests/data/date_time.crs`, `tests/io/waits.crs`, `tests/http/message.crs` |
| Rust tests | `curios/src/tests/runtime/std_tests.rs`, `tests/toml.rs`, `tests/tui.rs`, `tests/concurrent.rs`, `tests/host/http_tests.rs`, `tests/host/reader_tests.rs`, `tests/harness/resource_tests.rs`, `tests/instrumented/test_support.rs` |
| Documents | `curios-std/src/lib.crs`'s `time` paragraph; `curios-runtime/README.md`; `a-wait-is-a-suspension-and-only-the-host-waits.md`; `curios-abi`'s `event::READY` doc; `curios-std/README.md` at retirement |

## Stages

Each stage lands alone, as one commit, with `/std`, the corpus and the suite green.

1. **Foundation.**
   - `Bool/decide` replaces the thirteen decisions.
   - `Int/DivMod` lands with its identity and bound.
   - Check: lint; `curios-std`'s `numeric` and `flt` units; a `curios-std` test holding `Int/DivMod` against `Int`'s `/` and `%` over both signs.
2. **The absolute layer.**
   - `Duration`, `Timestamp` and `Instant` become one count each, with signed `diff`, `Cmp`, the unit constructors and readers, and `Spell`.
   - Their readers move over, and `http/Server.deadline` becomes `timeout`.
   - The wire is untouched: `Instant/now` and `Timestamp/now` fold the reading's pair into the count, and `Io/timer` hands `clock_timer` the length split into whole seconds and nanoseconds.
   - Check: `curios`'s `tests::runtime::std_tests` (restated over the counts), `tests::tui`, `tests::concurrent` and `tests::host::http_tests`; `curios-std`'s `io` and `http` units.
3. **The wire.**
   - `ClockReading`, signed, and `clock_deadline` land across `curios-abi`, `os_host`, the harness and `mock_host`, the row and its three implementations together. The mock host's monotonic clock reads its virtual time.
   - `Io/deadline` replaces `Io/timer`, and `Offer/after` waits on `due`.
   - "Timer" becomes "deadline" through the hosts' resources, `event::READY`'s doc, `curios-runtime/README.md` and the wait decision.
   - Check: `curios-abi`'s host tests; `curios-runtime`'s mock host tests, with a deadline already past ready at once; `curios-prelude-archive`'s `the_library_declares_every_row_a_host_answers`; the suites of stage 2; a `file_stat` of a file dated before 1970 crossing as itself.
4. **The civil layer.**
   - `Month`, `Weekday`, `Date` and `Time` are restated, with `DateTime { date, time }`, `Offset` and `OffsetDateTime { date_time, offset }`.
   - The calendar core and the crossings land.
   - `Toml` holds an `Offset` and `WholeMinutes`.
   - IMF-fixdate moves into `http`, and `Server` omits `Date` where `FourDigits` fails.
   - The seven-field `DateTime` retires.
   - Check: `curios-std`'s `data` and `http` units, with the date tests restated over the new fields; `curios`'s `tests::toml`, its rows unchanged.
5. **Text.**
   - `time/rfc3339` lands.
   - `Show` and `of_str` are added for every civil type, `Timestamp` and `Duration`.
   - `FourDigits` and `WholeMinutes` move from `Toml` into `rfc3339`, and `Toml` composes its pieces.
   - The tests that pin `Timestamp`'s text are restated for its new `Show`.
   - Check: `curios`'s `tests::toml`, its date-time rows reading and writing as before; `curios-std`'s `data` unit with round trips of every form, the expanded years and the leap second among them.
6. **Evidence and retirement.**
   - `of_days` and `to_days` are inverse over every day of one 400-year era, walked a day at a time against the successor date.
   - The absolute layer's laws are lemmas inside `/std/time`, each `Eq/refl()` there.
   - Closed dates are claimed in types: `Date/weekday` of 2000-02-29 is Tuesday, and `try_of` of 1900-02-29 is `none`.
   - `curios-std/README.md` gains the decision, and the specification is deleted.

## Verification

- **Every stage:** `cargo xtask workspace format` and `lint`, the stage's tests by name, and a scratch copy of `curios-std/src` put to `wonder diagnostics` before it lands.
- **Across the stages:** `curios-pipeline`'s paste-back sweeps over each new public declaration, the spelling tests, `curios-std`'s `every_source_is_canonically_formatted`, and `curios-prelude-archive`'s `every_authored_source_is_lint_clean` and purity tests.
  - `time` stays `Performs` and `Toml` `Pure`. `Toml` reaches only `time`'s calendar and text declarations, none of them `foreign`.
- **The full gate** once the last stage lands.
- **Figures the work may move**, named rather than retaken unless a stage is about them: `/std`'s elaboration units (`cargo xtask profile-prelude elaboration`), `curios/src/tests/codegen/census.rs` over `TOML_DRIVER`, and `fixpoint_pass_measurements`, cited in `curios-cont/src/optimize/fields.rs`.

## Design decisions this overturns or corrects

- **[A wait is a suspension and only the host waits](../../design/standard-library/a-wait-is-a-suspension-and-only-the-host-waits.md)** says "a deadline is a timer's handle". A deadline becomes a handle `clock_deadline` mints at an instant on the monotonic clock.
- **`curios-runtime/README.md`'s wait decision** names timers and their instants. The resource becomes a deadline, at its instant.
- **`lib.crs`'s `time` paragraph** calls a `DateTime` "a timestamp as the Gregorian calendar and the clock name it in UTC, written out as HTTP writes one". A `DateTime` becomes a civil date and time, and HTTP's form becomes HTTP's.

## Non-goals

Each waits on a consumer:

- time zones beyond a fixed offset;
- calendar spans of months and years;
- formatting by pattern;
- leap-aware timescales;
- calendars other than the Gregorian;
- `Key` witnesses for the time types;
- a public `Io` operation that waits to a deadline. `http/Client`'s one timeout already spans its redirects.

## Rejected

- **A `{secs, nanos}` pair**, which today's types are. The carry is written at every operation and proved at none, and associativity is no conversion.
- **A fixed-width count**, as jiff's and Rust's types are. `Int` is unbounded, and the wire already refuses past `i64`.
- **`Date` as a day count.** Every field read would be a walk, and a literal outside the calendar would be no refusal.
- **A `Nat` year.** A signed `Timestamp` reaches before year 0, so a year must be an `Int` for every timestamp to have a date.
- **Clamping or refusing the 61st second**, as Temporal and Go do. Either drops text that TOML's own rows read back.
- **A relative timer row**, today's. Each round reads the clock twice, the timer drifts by the gap, and each host carries its own carry.
- **The four-digit bound on `Date`.** It belongs to a format, and the formats that need it carry it.
- **A run-time check with a fallback where `of_days` cannot fail.** The walk's guards are its proof.

## Completion and retirement

Every stage has landed, `curios-std/README.md` states the convention as a decision of its own with the rationale and rejections above, the decisions this corrects say what the code does, and nothing cites this file. Then the specification is deleted, and its number is left free.
