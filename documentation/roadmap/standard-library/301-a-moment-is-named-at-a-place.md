# A moment is named at a place

**Not refined yet.** It waits for its decisions. `/std/time` names a moment at a fixed offset (`Offset`, so many seconds east of UTC, `curios-prelude-build/std/time/Offset.crs`), so the civil time in a city across a daylight-saving change, or a recurring local time, has no answer. The IANA time zone database is the one answer, and both hosts hold it: `/usr/share/zoneinfo` as TZif version 2 files, each ending in the POSIX rule that carries a zone past its last transition (`CET-1CEST,M3.5.0,M10.5.0/3`), tzdb 2026f on the Linux this was read on, and on macOS at `/var/db/timezone/zoneinfo` behind the same path.

## What it reserves

A `Zone` read by name, the offset at an instant, and an instant from a civil time, which a gap leaves without one and an overlap gives two; the reading in Curios over `File` and `Bytes`, of the TZif records and the POSIX rule.

## Peers

Python's `zoneinfo` and Go's `time.LoadLocation` read the host's database, Go with an embedded fallback; Java's `java.time` bundles one; Rust leaves it to `chrono-tz` and `jiff`, which embed or read.

## Open questions

- **The host's database or a pinned one**, as `curios-unicode` pins Unicode's: a host's version moves under a program, and a pinned one goes stale.
- **What a civil time in a gap or an overlap answers**: Java's and Temporal's compatible choice, a refusal, or both offered.
- **What a name the database lacks answers.**

## Stages

Once decided: the TZif reader and the POSIX rule; the `Zone` API and its civil conversions; their tests against a pinned set of zones.
