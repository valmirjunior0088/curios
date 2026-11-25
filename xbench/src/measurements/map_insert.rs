//! What a `/std/Map` insert costs: the `spines` insert slope, the end-to-end instrument behind `documentation/design/compilation/a-value-costs-when-it-is-kept-not-when-it-is-named.md`, and the share of it an insert's key encodings take.

use {super::*, curios::test_support::*};

/// The cross-language workload, read from the corpus so the probe measures the program the results files time.
const SPINES: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/spines/spines.crs"
));

/// Nanoseconds per `/std/Map` insert, as the slope of `spines` between two Ns so deserialization, instantiation, and the workload's fixed phases cancel.
///
/// # How to read it
///
/// `cargo xbench measure spines_insert_slope` takes it: `spines at 25000` and `spines at 75000`, each run's time, the two turned round-robin, and the slope the difference of their medians over the 50 000 inserts between them. The slope isolates the marginal insert between N=25 000 and N=75 000 — deeper-than-average descents plus the replace churn of returning keys — so it reads *steeper* than the harness's total-over-N average and is not comparable to that table. It is one fixed method compared against itself across steps, not a decomposition. The anchor `spines(8) = 28` fails the probe before any figure is read if the workload mistranslates.
///
/// # What it last printed
///
/// Taken at `5bccb9ebd`, release, x86-64 Linux, at a load average under four, seven readings in one sitting: 263, 264, 264, 265, 265, 267, 285 ns/insert, median 265. Six of the seven read within 1% of the median and one 8% over it; a sitting on another box once read bimodally, two clusters about 10% apart, so a figure is compared only within one sitting.
///
/// # What the design rests on
///
/// - **The key rides the i31.** Small-canonical `Bytes` — length in the top 2 payload bits, bytes LSB-first below — deletes the dependent loads of the key's bytes, which are the wall.
/// - **The leaf split is the emitter's.** At Binaryen optimize 3 / shrink 0 the optimized `spines` module still carries 36 of the emitter's 37 `call $bytes/read` sites, counted at `5bccb9ebd`, so no level produces the call-site leaf split (`emit_seq_get`) and the invariant is written in the emitter. A map's reads are of immediate keys, so the read protocol's *call* component is not this wall; a walk's are of one rope at every turn, which a read inside a loop keeps ([`curios-emit`'s README](../../../../curios-emit/README.md#a-read-in-a-loop-keeps-the-run-it-last-resolved)).
/// - **The trie forks sixteen ways.** A branch reads its twig by the key's digit and the descent writes each level's twig back in place where nothing else holds the list, so a level costs a read and a write of one slot and a key's five levels replace seventeen ([`Map` is a trie forking sixteen ways](../../../../curios-prelude-build/README.md#map-is-a-trie-forking-sixteen-ways-and-its-shape-is-its-api)).
/// - **The map walks once.** `Node/insert` and `Node/remove` descend once and decide on the way back up.
pub fn spines_insert_slope() {
    let compiled = compiled_of(SPINES);

    // The documented cross-language anchor, so a mistranslation fails before any figure is read.
    let (_, output) = run_at(&compiled, 8);
    assert_eq!(output, b"28\n", "spines(8) anchor");

    turn_runs(
        7,
        &[("spines", &compiled, 25_000), ("spines", &compiled, 75_000)],
    );
}

/// The per-iteration loop of `spines` with the map removed: the same LCG, the same key derivation, folding a checksum instead of inserting. The pair differs only in whether the two encodings an insert builds are built — its key's, and that of the leaf it is compared with, here the key the step before derived — so the slope delta is the key construction's own price per insert.
const KEY_ENCODED: &str = r#"
use /std/{Str, Nat, Bytes, Hash, Option, Io};

let walk(n: Nat, x: Nat, s: Nat) -> Nat =
    match n
    | 0 => s
    | k + 1 =>
        let y = 75 * x % 65537;
        walk(k, y, (s + y + Bytes/length(Hash/hash(y)) + Bytes/length(Hash/hash(x))) % 1000003)
    end;

let entry() -> /std/Io({}) =
    let arguments = /std/Proc/args()!;
    let input = /std/List/try_get(arguments, 1);
    match input
    | some(bytes) =>
        match Str/of_bytes(bytes)
        | success(str) =>
            match Nat/of_str(Str/trim(str))
            | some(n) => /std/print(Str/concat(Nat/to_str(walk(n, (n + 1) % 65537, 0)), "\n"))
            | none() => /std/print("bad input\n")
            end
        | failure(_) => /std/print("invalid utf-8\n")
        end
    | none() => /std/print("no input\n")
    end;
entry()
"#;

const KEY_CONTROL: &str = r#"
use /std/{Str, Nat, Bytes, Option, Io};

let walk(n: Nat, x: Nat, s: Nat) -> Nat =
    match n
    | 0 => s
    | k + 1 =>
        let y = 75 * x % 65537;
        walk(k, y, (s + y + 4) % 1000003)
    end;

let entry() -> /std/Io({}) =
    let arguments = /std/Proc/args()!;
    let input = /std/List/try_get(arguments, 1);
    match input
    | some(bytes) =>
        match Str/of_bytes(bytes)
        | success(str) =>
            match Nat/of_str(Str/trim(str))
            | some(n) => /std/print(Str/concat(Nat/to_str(walk(n, (n + 1) % 65537, 0)), "\n"))
            | none() => /std/print("bad input\n")
            end
        | failure(_) => /std/print("invalid utf-8\n")
        end
    | none() => /std/print("no input\n")
    end;
entry()
"#;

/// The key's share of the insert: what the two encodings an insert builds cost at `spines`' key magnitudes, isolated from the map. Sound as a pair of scalar loops because the collector's share of this workload measured nil (`spines_collection_decomposition`) — the live-set confound that would invalidate a whole-process ablation has no collections left to misattribute.
///
/// # How to read it
///
/// `cargo xbench measure map_insert_key_share` takes it: `encoded` and `control`, each at 25 000 and 75 000 iterations, the four runs turned round-robin, each slope the difference of its two medians over the 50 000 iterations between them. The control folds a constant where the probe folds the lengths of `Hash/hash` of this step's key and of the last's, so the delta is two encodings and two immediate `len`s — read it as a slight upper bound on the key class. An insert into a `Map(Nat, V)` builds those two, the map holding its keys and deriving their encodings where it compares them, so the share arithmetic is direct against `spines_insert_slope`'s figure read in the same sitting.
///
/// # What it last printed
///
/// Taken at `5bccb9ebd`, release, x86-64 Linux, seven readings in the sitting `spines_insert_slope`'s were taken in, alternating with its, the first of which printed:
///
/// ```text
/// outputs at N=1000: encoded "927554", control "925689"
///   encoded 44 ns/iter, control 4 ns/iter, key construction 40 ns/insert
/// ```
///
/// Over the seven: encoded 40, 40, 40, 42, 42, 44, 44, median 42; control 2, 3, 3, 3, 4, 4, 5, median 3; key construction 36, 36, 36, 39, 40, 40, 41, median 39 ns/insert. The two encodings an insert builds are 39 of its 265 nanoseconds, under 15%.
///
/// Beside the key class, the collector's share measured nil (`spines_collection_decomposition`). Of the emitted `spines` module's 25 `br_table`s, as `wonder --stage wasm` prints it at `06ea35a7d`, fourteen sit in the program's entry, the `io/bind` its description runs, three in `/std/Map/internal/Node/insert` itself and one in its heap version, two in the heap version's resumption, one in each of `/std/Map/internal/Node/fold`'s two copies and one in its heap version, one in `/std/answered` and one in a handle's retry, none in a digit read, so the descent still holds tables. What else remains of the insert is the uniform-representation tax, which `documentation/design/compilation/a-field-is-declared-at-the-carrier-its-shape-names.md` takes up.
pub fn map_insert_key_share() {
    let encoded = compiled_of(KEY_ENCODED);
    let control = compiled_of(KEY_CONTROL);

    // Each arm pins its own output, since the two legitimately differ: the lengths against the constant.
    assert_eq!(run_at(&encoded, 1000).1, b"927554\n", "encoded at N = 1000");
    assert_eq!(run_at(&control, 1000).1, b"925689\n", "control at N = 1000");

    turn_runs(
        7,
        &[
            ("encoded", &encoded, 25_000),
            ("encoded", &encoded, 75_000),
            ("control", &control, 25_000),
            ("control", &control, 75_000),
        ],
    );
}
