//! The programs the three carriers' read tests share: a run built in each shape a node's children take, every element of it read from the first to the last and back, against the same run settled into one flat payload; and runs flattened over one another, extending a payload in place or copying it, every element read against the leaf they repeat.

use curios_utilities::fill;

/// The chunk of standard input the program reads its leaf from: forty bytes, past an immediate's envelope and long enough for a window of twenty.
pub(super) const ARRIVAL: &str = "0123456789abcdefghijklmnopqrstuvwxyzABCD";

/// How one carrier spells the program's holes: its type, the bracket that opens a concatenation, its length and window, one element read as a number, the leaf made of the bytes that arrived, and the run settled into a fresh flat payload.
pub(super) struct Carrier {
    pub(super) run: &'static str,
    pub(super) open: &'static str,
    pub(super) len: &'static str,
    pub(super) slice: &'static str,
    pub(super) at: &'static str,
    pub(super) leaf: &'static str,
    pub(super) settled: &'static str,
}

pub(super) const BYTES: Carrier = Carrier {
    run: "Bytes",
    open: "x[",
    len: "Bytes/len",
    slice: "Bytes/slice",
    at: "match Bytes/try_get(run, i) | some(byte) => Byte/to_nat(byte) + 1 | none() => 0 end",
    leaf: "l",
    settled: "Bytes/xor(run, Bytes/replicate(Bytes/len(run), 0x00))",
};

pub(super) const LIST: Carrier = Carrier {
    run: "List(Nat)",
    open: "[",
    len: "List/len",
    slice: "List/slice",
    at: "match List/try_get(run, i) | some(value) => value + 1 | none() => 0 end",
    leaf: "List/map(List/range(0, Bytes/len(l)), (i: Nat) => i * 7 % 13)",
    settled: "List/map(run, (value: Nat) => value)",
};

/// A program over one chunk of standard input that prints a `1` for each of nine claims and a `0` for one that fails: that a node flattens to its own length, and that each of eight runs reads at every index, upward and then downward, as the same run settled into a fresh flat payload does. The eight are a leaf, two leaves, a window and a leaf either way round, a flattened node and a leaf, an unread node on either side of a leaf, and three appends.
///
/// Each read is of a run built for it, so the upward walk meets every node unread and the downward one does too: a read that flattened the run for the one would otherwise hide what the other answers.
pub(super) fn reads_in_every_shape(carrier: &Carrier) -> String {
    r#"
    use /std/{Nat, Byte, Bytes, Bool, List, Option, Str, Io, print, Try, Process};

    let at(run: %RUN%, i: Nat) -> Nat =
        %AT%;

    let settled(run: %RUN%) -> %RUN% =
        %SETTLED%;

    let alike_up(a: %RUN%, b: %RUN%, i: Nat) -> Bool =
        match Nat/lt/try(i, %LEN%(b))
        | none() => true
        | some(_) => at(a, i) == at(b, i) && alike_up(a, b, i + 1)
        end;

    let alike_down(a: %RUN%, b: %RUN%, left: Nat) -> Bool =
        match left | 0 => true | k + 1 => at(a, k) == at(b, k) && alike_down(a, b, k) end;

    let window(run: %RUN%) -> %RUN% =
        match Nat/le/try(25, %LEN%(run)) | some(fits) => %SLICE%(run, 5, 20) | none() => run end;

    let shape(which: Nat, leaf: %RUN%, forced: %RUN%, view: %RUN%) -> %RUN% =
        match which
        | 0 => leaf
        | 1 => %OPEN%..leaf, ..leaf]
        | 2 => %OPEN%..view, ..leaf]
        | 3 => %OPEN%..leaf, ..view]
        | 4 => %OPEN%..forced, ..leaf]
        | 5 => %OPEN%..%OPEN%..leaf, ..leaf], ..leaf]
        | 6 => %OPEN%..leaf, ..%OPEN%..leaf, ..leaf]]
        | _ => %OPEN%..%OPEN%..%OPEN%..leaf, ..view], ..leaf], ..forced]
        end;

    let holds(which: Nat, leaf: %RUN%, forced: %RUN%, view: %RUN%) -> Bool =
        let flat = settled(shape(which, leaf, forced, view));
        %LEN%(flat) == %LEN%(shape(which, leaf, forced, view))
            && alike_up(shape(which, leaf, forced, view), flat, 0)
            && alike_down(shape(which, leaf, forced, view), flat, %LEN%(flat));

    let mark(held: Bool) -> Str =
        match held | true => "1" | false => "0" end;

    let report(which: Nat, leaf: %RUN%, forced: %RUN%, view: %RUN%) -> Str =
        match which
        | 0 => mark(holds(0, leaf, forced, view))
        | k + 1; rest => Str/concat(rest, mark(holds(k + 1, leaf, forced, view)))
        end;

    let main() -> Io({}) =
    let arrived = Try/run(Io/read(Process/stdin, 64))!;
    match arrived
    | success(bytes(l)) =>
        let leaf: %RUN% = %LEAF%;
        let forced = %OPEN%..leaf, ..leaf];
        let ready = %LEN%(settled(forced)) == %LEN%(forced);
        let view = window(forced);
        print(Str/concat(mark(ready), report(7, leaf, forced, view)))
    | _ => print("no input")
    end;
    main()
    "#
    .replace("%RUN%", carrier.run)
    .replace("%OPEN%", carrier.open)
    .replace("%LEN%", carrier.len)
    .replace("%SLICE%", carrier.slice)
    .replace("%AT%", carrier.at)
    .replace("%LEAF%", carrier.leaf)
    .replace("%SETTLED%", carrier.settled)
}

/// A program over one chunk of standard input that prints a `1` for each of six claims and a `0` for one that fails, over runs that each repeat the leaf the arrival makes, flattened in an order that puts every arm of a flattening to work. `two` is flattened over its leaf, so exactly, and `three` over `two`, so with room, its extent its own length. `left` then claims the room `three` left and `right` finds the extent moved and copies, and `doubled` claims room enough for `again` appended to itself. A window of `three`, cut before anything extends its payload, is read after. Every run is read element by element against the leaf it repeats, and each flattening's settled copy as well, so a read of a run and the payload it was written into answer alike.
///
/// The bindings stand in the order they are flattened in, each settled copy read in the claims, so no flattening is left for a read to do later.
pub(super) fn extends_in_place(carrier: &Carrier) -> String {
    let (run, open, len, slice, at, leaf, settled) = (
        carrier.run,
        carrier.open,
        carrier.len,
        carrier.slice,
        carrier.at,
        carrier.leaf,
        carrier.settled,
    );
    fill!(
        r#"
    use /std/{Nat, Byte, Bytes, Bool, List, Option, Str, Io, print, Try, Process};

    let at(run: ${run}, i: Nat) -> Nat =
        ${at};

    let settled(run: ${run}) -> ${run} =
        ${settled};

    let repeats_from(run: ${run}, leaf: ${run}, i: Nat) -> Bool =
        match (Nat/lt/try(i, ${len}(run)), Nat/lt/try(0, ${len}(leaf)))
        | (some(_), some(_)) =>
            at(run, i) == at(leaf, i % ${len}(leaf)) && repeats_from(run, leaf, i + 1)
        | _ => true
        end;

    let repeats(run: ${run}, leaf: ${run}, times: Nat) -> Bool =
        ${len}(run) == times * ${len}(leaf) && repeats_from(run, leaf, 0);

    let window(run: ${run}, leaf: ${run}) -> ${run} =
        match Nat/le/try(${len}(leaf), ${len}(run))
        | some(fits) => ${slice}(run, 0, ${len}(leaf))
        | none() => run
        end;

    let mark(held: Bool) -> Str =
        match held | true => "1" | false => "0" end;

    let main() -> Io({}) =
    let arrived = Try/run(Io/read(Process/stdin, 64))!;
    match arrived
    | success(bytes(l)) =>
        let leaf: ${run} = ${leaf};
        let two = ${open}..leaf, ..leaf];
        let flat_two = settled(two);
        let three = ${open}..two, ..leaf];
        let flat_three = settled(three);
        let early = window(three, leaf);
        let left = ${open}..three, ..leaf];
        let flat_left = settled(left);
        let right = ${open}..three, ..two];
        let flat_right = settled(right);
        let again = ${open}..two, ..two];
        let flat_again = settled(again);
        let doubled = ${open}..again, ..again];
        let flat_doubled = settled(doubled);
        print(
            Str/flatten([
                mark(repeats(flat_two, leaf, 2) && repeats(flat_three, leaf, 3)),
                mark(repeats(flat_left, leaf, 4) && repeats(flat_right, leaf, 5)),
                mark(repeats(flat_again, leaf, 4) && repeats(flat_doubled, leaf, 8)),
                mark(repeats(three, leaf, 3) && repeats(early, leaf, 1)),
                mark(repeats(left, leaf, 4) && repeats(right, leaf, 5)),
                mark(repeats(doubled, leaf, 8) && repeats(two, leaf, 2))]))
    | _ => print("no input")
    end;
    main()
    "#,
        run,
        open,
        len,
        slice,
        at,
        leaf,
        settled
    )
}
