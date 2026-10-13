//! The program the three carriers' read tests share: a run built in each shape a node's children take, every element of it read from the first to the last and back, against the same run settled into one flat payload.

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

pub(super) const BITS: Carrier = Carrier {
    run: "Bits",
    open: "b[",
    len: "Bits/len",
    slice: "Bits/slice",
    at: "match Bits/try_get(run, i) | some(bit) => match bit | true => 2 | false => 1 end | none() => 0 end",
    leaf: "Bytes/to_bits(l)",
    settled: "Bits/xor(run, Bits/replicate(Bits/len(run), false))",
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
    use /std/{Nat, Byte, Bytes, Bits, Bool, List, Option, Str, Io, print};

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

    let arrived = Io/read(Io/stdin, 64)!;
    match arrived
    | chunk(l) =>
        let leaf: %RUN% = %LEAF%;
        let forced = %OPEN%..leaf, ..leaf];
        let ready = %LEN%(settled(forced)) == %LEN%(forced);
        let view = window(forced);
        print(Str/concat(mark(ready), report(7, leaf, forced, view)))
    | _ => print("no input")
    end
    "#
    .replace("%RUN%", carrier.run)
    .replace("%OPEN%", carrier.open)
    .replace("%LEN%", carrier.len)
    .replace("%SLICE%", carrier.slice)
    .replace("%AT%", carrier.at)
    .replace("%LEAF%", carrier.leaf)
    .replace("%SETTLED%", carrier.settled)
}
