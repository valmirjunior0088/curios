//! Indexed vectors a program declares for itself: the type-level `Vec` over `Nat` induction, and the indexed inductive whose `append` is executed rather than only compiled. The packed splices and folds are the corpus's `/aggregates/packed`.

use {
    super::test_support::{ARRIVAL, BYTES, extends_in_place, reads_in_every_shape},
    crate::tests::{run, run_text},
    curios_native::MockHost,
};

#[test]
fn vec_cons_with_nat_succ() {
    let source = r#"
        let Vec(T : Type, n : std/Nat) -> Type =
            match n : (_) => Type
            | 0 => {}
            | pred + 1; ih => { T, ih }
            end;

        let cons(T : Type, n : std/Nat, x : T, xs : Vec(T, n)) -> Vec(T, std/Nat/succ(n)) =
            (x, xs);

        let head(T : Type, n : std/Nat, xs : Vec(T, std/Nat/succ(n))) -> T =
            xs.0;

        let v : Vec(std/Nat, 1) = cons(std/Nat, 0, 42, ());
        let entry() -> /std/Io({}) =
            let _ = /std/print(std/Nat/to_str(head(std/Nat, 0, v)))!;
            /std/Io/pure(());
        entry()
    "#;

    assert_eq!(run(source), b"42");
}

#[test]
fn indexed_vec_append_executes() {
    // A motive binding an index, *executed*: `append`'s motive binds the length index (`(v : Vec(T)(k)) => Vec(T)(Nat/add(k, m))`), the `cons` arm meets it through the definitional successor-peeling of `Nat/add`, and the implicit index arguments of the recursive call are solved to the arm's *first* binder. Running (not just compiling) guards the zonk realignment of multi-binder arm scopes: with the in-group order flipped, the solved indices would silently reference the wrong binder and the program would trap at runtime.
    let source = r#"
        use /std/{Nat, Bytes, Io};
        induct Vec(T : Type) : (n : Nat) -> Type
        | nil() : (0)
        | cons(@m : Nat, x : T, xs : Vec(T)(m)) : (Nat/succ(m))
        end
        let append(@T : Type, @n : Nat, @m : Nat, v : Vec(T)(n), w : Vec(T)(m)) -> Vec(T)(Nat/add(n, m)) =
            match v : (k, v) => Vec(T)(Nat/add(k, m))
            | nil() => w
            | cons(@j, x, xs) => Vec/cons(x, append(xs, w))
            end;
        let total(@n : Nat, v : Vec(Nat)(n), acc : Nat) -> Nat =
            match v : (_, _) => Nat
            | nil() => acc
            | cons(@m, x, xs) => total(xs, Nat/add(acc, x))
            end;
        let a : Vec(Nat)(2) = Vec/cons(1, Vec/cons(2, Vec/nil()));
        let b : Vec(Nat)(1) = Vec/cons(4, Vec/nil());
        let c : Vec(Nat)(3) = append(a, b);
        let entry() -> /std/Io({}) =
            let _ = /std/print(Nat/to_str(total(c, 0)))!;
            /std/Io/pure(());
        entry()
        "#;

    assert_eq!(run(source), b"7");
}

/// **A packed value joined from two is the run its parts spell, at every length on either side of an immediate's envelope.** A concatenation of two immediates that fit one is arithmetic on their payloads and lengths, and any other the rope's node, so every cut of every prefix of a run is put to both and to the seam between them: three bytes are where one gives way to the other. The run arrives on standard input, so nothing here folds, and a cons a byte, a join of three and a literal that leads with a constant are the same lowering.
#[test]
fn a_packed_concatenation_is_its_parts_across_the_immediate_envelope() {
    let source = r#"
        use /std/{Nat, Byte, Bytes, Bool, Str, Option, Io};

        let front(b: Bytes) -> Bytes =
            match b | x[] => x[] | x[h, .._]; rest => x[h, ..rest] end;

        let cut(b: Bytes, k: Nat) -> Bool =
            match k <= Bytes/len(b)
            | true =>
                let joined = x[..Bytes/slice(b, 0, k), ..Bytes/drop(b, k)];
                joined == b && Bytes/len(joined) == Bytes/len(b)
            | false => false
            end;

        let cuts(b: Bytes, k: Nat) -> Bool =
            match k | 0 => cut(b, 0) | j + 1; rest => cut(b, j + 1) && rest end;

        let thirds(b: Bytes, k: Nat) -> Bool =
            match 2 * k <= Bytes/len(b)
            | true =>
                x[..Bytes/slice(b, 0, k), ..Bytes/slice(b, k, k), ..Bytes/drop(b, 2 * k)] == b
            | false => true
            end;

        let prefixes(whole: Bytes, n: Nat) -> Bool =
            match n
            | 0 => true
            | j + 1; rest =>
                match j + 1 <= Bytes/len(whole)
                | true =>
                    let b = Bytes/slice(whole, 0, j + 1);
                    cuts(b, j + 1) && front(b) == b && thirds(b, 1) && thirds(b, 2) && rest
                | false => false
                end
            end;

        let led(b: Bytes) -> Bool =
            match 2 <= Bytes/len(b)
            | true =>
                let (h, k) = (Bytes/get(b, 0), Bytes/get(b, 1));
                let short = x[0x02, h, k];
                let long = x[0x02, h, k, h];
                Bytes/len(short) == 3
                    && Bytes/get(short, 0) == 0x02
                    && Bytes/get(short, 1) == h
                    && Bytes/get(short, 2) == k
                    && Bytes/len(long) == 4
                    && Bytes/get(long, 0) == 0x02
                    && Bytes/get(long, 3) == h
            | false => false
            end;

        let entry() -> /std/Io({}) =
            let standard_input = /std/Io/Reader/new(/std/Process/stdin)!;
            let input = /std/Result/unwrap_or(/std/Try/run(/std/Io/Reader/line(standard_input))!, /std/Option/none());
            match input: (_) => Io({})
            | some(bytes) =>
                /std/print(
                    Str/join(" ", [
                        Bool/to_str(prefixes(bytes, Bytes/len(bytes))),
                        Bool/to_str(led(bytes))]))
            | none() => /std/print("no input")
            end;
        entry()
        "#;

    let (system, io) = MockHost::builder()
        .stdin_lines(["\u{1}aZ~\u{7f}09 "])
        .build();
    run_text(source, system).expect("the program runs");

    assert_eq!(io.output(), b"true true");
}

/// **A packed run reads as its flat copy does, whatever its node's children are.** A read of a node that was never flattened answers from the child that holds its index where that child is flat — a leaf, a window, a flattened node — and flattens the node where it is not, so each shape takes a different arm of the read: every element of each, read upward and then downward, is the settled run's.
#[test]
fn a_packed_run_reads_as_its_flat_copy_in_every_shape_its_children_take() {
    let (system, io) = MockHost::builder().stdin_chunks(vec![ARRIVAL]).build();

    run_text(&reads_in_every_shape(&BYTES), system).expect("the reads run");
    assert_eq!(io.output(), b"111111111");
}

/// **A packed run read after another extended its payload in place reads as it was written.** A flattening over a run that ends at its payload's written extent writes into the room past it, so the older run, a window of it, a second run over it that finds the extent moved, and a run appended to itself each read element by element as the copies of the leaf they are.
#[test]
fn a_packed_run_extended_in_place_reads_as_it_was_written() {
    let (system, io) = MockHost::builder().stdin_chunks(vec![ARRIVAL]).build();

    run_text(&extends_in_place(&BYTES), system).expect("the reads run");
    assert_eq!(io.output(), b"111111");
}

/// **A run whose payload holds room crosses to a host as its run.** `left` claims the room a flattening gave `three`, so its payload is longer than it is, and the host is handed the bytes of the run and nothing past them.
#[test]
fn a_run_whose_payload_holds_room_crosses_to_a_host_as_its_run() {
    let source = r#"
        use /std/{Bytes, Io, Try, Process};

        let settled(run: Bytes) -> Bytes =
            Bytes/xor(run, Bytes/replicate(Bytes/len(run), 0x00));

        let main() -> Io({}) =
            let arrived = Try/run(Io/read(Process/stdin, 64))!;
            match arrived
            | success(bytes(leaf)) =>
                let two = x[..leaf, ..leaf];
                let flat_two = settled(two);
                let three = x[..two, ..leaf];
                let flat_three = settled(three);
                let left = x[..three, ..leaf];
                let flat_left = settled(left);
                let _ = Try/run(Io/write(Process/stdout, x[..flat_two, ..flat_three, ..flat_left]))!;
                let _ = Try/run(Io/write(Process/stdout, left))!;
                Io/pure(())
            | _ => Io/pure(())
            end;
        main()
        "#;
    let (system, io) = MockHost::builder().stdin_chunks(vec![ARRIVAL]).build();

    run_text(source, system).expect("the program runs");
    assert_eq!(io.output(), ARRIVAL.repeat(2 + 3 + 4 + 4).as_bytes());
}
