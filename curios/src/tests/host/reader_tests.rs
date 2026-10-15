//! A stream's reader: its named reads over every way an input may arrive, a read a deadline abandons, a refusal past the bound, and one reader read by two branches.

use {
    crate::tests::{compile, run_text},
    curios_runtime::{ForeignBindings, MockHost},
};

/// Two lines, the bytes before a comma, two bytes, a line, what is left, and a line of a stream that has ended, read in that order through one reader and joined by bars, and then `tail`.
fn reads(tail: &str) -> String {
    format!(
        r#"
        use /std/{{Str, Bytes, Option, Result, Show, Try, Io, print}};
        let text(b: Bytes) -> Str = Result/unwrap_or(Str/of_bytes(b), "?");
        let line(found: Option(Bytes)) -> Str =
            match found | some(b) => text(b) | none() => "<end>" end;
        let reads(input: Io/Reader(Io/Input)) -> Try(Io, Io/Error, Str) =
            let a = Io/Reader/line(input)!;
            let b = Io/Reader/line(input)!;
            let c = Io/Reader/until(input, ',')!;
            let d = Io/Reader/exactly(input, 2)!;
            let e = Io/Reader/line(input)!;
            let f = Io/Reader/rest(input)!;
            let g = Io/Reader/line(input)!;
            Try/pure(
                Str/join(
                    "|",
                    [line(a), line(b), text(c.bytes), text(d.bytes), line(e), text(f), line(g)]));
        let said(outcome: Result(Io/Error, Str)) -> Io({{}}) =
            match outcome | success(read) => print(read) | failure(why) => print(Show/show(why)) end;
        {tail}
        "#
    )
}

/// A stream of a program's own, whose chunks are what its other end is sent: a read of it waits until the program sends more, and ends once the sender is closed.
const WIRE: &str = r#"
    use /std/{Nat, Str, Bytes, Option, Result, Show, Try, Io, print};
    use /std/Io/{Channel};
    use /std/time/{Duration};
    struct Wire: Type { Channel/Receiver(Bytes) }
    let next(w: Wire) -> Io(Io/Chunk) =
        let got = Channel/recv(w.0)!;
        match got
        | none() => Io/pure(Io/Chunk/eof())
        | some(bytes) =>
            match Nat/lt/try(0, Bytes/len(bytes))
            | some(held) => Io/pure(Io/Chunk/chunk(bytes, @held))
            | none() => next(w)
            end
        end;
    satisfy Io/Read(Wire) {
        read(w, n) = next(w),
    }
    let text(b: Bytes) -> Str = Result/unwrap_or(Str/of_bytes(b), "?");
    let line(read: Result(Io/Error, Option(Bytes))) -> Str =
        match read
        | success(some(b)) => text(b)
        | success(none()) => "<end>"
        | failure(why) => Show/show(why)
        end;
"#;

/// The reads answer alike however their input is cut in two: a line ends at its newline whichever chunk holds it, a carriage return before the newline goes with it, and each read begins where the one before it stopped.
#[test]
fn the_named_reads_answer_alike_over_every_cut_of_their_input() {
    let input: &[u8] = b"ab\ncd\r\nef,ghi\nrest";
    let source = reads(
        "let input = Io/Reader/new(Io/stdin)!;
         let outcome = Try/run(reads(input))!;
         said(outcome)",
    );

    let program = compile(&source).expect("the reads compile");

    for cut in 0..=input.len() {
        let chunks = [&input[..cut], &input[cut..]]
            .into_iter()
            .filter(|chunk| !chunk.is_empty())
            .collect::<Vec<_>>();
        let (system, io) = MockHost::builder().stdin_chunks(chunks).build();

        program
            .run(system, ForeignBindings::empty())
            .expect("the reads run");
        assert_eq!(
            String::from_utf8_lossy(&io.output()),
            "ab|cd|ef|gh|i|rest|<end>",
            "cut at {cut}"
        );
    }
}

/// A read a deadline abandons has put back what it took: the reader holds the bytes that had arrived, and the read after it answers the whole line. The clock climbs a second a reading, so the two-second deadline passes while the stream is quiet.
#[test]
fn a_line_a_deadline_abandons_is_read_whole_by_the_next_read() {
    let source = format!(
        r#"{WIRE}
        let program: Io({{}}) =
            let ends = Channel/new(@Bytes, 4)!;
            let input = Io/Reader/new(Wire {{ ends.1 }})!;
            let _ = Channel/try_send(ends.0, Str/to_bytes("ab"))!;
            let hurried = Io/timeout(Duration/of_secs(2), Try/run(Io/Reader/line(input)))!;
            let _ = print(match hurried | some(read) => line(read) | none() => "<deadline>" end)!;
            let held = Io/Reader/held(input)!;
            let _ = print(Str/concat("|", text(held)))!;
            let _ = Channel/try_send(ends.0, Str/to_bytes("cd\nef"))!;
            let patient = Try/run(Io/Reader/line(input))!;
            print(Str/concat("|", line(patient)));
        program
        "#
    );
    let (system, io) = MockHost::builder()
        .mono((0..40u64).map(|second| (second, 0)))
        .build();

    run_text(&source, system).expect("the reads run");
    assert_eq!(String::from_utf8_lossy(&io.output()), "<deadline>|ab|abcd");
}

/// A line past its reader's bound is `too_long`, and the bytes stay held: a read that fits the bound takes them.
#[test]
fn a_refusal_past_the_bound_leaves_the_bytes_held() {
    let source = r#"
        use /std/{Str, Bytes, Option, Result, Show, Try, Io, print};
        let text(b: Bytes) -> Str = Result/unwrap_or(Str/of_bytes(b), "?");
        let input = Io/Reader/within(Io/stdin, 4)!;
        let refused = Try/run(Io/Reader/line(input))!;
        let _ = print(match refused | success(_) => "read" | failure(why) => Show/show(why) end)!;
        let held = Io/Reader/held(input)!;
        let _ = print(Str/concat("|", text(held)))!;
        let two = Try/run(Io/Reader/exactly(input, 2))!;
        print(
            match two
            | success(found) => Str/concat("|", text(found.bytes))
            | failure(why) => Show/show(why)
            end)
    "#;
    let (system, io) = MockHost::builder().stdin_chunks(vec!["abcdef"]).build();

    run_text(source, system).expect("the reads run");
    assert_eq!(String::from_utf8_lossy(&io.output()), "too_long|abcdef|ab");
}

/// Two branches that read one reader each get a whole line. Both have searched six bytes for a newline when one of them takes the arrival that holds it, cuts its line and leaves `b` and a newline held; the stream then ends, and the other is asked again of what is held now, from the start, where the search it had made would pass the newline by.
#[test]
fn two_branches_on_one_reader_each_get_a_whole_line() {
    let source = format!(
        r#"{WIRE}
        let reading(input: Io/Reader(Wire)) -> Io({{}}) =
            let read = Try/run(Io/Reader/line(input))!;
            print(Str/flatten(["<", line(read), ">"]));
        let both_read(s: Io/Scope) -> Io({{}}) =
            let ends = Channel/new(@Bytes, 4)!;
            let input = Io/Reader/new(Wire {{ ends.1 }})!;
            let _ = Channel/try_send(ends.0, Str/to_bytes("aaaaaa"))!;
            let first = Io/spawn(s, reading(input))!;
            let second = Io/spawn(s, reading(input))!;
            let _ = Io/yield()!;
            let _ = Channel/try_send(ends.0, Str/to_bytes("\nb\n"))!;
            let _ = Channel/close(ends.0)!;
            let _ = Io/join(first)!;
            Io/join(second);
        Io/scope(both_read)
        "#
    );
    let (system, io) = MockHost::builder().build();

    run_text(&source, system).expect("the reads run");

    let output = String::from_utf8_lossy(&io.output()).into_owned();
    let mut lines = output
        .split_inclusive('>')
        .map(str::to_owned)
        .collect::<Vec<_>>();
    lines.sort();

    assert_eq!(lines, ["<aaaaaa>", "<b>"], "{output:?}");
}

/// A line read as text is the text of its bytes, `none` once the input has ended, and a refusal where the bytes are not text, with the length of their longest prefix that is: the line is taken all the same, so the read after it is of the next.
#[test]
fn a_line_is_read_as_text_or_refused_where_it_is_none() {
    let (system, io) = MockHost::builder()
        .stdin_chunks(vec![&b"h\xc3\xa9llo\nab\xff\xfecd\nlast"[..]])
        .build();
    run_text(
        r#"
        use /std/{Str, Option, Result, Show, Try, Io, print};
        let said(read: Result(Io/Error, Option(Str))) -> Str =
            match read
            | success(some(text)) => text
            | success(none()) => "<end>"
            | failure(why) => Show/show(why)
            end;
        let input = Io/Reader/new(Io/stdin)!;
        let a = Try/run(Io/Reader/line_text(input))!;
        let b = Try/run(Io/Reader/line_text(input))!;
        let c = Try/run(Io/Reader/line_text(input))!;
        let d = Try/run(Io/Reader/line_text(input))!;
        print(Str/join("|", [said(a), said(b), said(c), said(d)]))
        "#,
        system,
    )
    .expect("the program runs");

    assert_eq!(
        String::from_utf8_lossy(&io.output()),
        "h\u{e9}llo|not_text(2)|last|<end>"
    );
}
