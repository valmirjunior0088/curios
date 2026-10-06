//! Constructor specialization preserves nominal row identity across runtime-dependent types.

use {crate::tests::compile, curios_runtime::MockHost};

/// Both families have the same constructor tags and padded width. The dependent consumer reads the corresponding nominal row under each `Bool` arm; sharing a clone between those rows would rebuild one family where the other is read.
#[test]
fn distinct_families_with_matching_tags_and_widths_compile_and_run() {
    let compiled = compile(
        r#"
        use /std/{Bool, Nat, List, proc};
        induct Left: Type | wrap(Nat) | other(Nat) end
        induct Right: Type | wrap(Nat) | other(Nat) end
        let Shape(b: Bool) -> Type = match b | true => Left | false => Right end;
        let read(b: Bool, x: Shape(b)) -> Nat =
            match b
            | true => match x | wrap(n) => n | other(n) => n + 1 end
            | false => match x | wrap(n) => n | other(n) => n + 1 end
            end;
        let n = List/len(proc/args!);
        let a = read(true, Left/wrap(n));
        let b = read(false, Right/wrap(n + 1));
        /std/print(Nat/to_str(a + b))
        "#,
    )
    .expect("distinct families keep their constructor vocabulary through specialization");
    for (args, expected) in [
        (&[][..], b"1".as_slice()),
        (&["program"][..], b"3".as_slice()),
        (&["program", "argument"][..], b"5".as_slice()),
    ] {
        let (system, io) = MockHost::builder().args(args.iter().copied()).build();
        assert_eq!(compiled.run(system), Ok(0));
        assert_eq!(io.output(), expected);
    }
}
