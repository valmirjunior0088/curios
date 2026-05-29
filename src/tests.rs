use {super::ChannelHost, std::time::Duration};

#[test]
fn end_to_end() {
    let source = r#"
        let pair_ty : Type = {
            label : '[left, right],
            match label : _ => Type
            | 'left => sys/Int
            | 'right => sys/Flt
            end };
        let pair : pair_ty = ('left, +42);
        let score : (_ : pair_ty) -> sys/Int = p =>
            match p.0 : _ => sys/Int
            | 'left => +42
            | 'right => +7
            end;
        sys/Io/print(sys/Int/to_str(score(pair)))
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"42".to_vec()]
    );
}

#[test]
fn io_print() {
    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), r#"sys/Io/print("hello")"#, system)
        .expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"hello".to_vec()]
    );
}

#[test]
fn io_read() {
    let (system, receiver) = ChannelHost::in_out(["hello"]);
    crate::run_text(
        Duration::from_secs(5),
        r#"sys/Io/print(sys/Io/read())"#,
        system,
    )
    .expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"hello\n".to_vec()]
    );
}

#[test]
fn triangular_sum() {
    let source = r#"
        let result : sys/Nat =
            match 5 : _ => sys/Nat
            | 0 => 0
            | pred ih => sys/Nat/add(ih, pred)
            end;
        sys/Io/print(sys/Nat/to_str(result))
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"10".to_vec()]
    );
}

#[test]
fn multi_arg_function() {
    let source = r#"
        let add : (sys/Int, sys/Int) -> sys/Int = (x, y) => sys/Int/add(x, y);
        sys/Io/print(sys/Int/to_str(add(+3, +4)))
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(receiver.try_iter().collect::<Vec<_>>(), vec![b"7".to_vec()]);
}

#[test]
fn curried_function() {
    let source = r#"
        let add : sys/Int -> sys/Int -> sys/Int = x => y => sys/Int/add(x, y);
        sys/Io/print(sys/Int/to_str(add(+3)(+4)))
        "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(receiver.try_iter().collect::<Vec<_>>(), vec![b"7".to_vec()]);
}

#[test]
fn vec_cons_with_nat_succ() {
    let source = r#"
        rec Vec(T : Type, n : sys/Nat) -> Type =
            match n : Type
            | 0 => '[nil]
            | pred ih => { T, ih }
            end;

        let cons(T : Type, n : sys/Nat, x : T, xs : Vec(T, n)) -> Vec(T, n + 1) =
            (x, xs);

        let head(T : Type, n : sys/Nat, xs : Vec(T, n + 1)) -> T =
            xs.0;

        let v : Vec(sys/Nat, 1) = cons(sys/Nat, 0, 42, 'nil);
        sys/Io/print(sys/Nat/to_str(head(sys/Nat, 0, v)))
    "#;

    let (system, receiver) = ChannelHost::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"42".to_vec()]
    );
}
