use std::time::Duration;

#[test]
fn end_to_end() {
    let source = r#"
        let pair_ty : Type = {
            label : '[left, right],
            match label : _ => Type
            | 'left => Int
            | 'right => Flt
            end };
        let pair : pair_ty = ('left, +42);
        let score : (_ : pair_ty) -> Int = p =>
            match p.0 : _ => Int
            | 'left => +42
            | 'right => +7
            end;
        Sys.print(Int.to_str(score(pair)))
        "#;

    let (system, receiver) = crate::ChannelProvider::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"42".to_vec()]
    );
}

#[test]
fn sys_print() {
    let (system, receiver) = crate::ChannelProvider::out();
    crate::run_text(Duration::from_secs(5), r#"Sys.print("hello")"#, system)
        .expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"hello".to_vec()]
    );
}

#[test]
fn sys_read() {
    let (system, receiver) = crate::ChannelProvider::io(vec![b"hello\n".to_vec()]);
    crate::run_text(Duration::from_secs(5), r#"Sys.print(Sys.read)"#, system)
        .expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"hello\n".to_vec()]
    );
}

#[test]
fn triangular_sum() {
    let source = r#"
        let result : Nat =
            match 5 : _ => Nat
            | 0 => 0
            | pred ih => Nat.add(ih, pred)
            end;
        Sys.print(Nat.to_str(result))
        "#;

    let (system, receiver) = crate::ChannelProvider::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"10".to_vec()]
    );
}

#[test]
fn multi_arg_function() {
    let source = r#"
        let add : (Int, Int) -> Int = (x, y) => Int.add(x, y);
        Sys.print(Int.to_str(add(+3, +4)))
        "#;

    let (system, receiver) = crate::ChannelProvider::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(receiver.try_iter().collect::<Vec<_>>(), vec![b"7".to_vec()]);
}

#[test]
fn curried_function() {
    let source = r#"
        let add : Int -> Int -> Int = x => y => Int.add(x, y);
        Sys.print(Int.to_str(add(+3)(+4)))
        "#;

    let (system, receiver) = crate::ChannelProvider::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(receiver.try_iter().collect::<Vec<_>>(), vec![b"7".to_vec()]);
}

#[test]
fn vec_cons_with_nat_succ() {
    let source = r#"
        rec Vec(T : Type, n : Nat) -> Type =
            match n : Type
            | 0 => '[nil]
            | pred ih => { T, ih }
            end;

        let cons(T : Type, n : Nat, x : T, xs : Vec(T, n)) -> Vec(T, Nat.succ(n)) =
            (x, xs);

        let head(T : Type, n : Nat, xs : Vec(T, Nat.succ(n))) -> T =
            xs.0;

        let v : Vec(Nat, 1) = cons(Nat, 0, 42, 'nil);
        Sys.print(Nat.to_str(head(Nat, 0, v)))
    "#;

    let (system, receiver) = crate::ChannelProvider::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"42".to_vec()]
    );
}
