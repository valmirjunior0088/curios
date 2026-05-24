use std::time::Duration;

#[test]
fn triangular_sum() {
    let source = r#"
        let result : Nat =
            Nat.fold 5 : _ => Nat;
            | 0 => 0;
            | pred ih => Nat.add ih pred;;
        Sys.print (Nat.to_str result)
        "#;

    let (system, receiver) = crate::ChannelProvider::out();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"10".to_vec()]
    );
}
