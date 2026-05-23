use std::time::Duration;

#[test]
fn end_to_end() {
    let source = r#"
        let pair_ty : Type = {
            label : '[left, right],
            match label : _ => Type;
            | 'left => Int;
            | 'right => Flt; };
        let pair : pair_ty = ('left, +42);
        let score : (_ : pair_ty) -> Int = p =>
            match p.0 : _ => Int;
            | 'left => +42;
            | 'right => +7;;
        Sys.print (Int.to_str (score pair))
        "#;

    let (on_print, receiver) = crate::pipe_to_channel();
    crate::run_text(Duration::from_secs(5), source, on_print).expect("expected result");
    assert_eq!(receiver.try_iter().collect::<Vec<_>>(), vec![b"42".to_vec()]);
}

#[test]
fn sys_print() {
    let (on_print, receiver) = crate::pipe_to_channel();
    crate::run_text(Duration::from_secs(5), r#"Sys.print "hello""#, on_print)
        .expect("expected result");
    assert_eq!(receiver.try_iter().collect::<Vec<_>>(), vec![b"hello".to_vec()]);
}
