use std::{path::Path, time::Duration};

#[test]
fn anonymous_module_loads_from_file() {
    let path = Path::new(file!()).parent().unwrap().join("fixtures/main.crs");
    let (on_print, receiver) = crate::pipe_to_channel();
    crate::run_file(Duration::from_secs(5), &path, on_print).expect("expected result");
    assert_eq!(receiver.try_iter().collect::<Vec<_>>(), vec![b"42".to_vec()]);
}
