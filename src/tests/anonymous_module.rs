use std::{path::Path, time::Duration};

#[test]
fn anonymous_module_loads_from_file() {
    let path = Path::new(file!())
        .parent()
        .unwrap()
        .join("fixtures/main.crs");
    let result = crate::run_file(Duration::from_secs(5), &path).expect("expected result");
    assert_eq!(result, "#0 = i31(bits=0x0000002a, value=42)");
}
