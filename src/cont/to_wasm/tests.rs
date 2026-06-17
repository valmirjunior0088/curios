use super::*;

pub fn printed(module: &cont::Module) -> String {
    let (system, io) = crate::MockHost::builder().build();
    crate::run_wasm(&to_wasm(module), system).expect("run failed");
    String::from_utf8(io.output()).unwrap()
}

pub fn i32_result(module: &cont::Module) -> i32 {
    printed(module).parse().unwrap()
}

pub fn f32_result(module: &cont::Module) -> f32 {
    printed(module).parse().unwrap()
}

pub fn traps(module: &cont::Module) -> bool {
    let (system, _io) = crate::MockHost::builder().build();

    crate::run_wasm(&to_wasm(module), system).is_err()
}
