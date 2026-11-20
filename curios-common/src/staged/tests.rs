use {
    super::*,
    std::{os::unix::fs::PermissionsExt, process::Command},
};

/// An executable written again while it runs takes its new bytes, where a write in place is refused as a busy text file, and leaves no staging file behind. Mutation-checked: written in place, the second write fails.
#[test]
fn an_executable_running_is_written_again_whole() {
    let directory = std::env::temp_dir().join(format!("curios-staged-{}", std::process::id()));
    fs::create_dir_all(&directory).expect("a scratch directory");
    let program = directory.join("sleep");
    let sleep = fs::read("/bin/sleep").expect("the system's sleep");
    write_whole_prepared(&program, &sleep, |staged| {
        let mut permissions = fs::metadata(staged)?.permissions();
        permissions.set_mode(0o755);
        fs::set_permissions(staged, permissions)
    })
    .expect("the first write");
    // A spawn returns once the child has begun executing, so the file is a running executable from here on.
    let mut running = Command::new(&program)
        .arg("5")
        .spawn()
        .expect("the program runs");

    let written = write_whole(&program, b"replaced");
    let _ = running.kill();
    let _ = running.wait();

    assert!(written.is_ok(), "{written:?}");
    assert_eq!(fs::read(&program).expect("the file"), b"replaced");
    assert_eq!(
        fs::read_dir(&directory).expect("the directory").count(),
        1,
        "no staging file is left"
    );
    let _ = fs::remove_dir_all(&directory);
}
