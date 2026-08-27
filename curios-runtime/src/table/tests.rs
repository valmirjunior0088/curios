use super::*;

#[test]
fn tokens_are_unique_never_reused_and_clear_of_the_stdio_band() {
    let mut table: Table<u32> = Table::new();

    let a = table.mint(10);
    let b = table.mint(20);

    // Distinct live handles get distinct tokens...
    assert_ne!(a.bytes(), b.bytes());
    // ...and minted tokens never collide with stdin/stdout/stderr.
    assert_ne!(a.bytes(), Handle::Stdin.bytes());
    assert_ne!(a.bytes(), Handle::Stdout.bytes());
    assert_ne!(a.bytes(), Handle::Stderr.bytes());

    assert_eq!(table.get(&a), Some(&10));
    assert_eq!(table.get(&b), Some(&20));
}

#[test]
fn use_after_close_is_a_loud_miss_never_an_alias() {
    let mut table: Table<u32> = Table::new();

    let a = table.mint(10);
    // Closing removes the entry and hands the resource back.
    assert_eq!(table.remove(&a), Some(10));
    // Use-after-close misses; double-close is a clean miss, not an alias.
    assert_eq!(table.get(&a), None);
    assert_eq!(table.remove(&a), None);

    // A later mint never reuses the closed token (the counter never wraps), and the stale handle keeps missing rather than aliasing the new entry.
    let b = table.mint(99);
    assert_ne!(b.bytes(), a.bytes());
    assert_eq!(table.get(&a), None);
    assert_eq!(table.get(&b), Some(&99));
}

#[test]
fn a_declined_resource_stays_filed_under_its_handle() {
    let mut table: Table<u32> = Table::new();

    let a = table.mint(10);

    // Declining hands the resource back to the table rather than to the caller: the handle still resolves, and to the same value.
    let declined = table.take_if(&a, |value| match value {
        10 => Err(10),
        other => Ok(other),
    });
    assert_eq!(declined, None);
    assert_eq!(table.get(&a), Some(&10));

    // Claiming takes it out, so the handle misses afterwards — the transition `connect`/`start_tls` perform.
    assert_eq!(
        table.take_if(&a, |value| Ok::<u32, u32>(value + 1)),
        Some(11)
    );
    assert_eq!(table.get(&a), None);

    // A handle that is not filed never reaches `select`.
    let missing = table.take_if(&a, |_: u32| -> Result<u32, u32> {
        panic!("select must not run for a handle the table does not hold")
    });
    assert_eq!(missing, None);
}

#[test]
fn stdio_handles_are_never_in_the_table() {
    let table: Table<u32> = Table::new();

    assert_eq!(table.get(&Handle::Stdin), None);
    assert_eq!(table.get(&Handle::Stdout), None);
    assert_eq!(table.get(&Handle::Stderr), None);
}
