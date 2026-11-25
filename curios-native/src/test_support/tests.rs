use super::*;

/// The engine's sentence for a growth names the bytes it grew by in hex, before the size it grew to; a sentence of another kind names none.
#[test]
fn a_growth_is_read_as_the_bytes_the_engine_says_it_grew_by() {
    assert_eq!(
        grown_by("  -> grew GC heap by 0x8000000 bytes: new size is 0x10000000 bytes"),
        Some(0x800_0000)
    );
    assert_eq!(grown_by("Begin GC"), None);
}
