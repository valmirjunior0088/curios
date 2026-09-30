//! Interned spellings: one allocation per spelling, compared by address and ordered by content.

use {super::Symbol, std::thread};

#[test]
fn equal_spellings_are_one_symbol() {
    assert_eq!(Symbol::new("hint"), Symbol::new("hint"));
    assert_ne!(Symbol::new("hint"), Symbol::new("other"));
}

/// Ordered by spelling rather than by address, so the order is the same in every process whatever order the spellings were interned in.
#[test]
fn symbols_order_by_spelling_whatever_order_they_were_interned_in() {
    let later = Symbol::new("zeta_interned_first");
    let earlier = Symbol::new("alpha_interned_second");

    assert!(earlier < later);
}

#[test]
fn a_symbol_interned_on_another_thread_is_the_same_symbol() {
    let there = thread::spawn(|| Symbol::new("crossing"))
        .join()
        .expect("the thread interns");

    assert_eq!(there, Symbol::new("crossing"));
}
