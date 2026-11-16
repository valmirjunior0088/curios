use super::*;

#[test]
fn every_word_reads_back_from_its_spelling() {
    for keyword in Keyword::ALL {
        assert_eq!(Keyword::read(keyword.spelling()), Some(keyword));
    }
    assert_eq!(Keyword::read("lets"), None);
}

#[test]
fn only_a_reserved_word_is_refused_as_a_name() {
    assert!(is_reserved("match"));
    assert!(is_reserved("true"));
    assert!(!is_reserved("concept"));
    assert!(!is_reserved("and"));
    assert!(!is_reserved("Type"));
    assert!(!is_reserved("matches"));
}
