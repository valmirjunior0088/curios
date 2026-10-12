use super::*;

#[test]
fn a_swap_of_two_neighbours_is_one_edit() {
    assert_eq!(edit_distance("coutn", "count", 1), Some(1));
    assert_eq!(edit_distance("lenght", "length", 2), Some(1));
}

#[test]
fn an_insertion_a_deletion_and_a_substitution_are_one_edit_each() {
    assert_eq!(edit_distance("totl", "total", 1), Some(1));
    assert_eq!(edit_distance("total", "totl", 1), Some(1));
    assert_eq!(edit_distance("tetal", "total", 1), Some(1));
    assert_eq!(edit_distance("total", "total", 0), Some(0));
}

// The restriction: a letter is edited once, so a swap is not followed by an insertion between the letters it swapped.
#[test]
fn no_letter_is_edited_twice() {
    assert_eq!(edit_distance("ca", "abc", 3), Some(3));
}

#[test]
fn a_distance_past_the_limit_is_none() {
    assert_eq!(edit_distance("abc", "xyz", 2), None);
    assert_eq!(edit_distance("a", "abcd", 2), None);
}

#[test]
fn the_nearest_name_is_within_a_third_of_the_written_one() {
    let scope = ["count", "total", "length"];

    assert_eq!(nearest("coutn", scope), Some("count"));
    assert_eq!(nearest("totl", scope), Some("total"));
    assert_eq!(nearest("lenght", scope), Some("length"));
    // Two edits in five letters is past a third.
    assert_eq!(nearest("cuotn", scope), None);
    assert_eq!(nearest("widget", scope), None);
}

#[test]
fn a_name_that_differs_in_case_alone_is_nearest_whatever_else_is_close() {
    assert_eq!(nearest("nat", ["Nat", "not", "net"]), Some("Nat"));
}

// A short name is offered a name one edit away and no further: three letters are the least a name counts for.
#[test]
fn a_short_name_is_offered_one_edit() {
    assert_eq!(nearest("sme", ["some", "none"]), Some("some"));
    assert_eq!(nearest("el", ["eq", "le", "add"]), Some("eq"));
    assert_eq!(nearest("x", ["y"]), Some("y"));
    assert_eq!(nearest("ab", ["cd"]), None);
}

#[test]
fn a_tie_goes_to_the_name_that_sorts_first_whatever_the_order() {
    assert_eq!(nearest("cat", ["cot", "bat"]), Some("bat"));
    assert_eq!(nearest("cat", ["bat", "cot"]), Some("bat"));
}

#[test]
fn the_written_name_itself_is_no_near_miss() {
    assert_eq!(nearest("Nat", ["Nat"]), None);
}
