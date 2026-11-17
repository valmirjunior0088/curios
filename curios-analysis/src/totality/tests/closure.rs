//! The closure and the gate over a call graded over other than its caller part's parameters, which no path through a group can be read off: each claims nothing.

use super::*;

/// A self-call whose matrix is not square was graded short, and a pair of calls that do not compose was too, so neither closes to a cycle that passes: `decide` reads each as `Partial`, where reading it as no cycle would accept a recursion the walk found and could not grade.
#[test]
fn a_call_graded_short_claims_nothing() {
    let short = call(0, 0, matrix(&[&[Size::Same, Size::Same]]));
    assert_eq!(decide(vec![short]), Totality::Partial);

    let into = call(0, 1, matrix(&[&[Size::Less]]));
    let back = call(1, 0, matrix(&[&[Size::Less], &[Size::Less]]));
    assert_eq!(decide(vec![into, back]), Totality::Partial);
}
