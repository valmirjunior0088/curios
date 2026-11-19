//! The members of a fact: what a gain answers and what it leaves held, few or many, whatever the widths and the interleaving of the two sets.

use {super::*, std::collections::BTreeSet};

/// Every `stride`th number from `start`, `count` of them.
fn spaced(start: u32, stride: u32, count: u32) -> Vec<u32> {
    (0..count).map(|index| start + index * stride).collect()
}

/// A gain answers the members the union adds, ascending, and leaves the union held, a bit a member exactly where it is wider than [`WIDE`]: over held and incoming sets each empty, few or many, interleaved, nested, disjoint above or below, against a set that sorts them.
#[test]
fn a_gain_answers_what_the_union_adds_and_holds_the_union() {
    let wide = u32::try_from(WIDE).expect("the width is a number");
    let widths = [0, 1, 7, wide - 1, wide, wide + 1, 300];
    let shapes = |width: u32| {
        [
            spaced(0, 1, width),
            spaced(0, 3, width),
            spaced(1, 5, width),
            spaced(1_000, 2, width),
            spaced(0, 97, width),
        ]
    };
    for held_width in widths {
        for incoming_width in widths {
            for held in shapes(held_width) {
                for incoming in shapes(incoming_width) {
                    let union = held
                        .iter()
                        .chain(&incoming)
                        .copied()
                        .collect::<BTreeSet<_>>();
                    let added = incoming
                        .iter()
                        .copied()
                        .filter(|number| !held.contains(number))
                        .collect::<Vec<_>>();
                    let mut members = Members::of_ascending(held.clone());

                    let gained = members.gain(&Members::of_ascending(incoming.clone()));

                    let case = format!("held {held:?}, incoming {incoming:?}");
                    assert_eq!(gained.iter().collect::<Vec<_>>(), added, "{case}");
                    assert_eq!(
                        matches!(gained, Members::Many(_)),
                        added.len() > WIDE,
                        "{case}"
                    );
                    assert_eq!(
                        members.iter().collect::<Vec<_>>(),
                        union.iter().copied().collect::<Vec<_>>(),
                        "{case}"
                    );
                    assert_eq!(
                        matches!(members, Members::Many(_)),
                        union.len() > WIDE,
                        "{case}"
                    );
                }
            }
        }
    }
}
