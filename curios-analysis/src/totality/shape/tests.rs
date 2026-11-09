//! The size order over shapes: what a tag identifies, and what carries a decrease.

use {
    super::*,
    curios_core::{Atom, Size},
};

/// A tag two families share is one leaf, so what grades a call is the shape and not the family it was declared in.
///
/// `Tag::Variant` holds a tag and no family, and a nullary constructor's node has no children, so a closed leaf names no binder the walk minted — the case the tag's own reason does not cover. What makes the grading sound is the order: a shape *is* the measure, so two values of one shape have one measure and a strict containment is a strict decrease whichever family each tag belongs to. The last assertion is the point stated directly — a wrap built from the same atom is the same shape, so there is no "another family's wrap" to tell apart at this level.
#[test]
fn a_constructor_tag_two_families_share_is_graded_by_the_shape_and_not_by_the_family() {
    let tip = Shape::node(Tag::Variant(Atom::from("tip")), []);
    let wrapped = Shape::node(Tag::Variant(Atom::from("wrap")), [tip.clone()]);

    assert_eq!(tip.against(&wrapped), Size::Less);
    assert_eq!(tip.against(&tip), Size::Same);
    assert_eq!(wrapped.against(&tip), Size::Unknown);

    assert_eq!(
        wrapped,
        Shape::node(Tag::Variant(Atom::from("wrap")), [tip]),
        "a shape holds no family, so one tag spells one leaf",
    );
}
