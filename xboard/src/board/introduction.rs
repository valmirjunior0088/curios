use xboard::Part;

/// Is this value built from checked parts, each put into the binder meant for it? A lambda's binder, an argument, a constructor's payload, a record's field. The identity of a binder is filed here too, though every rule opens one: building is where a binder is first made.
pub(super) const INTRODUCTION: Part = Part {
    name: "introduction",
    tickets: &[],
};
