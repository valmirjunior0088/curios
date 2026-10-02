use xboard::Part;

/// Are these two terms equal? Definitional equality and everything it believes about computation: the intrinsics' folds and algebra, the bounds oracle, the binary64 model, the closed machine, the evaluation memo.
pub(super) const CONVERSION: Part = Part {
    name: "conversion",
    tickets: &[],
};
