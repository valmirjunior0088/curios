//! The standard library's [`Documentation`] record. [`std_documentation`] is the record of the standard library this compiler was built with, which is how `curios document --std` documents it: it has no package a build would compile it from, and the build that lowered the prelude filed its records beside it. A package's record is made by `curios document`, a build, from its own compilation, in `curios`'s pipeline, which writes the pages.

use {
    curios_common::Qualifier,
    curios_document::Documentation,
    curios_package::Refused,
    curios_pipeline::{CompileError, prelude_documentation},
};

/// The standard library's record, read off the image this compiler was built with — no sources, no store, nothing compiled and no unit restored.
///
/// **The record of `/std`, named by its prefix rather than taken as the first one found.** Both prelude roots have one — `/sys` documents itself so that `/std` has something to adopt its intrinsic declarations out of — and the fold puts `/sys` first, so a search for "the" record finds the wrong half.
pub fn std_documentation() -> Result<Documentation, CompileError> {
    let std = Qualifier::from(["std"]);

    prelude_documentation()
        .into_iter()
        .find(|record| record.prefix == std)
        .ok_or_else(|| {
            Refused::of("compiler fault")
                .stating("the prelude carries no `/std` record")
                .about_named("/std")
                .into()
        })
}
