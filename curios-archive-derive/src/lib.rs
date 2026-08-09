//! The attribute macro behind `curios-archive`. Depend on that crate, never on this one.
//!
//! A `proc-macro = true` crate can export nothing but macros, which is the only reason this is separate — the serde/serde_derive arrangement, for the same reason serde has it.

use proc_macro::TokenStream;

/// Mark a type as archived: gate rkyv's three derives on the crate's own `archive` feature, and redirect the paths they expand to.
///
/// ```text
/// #[curios_archive::archived]
/// pub struct Qualifier { … }
/// ```
///
/// Arguments are forwarded into the same `rkyv(…)` clause as the crate redirect, so anything rkyv's derive accepts is written where it would be otherwise:
///
/// ```text
/// #[curios_archive::archived(derive(PartialEq, Eq, Hash))]
/// pub struct Atom(String);
/// ```
///
/// # Why the expansion is `cfg_attr` rather than `cfg`
///
/// The annotation is unconditional at the call site and the *expansion* is what the feature gates. `cfg_attr` is evaluated where the macro expands, so `feature = "archive"` names the consuming crate's own feature — each crate keeps its gate, and this one neither knows nor needs to know which crates have it on.
///
/// That is also why this macro is not itself feature-gated. A macro that vanished with the feature would make every annotated type a compile error in a build with archiving off.
///
/// # What it does not reach
///
/// Field attributes. `rkyv(with = …)` on a field stays written out, and if the adapter it names is one of rkyv's own it needs `curios_archive::rkyv::with::…` rather than a bare `rkyv::`. This macro decorates an item and does not read its body — which is what keeps it free of `syn`, and is a fair trade only because field adapters are rare.
#[proc_macro_attribute]
pub fn archived(args: TokenStream, item: TokenStream) -> TokenStream {
    let arguments = args.to_string();
    let arguments = arguments.trim();
    let forwarded = match arguments.is_empty() {
        true => String::new(),
        false => format!(", {arguments}"),
    };

    let attribute = format!(
        "#[cfg_attr(feature = \"archive\", \
            derive(\
                ::curios_archive::Archive, \
                ::curios_archive::Serialize, \
                ::curios_archive::Deserialize\
            ), \
            rkyv(crate = ::curios_archive::rkyv{forwarded})\
        )]"
    );

    let mut expanded: TokenStream = attribute
        .parse()
        .expect("the generated attribute is well-formed");
    expanded.extend(item);
    expanded
}
