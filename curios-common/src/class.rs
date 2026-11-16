//! The classes Curios is coloured in: what a reader tells a span of code apart as — a word, a literal, a comment, a mark, or a name by what it names — each named once, so the documentation, the editors' tables and a page's palette paint by one name.
//!
//! What classes which text is `documentation/design/tools/curios-is-coloured-from-one-class-set.md`'s: the printer what the compiler prints, a grammar what a person types, and the server's semantic tokens over the grammar in an editor. A name's class is read off what its declaration states, never off where it lives.

/// Declares [`Class`] from one table, so the roster, [`Class::ALL`] and the names cannot drift apart.
macro_rules! classes {
    ($($(#[$doc:meta])* $variant:ident => $name:literal,)+) => {
        /// What a span of code is painted as.
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum Class {
            $($(#[$doc])* $variant,)+
        }

        impl Class {
            /// Every class, in declaration order.
            pub const ALL: [Class; [$(Class::$variant),+].len()] = [$(Class::$variant),+];

            /// The name every consumer paints the class by: a page's CSS class, a key of an editor's table.
            pub fn name(self) -> &'static str {
                match self {
                    $(Class::$variant => $name,)+
                }
            }
        }
    };
}

classes! {
    /// A word of the language that is neither a boolean nor a sort, and the implicit mark `@`, as the witness mark `use` is one.
    Keyword => "keyword",
    /// `true` or `false`.
    Boolean => "boolean",
    /// `Type`, `Prop` or `Sort` where a term stands.
    Sort => "sort",
    Number => "number",
    String => "string",
    Character => "character",
    /// An escape inside a string or a character.
    Escape => "escape",
    Comment => "comment",
    /// A comment that opens with `---`, which documents what follows it.
    DocumentationComment => "documentation-comment",
    Operator => "operator",
    /// A bracket, a separator, a path's `/`, a projection's `.`, and `_` alone.
    Punctuation => "punctuation",
    /// `?`, which the compiler answers with what it knows of the place.
    Goal => "goal",
    /// A decorator of Core text, `#{…}`, and the fields it states.
    Decorator => "decorator",
    /// A module: a segment of a path, or the module a `use` line names.
    Module => "module",
    /// An inductive, a structure, a type of `/sys`, or a wire word of a `foreign` signature.
    Type => "type",
    /// An inductive's constructor.
    Constructor => "constructor",
    Concept => "concept",
    /// A concept's method.
    Method => "method",
    /// A declaration whose stated type is a function's — a `foreign`, an operation of `/sys` — and a test.
    Function => "function",
    /// Any other declaration, a witness's name in Core text among them.
    Value => "value",
    /// A structure's field, written or projected.
    Field => "field",
    /// A function's or a lambda's binder.
    Parameter => "parameter",
    /// A binder whose stated type is a sort.
    TypeParameter => "type-parameter",
    /// A `let`'s or a pattern's binder.
    Local => "local",
}
