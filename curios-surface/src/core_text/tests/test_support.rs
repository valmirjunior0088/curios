//! What the suites of `core_text` share: the terms and modules they write and read back.

use {
    crate::{CoreText, ReadError, Refusal, Spellable, Spelling, core_text::*, parse_whole_term},
    curios_common::{Qualifier, Source},
    curios_core::{
        Atom, Constructors, Definition, Definitions, Free, Global, InductDecl, InductParam,
        Intrinsic, Item, Nominal, Nominals, Telescope, Term, UniverseContext,
    },
    curios_module::SYNTAX,
    std::rc::Rc,
};

/// The term `term` writes, read on its own.
pub fn term_to_core(term: &crate::Term<CoreText>) -> Result<Term, ReadError> {
    core_read(term, &mut Reader::new(None))
}

/// `term` as Curios/Core proper writes it.
pub fn written(term: &Term) -> String {
    term.spelled(&Rc::new(Spelling::core_text(&SYNTAX)))
        .to_string()
}

/// The term `term`'s Curios/Core reads back as.
pub fn read_back(term: &Term) -> Term {
    let text = written(term);
    let parsed = parse_whole_term::<CoreText>(&Source::inline(&text))
        .unwrap_or_else(|error| panic!("{text}\n{}", error.format()));
    term_to_core(&parsed).unwrap_or_else(|error| panic!("{text}\n{error}"))
}

pub fn global(path: &[&str]) -> Global {
    Global::Authored(Qualifier::from(path.to_vec()))
}

pub fn nat_type() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

/// A module declaring `/Bit`, beside the `let`s of `beside`, in the order a module is stored in.
pub fn module(declaration: InductDecl, beside: Vec<Definition>) -> Definitions {
    let items = [Item::Nominal(Nominals::new(vec![(
        global(&["Bit"]),
        Nominal::Inductive(declaration),
    )]))]
    .into_iter()
    .chain(
        beside
            .into_iter()
            .map(|definition| Item::Let(Box::new(definition))),
    )
    .collect::<Vec<_>>();

    Definitions {
        items: items.into(),
    }
    .ordered()
}

/// What reading `text` as a term refuses, and where the refusal begins in it.
pub fn refused(text: &str) -> (Refusal, Option<usize>) {
    let parsed = parse_whole_term::<CoreText>(&Source::inline(text))
        .unwrap_or_else(|error| panic!("{text}\n{}", error.format()));
    let error = term_to_core(&parsed).expect_err(text);

    (error.refusal, error.span.map(|span| span.start))
}

/// `induct /Bit: Type | zero() | one() end`'s entry, whose name stands in the root.
pub fn bit() -> InductDecl {
    let case = |tag: &str| {
        (
            Atom::from(tag),
            InductParam::over(
                &[],
                Telescope::build(Vec::<(Free, Term)>::new(), Vec::new()),
            ),
        )
    };

    InductDecl::new(
        UniverseContext::empty(),
        Telescope::build(
            Vec::<(Free, Term)>::new(),
            Telescope::build(Vec::<(Free, Term)>::new(), ()),
        ),
        Constructors::try_from(vec![case("zero"), case("one")]).unwrap(),
        Term::type_ground(),
        Qualifier::empty(),
        false,
    )
}
