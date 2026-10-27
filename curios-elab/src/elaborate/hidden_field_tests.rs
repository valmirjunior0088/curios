//! A structure's hidden fields: a position counts the plain ones, a literal writes one under its mark or leaves it out, and a spread never copies one.

use {
    super::test_support::*,
    crate::*,
    curios_core::{
        Struct, StructDecl, StructEntry, StructType, Subterm, Telescope, Term, UniverseContext,
        Universes,
    },
    curios_utilities::{Plicity, Qualifier},
};

// A structure of the fields `{ @n : Nat, x : Nat }` — a hidden field ahead of a plain one, the least telescope a written position is counted over. A concept's edge ahead of its first method is the same shape.
fn register_marked(context: &mut Context) -> Term {
    let hidden = context.fresh(Some("n"));
    let plain = context.fresh(Some("x"));
    context
        .register_struct(
            &nominal("Marked"),
            StructDecl {
                universe_context: UniverseContext::empty(),
                arity: Telescope::done(Telescope::build_marked(
                    [
                        (Plicity::Implicit, hidden, nat()),
                        (Plicity::Explicit, plain, nat()),
                    ],
                    (),
                )),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )
        .unwrap();

    Term::from(Subterm::StructType(StructType {
        name: nominal("Marked"),
        universes: Universes::default(),
        params: Vec::new(),
    }))
}

// A written position counts the plain fields, so `v.0` is the first of them and the hidden field ahead of it is reached by its label. The projection is rebuilt as the slot of the structure its head is a value of, which is what it reads as when elaborated again.
#[test]
fn a_written_position_counts_the_plain_fields() {
    let mut context = context();
    let marked = register_marked(&mut context);
    let value = context.fresh(Some("v"));
    context.assume(&value, &marked);
    let written = |position| Term::proj_position(Term::free_var(&value), position);
    let slot = |index| Term::proj_struct(Term::free_var(&value), nominal("Marked"), index);

    let (elaborated, type_) = elaborate(&mut context, &written(0), Mode::Infer).unwrap();
    assert_eq!((&elaborated, &type_), (&slot(1), &nat()));
    let (again, _) = elaborate(&mut context, &elaborated, Mode::Infer).unwrap();
    assert_eq!(again, slot(1));

    let labelled = Term::proj_label(Term::free_var(&value), "n");
    let (elaborated, _) = elaborate(&mut context, &labelled, Mode::Infer).unwrap();
    assert_eq!(elaborated, slot(0));

    // A slot arrives as whatever built it called it, and leaves as one of the head's structure.
    let bare = Term::proj(Term::free_var(&value), 1);
    let (elaborated, _) = elaborate(&mut context, &bare, Mode::Infer).unwrap();
    assert_eq!(elaborated, slot(1));

    // Past the plain fields, counted as a reader counts them.
    assert!(matches!(
        elaborate(&mut context, &written(1), Mode::Infer),
        Err(Error::TupleIndexOutOfBounds { index: 1, arity: 1 })
    ));
}

// A structure value with a field per slot and no entries is the normal form: every slot in order, the hidden one included, read as the literal written in full and so elaborated again unchanged. A literal as written carries its entries, and a plain one never fills a hidden slot.
#[test]
fn a_structure_value_in_normal_form_is_elaborated_again_unchanged() {
    let mut context = context();
    let marked = register_marked(&mut context);
    let whole = Term::struct_(
        nominal("Marked"),
        Vec::<Term>::new(),
        [nat_lit(3), nat_lit(5)],
    );

    let (elaborated, type_) = elaborate(&mut context, &whole, Mode::Infer).unwrap();
    assert_eq!((&elaborated, &type_), (&whole, &marked));
    let (again, _) = elaborate(&mut context, &elaborated, Mode::Infer).unwrap();
    assert_eq!(again, whole);

    let positional = Term::struct_entries(
        nominal("Marked"),
        Vec::<Term>::new(),
        [
            (StructEntry::Field(None), nat_lit(3)),
            (StructEntry::Field(None), nat_lit(5)),
        ],
    );
    assert!(matches!(
        elaborate(&mut context, &positional, Mode::Infer),
        Err(Error::WrongNumberOfFields {
            expected: 1,
            got: 2,
            ..
        })
    ));
}

// `Marked { <entries> }`, as lowering hands a written literal over.
fn marked_literal<const N: usize>(entries: [(StructEntry, Term); N]) -> Term {
    Term::struct_entries(nominal("Marked"), Vec::<Term>::new(), entries)
}

// A literal writes a hidden field under `@`, named or alone in its run, and the value it builds is the normal form, every slot in order.
#[test]
fn a_literal_writes_a_hidden_field_under_its_mark() {
    let mut context = context();
    register_marked(&mut context);
    let whole = Term::struct_(
        nominal("Marked"),
        Vec::<Term>::new(),
        [nat_lit(3), nat_lit(5)],
    );

    for hidden in [
        StructEntry::Implicit(None),
        StructEntry::Implicit(Some("n".to_string())),
    ] {
        let literal =
            marked_literal([(hidden, nat_lit(3)), (StructEntry::Field(None), nat_lit(5))]);
        let (elaborated, _) = elaborate(&mut context, &literal, Mode::Infer).unwrap();
        assert_eq!(elaborated, whole);
    }
}

// A hidden field left out is no entry the literal lacks: its slot is filled as a call's omitted `@` argument is — here by a metavariable, nothing having determined it — and the plain field is counted against the plain fields alone.
#[test]
fn a_literal_leaves_a_hidden_field_out() {
    let mut context = context();
    register_marked(&mut context);
    let literal = marked_literal([(StructEntry::Field(None), nat_lit(5))]);

    let (elaborated, _) = elaborate(&mut context, &literal, Mode::Infer).unwrap();
    let Subterm::Struct(Struct {
        fields, entries, ..
    }) = &*elaborated
    else {
        panic!("expected a structure value, got {elaborated:?}");
    };
    assert!(entries.is_empty());
    assert!(matches!(&*fields[0], Subterm::Metavar(_)));
    assert_eq!(fields[1], nat_lit(5));
}

// A hidden entry is held to its run and to the name of the field it fills, and `use` is a concept's mark alone.
#[test]
fn a_hidden_entry_that_misaligns_is_refused() {
    let mut context = context();
    register_marked(&mut context);
    let mut refusal = |entries: [(StructEntry, Term); 2]| {
        elaborate(&mut context, &marked_literal(entries), Mode::Infer).unwrap_err()
    };

    assert!(matches!(
        refusal([
            (StructEntry::Field(None), nat_lit(5)),
            (StructEntry::Implicit(None), nat_lit(3)),
        ]),
        Error::HiddenMemberWithoutSlot {
            written: Plicity::Implicit
        }
    ));
    assert!(matches!(
        refusal([
            (StructEntry::Implicit(Some("m".to_string())), nat_lit(3)),
            (StructEntry::Field(None), nat_lit(5)),
        ]),
        Error::UnknownStructField { label, .. } if label == "@m"
    ));
    assert!(matches!(
        refusal([
            (StructEntry::Use, nat_lit(3)),
            (StructEntry::Field(None), nat_lit(5)),
        ]),
        Error::UseEntryOutsideConcept { .. }
    ));
}

// A spread copies the plain fields and never a hidden one: left out it is inferred anew, and written it is held to its field as in any literal.
#[test]
fn a_spread_never_copies_a_hidden_field() {
    let mut context = context();
    let marked = register_marked(&mut context);
    let value = context.fresh(Some("v"));
    context.assume(&value, &marked);
    let base = || (StructEntry::Spread, Term::free_var(&value));

    for literal in [
        marked_literal([base()]),
        marked_literal([base(), (StructEntry::Implicit(None), nat_lit(7))]),
        marked_literal([
            base(),
            (StructEntry::Implicit(Some("n".to_string())), nat_lit(7)),
        ]),
    ] {
        let (_, type_) = elaborate(&mut context, &literal, Mode::Infer).unwrap();
        assert_eq!(type_, marked);
    }

    let misnamed = marked_literal([
        base(),
        (StructEntry::Implicit(Some("m".to_string())), nat_lit(7)),
    ]);
    assert!(matches!(
        elaborate(&mut context, &misnamed, Mode::Infer),
        Err(Error::UnknownStructField { label, .. }) if label == "@m"
    ));
}
