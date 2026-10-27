//! A structure's fields: sized against its level as payloads are, and held non-informative where it is a proposition.

use {
    super::test_support::*,
    crate::{Error, Kernel, check_struct_decl},
    curios_core::{
        Free, Global, Intrinsic, Level, StructDecl, StructType, Subterm, Telescope, Term,
        UniverseContext, Universes,
    },
    curios_utilities::{Plicity, Qualifier},
};

/// A `Prop`-sorted structure carrying a `Nat`, which is the shape `Prop` non-informativeness exists to forbid.
///
/// Irrelevance identifies every inhabitant of a proposition, and a structure's payload is read back by *projection*, which is not an elimination and so meets no large-elimination guard: an informative field hands two convertible values to the same projection, and `Eq` plus congruence turns that into `False`.
///
/// The kernel holds the rule itself rather than leaving it to `curios-elab`: the elaborator refuses such a declaration before the kernel is asked, so no program backs the kernel's copy up, and `invert.rs`'s irrelevance guard cites this very property as its reason for leaving structures undecomposed.
#[test]
fn a_proposition_may_not_carry_an_informative_field() {
    let mut kernel = kernel();
    let declaration = proposition_with_field(&mut kernel, Term::intrinsic(Intrinsic::NatType));

    assert!(matches!(
        check_struct_decl(&mut kernel, &declaration),
        Err(Error::Informative { .. })
    ));
}

/// A `Prop`-sorted structure carrying a *type*, which the same rule forbids for the same reason.
///
/// `carried : Type 0` makes the field a type held as data. Irrelevance identifies `Bad{{}}` with `Bad{False}` at `Bad`, and `.0` reads the carried type straight back out — a projection meets no elimination guard at all — so `{}` and `False` become convertible, and `subst` turns `()` into a closed inhabitant of `False`.
///
/// This is the field-side face of the elimination-side hole in `super::super::infer::eliminate::large_elimination_tests`: both gates ask `carries_information`, which must count a universe-typed field as carrying information however completely erasure deletes it. No `.crs` file reaches it — `curios-elab` refuses the declaration by its own `is_prop` test, so the kernel is never asked from source, and only this fixture holds `check_struct_decl` to it.
#[test]
fn a_proposition_may_not_carry_a_type() {
    let mut kernel = kernel();
    let declaration = proposition_with_field(&mut kernel, Term::type_ground());

    assert!(matches!(
        check_struct_decl(&mut kernel, &declaration),
        Err(Error::Informative { .. })
    ));
}

/// The other end of the discrimination: a proposition whose field is *itself* a proposition carries nothing a program can read, and must stay legal — a rule that refused every `Prop` structure would be indistinguishable from one that refused this.
///
/// The field type is a `Prop`-sorted family, not the universe `Prop`. Typed at `Term::prop()`, the field would be a proposition held *as data* rather than a proof of one — precisely the shape the fixture above forbids — and this control would pass for the wrong reason.
#[test]
fn a_proposition_may_carry_a_proof() {
    let mut kernel = kernel();
    let held = proposition(&mut kernel, "Held");
    let declaration = proposition_with_field(&mut kernel, held);

    assert!(check_struct_decl(&mut kernel, &declaration).is_ok());
}

/// A field's mark changes nothing of what it holds: a hidden field at `Nat`, and a concept's superclass edge at a relevant record, are each refused as a written field is. The elaborator refuses both sources before the kernel is asked (`curios`'s `tests::board::proposition_tests`), so this is the kernel's copy of the rule under each mark a generated record entry carries.
#[test]
fn a_proposition_may_not_carry_an_informative_field_under_a_mark() {
    let dictionary = Global::Authored(Qualifier::from(["Dict"]));
    let marked = |kernel: &mut Kernel, mark: Plicity, field_type: Term| {
        let name = Global::Authored(Qualifier::from(["Bad"]));
        let declaration = StructDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::done(Telescope::build_marked(
                [(mark, Free::local(0, Some("value")), field_type)],
                (),
            )),
            result_sort: Term::prop(),
            module: Qualifier::default(),
            rep_public: true,
            polarities: Vec::new(),
            variances: Vec::new(),
        };
        kernel
            .declare_struct(&name, &declaration)
            .expect("a name is declared once");

        check_struct_decl(kernel, &declaration)
    };

    let mut hidden = kernel();
    assert!(matches!(
        marked(
            &mut hidden,
            Plicity::Implicit,
            Term::intrinsic(Intrinsic::NatType),
        ),
        Err(Error::Informative { .. })
    ));

    // A relevant record standing for a data concept's dictionary.
    let mut superclass = kernel();
    let counted = StructDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build(
            [(
                Free::local(1, Some("count")),
                Term::intrinsic(Intrinsic::NatType),
            )],
            (),
        )),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };
    superclass
        .declare_struct(&dictionary, &counted)
        .expect("a name is declared once");
    let dictionary_type: Term = Subterm::StructType(StructType {
        name: dictionary,
        universes: Universes::default(),
        params: Vec::new(),
    })
    .into();
    assert!(matches!(
        marked(&mut superclass, Plicity::Witness, dictionary_type),
        Err(Error::Informative { .. })
    ));
}

/// A `struct` field is sized against the structure's level exactly as an inductive payload is: `struct Bad : Type 0 { value : Type 0 }` holds a field at its own level and is refused, the struct face of [`a_payload_at_the_familys_own_level_is_refused`]. The existing `Prop`-structure fixtures reach `check_non_informative` first and never exercise the size clause, so without this the field-sizing arm of `check_signature` could stop firing and only the `Prop` path would notice.
#[test]
fn a_struct_field_at_the_structs_own_level_is_refused() {
    let mut kernel = kernel();
    let declaration = struct_with_field(&mut kernel, Term::type_ground(), Term::type_ground());

    assert!(matches!(
        check_struct_decl(&mut kernel, &declaration),
        Err(Error::Oversized { .. }),
    ));
}

/// The control: the same field one level below the structure is admitted, so the clause refuses a field at or above the level and not every field.
#[test]
fn a_struct_field_below_the_structs_level_is_admitted() {
    let mut kernel = kernel();
    let one = Level::zero().succ().expect("level zero has a successor");
    let declaration = struct_with_field(&mut kernel, Term::type_at(one), Term::type_ground());

    assert_eq!(check_struct_decl(&mut kernel, &declaration), Ok(()));
}

/// A parameterized intrinsic carrier sits at its element's level, so one of the structure's own types is a field at the structure's level: `struct Bad : Type 0 { value : F(Type 0) }` for each of `List`, `Cell`, `Channel` and `Io` would hold a code for every type of its own universe, `Bad` among them, which is type in type and Girard's paradox. Each is refused as `value : Type 0` is, and admitted a level up, the control.
///
/// Mutation-checked at `sort_of_intrinsic`: a carrier pinned at level zero admits the small structure.
#[test]
fn a_field_holding_a_carrier_of_the_structs_own_types_is_refused() {
    let formers: [fn(Term) -> Intrinsic; 4] = [
        Intrinsic::ListType,
        Intrinsic::CellType,
        Intrinsic::ChannelType,
        Intrinsic::IoType,
    ];
    let one = Level::zero().succ().expect("level zero has a successor");

    for former in formers {
        let codes = || Term::intrinsic(former(Term::type_ground()));

        let mut kernel = kernel();
        let declaration = struct_with_field(&mut kernel, Term::type_ground(), codes());
        assert!(
            matches!(
                check_struct_decl(&mut kernel, &declaration),
                Err(Error::Oversized { .. }),
            ),
            "a small structure held `{}`",
            codes(),
        );

        let mut kernel = self::kernel();
        let declaration = struct_with_field(&mut kernel, Term::type_at(one.clone()), codes());
        assert_eq!(check_struct_decl(&mut kernel, &declaration), Ok(()));
    }
}
