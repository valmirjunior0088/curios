//! What Core text writes beside a program's grammar, and that a program's grammar reads none of it: the decorator and what it attaches to, the values it states, a witness by its identity, an intrinsic node, a witness binder's word, and the file a `.crsc` is.

use {
    crate::*,
    curios_common::{Plicity, Qualifier, Source},
    curios_core::{Global, HeadKey, WitnessId},
    curios_parse::ParserError,
};

/// A term of Core text.
fn core(source: &str) -> Term<CoreText> {
    parse_whole_term::<CoreText>(&Source::inline(source)).unwrap()
}

/// The name `label` as a term of Core text.
fn core_name(label: &str) -> Term<CoreText> {
    Subterm::Name(Name::from([label.to_string()])).into()
}

/// What a decorator states and the term it states it of, where `term` is one.
fn decorated(term: &Term<CoreText>) -> (Vec<(String, CoreValue)>, &Term<CoreText>) {
    match term.as_subterm() {
        Subterm::Core(CoreForm::Decorated(Decorated { fields, term })) => (
            fields
                .iter()
                .map(|field| (field.label.to_string(), field.value.clone()))
                .collect(),
            term,
        ),
        other => panic!("expected a decorator, got {other:?}"),
    }
}

/// A decorator states the fields of the operand an infix operator takes: a call whole, and its head alone where the head is parenthesized.
#[test]
fn a_decorator_attaches_to_the_call_and_a_parenthesized_one_to_its_head() {
    let call = core("#{island = m} f(x)");
    let (fields, operand) = decorated(&call);
    assert_eq!(
        fields,
        [("island".to_string(), CoreValue::Term(core_name("m")))]
    );
    assert!(matches!(operand.as_subterm(), Subterm::Apply(_)));

    let head = core("(#{island = m} f)(x)");
    match head.as_subterm() {
        Subterm::Apply(apply) => assert_eq!(decorated(&apply.head).1, &core_name("f")),
        other => panic!("expected a call, got {other:?}"),
    }
}

/// An operator's operands may each be decorated, and a decorator binds tighter than any operator: it reaches no further than the operand it precedes.
#[test]
fn a_decorated_operand_stands_on_either_side_of_an_operator() {
    let sum = core("#{a = x} p + #{b = y} q");
    match sum.as_subterm() {
        Subterm::Infix(Infix { left, right, .. }) => {
            assert_eq!(decorated(left).1, &core_name("p"));
            assert_eq!(decorated(right).1, &core_name("q"));
        }
        other => panic!("expected an operator, got {other:?}"),
    }
}

/// A decorator states any number of fields, its last comma optional, and a match closed by `end` is an operand like a name.
#[test]
fn a_decorator_states_several_fields_of_a_match() {
    let term = core("#{stated = T(b), proof = true,} match p | mk(x) => x end");
    let (fields, operand) = decorated(&term);
    assert_eq!(
        fields
            .iter()
            .map(|(label, _)| label.as_str())
            .collect::<Vec<_>>(),
        ["stated", "proof"]
    );
    assert!(matches!(operand.as_subterm(), Subterm::Match(_)));
}

/// A larger term is decorated in parentheses, a decorator reaches nothing past `}` but an operand, and a variable no binder in the text binds is `_` under one.
#[test]
fn a_decorator_takes_an_operand_and_nothing_larger() {
    let read = |source: &str| parse_whole_term::<CoreText>(&Source::inline(source));
    assert!(read("#{a = x} (y) => y").is_err());
    assert!(read("#{a = x} ((y) => y)").is_ok());
    assert!(read("#{a = x}").is_err());
    let bound = core("#{binding = Bound(2)} _");
    assert!(matches!(decorated(&bound).1.as_subterm(), Subterm::Held));
}

/// A field holds a struct value, a list whose entries take marks, the root module, or a term — `{}` being a struct value that states nothing.
#[test]
fn a_decorator_states_struct_list_and_root_values() {
    let term = core(
        "#{universes = {levels = [u + 1]}, params = [@Type, use /Show(A)], island = /, empty = {}} f",
    );
    let (fields, _) = decorated(&term);
    let values = fields
        .into_iter()
        .map(|(_, value)| value)
        .collect::<Vec<_>>();
    match &values[0] {
        CoreValue::Fields(nested) => {
            assert_eq!(nested[0].label.to_string(), "levels");
            assert!(matches!(&nested[0].value, CoreValue::List(levels) if levels.len() == 1));
        }
        other => panic!("expected a struct value, got {other:?}"),
    }
    match &values[1] {
        CoreValue::List(entries) => assert_eq!(
            entries.iter().map(|(mark, _)| *mark).collect::<Vec<_>>(),
            [Plicity::Implicit, Plicity::Witness]
        ),
        other => panic!("expected a list, got {other:?}"),
    }
    assert_eq!(values[2], CoreValue::Root);
    assert_eq!(values[3], CoreValue::Fields(Vec::new()));
}

/// A witness is named by its identity — its module, its concept, its key's heads and its place where it is not the first — and one spelling no key by its place; a call chain may follow the name.
#[test]
fn a_witness_is_named_by_its_identity() {
    let spelled = |source: &str| match core(source).as_subterm() {
        Subterm::Core(CoreForm::Witness(id)) => *id,
        Subterm::Apply(apply) => match apply.head.as_subterm() {
            Subterm::Core(CoreForm::Witness(id)) => *id,
            other => panic!("expected a witness, got {other:?}"),
        },
        other => panic!("expected a witness, got {other:?}"),
    };
    let path = |segments: &[&str]| {
        Qualifier::from(
            segments
                .iter()
                .map(|segment| segment.to_string())
                .collect::<Vec<_>>(),
        )
    };

    assert_eq!(
        spelled("/std/Nat/witness~/std/ops/Add(Nat)"),
        WitnessId::Spelled {
            concept: path(&["std", "ops", "Add"]),
            key: [HeadKey::Nat].into(),
            module: path(&["std", "Nat"]),
            place: 0,
        }
    );
    assert_eq!(
        spelled("/witness~/Pair(/m/T, {x: _, _})#1(y)"),
        WitnessId::Spelled {
            concept: path(&["Pair"]),
            key: [
                HeadKey::Nominal(Global::Authored(path(&["m", "T"]))),
                HeadKey::TupleType(vec!["x".to_string(), String::new()]),
            ]
            .into(),
            module: Qualifier::empty(),
            place: 1,
        }
    );
    assert_eq!(
        spelled("/m/witness~3"),
        WitnessId::Unspelled {
            module: path(&["m"]),
            place: 3,
        }
    );
}

/// An intrinsic node is `#` and its variant, with its operands where it has any, each a value.
#[test]
fn an_intrinsic_reads_its_variant_and_operands() {
    let intrinsic = |term: &Term<CoreText>| match term.as_subterm() {
        Subterm::Core(CoreForm::Intrinsic(intrinsic)) => intrinsic.clone(),
        other => panic!("expected an intrinsic, got {other:?}"),
    };

    let nat = intrinsic(&core("#NatType"));
    assert_eq!(nat.variant.to_string(), "NatType");
    assert_eq!(nat.operands, None);

    let list = intrinsic(&core("#List(#NatType, [1, 2])"));
    assert_eq!(list.variant.to_string(), "List");
    assert!(matches!(
        list.operands.as_deref(),
        Some([CoreValue::Term(_), CoreValue::List(items)]) if items.len() == 2
    ));
}

/// A `use` binder is written by its word in Core text, where a later member reads it, and a program's grammar has no word there.
#[test]
fn a_witness_binder_takes_its_word_in_core_text_alone() {
    match core("(use d: Show(A)) -> P(d)").as_subterm() {
        Subterm::FuncType(FuncType { params, .. }) => {
            assert_eq!(params[0].label.as_deref(), Some("d"))
        }
        other => panic!("expected a function type, got {other:?}"),
    }
    match core("(use d, x) => d").as_subterm() {
        Subterm::Func(Func { params, .. }) => {
            assert_eq!(params[0].pattern, Pattern::Binder(Label::from("d")))
        }
        other => panic!("expected a function, got {other:?}"),
    }
    assert!("(use d: Show(A)) -> P(d)".parse::<Term>().is_err());
    assert!("(use d, x) => d".parse::<Term>().is_err());
}

/// A program's grammar reads none of Core text's forms — a decorator, a declaration's absolute name, a witness by its identity, an intrinsic node — and refuses each where it stands, at the token it does not expect; `%` between two operands is the remainder still.
#[test]
fn a_program_reads_no_form_of_core_text() {
    let refused_at = |error: ParserError| error.report().span.map(|span| span.start);
    for (source, at) in [
        ("#{a = x} f", 0),
        ("f(#{a = x} y)", 2),
        ("/std/Nat/witness~/std/ops/Add(Nat)", 16),
        ("#NatType", 0),
    ] {
        let error = source.parse::<Term>().expect_err(source);
        assert_eq!(refused_at(error), Some(at), "{source:?}");
    }
    for (source, at) in [
        ("#{a = b}\nlet a : Nat = 1;\n", 0),
        ("let /a : Nat = 1;\n", 4),
        ("let a : Type =\n    #{island = m} Type;\n", 19),
        ("induct /Box: Type\n| box()\nend\n", 7),
    ] {
        let error = source.parse::<Module>().expect_err(source);
        assert_eq!(refused_at(error), Some(at), "{source:?}");
    }
    assert!("a % b".parse::<Term>().is_ok());
}

/// A file's items: a group of declarations of any kinds joined by `and`, each under its decorator, a `let` member after `and` by its name alone and the `;` owed after a last `let`; a test and a foreign row; then the entry and the type it states.
#[test]
fn a_file_reads_its_items_and_its_entry() {
    let file = "#{totality = partial}\nlet /std/Nat/double(n: Nat) -> Nat = n\n#{proof = true}\nand /Read : Type = Nat;\ninduct /Box: #{level = u} Type\n| #{params = [@Type]} box()\nend\nand struct /Pair: (/T) { x: Nat }\nand /f : Nat = 1;\ntest /t = x;\nforeign /host_echo(Bytes) -> Bytes;\nmain\n: Nat\n"
        .parse::<CoreFile>()
        .unwrap();
    let kinds = file
        .items
        .iter()
        .map(|item| match item {
            CoreItem::Group(members) => members
                .iter()
                .map(|member| match member {
                    CoreMember::Let(member) => format!("let {}", member.label),
                    CoreMember::Induct(member) => format!("induct {}", member.label),
                    CoreMember::Struct(member) => format!("struct {}", member.label),
                    CoreMember::Concept(member) => format!("concept {}", member.label),
                })
                .collect::<Vec<_>>()
                .join(" and "),
            CoreItem::Test(test) => format!("test {}", test.label),
            CoreItem::Foreign(row) => format!("foreign {}", row.label),
        })
        .collect::<Vec<_>>();
    assert_eq!(
        kinds,
        [
            "let double and let Read",
            "induct Box and struct Pair and let f",
            "test t",
            "foreign host_echo",
        ]
    );
    let CoreItem::Group(members) = &file.items[0] else {
        panic!("expected a group")
    };
    let CoreMember::Let(second) = &members[1] else {
        panic!("expected a let")
    };
    assert_eq!(second.core.fields[0].label.to_string(), "proof");
    let CoreItem::Group(members) = &file.items[1] else {
        panic!("expected a group")
    };
    let CoreMember::Induct(family) = &members[0] else {
        panic!("expected an induct")
    };
    assert_eq!(family.cases[0].core[0].label.to_string(), "params");
    let entry = file.entry.expect("a program's entry");
    assert_eq!(entry.body, core_name("main"));
    assert!(entry.type_.is_some());
}

/// A declaration of Core text is named by its absolute path, or a witness's identity, never a bare label.
#[test]
fn a_declaration_of_core_text_is_named_absolutely() {
    assert!("let a : Nat = 1;\n".parse::<CoreFile>().is_err());
    let file = "#{kind = Witness}\nlet /witness~/Show(Nat) : /Show(Nat) = x;\n"
        .parse::<CoreFile>()
        .unwrap();
    let CoreItem::Group(members) = &file.items[0] else {
        panic!("expected a group")
    };
    let CoreMember::Let(witness) = &members[0] else {
        panic!("expected a let")
    };
    assert!(matches!(witness.core.name, Global::Witness(_)));
}
