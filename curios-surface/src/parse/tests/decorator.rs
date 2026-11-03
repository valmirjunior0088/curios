//! What Core text writes beside a program's grammar: the decorator `#{…}`, what it attaches to, a declaration named by its absolute path, and that each reads back.

use {crate::parse::test_support::*, crate::*};

/// What a decorator states and the term it states it of, where `term` is one.
fn decorated(term: &Term) -> (Vec<(String, Term)>, &Term) {
    match term.as_subterm() {
        Subterm::Decorated(Decorated { fields, term }) => (
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
    let call = "#{island = m} f(x)".parse::<Term>().unwrap();
    let (fields, operand) = decorated(&call);
    assert_eq!(fields, [("island".to_string(), name("m"))]);
    assert!(matches!(operand.as_subterm(), Subterm::Apply(_)));

    let head = "(#{island = m} f)(x)".parse::<Term>().unwrap();
    match head.as_subterm() {
        Subterm::Apply(apply) => assert_eq!(decorated(&apply.head).1, &name("f")),
        other => panic!("expected a call, got {other:?}"),
    }
}

/// An operator's operands may each be decorated, and a decorator binds tighter than any operator: it reaches no further than the operand it precedes.
#[test]
fn a_decorated_operand_stands_on_either_side_of_an_operator() {
    let sum = "#{a = x} p + #{b = y} q".parse::<Term>().unwrap();
    match sum.as_subterm() {
        Subterm::Infix(Infix { left, right, .. }) => {
            assert_eq!(decorated(left).1, &name("p"));
            assert_eq!(decorated(right).1, &name("q"));
        }
        other => panic!("expected an operator, got {other:?}"),
    }
}

/// A decorator states any number of fields, its last comma optional, and a match closed by `end` is an operand like a name.
#[test]
fn a_decorator_states_several_fields_of_a_match() {
    let term = "#{stated = T(b), proof = true,} match p | mk(x) => x end"
        .parse::<Term>()
        .unwrap();
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

/// A larger term is decorated in parentheses, and a decorator reaches nothing past `}` but an operand.
#[test]
fn a_decorator_takes_an_operand_and_nothing_larger() {
    assert!("#{a = x} (y) => y".parse::<Term>().is_err());
    assert!("#{a = x} ((y) => y)".parse::<Term>().is_ok());
    assert!("#{a = x}".parse::<Term>().is_err());
}

/// What the printer writes reads back as the tree it printed: a decorated head keeps its parentheses, and a decorator on a decorated term takes them.
#[test]
fn a_decorator_round_trips() {
    for source in [
        "#{island = m} f(x)",
        "(#{island = m} f)(x)",
        "#{a = x} p + #{b = y} q",
        "#{a = x} ((y) => y)",
        "#{a = x} (#{b = y} z)",
        "#{stated = T(b)} match p | mk(x) => x end",
    ] {
        let term = source.parse::<Term>().unwrap();
        assert_eq!(
            term.to_string().parse::<Term>().unwrap(),
            term,
            "round-trip failed for {source:?}: printed {term}"
        );
    }
}

/// A declaration's Core head: the module its absolute name lies under, and the decorator before it — before the declaring word for a group's first member and before `and` for a later one.
#[test]
fn a_declaration_reads_its_path_and_the_decorator_before_it() {
    let module = "#{totality = partial}\nlet /std/Nat/double(n: Nat) -> Nat = n\n#{proof = true}\nand /Read : Type = Nat;\ninduct /Box: Type\n| box()\nend\n"
        .parse::<Module>()
        .unwrap();
    let heads = module
        .items
        .iter()
        .flat_map(TopItem::core_heads)
        .map(|(core, label)| {
            let path = core.path.as_ref().map(|path| path.segments().to_vec());
            let fields = core
                .fields
                .iter()
                .map(|field| field.label.to_string())
                .collect::<Vec<_>>();

            (path, label.to_string(), fields)
        })
        .collect::<Vec<_>>();

    assert_eq!(
        heads,
        [
            (
                Some(vec!["std".to_string(), "Nat".to_string()]),
                "double".to_string(),
                vec!["totality".to_string()]
            ),
            (
                Some(Vec::new()),
                "Read".to_string(),
                vec!["proof".to_string()]
            ),
            (Some(Vec::new()), "Box".to_string(), Vec::new()),
        ]
    );
}

/// A decorator states the fields of a declaration, so one before an item that declares nothing a decorator could state — a module, an import, an anonymous witness — is refused, and so is a declaring path that is not absolute.
#[test]
fn a_decorator_before_no_declaration_is_refused() {
    for source in [
        "#{a = b}\nmod m;\n",
        "#{a = b}\nuse /std/{Nat};\n",
        "#{a = b}\nsatisfy C(Nat) { f = x }\n",
        "let a/b : Nat = 1;\n",
    ] {
        assert!(source.parse::<Module>().is_err(), "{source:?} was read");
    }
}

/// A module whose declarations carry Core heads prints back as the module it was read from.
#[test]
fn a_declarations_core_head_round_trips() {
    let source = "#{totality = partial}\nlet /std/Nat/double(n: Nat) -> Nat = n\n#{proof = true}\nand /Read : Type = Nat;\ninduct /Box: Type\n| box()\nend\n#{kind = test}\ntest /t = x;\n";
    let module = source.parse::<Module>().unwrap();

    assert_eq!(
        module.to_string().parse::<Module>().unwrap(),
        module,
        "printed {module}"
    );
}
