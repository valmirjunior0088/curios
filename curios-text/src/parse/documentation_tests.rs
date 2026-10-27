//! Documentation comments: the declaration, member or module each attaches to, the line and space it takes, and the places one is refused.

use {super::test_support::comments_of, crate::*};

/// A `---` block attaches to the declaration below it, line by line, with a bare `---` as a paragraph break; blank lines and plain comments between the block and the declaration are insignificant, and the plain comment stays a comment.
#[test]
fn a_documentation_comment_attaches_to_the_declaration_below_it() {
    let source = "--- Doubles.\n---\n--- Twice, that is.\n\n-- plain\npub let double(n: Nat) -> Nat = n + n;";
    let module = source.parse::<Module>().unwrap();
    let TopItem::Let(members) = &module.items[0] else {
        panic!("expected a let item");
    };
    let doc = members[0].doc.as_ref().expect("documented");
    assert_eq!(doc.lines, ["Doubles.", "", "Twice, that is."]);
    assert_eq!(comments_of(source), ["-- plain"]);
}

/// A block attaches to a constructor, a struct field and a concept method, and only to the member directly below it.
#[test]
fn a_documentation_comment_attaches_to_a_constructor_a_field_and_a_method() {
    let source = concat!(
        "induct Shape: pub Type\n--- Round.\n| circle(Nat)\n| square(Nat)\nend\n",
        "struct Point: pub Type {\n    --- Across.\n    x: Nat,\n    y: Nat,\n}\n",
        "concept Show(A: Type): pub Type {\n    --- Renders.\n    show(A) -> Str,\n}\n",
    );
    let module = source.parse::<Module>().unwrap();

    let TopItem::Induct(inducts) = &module.items[0] else {
        panic!("expected an induct item");
    };
    assert_eq!(inducts[0].cases[0].doc.as_ref().unwrap().lines, ["Round."]);
    assert_eq!(inducts[0].cases[1].doc, None);

    let TopItem::Struct(structs) = &module.items[1] else {
        panic!("expected a struct item");
    };
    assert_eq!(
        structs[0].fields[0].doc.as_ref().unwrap().lines,
        ["Across."]
    );
    assert_eq!(structs[0].fields[1].doc, None);

    let TopItem::Concept(concepts) = &module.items[2] else {
        panic!("expected a concept item");
    };
    assert_eq!(
        concepts[0].fields[0].doc.as_ref().unwrap().lines,
        ["Renders."]
    );
}

/// A block attaches to a `mod`, a `satisfy`, a `foreign`, and a later member of an `and` group.
#[test]
fn a_documentation_comment_attaches_to_a_module_a_witness_a_foreign_and_a_later_member() {
    let source = concat!(
        "--- Numbers.\nmod nat;\n",
        "--- Structural.\nsatisfy Equal(Nat) {\n    ..\n}\n",
        "--- Ticks.\nforeign clock() -> Nat;\n",
        "let a: Nat = 1\n--- The other.\nand b: Nat = 2;\n",
    );
    let module = source.parse::<Module>().unwrap();

    let TopItem::Mod(declaration) = &module.items[0] else {
        panic!("expected a mod item");
    };
    assert_eq!(declaration.doc.as_ref().unwrap().lines, ["Numbers."]);
    let TopItem::Witness(witnesses) = &module.items[1] else {
        panic!("expected a witness item");
    };
    assert_eq!(witnesses[0].doc.as_ref().unwrap().lines, ["Structural."]);
    let TopItem::Foreign(foreign) = &module.items[2] else {
        panic!("expected a foreign item");
    };
    assert_eq!(foreign.doc.as_ref().unwrap().lines, ["Ticks."]);
    let TopItem::Let(members) = &module.items[3] else {
        panic!("expected a let item");
    };
    assert_eq!(members[0].doc, None);
    assert_eq!(members[1].doc.as_ref().unwrap().lines, ["The other."]);
}

/// A block followed by nothing it may document is refused naming the rule, wherever it is written: at the end of a file, before a constructor list's `end`, before a field list's `}`, and before a program's tail.
#[test]
fn a_documentation_comment_before_nothing_is_refused() {
    for source in [
        "--- lost\n",
        "let x: Nat = 1;\n--- lost\n\n",
        "induct T: Type\n| a()\n--- lost\nend",
        "struct P: Type {\n    x: Nat,\n    --- lost\n}",
        "concept C(A: Type): Type {\n    --- lost\n}",
    ] {
        let error = source.parse::<Module>().unwrap_err().format();
        assert!(
            error.contains("must immediately precede"),
            "{source}: {error}"
        );
    }

    // A program refuses the same three places: before its tail, after it, and where the block is all that follows its items — the last read as a program by a command that tries one first.
    for source in [
        "let x: Nat = 1;\n--- lost\nx",
        "let x: Nat = 1;\nx\n--- lost\n",
        "let x: Nat = 1;\n--- lost\n",
    ] {
        let error = source.parse::<Entrypoint>().unwrap_err().format();
        assert!(
            error.contains("must immediately precede"),
            "{source}: {error}"
        );
    }
}

/// An import has no page and a test is not part of the interface, so a block before either is refused by name.
#[test]
fn a_documentation_comment_before_use_or_test_is_refused() {
    let error = "--- lost\nuse /std/{Nat};"
        .parse::<Module>()
        .unwrap_err()
        .format();
    assert!(error.contains("cannot precede `use`"), "{error}");

    let error = "--- lost\ntest t = Test/assert(true);"
        .parse::<Module>()
        .unwrap_err()
        .format();
    assert!(error.contains("cannot precede `test`"), "{error}");
}

/// Two blocks with a gap between them and one declaration below are refused, so a stray block far above is never silently absorbed.
#[test]
fn two_documentation_comments_before_one_declaration_are_refused() {
    for source in [
        "--- one\n\n--- two\nlet x: Nat = 1;",
        "--- one\n-- plain\n--- two\nlet x: Nat = 1;",
    ] {
        let error = source.parse::<Module>().unwrap_err().format();
        assert!(
            error.contains("two documentation comments"),
            "{source}: {error}"
        );

        // Read as a program, whose item loop backtracks where a module's does not, the refusal is the same one rather than the term grammar's reading of the first block.
        let error = format!("{source}\nx")
            .parse::<Entrypoint>()
            .unwrap_err()
            .format();
        assert!(
            error.contains("two documentation comments"),
            "{source}: {error}"
        );
    }
}

/// A block takes lines of its own: `---` after code is refused, and so is `---` glued to its text, a fourth dash included.
#[test]
fn a_documentation_comment_takes_a_line_of_its_own_and_its_space() {
    let error = "let x: Nat = 1; --- trailing\nlet y: Nat = 2;"
        .parse::<Module>()
        .unwrap_err()
        .format();
    assert!(error.contains("line of its own"), "{error}");

    for source in ["---glued\nlet x: Nat = 1;", "----\nlet x: Nat = 1;"] {
        let error = source.parse::<Module>().unwrap_err().format();
        assert!(error.contains("with the space"), "{source}: {error}");
    }
}

/// A block after a group's last member belongs to whatever follows it: `pub and` opens a member, while `pub let`, `pub induct` and a bare `satisfy` open the next item and end the group. The look-ahead reads past the `pub`, since a `pub` alone says nothing about which it is.
#[test]
fn a_documentation_comment_after_a_group_may_open_the_next_item() {
    let source = concat!(
        "satisfy Show(Nat) {\n    show = f,\n}\n",
        "--- Next.\npub let x: Nat = 1;\n",
        "let a: Nat = 1\n--- Member.\npub and b: Nat = 2;\n",
        "--- Type.\npub induct T: Type\nend\n",
        "--- Witness.\nsatisfy Spell(T) {\n    ..\n}\n",
    );
    let module = source.parse::<Module>().unwrap();
    assert_eq!(module.items.len(), 5);

    let TopItem::Let(members) = &module.items[1] else {
        panic!("expected the documented let");
    };
    assert_eq!(members[0].doc.as_ref().unwrap().lines, ["Next."]);
    let TopItem::Let(members) = &module.items[2] else {
        panic!("expected the group");
    };
    assert!(members[1].vis_pub);
    assert_eq!(members[1].doc.as_ref().unwrap().lines, ["Member."]);
    let TopItem::Induct(members) = &module.items[3] else {
        panic!("expected the documented induct");
    };
    assert_eq!(members[0].doc.as_ref().unwrap().lines, ["Type."]);
}
