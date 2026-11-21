use super::*;

/// A text's form is read off the text in its dialect: declarations alone are a module, a final term after them makes a program, and a text that does not parse is a program, whose own reading reports what is wrong with it.
#[test]
fn a_text_is_a_module_when_no_final_term_follows_its_declarations() {
    let form = |text: &str, written| Form::of(&Source::held(Path::new("probe"), text), written);

    assert_eq!(
        form("pub let w : /std/Str = \"a\";\n", WrittenIn::Program),
        Form::Module
    );
    assert_eq!(form("", WrittenIn::Program), Form::Module);
    assert_eq!(
        form(
            "pub let w : /std/Str = \"a\";\n/std/print(w)\n",
            WrittenIn::Program
        ),
        Form::Program
    );
    assert_eq!(form("let w : = ;\n", WrittenIn::Program), Form::Program);

    // As `--stage core-elab` prints a declaration of a program.
    let declared = "let /double: (n: #NatType) -> #NatType =\n    (n: #NatType) =>\n        (#{field = Struct(/std/ops/Add, 0)} /std/Nat/witness~/std/ops/Add(Nat).0)(n, n);\n";
    assert_eq!(form(declared, WrittenIn::CoreText), Form::Module);
    assert_eq!(
        form(
            &format!("{declared}\n/sys/Bool/True()\n"),
            WrittenIn::CoreText
        ),
        Form::Program
    );
    assert_eq!(form("", WrittenIn::CoreText), Form::Module);
    assert_eq!(
        form("/sys/Bool/True()\n", WrittenIn::CoreText),
        Form::Program
    );
    assert_eq!(
        form("pub let w : /std/Str = \"a\";\n", WrittenIn::CoreText),
        Form::Program
    );
}
