use {super::*, std::path::Path};

/// A text's form is read off the text: items alone are a module, a final term after them makes a program, and a text that does not parse is a program, whose own reading reports what is wrong with it.
#[test]
fn a_text_is_a_module_when_no_final_term_follows_its_items() {
    let path = Path::new("probe.crs");

    assert_eq!(
        Form::of(path, "pub let w : /std/Str = \"a\";\n"),
        Form::Module
    );
    assert_eq!(Form::of(path, ""), Form::Module);
    assert_eq!(
        Form::of(path, "pub let w : /std/Str = \"a\";\n/std/print(w)\n"),
        Form::Program
    );
    assert_eq!(Form::of(path, "let w : = ;\n"), Form::Program);
}
