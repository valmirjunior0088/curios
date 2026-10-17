//! A fixture's text with values spliced into it, its own braces written as they stand: `${name}` marks a hole, a mark no fixture's language holds, where `format!` would make every literal brace a doubled one.

#[cfg(test)]
mod tests;

use std::fmt::{Display, Write};

/// `template` with each `${name}` replaced by the value `holes` gives that name. A hole the template leaves without a value, and a value the template never asks for, panic, so a misspelled name is never left standing in a fixture.
pub fn fill(template: &str, holes: &[(&str, &dyn Display)]) -> String {
    let mut filled = String::with_capacity(template.len());
    let mut asked = vec![false; holes.len()];
    let mut rest = template;

    while let Some(start) = rest.find("${") {
        filled.push_str(&rest[..start]);
        let after = &rest[start + 2..];
        let end = after
            .find('}')
            .unwrap_or_else(|| panic!("a `${{` with no `}}` closing it in the template"));
        let name = &after[..end];
        let place = holes
            .iter()
            .position(|(hole, _)| *hole == name)
            .unwrap_or_else(|| panic!("the template's `${{{name}}}` is given no value"));

        asked[place] = true;
        write!(filled, "{}", holes[place].1).expect("writing to a string succeeds");
        rest = &after[end + 1..];
    }
    filled.push_str(rest);

    if let Some(place) = asked.iter().position(|asked| !asked) {
        panic!(
            "`{}` is given a value the template never asks for",
            holes[place].0
        );
    }

    filled
}

/// [`fill`], each hole named by the variable that holds its value or written `name = value`, as `format!` takes them.
#[macro_export]
macro_rules! fill {
    (@value $name:ident = $value:expr) => {
        $value
    };
    (@value $name:ident) => {
        $name
    };
    ($template:expr $(, $name:ident $(= $value:expr)?)* $(,)?) => {
        $crate::test_support::fill(
            $template,
            &[$((
                stringify!($name),
                &$crate::fill!(@value $name $(= $value)?) as &dyn ::std::fmt::Display,
            )),*],
        )
    };
}
