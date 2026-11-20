//! The encoder: what it refuses at index-space construction, before any byte is written, what each function's entry of the code section measures, and what it leaves out for holding nothing.

use crate::*;

/// The local index space is the type's inputs followed by the locals; a param list of another length would number every local into the wrong slot, in a module that still validates.
#[test]
#[should_panic(expected = "func `f` names 1 params for a type of 2 inputs")]
fn a_param_list_of_another_length_than_its_type_is_refused() {
    let module = r#"
        (module $m
            (type $t (func (param i32 i32) (result i32)))
            (func $f (type $t) (param $x i32)
                (local $l i32)
                i32.const 7
                local.set $l
                local.get $l))
"#
    .parse::<Module>()
    .expect("expected a module");

    to_bytes(&module);
}

/// A body's size is its entry of the code section without the size before it: the locals, run-length encoded, then the expression and its `end`, the very bytes the module holds after that size.
#[test]
fn a_functions_code_size_is_its_entry_after_the_size() {
    let module = r#"
        (module $m
            (type $t (func (result i32)))
            (func $seven (type $t)
                i32.const 7)
            (func $local (type $t)
                (local $l i32)
                local.get $l))
"#
    .parse::<Module>()
    .expect("expected a module");

    let sizes = code_sizes(&module);
    // No locals, `i32.const 7`, `end`; then one run of one `i32`, `local.get 0`, `end`.
    let seven = [0x00, 0x41, 0x07, 0x0B];
    let local = [0x01, 0x01, 0x7F, 0x20, 0x00, 0x0B];
    assert_eq!(sizes[&FuncName::from("seven")], seven.len());
    assert_eq!(sizes[&FuncName::from("local")], local.len());

    let bytes = to_bytes(&module);
    for body in [&seven[..], &local[..]] {
        let entry = [&[body.len() as u8][..], body].concat();
        assert!(
            bytes.windows(entry.len()).any(|window| window == entry),
            "the module holds {entry:02X?}"
        );
    }
}

/// Each section `bytes` holds, in order, by its id, and each subsection of its name section, by its id after the section's.
fn sections(bytes: &[u8]) -> Vec<String> {
    fn leb(bytes: &[u8], mut at: usize) -> (usize, usize) {
        let (mut value, mut shift) = (0, 0);
        loop {
            let byte = bytes[at];
            at += 1;
            value |= usize::from(byte & 0x7f) << shift;
            shift += 7;
            if byte < 0x80 {
                return (value, at);
            }
        }
    }

    let mut found = Vec::new();
    let mut at = 8;
    while at < bytes.len() {
        let (size, start) = leb(bytes, at + 1);
        found.push(bytes[at].to_string());
        if bytes[at] == 0 {
            let (length, name) = leb(bytes, start);
            let mut sub = name + length;
            while sub < start + size {
                let (sub_size, sub_start) = leb(bytes, sub + 1);
                found.push(format!("0/{}", bytes[sub]));
                sub = sub_start + sub_size;
            }
        }
        at = start + size;
    }

    found
}

/// A section or a name subsection that would hold nothing is left out, an empty one decoding to the same module as none: no import, global, export or data section here, and of the names, the module's, its functions' and its type's alone, `$seven` naming no local.
#[test]
fn a_section_that_would_hold_nothing_is_left_out() {
    let module = r#"
        (module $m
            (type $t (func (result i32)))
            (func $seven (type $t)
                i32.const 7))
"#
    .parse::<Module>()
    .expect("expected a module");

    assert_eq!(
        sections(&to_bytes(&module)),
        ["1", "3", "10", "0", "0/0", "0/1", "0/4"]
    );
}

/// A function that names no local is no entry of the local names, which list the functions that name one.
#[test]
fn a_function_naming_no_local_is_no_entry_of_the_local_names() {
    let module = r#"
        (module $m
            (type $t (func (result i32)))
            (func $seven (type $t)
                i32.const 7)
            (func $local (type $t)
                (local $l i32)
                local.get $l))
"#
    .parse::<Module>()
    .expect("expected a module");
    let bytes = to_bytes(&module);

    // The local names: one entry, `$local`'s, at function index 1, naming one local, `l`.
    let entry = [0x02, 0x06, 0x01, 0x01, 0x01, 0x00, 0x01, b'l'];
    assert!(
        bytes.windows(entry.len()).any(|window| window == entry),
        "the local names hold `$local`'s entry alone: {bytes:02X?}"
    );
}

