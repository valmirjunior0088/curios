//! A witness's read of one of its own methods.
//!
//! A goal inside a witness's body that resolves to the witness being declared arrives as a projection of the witness's own name, applied to the parameters the body binds. Where the method projected mentions that name nowhere, the projection is that method's term — the witness unfolded once and projected — so the read is no recursion and is replaced by the term. A method that reaches itself, alone or through another, keeps the name, and only then is the witness the recursive group [`elaborate_module_let`](super::elaborate_module_let) makes of a body that names itself: a group is a knot at run time, forced at every use, which a body that states one method by another must not pay.

use {
    super::Context,
    curios_core::{Advance, Free, Func, Struct, Subterm, Term},
};

/// `body`, the zonked body of the witness `name`, with every read of one of its own methods that does not itself mention `name` replaced by that method's term.
///
/// Rewritten to a fixed point, since a method cleared of the name is one the others may then read. A body that is no concept literal under its parameters, and one that reads no such method, is returned as it stands.
pub(super) fn own_methods_read(context: &mut Context, name: &Free, body: &Term) -> Term {
    if !body.mentions_free(name) {
        return body.clone();
    }

    // The parameters are opened at fresh binders, so that a read at them is one closed spelling at every depth and a method's term carries no loose index to where it is read.
    let mut binders = Vec::new();
    let literal = match &**body {
        Subterm::Func(Func { telescope, .. }) => {
            let mut cursor = telescope.cursor();
            while let Some((_, annotation)) = cursor.entry() {
                let mark = cursor.mark().expect("a mark stands at an entry");
                let binder = cursor.advance_fresh(|hint| context.fresh(hint));
                binders.push((mark, binder, annotation));
            }
            cursor.body().expect("a cursor past every entry")
        }
        _ => body.clone(),
    };
    let Subterm::Struct(node) = &*literal else {
        return body.clone();
    };

    let itself = match binders.is_empty() {
        true => Term::free_var(name),
        false => Term::apply_marked(
            Term::free_var(name),
            binders
                .iter()
                .map(|(mark, binder, _)| (*mark, Term::free_var(binder))),
        ),
    };

    let mut fields = node.fields.clone();
    let mut cleared = true;
    while cleared {
        cleared = false;
        for index in 0..fields.len() {
            if fields[index].mentions_free(name) {
                continue;
            }
            let read = Term::proj_struct(itself.clone(), node.name, index);
            let method = fields[index].clone();
            for field in fields.iter_mut().filter(|field| field.mentions_free(name)) {
                let rewritten = field.replace_term(&read, &method);
                cleared |= rewritten != *field;
                *field = rewritten;
            }
        }
    }
    if fields == node.fields {
        return body.clone();
    }

    let literal = Term::from(Subterm::Struct(Struct {
        fields,
        ..node.clone()
    }));
    match binders.is_empty() {
        true => literal,
        false => Term::func_marked(binders, literal),
    }
}
