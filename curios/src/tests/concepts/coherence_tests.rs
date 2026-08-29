//! One witness per key: duplicates, orphans, and the standard library's witnesses that a user program may not shadow.

use super::super::{error, run};

// Registering two witnesses for the same `(concept, head)` key is a coherence error (global uniqueness) — independent of, and checked alongside, the orphan rule below.
#[test]
fn duplicate_witness_is_an_error() {
    let source = r#"
        use /std/{Nat, Handle, Str};
        pub concept Show(A : Type) : pub Type {
            show(A) -> Str
        }
        satisfy Show(Nat) {
            show(n) = Nat/to_str(n)
        }
        satisfy Show(Nat) {
            show(n) = Nat/to_str(n)
        }
        let n : Nat = 1;
        /std/print(Show/show(n))
        "#;

    // Both the concept and the declaring module are pinned, not just the word "witness". Witnesses are anonymous, so the module is the only coordinate the report can give a reader — and it comes from each definition's `island`, not from splitting the compiler-minted `witness@N` name. Matched on the message body: the `while elaborating …` prefix names the minted `witness@N`, which Phase C of the name-identity work replaces.
    assert!(error(source).ends_with(
        "duplicate witness of '/Show' for head 'Nat'\n  \
         one is declared in the entry module, another in the entry module\n  \
         every concept-head pair has at most one witness, program-wide"
    ));
}

// The declaring module of a nested-module witness renders as that module, which the entry-module cases above cannot distinguish from a bug that always reports the root.
#[test]
fn duplicate_witness_reports_its_declaring_module() {
    let source = r#"
        mod M
            pub concept C(A : Type) : pub Type {
                f(A) -> A
            }
            pub induct T : pub Type
            | t()
            end
            satisfy C(T) {
                f(x) = x
            }
            satisfy C(T) {
                f(x) = x
            }
        end
        /std/Io/pure(())
        "#;

    assert!(error(source).ends_with(
        "duplicate witness of '/M/C' for head '/M/T'\n  \
         one is declared in module '/M', another in module '/M'\n  \
         every concept-head pair has at most one witness, program-wide"
    ));
}

// The orphan rule: a witness may be declared only where the concept it witnesses, or a type in its key, is already declared. `Ord` and `Bool` are both `/std`/`/sys`-owned and the entry program owns neither, so `Ord(Bool)` (not already witnessed anywhere in the standard library) is rejected.
#[test]
fn orphan_witness_is_rejected() {
    let source = r#"
        use /std/{Bool, Ord, Order};
        satisfy Ord(Bool) {
            cmp(a, b) = Order/eq()
        }
        let n : Bool = true;
        n
        "#;

    assert!(error(source).ends_with(
        "orphan witness of '/std/Ord/Ord' for head 'Bool', declared in the entry module\n  \
         a witness may only be declared where the concept or a type in its head is already declared"
    ));
}

// The user's most natural attempt at incoherence, and the one the fixtures above leave out. `orphan_witness_is_rejected` deliberately picks `Ord(Bool)`, a pair the standard library does *not* witness, so nothing yet pins what happens when the entry program re-declares a witness the prelude already holds. `/std/Bool` witnesses `Show(Bool)`, and the answer must be a refusal — otherwise a program could silently replace a standard-library instance at every site that resolves it, which is exactly the incoherence "one witness per key, program-wide" exists to exclude.
//
// The orphan rule is what refuses it, and the ordering is deliberate: `register_witness` checks orphanhood before the duplicate-key insert, because "not allowed to declare this at all" is the more fundamental violation than "and it also collides". Coherence is not resting on that ordering, though, and this is the part worth recording. The replay path re-registers *every* prefix witness through the same `register_witness` — orphan check and duplicate insert alike — rather than trusting the archive, so the prelude's keys are already in the map a user's declaration is inserted into. Both barriers are live and each would refuse this alone; the fixture pins the one a user actually reaches.
#[test]
fn a_standard_library_witness_cannot_be_shadowed() {
    let source = r#"
        use /std/{Bool, Show, Str};
        satisfy Show(Bool) {
            show(b) = "shadow"
        }
        /std/print(Show/show(true))
        "#;

    assert!(error(source).ends_with(
        "orphan witness of '/std/Show/Show' for head 'Bool', declared in the entry module\n  \
         a witness may only be declared where the concept or a type in its head is already declared"
    ));
}

// The sanctioned counterpart: a user's own type is legal to `satisfy` a standard-library concept for, since the declaring root (the entry program) owns the key's type even though it doesn't own the concept.
#[test]
fn witness_for_a_locally_owned_type_is_not_an_orphan() {
    let source = r#"
        use /std/{Nat, Handle, Str, Show};
        pub struct Wrapper : pub Type { inner : Nat }
        satisfy Show(Wrapper) {
            show(w) = Nat/to_str(w.inner)
        }
        let w : Wrapper = Wrapper { inner = 7 };
        /std/print(Show/show(w))
        "#;

    assert_eq!(run(source), b"7");
}

// The single-occupancy rule holds through the new keying: two parametric witnesses over the same family collide on the family's head, whatever their prefix arguments.
#[test]
fn parametric_witnesses_over_one_family_still_collide() {
    let source = r#"
        use /std/{Nat, Str, Monad};
        induct Box(S : Type, A : Type) : Type
        | wrap(A)
        end
        satisfy (@S : Type) => Monad((A : Type) => Box(S, A)) {
            pure(@A, a) = Box/wrap(a),
            bind(@A, @B, m, f) =
                match m : (_) => Box(S, B)
                | wrap(a) => f(a)
                end,
        }
        satisfy Monad((A : Type) => Box(Str, A)) {
            pure(@A, a) = Box/wrap(a),
            bind(@A, @B, m, f) =
                match m : (_) => Box(Str, B)
                | wrap(a) => f(a)
                end,
        }
        /std/print("unreachable")
        "#;

    let message = error(source);
    assert!(
        message.contains("duplicate witness"),
        "expected the key collision, got: {message}"
    );
}

// A group's members register on their signatures before any body elaborates, so a member whose key another member already holds is refused as any duplicate is.
#[test]
fn a_group_member_with_a_taken_key_is_a_duplicate() {
    let source = r#"
        use /std/{Nat, Str};
        pub concept Show(A : Type) : pub Type {
            show(A) -> Str
        }
        induct A : pub Type | a(Nat) end
        satisfy Show(A) { show(x) = "first", }
        and Show(A) { show(x) = "second", }
        /std/print("unreachable")
        "#;

    let report = error(source);
    assert!(
        report.contains("duplicate witness of '/Show' for head '/A'"),
        "expected the member refused as a duplicate:\n{report}"
    );
}
