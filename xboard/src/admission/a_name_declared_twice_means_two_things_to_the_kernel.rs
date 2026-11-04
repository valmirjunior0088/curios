use crate::{Date, Proof, Status, Ticket, Witness, refused};

pub(super) const TICKET: Ticket = Ticket {
    title: "A name declared twice means two things to the kernel",
    found_on: Date::new(2026, 10, 6),
    status: Status::Fixed,
    witnesses: &[
        Witness {
            what: "an inhabited `False` under the prelude's own name",
            proof: Proof::Module(A_REGISTRY_ENTRY_UNDER_THE_PRELUDES_FALSE),
            expect: refused![kernel: curios_cert::Error::Redeclared(_)],
        },
        Witness {
            what: "a theorem typed under one definition of its statement and read under another",
            proof: Proof::Module(A_STATEMENT_DECLARED_TWICE),
            expect: refused![kernel: curios_cert::Error::Redeclared(_)],
        },
    ],
};

/// Declared over the environment a walk is seeded with, the entry stands where the prelude's did and is the one every rule reads. Checking it would not refuse it: an inhabited proposition is a well-formed declaration, and what is wrong with this one is its name.
const A_REGISTRY_ENTRY_UNDER_THE_PRELUDES_FALSE: &str = r#"
    #{module = /, absent = [/sys/Bool/False/mk]} induct /sys/Bool/False: pub Prop
    | mk()
    end
    #{node = Variant} /sys/Bool/False/mk()
    "#;

/// A walk takes items as they are stored, and a name declared twice is written twice: `thm` is typed while `p` is `Holds`, and the second `p`, which names `thm`, is defined where the first stood. The closing term is then read at a `p` that reduces to `False`.
const A_STATEMENT_DECLARED_TWICE: &str = r#"
    #{absent = [/Holds/qed]} induct /Holds: pub Prop
    | qed()
    end
    let /p : Prop =
        #{node = InductType} /Holds();
    let /thm : /p =
        #{node = Variant} /Holds/qed();
    let /p : Prop =
        ((x: #{node = InductType} /Holds()) => /sys/Bool/False)(/thm);
    /thm
    "#;
