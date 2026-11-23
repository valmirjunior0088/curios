//! A proof read as the checkers reduce it, through a real certifier: what reduction discards and what it keeps, a match's scrutinee read before the match reduces, a part reading a binder the proof writes, and a host row that diverges behind a definition.

use {
    super::test_support::*,
    curios_abi::{ForeignFunction, WireReturn, WireSignature},
    curios_analysis::reached_as_reduced,
    curios_cert::{Certifier, test_support::*},
    curios_common::{Qualifier, fill},
    curios_core::{Free, Global, Intrinsic, Term, UniverseContext},
    std::sync::Arc,
};

/// `/K` hands back the first of two naturals, and the proposition `/P` has one constructor.
const DECLARATIONS: &str = r#"
induct /P: Prop
| c()
end

let /K : (a: #NatType, b: #NatType) -> #NatType = (a: #NatType, b: #NatType) => a;
"#;

fn global(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

/// A certifier holding [`DECLARATIONS`] beside three definitions not known to terminate: `/spin`, at a natural, and `/lp`, a proof of `/P`, each with no value, as both checkers hold a definition whose recursion does not descend; and `/exits`, which unfolds to a call of a host row that diverges.
fn certifier_over_loops() -> Certifier<'static> {
    let mut certifier = certifier();
    declared(&mut certifier, DECLARATIONS);
    for (name, type_) in [
        ("spin", "(n: #NatType) -> #NatType"),
        ("lp", "#{node = InductType} /P()"),
    ] {
        certifier
            .declare(&global(name), &term(type_), &UniverseContext::default())
            .expect("a name is declared once");
    }

    let row = Arc::new(ForeignFunction {
        name: "/never".to_string(),
        signature: WireSignature {
            params: Vec::new(),
            returns: WireReturn::Never,
        },
    });
    let natural = Term::intrinsic(Intrinsic::NatType);
    let binder = Free::local(1, Some("n"));
    certifier
        .define(
            &global("exits"),
            &Term::func_type(
                [(binder, natural.clone())],
                Term::intrinsic(Intrinsic::io_type(natural.clone())),
            ),
            &Term::func(
                [(binder, natural.clone())],
                Term::foreign(row, global("never"), vec![natural]),
            ),
            &UniverseContext::default(),
        )
        .expect("a name is declared once");

    certifier
}

/// What the term `text` writes reaches as reduced, `/spin`, `/lp` and `/exits` being the definitions not known to terminate.
fn reached(text: &str) -> Option<Global> {
    let partial = |_: &Certifier, name: &Global| {
        ["spin", "lp", "exits"]
            .into_iter()
            .any(|path| *name == global(path))
    };

    reached_as_reduced(&mut certifier_over_loops(), &term(text), &partial)
        .expect("the walk is within its budget")
}

// `/K` discards its second argument, so `/spin` handed there is no reach, and handed as its first it is what the reduct is headed by.
#[test]
fn a_definition_reduction_discards_is_no_reach_and_one_it_keeps_is() {
    assert_eq!(reached("/K(1, /spin(0))"), None);
    assert_eq!(reached("/K(/spin(0), 1)"), Some(global("spin")));
}

// A match that states its proposition reads nothing of a closed scrutinee and is its arm, so reduction alone would pass over `/lp` and leave `0`. The walk reads the scrutinee first, and refuses it; a scrutinee that is the constructor lets the match reduce, and what its arm discards is no reach. Mutation-checked: reading no scrutinee before the match reduces admits `/lp`.
#[test]
fn a_matchs_scrutinee_is_read_before_the_match_reduces() {
    let stated = |scrutinee: &str, arm: &str| {
        fill!(
            "#{goal = #NatType, cases = {stated = #{node = InductType} /P()}} match ${scrutinee} | c() => ${arm} end",
            scrutinee,
            arm
        )
    };

    assert_eq!(reached(&stated("/lp", "0")), Some(global("lp")));
    assert_eq!(reached(&stated("/P/c()", "/K(0, /spin(0))")), None);
}

// A lambda's body that reads its binder is read by the names it writes: forced, a recursion that descends on the binder would unfold once for every arm the walk opened. One that reads no binder is closed, forced, and what it discards is no reach. Mutation-checked: forcing a part whatever it reads unfolds `/K(x, /spin(0))` to `x`, and admits it.
#[test]
fn a_part_reading_a_binder_the_proof_writes_is_read_by_the_names_it_writes() {
    assert_eq!(
        reached("(x: #NatType) => /K(x, /spin(0))"),
        Some(global("spin"))
    );
    assert_eq!(reached("(x: #NatType) => /K(0, /spin(0))"), None);
}

// `/exits` unfolds to a call of a host row that diverges, which no checker evaluates and which names no definition: the reduct keeps it, and it is reached by the definition that unfolded to it. Discarded, it is no reach. Mutation-checked: reading a kept call as nothing admits `/exits(0)`.
#[test]
fn a_host_row_that_diverges_is_reached_by_the_definition_unfolding_to_it() {
    assert_eq!(reached("/exits(0)"), Some(global("exits")));
    assert_eq!(reached("/K(0, /exits(0))"), None);
}
