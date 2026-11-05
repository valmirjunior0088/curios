//! What a later walk believes of a unit an earlier walk filed — a record withheld in part or whole, a unit compiled over its earlier step — each attack a history the kernel refuses where it holds.

use {
    super::*,
    crate::*,
    curios_cert::{Error, Verdict},
    curios_common::{Qualifier, fill},
    curios_core::{Entrypoint, Program, Term, Universes, Zonked},
    curios_pipeline::{Earlier, recheck},
};

/// The declaration `/<path>`.
fn named(path: &str) -> Global {
    Global::Authored(Qualifier::from(path.split('/')))
}

/// What the kernel says of `proof`, Core text closing with a term, put at `/std/Bool/False` over the prelude and every unit `history` built, through the walk a compilation puts a program to.
fn put(history: &History, proof: &str) -> Vec<Verdict> {
    history
        .built(|predecessors| {
            let file = proof.parse::<CoreFile>().expect("the proof parses");
            let (definitions, entry) =
                to_core(&file, Some(&FormScope::over(SYNTAX, predecessors.cores())))
                    .expect("the proof reads over the units in scope");
            let program = Program {
                definitions,
                entrypoint: Entrypoint {
                    body: entry.expect("the proof closes with a term").body,
                    type_: Some(Term::instance(
                        Global::Authored(SYNTAX.proof.false_type.qualifier()),
                        Universes::default(),
                    )),
                },
            };
            let program =
                Zonked::project(&program).expect("the proof holds nothing elaboration solves");

            recheck(&program, DEFAULT_BUDGET, predecessors, &SYNTAX)
        })
        .expect("the history builds")
}

/// `loopy` loops at a type holding a proof of `False`, and `unbox` takes the proof out, so a proof unpacking `loopy(0)` has only (V)'s reading of `loopy` as partial between it and `False`.
const PARTIAL_BOX: &str = "\
use /std/{Nat};
use /std/Bool/{False};

pub induct Box: pub Type
| box(p: False)
end

pub let loopy(n: Nat) -> Box = loopy(n);

pub let unbox(b: Box) -> False =
    match b
    | box(p) => p
    end;
";

/// A proof position reaching `/lib/loopy`.
const UNPACKED: &str = "/lib/unbox(/lib/loopy(0))\n";

/// Whether `verdicts` refuse the closing term as reaching what is not known to terminate.
fn refused_as_partial(verdicts: &[Verdict]) -> bool {
    verdicts
        .iter()
        .any(|verdict| verdict.name.is_none() && matches!(verdict.error, Error::NotTotal { .. }))
}

/// A record withheld whole leaves its unit's items classified from their terms, every recursion among them one no walk typed and so partial. Mutation-checked: with `Totals::seeded` classifying none of `Globals::unclassified`, the proof is admitted.
#[test]
fn a_partial_definition_whose_record_is_withheld_whole_is_classified_partial_from_its_terms() {
    let mut history = History::new();
    history
        .step("lib", Written::Program, PARTIAL_BOX)
        .expect("the library builds");
    history
        .withhold("lib", None)
        .expect("the record is withheld whole");

    let verdicts = put(&history, UNPACKED);

    assert!(refused_as_partial(&verdicts), "{verdicts:?}");
}

/// A record naming fewer definitions than its unit is read of none of them, so withholding the partial definition's verdict alone leaves every item, `unbox` among them, classified from its terms. Mutation-checked as the test above.
#[test]
fn a_partial_definition_whose_verdict_alone_is_withheld_is_classified_partial_from_its_terms() {
    let mut history = History::new();
    history
        .step("lib", Written::Program, PARTIAL_BOX)
        .expect("the library builds");
    history
        .withhold("lib", Some(&[named("lib/loopy")]))
        .expect("the verdict is withheld");

    let verdicts = put(&history, UNPACKED);

    assert!(refused_as_partial(&verdicts), "{verdicts:?}");
}

/// `loop` is a group generic in a sort that answers a `Box`, data at every assignment, so it may loop and its walk files it partial under its own spelling; read at `Prop` at `False`, its value's field is a proof.
const PARTIAL_AT_A_SORT_LEFT_TO_A_CALLER: &str = "\
use /std/{Nat};

pub struct Box(A: Sort): pub Type {
    a: A,
}

pub let loop(@A: Sort, n: Nat) -> Box(A) = loop(n);
";

/// A proof position reaching `/lib/loop` at `Prop`, at `False`.
const PROJECTED_AT_PROP: &str = r"
#{field = Struct(/lib/Box, 0)}
(
    (
        #{universes = {levels = [0], qualities = [Prop]}}
        /lib/loop)(@/sys/Bool/False, 0)
).0
";

/// A group that leaves a sort to its caller is filed under its own spelling with what every assignment reached, so a later walk reading it at `Prop`, where its field is a proof, reads it partial: over its unit's record, and with the record withheld, from its terms.
///
/// Mutation-check: have `check_rec_group` file the group under its own spelling `Total` whatever its assignments reached, and the proof over the kept record is admitted at `False`.
#[test]
fn a_group_generic_in_a_sort_filed_partial_is_read_partial_at_prop() {
    for withheld in [false, true] {
        let mut history = History::new();
        history
            .step("lib", Written::Program, PARTIAL_AT_A_SORT_LEFT_TO_A_CALLER)
            .expect("the library builds");
        if withheld {
            history
                .withhold("lib", None)
                .expect("the record is withheld whole");
        }

        let verdicts = put(&history, PROJECTED_AT_PROP);

        assert!(
            refused_as_partial(&verdicts),
            "withheld {withheld}: {verdicts:?}"
        );
    }
}

/// `Neg` uses its parameter negatively.
const NEG: &str = "\
use /std/Bool/{False};

pub induct Neg(A: Type): pub Type
| mk(f: (A) -> False)
end
";

/// `Wrap` is negative in its parameter through the `Neg` it holds and in no other way.
const WRAP: &str = "\
use /lib/{Neg};

pub induct Wrap(A: Type): pub Type
| w(inner: Neg(A))
end
";

/// `Bad` holds a function out of itself, through `/lib/Neg`, and `self_app` applies one to itself: no recursion to grade, so only strict positivity stands between it and `False`.
const BAD_THROUGH_NEG: &str = r"
induct /Bad: pub Type
| c(
    n:
        #{node = InductType, universes = {levels = [0]}}
        /lib/Neg(
            #{node = InductType}
            /Bad())
)
end
let /self_app : (
    b:
        #{node = InductType}
        /Bad()
) ->
    #{node = InductType}
    /sys/Bool/False() =
    (
        b:
            #{node = InductType}
            /Bad()
    ) =>
        #{
            result = Ambient(
                #{node = InductType}
                /sys/Bool/False())}
        match b
        | c(m) =>
            #{
                result = Ambient(
                    #{node = InductType}
                    /sys/Bool/False())}
            match m
            | mk(f) =>
                f(b)
            end
        end;
/self_app(
    /Bad/c(
        (
            #{universes = {levels = [0]}}
            /lib/Neg/mk)(
            @
                #{node = InductType}
                /Bad(),
            /self_app
        )
    )
)
";

/// The same through `/wrap/Wrap`, negative only through the `Neg` it holds.
const BAD_THROUGH_WRAP: &str = r"
induct /Bad: pub Type
| c(
    n:
        #{node = InductType, universes = {levels = [0]}}
        /wrap/Wrap(
            #{node = InductType}
            /Bad())
)
end
let /self_app : (
    b:
        #{node = InductType}
        /Bad()
) ->
    #{node = InductType}
    /sys/Bool/False() =
    (
        b:
            #{node = InductType}
            /Bad()
    ) =>
        #{
            result = Ambient(
                #{node = InductType}
                /sys/Bool/False())}
        match b
        | c(m) =>
            #{
                result = Ambient(
                    #{node = InductType}
                    /sys/Bool/False())}
            match m
            | w(i) =>
                #{
                    result = Ambient(
                        #{node = InductType}
                        /sys/Bool/False())}
                match i
                | mk(f) =>
                    f(b)
                end
            end
        end;
/self_app(
    /Bad/c(
        (
            #{universes = {levels = [0]}}
            /wrap/Wrap/w)(
            @
                #{node = InductType}
                /Bad(),
            (
                #{universes = {levels = [0]}}
                /lib/Neg/mk)(
                @
                    #{node = InductType}
                    /Bad(),
                /self_app
            )
        )
    )
)
";

/// Whether `verdicts` refuse `/Bad` as not strictly positive.
fn refused_as_negative(verdicts: &[Verdict]) -> bool {
    verdicts.iter().any(|verdict| {
        verdict.name == Some(named("Bad")) && matches!(verdict.error, Error::NotPositive { .. })
    })
}

/// A family whose vector its unit's record no longer files is analysed by the walk that mounts the unit, before anything is judged, and `Bad` composes through what that analysis found. No mutation is claimed: an unfiled family answers `Mixed`, which refuses as well.
#[test]
fn a_family_whose_record_is_withheld_is_composed_through_as_its_terms_say() {
    let mut history = History::new();
    history
        .step("lib", Written::Program, NEG)
        .expect("the library builds");
    history
        .withhold("lib", Some(&[named("lib/Neg")]))
        .expect("the family's verdict and vectors are withheld");

    let verdicts = put(&history, BAD_THROUGH_NEG);

    assert!(refused_as_negative(&verdicts), "{verdicts:?}");
}

/// The analysis of a unit mounted with no vector on record composes through the vectors of the units before it, so `Wrap`, withheld, is found negative through `Neg`, kept. No mutation is claimed, as above.
#[test]
fn a_family_whose_record_is_withheld_is_analysed_through_the_vectors_filed_before_it() {
    let mut history = History::new();
    history
        .step("lib", Written::Program, NEG)
        .expect("the library builds");
    history
        .step("wrap", Written::Program, WRAP)
        .expect("the wrapper builds");
    history
        .withhold("wrap", None)
        .expect("the wrapper's record is withheld whole");

    let verdicts = put(&history, BAD_THROUGH_WRAP);

    assert!(refused_as_negative(&verdicts), "{verdicts:?}");
}

/// A family holding a type of its own level, which `/down` takes a level lower. The nearest attack on a variance vector withheld: it holds whether the walk mounting `/lib` computes the vector or answers invariant, `Big` comparing invariantly over its record as filed too, so no mutation is claimed.
#[test]
fn a_large_familys_instance_is_not_taken_a_level_down_once_its_record_is_withheld() {
    let mut history = History::new();
    history
        .step(
            "lib",
            Written::CoreText,
            r"
#{universe_context = {parameter_count = 1}}
induct /lib/Big: pub
    #{level = u + 1}
    Type
| big(
    T:
        #{level = u}
        Type
)
end
",
        )
        .expect("the library builds");
    history
        .withhold("lib", None)
        .expect("the record is withheld whole");

    let verdicts = put(
        &history,
        r"
let /down : (
    b:
        #{node = InductType, universes = {levels = [1]}}
        /lib/Big()
) ->
    #{node = InductType, universes = {levels = [0]}}
    /lib/Big() =
    (
        b:
            #{node = InductType, universes = {levels = [1]}}
            /lib/Big()
    ) =>
        b;
0
",
    );

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name == Some(named("down"))
                && matches!(verdict.error, Error::Mismatch { .. })),
        "{verdicts:?}"
    );
}

/// `r` loops at a type holding a proof of `False`, written as Core text so the kernel alone judges the unit.
const PARTIAL_BOX_TEXT: &str = r"
induct /app/Box: pub Type
| box(
    p:
        #{node = InductType}
        /sys/Bool/False()
)
end
#{node = Rec}
let /app/r : (n: #NatType) ->
    #{node = InductType}
    /app/Box() =
    (n: #NatType) => /app/r(n);
let /app/unbox : (
    b:
        #{node = InductType}
        /app/Box()
) ->
    #{node = InductType}
    /sys/Bool/False() =
    (
        b:
            #{node = InductType}
            /app/Box()
    ) =>
        #{
            result = Ambient(
                #{node = InductType}
                /sys/Bool/False())}
        match b
        | box(p) =>
            p
        end;
";

/// A baseline whose record misses a definition is no baseline, so the unit is judged whole and `r`, which no edit reaches, is classified rather than read as the baseline's record leaves it. Mutation-checked: with a baseline taken whatever its record covers (`verdicts_within`'s `covers` filter dropped), the step builds.
#[test]
fn a_definition_added_beside_one_whose_verdict_is_withheld_is_judged_against_it_as_partial() {
    let mut history = History::new();
    history
        .step("app", Written::CoreText, PARTIAL_BOX_TEXT)
        .expect("the unit builds");
    history
        .withhold("app", Some(&[named("app/r")]))
        .expect("the verdict is withheld");

    let stepped = history.step(
        "app",
        Written::CoreText,
        &fill!(
            r"${unit}let /app/t :
    #{node = InductType}
    /sys/Bool/False() =
    /app/unbox(/app/r(0));
",
            unit = PARTIAL_BOX_TEXT,
        ),
    );

    assert!(
        matches!(
            &stepped,
            Err(Stopped { why: Unbuilt::Unmade(Unmade::Kernel(verdict)), .. })
                if verdict.name == Some(named("app/t")) && matches!(verdict.error, Error::NotTotal { .. })
        ),
        "{stepped:?}"
    );
}

/// `p` proves `c == 0` by `refl`, so it reads `c`'s body; the edit to `c` leaves `p`'s terms as they were. Mutation-checked: with `Changed::of` following no reader of what moved, the step builds.
#[test]
fn a_proof_unfolding_a_definition_an_edit_moved_is_judged_again() {
    let unit = |value: u8| {
        fill!(
            r"let /lib/c : #NatType = ${value};
let /lib/p :
    #{node = InductType, universes = {levels = [0], qualities = [Type]}}
    /std/Eq/Eq(#NatType)(/lib/c, 0) =
    (
        #{universes = {levels = [0], qualities = [Type]}}
        /std/Eq/Eq/refl)(@#NatType, @0);
",
            value,
        )
    };
    let mut history = History::new();
    history
        .step("lib", Written::CoreText, &unit(0))
        .expect("the unit builds");

    let stepped = history.step("lib", Written::CoreText, &unit(1));

    assert!(
        matches!(
            &stepped,
            Err(Stopped { why: Unbuilt::Unmade(Unmade::Kernel(verdict)), .. })
                if verdict.name == Some(named("lib/p")) && matches!(verdict.error, Error::Mismatch { .. })
        ),
        "{stepped:?}"
    );
}

/// `/app/p` unfolds `/mid/m`, a unit that does not move, into `/lib/c`, which an edit moves: both bodies were read by `p`'s judgment, so the read of `/lib` is asked again. Mutation-checked: with `read_stands` answering that every read of a unit in scope stands, the step builds.
#[test]
fn a_proof_unfolding_through_a_unit_that_did_not_move_into_one_that_did_is_judged_again() {
    let lib = |value: u8| fill!("use /std/{Nat};\n\npub let c: Nat = ${value};\n", value);
    let mut history = History::new();
    history
        .step("lib", Written::Program, &lib(0))
        .expect("the library builds");
    history
        .step(
            "mid",
            Written::Program,
            "use /std/{Nat};\nuse /lib/{c};\n\npub let m: Nat = c;\n",
        )
        .expect("the middle unit builds");
    history
        .step(
            "app",
            Written::CoreText,
            r"
let /app/p :
    #{node = InductType, universes = {levels = [0], qualities = [Type]}}
    /std/Eq/Eq(#NatType)(/mid/m, 0) =
    (
        #{universes = {levels = [0], qualities = [Type]}}
        /std/Eq/Eq/refl)(@#NatType, @0);
",
        )
        .expect("the proof's unit builds");

    let stepped = history.step("lib", Written::Program, &lib(1));

    assert!(
        matches!(
            &stepped,
            Err(Stopped { mount, why: Unbuilt::Unmade(Unmade::Kernel(verdict)) })
                if mount == "app"
                    && verdict.name == Some(named("app/p"))
                    && matches!(verdict.error, Error::Mismatch { .. })
        ),
        "{stepped:?}"
    );
}

/// The unit of `t`, which unpacks `w(0)` at `Q`: `w` total and `Q` true, or `w` looping at the same type and `Q` false. `t`'s terms are the same in both, and it reads `w` and `Q` by their signatures alone.
fn verdict_moved(looping: bool) -> String {
    let (q, w) = match looping {
        false => (
            "/sys/Bool/True()",
            r"let /lib/w : (n: #NatType) ->
    #{node = InductType, universes = {levels = [0]}}
    /lib/Box(/lib/Q) =
    (n: #NatType) =>
        (
            #{universes = {levels = [0]}}
            /lib/Box/box)(
            @
                #{node = InductType}
                /sys/Bool/True(),
            /sys/Bool/True/qed()
        );
",
        ),
        true => (
            "/sys/Bool/False()",
            r"#{node = Rec}
let /lib/w : (n: #NatType) ->
    #{node = InductType, universes = {levels = [0]}}
    /lib/Box(/lib/Q) =
    (n: #NatType) => /lib/w(n);
",
        ),
    };

    fill!(
        r"#{universe_context = {parameter_count = 1}}
induct /lib/Box(
    P:
        #{level = u}
        Prop
): pub
    #{level = u}
    Type
| box(p: P)
end
let /lib/Q : Prop =
    #{node = InductType}
    ${q};
#{universe_context = {parameter_count = 1}}
let /lib/unbox : (
    @P:
        #{level = u}
        Prop,
    b:
        #{node = InductType, universes = {levels = [u]}}
        /lib/Box(P)
) -> P =
    (
        @P:
            #{level = u}
            Prop,
        b:
            #{node = InductType, universes = {levels = [u]}}
            /lib/Box(P)
    ) =>
        #{result = Ambient(P)}
        match b
        | box(p) =>
            p
        end;
${w}let /lib/t : /lib/Q =
    (
        #{universes = {levels = [0]}}
        /lib/unbox)(@/lib/Q, /lib/w(0));
",
        q,
        w,
    )
}

/// The edit moves `w`'s verdict and `Q`'s body and nothing `t` reads but `w`'s verdict, which a signature's digest holds.
#[test]
fn a_proof_reaching_a_definition_whose_verdict_alone_moved_is_judged_again() {
    let mut history = History::new();
    history
        .step("lib", Written::CoreText, &verdict_moved(false))
        .expect("the unit builds");

    let stepped = history.step("lib", Written::CoreText, &verdict_moved(true));

    assert!(
        matches!(
            &stepped,
            Err(Stopped { why: Unbuilt::Unmade(Unmade::Kernel(verdict)), .. })
                if verdict.name == Some(named("lib/t")) && matches!(verdict.error, Error::NotTotal { .. })
        ),
        "{stepped:?}"
    );
}

/// The unit of `f`, which covers `T` with the arm `a` alone and answers `Q(t)`, whose one case is at `a`: `T` holds `a`, or `a` and `b`. `f`'s terms are the same in both, so only `T`'s registry entry, read where `f` matches, moved.
fn covered(cases: &str) -> String {
    fill!(
        r"induct /lib/T: pub Type
${cases}end
induct /lib/Q: (
    t:
        #{node = InductType}
        /lib/T()
) -> pub Prop
| qa(): (/lib/T/a())
end
let /lib/f : (
    t:
        #{node = InductType}
        /lib/T()
) ->
    #{node = InductType}
    /lib/Q()(t) =
    (
        t:
            #{node = InductType}
            /lib/T()
    ) =>
        #{
            result = Ambient(
                #{node = InductType}
                /lib/Q()(t))}
        match t
        | a() =>
            /lib/Q/qa()
        end;
",
        cases,
    )
}

/// Adopted, `f(T/b())` would be a `Q(T/b())`, which has no case. Mutation-checked: with `Baseline::entries_alike` answering alike whatever the entries, the step builds.
#[test]
fn a_match_over_a_family_an_edit_gave_a_case_is_judged_again() {
    let mut history = History::new();
    history
        .step("lib", Written::CoreText, &covered("| a()\n"))
        .expect("the unit builds");

    let stepped = history.step("lib", Written::CoreText, &covered("| a()\n| b()\n"));

    assert!(
        matches!(
            &stepped,
            Err(Stopped { why: Unbuilt::Unmade(Unmade::Kernel(verdict)), .. })
                if verdict.name == Some(named("lib/f")) && matches!(verdict.error, Error::MissingArm { .. })
        ),
        "{stepped:?}"
    );
}

/// `/lib/Q`, the proposition `q` spells, and `/lib/t`, which proves it by `True`'s constructor: `t` stored after `Q`, or ahead of it where `t_first`.
fn q_and_t(q: &str, t_first: bool) -> String {
    let q = fill!(
        r"let /lib/Q : Prop =
    #{node = InductType}
    ${q};
",
        q
    );
    let t = r"let /lib/t : /lib/Q =
    #{node = Variant}
    /sys/Bool/True/qed();
";

    match t_first {
        true => [t, q.as_str()].concat(),
        false => [q.as_str(), t].concat(),
    }
}

/// `t` moved ahead of the `Q` it reads while `Q` is made `False`: `Q` is not the baseline's, so `t`, which read `Q`'s body, is asked where it now stands, before `Q` is settled, and is judged there and finds `Q` unbound, as a walk over every declaration does.
///
/// Mutation-check to make: with the closure `verdicts_within` hands `Adopting::adopts` answering that every declaration the module holds stands, `t` is adopted and the step builds.
#[test]
fn a_declaration_an_edit_moved_past_its_reader_is_unbound_where_the_reader_is_judged() {
    let mut history = History::new();
    history
        .step(
            "lib",
            Written::CoreText,
            &q_and_t("/sys/Bool/True()", false),
        )
        .expect("the unit builds");

    let stepped = history.step(
        "lib",
        Written::CoreText,
        &q_and_t("/sys/Bool/False()", true),
    );

    assert!(
        matches!(
            &stepped,
            Err(Stopped { why: Unbuilt::Unmade(Unmade::Kernel(verdict)), .. })
                if verdict.name == Some(named("lib/t")) && matches!(verdict.error, Error::Undeclared(_))
        ),
        "{stepped:?}"
    );
}

/// `Q` true and `R` false, then each item renamed into the other's name where it stood: the item at `Q`'s place is now `R`, true, and the item at `R`'s place is now `Q`, false. A declaration is the baseline's by the item the baseline holds under its name, so `Q` is not, and `t`, which unfolded `Q`, is judged again.
#[test]
fn declarations_renamed_into_each_others_places_are_compared_by_their_names() {
    let unit = |first: (&str, &str), second: (&str, &str)| {
        fill!(
            r"let ${first_name} : Prop =
    #{node = InductType}
    ${first_value};
let ${second_name} : Prop =
    #{node = InductType}
    ${second_value};
let /lib/t : /lib/Q =
    #{node = Variant}
    /sys/Bool/True/qed();
",
            first_name = first.0,
            first_value = first.1,
            second_name = second.0,
            second_value = second.1,
        )
    };
    let mut history = History::new();
    history
        .step(
            "lib",
            Written::CoreText,
            &unit(
                ("/lib/Q", "/sys/Bool/True()"),
                ("/lib/R", "/sys/Bool/False()"),
            ),
        )
        .expect("the unit builds");

    let stepped = history.step(
        "lib",
        Written::CoreText,
        &unit(
            ("/lib/R", "/sys/Bool/True()"),
            ("/lib/Q", "/sys/Bool/False()"),
        ),
    );

    assert!(
        matches!(
            &stepped,
            Err(Stopped { why: Unbuilt::Unmade(Unmade::Kernel(verdict)), .. })
                if verdict.name == Some(named("lib/t")) && matches!(verdict.error, Error::Mismatch { .. })
        ),
        "{stepped:?}"
    );
}

/// The unit claiming `/a` declares `/b/Q`, a name under a mount it does not claim, and the unit claiming `/b` proves it by unfolding it. Once `/a` makes `/b/Q` false, the read is asked of the unit that holds the name, which moved, and not of the mount the name spells, which is the unit being compiled.
#[test]
fn a_name_declared_under_a_mount_its_unit_does_not_claim_is_asked_of_the_unit_that_holds_it() {
    let declared = |q: &str| {
        fill!(
            r"let /b/Q : Prop =
    #{node = InductType}
    ${q};
",
            q
        )
    };
    let mut history = History::new();
    history
        .step("a", Written::CoreText, &declared("/sys/Bool/True()"))
        .expect("the unit claiming `/a` builds");
    history
        .step(
            "b",
            Written::CoreText,
            r"let /b/t : /b/Q =
    #{node = Variant}
    /sys/Bool/True/qed();
",
        )
        .expect("the unit claiming `/b` builds");

    let stepped = history.step("a", Written::CoreText, &declared("/sys/Bool/False()"));

    assert!(
        matches!(
            &stepped,
            Err(Stopped { mount, why: Unbuilt::Unmade(Unmade::Kernel(verdict)) })
                if mount == "b"
                    && verdict.name == Some(named("b/t"))
                    && matches!(verdict.error, Error::Mismatch { .. })
        ),
        "{stepped:?}"
    );
}

/// `/lib/Q`, the proposition `q` spells, `/lib/t`, which proves it, and, where `tested`, `/lib/check`, a test that proves it too: what the unit's two kinds differ by.
fn kinds(q: &str, tested: bool) -> String {
    let check = match tested {
        true => {
            r"#{kind = Test}
let /lib/check : /lib/Q =
    #{node = Variant}
    /sys/Bool/True/qed();
"
        }
        false => "",
    };

    fill!(
        r"let /lib/Q : Prop =
    #{node = InductType}
    ${q};
${check}let /lib/t : /lib/Q =
    #{node = Variant}
    /sys/Bool/True/qed();
",
        q,
        check,
    )
}

/// A unit compiled for one kind is the baseline of its text compiled for the other ([Cached verdicts](../../../documentation/design/soundness/cached-verdicts.md)): `/lib` filed tested with `Q` true, then compiled built, without its test and with `Q` false, over that slot. `t`'s terms are the slot's and it read `Q`'s body, so it is judged again and refused.
#[test]
fn a_proof_over_its_unit_compiled_for_the_other_kind_is_judged_again_where_what_it_read_moved() {
    let directory = Temporary::new("history", "kinds");
    let file = directory.join("lib.crsc");
    let written = |text: &str| {
        fs::create_dir_all(&directory).expect("the directory is made");
        fs::write(&file, text).expect("the text is written");

        Source::read(&file).expect("the text reads back")
    };
    let store = || Verdicts::alone(directory.join("project"));

    let tested = written(&kinds("/sys/Bool/True()", true));
    let tested_root = RootSource::core_text("lib", &file, &tested).tested();
    with_prelude(|prelude| {
        super::read(
            &store(),
            Predecessors::over(prelude),
            &UnitSource::mounted(&tested_root),
            &tested.text,
        )
    })
    .map(|_| ())
    .expect("the tested unit builds");

    let built = written(&kinds("/sys/Bool/False()", false));
    let built_root = RootSource::core_text("lib", &file, &built);
    let verdicts = store();
    assert!(
        verdicts
            .baseline(&UnitSource::mounted(&built_root), None)
            .is_some(),
        "the slot filed tested is offered to the text compiled built"
    );
    let refused = with_prelude(|prelude| {
        super::read(
            &verdicts,
            Predecessors::over(prelude),
            &UnitSource::mounted(&built_root),
            &built.text,
        )
    })
    .err();

    assert!(
        matches!(
            &refused,
            Some(Unbuilt::Unmade(Unmade::Kernel(verdict)))
                if verdict.name == Some(named("lib/t")) && matches!(verdict.error, Error::Mismatch { .. })
        ),
        "{refused:?}"
    );
}

/// `/lib/Q`, the proposition `q` spells, `/lib/t`, which proves it, and `/lib/filler`, the proposition `filler` spells.
fn beside_filler(q: &str, filler: &str) -> String {
    fill!(
        r"let /lib/Q : Prop =
    #{node = InductType}
    ${q};
let /lib/t : /lib/Q =
    #{node = Variant}
    /sys/Bool/True/qed();
let /lib/filler : Prop =
    #{node = InductType}
    ${filler};
",
        q,
        filler,
    )
}

/// A baseline that was itself compiled over a baseline answers by the record its walk revised: `filler` edited, so the second unit adopts `t` from the first and files the first's record of what `t` read; then `Q` made false, and `t` is judged again by that record.
#[test]
fn a_proof_over_a_unit_compiled_over_a_baseline_is_judged_again_where_what_it_read_moved() {
    let mut history = History::new();
    history
        .step(
            "lib",
            Written::CoreText,
            &beside_filler("/sys/Bool/True()", "/sys/Bool/True()"),
        )
        .expect("the unit builds");
    history
        .step(
            "lib",
            Written::CoreText,
            &beside_filler("/sys/Bool/True()", "/sys/Bool/False()"),
        )
        .expect("the unit builds over its first text");

    let stepped = history.step(
        "lib",
        Written::CoreText,
        &beside_filler("/sys/Bool/False()", "/sys/Bool/False()"),
    );

    assert!(
        matches!(
            &stepped,
            Err(Stopped { why: Unbuilt::Unmade(Unmade::Kernel(verdict)), .. })
                if verdict.name == Some(named("lib/t")) && matches!(verdict.error, Error::Mismatch { .. })
        ),
        "{stepped:?}"
    );
}

/// A cache that keeps in its session what its fold compiles and files nothing, as a language server's question keeps a unit it compiled from a text it does not file.
struct Keeping<'a>(&'a Verdicts);

impl Cache for Keeping<'_> {
    fn get(&self, source: &UnitSource<'_>) -> Option<Unit> {
        Cache::get(self.0, source)
    }

    fn baseline(&self, source: &UnitSource<'_>, offered: Option<&Unit>) -> Option<Earlier> {
        Cache::baseline(self.0, source, offered)
    }

    fn put(&self, source: &UnitSource<'_>, unit: &Unit) {
        self.0.keep(source, unit);
    }
}

/// The library a session compiles twice: `Q`, true, `t`, which proves it, and `filler`, which `edited` makes false, so the second compile is over what the session kept of the first.
fn session_library(edited: bool) -> String {
    let filler = match edited {
        true => "False",
        false => "True",
    };

    fill!(
        "use /std/Bool/{False, True};\n\npub let Q: Prop = True;\n\npub let t: Q = True/qed();\n\npub let filler: Prop = ${filler};\n",
        filler
    )
}

/// A unit a session kept across its questions, itself a recompile and so a version of the unit before it, is the baseline of a module made of it with `Q` replaced, past the elaborator: the walk compares the two by what they do not share, finds `Q` replaced and judges `t`, which unfolded `Q`, again. [Cached verdicts](../../../documentation/design/soundness/cached-verdicts.md): "A language server's baseline may itself be a recompile (`curios-verdict`'s `Session`), which extends by induction".
///
/// Mutation-check to make: with `Changed::of` asking no item `Items::changed_since` reports whether it is the baseline's, so `items` is empty where the module is a version of the baseline, the module is admitted.
#[test]
fn a_proof_over_a_unit_a_session_kept_is_judged_again_where_what_it_read_moved() {
    let directory = Temporary::new("history", "session");
    let file = directory.join("lib.crs");
    let root = || {
        RootSource::mounted(
            "lib",
            RootKind::Ordinary,
            &file,
            RootSource::namespace_of(&file),
        )
    };
    let session = Session::default();
    let asked = |edited: bool| {
        fs::create_dir_all(&directory).expect("the directory is made");
        fs::write(&file, session_library(edited)).expect("the library is written");
        let root = root();
        let verdicts = Verdicts::kept_by(session.clone());
        with_prelude(|prelude| {
            compile_units(
                DEFAULT_BUDGET,
                Predecessors::over(prelude),
                &SYNTAX,
                &[(UnitSource::mounted(&root), None)],
                Some(&Keeping(&verdicts)),
                |_| {},
            )
            .map(|_| ())
        })
        .expect("the library builds");

        verdicts
    };
    asked(false);
    let verdicts = asked(true);
    let root = root();
    let source = UnitSource::mounted(&root);
    let earlier = verdicts
        .kept(&source)
        .expect("the session kept the library");
    let kept = &earlier.unit;
    assert!(
        kept.core().items.is_version(),
        "the kept unit is a recompile's, a version of the one before it"
    );
    let place = kept
        .places()
        .place(&named("lib/Q"))
        .expect("the kept unit declares `Q`")
        .item();
    let mut items = kept.core().items.version();

    let refused = with_prelude(|prelude| {
        let predecessors = Predecessors::over(prelude);
        let text = r"let /lib/Q : Prop =
    #{node = InductType}
    /sys/Bool/False();
"
        .parse::<CoreFile>()
        .expect("the item parses");
        let (replacing, _) = to_core(&text, Some(&FormScope::over(SYNTAX, predecessors.cores())))
            .expect("the item reads over the prelude");
        items.replace(place, replacing.items[0].clone());

        compile_read(
            DEFAULT_BUDGET,
            predecessors,
            source.claims(),
            (Definitions { items }, ForeignStore::new()),
            Some(&earlier),
        )
        .err()
    });

    assert!(
        matches!(
            &refused,
            Some(Unmade::Kernel(verdict))
                if verdict.name == Some(named("lib/t")) && matches!(verdict.error, Error::Mismatch { .. })
        ),
        "{refused:?}"
    );
}
