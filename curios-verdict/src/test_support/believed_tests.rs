//! What a later walk believes of a unit an earlier walk filed — a unit in scope, a unit compiled over its earlier step — each attack a history the certifier refuses where it holds.

use {
    super::*,
    crate::*,
    curios_cert::{Error, Verdict},
    curios_common::{Qualifier, RootKind, fill},
    curios_core::{Entrypoint, Global, Program, Term, Universes},
    curios_pipeline::{Earlier, test_support::*},
};

/// The declaration `/<path>`.
fn named(path: &str) -> Global {
    Global::Authored(Qualifier::from(path.split('/')))
}

/// What the certifier says of `proof`, Core text closing with a term, put at `/std/Bool/False` over the prelude and every unit `history` built, through the walk a compilation puts a program to.
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

            recheck(&program, DEFAULT_BUDGET, predecessors, &SYNTAX)
        })
        .expect("the history builds")
}

/// Whether `verdicts` refuse the closing term as reaching what is not known to terminate.
fn refused_as_partial(verdicts: &[Verdict]) -> bool {
    verdicts
        .iter()
        .any(|verdict| verdict.name.is_none() && matches!(verdict.error, Error::NotTotal { .. }))
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

/// A group that leaves a sort to its caller is filed under its own spelling with what every assignment reached, so a later walk reading it at `Prop`, where its field is a proof, reads it partial over its unit's record.
///
/// Mutation-check: have `check_rec_group` file the group under its own spelling `Total` whatever its assignments reached, and the proof is admitted at `False`.
#[test]
fn a_group_generic_in_a_sort_filed_partial_is_read_partial_at_prop() {
    let mut history = History::new();
    history
        .step(
            "lib",
            WrittenIn::Program,
            PARTIAL_AT_A_SORT_LEFT_TO_A_CALLER,
        )
        .expect("the library builds");

    let verdicts = put(&history, PROJECTED_AT_PROP);

    assert!(refused_as_partial(&verdicts), "{verdicts:?}");
}

/// A family holding a type of its own level, which `/down` takes a level lower: `Big` compares invariantly over its record as filed, so the instance is refused.
#[test]
fn a_large_familys_instance_is_not_taken_a_level_down() {
    let mut history = History::new();
    history
        .step(
            "lib",
            WrittenIn::CoreText,
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
        .step("lib", WrittenIn::CoreText, &unit(0))
        .expect("the unit builds");

    let stepped = history.step("lib", WrittenIn::CoreText, &unit(1));

    assert!(
        matches!(
            &stepped,
            Err(Stopped { why: Unbuilt::Unmade(Unmade::Certifier(verdict)), .. })
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
        .step("lib", WrittenIn::Program, &lib(0))
        .expect("the library builds");
    history
        .step(
            "mid",
            WrittenIn::Program,
            "use /std/{Nat};\nuse /lib/{c};\n\npub let m: Nat = c;\n",
        )
        .expect("the middle unit builds");
    history
        .step(
            "app",
            WrittenIn::CoreText,
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

    let stepped = history.step("lib", WrittenIn::Program, &lib(1));

    assert!(
        matches!(
            &stepped,
            Err(Stopped { mount, why: Unbuilt::Unmade(Unmade::Certifier(verdict)) })
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
        #{goal = P}
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
        .step("lib", WrittenIn::CoreText, &verdict_moved(false))
        .expect("the unit builds");

    let stepped = history.step("lib", WrittenIn::CoreText, &verdict_moved(true));

    assert!(
        matches!(
            &stepped,
            Err(Stopped { why: Unbuilt::Unmade(Unmade::Certifier(verdict)), .. })
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
            goal = 
                #{node = InductType}
                /lib/Q()(t)}
        match t
        | a() =>
            /lib/Q/qa()
        end;
",
        cases,
    )
}

/// Adopted, `f(T/b())` would be a `Q(T/b())`, which has no case. Mutation-checked: with `Filed::entries_alike` answering alike whatever the entries, the step builds.
#[test]
fn a_match_over_a_family_an_edit_gave_a_case_is_judged_again() {
    let mut history = History::new();
    history
        .step("lib", WrittenIn::CoreText, &covered("| a()\n"))
        .expect("the unit builds");

    let stepped = history.step("lib", WrittenIn::CoreText, &covered("| a()\n| b()\n"));

    assert!(
        matches!(
            &stepped,
            Err(Stopped { why: Unbuilt::Unmade(Unmade::Certifier(verdict)), .. })
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
            WrittenIn::CoreText,
            &q_and_t("/sys/Bool/True()", false),
        )
        .expect("the unit builds");

    let stepped = history.step(
        "lib",
        WrittenIn::CoreText,
        &q_and_t("/sys/Bool/False()", true),
    );

    assert!(
        matches!(
            &stepped,
            Err(Stopped { why: Unbuilt::Unmade(Unmade::Certifier(verdict)), .. })
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
            WrittenIn::CoreText,
            &unit(
                ("/lib/Q", "/sys/Bool/True()"),
                ("/lib/R", "/sys/Bool/False()"),
            ),
        )
        .expect("the unit builds");

    let stepped = history.step(
        "lib",
        WrittenIn::CoreText,
        &unit(
            ("/lib/R", "/sys/Bool/True()"),
            ("/lib/Q", "/sys/Bool/False()"),
        ),
    );

    assert!(
        matches!(
            &stepped,
            Err(Stopped { why: Unbuilt::Unmade(Unmade::Certifier(verdict)), .. })
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
        .step("a", WrittenIn::CoreText, &declared("/sys/Bool/True()"))
        .expect("the unit claiming `/a` builds");
    history
        .step(
            "b",
            WrittenIn::CoreText,
            r"let /b/t : /b/Q =
    #{node = Variant}
    /sys/Bool/True/qed();
",
        )
        .expect("the unit claiming `/b` builds");

    let stepped = history.step("a", WrittenIn::CoreText, &declared("/sys/Bool/False()"));

    assert!(
        matches!(
            &stepped,
            Err(Stopped { mount, why: Unbuilt::Unmade(Unmade::Certifier(verdict)) })
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

        text.to_string()
    };
    let store = || Verdicts::alone(directory.join("project"));

    let tested = written(&kinds("/sys/Bool/True()", true));
    let tested_root = RootSource::one_file("lib", &file, &tested).tested();
    with_prelude(|prelude| {
        super::read(
            &store(),
            Predecessors::over(prelude),
            &UnitSource::mounted(&tested_root),
            &tested,
        )
    })
    .map(|_| ())
    .expect("the tested unit builds");

    let built = written(&kinds("/sys/Bool/False()", false));
    let built_root = RootSource::one_file("lib", &file, &built);
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
            &built,
        )
    })
    .err();

    assert!(
        matches!(
            &refused,
            Some(Unbuilt::Unmade(Unmade::Certifier(verdict)))
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
            WrittenIn::CoreText,
            &beside_filler("/sys/Bool/True()", "/sys/Bool/True()"),
        )
        .expect("the unit builds");
    history
        .step(
            "lib",
            WrittenIn::CoreText,
            &beside_filler("/sys/Bool/True()", "/sys/Bool/False()"),
        )
        .expect("the unit builds over its first text");

    let stepped = history.step(
        "lib",
        WrittenIn::CoreText,
        &beside_filler("/sys/Bool/False()", "/sys/Bool/False()"),
    );

    assert!(
        matches!(
            &stepped,
            Err(Stopped { why: Unbuilt::Unmade(Unmade::Certifier(verdict)), .. })
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
                Listeners::default(),
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
            Some(Unmade::Certifier(verdict))
                if verdict.name == Some(named("lib/t")) && matches!(verdict.error, Error::Mismatch { .. })
        ),
        "{refused:?}"
    );
}

/// `t` reads `Q`, and a step stores the two the other way round: a walk over every declaration meets `t` before `Q` is declared and refuses it, and so does a walk over the baseline, which held the two in the order `t` read them. Mutation-checked: with `Changed::of` visiting no held declaration stored ahead of what it read, the second step builds.
#[test]
fn a_declaration_stored_ahead_of_what_it_reads_is_refused_over_its_baseline() {
    let ordered = "\
let /lib/Q : Prop =
    #{node = InductType} /sys/Bool/True();
let /lib/t : /lib/Q =
    /sys/Bool/True/qed();
";
    let reordered = "\
let /lib/t : /lib/Q =
    /sys/Bool/True/qed();
let /lib/Q : Prop =
    #{node = InductType} /sys/Bool/True();
";
    let undeclared = |stepped: &Result<(), Stopped>| {
        matches!(
            stepped,
            Err(Stopped { why: Unbuilt::Unmade(Unmade::Certifier(verdict)), .. })
                if verdict.name == Some(named("lib/t")) && matches!(verdict.error, Error::Undeclared(_))
        )
    };

    let mut whole = History::new();
    let alone = whole.step("lib", WrittenIn::CoreText, reordered);
    assert!(undeclared(&alone), "{alone:?}");

    let mut history = History::new();
    history
        .step("lib", WrittenIn::CoreText, ordered)
        .expect("the library builds");
    let stepped = history.step("lib", WrittenIn::CoreText, reordered);
    assert!(undeclared(&stepped), "{stepped:?}");
}

/// `/u/A` is indexed by `/u/V`, and a step makes `/u/V` hold an `/u/A`: a walk over every declaration refuses `/u/A` as indexed by a family that holds it, and so does a walk over the baseline, though only `/u/V`'s terms moved, since what `/u/A`'s domains reach back is what the items around it now hold. Mutation-checked: with the cycle asked only of the items whose terms are not the baseline's, the second step builds.
#[test]
fn a_family_a_step_makes_its_index_hold_is_refused_over_its_baseline() {
    let holding = |wrap: &str| {
        fill!(
            r"induct /u/A: (v: #{node = InductType} /u/V()) -> pub Prop
| mk(): (#{node = Variant} /u/V/base())
end
induct /u/V: pub Prop
| base()${wrap}
end
",
            wrap = wrap,
        )
    };
    let wrapping = "\n| wrap(x: #{node = InductType} /u/V(), s: #{node = InductType} /u/A()(x))";
    let indexed = |stepped: &Result<(), Stopped>| {
        matches!(
            stepped,
            Err(Stopped { why: Unbuilt::Unmade(Unmade::Certifier(verdict)), .. })
                if verdict.name == Some(named("u/A")) && matches!(verdict.error, Error::IndexedByItsGroup { .. })
        )
    };

    let mut whole = History::new();
    let alone = whole.step("u", WrittenIn::CoreText, &holding(wrapping));
    assert!(indexed(&alone), "{alone:?}");

    let mut history = History::new();
    history
        .step("u", WrittenIn::CoreText, &holding(""))
        .expect("the library builds");
    let stepped = history.step("u", WrittenIn::CoreText, &holding(wrapping));
    assert!(indexed(&stepped), "{stepped:?}");
}

/// A family whose one edit is the variance vector it carries, now claiming an irrelevance its telescope denies: a walk over every declaration refuses the claim where the family is declared, and so does a walk over the baseline, which held the family carrying another vector. Mutation-checked: with `induct_alike` reading no carried vector, the second step builds.
#[test]
fn a_family_stepped_to_a_variance_its_telescope_denies_is_refused_over_its_baseline() {
    let carrying = |variance: &str| {
        fill!(
            r"#{universe_context = {parameter_count = 1}, variances = [${variance}]}
induct /u/Box: #{level = u + 1} Type
| box(T: #{level = u} Type)
end
",
            variance = variance,
        )
    };
    let denied = |stepped: &Result<(), Stopped>| {
        matches!(
            stepped,
            Err(Stopped { why: Unbuilt::Unmade(Unmade::Certifier(verdict)), .. })
                if verdict.name == Some(named("u/Box")) && matches!(verdict.error, Error::VarianceDenied { .. })
        )
    };

    let mut whole = History::new();
    let alone = whole.step("u", WrittenIn::CoreText, &carrying("Irrelevant"));
    assert!(denied(&alone), "{alone:?}");

    let mut history = History::new();
    history
        .step("u", WrittenIn::CoreText, &carrying("Invariant"))
        .expect("the family builds");
    let stepped = history.step("u", WrittenIn::CoreText, &carrying("Irrelevant"));
    assert!(denied(&stepped), "{stepped:?}");
}

/// [`a_family_stepped_to_a_variance_its_telescope_denies_is_refused_over_its_baseline`] for a record, whose entry `struct_alike` compares. Mutation-checked: with `struct_alike` reading no carried vector, the second step builds.
#[test]
fn a_record_stepped_to_a_variance_its_fields_deny_is_refused_over_its_baseline() {
    let carrying = |variance: &str| {
        fill!(
            r"#{universe_context = {parameter_count = 1}, variances = [${variance}]}
struct /u/R: #{level = u + 1} Type { field: #{level = u} Type }
",
            variance = variance,
        )
    };
    let denied = |stepped: &Result<(), Stopped>| {
        matches!(
            stepped,
            Err(Stopped { why: Unbuilt::Unmade(Unmade::Certifier(verdict)), .. })
                if verdict.name == Some(named("u/R")) && matches!(verdict.error, Error::VarianceDenied { .. })
        )
    };

    let mut whole = History::new();
    let alone = whole.step("u", WrittenIn::CoreText, &carrying("Irrelevant"));
    assert!(denied(&alone), "{alone:?}");

    let mut history = History::new();
    history
        .step("u", WrittenIn::CoreText, &carrying("Invariant"))
        .expect("the record builds");
    let stepped = history.step("u", WrittenIn::CoreText, &carrying("Irrelevant"));
    assert!(denied(&stepped), "{stepped:?}");
}

/// The carrier the summary histories below build on: `N`, whose successor is a constructor, so a call that unfolds to its own successor converts with `N/s` of itself in one step, and `no_fix`, which takes an `n` equal to its own successor to `False`.
const SUCCESSOR: &str = "\
use /std/{Nat, Eq, proved};
use /std/Bool/{False, Holds};

pub induct N: pub Type
| z()
| s(p: N)
end

pub let depth(n: N) -> Nat =
    match n
    | z() => 0
    | s(p) => depth(p) + 1
    end;

pub let no_fix_nat(n: Nat, e: Eq()(n, n + 1)) -> False =
    let h: Holds(n + 1 < n + 1) = Eq/subst((m: Nat) => Holds(m < n + 1), e, proved());
    proved(@False);

pub let no_fix(n: N, e: Eq()(n, N/s(n))) -> False =
    no_fix_nat(depth(n), Eq/cong(depth, e));
";

/// `/lib/apply` at one type either way: applying `k` below `x` where `below`, and otherwise at `x` itself where `x` is `0`, so its record files `k` applied below `x` or at it.
fn applying(below: bool) -> &'static str {
    match below {
        true => {
            "\
use /std/{Nat};
use /base/{N};

pub let apply(x: Nat, k: (Nat) -> N) -> N =
    match x
    | 0 => N/z()
    | n + 1 => match apply(n, k) | z() => k(n) | s(_) => k(n) end
    end;
"
        }
        false => {
            "\
use /std/{Nat};
use /base/{N};

pub let apply(x: Nat, k: (Nat) -> N) -> N =
    match x
    | 0 => k(0)
    | n + 1 => apply(n, k)
    end;
"
        }
    }
}

/// `/app/loop`, which recurses through the callback `/lib/apply` is handed: total while `apply` applies `k` below `x`, and `loop(0)` is `N/s(loop(0))` once it applies `k` at `0`.
const LOOPING_THROUGH_APPLY: &str = "\
use /std/{Nat};
use /base/{N};
use /lib/{apply};

pub let loop(x: Nat) -> N =
    apply(x, (y) => N/s(loop(y)));
";

/// A proof that `/<unit>/loop` at `at` is no fixed point of `N/s`: refused for its equation where `loop` terminates, and admitted at `False` were `loop` read total where `loop(at)` unfolds to `N/s(loop(at))`.
fn no_fix_of_loop(unit: &str, at: &str) -> String {
    fill!(
        r"
/base/no_fix(
    /${unit}/loop(${at}),
    (#{universes = {levels = [0], qualities = [Type]}} /std/Eq/Eq/refl)(
        @#{node = InductType} /base/N(),
        @/${unit}/loop(${at})))
",
        unit,
        at,
    )
}

/// Whether `verdicts` refuse the closing term for its type alone: a mismatch there, and nothing it reaches read as not known to terminate.
fn refused_for_its_equation_alone(verdicts: &[Verdict]) -> bool {
    verdicts
        .iter()
        .any(|verdict| verdict.name.is_none() && matches!(verdict.error, Error::Mismatch { .. }))
        && !refused_as_partial(verdicts)
}

/// `loop` is graded through what `/lib`'s record files of how `apply` applies `k`: below `x`, so `loop` is total and `loop(0)` is `N/z()`. Edited at the same type to apply `k` at `0` itself, `apply` moves its summary and nothing else `loop` reads of it, so `/app`, compiled over its baseline, keeps `loop` total only where a read of `apply`'s signature digests no summary. Stepped back and forth, `/app` finds `loop`'s verdict among the records the store filed of its item, one under each summary, and takes the one filed under the summary `/lib` files now. Mutation-check: leave `self.applications` out of the signature part `Readable::digest` archives (`curios-cert/src/baseline.rs`), and the proof over a library last stepped to apply `k` at `0` is admitted at `False`.
#[test]
fn a_recursion_through_a_callees_summary_is_judged_again_where_the_callee_moves_in_another_unit() {
    for steps in [
        &[true][..],
        &[true, false],
        &[true, false, true],
        &[true, false, true, false],
    ] {
        let (first, later) = steps.split_first().expect("a step of the library");
        let mut history = History::new();
        history
            .step("base", WrittenIn::Program, SUCCESSOR)
            .expect("the carrier builds");
        history
            .step("lib", WrittenIn::Program, applying(*first))
            .expect("the library builds");
        history
            .step("app", WrittenIn::Program, LOOPING_THROUGH_APPLY)
            .expect("the recursion builds");
        for below in later {
            history
                .step("lib", WrittenIn::Program, applying(*below))
                .expect("the edited library builds");
        }

        let verdicts = put(&history, &no_fix_of_loop("app", "0"));

        match steps.last() == Some(&true) {
            true => assert!(
                refused_for_its_equation_alone(&verdicts),
                "{steps:?}: {verdicts:?}"
            ),
            false => assert!(refused_as_partial(&verdicts), "{steps:?}: {verdicts:?}"),
        }
    }
}

/// `/lib/apply` at one type either way, applying the field of `k` where [`applying`] applies `k` itself: below `x` where `below`, and otherwise at `x` itself where `x` is `0`.
fn applying_field(below: bool) -> &'static str {
    match below {
        true => {
            "\
use /std/{Nat};
use /base/{N};

pub let apply(x: Nat, k: {(Nat) -> N}) -> N =
    match x
    | 0 => N/z()
    | n + 1 => match apply(n, k) | z() => k.0(n) | s(_) => k.0(n) end
    end;
"
        }
        false => {
            "\
use /std/{Nat};
use /base/{N};

pub let apply(x: Nat, k: {(Nat) -> N}) -> N =
    match x
    | 0 => k.0(0)
    | n + 1 => apply(n, k)
    end;
"
        }
    }
}

/// `/app/loop`, which recurses through the field of the record `/lib/apply` is handed, as [`LOOPING_THROUGH_APPLY`] recurses through the callback itself.
const LOOPING_THROUGH_APPLY_FIELD: &str = "\
use /std/{Nat};
use /base/{N};
use /lib/{apply};

pub let loop(x: Nat) -> N =
    apply(x, ((y) => N/s(loop(y)),));
";

/// The history above with the callback held at a record's field: what `/lib`'s record files of `k` is how `apply` applies `k.0`, so an edit moving that alone moves the summary `loop` was graded through. Mutation-check: as for the history above.
#[test]
fn a_recursion_through_a_callees_summary_of_a_field_is_judged_again_where_the_callee_moves() {
    for steps in [
        &[true][..],
        &[true, false],
        &[true, false, true],
        &[true, false, true, false],
    ] {
        let (first, later) = steps.split_first().expect("a step of the library");
        let mut history = History::new();
        history
            .step("base", WrittenIn::Program, SUCCESSOR)
            .expect("the carrier builds");
        history
            .step("lib", WrittenIn::Program, applying_field(*first))
            .expect("the library builds");
        history
            .step("app", WrittenIn::Program, LOOPING_THROUGH_APPLY_FIELD)
            .expect("the recursion builds");
        for below in later {
            history
                .step("lib", WrittenIn::Program, applying_field(*below))
                .expect("the edited library builds");
        }

        let verdicts = put(&history, &no_fix_of_loop("app", "0"));

        match steps.last() == Some(&true) {
            true => assert!(
                refused_for_its_equation_alone(&verdicts),
                "{steps:?}: {verdicts:?}"
            ),
            false => assert!(refused_as_partial(&verdicts), "{steps:?}: {verdicts:?}"),
        }
    }
}

/// `fwd`, `apply` and `loop` in one unit: `apply` applies `k` to `fwd(p)` in the arm that takes `x` to `s(p)`, `fwd` the identity or the successor.
fn forwarding(identity: bool) -> String {
    let body = match identity {
        true => "n",
        false => "N/s(n)",
    };

    fill!(
        "\
use /base/{N};

pub let fwd(n: N) -> N =
    ${body};

pub let apply(x: N, k: (N) -> N) -> N =
    match x
    | z() => N/z()
    | s(p) => match apply(p, k) | z() => k(fwd(p)) | s(_) => k(fwd(p)) end
    end;

pub let loop(x: N) -> N =
    apply(x, (y) => N/s(loop(y)));
",
        body,
    )
}

/// What `apply`'s record files of `k` is what its grader unfolds `fwd(p)` to: `p`, below `x`, while `fwd` is the identity, and `x` itself once `fwd` rebuilds the successor, where `loop(N/s(N/z()))` is `N/s` of itself. The edit moves `fwd`'s body alone, so `apply`, whose terms are the baseline's, is judged again for the body its grader read, and `loop`, whose terms are the baseline's too, for the summary it read of `apply`. Mutation-check: as for the history across units above, and the proof over the edited unit is admitted.
#[test]
fn a_recursion_through_a_summary_is_judged_again_where_what_the_callees_grader_unfolded_moves() {
    for edited in [false, true] {
        let mut history = History::new();
        history
            .step("base", WrittenIn::Program, SUCCESSOR)
            .expect("the carrier builds");
        history
            .step("one", WrittenIn::Program, &forwarding(true))
            .expect("the unit builds");
        if edited {
            history
                .step("one", WrittenIn::Program, &forwarding(false))
                .expect("the edited unit builds");
        }

        let verdicts = put(&history, &no_fix_of_loop("one", "/base/N/s(/base/N/z())"));

        match edited {
            false => assert!(refused_for_its_equation_alone(&verdicts), "{verdicts:?}"),
            true => assert!(refused_as_partial(&verdicts), "{verdicts:?}"),
        }
    }
}

/// `/base` with a witness of `Down(N)` whose method answers `n` as first written, and `N/s(n)` once edited.
fn descending(edited: bool) -> String {
    let body = match edited {
        false => "n",
        true => "N/s(n)",
    };

    fill!(
        "${carrier}
pub concept Down(T: Type): pub Type {
    down(T) -> T,
}

satisfy Down(N) {
    down(n) = ${body},
}
",
        carrier = SUCCESSOR,
        body,
    )
}

/// `/base` with `each`, which applies `k` below `x` as first written, and at `x` itself once edited.
fn iterating(edited: bool) -> String {
    let at = match edited {
        false => "p",
        true => "x",
    };

    fill!(
        "${carrier}
pub let each(x: N, k: (N) -> N) -> N =
    match x
    | z() => N/z()
    | s(p) => match each(p, k) | z() => k(${at}) | s(_) => k(${at}) end
    end;
",
        carrier = SUCCESSOR,
        at,
    )
}

/// `apply` applies `k` to `Down/down(p)` in the arm that takes `x` to `s(p)`, which its grader reads through the witness the call resolved.
const APPLYING_THROUGH_A_WITNESS: &str = "\
use /base/{N, Down};

pub let apply(x: N, k: (N) -> N) -> N =
    match x
    | z() => N/z()
    | s(p) => match apply(p, k) | z() => k(Down/down(p)) | s(_) => k(Down/down(p)) end
    end;
";

/// `apply` applies `k` in a lambda it hands `each`, whose binder stands to `x` as `each`'s summary says each application of its callback does.
const APPLYING_THROUGH_EACH: &str = "\
use /base/{N, each};

pub let apply(x: N, k: (N) -> N) -> N =
    match x
    | z() => N/z()
    | s(p) => match apply(p, k) | z() => each(x, (y) => k(y)) | s(_) => each(x, (y) => k(y)) end
    end;
";

/// `loop` recursing through `/lib/apply` at `N`.
const LOOPING_THROUGH_APPLY_AT_N: &str = "\
use /base/{N};
use /lib/{apply};

pub let loop(x: N) -> N =
    apply(x, (y) => N/s(loop(y)));
";

/// What `/lib`'s record files of `apply`'s `k` is read off `/base`: through the witness `Down/down(p)` resolves to, or through `each`'s summary. Either reads `k` applied below `x` as `/base` is first written, and at `x` itself once it is edited, where `loop(N/s(N/z()))` is `N/s` of itself. Neither `/lib` nor `/app` is edited, so `apply` is judged again for what it read of `/base`, and `loop` for the summary it read of `apply`. Mutation-check: as for the history across units above, and the proof over each edited carrier is admitted.
#[test]
fn a_recursion_through_a_summary_is_judged_again_where_a_unit_the_callee_read_moves() {
    for (carrier, library) in [
        (descending as fn(bool) -> String, APPLYING_THROUGH_A_WITNESS),
        (iterating, APPLYING_THROUGH_EACH),
    ] {
        for edited in [false, true] {
            let mut history = History::new();
            history
                .step("base", WrittenIn::Program, &carrier(false))
                .expect("the carrier builds");
            history
                .step("lib", WrittenIn::Program, library)
                .expect("the library builds");
            history
                .step("app", WrittenIn::Program, LOOPING_THROUGH_APPLY_AT_N)
                .expect("the recursion builds");
            if edited {
                history
                    .step("base", WrittenIn::Program, &carrier(true))
                    .expect("the edited carrier builds");
            }

            let verdicts = put(&history, &no_fix_of_loop("app", "/base/N/s(/base/N/z())"));

            match edited {
                false => assert!(refused_for_its_equation_alone(&verdicts), "{verdicts:?}"),
                true => assert!(refused_as_partial(&verdicts), "{verdicts:?}"),
            }
        }
    }
}
