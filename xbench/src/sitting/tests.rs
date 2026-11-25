use super::*;

/// A tree with nothing to commit is one a record can name by its commit.
#[test]
fn a_clean_tree_is_one_a_sitting_takes() {
    assert_eq!(clean(""), Ok(()));
}

/// A change no commit holds, staged, unstaged or untracked, refuses the sitting, naming what changed.
#[test]
fn a_tree_holding_a_change_refuses_the_sitting() {
    for status in [
        " M xbench/src/main.rs\n",
        "A  xbench/src/runner.rs\n",
        "?? xbench/src/new.rs\n",
    ] {
        assert!(
            clean(status).is_err_and(|why| why.contains(status.trim())),
            "{status}"
        );
    }
}

/// `date -I` answers a zero-padded day, and a record holds it as TOML's local date, which refuses a day its month does not have.
#[test]
fn the_day_date_answers_is_the_day_a_record_holds() {
    assert_eq!(
        today("2026-10-07").map(|day| day.to_string()),
        Ok("2026-10-07".to_string())
    );
    assert!(today("2026-02-30").is_err());
    assert!(today("yesterday").is_err());
}
