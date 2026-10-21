# A learner is taught by what the compiler answers

What is left of `/learn-curios` (`.claude/commands/learn-curios.md`), whose shape and reasons are [its decision](../../design/tools/a-learner-is-taught-by-what-the-compiler-answers.md): the dry runs that read the command off a session, and a learner with no checkout.

## The gap

- **Nothing has read the command off a session.** What it asks is followed and not enforced: how long a turn runs, whether a fix is written for the learner, whether the logic is named before it is used, whether a step is set unclosed and whether a call is denied after the first turn are each unknown until a session is counted.

## What waits

**A learner with no checkout** waits for one the maintainer does not reach. What is known for that day: a repository is a marketplace by one file, `.claude-plugin/marketplace.json`, and is added at a ref, `owner/repo@release/<version>`; an entry whose `source` is `"."` may list one `skills` subdirectory, so one copy of the command serves the checkout and the plugin, with the reference of that tag beside it ([Marketplace reference](https://code.claude.com/docs/en/plugins/marketplace-reference.md)); a plugin's settings carry no permission, so its leave to probe is a hook ([Plugin manifest reference](https://code.claude.com/docs/en/plugins/manifest-reference.md)); and `curios document --std` writes the library's interface and no proof's body, and no command writes the language reference.

## Stages

1. **Three dry runs**, by [the decision's protocol](../../design/tools/a-learner-is-taught-by-what-the-compiler-answers.md), against the command as the release ships it. Check: the counts stated; no call denied after the first turn, no `cargo` command run, and `documentation/syntax.md` not read whole before the first message.

## Verification

Prose and agent configuration alone change, so the tree's check is `cargo xtask workspace format`.

## Completion and retirement

The counts go to the decision beside its protocol. A learner with no checkout that no one has reached by then is lifted into a specification of its own, and this file is deleted.
