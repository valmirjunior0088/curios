// The runner's pure pieces under Node's own test runner: which window messages it admits, and what it sends over the run's socket for an outcome. The loop over iframes runs under a document alone, which the end-to-end cases of `curios/tests/serve.rs` play the part of from the server's side, and a browser from the page's.

import { test } from "node:test";
import assert from "node:assert/strict";
import {
  NEVER_STARTED,
  admitted,
  decoded,
  encoded,
  posted,
} from "./runner.mjs";

test("a message is admitted from the opened iframe's window at the runner's own origin alone", () => {
  const window = {};
  const iframe = { contentWindow: window };
  const origin = "http://127.0.0.1:8000";

  assert.equal(admitted({ source: window, origin }, iframe, origin), true);
  assert.equal(admitted({ source: {}, origin }, iframe, origin), false);
  assert.equal(
    admitted(
      { source: window, origin: "http://localhost:8000" },
      iframe,
      origin,
    ),
    false,
  );
});

test("an outcome is sent with its streams in base64 and the rest as the page posted it", () => {
  const stdout = new TextEncoder().encode("/app/t: passed\n");
  const message = JSON.parse(
    posted({
      stdout,
      stderr: new Uint8Array(0),
      exitCode: 0,
      trap: null,
      held: true,
    }),
  );

  assert.deepEqual(message, {
    stdout: "L2FwcC90OiBwYXNzZWQK",
    stderr: "",
    exitCode: 0,
    trap: null,
    held: true,
  });
  assert.deepEqual(decoded(message.stdout), stdout);
  assert.deepEqual(
    decoded(encoded(new Uint8Array([0, 255, 128]))),
    new Uint8Array([0, 255, 128]),
  );
});

test("a page whose loader never started is reported trapped, naming the loader", () => {
  assert.equal(NEVER_STARTED.held, null);
  assert.equal(NEVER_STARTED.exitCode, null);
  assert.match(NEVER_STARTED.trap, /^the loader of index\.html did not start/);
});
