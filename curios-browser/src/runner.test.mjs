// The runner's pure pieces under Node's own test runner: which window messages it admits, what it sends over the run's socket for a write and for an outcome, and when its page follows what is appended. The loop over iframes runs under a document alone, which the end-to-end cases of `curios/tests/serve.rs` play the part of from the server's side, and a browser from the page's.

import { test } from "node:test";
import assert from "node:assert/strict";
import {
  NEVER_STARTED,
  admitted,
  decoded,
  encoded,
  following,
  posted,
  wrote,
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

test("a write is sent as it was made, in base64, and an outcome as the page posted it", () => {
  const bytes = new TextEncoder().encode("├─ /app/t; passed in 0.0s\n");
  const written = JSON.parse(wrote("stderr", bytes));

  assert.deepEqual(written, {
    wrote: {
      stream: "stderr",
      bytes: "4pSc4pSAIC9hcHAvdDsgcGFzc2VkIGluIDAuMHMK",
    },
  });
  assert.deepEqual(decoded(written.wrote.bytes), bytes);
  assert.deepEqual(
    JSON.parse(posted({ exitCode: 0, trap: null, held: true })),
    { outcome: { exitCode: 0, trap: null, held: true } },
  );
  assert.deepEqual(
    decoded(encoded(new Uint8Array([0, 255, 128]))),
    new Uint8Array([0, 255, 128]),
  );
});

test("the page follows what is appended while its reader stands at its end, and stays where they scrolled back to", () => {
  assert.equal(following(0, 600, 600), true);
  assert.equal(following(1400, 600, 2000), true);
  assert.equal(following(1390, 600, 2000), true);
  assert.equal(following(900, 600, 2000), false);
});

test("a page whose loader never started is reported trapped, naming the loader", () => {
  assert.equal(NEVER_STARTED.held, null);
  assert.equal(NEVER_STARTED.exitCode, null);
  assert.match(NEVER_STARTED.trap, /^the loader of index\.html did not start/);
});
