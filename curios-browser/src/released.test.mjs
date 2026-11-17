// What the compiler as it is released carries of the machine that built it, under Node's own test runner beside the page's suites, which `cargo xtask compiler test` runs against the same compiler: no directory of its build — the target directory, Cargo's home, the toolchain's root, the repository's or the home they lie in — is in the compiler `CURIOS` names, nor in an executable it writes, whose launcher it embeds.

import { test } from "node:test";
import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { homedir, tmpdir } from "node:os";
import { join } from "node:path";

const compiler = process.env.CURIOS;

assert.ok(compiler, "CURIOS names the compiler under test");

/** Each directory of the machine the compiler was built on, as the step hands it over, the repository's, where the step runs, and the home every one of them usually lies in, which also holds what a build copied in from another checkout. */
const directories = [
  process.env.TARGET,
  process.env.CARGO_HOME,
  process.env.SYSROOT,
  process.cwd(),
  homedir(),
];

assert.ok(
  directories.every(Boolean),
  "TARGET, CARGO_HOME and SYSROOT name the build's directories",
);

/** The directories the file at `path` holds. */
const held = (path) => {
  const bytes = readFileSync(path).toString("latin1");

  return directories.filter((directory) => bytes.includes(directory));
};

test("the compiler holds no directory of the machine it was built on", () => {
  assert.deepEqual(held(compiler), []);
});

test("an executable the compiler writes holds no directory of the machine it was built on", () => {
  const scratch = mkdtempSync(join(tmpdir(), "curios-released-"));
  try {
    const executable = join(scratch, "hello");
    const compiled = spawnSync(
      compiler,
      ["compile", "programs/hello_world.crs", "-o", executable],
      { encoding: "utf8" },
    );
    assert.equal(compiled.status, 0, compiled.stderr);

    assert.deepEqual(held(executable), []);
  } finally {
    rmSync(scratch, { recursive: true, force: true });
  }
});
