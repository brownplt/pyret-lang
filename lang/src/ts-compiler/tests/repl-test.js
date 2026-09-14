// REPL-library tests (node:test), driven by the REAL in-process load-lib runtime.
//   node --test src/ts-compiler/tests/repl-test.js     (or `make ts-repl-test`)
// (node:test rather than jest because the in-process runtime needs requirejs,
// which can't load inside a jest worker.)
//
// Exercises repl.ts end to end: provide rewriting, globals chaining across
// interactions, realm threading, compile-error reporting, and that a failed
// interaction does not drop earlier bindings. Asserts actual computed values.
// Needs a warm builtin cache in tests/ts-compiled; `make ts-repl-test` warms it.

const path = require('path');
const { describe, test, before } = require('node:test');
const assert = require('node:assert/strict');

const OUT = path.join(__dirname, '..', '..', '..', 'build', 'ts-compiler');
const REPL = require(path.join(OUT, 'repl.js'));
const CS = require(path.join(OUT, 'compile-structs.js'));
const { makeHost } = require('./in-process-host.js');

describe('repl (in-process load-lib)', () => {
  let host, X, repl, defs;
  const opts = { ...CS.defaultCompileOptions, checks: 'none', displayProgress: false };

  before(async () => {
    host = await makeHost({ quiet: true });
    X = host.executor;
    repl = REPL.makeRepl(X, host.modules, host.realm, host.context, host.makeFinder);
    defs = repl.makeDefinitionsLocator(
      () => 'x = 10\nfun sq(n :: Number): n * n end\n', CS.standardGlobals);
  });

  function num(rr) {
    assert.equal(rr.$name, 'right');
    assert.equal(X.isSuccessResult(rr.v), true);
    return X.runtimeOf(rr.v).num_to_string(X.getAnswer(rr.v));
  }

  // These run in order and share repl state (the chaining is the point).
  test('definitions compile and run', async () => {
    const r0 = await repl.restartInteractions(defs, opts);
    assert.equal(r0.$name, 'right');
    assert.equal(X.isSuccessResult(r0.v), true);
  });

  test('interaction sees definitions (x, sq): sq(10) === 100', async () => {
    const r = await repl.runInteraction(repl.makeInteractionLocator(() => 'sq(x)\n'));
    assert.equal(num(r), '100');
  });

  test('interaction sees both definitions and a prior interaction: 110', async () => {
    await repl.runInteraction(repl.makeInteractionLocator(() => 'y = sq(x)\n'));
    const r = await repl.runInteraction(repl.makeInteractionLocator(() => 'y + x\n'));
    assert.equal(num(r), '110');
  });

  test('unbound name is a compile error (left)', async () => {
    const r = await repl.runInteraction(repl.makeInteractionLocator(() => 'no-such-name-xyz + 1\n'));
    assert.equal(r.$name, 'left');
    const rendered = JSON.stringify(r.v.map((e) =>
      (e.problems ? e.problems.map((p) => p.$name) : e.$name)));
    assert.match(rendered, /unbound/);
  });

  test('chain continues after a failed interaction; the failure did not drop y: 200', async () => {
    const r = await repl.runInteraction(repl.makeInteractionLocator(() => 'y * 2\n'));
    assert.equal(num(r), '200');
  });

  test('restart clears interaction scope (y unbound again)', async () => {
    const r5 = await repl.restartInteractions(defs, opts);
    assert.equal(r5.$name, 'right');
    const r6 = await repl.runInteraction(repl.makeInteractionLocator(() => 'y\n'));
    assert.equal(r6.$name, 'left');
  });

  // A var's annotation travels on its box (checkVarAssign), so a later
  // interaction's store is checked without the annotation being in scope
  // there, and a failed store leaves the var as it was.
  test('var annotation is checked from a later interaction; a failed store keeps the value', async () => {
    const r7 = await repl.runInteraction(repl.makeInteractionLocator(() => 'var count :: Number = 5\n'));
    assert.equal(r7.$name, 'right');
    assert.equal(X.isSuccessResult(r7.v), true);
    const r8 = await repl.runInteraction(repl.makeInteractionLocator(() => 'count := "no"\n'));
    assert.equal(r8.$name, 'right');
    assert.equal(X.isSuccessResult(r8.v), false);
    assert.equal(num(await repl.runInteraction(repl.makeInteractionLocator(() => 'count\n'))), '5');
    const r9 = await repl.runInteraction(repl.makeInteractionLocator(() => 'count := 6\n'));
    assert.equal(X.isSuccessResult(r9.v), true);
    assert.equal(num(await repl.runInteraction(repl.makeInteractionLocator(() => 'count\n'))), '6');
  });
});
