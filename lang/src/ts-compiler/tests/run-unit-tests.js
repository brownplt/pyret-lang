#!/usr/bin/env node
// Unit tests for the TypeScript compiler port. Run from lang/:
//   node src/ts-compiler/tests/run-unit-tests.js
// (or `make ts-unit-test`). Requires `make ts-compiler` first.

const path = require('path');
const assert = require('assert');

const OUT = path.join(__dirname, '..', '..', '..', 'build', 'ts-compiler');
function load(mod) { return require(path.join(OUT, mod)); }

let passed = 0;
let failed = 0;
function test(name, fn) {
  try {
    fn();
    passed++;
    console.log('ok   ' + name);
  } catch (e) {
    failed++;
    console.log('FAIL ' + name + ': ' + e.message);
    if (process.env.VERBOSE) console.log(e.stack);
  }
}

// ---------- pprint ----------
const PP = load('pprint.js');
test('pprint: flow reflows at width', () => {
  const words = ['This', 'is', 'a', 'sentence', 'with', 'eight', 'words']
    .map((s) => PP.str(s));
  const doc = PP.flow(words);
  assert.deepStrictEqual(doc.pretty(40), ['This is a sentence with eight words']);
  assert.deepStrictEqual(doc.pretty(30), ['This is a sentence with eight', 'words']);
  assert.deepStrictEqual(doc.pretty(10), ['This is a', 'sentence', 'with eight', 'words']);
});
test('pprint: group/nest', () => {
  const doc = PP.group(PP.str('hello').append(PP.nest(2, PP.sbreak(1).append(PP.str('world')))));
  assert.deepStrictEqual(doc.pretty(80), ['hello world']);
  assert.deepStrictEqual(doc.pretty(8), ['hello', '  world']);
});

// ---------- srcloc ----------
const SL = load('srcloc.js');
test('srcloc: format and key', () => {
  const l = new SL.Srcloc('my-file.arr', 1, 5, 4, 2, 3, 20);
  assert.strictEqual(l.format(true), 'my-file.arr:1:5-2:3');
  assert.strictEqual(l.format(false), 'line 1, column 5');
  assert.strictEqual(l.key(), 'my-file.arr:4-20');
  assert.strictEqual(new SL.Builtin('dummy location').format(true), '<builtin dummy location>');
});

// ---------- gensym ----------
const G = load('gensym.js');
test('gensym: format and reset', () => {
  G.reset();
  const a = G.makeName('tail');
  G.reset();
  const b = G.makeName('tail');
  assert.strictEqual(a, b);
  assert.match(a, /^tail/);
});

// ---------- concat-lists ----------
const CL = load('concat-lists.js');
test('concat-lists: toList order', () => {
  let l = CL.clEmpty;
  l = CL.clSnoc(l, 1);
  l = CL.clSnoc(l, 2);
  l = CL.clCons(0, l);
  assert.deepStrictEqual(l.toList(), [0, 1, 2]);
});

// ---------- js-numbers interop ----------
const jn = load('interop/js-numbers.js');
test('js-numbers: exact rational round trip', () => {
  const third = jn.jsnums.fromString('1/3');
  assert.strictEqual(String(third), '1/3');
  const sum = jn.jsnums.add(third, jn.jsnums.fromString('1/6'));
  assert.strictEqual(String(sum), '1/2');
  assert.strictEqual(String(jn.jsnums.fromString('~0')), '~0');
});

// ---------- parse-pyret ----------
const P = load('parse-pyret.js');
test('parse: basic program shape', () => {
  const prog = P.surfaceParse('x = 2 + 3\nprint(x)\n', 'file://test');
  assert.strictEqual(prog.$name, 's-program');
  assert.strictEqual(prog.block.stmts.length, 2);
  assert.strictEqual(prog.block.stmts[0].$name, 's-let');
});
test('parse: data + cases + check', () => {
  const prog = P.surfaceParse(
    'data D: | foo | bar(x :: Number) end\n' +
    'check: 1 is 1 end\n', 'file://test');
  assert.strictEqual(prog.block.stmts[0].$name, 's-data');
  assert.strictEqual(prog.block.stmts[0].variants.length, 2);
});
test('parse: parse errors are PyretParseErrors', () => {
  assert.throws(() => P.surfaceParse('fun f(: 3 end', 'file://test'));
  assert.throws(() => P.surfaceParse('x = ', 'file://test'));
});
test('parse: number literals are exact', () => {
  const prog = P.surfaceParse('x = 100000000000000000000001\n', 'file://test');
  const n = prog.block.stmts[0].value.n;
  assert.strictEqual(String(n), '100000000000000000000001');
});

// ---------- js-ast printing ----------
const J = load('js-ast.js');
const CLmod = CL;
function clist(arr) {
  let acc = CLmod.clEmpty;
  for (const x of arr) acc = CLmod.clSnoc(acc, x);
  return acc;
}
test('js-ast: ugly source basics', () => {
  const e = new J.JBinop(new J.JNum(1), J.jPlus, new J.JNum(2));
  assert.strictEqual(e.toUglySource(), '1 + 2');
  const obj = new J.JObj(clist([new J.JField('a', new J.JNum(1))]));
  assert.strictEqual(obj.toUglySource(), '{"a":1}');
});

// ---------- cmdline ----------
const C = load('cmdline.js');
test('cmdline: flag vs value option dash rules', () => {
  const opts = new Map([
    ['flagopt', C.flag(C.once, 'a flag')],
    ['valopt', C.nextVal(C.Str, C.once, 'a value')],
  ]);
  const ok = C.parseArgs(opts, ['-flagopt', '--valopt', 'v', 'prog.arr']);
  assert.strictEqual(ok.$name, 'success');
  assert.strictEqual(ok.parsed.has('flagopt'), true);
  assert.strictEqual(ok.parsed.get('valopt'), 'v');
  assert.deepStrictEqual(ok.unknown, ['prog.arr']);

  const bad = C.parseArgs(opts, ['--flagopt']);
  assert.strictEqual(bad.$name, 'arg-error');
  assert.match(bad.message, /does not start with two dashes/);

  const bad2 = C.parseArgs(opts, ['-valopt', 'v']);
  assert.strictEqual(bad2.$name, 'arg-error');
  assert.match(bad2.message, /must start with two dashes/);
});

// ---------- well-formedness ----------
const WF = load('well-formed.js');
const AU = load('ast-util.js');
test('well-formed: mixed binops error', () => {
  const prog = P.surfaceParse('x = 1 + 2 * 3\n', 'file://test');
  const res = WF.checkWellFormed(AU.appendNothingIfNecessary(prog));
  assert.strictEqual(res.$name, 'err');
  assert.ok(res.problems.length >= 1);
});
test('well-formed: ok program passes through', () => {
  const prog = P.surfaceParse('x = 5\nprint(x)\n', 'file://test');
  const res = WF.checkWellFormed(AU.appendNothingIfNecessary(prog));
  assert.strictEqual(res.$name, 'ok');
});

// ---------- error rendering ----------
const RED = load('render-error-display.js');
const ED = load('error-display.js');
test('error-display: renders text and locs', () => {
  const e = ED.error(
    ED.para(ED.text('The function'), ED.code(ED.text('f')), ED.text('is bad')));
  const s = RED.displayToString(e, String, []);
  assert.strictEqual(s, '\nThe function`f`is bad');
});
// The reactor "Valid options" error lists the option names. reactorFields is a
// Map (insertion order) here and a StringDict (hash order) in the .arr original,
// so the two compilers diverged; both now sort() for a canonical, byte-identical
// order (port review cross-cutting #1). This asserts the sorted order, which is
// what well-formed.arr also produces after its matching keys-list().sort().
test('well-formed: reactor Valid-options message is sorted', () => {
  const prog = P.surfaceParse('r = reactor:\n  init: 0,\n  not-a-real-option: 5\nend\nprint(r)\n', 'file://test');
  const res = WF.checkWellFormed(AU.appendNothingIfNecessary(prog));
  assert.strictEqual(res.$name, 'err');
  const problem = res.problems.find((p) => typeof p.renderReason === 'function');
  const rendered = RED.displayToString(problem.renderReason(), String, []);
  assert.ok(rendered.includes('Valid options for reactors are'),
    'expected the reactor Valid-options error');
  assert.ok(rendered.includes(
    '`close-when-stop`, `init`, `last-image`, `on-key`, `on-mouse`, `on-raw-key`, '
    + '`on-tick`, `seconds-per-tick`, `stop-when`, `title`, or `to-draw`'),
    'reactor options must be listed in sorted order');
});

// ---------- typechecker: record-type identity ----------
// A record type is a *set* of (name :: type) fields; field order is not part
// of its identity. The typechecker keys its Set<Type> (TypeSet) off type.key(),
// and Pyret's tree-set of types is ordered by `<`, which is likewise defined
// via key(). In Pyret key() iterates a StringDict in content-deterministic hash
// order, so two structurally-equal records built in different field orders get
// the SAME key() and collapse to one TypeSet entry. The port must preserve that
// invariant: key() has to agree with equals(), which is field-order-independent.
const TS = load('type-structs.js');
const TCS = load('type-check-structs.js');
test('typecheck: record key() is field-order independent (agrees with equals)', () => {
  const anyT = () => new TS.TTop(SL.dummyLoc, false);
  const botT = () => new TS.TBot(SL.dummyLoc, false);
  const record = (pairs) => {
    const m = new Map();
    for (const [n, t] of pairs) { m.set(n, t); }
    return new TS.TRecord(m, SL.dummyLoc, false);
  };
  // Same name->type mapping, inserted in two different orders.
  const recAB = record([['a', anyT()], ['b', botT()]]);
  const recBA = record([['b', botT()], ['a', anyT()]]);

  // Premise: the two records ARE the same type (equals is order-independent).
  assert.ok(recAB.equals(recBA), 'structurally-equal records must compare equal');
  // key() is the identity notion the typechecker actually uses; it must agree.
  assert.strictEqual(recAB.key(), recBA.key(),
    'key() must be field-order independent to agree with equals()');
});
test('typecheck: listToTypeSet collapses structurally-equal records', () => {
  const anyT = () => new TS.TTop(SL.dummyLoc, false);
  const botT = () => new TS.TBot(SL.dummyLoc, false);
  const record = (pairs) => {
    const m = new Map();
    for (const [n, t] of pairs) { m.set(n, t); }
    return new TS.TRecord(m, SL.dummyLoc, false);
  };
  const recAB = record([['a', anyT()], ['b', botT()]]);
  const recBA = record([['b', botT()], ['a', anyT()]]);
  // A Set<Type> of two equal types has one element (Pyret tree-set semantics).
  const set = TCS.listToTypeSet([recAB, recBA]);
  assert.strictEqual(set.size, 1,
    'a TypeSet of two structurally-equal records must hold a single entry');
});

console.log('');
console.log(`unit tests: ${passed} passed, ${failed} failed`);
process.exit(failed === 0 ? 0 : 1);
