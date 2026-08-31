/*
 * stack-safety-test.js -- compiling a BIG program must not exhaust the JS
 * stack when the program is merely large, not deeply nested.
 *
 * The Pyret-hosted compiler runs on the runtime's segmented stack, so its
 * recursion depth never meets the JS limit; this port recurses on the real
 * JS stack, so any place the compiler's recursion depth grows with program
 * SIZE (statement count, operand count, arm count, chain length -- the
 * freshId class) breaks flat programs that users really write. Genuinely
 * nested source (10k nested ifs) overflowing the parser is the accepted
 * cost of the port and is not asserted here.
 *
 * Each case compiles a generated flat program in a subprocess pinned to
 * STACK_KB -- roughly the stack budget the compiler gets inside Chrome --
 * via PYRET_TS_NO_RESPAWN (see pyret.ts's re-exec). Green cases pin sizes
 * that compile today and must stay compiling. `todo` cases are the known
 * frontier: sizes that overflow today, each annotated with the site that
 * takes the hit (run with PYRET_TS_STACK=1 to see the stack). Fixing a
 * site flips its cases green; remove the todo flag when it does.
 *
 * Object literals, tuples, and argument lists were probed clean to n=8000
 * at 600k and carry no case here.
 */
const path = require('path');
const fs = require('fs');
const os = require('os');
const { spawnSync } = require('child_process');
const { describe, test } = require('node:test');
const assert = require('node:assert/strict');

const STACK_KB = 600;

const LANG = path.join(__dirname, '..', '..', '..');
const PYRET = path.join(LANG, 'build', 'ts-compiler', 'pyret.js');

const line = (n, f) => Array.from({ length: n }, (_, i) => f(i + 1)).join('\n');
const GENERATORS = {
  // one binding statement per line; pins the ConcatList spine + emit path
  stmts: (n) => line(n, (i) => `x${i} = ${i}`) + '\nx1',
  // alternating binding/use pairs; pins the s-scope-block entry list
  alt: (n) => line(n, (i) => `y${i} = ${i}\nprint(y${i})`),
  // one long left-associated arithmetic chain on a single source line
  binop: (n) => 'n = ' + new Array(n).fill('1').join(' + ') + '\nn',
  // n statements of MODEST (150-term) chains: each operand compiles to one
  // ANF let, so this amplifies into a huge downstream program (n x 150
  // statements) from realistic source
  sums: (n) =>
    line(n, (i) => `s${i} = ` + new Array(150).fill('1').join(' + ')) + '\ns1',
  // one N-link method chain on a single source line
  chain: (n) => 'l = [list: 1]' + '.push(1)'.repeat(n) + '\nl.length()',
  // one ask with N flat arms (a deep AIf else-chain after ANF)
  ask: (n) =>
    'fun f(x):\n  ask:\n' + line(n, (i) => `    | x == ${i} then: ${i}`) +
    '\n    | otherwise: 0\n  end\nend\nf(3)',
  // one data with N variants
  data: (n) => 'data D:\n' + line(n, (i) => `  | v${i}`) + '\nend\nv1',
  // one cases with N branches
  cases: (n) =>
    'data E:\n' + line(n, (i) => `  | w${i}`) + '\nend\n' +
    'fun pick(e):\n  cases(E) e:\n' + line(n, (i) => `    | w${i} => ${i}`) +
    '\n  end\nend\npick(w2)',
  // N one-line functions (one big letrec group after resolve-scope)
  funs: (n) => line(n, (i) => `fun h${i}(): ${i} end`) + '\nh1()',
  // one [list: ...] literal with N elements
  list: (n) =>
    'l = [list: ' + Array.from({ length: n }, (_, i) => i % 10).join(', ') +
    ']\nl.length()',
};

// [shape, n, todo?, site that overflows today]
const CASES = [
  ['stmts', 2000, false],
  ['stmts', 8000, false],
  ['alt', 200, false],
  ['alt', 1000, false],
  ['alt', 4000, false],
  ['binop', 200, false],
  ['binop', 1000, true, 'anf continuation unwind'],
  ['binop', 3000, true, 'resolve-scope CheckUnbound (ast-visitors app spine)'],
  ['sums', 10, false],
  ['sums', 40, false],
  ['chain', 100, false],
  ['chain', 500, true, 'anf continuation unwind (borderline at 600k)'],
  ['chain', 2000, true, 'parse-pyret tr app-expr chain'],
  ['ask', 400, false],
  ['ask', 2000, false],
  ['data', 150, false],
  ['data', 1000, false],
  ['cases', 150, false],
  ['cases', 1000, false],
  ['funs', 400, false],
  ['funs', 2000, false],
  ['list', 8000, false],
];

function compileAt(stackKb, source) {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'pyret-stack-'));
  const file = path.join(dir, 'gen.arr');
  fs.writeFileSync(file, source + '\n');
  const res = spawnSync(process.execPath,
    ['--stack-size=' + stackKb, PYRET,
      '--build', file,
      '--builtin-js-dir', path.join(LANG, 'src', 'js', 'trove'),
      '--builtin-arr-dir', path.join(LANG, 'src', 'arr', 'trove'),
      '--compiled-dir', path.join(dir, 'compiled'),
      '-no-display-progress'],
    { cwd: LANG, encoding: 'utf8', timeout: 120000,
      env: { ...process.env, PYRET_TS_NO_RESPAWN: '1', PYRET_TS_STACK: '1' } });
  fs.rmSync(dir, { recursive: true, force: true });
  return res;
}

describe(`flat programs compile within a ${STACK_KB}KB stack`, () => {
  for (const [shape, n, todo, site] of CASES) {
    const name = `${shape} n=${n}` + (todo ? ` (today: ${site})` : '');
    test(name, { todo, timeout: 180000 }, () => {
      const res = compileAt(STACK_KB, GENERATORS[shape](n));
      assert.equal(res.status, 0,
        `compiling a flat ${shape} program of size ${n} under a ${STACK_KB}KB ` +
        'stack exited ' + res.status + ':\n' +
        ((res.stderr || '') + (res.stdout || '')).slice(0, 2000));
    });
  }
});
