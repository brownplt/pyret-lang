'use strict';

// node --test runner over the frozen oracle cases, under each jsbn digit
// configuration. JSNUMS_PATH points at an alternative js-numbers.js;
// JSNUMS_FILTER restricts to case ids containing the given text;
// JSNUMS_TIMEOUT is the per-case watchdog in milliseconds.

const test = require('node:test');
const assert = require('node:assert/strict');
const S = require('./suite');
const { loadLibrary } = require('./load');
const { runConfig } = require('./watchdog');

const libPath = process.env.JSNUMS_PATH || undefined;
const filterText = process.env.JSNUMS_FILTER;
const MAX_SHOWN = 25;
const CONFIGS = [28, 30, 26];
const names = S.caseFiles();

const results = {};
const ulpStats = {};

test.describe('frozen oracle cases', () => {
  test.before(async () => {
    for (const bits of CONFIGS) {
      assert.equal(loadLibrary(libPath, bits).digitBits, bits, 'digit configuration did not take effect');
      results[bits] = await runConfig(libPath, bits, names, { filterText });
    }
  });

  for (const bits of CONFIGS) {
    for (const name of names) {
      test(name.replace(/\.json$/, '') + ' [' + bits + '-bit digits]', () => {
        const s = S.summarize(results[bits].get(name));
        const key = name.replace(/\.json$/, '');
        ulpStats[key] = ulpStats[key] === undefined || s.maxUlp > ulpStats[key] ? s.maxUlp : ulpStats[key];
        if (s.fail) {
          const shown = s.failures.slice(0, MAX_SHOWN).map(S.formatFailure);
          assert.fail(s.fail + ' of ' + s.total + ' cases failed:\n  ' + shown.join('\n  ') +
            (s.fail > MAX_SHOWN ? '\n  ... ' + (s.fail - MAX_SHOWN) + ' more' : ''));
        }
      });
    }
  }

  test.after(() => {
    const lines = Object.entries(ulpStats).filter(([, v]) => v > 0n).map(([k, v]) => k + '=' + v);
    if (lines.length) console.log('# max ulp error observed: ' + lines.join(' '));
  });
});
