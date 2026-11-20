'use strict';

// node --test runner over the frozen oracle cases.
// JSNUMS_PATH points at an alternative js-numbers.js;
// JSNUMS_FILTER restricts to case ids containing the given text;
// JSNUMS_TIMEOUT is the per-case watchdog in milliseconds.

const test = require('node:test');
const assert = require('node:assert/strict');
const S = require('./suite');
const { runFiles } = require('./watchdog');

const libPath = process.env.JSNUMS_PATH || undefined;
const filterText = process.env.JSNUMS_FILTER;
const MAX_SHOWN = 25;
const names = S.caseFiles();

let results;
const ulpStats = {};

test.describe('frozen oracle cases', () => {
  test.before(async () => {
    results = await runFiles(libPath, names, { filterText });
  });

  for (const name of names) {
    const key = name.replace(/\.json$/, '');
    test(key, () => {
      const s = S.summarize(results.get(name));
      ulpStats[key] = s.maxUlp;
      if (s.fail) {
        const shown = s.failures.slice(0, MAX_SHOWN).map(S.formatFailure);
        assert.fail(s.fail + ' of ' + s.total + ' cases failed:\n  ' + shown.join('\n  ') +
          (s.fail > MAX_SHOWN ? '\n  ... ' + (s.fail - MAX_SHOWN) + ' more' : ''));
      }
    });
  }

  test.after(() => {
    const lines = Object.entries(ulpStats).filter(([, v]) => v > 0n).map(([k, v]) => k + '=' + v);
    if (lines.length) console.log('# max ulp error observed: ' + lines.join(' '));
  });
});
