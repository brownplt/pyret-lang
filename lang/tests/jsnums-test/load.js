'use strict';

const fs = require('fs');
const path = require('path');

const DEFAULT_SRC = path.join(__dirname, '..', '..', 'src', 'js', 'base', 'js-numbers.js');

class JsNumsError extends Error {
  constructor(tag, msg, extras) {
    super('js-numbers ' + tag + ': ' + msg);
    this.tag = tag;
    this.extras = extras;
  }
}

const ERRBACK_TAGS = {
  throwDivByZero: 'div-by-zero',
  throwDomainError: 'domain-error',
  throwGeneralError: 'general-error',
  throwIncomparableValues: 'incomparable-values',
  throwInternalError: 'internal-error',
  throwLogNonPositive: 'log-non-positive',
  throwRelToleranceError: 'relative-tolerance-error',
  throwSqrtNegative: 'sqrt-negative',
  throwToleranceError: 'tolerance-error',
  throwUndefinedValue: 'undefined-value',
};

function makeErrbacks() {
  const eb = {};
  for (const [name, tag] of Object.entries(ERRBACK_TAGS)) {
    eb[name] = function(msg, ...extras) { throw new JsNumsError(tag, String(msg), extras); };
  }
  return eb;
}

// Digit configurations jsbn picks from navigator.appName.
const CONFIGS = {
  28: { appName: 'Netscape' },
  30: { appName: 'Microsoft Internet Explorer' },
  26: { appName: 'Opera' },
};

// navigator === undefined means "whatever this Node process has".
function loadModule(srcPath, navigator) {
  const src = fs.readFileSync(srcPath || DEFAULT_SRC, 'utf8');
  let mod;
  const define = (name, factory) => { mod = factory(); };
  if (navigator === undefined) {
    new Function('define', src)(define);
  } else {
    new Function('define', 'navigator', src)(define, navigator);
  }
  if (!mod) throw new Error('js-numbers did not call define()');
  return mod;
}

function loadLibrary(srcPath, digitBits) {
  const nav = digitBits === undefined ? undefined : CONFIGS[digitBits];
  if (digitBits !== undefined && !nav) throw new Error('unknown digit config ' + digitBits);
  const mod = loadModule(srcPath, nav);
  const lib = mod.MakeNumberLibrary(makeErrbacks());
  return { mod, lib, digitBits: lib.BigInteger.prototype.DB };
}

module.exports = { DEFAULT_SRC, JsNumsError, ERRBACK_TAGS, makeErrbacks, CONFIGS, loadModule, loadLibrary };
